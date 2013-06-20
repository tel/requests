{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module      : URI
-- Copyright   : (c) Joseph Abrahamson 2013
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Printer/Parsers for URIs following RFC 2396. Loosely inspired by
-- Rendel 2010 and Kennedy 2004. The goal should be to have printer
-- parser pairs on URIs such that parsing is a left identity of
-- printing. This is a useful property for servers, clients, and
-- proxies, but also allows for lens-like access to complex types
-- (e.g. the raw URI types like 'AbsoluteURI') via less complex types
-- (like strings) which observe a partial isomorphism with the target
-- type. The current implementation strives to be a reference
-- implementation following the RFC very closely---it can be optimized
-- later.
-- 
-- Rendel, T., & Ostermann, K. (2010). Invertible syntax descriptions:
--     unifying parsing and pretty printing, 45(11), 1–12.
-- 
-- Kennedy, A. J. (2004). Functional pearl pickler combinators. Journal
--     of functional programming, 14(6), 727–739.
module URI where

import Prelude hiding (print)

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.PrettyPrint hiding ((<>), empty)

import Hex
import Data.Char
import Data.Monoid
import Data.List.NonEmpty (NonEmpty (..))

import Data.Functor.Contravariant
import Data.Foldable (foldMap)
import Data.Ix

import Control.Arrow

-- $combinators

unconsNE :: NonEmpty a -> (a, [a])
unconsNE (a :| as) = (a, as)

uncons :: [a] -> (a, [a])
uncons (a:as) = (a, as)

pAnyOf :: String -> Parser Char
pAnyOf = pAny pSym

pString :: String -> Parser ()
pString = pFoldr_ng (\_ _ -> (), ()) . sequence . map pSym

newtype Printer a = Printer { print :: a -> Maybe Doc }

infixr 4 .$.
infixr 4 .*.

instance Contravariant Printer where
  contramap g (Printer f) = Printer (f . g)

class Monoidal f where
  unit  :: f ()
  (.*.) :: f a -> f b -> f (a, b)

instance Monoidal Printer where
  unit = Printer (const $ Just mempty)
  (Printer pa) .*. (Printer pb) = Printer $ \(a, b) -> pa a <> pb b

instance Monoidal (P state) where
  pa .*. pb = (,) <$> pa <*> pb
  unit      = pure ()

class WAlternative f where
  wempty :: f a
  (.|.)  :: f a -> f a -> f a

instance WAlternative Printer where
  wempty    = Printer (const Nothing)
  p1 .|. p2 = Printer $ \c -> getFirst $ First (print p1 c) <> First (print p2 c)

(.$.) :: Contravariant f => (a -> b) -> f b -> f a
(.$.) = contramap

(.$) :: Contravariant f => b -> f b -> f a
(.$) = contramap . const

liftW2 :: (Contravariant f, Monoidal f) =>
          (x -> (a, b)) -> f a -> f b -> f x
liftW2 f a b = f .$. a .*. b

(.*) :: (Contravariant f, Monoidal f) => forall b. f a -> f () -> f a
(.*) = liftW2 (,())

(*.) :: (Contravariant f, Monoidal f) => f () -> f b -> f b
(*.) = liftW2 ((),)

maybeIf :: Bool -> a -> Maybe a
maybeIf x a = if x then Just a else Nothing

sSatisfy :: Predicate Char -> Printer Char
sSatisfy (Predicate p) = Printer $ \c -> maybeIf (p c) (char c)

sAnyOf :: String -> Printer Char
sAnyOf = sSatisfy . Predicate . flip elem

sSym :: Char -> Printer Char
sSym = sSatisfy . Predicate . (==)

sRange :: (Char, Char) -> Printer Char
sRange = sSatisfy . Predicate . inRange

sMany :: Printer a -> Printer [a]
sMany (Printer p) = Printer (foldMap p)

sSome :: Printer a -> Printer [a]
sSome (Printer p) = Printer go where
  go [] = Nothing
  go xs = foldMap p xs

sJust :: Char -> Printer a
sJust c = Printer (const $ Just $ char c)

sText :: String -> Printer a
sText s = Printer (const $ Just $ text s)

sChoice :: Printer () -> Printer () -> Printer Bool
sChoice pa pb =
  Printer $ \b -> if b then print pa () else print pb ()

sEither :: Printer a -> Printer b -> Printer (Either a b)
sEither pa pb = Printer go where
  go (Left a)  = print pa a
  go (Right b) = print pb b
data F
sMaybe :: Printer a -> Printer (Maybe a)
sMaybe pa = Printer go where
  go (Just x) = print pa x
  go Nothing  = print unit ()

-- $character-classes
pLowAlpha, pUpAlpha, pDigit, pHex, pAlpha, pAlphaNum :: Parser Char
pMark, pUnreserved, pReserved, pEscaped, pUriC, pPChar :: Parser Char
sLowAlpha, sUpAlpha, sDigit, sHex, sAlpha, sAlphaNum :: Printer Char
sMark, sUnreserved, sReserved, sEscaped, sUriC, sPChar :: Printer Char

pLowAlpha   = pRange ('a', 'z') <?> "lowercase alpha"
sLowAlpha   = sRange ('a', 'z')
pUpAlpha    = pRange ('A', 'Z') <?> "uppercase alpha"
sUpAlpha    = sRange ('A', 'Z')
pDigit      = pRange ('0', '9') <?> "digit"
sDigit      = sRange ('0', '9')
pHex        = pRange ('a', 'f')
              <|> pRange ('A', 'F')
              <|> pDigit
              <?> "hexidecimal digit"
sHex        = sRange ('a', 'f')
              .|. sRange ('A', 'F')
              .|. sDigit

pAlpha      = pLowAlpha <|> pUpAlpha <?> "alpha"
sAlpha      = sLowAlpha .|. sUpAlpha
pAlphaNum   = pAlpha <|> pDigit <?> "alphanumeric"
sAlphaNum   = sAlpha .|. sDigit

pMark       = pAnyOf "-_.!~*'()" <?> "mark"
sMark       = sAnyOf "-_.!~*'()"
pUnreserved = pAlphaNum <|> pMark <?> "unreserved character"
sUnreserved = sAlphaNum .|. sMark
pReserved   = pAnyOf ";/?:@&=+$,"
sReserved   = sAnyOf ";/?:@&=+$,"

pEscaped    = unHex <$ pSym '%' <*> pHex <*> pHex
              <?> "percent-escaped character"
  where
    unHex a b = chr . hex . map (fromIntegral . ord) $ [a, b]

sEscaped    = contramap go (sHex .*. sHex)
  where
    go c = let (a:b:_) = toHex (ord c) in (a, b)

pUriC       = pReserved
              <|> pUnreserved
              <|> pEscaped
              <?> "URI character"
sUriC       = sReserved
              .|. sUnreserved
              .|. sEscaped

pPChar      = pUnreserved
              <|> pAnyOf ":@&=+$,"
              <|> pEscaped
              <?> "path-character"
sPChar      = sUnreserved
              .|. sAnyOf ":@&=+$,"
              .|. sEscaped

data Either3 a b c   = Ei1 a | Ei2 b | Ei3 c deriving (Eq, Show, Read, Ord)

-- $small-parses

newtype Query        = Query        { unQuery :: String }
                       deriving (Show, Eq, Ord)
newtype Fragment     = Fragment     { unFragment :: String }
                       deriving (Show, Eq, Ord)
newtype Param        = Param        { unParam :: String }
                       deriving (Show, Eq, Ord)
newtype Segment      = Segment      { unSegment :: (String, [Param]) }
                       deriving (Show, Eq, Ord)
newtype PathSegments = PathSegments { unPathSegments :: (NonEmpty Segment) }
                       deriving (Show, Eq, Ord)
newtype Port         = Port         { unPort :: Int }
                       deriving (Show, Eq, Ord)
newtype IPv4Address  = IPv4Address  (Int, Int, Int, Int)
                       deriving (Show, Eq, Ord)
data    HostName     = HostName
                       [String]       -- ^ Zero or more Domain Names
                       String         -- ^ A TLD
                       Bool           -- ^ Is it an FQDN? It ended in '.'?
                       deriving (Show, Eq, Ord)
newtype Host         = Host         { unHost :: (Either HostName IPv4Address) }
                       deriving (Show, Eq, Ord)
data    HostPort     = HostPort     Host (Maybe Port)
                       deriving (Show, Eq, Ord)
newtype UserInfo     = UserInfo     { unUserInfo :: String }
                       deriving (Show, Eq, Ord)
data    Server       = Server       (Maybe UserInfo) HostPort
                       deriving (Show, Eq, Ord)
newtype RegName      = RegName      { unRegName :: String }
                       deriving (Show, Eq, Ord)
newtype Authority    = Authority    { unAuthority :: Either Server RegName }
                       deriving (Show, Eq, Ord)
newtype Scheme       = Scheme       { unScheme :: String }
                       deriving (Show, Eq, Ord)
newtype RelSegment   = RelSegment   { unRelSegment :: String }
                       deriving (Show, Eq, Ord)
newtype AbsPath      = AbsPath      { unAbsPath :: PathSegments }
                       deriving (Show, Eq, Ord)
data    NetPath      = NetPath      (Maybe Authority) (Maybe AbsPath)
                       deriving (Show, Eq, Ord)
data    RelPath      = RelPath      RelSegment (Maybe AbsPath)
                       deriving (Show, Eq, Ord)
newtype OpaquePart   = OpaquePart   { unOpaquePart :: String }
                       deriving (Show, Eq, Ord)
data    HierPart     = HierPart     (Either NetPath AbsPath) (Maybe Query)
                       deriving (Show, Eq, Ord)
newtype Path         = Path         { unPath :: Either AbsPath OpaquePart }
                       deriving (Show, Eq, Ord)
data    RelativeURI  = RelativeURI  (Either3 NetPath AbsPath RelPath)
                                    (Maybe Query)
                       deriving (Show, Eq, Ord)
data    AbsoluteURI  = AbsoluteURI  Scheme (Either HierPart OpaquePart)
                       deriving (Show, Eq, Ord)

-- fragment      = *uric

pFragment     :: Parser Fragment
pFragment     = Fragment    <$> pMany pUriC  <?> "fragment"

sFragment     :: Printer Fragment
sFragment     = unFragment  .$. sMany sUriC

-- query         = *uric

pQuery        :: Parser Query
pQuery        = Query       <$> pMany pUriC  <?> "query string"

sQuery        :: Printer Query
sQuery        = unQuery     .$. sMany sUriC

-- param         = *pchar

pParam        :: Parser Param
pParam        = Param       <$> pMany pPChar <?> "parameter"

sParam        :: Printer Param
sParam        = unParam     .$. sMany sPChar

-- segment       = *pchar *( ";" param )

pSegment      :: Parser Segment
pSegment      = go          <$> pMany pPChar
                            <*> pMany (pSym ';' *> pParam)
                            <?> "segment"
  where go a b = Segment (a, b)

sSegment      :: Printer Segment
sSegment      = unSegment   .$. sMany sPChar
                            .*. sMany (sJust ';' *. sParam)

-- path_segments = segment *( "/" segment )

pPathSegments :: Parser PathSegments
pPathSegments = go          <$> pSegment
                            <*> pMany (pSym '/' *> pSegment)
                            <?> "path segments"
  where go :: Segment -> [Segment] -> PathSegments
        go s ss = PathSegments (s :| ss)

sPathSegments :: Printer PathSegments
sPathSegments = unPS        .$. sSegment
                            .*. sMany (sJust '/' *. sSegment)
  where unPS :: PathSegments -> (Segment, [Segment])
        unPS = unconsNE . unPathSegments

-- port          = *digit

pPort         :: Parser Port
pPort         = go          <$> pMany pDigit
  where go :: String -> Port
        go = Port . read

sPort         :: Printer Port
sPort         = show.unPort .$. sMany sDigit

-- IPv4address   = 1*digit "." 1*digit "." 1*digit "." 1*digit

pIPv4Address  :: Parser IPv4Address
pIPv4Address  = go          <$> pSome pDigit
                            <*> (pSym '.' *> pSome pDigit)
                            <*> (pSym '.' *> pSome pDigit)
                            <*> (pSym '.' *> pSome pDigit)
                            <?> "IPv4 address"
  where go :: String -> String -> String -> String -> IPv4Address
        go a b c d = IPv4Address (read a, read b, read c, read d)

sIPv4Address  :: Printer IPv4Address
sIPv4Address  = go          .$. sMany sDigit
                            .*. (sJust '.' *. sMany sDigit)
                            .*. (sJust '.' *. sMany sDigit)
                            .*. (sJust '.' *. sMany sDigit)
  where go :: IPv4Address -> (String, (String, (String, String)))
        go (IPv4Address (a,b,c,d)) = (show a, (show b, (show c, show d)))

-- hostname      = *( domainlabel "." ) toplabel [ "." ]
-- domainlabel   = alphanum | alphanum *( alphanum | "-" ) alphanum
-- toplabel      = alpha | alpha *( alphanum | "-" ) alphanum

pHostName     :: Parser HostName
pHostName     = HostName    <$> pList_ng (pBit pAlpha <* pSym '.')
                            <*> pBit pAlphaNum
                            <*> ((True <$ pSym '.') <|> pure False)
                            <?> "hostname"
  where pBit     :: Parser Char -> Parser String
        pBit p   = go <$> p <*> pMaybe remainder
        remainder :: Parser String
        remainder = do
          grab <- pMany (pAlphaNum <|> pSym '-')
          case grab of
            []    -> pFail
            grab' -> if last grab' == '-'
                     then pFail
                     else return grab
        go :: Char -> Maybe String -> String
        go c Nothing  = [c]
        go c (Just s) = c:s
        snoc :: String -> Char -> String
        snoc as a = as ++ [a]

sHostName     :: Printer HostName
sHostName     = go          .$. sMany (sBit sAlpha .* sJust '.')
                            .*. sBit sAlphaNum
                            .*. sChoice (sJust '.') unit
  where go :: HostName -> ([String], (String, Bool))
        go (HostName dl tld fqdnp) = (dl, (tld, fqdnp))
        -- Not clear exactly how to parallel this---it actually seems
        -- to indicate to me that pHostName is broken.
        sBit :: Printer Char -> Printer String
        sBit _ = sMany (sAlphaNum .|. sSym '-')

-- host          = hostname | IPv4address

pHost         :: Parser Host
pHost         = Host        <$> pEither pHostName pIPv4Address

sHost         :: Printer Host
sHost         = unHost      .$. sEither sHostName sIPv4Address

-- hostport      = host [ ":" port ]

pHostPort     :: Parser HostPort
pHostPort     = HostPort    <$> pHost <*> pMaybe (pSym  ':' *> pPort)

sHostPort     :: Printer HostPort
sHostPort     = go          .$. sHost .*. sMaybe (sJust ':' *. sPort)
  where go :: HostPort -> (Host, Maybe Port)
        go (HostPort h mp) = (h, mp)

--  userinfo      = *( unreserved | escaped |
--                     ";" | ":" | "&" | "=" | "+" | "$" | "," )

pUserInfo     :: Parser UserInfo
pUserInfo     = UserInfo    <$> pMany (    pUnreserved
                                       <|> pAnyOf ";:&=+$,"
                                       <|> pEscaped )

sUserInfo     :: Printer UserInfo
sUserInfo     = unUserInfo  .$. sMany (    sUnreserved
                                       .|. sAnyOf ";:&=+$,"
                                       .|. sEscaped )

-- server        = [ [ userinfo "@" ] hostport ]

pServer       :: Parser Server
pServer       = Server      <$> pMaybe (pUserInfo <* pSym  '@')
                            <*> pHostPort

sServer       :: Printer Server
sServer       = go          .$. sMaybe (sUserInfo .* sJust '@')
                            .*. sHostPort
  where go :: Server -> (Maybe UserInfo, HostPort)
        go (Server mui hp) = (mui, hp)

-- reg_name      = 1*( unreserved | escaped | "$" | "," |
--                     ";" | ":" | "@" | "&" | "=" | "+" )

pRegName      :: Parser RegName
pRegName      = RegName     <$> pSome (    pUnreserved
                                       <|> pEscaped
                                       <|> pAnyOf "$,;:@&=+" )

sRegName      :: Printer RegName
sRegName      = unRegName   .$. sSome (    sUnreserved
                                       .|. sEscaped
                                       .|. sAnyOf "$,;:@&=+" )

-- authority     = server | reg_name

pAuthority    :: Parser Authority
pAuthority    = Authority   <$> pEither pServer pRegName

sAuthority    :: Printer Authority
sAuthority    = unAuthority .$. sEither sServer sRegName

-- scheme        = alpha *( alpha | digit | "+" | "-" | "." )

pScheme       :: Parser Scheme
pScheme       = go          <$> pAlpha
                            <*> pMany (pAlpha <|> pDigit <|> pAnyOf "+-.")
  where go :: Char -> String -> Scheme
        go c cs = Scheme (c:cs)

-- This one is a little weird since it's possible that we can't
-- destruct the scheme---failure happens early.
sScheme       :: Printer Scheme
sScheme       = Printer go 
  where go :: Scheme -> Maybe Doc
        go (Scheme []) = Nothing
        go (Scheme s)  =
          print (uncons .$. sAlpha
                        .*. sMany (sAlpha .|. sDigit .|. sAnyOf "+-.")) s

-- rel_segment   = 1*( unreserved | escaped |
--                     ";" | "@" | "&" | "=" | "+" | "$" | "," )

pRelSegment   :: Parser RelSegment
pRelSegment   = RelSegment  <$> pSome (    pUnreserved
                                       <|> pAnyOf ";@&=+$,"
                                       <|> pEscaped )

sRelSegment   :: Printer RelSegment
sRelSegment   = unRelSegment .$. sSome (sUnreserved
                                        .|. sAnyOf ";@&=+$,"
                                        .|. sEscaped )

-- abs_path      = "/"  path_segments

pAbsPath      :: Parser AbsPath
pAbsPath      = AbsPath     <$> (pSym '/' *> pPathSegments)

sAbsPath      :: Printer AbsPath
sAbsPath      = unAbsPath   .$. (sJust '/' *. sPathSegments)

-- net_path      = "//" authority [ abs_path ]

pNetPath      :: Parser NetPath
pNetPath      = NetPath     <$  pString "//"
                            <*> pMaybe pAuthority
                            <*> pMaybe pAbsPath

sNetPath      :: Printer NetPath
sNetPath      = go          .$. sText "//"
                             *. sMaybe sAuthority
                            .*. sMaybe sAbsPath
  where go :: NetPath -> (Maybe Authority, Maybe AbsPath)
        go (NetPath ma mabs) = (ma, mabs)

-- rel_path      = rel_segment [ abs_path ]

pRelPath      :: Parser RelPath
pRelPath      = RelPath     <$> pRelSegment
                            <*> pMaybe pAbsPath

sRelPath      :: Printer RelPath
sRelPath      = go          .$. sRelSegment
                            .*. sMaybe sAbsPath
  where go :: RelPath -> (RelSegment, Maybe AbsPath)
        go (RelPath rs mabs) = (rs, mabs)

-- opaque_part   = uric_no_slash *uric

pOpaquePart   :: Parser OpaquePart
pOpaquePart   = go          <$> (    pUnreserved
                                 <|> pAnyOf ";?:@&=+$,"
                                 <|> pEscaped )
                            <*> pMany pUriC
  where go c cs = OpaquePart (c:cs)

sOpaquePart   :: Printer OpaquePart
sOpaquePart   = Printer go 
  where go :: OpaquePart -> Maybe Doc
        go (OpaquePart []) = Nothing
        go (OpaquePart s)  =
          print (uncons .$. (    sUnreserved
                             .|. sAnyOf ";?:@&=+$,"
                             .|. sEscaped )
                        .*. sMany sUriC)
                s

-- hier_part     = ( net_path | abs_path ) [ "?" query ]

pHierPart     :: Parser HierPart
pHierPart     = HierPart    <$> pEither pNetPath pAbsPath
                            <*> pMaybe (pSym  '?' *> pQuery)

sHierPart     :: Printer HierPart
sHierPart     = go          .$. sEither sNetPath sAbsPath
                            .*. sMaybe (sJust '?' *. sQuery)
  where go :: HierPart
              -> (Either NetPath AbsPath, Maybe Query)
        go (HierPart npap mayq) = (npap, mayq)

-- path          = [ abs_path | opaque_part ]

pPath         :: Parser Path
pPath         = Path        <$> pEither pAbsPath pOpaquePart

sPath         :: Printer Path
sPath         = unPath      .$. sEither sAbsPath sOpaquePart

-- relativeURI   = ( net_path | abs_path | rel_path ) [ "?" query ]

pRelativeURI  :: Parser RelativeURI
pRelativeURI  = RelativeURI <$> (    (Ei1 <$> pNetPath)
                                 <|> (Ei2 <$> pAbsPath)
                                 <|> (Ei3 <$> pRelPath) )
                            <*> pMaybe (pSym '?'  *> pQuery)

sRelativeURI  :: Printer RelativeURI
sRelativeURI  = go         .$. sEither3 sNetPath sAbsPath sRelPath
                            .*. sMaybe (sJust '?' *. sQuery)
  where go :: RelativeURI
              -> (Either3 NetPath AbsPath RelPath, Maybe Query)
        go (RelativeURI ei3 mayq) = (ei3, mayq)
        sEither3 :: Printer a -> Printer b -> Printer c
                    -> Printer (Either3 a b c)
        sEither3 pa pb pc = Printer go3 where
          go3 (Ei1 a) = print pa a
          go3 (Ei2 b) = print pb b
          go3 (Ei3 c) = print pc c
        
-- absoluteURI   = scheme ":" ( hier_part | opaque_part )

pAbsoluteURI  :: Parser AbsoluteURI
pAbsoluteURI  = AbsoluteURI <$> ( pScheme <* pSym  ':' )
                            <*> pEither pHierPart pOpaquePart

sAbsoluteURI  :: Printer AbsoluteURI
sAbsoluteURI  = go          .$. ( sScheme .* sJust ':' )
                            .*. sEither sHierPart sOpaquePart
  where go :: AbsoluteURI
              -> (Scheme, Either HierPart OpaquePart)
        go (AbsoluteURI s eith) = (s, eith)