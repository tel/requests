{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module URI where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances

import Hex
import Data.Char
import           Data.List.NonEmpty (NonEmpty (..))

-- $combinators

pAnyOf :: String -> Parser Char
pAnyOf = pAny pSym

pString :: String -> Parser ()
pString = pFoldr_ng (\_ _ -> (), ()) . sequence . map pSym

-- $character-classes

pLowAlpha, pUpAlpha, pDigit, pHex, pAlpha, pAlphaNum :: Parser Char
pMark, pUnreserved, pReserved, pEscaped, pUriC, pPChar :: Parser Char

pLowAlpha   = pRange ('a', 'z') <?> "lowercase alpha"
pUpAlpha    = pRange ('A', 'Z') <?> "uppercase alpha"
pDigit      = pRange ('0', '9') <?> "digit"
pHex        = pRange ('a', 'f')
              <|> pRange ('A', 'F')
              <|> pDigit
              <?> "hexidecimal digit"

pAlpha      = pLowAlpha <|> pUpAlpha <?> "alpha"
pAlphaNum   = pAlpha <|> pDigit <?> "alphanumeric"

pMark       = pAnyOf "-_.!~*'()" <?> "mark"
pUnreserved = pAlphaNum <|> pMark <?> "unreserved character"
pReserved   = pAnyOf ";/?:@&=+$,"

pEscaped    = unHex <$ pSym '%' <*> pHex <*> pHex
              <?> "percent-escaped character"
  where
    unHex a b = chr . hex . map (fromIntegral . ord) $ [a, b]

pUriC       = pReserved
              <|> pUnreserved
              <|> pEscaped
              <?> "URI character"

pPChar      = pUnreserved
              <|> pEscaped
              <|> pAnyOf ":@&=+$,"
              <?> "path-character"

data Either3 a b c   = Ei1 a | Ei2 b | Ei3 c deriving (Eq, Show, Read, Ord)

-- $small-parses

newtype Query        = Query        String
                       deriving (Show, Eq, Ord)
newtype Fragment     = Fragment     String
                       deriving (Show, Eq, Ord)
newtype Param        = Param        String
                       deriving (Show, Eq, Ord)
data    Segment      = Segment      String [Param]
                       deriving (Show, Eq, Ord)
newtype PathSegments = PathSegments (NonEmpty Segment)
                       deriving (Show, Eq, Ord)
newtype Port         = Port         Int
                       deriving (Show, Eq, Ord)
newtype IPv4Address  = IPv4Address  (Int, Int, Int, Int)
                       deriving (Show, Eq, Ord)
data    HostName     = HostName
                       [String]       -- ^ Zero or more Domain Names
                       String         -- ^ A TLD
                       Bool           -- ^ Is it an FQDN? It ended in '.'?
                       deriving (Show, Eq, Ord)
newtype Host         = Host         (Either HostName IPv4Address)
                       deriving (Show, Eq, Ord)
data    HostPort     = HostPort     Host (Maybe Port)
                       deriving (Show, Eq, Ord)
newtype UserInfo     = UserInfo     String
                       deriving (Show, Eq, Ord)
data    Server       = Server       (Maybe UserInfo) HostPort
                       deriving (Show, Eq, Ord)
newtype RegName      = RegName      String
                       deriving (Show, Eq, Ord)
newtype Authority    = Authority    (Either Server RegName)
                       deriving (Show, Eq, Ord)
newtype Scheme       = Scheme       String
                       deriving (Show, Eq, Ord)
newtype RelSegment   = RelSegment   String
                       deriving (Show, Eq, Ord)
newtype AbsPath      = AbsPath      PathSegments
                       deriving (Show, Eq, Ord)
data    NetPath      = NetPath      (Maybe Authority) (Maybe AbsPath)
                       deriving (Show, Eq, Ord)
data    RelPath      = RelPath      RelSegment (Maybe AbsPath)
                       deriving (Show, Eq, Ord)
newtype OpaquePart   = OpaquePart   String
                       deriving (Show, Eq, Ord)
data    HierPart     = HierPart     (Either NetPath AbsPath) (Maybe Query)
                       deriving (Show, Eq, Ord)
newtype Path         = Path         (Either AbsPath OpaquePart)
                       deriving (Show, Eq, Ord)
data    RelativeURI  = RelativeURI  (Either3 NetPath AbsPath RelPath)
                                    (Maybe Query)
                       deriving (Show, Eq, Ord)
data    AbsoluteURI  = AbsoluteURI  Scheme (Either HierPart OpaquePart)
                       deriving (Show, Eq, Ord)

pFragment     :: Parser Fragment
pFragment     = Fragment    <$> pMany pUriC  <?> "fragment"

pQuery        :: Parser Query
pQuery        = Query       <$> pMany pUriC  <?> "query string"

pParam        :: Parser Param
pParam        = Param       <$> pMany pPChar <?> "parameter"

pSegment      :: Parser Segment
pSegment      = Segment     <$> pMany pPChar
                            <*> pMany (pSym ';' *> pParam)
                            <?> "segment"

pPathSegments :: Parser PathSegments
pPathSegments = go          <$> pSegment
                            <*> pMany (pSym '/' *> pSegment)
                            <?> "path segments"
  where go :: Segment -> [Segment] -> PathSegments
        go s ss = PathSegments (s :| ss)

pPort         :: Parser Port
pPort         = go          <$> pMany pDigit
  where go :: String -> Port
        go = Port . read

pIPv4Address  :: Parser IPv4Address
pIPv4Address  = go          <$> pSome pDigit
                            <*> pSome pDigit
                            <*> pSome pDigit
                            <*> pSome pDigit
                            <?> "IPv4 address"
  where go :: String -> String -> String -> String -> IPv4Address
        go a b c d = IPv4Address (read a, read b, read c, read d)

pHostName     :: Parser HostName
pHostName     = HostName    <$> pMany (pBit pAlpha <* pSym '.')
                            <*> pBit pAlphaNum
                            <*> ((True <$ pSym '.') <|> pure False)
                            <?> "hostname"
  where pBit :: Parser Char -> Parser String
        pBit p =     ( pure <$> p )
                 <|> ( backAndFront <$> p
                                    <*> pMany (pAlphaNum <|> pSym '-')
                                    <*> pAlphaNum )
        backAndFront :: Char -> [Char] -> Char -> String
        backAndFront f mid b = f : mid ++ [b]

pHost         :: Parser Host
pHost         = Host        <$> pEither pHostName pIPv4Address

pHostPort     :: Parser HostPort
pHostPort     = HostPort    <$> pHost <*> pMaybe (pSym ':' *> pPort)

pUserInfo     :: Parser UserInfo
pUserInfo     = UserInfo    <$> pMany (    pUnreserved
                                       <|> pEscaped
                                       <|> pAnyOf ";:&=+$," )

pServer       :: Parser Server
pServer       = Server      <$> pMaybe (pUserInfo <* pSym '@')
                            <*> pHostPort

pRegName      :: Parser RegName
pRegName      = RegName     <$> pSome (    pUnreserved
                                       <|> pEscaped
                                       <|> pAnyOf "$,;:@&=+" )

pAuthority    :: Parser Authority
pAuthority    = Authority   <$> pEither pServer pRegName

pScheme       :: Parser Scheme
pScheme       = go          <$> pAlpha
                            <*> pMany (pAlpha <|> pDigit <|> pAnyOf "+-.")
  where go :: Char -> String -> Scheme
        go c cs = Scheme (c:cs)

pRelSegment   :: Parser RelSegment
pRelSegment   = RelSegment  <$> pSome (    pUnreserved
                                       <|> pEscaped
                                       <|> pAnyOf ";@&=+$," )

pAbsPath      :: Parser AbsPath
pAbsPath      = AbsPath     <$> (pSym '/' *> pPathSegments)

pNetPath      :: Parser NetPath
pNetPath      = NetPath     <$  pString "//"
                            <*> pMaybe pAuthority
                            <*> pMaybe pAbsPath

pRelPath      :: Parser RelPath
pRelPath      = RelPath     <$> pRelSegment
                            <*> pMaybe pAbsPath

pOpaquePart   :: Parser OpaquePart
pOpaquePart   = go          <$> (    pUnreserved
                                 <|> pEscaped
                                 <|> pAnyOf ";?:@&=+$," )
                            <*> pMany pUriC
  where go c cs = OpaquePart (c:cs)

pHierPart     :: Parser HierPart
pHierPart     = HierPart    <$> pEither pNetPath pAbsPath
                            <*> pMaybe (pSym '?' *> pQuery)

pPath         :: Parser Path
pPath         = Path        <$> pEither pAbsPath pOpaquePart

pRelativeURI  :: Parser RelativeURI
pRelativeURI  = RelativeURI <$> (    (Ei1 <$> pNetPath)
                                 <|> (Ei2 <$> pAbsPath)
                                 <|> (Ei3 <$> pRelPath) )
                            <*> pMaybe (pSym '?' *> pQuery)

pAbsoluteURI  :: Parser AbsoluteURI
pAbsoluteURI  = AbsoluteURI <$> ( pScheme <* pSym ':' )
                            <*> pEither pHierPart pOpaquePart