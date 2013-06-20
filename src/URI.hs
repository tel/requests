{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module URI where

import Text.ParserCombinators.UU
import qualified Text.ParserCombinators.UU.Utils as U
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Idioms

import Hex
import Data.Char
import qualified Data.List.NonEmpty as Ne
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
newtype Fragment     = Fragment     String
newtype Param        = Param        String
data    Segment      = Segment      String [Param]
newtype PathSegments = PathSegments (NonEmpty Segment)
newtype Port         = Port         Int
newtype IPv4Address  = IPv4Address  (Int, Int, Int, Int)
data    HostName     = HostName
                       [String]       -- ^ Zero or more Domain Names
                       String         -- ^ A TLD
                       Bool           -- ^ Is it an FQDN? It ended in '.'?
newtype Host         = Host         (Either HostName IPv4Address)
data    HostPort     = HostPort     Host (Maybe Port)
newtype UserInfo     = UserInfo     String
data    Server       = Server       (Maybe UserInfo) HostPort
newtype RegName      = RegName      String
newtype Authority    = Authority    (Either Server RegName)
newtype Scheme       = Scheme       String
newtype RelSegment   = RelSegment   String
newtype AbsPath      = AbsPath      PathSegments
data    NetPath      = NetPath      (Maybe Authority) (Maybe AbsPath)
data    RelPath      = RelPath      RelSegment (Maybe AbsPath)
newtype OpaquePart   = OpaquePart   String
data    HierPart     = HierPart     (Either NetPath AbsPath) (Maybe Query)
newtype Path         = Path         (Either AbsPath OpaquePart)
data    RelativeURI  = RelativeURI  (Either3 NetPath AbsPath RelPath)
                                    (Maybe Query)
data    AbsoluteURI  = AbsoluteURI  Scheme (Either HierPart OpaquePart)

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
pHostPort     = HostPort    <$> pHost <*> (pMaybe pPort)

pUserInfo     :: Parser UserInfo
pUserInfo     = UserInfo    <$> pMany (    pUnreserved
                                       <|> pEscaped
                                       <|> pAnyOf ";:&=+$," )

pServer       :: Parser Server
pServer       = Server      <$> (pMaybe pUserInfo <* pSym '@')
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
pNetPath      = pString "//"
                *> ( NetPath <$> pMaybe pAuthority
                             <*> pMaybe pAbsPath   )

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
                            <*> (pSym '?' *> pMaybe pQuery)

pPath         :: Parser Path
pPath         = Path        <$> pEither pAbsPath pOpaquePart

pRelativeURI  :: Parser RelativeURI
pRelativeURI  = RelativeURI <$> (    (Ei1 <$> pNetPath)
                                 <|> (Ei2 <$> pAbsPath)
                                 <|> (Ei3 <$> pRelPath) )
                            <*> (pSym '?' *> pMaybe pQuery)

pAbsoluteURI  :: Parser AbsoluteURI
pAbsoluteURI  = AbsoluteURI <$> ( pScheme <* pSym ':' )
                            <*> pEither pHierPart pOpaquePart