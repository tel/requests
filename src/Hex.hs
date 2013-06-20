module Hex (toHex, hex)  where

import Data.Word
import Data.Char
import Data.List

-- | Hex encoding
toHex :: Integral a => a -> String
toHex w = let s = "0123456789ABCDEF"
              x = fromIntegral w
          in [s !! div x 16, s !! mod x 16]

-- | From bytestring-nums (DO NOT EXPORT, unsafeish).
hexalize                     :: Integral a => a -> Word8 -> a
hexalize acc byte
  | between 'a' 'f'          =  place_up (byte + 0x0a - gord 'a')
  | between 'A' 'F'          =  place_up (byte + 0x0a - gord 'A')
  | between '0' '9'          =  place_up (byte - gord '0')
  | otherwise                =  acc
  where
    between a z              =  byte >= gord a && byte <= gord z
    place_up b               =  (0x10 * acc) + fromIntegral b

-- | From bytestring-nums (DO NOT EXPORT, unsafeish). Note that this
-- reads a ByteString into any numeric type, shifting for hex. Any
-- non-hex digits are IGNORED.
hex                          :: Integral a => [Word8] -> a
hex bytes                    =  foldl' hexalize 0 bytes

gord :: Num c => Char -> c
gord = fromIntegral . ord