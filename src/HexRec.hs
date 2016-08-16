module HexRec
  ( HexRec(..)
  , hexRec2Words
  , parseHexFile
  , ppHexRec
  )
where

import Data.Bits
import Data.List (intercalate)
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric (readHex, showHex)
import Text.ParserCombinators.ReadP
import Text.Printf (printf)


data HexRec = HexRec
  { recLen :: Word8
  , recOff :: Word16
  , recTyp :: Word8
  , recDat :: [Word8]
  , recChk :: Word8
  }
  deriving (Show)
-- | Convert the data field to list of words, as big-endian bit-vectors
hexRec2Words :: HexRec -> [BV]
hexRec2Words hr =
    map toBV $ toWord16s (recDat hr)
  where
    toWord16s [] = []
    toWord16s (l:h:rest) = ((zeroBits :: Word16) .|.
                            (fromIntegral l)     .|.
                            (shiftL (fromIntegral h) 8)) :
                           toWord16s rest
    toWord16s _ = error "Encountered data record with odd number of bytes!"

    toBV w = V.fromList [ testBit w (15-i) | i <- [0..15] ]

-- | TODO verify checksum
hexRecValid :: HexRec -> Bool
hexRecValid = undefined

ppHexRec :: HexRec -> String
ppHexRec h = unwords
  [ printf "%3d" $ recLen h
  , printf "%4s" $ showHex (recOff h) ""
  , ppTyp (recTyp h)
  , intercalate " " $ map ppBV (hexRec2Words h)
  , "chk"
  ]

-- | Pretty print record type
ppTyp :: Word8 -> String
ppTyp w = case w of
             0 -> "DAT"
             1 -> "EOF"
             4 -> "EXT"
             _ -> "???"

-- Parser --------------------------------------------------------------


readHexByte = do
    h <- get
    l <- get
    let hn = char2word8 h
    let ln = char2word8 l
    return $ (shift hn 4) .|. ln

char2word8 :: Char -> Word8
char2word8 c = case readHex [c] of
                 [] -> error ("not a hex digit: " ++ [c])
                 ((x,_):_) -> x :: Word8

readHexWord16 :: ReadP Word16
readHexWord16 = do
  a <- readHexByte
  b <- readHexByte
  let z = zeroBits :: Word16
  let r = z .|. (shift (fromIntegral a) 8) .|. (fromIntegral b)
  return r

readRec :: ReadP HexRec
readRec = do
  _ <- char ':'
  recLen  <- readHexByte
  recOff  <- readHexWord16
  recTyp  <- readHexByte
  recDat  <- sequence $ replicate (fromIntegral recLen)
                                  readHexByte
  recChk  <- readHexByte
  return HexRec
           { recLen = recLen
           , recOff = recOff
           , recTyp = recTyp
           , recDat = recDat
           , recChk = recChk
           }

readHexFile :: ReadP [HexRec]
readHexFile = do
  r <- sepBy readRec (string "\r\n" <++ string "\n")
  let ws = char '\r' +++ char '\n'
  skipMany ws
  eof
  return r

parseHexFile :: FilePath -> IO [HexRec]
parseHexFile fn = do
  cont <- readFile fn
  let parser = readP_to_S readHexFile
  let good = filter (null . snd) $ parser cont
  return $ case good of
             [] -> error "No full parse"
             ((x,_):_) -> x


-- Bit Vectors ---------------------------------------------------------


-- | Bit vector type
type BV = Vector Bool

ppBV :: BV -> String
ppBV = concatMap (\b -> if b then "1" else "0") . V.toList
