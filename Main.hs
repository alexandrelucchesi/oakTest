-- Alexandre Lucchesi Alencar
-- alexandrelucchesi@gmail.com
-- OAK TEST
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as C
import Control.Monad
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M
import Network
import Numeric
import System.Environment
import System.IO
import GHC.Word

main = withSocketsDo $ do
    args <- getArgs
    if length args /= 2
       then error "Invalid parameters. Usage: main <ip> <port>."
       else do
           handle <- connectTo (head args) (Service $ last args) 
           printHeader
           go handle
           printFooter
           hClose handle

printHeader = do
    putStrLn "    )            )             (            "
    putStrLn " ( /(  (      ( /(    *   )    )\\ )  *   )  "
    putStrLn " )\\()) )\\     )\\()) ` )  /((  (()/(` )  /(  "
    putStrLn "((_)((((_)( |((_)\\   ( )(_))\\  /(_))( )(_)) "
    putStrLn "  ((_)\\ _ )\\|_ ((_) (_(_()|(_)(_)) (_(_())  "
    putStrLn " / _ (_)_\\(_) |/ /  |_   _| __/ __||_   _|  "
    putStrLn "| (_) / _ \\   ' <     | | | _|\\__ \\  | |    "
    putStrLn " \\___/_/ \\_\\ _|\\_\\    |_| |___|___/  |_|    "
    putStrLn ""
    putStrLn "Nome   : Alexandre Lucchesi Alencar"
    putStrLn "E-mail : alexandrelucchesi@gmail.com"
    putStrLn ""

printFooter = do
    putStrLn ""

go handle = do
    contents <- B.hGetSome handle 4096
    when (not $ B.null contents) $ do
        let packets  = parse contents
            msgRec = B.pack . applyAll3 . map toFour . concatMap translateDecode $ packets
            msgNew = C.dropWhile (== ' ') (C.reverse msgRec)
            msgEnc = encode msgNew 

        putStrLn "=================================================="
        putStrLn ""
        putStr ">> Received (bytes) : " >> print (bs2Hex contents)
        putStrLn ""
        putStr ">> Decoded message  : " >> C.putStrLn msgRec
        putStr "   " >> (print $ w82Hex (B.unpack msgRec))
        putStrLn ("   Length : " ++ show (C.length msgRec))
        putStrLn ""

        if B.unpack contents == ok
            then putStrLn "It works! (-:" 
            else if B.unpack contents == err
                    then putStrLn "Really? :-( Maybe I misunderstood Protocol X workings. Let me know, please."
                    else do
                        putStr ">> Inverted message (no trailing) : " >> C.putStrLn msgNew
                        putStr "   " >> (print $ w82Hex (B.unpack msgNew))
                        putStrLn ("   Length : " ++ show (C.length msgNew))
                        putStrLn ""
                        putStr ">> New encoded message : " >> print (bs2Hex msgEnc)
                        putStrLn ""
                        B.hPut handle msgEnc >> hFlush handle -- send to remote server.
                        go handle

table = M.fromList . map (\(v1,v2) -> (bin2dec v1, bin2dec v2)) $
    [ ("0000", "11110")
    , ("0001", "01001")
    , ("0010", "10100")
    , ("0011", "10101")
    , ("0100", "01010")
    , ("0101", "01011")
    , ("0110", "01110")
    , ("0111", "01111")
    , ("1000", "10010")
    , ("1001", "10011")
    , ("1010", "10110")
    , ("1011", "10111")
    , ("1100", "11010")
    , ("1101", "11011")
    , ("1110", "11100")
    , ("1111", "11101")
    ]

startPacket, endPacket, endTransmission :: Word8
startPacket     = hex2Dec "C6"
endPacket       = hex2Dec "6B"
endTransmission = hex2Dec "21"
ok              = map hex2Dec ["C6","57","55","7A","7A","9E","21"]
err             = map hex2Dec ["C6","52","D7","45","D2","9E","21"]

-----------------------------------------------------------------------------------------
-- DECODING
-----------------------------------------------------------------------------------------

parse :: B.ByteString -> [B.ByteString]
parse ws
    | B.null ws              = []
    | begin /= startPacket   = []
    | end /= endTransmission = []
    | otherwise              = validate $ map delHeader (B.splitWith (\x -> x == endPacket) contents)
    where
        begin       = B.head ws
        end         = B.last ws
        contents    = B.init ws
        delHeader x = if B.head x == startPacket then B.tail x else error "Bad packet format"
        validate xs  = if and $ map (\y -> B.length y == 5) xs then xs else error "Bad packet format" 

translateDecode = applyAll2 . B.unpack

toFour w = fromJust $ M.lookup w tableInv 
    where tableInv = M.fromList $ map swap . M.toList $ table 

applyAll2 ws = applyAll2' (8, ws) 8
    where
    applyAll2' _ 0 = []
    applyAll2' (x,xs) n =
        let (v,(x',xs')) = ret (x,xs) `apply` g
            in v : applyAll2' (x',xs') (n-1)
    g ws = nextByte' ws 5

applyAll3 ws = applyAll3' (4,map (`shiftL` 4) ws) 8 
    where
    applyAll3' _ 0 = []
    applyAll3' (x,xs) n =
        let (v,(x',xs')) = ret (x,xs) `apply` h
            in v : applyAll3' (x',xs') (n-1)
    h ws = nextByte'' ws 8

nextByte'' :: (Int,[Word8]) -> Int -> (Word8,(Int,[Word8]))
nextByte'' (x,ws)     0 = (0,(x,ws))
nextByte'' (x,(w:ws)) n
    | n == x = (fst (takeBits w n),(4,ws))  
    | n <  x = (fst (takeBits w n),((x-n),snd (takeBits w n):ws))
    | n >  x = let res = nextByte'' (4,ws) (n-x)
                   in ((fst (takeBits w x) `shiftL` (n-x)) .|. fst res, snd res)
nextByte'' (x,[])     n = (hex2Dec "20",(0,[])) -- "OK" breaking fix.

-- n = number of bits I want to read
-- x = number of content bits in the next word
nextByte' :: (Int,[Word8]) -> Int -> (Word8,(Int,[Word8]))
nextByte' (x,ws)     0 = (0,(x,ws))
nextByte' (x,(w:ws)) n
    | n == x = (fst (takeBits w n),(8,ws))  
    | n <  x = (fst (takeBits w n),((x-n),snd (takeBits w n):ws))
    | n >  x = let res = nextByte' (8,ws) (n-x)
                   in ((fst (takeBits w x) `shiftL` (n-x)) .|. fst res, snd res)


-----------------------------------------------------------------------------------------
-- ENCODING
-----------------------------------------------------------------------------------------

encode bs = frame . flatten . translate . pad $ bs

pad :: C.ByteString -> C.ByteString
pad bs
    | C.length bs `mod` 4 == 0 = bs
    | otherwise                = pad (bs `C.append` B.singleton (hex2Dec "20"))

translate :: B.ByteString -> B.ByteString
translate bs = B.pack $ concatMap toFive (B.unpack bs)

toFive x =
    let first  = (x .&. hex2Dec "F0") `shiftR` 4
        second = x .&. hex2Dec "0F"
        in let v1 = fromJust $ M.lookup first table
               v2 = fromJust $ M.lookup second table
               in [v1,v2]

flatten ws = B.pack $ flatten' (5,map (`shiftL` 3) (B.unpack ws)) ((B.length ws `div` 2)+(B.length ws `div` 8))
    where
    flatten' _ 0 = []
    flatten' (x,xs) n =
        let (v,(x',xs')) = ret (x,xs) `apply` f
            in v : flatten' (x',xs') (n-1)
    f ws = nextByte ws 8

frame ws
    | B.null ws = B.empty
    | otherwise = let (v1,v2) = B.splitAt 5 ws
                      start = B.singleton startPacket
                      endP  = B.singleton endPacket
                      endT  = B.singleton endTransmission
                      in if B.null v2
                            then start `B.append` v1 `B.append` endT
                            else start `B.append` v1 `B.append` endP `B.append` frame v2
                              

-- There's room to improvement! Abstract code below to the Monads world! :-)
-- M a -> (a -> M b) -> M b (>>=)
apply :: (Word8,(Int,[Word8])) -> ((Int,[Word8]) -> (Word8,(Int,[Word8]))) -> (Word8,(Int,[Word8]))
apply (_,x) f = let (v,y) = f x in (v,y) 

-- a -> M a (return)
ret :: (Int,[Word8]) -> (Word8,(Int,[Word8]))
ret (x,ws) = (0,(x,ws))

-- x = num of data bits in the first byte of [Word8] 
-- n = num of bits we want to group in each element
nextByte :: (Int,[Word8]) -> Int -> (Word8,(Int,[Word8]))
nextByte (x,ws)     0 = (0,(x,ws))
nextByte (x,(w:ws)) n
    | n == x = (fst (takeBits w n),(5,ws))  
    | n <  x = (fst (takeBits w n),((x-n),snd (takeBits w n):ws))
    | n >  x = let res = nextByte (5,ws) (n-x)
                   in ((fst (takeBits w x) `shiftL` (n-x)) .|. fst res, snd res)

-- w = word to read bits from
-- n = number of bits to be read
-- returns = (bits read,remainder of w)
takeBits :: Word8 -> Int -> (Word8,Word8)
takeBits w 0 = (0,w)
takeBits w n = 
    let bit  = msb w
        next = takeBits (w `shiftL` 1) (n-1)
        in (bit `shiftL` (n-1) .|. fst next, snd next)

msb w = if testBit w 7 == True then 1 else 0

-- UTIL
msg = C.pack "BSB KAO"
msg' = B.pack . map hex2Dec $ ["C6", "57", "54", "95", "5E", "9E", "6B", "C6", "55", "17", "55", "52", "9E", "21"]

hex2Dec = fst . head . readHex

bs2Hex = map (\x -> "0x" ++ showIntAtBase 16 intToDigit x "") . B.unpack
bs2Bin = map (\x -> showIntAtBase 2 intToDigit x "") . B.unpack

w82Hex, w82Bin :: (Integral a, Show a) => [a] -> [String]
w82Hex = map (\x -> "0x" ++ showIntAtBase 16 intToDigit x "")
w82Bin = map (\x -> showIntAtBase 2 intToDigit x "")

bin2dec :: String -> Word8
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

