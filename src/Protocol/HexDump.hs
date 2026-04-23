module Protocol.HexDump (analyzeHex, hexDump) where

import Numeric (showHex)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Bits (testBit, (.&.), (.|.), shiftL, shiftR)
import Data.Word (Word8, Word16)

-- | ANSI 颜色码
colRed, colGreen, colYellow, colBlue, colMagenta, colCyan, colReset :: String
colRed     = "\ESC[31m"
colGreen   = "\ESC[32m"
colYellow  = "\ESC[33m"
colBlue    = "\ESC[34m"
colMagenta = "\ESC[35m"
colCyan    = "\ESC[36m"
colReset   = "\ESC[0m"

formatHex :: Word8 -> String
formatHex w = let s = showHex w "" in if length s == 1 then "0" ++ s else s

-- | 纯十六进制输出
hexDump :: ByteString -> String
hexDump bs = unwords $ map formatHex (BS.unpack bs)

-- | 多行结构化、带颜色的十六进制输出
analyzeHex :: ByteString -> String
analyzeHex bs
    | BS.length bs < 2 = hexDump bs
    | BS.index bs 0 /= 0x68 = hexDump bs
    | otherwise =
        let len = fromIntegral (BS.index bs 1) :: Int
            (frameBs, restBs) = BS.splitAt (len + 2) bs
            parsed = "\n" ++ parseSingleFrame frameBs
        in parsed ++ if BS.null restBs then "" else "\n" ++ analyzeHex restBs
  where
    parseSingleFrame fb
        | BS.length fb < 6 = "  [不完整帧]: " ++ hexDump fb
        | otherwise =
            let b0 = BS.index fb 0
                b1 = BS.index fb 1
                b2 = BS.index fb 2
                b3 = BS.index fb 3
                b4 = BS.index fb 4
                b5 = BS.index fb 5
                isIFrame = not (testBit b2 0)
                isSFrame = testBit b2 0 && not (testBit b2 1)
                isUFrame = testBit b2 0 && testBit b2 1

                apciStr = unlines
                    [ "  ┌── APCI ────────────────────────────────"
                    , "  │ 起始符   (Start) : " ++ colRed ++ formatHex b0 ++ colReset
                    , "  │ 帧长度   (Len)   : " ++ colGreen ++ formatHex b1 ++ colReset ++ " (" ++ show b1 ++ " 字节)"
                    , "  │ 控制域   (Ctrl)  : " ++ colYellow ++ formatHex b2 ++ " " ++ formatHex b3 ++ " " ++ formatHex b4 ++ " " ++ formatHex b5 ++ colReset ++ " " ++ ctrlDesc isIFrame isSFrame isUFrame b2 b3 b4 b5
                    ]
                
                asduStr = if isIFrame && BS.length fb >= 12 then
                    let b6 = BS.index fb 6
                        b7 = BS.index fb 7
                        b8 = BS.index fb 8
                        b9 = BS.index fb 9
                        b10 = BS.index fb 10
                        b11 = BS.index fb 11
                        restObj = BS.drop 12 fb
                        sq = if testBit b7 7 then 1 else 0 :: Int
                        num = b7 .&. 0x7F
                    in unlines
                        [ "  ├── ASDU ────────────────────────────────"
                        , "  │ 类型标识 (Type)  : " ++ colBlue ++ formatHex b6 ++ colReset ++ " (" ++ show b6 ++ ")"
                        , "  │ 可变结构 (SQ/Num): " ++ colMagenta ++ formatHex b7 ++ colReset ++ " (SQ=" ++ show sq ++ ", 对象数=" ++ show num ++ ")"
                        , "  │ 传送原因 (COT)   : " ++ colCyan ++ formatHex b8 ++ colReset ++ " (" ++ show (b8 .&. 0x3F) ++ ")"
                        , "  │ 源发地址 (ORG)   : " ++ colRed ++ formatHex b9 ++ colReset ++ " (" ++ show b9 ++ ")"
                        , "  │ 公共地址 (CA)    : " ++ colGreen ++ formatHex b10 ++ " " ++ formatHex b11 ++ colReset ++ " (" ++ show (fromIntegral b10 + (fromIntegral b11 * 256) :: Int) ++ ")"
                        , "  │ 信息对象 (Objs)  : " ++ if BS.null restObj then "无" else colReset ++ hexDump restObj
                        , "  └────────────────────────────────────────"
                        ]
                else if isIFrame && BS.length fb > 6 then
                    "  ├── ASDU (部分数据)\n  │ " ++ hexDump (BS.drop 6 fb) ++ "\n  └────────────────────────────────────────\n"
                else
                    "  └────────────────────────────────────────\n"
            in init (apciStr ++ asduStr) -- 移除最后多余的换行符

    ctrlDesc i s u b2 b3 b4 b5
        | i =
            let ssn = (fromIntegral b2 .|. (fromIntegral b3 `shiftL` 8 :: Word16)) `shiftR` 1
                rsn = (fromIntegral b4 .|. (fromIntegral b5 `shiftL` 8 :: Word16)) `shiftR` 1
            in "-> I 帧 (SSN=" ++ show ssn ++ ", RSN=" ++ show rsn ++ ")"
        | s =
            let rsn = (fromIntegral b4 .|. (fromIntegral b5 `shiftL` 8 :: Word16)) `shiftR` 1
            in "-> S 帧 (RSN=" ++ show rsn ++ ")"
        | u = "-> U 帧 " ++ case b2 of
                0x07 -> "(STARTDT Act)"
                0x0B -> "(STARTDT Con)"
                0x13 -> "(STOPDT Act)"
                0x23 -> "(STOPDT Con)"
                0x43 -> "(TESTFR Act)"
                0x83 -> "(TESTFR Con)"
                _    -> ""
        | otherwise = "-> 未知帧类型"
