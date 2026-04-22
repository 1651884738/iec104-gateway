-- | IEC 60870-5-104 APCI (Application Protocol Control Information)
-- 
-- APCI 是 104 协议的传输层帧头，所有帧都以 0x68 起始。
-- 三种帧格式：I 帧（信息传输）、S 帧（监视确认）、U 帧（控制）
--
-- 帧结构：
-- +------+--------+-------------------+
-- | 0x68 | Length | 4-byte Control    |
-- +------+--------+-------------------+
--   1B      1B       4B (+ ASDU for I-frame)
--
module Protocol.APCI
    ( -- * 帧类型
      APCIFrame(..)
    , UFrameType(..)
      -- * 编码
    , encodeFrame
      -- * 解码
    , decodeFrame
      -- * 常量
    , startByte
    , maxFrameLength
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16)

-- | 起始字节
startByte :: Word8
startByte = 0x68

-- | 最大帧长度（APDU = APCI + ASDU，最大 253 字节）
maxFrameLength :: Int
maxFrameLength = 253

-- | U 帧类型
data UFrameType
    = StartDTAct    -- ^ STARTDT 激活
    | StartDTCon    -- ^ STARTDT 确认
    | StopDTAct     -- ^ STOPDT 激活
    | StopDTCon     -- ^ STOPDT 确认
    | TestFRAct     -- ^ TESTFR 激活
    | TestFRCon     -- ^ TESTFR 确认
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | APCI 帧
data APCIFrame
    = IFrame
        { iSendSeq :: !Word16       -- ^ 发送序号 V(S)，0-32767
        , iRecvSeq :: !Word16       -- ^ 接收序号 V(R)，0-32767
        , iPayload :: !ByteString   -- ^ ASDU 载荷
        }
    | SFrame
        { sRecvSeq :: !Word16       -- ^ 接收序号 V(R)
        }
    | UFrame
        { uType :: !UFrameType      -- ^ U 帧类型
        }
    deriving (Show, Eq)

-- ============================================================
-- 编码
-- ============================================================

-- | 将 APCIFrame 编码为 ByteString（包含起始字节和长度）
encodeFrame :: APCIFrame -> ByteString
encodeFrame frame = BS.cons startByte $ BS.cons len controlAndPayload
  where
    controlAndPayload = encodeControl frame
    len = fromIntegral (BS.length controlAndPayload)

-- | 编码控制域 + 可能的 ASDU 载荷
encodeControl :: APCIFrame -> ByteString
encodeControl (IFrame ssn rsn payload) =
    -- I 帧控制域：4 字节
    -- byte1: SSN 低 7 位 左移1位, bit0=0 (I帧标识)
    -- byte2: SSN 高 8 位
    -- byte3: RSN 低 7 位 左移1位
    -- byte4: RSN 高 8 位
    let ssnShifted = ssn `shiftL` 1     -- bit0 = 0 表示 I 帧
        rsnShifted = rsn `shiftL` 1
        b1 = fromIntegral (ssnShifted .&. 0xFF) :: Word8
        b2 = fromIntegral (ssnShifted `shiftR` 8) :: Word8
        b3 = fromIntegral (rsnShifted .&. 0xFF) :: Word8
        b4 = fromIntegral (rsnShifted `shiftR` 8) :: Word8
    in BS.pack [b1, b2, b3, b4] <> payload

encodeControl (SFrame rsn) =
    -- S 帧控制域：4 字节
    -- byte1: 0x01 (bit0=1, bit1=0 表示 S 帧)
    -- byte2: 0x00
    -- byte3: RSN 低 7 位 左移1位
    -- byte4: RSN 高 8 位
    let rsnShifted = rsn `shiftL` 1
        b3 = fromIntegral (rsnShifted .&. 0xFF) :: Word8
        b4 = fromIntegral (rsnShifted `shiftR` 8) :: Word8
    in BS.pack [0x01, 0x00, b3, b4]

encodeControl (UFrame utype) =
    -- U 帧控制域：4 字节
    -- byte1: bit0=1, bit1=1 (U帧标识), bit2-7 表示具体类型
    -- byte2-4: 0x00
    let b1 = 0x03 .|. uTypeBits utype :: Word8
    in BS.pack [b1, 0x00, 0x00, 0x00]

-- | U 帧类型对应的控制域 bit 位
uTypeBits :: UFrameType -> Word8
uTypeBits StartDTAct = 0x04    -- bit2
uTypeBits StartDTCon = 0x08    -- bit3
uTypeBits StopDTAct  = 0x10    -- bit4
uTypeBits StopDTCon  = 0x20    -- bit5
uTypeBits TestFRAct  = 0x40    -- bit6
uTypeBits TestFRCon  = 0x80    -- bit7

-- ============================================================
-- 解码
-- ============================================================

-- | 解析错误类型
data DecodeError
    = InvalidStartByte Word8
    | FrameTooShort Int
    | InvalidControlField Word8
    | InvalidUFrameType Word8
    deriving (Show, Eq)

-- | 从 ByteString 解码一个 APCI 帧
-- 输入应包含完整的帧（起始字节 + 长度 + 控制域 + 载荷）
decodeFrame :: ByteString -> Either DecodeError APCIFrame
decodeFrame bs
    | BS.length bs < 6 = Left (FrameTooShort (BS.length bs))
    | BS.index bs 0 /= startByte = Left (InvalidStartByte (BS.index bs 0))
    | otherwise =
        let _len = BS.index bs 1
            b1   = BS.index bs 2    -- 控制域第 1 字节
            b2   = BS.index bs 3    -- 控制域第 2 字节
            b3   = BS.index bs 4    -- 控制域第 3 字节
            b4   = BS.index bs 5    -- 控制域第 4 字节
        in decodeControl b1 b2 b3 b4 (BS.drop 6 bs)

-- | 根据控制域字节判断帧类型并解析
decodeControl :: Word8 -> Word8 -> Word8 -> Word8 -> ByteString -> Either DecodeError APCIFrame
decodeControl b1 b2 b3 b4 payload
    -- I 帧：bit0 = 0
    | not (testBit b1 0) =
        let ssn = (fromIntegral b1 .|. (fromIntegral b2 `shiftL` 8)) `shiftR` 1 :: Word16
            rsn = (fromIntegral b3 .|. (fromIntegral b4 `shiftL` 8)) `shiftR` 1 :: Word16
        in Right $ IFrame ssn rsn payload
    -- S 帧：bit0 = 1, bit1 = 0
    | testBit b1 0 && not (testBit b1 1) =
        let rsn = (fromIntegral b3 .|. (fromIntegral b4 `shiftL` 8)) `shiftR` 1 :: Word16
        in Right $ SFrame rsn
    -- U 帧：bit0 = 1, bit1 = 1
    | testBit b1 0 && testBit b1 1 =
        case decodeUType b1 of
            Just ut -> Right $ UFrame ut
            Nothing -> Left $ InvalidUFrameType b1
    | otherwise = Left $ InvalidControlField b1

-- | 从控制域第 1 字节解析 U 帧类型
decodeUType :: Word8 -> Maybe UFrameType
decodeUType b
    | testBit b 2 = Just StartDTAct
    | testBit b 3 = Just StartDTCon
    | testBit b 4 = Just StopDTAct
    | testBit b 5 = Just StopDTCon
    | testBit b 6 = Just TestFRAct
    | testBit b 7 = Just TestFRCon
    | otherwise   = Nothing
