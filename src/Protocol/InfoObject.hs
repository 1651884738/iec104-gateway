-- | IEC 60870-5-104 信息对象
--
-- 信息对象包含：信息对象地址 (IOA, 3字节) + 信息元素（取决于 TypeID）
--
module Protocol.InfoObject
    ( -- * 信息对象
      InfoObject(..)
    , IOA
      -- * 信息元素值
    , InfoValue(..)
      -- * IOA 编解码
    , decodeIOA
    , encodeIOA
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word32)

import Protocol.Quality (QualityFlags)
import Protocol.Time (CP56Time2a)

-- | 信息对象地址 (3 字节, 0-16777215)
type IOA = Word32

-- | 信息元素值（根据 TypeID 不同而不同）
data InfoValue
    = SinglePoint !Bool !QualityFlags                         -- ^ 单点遥信 (M_SP_NA/M_SP_TB)
    | DoublePoint !Int !QualityFlags                          -- ^ 双点遥信 (M_DP_NA/M_DP_TB), 值 0-3
    | NormalizedValue !Double !QualityFlags                   -- ^ 归一化遥测 (M_ME_NA), -1.0 ~ +1.0
    | ShortFloat !Float !QualityFlags                         -- ^ 短浮点遥测 (M_ME_NC/M_ME_TF)
    | SingleCommand !Bool !Bool                               -- ^ 单点遥控 (C_SC_NA), 值+选择/执行
    | DoubleCommand !Int !Bool                                -- ^ 双点遥控 (C_DC_NA), 值(1/2)+选择/执行
    | InterrogationCmd !Word8                                 -- ^ 站召唤 (C_IC_NA), QOI
    | CounterInterrogationCmd !Word8                          -- ^ 电度总召 (C_CI_NA), QCC
    | ClockSyncCmd !CP56Time2a                                -- ^ 时钟同步 (C_CS_NA)
    deriving (Show, Eq)

-- | 信息对象 = IOA + 值 + 可选时标
data InfoObject = InfoObject
    { ioIOA       :: !IOA                -- ^ 信息对象地址
    , ioValue     :: !InfoValue          -- ^ 信息元素值
    , ioTimestamp :: !(Maybe CP56Time2a) -- ^ 可选 CP56Time2a 时标
    } deriving (Show, Eq)

-- | 从 3 字节解码 IOA (低字节在前)
decodeIOA :: ByteString -> Maybe (IOA, ByteString)
decodeIOA bs
    | BS.length bs < 3 = Nothing
    | otherwise =
        let b0 = fromIntegral (BS.index bs 0) :: Word32
            b1 = fromIntegral (BS.index bs 1) :: Word32
            b2 = fromIntegral (BS.index bs 2) :: Word32
            ioa = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
        in Just (ioa, BS.drop 3 bs)

-- | 编码 IOA 为 3 字节 (低字节在前)
encodeIOA :: IOA -> ByteString
encodeIOA ioa = BS.pack
    [ fromIntegral (ioa .&. 0xFF)
    , fromIntegral ((ioa `shiftR` 8) .&. 0xFF)
    , fromIntegral ((ioa `shiftR` 16) .&. 0xFF)
    ]
