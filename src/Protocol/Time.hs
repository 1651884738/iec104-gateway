-- | IEC 60870-5-104 CP56Time2a 时标
--
-- CP56Time2a 是 IEC 104 中使用的 7 字节时间戳格式。
-- 字节顺序: ms_lo ms_hi min hour day month year
--
module Protocol.Time
    ( -- * 时标类型
      CP56Time2a(..)
      -- * 编解码
    , decodeCP56Time2a
    , encodeCP56Time2a
      -- * 工具函数
    , cp56TimeToString
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16)

-- | CP56Time2a 七字节时标
data CP56Time2a = CP56Time2a
    { cpMillisecond :: !Word16   -- ^ 毫秒 (0-59999)
    , cpMinute      :: !Word8    -- ^ 分钟 (0-59)
    , cpHour        :: !Word8    -- ^ 小时 (0-23)
    , cpDay         :: !Word8    -- ^ 日 (1-31)
    , cpMonth       :: !Word8    -- ^ 月 (1-12)
    , cpYear        :: !Word8    -- ^ 年 (0-99, 相对于 2000)
    , cpInvalid     :: !Bool     -- ^ IV 位（分钟字节 bit7）
    , cpSummerTime  :: !Bool     -- ^ SU 位（小时字节 bit7，夏令时）
    } deriving (Show, Eq)

-- | 从 7 字节 ByteString 解码 CP56Time2a
-- 字节顺序: [ms_lo, ms_hi, min, hour, day, month, year]
decodeCP56Time2a :: ByteString -> Maybe CP56Time2a
decodeCP56Time2a bs
    | BS.length bs < 7 = Nothing
    | otherwise = Just CP56Time2a
        { cpMillisecond = fromIntegral (BS.index bs 0)
                        .|. (fromIntegral (BS.index bs 1) `shiftL` 8)
        , cpMinute      = BS.index bs 2 .&. 0x3F
        , cpHour        = BS.index bs 3 .&. 0x1F
        , cpDay         = BS.index bs 4 .&. 0x1F
        , cpMonth       = BS.index bs 5 .&. 0x0F
        , cpYear        = BS.index bs 6 .&. 0x7F
        , cpInvalid     = BS.index bs 2 .&. 0x80 /= 0
        , cpSummerTime  = BS.index bs 3 .&. 0x80 /= 0
        }

-- | 编码 CP56Time2a 为 7 字节 ByteString
encodeCP56Time2a :: CP56Time2a -> ByteString
encodeCP56Time2a t = BS.pack
    [ fromIntegral (cpMillisecond t .&. 0xFF)                          -- ms 低字节
    , fromIntegral ((cpMillisecond t `shiftR` 8) .&. 0xFF)             -- ms 高字节
    , (cpMinute t .&. 0x3F) .|. (if cpInvalid t then 0x80 else 0x00)   -- 分钟 + IV
    , (cpHour t .&. 0x1F) .|. (if cpSummerTime t then 0x80 else 0x00)  -- 小时 + SU
    , cpDay t .&. 0x1F                                                  -- 日
    , cpMonth t .&. 0x0F                                                -- 月
    , cpYear t .&. 0x7F                                                 -- 年
    ]

-- | 格式化时标为可读字符串
cp56TimeToString :: CP56Time2a -> String
cp56TimeToString t =
    let yr  = 2000 + fromIntegral (cpYear t) :: Int
        mon = fromIntegral (cpMonth t) :: Int
        dy  = fromIntegral (cpDay t) :: Int
        hr  = fromIntegral (cpHour t) :: Int
        mn  = fromIntegral (cpMinute t) :: Int
        sec = fromIntegral (cpMillisecond t) `div` 1000 :: Int
        ms  = fromIntegral (cpMillisecond t) `mod` 1000 :: Int
    in pad4 yr ++ "-" ++ pad2 mon ++ "-" ++ pad2 dy
       ++ " " ++ pad2 hr ++ ":" ++ pad2 mn ++ ":" ++ pad2 sec
       ++ "." ++ pad3 ms
  where
    pad2 n = if n < 10 then "0" ++ show n else show n
    pad3 n
        | n < 10    = "00" ++ show n
        | n < 100   = "0" ++ show n
        | otherwise = show n
    pad4 n
        | n < 10    = "000" ++ show n
        | n < 100   = "00" ++ show n
        | n < 1000  = "0" ++ show n
        | otherwise = show n
