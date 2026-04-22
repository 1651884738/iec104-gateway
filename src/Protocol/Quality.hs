-- | IEC 60870-5-104 品质描述词
--
-- 品质位表示数据点的有效性和状态。
-- 适用于遥信和遥测类型。
--
module Protocol.Quality
    ( -- * 品质类型
      Quality(..)
    , QualityFlags(..)
      -- * 编解码
    , decodeQuality
    , encodeQuality
      -- * 查询
    , isValid
    , defaultQuality
    ) where

import Data.Bits ((.&.), (.|.), testBit, setBit, zeroBits)
import Data.Word (Word8)

-- | 品质标志位
data QualityFlags = QualityFlags
    { qOverflow    :: !Bool   -- ^ OV: 溢出（仅遥测）
    , qBlocked     :: !Bool   -- ^ BL: 被闭锁
    , qSubstituted :: !Bool   -- ^ SB: 被取代
    , qNotTopical  :: !Bool   -- ^ NT: 非当前值
    , qInvalid     :: !Bool   -- ^ IV: 无效
    } deriving (Show, Eq)

-- | 品质描述词（包含值和标志位）
data Quality
    = SIQ   !QualityFlags !Bool          -- ^ 单点信息品质 + SPI值
    | DIQ   !QualityFlags !Int           -- ^ 双点信息品质 + DPI值(0-3)
    | QDS   !QualityFlags                -- ^ 遥测品质描述词
    deriving (Show, Eq)

-- | 默认品质（全部正常）
defaultQuality :: QualityFlags
defaultQuality = QualityFlags
    { qOverflow    = False
    , qBlocked     = False
    , qSubstituted = False
    , qNotTopical  = False
    , qInvalid     = False
    }

-- | 数据是否有效（IV 位为 0）
isValid :: QualityFlags -> Bool
isValid = not . qInvalid

-- | 从 Word8 解码品质标志位
decodeQuality :: Word8 -> QualityFlags
decodeQuality w = QualityFlags
    { qOverflow    = testBit w 0    -- bit0: OV
    , qBlocked     = testBit w 4    -- bit4: BL
    , qSubstituted = testBit w 5    -- bit5: SB
    , qNotTopical  = testBit w 6    -- bit6: NT
    , qInvalid     = testBit w 7    -- bit7: IV
    }

-- | 编码品质标志位为 Word8
encodeQuality :: QualityFlags -> Word8
encodeQuality q = foldl setBitIf zeroBits conditions
  where
    conditions =
        [ (0, qOverflow q)
        , (4, qBlocked q)
        , (5, qSubstituted q)
        , (6, qNotTopical q)
        , (7, qInvalid q)
        ]
    setBitIf acc (bit, True)  = setBit acc bit
    setBitIf acc (_, False)   = acc
