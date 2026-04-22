-- | 数据点模型
--
-- 统一抽象遥信、遥测、遥控、电度量等数据类型。
-- 所有子站数据和主站数据都使用此模型。
--
module Core.DataPoint
    ( -- * 数据点类型
      DataPoint(..)
    , DataValue(..)
    , DataDirection(..)
      -- * 工具函数
    , mkDataPoint
    , updateValue
    , isStale
    ) where

import Data.Time.Clock (UTCTime, NominalDiffTime, diffUTCTime)
import Data.Word (Word32)
import Protocol.Quality (QualityFlags, defaultQuality)

-- | 数据方向
data DataDirection
    = Monitor    -- ^ 监视方向（子站 → 主站）
    | Control    -- ^ 控制方向（主站 → 子站）
    deriving (Show, Eq, Ord)

-- | 数据值
data DataValue
    = BoolValue   !Bool      -- ^ 布尔值（遥信）
    | IntValue    !Int       -- ^ 整数值（双点遥信、电度量）
    | FloatValue  !Float     -- ^ 浮点值（遥测）
    | DoubleValue !Double    -- ^ 双精度值（归一化遥测）
    | NoValue                -- ^ 无值（初始状态）
    deriving (Show, Eq)

-- | 数据点
data DataPoint = DataPoint
    { dpIOA        :: !Word32         -- ^ 信息对象地址
    , dpValue      :: !DataValue      -- ^ 当前值
    , dpQuality    :: !QualityFlags   -- ^ 品质标志
    , dpTimestamp  :: !(Maybe UTCTime) -- ^ 最后更新时间
    , dpDirection  :: !DataDirection   -- ^ 数据方向
    , dpSource     :: !String         -- ^ 数据来源（子站名称）
    , dpDesc       :: !String         -- ^ 描述信息
    } deriving (Show, Eq)

-- | 创建新数据点
mkDataPoint :: Word32 -> DataDirection -> String -> DataPoint
mkDataPoint ioa dir source = DataPoint
    { dpIOA       = ioa
    , dpValue     = NoValue
    , dpQuality   = defaultQuality
    , dpTimestamp  = Nothing
    , dpDirection = dir
    , dpSource    = source
    , dpDesc      = ""
    }

-- | 更新数据点的值
updateValue :: DataValue -> QualityFlags -> UTCTime -> DataPoint -> DataPoint
updateValue val qual ts dp = dp
    { dpValue     = val
    , dpQuality   = qual
    , dpTimestamp  = Just ts
    }

-- | 检查数据是否过期（超过指定秒数未更新）
isStale :: NominalDiffTime -> UTCTime -> DataPoint -> Bool
isStale maxAge now dp = case dpTimestamp dp of
    Nothing -> True  -- 从未更新过
    Just ts -> diffUTCTime now ts > maxAge
