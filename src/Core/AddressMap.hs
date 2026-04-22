-- | 地址映射
--
-- 管理子站 IOA → 网关 IOA 的地址映射规则。
-- 将不同子站的地址空间映射到统一的网关地址空间。
--
module Core.AddressMap
    ( -- * 映射规则
      AddressMapping(..)
    , AddressMap
      -- * 创建与操作
    , newAddressMap
    , addMapping
    , lookupForward
    , lookupReverse
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

-- | 单条地址映射规则
data AddressMapping = AddressMapping
    { amStation    :: !String    -- ^ 子站名称
    , amSrcStart   :: !Word32   -- ^ 子站起始 IOA
    , amSrcCount   :: !Int      -- ^ 连续地址数量
    , amDstStart   :: !Word32   -- ^ 网关起始 IOA
    } deriving (Show, Eq)

-- | 地址映射表
data AddressMap = AddressMap
    { amMappings :: ![AddressMapping]                -- ^ 映射规则列表
    , amForward  :: !(Map (String, Word32) Word32)    -- ^ 正向映射缓存: (子站, 子站IOA) → 网关IOA
    , amReverse  :: !(Map Word32 (String, Word32))    -- ^ 反向映射缓存: 网关IOA → (子站, 子站IOA)
    } deriving (Show)

-- | 创建空的地址映射表
newAddressMap :: AddressMap
newAddressMap = AddressMap
    { amMappings = []
    , amForward  = Map.empty
    , amReverse  = Map.empty
    }

-- | 添加映射规则（同时构建正/反向缓存）
addMapping :: AddressMapping -> AddressMap -> AddressMap
addMapping mapping am =
    let pairs = [ (i, fromIntegral i - amSrcStart mapping + amDstStart mapping)
                | i <- [amSrcStart mapping .. amSrcStart mapping + fromIntegral (amSrcCount mapping) - 1]
                ]
        station = amStation mapping
        fwd = foldl (\m (src, dst) -> Map.insert (station, src) dst m) (amForward am) pairs
        rev = foldl (\m (src, dst) -> Map.insert dst (station, src) m) (amReverse am) pairs
    in am
        { amMappings = mapping : amMappings am
        , amForward  = fwd
        , amReverse  = rev
        }

-- | 正向查找: (子站名, 子站IOA) → 网关IOA
lookupForward :: AddressMap -> String -> Word32 -> Maybe Word32
lookupForward am station srcIOA = Map.lookup (station, srcIOA) (amForward am)

-- | 反向查找: 网关IOA → (子站名, 子站IOA)
lookupReverse :: AddressMap -> Word32 -> Maybe (String, Word32)
lookupReverse am dstIOA = Map.lookup dstIOA (amReverse am)
