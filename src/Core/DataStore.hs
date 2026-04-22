-- | 实时数据库
--
-- 存储所有数据点的当前值。
-- 使用 STM (TVar) 实现无锁并发读写。
--
module Core.DataStore
    ( -- * 数据库类型
      DataStore
      -- * 创建与操作
    , newDataStore
    , getPoint
    , putPoint
    , getAllPoints
    , getPointsBySource
      -- * 变化检测
    , putPointWithChange
    , ChangeEvent(..)
    ) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word32)

import Core.DataPoint (DataPoint(..), DataValue(..))

-- | 变化事件
data ChangeEvent = ChangeEvent
    { ceIOA      :: !Word32     -- ^ 变化的数据点地址
    , ceOldValue :: !DataValue  -- ^ 旧值
    , ceNewValue :: !DataValue  -- ^ 新值
    , cePoint    :: !DataPoint  -- ^ 更新后的完整数据点
    } deriving (Show)

-- | 实时数据库
data DataStore = DataStore
    { dsPoints    :: !(TVar (Map Word32 DataPoint))  -- ^ 数据点映射
    , dsChanges   :: !(TQueue ChangeEvent)           -- ^ 变化事件队列
    }

-- | 创建新的实时数据库
newDataStore :: IO DataStore
newDataStore = atomically $ do
    points  <- newTVar Map.empty
    changes <- newTQueue
    return DataStore
        { dsPoints  = points
        , dsChanges = changes
        }

-- | 获取数据点
getPoint :: DataStore -> Word32 -> IO (Maybe DataPoint)
getPoint ds ioa = atomically $ do
    points <- readTVar (dsPoints ds)
    return (Map.lookup ioa points)

-- | 写入数据点（不检测变化）
putPoint :: DataStore -> DataPoint -> IO ()
putPoint ds dp = atomically $
    modifyTVar' (dsPoints ds) (Map.insert (dpIOA dp) dp)

-- | 写入数据点并检测变化
-- 如果值发生变化，产生 ChangeEvent 到队列
putPointWithChange :: DataStore -> DataPoint -> IO (Maybe ChangeEvent)
putPointWithChange ds dp = atomically $ do
    points <- readTVar (dsPoints ds)
    let ioa = dpIOA dp
        oldPoint = Map.lookup ioa points
        oldVal = maybe NoValue dpValue oldPoint
        newVal = dpValue dp
    writeTVar (dsPoints ds) (Map.insert ioa dp points)
    if oldVal /= newVal
        then do
            let event = ChangeEvent
                    { ceIOA      = ioa
                    , ceOldValue = oldVal
                    , ceNewValue = newVal
                    , cePoint    = dp
                    }
            writeTQueue (dsChanges ds) event
            return (Just event)
        else return Nothing

-- | 获取所有数据点
getAllPoints :: DataStore -> IO (Map Word32 DataPoint)
getAllPoints ds = atomically $ readTVar (dsPoints ds)

-- | 按来源获取数据点
getPointsBySource :: DataStore -> String -> IO [DataPoint]
getPointsBySource ds source = do
    points <- atomically $ readTVar (dsPoints ds)
    return $ filter (\dp -> dpSource dp == source) (Map.elems points)
