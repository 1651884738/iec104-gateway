-- | IEC 104 Client 端 — 子站管理器
--
-- 管理多个子站连接：每个子站一个连接线程，
-- 统一管理连接状态和数据流。
--
module Client.Station
    ( -- * 子站信息
      StationInfo(..)
    , StationStatus(..)
      -- * 管理器
    , StationManager
    , newStationManager
    , addStation
    , removeStation
    , getStationStatus
    , getAllStations
    ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word16)

import Client.Connector (ConnectorConfig(..))
import Connection.StateMachine (ConnState(..))

-- | 子站状态
data StationStatus
    = StationDisconnected   -- ^ 未连接
    | StationConnecting     -- ^ 连接中
    | StationConnected      -- ^ 已连接
    | StationActive         -- ^ 数据传输中
    | StationError String   -- ^ 错误
    deriving (Show, Eq)

-- | 子站信息
data StationInfo = StationInfo
    { siName       :: !String              -- ^ 子站名称
    , siConfig     :: !ConnectorConfig     -- ^ 连接配置
    , siStatus     :: !StationStatus       -- ^ 当前状态
    , siCommonAddr :: !Word16              -- ^ 公共地址
    } deriving (Show)

-- | 子站管理器
newtype StationManager = StationManager
    { smStations :: IORef (Map String StationInfo)
    }

-- | 创建子站管理器
newStationManager :: IO StationManager
newStationManager = do
    ref <- newIORef Map.empty
    return (StationManager ref)

-- | 添加子站
addStation :: StationManager -> StationInfo -> IO ()
addStation mgr info =
    modifyIORef' (smStations mgr) (Map.insert (siName info) info)

-- | 移除子站
removeStation :: StationManager -> String -> IO ()
removeStation mgr name =
    modifyIORef' (smStations mgr) (Map.delete name)

-- | 获取子站状态
getStationStatus :: StationManager -> String -> IO (Maybe StationStatus)
getStationStatus mgr name = do
    stations <- readIORef (smStations mgr)
    return $ siStatus <$> Map.lookup name stations

-- | 获取所有子站
getAllStations :: StationManager -> IO [StationInfo]
getAllStations mgr = do
    stations <- readIORef (smStations mgr)
    return (Map.elems stations)
