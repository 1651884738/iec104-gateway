-- | IEC 104 超时管理
--
-- 管理 t0/t1/t2/t3 四个定时器。
-- 使用 IORef + 时间戳方式实现，避免创建大量系统定时器。
--
module Connection.Timeout
    ( -- * 超时管理器
      TimeoutManager(..)
    , newTimeoutManager
      -- * 超时操作
    , resetT1
    , resetT2
    , resetT3
    , clearT1
    , clearT2
      -- * 超时检查
    , checkTimeouts
    , TimeoutEvent(..)
    ) where

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime, addUTCTime)
import Connection.Config (ConnectionConfig(..))

-- | 超时事件
data TimeoutEvent
    = T1Expired   -- ^ t1 超时：发送帧未得到确认
    | T2Expired   -- ^ t2 超时：需要发送 S 帧确认
    | T3Expired   -- ^ t3 超时：需要发送 TESTFR
    deriving (Show, Eq, Ord)

-- | 超时管理器
data TimeoutManager = TimeoutManager
    { tmT1Deadline :: !(Maybe UTCTime)  -- ^ t1 超时截止时间
    , tmT2Deadline :: !(Maybe UTCTime)  -- ^ t2 超时截止时间
    , tmT3Deadline :: !(Maybe UTCTime)  -- ^ t3 超时截止时间
    , tmConfig     :: !ConnectionConfig -- ^ 超时配置
    } deriving (Show)

-- | 创建新的超时管理器
newTimeoutManager :: ConnectionConfig -> IO TimeoutManager
newTimeoutManager cfg = do
    now <- getCurrentTime
    return TimeoutManager
        { tmT1Deadline = Nothing
        , tmT2Deadline = Nothing
        , tmT3Deadline = Just (addSeconds (cfgT3 cfg) now)
        , tmConfig     = cfg
        }

-- | 重置 t1 定时器（发送 I 帧后启动）
resetT1 :: TimeoutManager -> IO TimeoutManager
resetT1 tm = do
    now <- getCurrentTime
    return tm { tmT1Deadline = Just (addSeconds (cfgT1 (tmConfig tm)) now) }

-- | 重置 t2 定时器（收到 I 帧后启动）
resetT2 :: TimeoutManager -> IO TimeoutManager
resetT2 tm = do
    now <- getCurrentTime
    return tm { tmT2Deadline = Just (addSeconds (cfgT2 (tmConfig tm)) now) }

-- | 重置 t3 定时器（收到/发送任何帧后重启）
resetT3 :: TimeoutManager -> IO TimeoutManager
resetT3 tm = do
    now <- getCurrentTime
    return tm { tmT3Deadline = Just (addSeconds (cfgT3 (tmConfig tm)) now) }

-- | 清除 t1 定时器（收到确认后）
clearT1 :: TimeoutManager -> TimeoutManager
clearT1 tm = tm { tmT1Deadline = Nothing }

-- | 清除 t2 定时器（发送 S 帧后）
clearT2 :: TimeoutManager -> TimeoutManager
clearT2 tm = tm { tmT2Deadline = Nothing }

-- | 检查是否有超时事件发生
-- 返回所有已超时的事件列表
checkTimeouts :: TimeoutManager -> IO [TimeoutEvent]
checkTimeouts tm = do
    now <- getCurrentTime
    let events = concat
            [ [T1Expired | Just d <- [tmT1Deadline tm], now >= d]
            , [T2Expired | Just d <- [tmT2Deadline tm], now >= d]
            , [T3Expired | Just d <- [tmT3Deadline tm], now >= d]
            ]
    return events

-- | 辅助：在时间上加秒数
addSeconds :: Int -> UTCTime -> UTCTime
addSeconds secs t =
    let diff = fromIntegral secs :: NominalDiffTime
    in addUTCTime diff t
