-- | 命令路由
--
-- 将主站下发的控制命令路由到正确的子站。
-- 基于地址映射表进行反向查找。
--
module Core.CommandRouter
    ( -- * 路由器
      CommandRouter
    , newCommandRouter
      -- * 命令
    , Command(..)
    , RouteResult(..)
    , routeCommand
    ) where

import Data.Word (Word8, Word32)
import Protocol.TypeID (TypeID)
import Protocol.ASDU (COT)
import Core.AddressMap (AddressMap, lookupReverse)

-- | 控制命令
data Command = Command
    { cmdTypeID     :: !TypeID    -- ^ 命令类型
    , cmdIOA        :: !Word32    -- ^ 信息对象地址（网关地址空间）
    , cmdCOT        :: !COT       -- ^ 传送原因
    , cmdPayload    :: !CommandPayload  -- ^ 命令载荷
    } deriving (Show)

-- | 命令载荷
data CommandPayload
    = SingleCmdPayload !Bool !Bool      -- ^ 单点遥控: 值 + 选择/执行
    | DoubleCmdPayload !Int !Bool       -- ^ 双点遥控: 值(1/2) + 选择/执行
    | InterrogationPayload !Word8       -- ^ 站召唤: QOI
    | CounterIntPayload !Word8          -- ^ 电度总召: QCC
    | GenericPayload                    -- ^ 通用（其他命令）
    deriving (Show)

-- | 路由结果
data RouteResult
    = RouteOK
        { rrStation    :: !String    -- ^ 目标子站名称
        , rrStationIOA :: !Word32    -- ^ 子站地址空间的 IOA
        , rrCommand    :: !Command   -- ^ 原始命令（IOA 已转换为子站地址）
        }
    | RouteNotFound !Word32          -- ^ 找不到路由（无此 IOA 的映射）
    | RouteBroadcast                 -- ^ 广播命令（如总召唤）
    deriving (Show)

-- | 命令路由器
newtype CommandRouter = CommandRouter
    { crAddressMap :: AddressMap
    }

-- | 创建命令路由器
newCommandRouter :: AddressMap -> CommandRouter
newCommandRouter = CommandRouter

-- | 路由命令到目标子站
routeCommand :: CommandRouter -> Command -> RouteResult
routeCommand router cmd =
    let ioa = cmdIOA cmd
    in case lookupReverse (crAddressMap router) ioa of
        Just (station, stationIOA) ->
            RouteOK
                { rrStation    = station
                , rrStationIOA = stationIOA
                , rrCommand    = cmd { cmdIOA = stationIOA }
                }
        Nothing ->
            RouteNotFound ioa
