-- | IEC 104 连接状态机
--
-- 管理 IEC 104 连接的完整生命周期：
-- TCP 连接 → STARTDT 握手 → 数据传输 → STOPDT → 关闭
--
module Connection.StateMachine
    ( -- * 状态类型
      ConnState(..)
    , ConnEvent(..)
      -- * 状态转换
    , transition
    , isDataTransferState
    , isTerminalState
    ) where

-- | 连接状态
data ConnState
    = Idle                -- ^ 空闲，未连接
    | TCPConnecting       -- ^ TCP 连接建立中
    | TCPConnected        -- ^ TCP 已连接，待 STARTDT
    | WaitStartDTCon      -- ^ 已发送 STARTDT Act，等待 Con
    | DataTransfer        -- ^ 数据传输状态（正常工作）
    | WaitStopDTCon       -- ^ 已发送 STOPDT Act，等待 Con
    | Stopped             -- ^ 已停止数据传输
    | Failed              -- ^ 连接失败
    | Closed              -- ^ 连接已关闭
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | 连接事件
data ConnEvent
    = EvTCPConnectOK      -- ^ TCP 连接建立成功
    | EvTCPConnectFail    -- ^ TCP 连接建立失败
    | EvSendStartDT       -- ^ 发送 STARTDT Act
    | EvRecvStartDTCon    -- ^ 收到 STARTDT Con
    | EvRecvStartDTAct    -- ^ 收到 STARTDT Act（Server 侧）
    | EvSendStartDTCon    -- ^ 发送 STARTDT Con（Server 侧响应）
    | EvSendStopDT        -- ^ 发送 STOPDT Act
    | EvRecvStopDTCon     -- ^ 收到 STOPDT Con
    | EvRecvStopDTAct     -- ^ 收到 STOPDT Act（Server 侧）
    | EvSendStopDTCon     -- ^ 发送 STOPDT Con（Server 侧响应）
    | EvTimeout           -- ^ 超时事件
    | EvError             -- ^ 错误事件
    | EvClose             -- ^ 关闭连接
    deriving (Show, Eq, Ord)

-- | 状态转换函数
-- 返回 Nothing 表示该事件在当前状态下无效
transition :: ConnState -> ConnEvent -> Maybe ConnState
-- TCP 连接阶段
transition Idle            EvTCPConnectOK   = Just TCPConnected
transition Idle            EvTCPConnectFail = Just Failed
transition TCPConnecting   EvTCPConnectOK   = Just TCPConnected
transition TCPConnecting   EvTCPConnectFail = Just Failed
transition TCPConnecting   EvTimeout        = Just Failed

-- Client 侧 STARTDT 握手
transition TCPConnected    EvSendStartDT    = Just WaitStartDTCon
transition WaitStartDTCon  EvRecvStartDTCon = Just DataTransfer
transition WaitStartDTCon  EvTimeout        = Just Failed

-- Server 侧 STARTDT 握手
transition TCPConnected    EvRecvStartDTAct = Just TCPConnected  -- 准备发送 Con
transition TCPConnected    EvSendStartDTCon = Just DataTransfer

-- 数据传输状态
transition DataTransfer    EvSendStopDT     = Just WaitStopDTCon
transition DataTransfer    EvRecvStopDTAct  = Just DataTransfer  -- 准备发送 Con
transition DataTransfer    EvSendStopDTCon  = Just Stopped
transition DataTransfer    EvTimeout        = Just Failed
transition DataTransfer    EvError          = Just Failed

-- STOPDT 阶段
transition WaitStopDTCon   EvRecvStopDTCon  = Just Stopped
transition WaitStopDTCon   EvTimeout        = Just Failed

-- 停止后
transition Stopped         EvClose          = Just Closed

-- 通用：任何状态都可以因错误或关闭而终止
transition _               EvError          = Just Failed
transition _               EvClose          = Just Closed

-- 无效转换
transition _               _                = Nothing

-- | 是否处于数据传输状态
isDataTransferState :: ConnState -> Bool
isDataTransferState DataTransfer = True
isDataTransferState _            = False

-- | 是否处于终态
isTerminalState :: ConnState -> Bool
isTerminalState Failed = True
isTerminalState Closed = True
isTerminalState _      = False
