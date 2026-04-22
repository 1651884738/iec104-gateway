-- | IEC 104 Server 端 — 会话管理
--
-- 管理与主站的单个会话：STARTDT 握手、数据传输、状态维护。
--
module Server.Session
    ( -- * 会话
      Session(..)
    , SessionState(..)
    , newSession
      -- * 会话管理
    , SessionManager
    , newSessionManager
    , addSession
    , removeSession
    , getActiveSessions
    ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word16)
import Network.Socket (Socket, SockAddr)

import Connection.Config (ConnectionConfig(..))
import Connection.SeqNum (SeqManager, newSeqManager)
import Connection.StateMachine (ConnState(..))

-- | 会话 ID
type SessionID = Int

-- | 会话状态
data SessionState = SessionState
    { ssConnState  :: !ConnState     -- ^ 连接状态
    , ssSeqManager :: !SeqManager    -- ^ 序号管理器
    } deriving (Show)

-- | 单个主站会话
data Session = Session
    { sessionID     :: !SessionID          -- ^ 会话 ID
    , sessionSocket :: !Socket             -- ^ 客户端 socket
    , sessionAddr   :: !SockAddr           -- ^ 客户端地址
    , sessionState  :: !(IORef SessionState)  -- ^ 会话状态
    , sessionConfig :: !ConnectionConfig   -- ^ 连接配置
    }

-- | 创建新会话
newSession :: SessionID -> Socket -> SockAddr -> ConnectionConfig -> IO Session
newSession sid sock addr cfg = do
    let initState = SessionState
            { ssConnState  = TCPConnected
            , ssSeqManager = newSeqManager (cfgK cfg) (cfgW cfg)
            }
    stateRef <- newIORef initState
    return Session
        { sessionID     = sid
        , sessionSocket = sock
        , sessionAddr   = addr
        , sessionState  = stateRef
        , sessionConfig = cfg
        }

-- | 会话管理器
data SessionManager = SessionManager
    { smSessions  :: !(IORef (Map SessionID Session))
    , smNextID    :: !(IORef SessionID)
    }

-- | 创建新的会话管理器
newSessionManager :: IO SessionManager
newSessionManager = do
    sessionsRef <- newIORef Map.empty
    nextIDRef   <- newIORef 1
    return SessionManager
        { smSessions = sessionsRef
        , smNextID   = nextIDRef
        }

-- | 添加会话
addSession :: SessionManager -> Session -> IO ()
addSession mgr session =
    modifyIORef' (smSessions mgr) (Map.insert (sessionID session) session)

-- | 移除会话
removeSession :: SessionManager -> SessionID -> IO ()
removeSession mgr sid =
    modifyIORef' (smSessions mgr) (Map.delete sid)

-- | 获取所有活跃会话
getActiveSessions :: SessionManager -> IO [Session]
getActiveSessions mgr = do
    sessions <- readIORef (smSessions mgr)
    return (Map.elems sessions)
