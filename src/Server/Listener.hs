-- | IEC 104 Server 端 — TCP 监听器
--
-- 监听指定端口，接受主站连接。
-- 对主站扮演 受控站(Slave) 角色。
--
module Server.Listener
    ( -- * Server 启动
      startServer
    , ServerConfig(..)
    , ServerHandle(..)
    ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Exception (bracket, SomeException, catch)
import Control.Monad (forever, void)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Network.Socket
import System.IO (hFlush, stdout)

import Connection.Config (ConnectionConfig(..))

-- | Server 配置
data ServerConfig = ServerConfig
    { scListenPort     :: !Int              -- ^ 监听端口
    , scMaxConnections :: !Int              -- ^ 最大连接数
    , scConnConfig     :: !ConnectionConfig -- ^ 连接参数
    } deriving (Show)

-- | Server 句柄（用于外部管理）
data ServerHandle = ServerHandle
    { shListenSocket :: !Socket            -- ^ 监听 socket
    , shThreadId     :: !ThreadId          -- ^ 监听线程 ID
    , shActiveCount  :: !(IORef Int)       -- ^ 活跃连接数
    }

-- | 启动 Server
-- 返回 ServerHandle，可用于后续管理（停止等）
startServer :: ServerConfig -> (Socket -> SockAddr -> IO ()) -> IO ServerHandle
startServer cfg onNewConnection = do
    activeCount <- newIORef (0 :: Int)
    let port = show (scListenPort cfg)
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    sock <- openSocket addr
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock (scMaxConnections cfg)

    putStrLn $ "[Server] 监听端口 " ++ port ++ "，最大连接数 " ++ show (scMaxConnections cfg)
    hFlush stdout

    tid <- forkIO $ acceptLoop sock cfg activeCount onNewConnection
    return ServerHandle
        { shListenSocket = sock
        , shThreadId     = tid
        , shActiveCount  = activeCount
        }

-- | 接受连接循环
acceptLoop :: Socket -> ServerConfig -> IORef Int -> (Socket -> SockAddr -> IO ()) -> IO ()
acceptLoop listenSock cfg activeCount onNewConnection = forever $ do
    (clientSock, clientAddr) <- accept listenSock
    count <- atomicModifyIORef' activeCount (\n -> (n + 1, n + 1))
    if count > scMaxConnections cfg
        then do
            putStrLn $ "[Server] 连接数超限，拒绝: " ++ show clientAddr
            close clientSock
            atomicModifyIORef' activeCount (\n -> (n - 1, ()))
        else do
            putStrLn $ "[Server] 新连接: " ++ show clientAddr ++ " (活跃: " ++ show count ++ ")"
            hFlush stdout
            void $ forkIO $ do
                onNewConnection clientSock clientAddr
                    `catch` (\e -> do
                        let _ = e :: SomeException
                        putStrLn $ "[Server] 连接异常: " ++ show e
                    )
                close clientSock
                atomicModifyIORef' activeCount (\n -> (n - 1, ()))
