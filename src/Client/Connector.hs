-- | IEC 104 Client 端 — 连接器
--
-- 主动连接子站/RTU，支持断线重连。
-- 对子站扮演 控制站(Master) 角色。
--
module Client.Connector
    ( -- * 连接器配置
      ConnectorConfig(..)
      -- * 连接操作
    , connectToStation
    , reconnectLoop
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, catch, bracket)
import Network.Socket
import System.IO (hFlush, stdout)

import Connection.Config (ConnectionConfig(..))

-- | 连接器配置
data ConnectorConfig = ConnectorConfig
    { ccName             :: !String            -- ^ 子站名称
    , ccHost             :: !String            -- ^ 子站 IP 地址
    , ccPort             :: !Int               -- ^ 子站端口
    , ccCommonAddr       :: !Int               -- ^ 公共地址
    , ccReconnectDelay   :: !Int               -- ^ 重连间隔（秒）
    , ccConnConfig       :: !ConnectionConfig  -- ^ 连接参数
    } deriving (Show)

-- | 连接到子站
-- 成功返回 Just Socket，失败返回 Nothing
connectToStation :: ConnectorConfig -> IO (Maybe Socket)
connectToStation cfg = do
    let host = ccHost cfg
        port = show (ccPort cfg)
    putStrLn $ "[Client:" ++ ccName cfg ++ "] 连接 " ++ host ++ ":" ++ port ++ "..."
    hFlush stdout

    result <- tryConnect host port
    case result of
        Right sock -> do
            putStrLn $ "[Client:" ++ ccName cfg ++ "] 连接成功"
            hFlush stdout
            return (Just sock)
        Left err -> do
            putStrLn $ "[Client:" ++ ccName cfg ++ "] 连接失败: " ++ err
            hFlush stdout
            return Nothing

-- | 重连循环：断线后自动重连
reconnectLoop :: ConnectorConfig -> (Socket -> IO ()) -> IO ()
reconnectLoop cfg handler = loop
  where
    loop = do
        result <- connectToStation cfg
        case result of
            Just sock -> do
                handler sock
                    `catch` (\e -> do
                        let _ = e :: SomeException
                        putStrLn $ "[Client:" ++ ccName cfg ++ "] 连接异常: " ++ show e
                    )
                close sock
            Nothing -> return ()

        -- 等待后重连
        let delay = ccReconnectDelay cfg
        putStrLn $ "[Client:" ++ ccName cfg ++ "] " ++ show delay ++ " 秒后重连..."
        hFlush stdout
        threadDelay (delay * 1000000)
        loop

-- | 尝试 TCP 连接
tryConnect :: String -> String -> IO (Either String Socket)
tryConnect host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just host) (Just port)
    case addrs of
        [] -> return (Left "无法解析地址")
        (addr:_) -> do
            sock <- openSocket addr
            (do connect sock (addrAddress addr)
                return (Right sock))
                `catch` (\e -> do
                    let _ = e :: SomeException
                    close sock
                    return (Left (show e)))
