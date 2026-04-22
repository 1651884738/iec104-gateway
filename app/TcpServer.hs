module Main where

import Control.Concurrent (forkIO)
import Control.Exception (bracket, bracketOnError, finally, SomeException, catch)
import Control.Monad (forever, void, when)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import qualified Data.ByteString as BS
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hFlush, stdout)

-- | TCP Echo Server
-- 监听指定端口，接受连接，回显收到的数据
main :: IO ()
main = do
    let port = "9104"
    connCount <- newIORef (0 :: Int)
    activeCount <- newIORef (0 :: Int)

    putStrLn $ "=== TCP Echo Server ==="
    putStrLn $ "监听端口: " ++ port
    putStrLn $ "等待客户端连接..."
    hFlush stdout

    -- 创建监听 socket
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    bracket (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        -- 设置 backlog 为 1024，足够排队 1000 个连接
        listen sock 1024
        putStrLn "Server 已启动，等待连接...\n"
        hFlush stdout

        -- 主循环：接受连接
        forever $ do
            (clientSock, clientAddr) <- accept sock
            -- 更新计数器
            totalNum <- atomicModifyIORef' connCount (\n -> (n + 1, n + 1))
            activeNum <- atomicModifyIORef' activeCount (\n -> (n + 1, n + 1))
            putStrLn $ "[连接 #" ++ show totalNum ++ "] 新客户端: "
                ++ show clientAddr ++ " (当前活跃: " ++ show activeNum ++ ")"
            hFlush stdout

            -- 为每个客户端 fork 一个线程处理
            void $ forkIO $ handleClient clientSock totalNum activeCount

-- | 处理单个客户端连接
handleClient :: Socket -> Int -> Data.IORef.IORef Int -> IO ()
handleClient sock connId activeCount = do
    handle `finally` cleanup
  where
    handle = do
        -- 持续接收并回显数据
        loop
    loop = do
        msg <- recv sock 4096
        if BS.null msg
            then return ()  -- 客户端关闭了连接
            else do
                -- 回显数据
                sendAll sock msg
                loop
    cleanup = do
        close sock
        activeNum <- atomicModifyIORef' activeCount (\n -> (n - 1, n - 1))
        putStrLn $ "[连接 #" ++ show connId ++ "] 断开 (当前活跃: "
            ++ show activeNum ++ ")"
        hFlush stdout
