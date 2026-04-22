module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, when)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import qualified Data.ByteString.Char8 as BS
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hFlush, stdout)

-- | 配置
totalClients :: Int
totalClients = 1000

serverHost :: String
serverHost = "127.0.0.1"

serverPort :: String
serverPort = "9104"

main :: IO ()
main = do
    putStrLn $ "=== TCP Client 压力测试 ==="
    putStrLn $ "目标服务器: " ++ serverHost ++ ":" ++ serverPort
    putStrLn $ "并发客户端数: " ++ show totalClients
    putStrLn ""
    hFlush stdout

    -- 统计计数器
    successCount <- newIORef (0 :: Int)
    failCount    <- newIORef (0 :: Int)
    echoOkCount  <- newIORef (0 :: Int)

    -- 用 MVar 等待所有客户端完成
    doneSignals <- mapM (\_ -> newEmptyMVar) [1..totalClients]

    startTime <- getCurrentTime

    -- 启动 1000 个客户端线程
    forM_ (zip [1..totalClients] doneSignals) $ \(clientId, done) -> do
        forkIO $ do
            runClient clientId successCount failCount echoOkCount
                `catch` (\e -> do
                    let _ = e :: SomeException
                    atomicModifyIORef' failCount (\n -> (n + 1, ()))
                    when (clientId `mod` 100 == 0) $ do
                        putStrLn $ "[客户端 #" ++ show clientId ++ "] 失败: " ++ show e
                        hFlush stdout
                )
            putMVar done ()

        -- 每批 100 个之间短暂暂停，避免瞬间 SYN 洪泛
        when (clientId `mod` 100 == 0) $ do
            putStrLn $ "已启动 " ++ show clientId ++ "/" ++ show totalClients ++ " 个客户端..."
            hFlush stdout
            threadDelay 50000  -- 50ms

    -- 等待所有客户端完成
    putStrLn "\n等待所有客户端完成..."
    hFlush stdout
    mapM_ takeMVar doneSignals

    endTime <- getCurrentTime
    let elapsed = diffUTCTime endTime startTime
        elapsedMs = realToFrac elapsed * 1000.0 :: Double

    -- 打印结果
    succN <- readRef successCount
    failN <- readRef failCount
    echoN <- readRef echoOkCount

    putStrLn "\n========== 测试结果 =========="
    putStrLn $ "总客户端数:     " ++ show totalClients
    putStrLn $ "连接成功:       " ++ show succN
    putStrLn $ "连接失败:       " ++ show failN
    putStrLn $ "Echo 验证通过:  " ++ show echoN
    putStrLn $ "总耗时:         " ++ show (round elapsedMs :: Int) ++ " ms"
    putStrLn $ "平均每连接:     " ++ show (elapsedMs / fromIntegral totalClients) ++ " ms"
    putStrLn "==============================\n"
    hFlush stdout

-- | 读取 IORef 的当前值
readRef :: IORef Int -> IO Int
readRef ref = atomicModifyIORef' ref (\n -> (n, n))

-- | 单个客户端的逻辑
runClient :: Int -> IORef Int -> IORef Int -> IORef Int -> IO ()
runClient clientId successRef _failRef echoOkRef = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just serverHost) (Just serverPort)
    sock <- openSocket addr
    connect sock (addrAddress addr)

    -- 连接成功
    atomicModifyIORef' successRef (\n -> (n + 1, ()))

    -- 发送测试消息
    let msg = BS.pack $ "Hello from client #" ++ show clientId ++ "!"
    sendAll sock msg

    -- 接收回显
    response <- recv sock 4096

    -- 验证回显
    if response == msg
        then atomicModifyIORef' echoOkRef (\n -> (n + 1, ()))
        else putStrLn $ "[客户端 #" ++ show clientId ++ "] Echo 不匹配!"

    -- 短暂保持连接，模拟真实场景
    threadDelay 100000  -- 100ms

    close sock
