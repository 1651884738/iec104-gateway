module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket, SomeException, catch, finally)
import Control.Monad (forever, void, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))

import Protocol.APCI
import Protocol.HexDump (analyzeHex)
import Protocol.Parser

-- | 网关配置
gatewayPort :: String
gatewayPort = "2404"

gatewayName :: String
gatewayName = "IEC104-GW-01"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    putStrLn "╔═══════════════════════════════════════════╗"
    putStrLn "║     IEC 60870-5-104 Gateway  v0.1.0       ║"
    putStrLn "╠═══════════════════════════════════════════╣"
    putStrLn $ "║  网关名称: " ++ padRight 31 gatewayName ++ "║"
    putStrLn $ "║  监听端口: " ++ padRight 31 gatewayPort ++ "║"
    putStrLn "╚═══════════════════════════════════════════╝"
    putStrLn ""

    connCount <- newIORef (0 :: Int)
    activeCount <- newIORef (0 :: Int)

    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just gatewayPort)

    bracket (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 64

        putStrLn $ "[Gateway] 服务已启动，监听端口 " ++ gatewayPort
        putStrLn "[Gateway] 等待主站连接..."
        putStrLn ""
        hFlush stdout

        -- 主循环：接受连接
        forever $ do
            (clientSock, clientAddr) <- accept sock
            totalNum <- atomicModifyIORef' connCount (\n -> (n + 1, n + 1))
            activeNum <- atomicModifyIORef' activeCount (\n -> (n + 1, n + 1))
            putStrLn $ "[连接 #" ++ show totalNum ++ "] 主站连入: "
                ++ show clientAddr ++ " (活跃: " ++ show activeNum ++ ")"
            hFlush stdout

            void $ forkIO $
                handleMasterSession clientSock clientAddr totalNum activeCount
                    `finally` do
                        close clientSock
                        activeNum' <- atomicModifyIORef' activeCount (\n -> (n - 1, n - 1))
                        putStrLn $ "[连接 #" ++ show totalNum ++ "] 主站断开: "
                            ++ show clientAddr ++ " (活跃: " ++ show activeNum' ++ ")"
                        hFlush stdout

-- | 处理单个主站会话
-- 实现 IEC 104 协议的 STARTDT/STOPDT/TESTFR 握手和数据传输
handleMasterSession :: Socket -> SockAddr -> Int -> IORef Int -> IO ()
handleMasterSession sock addr connId _ = do
    let tag = "[会话 #" ++ show connId ++ "] "
    putStrLn $ tag ++ "等待 STARTDT..."
    hFlush stdout

    -- 协议状态
    started  <- newIORef False
    parser   <- newIORef newParser

    -- 接收循环
    loop tag sock started parser
  where
    loop tag sock started parserRef = do
        rawData <- recv sock 4096
        if BS.null rawData
            then putStrLn $ tag ++ "连接关闭（对端断开）"
            else do
                putStrLn $ tag ++ ">>> 收到 [Hex]: " ++ analyzeHex rawData
                -- 将数据喂入解析器
                p <- readIORef parserRef
                processFrames tag sock started parserRef p rawData
                loop tag sock started parserRef

-- | 处理解析出的帧
processFrames :: String -> Socket -> IORef Bool -> IORef FrameParser -> FrameParser -> ByteString -> IO ()
processFrames tag sock started parserRef parser input = do
    let (result, parser') = feedBytes parser input
    case result of
        NeedMore -> do
            writeIORef' parserRef parser'
        Parsed frameData rest -> do
            writeIORef' parserRef parser'
            handleFrame tag sock started frameData
            -- 继续处理剩余数据
            when (not (BS.null rest)) $
                processFrames tag sock started parserRef parser' rest
        ParseError err -> do
            writeIORef' parserRef parser'
            putStrLn $ tag ++ "帧解析错误: " ++ err
            hFlush stdout

-- | 处理单个 APCI 帧
handleFrame :: String -> Socket -> IORef Bool -> ByteString -> IO ()
handleFrame tag sock started frameData = do
    case decodeFrame frameData of
        Left err -> do
            putStrLn $ tag ++ "帧解码错误: " ++ show err
            hFlush stdout
        Right frame -> do
            isStarted <- readIORef started
            case frame of
                -- U 帧处理
                UFrame StartDTAct -> do
                    putStrLn $ tag ++ "收到 STARTDT Act → 回复 STARTDT Con"
                    sendFrame tag sock (UFrame StartDTCon)
                    writeIORef' started True
                    putStrLn $ tag ++ "✓ 数据传输已激活"
                    hFlush stdout

                UFrame StopDTAct -> do
                    putStrLn $ tag ++ "收到 STOPDT Act → 回复 STOPDT Con"
                    sendFrame tag sock (UFrame StopDTCon)
                    writeIORef' started False
                    putStrLn $ tag ++ "✗ 数据传输已停止"
                    hFlush stdout

                UFrame TestFRAct -> do
                    putStrLn $ tag ++ "收到 TESTFR Act → 回复 TESTFR Con"
                    sendFrame tag sock (UFrame TestFRCon)
                    hFlush stdout

                UFrame utype -> do
                    putStrLn $ tag ++ "收到 U 帧: " ++ show utype
                    hFlush stdout

                -- S 帧处理
                SFrame rsn -> do
                    putStrLn $ tag ++ "收到 S 帧 (RSN=" ++ show rsn ++ ")"
                    hFlush stdout

                -- I 帧处理
                IFrame ssn rsn payload -> do
                    if isStarted
                        then do
                            putStrLn $ tag ++ "收到 I 帧 (SSN=" ++ show ssn
                                ++ ", RSN=" ++ show rsn
                                ++ ", 载荷=" ++ show (BS.length payload) ++ " 字节)"
                            -- 回复 S 帧确认
                            sendFrame tag sock (SFrame (ssn + 1))
                            hFlush stdout
                        else do
                            putStrLn $ tag ++ "⚠ 收到 I 帧但数据传输未激活，忽略"
                            hFlush stdout

-- | IORef 严格写入
writeIORef' :: IORef a -> a -> IO ()
writeIORef' ref val = val `seq` atomicModifyIORef' ref (\_ -> (val, ()))

-- | 右填充字符串
padRight :: Int -> String -> String
padRight n s
    | length s >= n = s
    | otherwise = s ++ replicate (n - length s) ' '

-- | 发送帧并打印 Hex
sendFrame :: String -> Socket -> APCIFrame -> IO ()
sendFrame tag sock frame = do
    let bs = encodeFrame frame
    putStrLn $ tag ++ "<<< 发送 [Hex]: " ++ analyzeHex bs
    sendAll sock bs