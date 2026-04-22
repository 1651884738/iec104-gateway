module Main where

import Control.Concurrent (forkIO, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, SomeException, catch, finally)
import Control.Monad (forever, void, when, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Data.Word (Word8, Word16)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.IO (hFlush, stdout, hSetBuffering, hSetEcho, BufferMode(..), stdin)
import Data.Bits ((.&.), shiftR)

import Protocol.APCI
import Protocol.Parser
import Protocol.ASDU
import Protocol.TypeID

-- | 主站配置
masterHost :: String
masterHost = "127.0.0.1"

masterPort :: String
masterPort = "2404"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering

    putStrLn "╔═══════════════════════════════════════════╗"
    putStrLn "║   IEC 104 主站测试客户端  v0.1.0         ║"
    putStrLn "╠═══════════════════════════════════════════╣"
    putStrLn $ "║  目标: " ++ padRight 35 (masterHost ++ ":" ++ masterPort) ++ "║"
    putStrLn "╚═══════════════════════════════════════════╝"
    putStrLn ""

    -- 连接到网关
    putStrLn "[Master] 连接到网关..."
    hFlush stdout

    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just masterHost) (Just masterPort)

    bracket (openSocket addr) close $ \sock -> do
        connect sock (addrAddress addr)
        putStrLn "[Master] ✓ TCP 连接成功"
        hFlush stdout

        -- 状态
        sendSeqRef <- newIORef (0 :: Word16)
        recvSeqRef <- newIORef (0 :: Word16)
        startedRef <- newIORef False
        parserRef  <- newIORef newParser

        -- 启动接收线程
        void $ forkIO $ receiveLoop sock parserRef recvSeqRef

        -- STARTDT 握手
        putStrLn "[Master] 发送 STARTDT Act..."
        sendAll sock (encodeFrame (UFrame StartDTAct))
        threadDelay 500000  -- 等待 0.5s 收到回复
        atomicModifyIORef' startedRef (\_ -> (True, ()))

        -- 交互式菜单
        interactiveMenu sock sendSeqRef recvSeqRef startedRef

-- | 接收线程
receiveLoop :: Socket -> IORef FrameParser -> IORef Word16 -> IO ()
receiveLoop sock parserRef recvSeqRef = loop
  where
    loop = do
        rawData <- recv sock 4096
        if BS.null rawData
            then putStrLn "\n[Master] 网关断开连接" >> hFlush stdout
            else do
                p <- readIORef parserRef
                processRecv sock parserRef recvSeqRef p rawData
                loop
    
    processRecv sock parserRef recvSeqRef parser input = do
        let (result, parser') = feedBytes parser input
        writeRef parserRef parser'
        case result of
            NeedMore -> return ()
            Parsed frameData rest -> do
                handleReceivedFrame sock recvSeqRef frameData
                when (not (BS.null rest)) $
                    processRecv sock parserRef recvSeqRef parser' rest
            ParseError err -> do
                putStrLn $ "\n[Master] 帧解析错误: " ++ err
                hFlush stdout

-- | 处理收到的帧
handleReceivedFrame :: Socket -> IORef Word16 -> ByteString -> IO ()
handleReceivedFrame sock recvSeqRef frameData = do
    case decodeFrame frameData of
        Left err ->
            putStrLn $ "\n[收到] 解码错误: " ++ show err
        Right frame -> case frame of
            UFrame StartDTCon ->
                putStrLn "\n[收到] ✓ STARTDT Con — 数据传输已激活"
            UFrame StopDTCon ->
                putStrLn "\n[收到] ✗ STOPDT Con — 数据传输已停止"
            UFrame TestFRCon ->
                putStrLn "\n[收到] ♥ TESTFR Con — 心跳正常"
            UFrame utype ->
                putStrLn $ "\n[收到] U帧: " ++ show utype
            SFrame rsn ->
                putStrLn $ "\n[收到] S帧 (RSN=" ++ show rsn ++ ")"
            IFrame ssn rsn payload -> do
                putStrLn $ "\n[收到] I帧 (SSN=" ++ show ssn
                    ++ ", RSN=" ++ show rsn
                    ++ ", 载荷=" ++ show (BS.length payload) ++ " 字节)"
                -- 解析 ASDU 头部
                when (BS.length payload >= 6) $ do
                    case decodeASDUHeader payload of
                        Nothing -> putStrLn "        ASDU 解析失败"
                        Just (asdu, _) -> do
                            putStrLn $ "        TypeID: " ++ show (asduTypeID asdu)
                            putStrLn $ "        COT:    " ++ show (asduCOT asdu)
                            putStrLn $ "        CA:     " ++ show (asduCommonAddr asdu)
                            putStrLn $ "        对象数: " ++ show (asduNumObjects asdu)
                -- 更新接收序号
                atomicModifyIORef' recvSeqRef (\_ -> (ssn + 1, ()))
                -- 回复 S 帧
                sendAll sock (encodeFrame (SFrame (ssn + 1)))
    hFlush stdout
    putStr "> "
    hFlush stdout

-- | 交互式菜单
interactiveMenu :: Socket -> IORef Word16 -> IORef Word16 -> IORef Bool -> IO ()
interactiveMenu sock sendSeqRef recvSeqRef startedRef = do
    threadDelay 300000  -- 让接收线程先打印
    showMenu
    loop
  where
    loop = do
        putStr "> "
        hFlush stdout
        cmd <- getLine
        case cmd of
            "1" -> do  -- 总召唤
                sendInterrogation sock sendSeqRef recvSeqRef
                loop
            "2" -> do  -- TESTFR
                putStrLn "[Master] 发送 TESTFR Act..."
                sendAll sock (encodeFrame (UFrame TestFRAct))
                loop
            "3" -> do  -- 单点遥控
                sendSingleCommand sock sendSeqRef recvSeqRef
                loop
            "4" -> do  -- 电度总召
                sendCounterInterrogation sock sendSeqRef recvSeqRef
                loop
            "5" -> do  -- STOPDT
                putStrLn "[Master] 发送 STOPDT Act..."
                sendAll sock (encodeFrame (UFrame StopDTAct))
                atomicModifyIORef' startedRef (\_ -> (False, ()))
                loop
            "6" -> do  -- STARTDT (重新激活)
                putStrLn "[Master] 发送 STARTDT Act..."
                sendAll sock (encodeFrame (UFrame StartDTAct))
                atomicModifyIORef' startedRef (\_ -> (True, ()))
                loop
            "h" -> do
                showMenu
                loop
            "q" -> do
                putStrLn "[Master] 退出"
                return ()
            "" -> loop  -- 空行忽略
            _ -> do
                putStrLn "未知命令，输入 h 查看帮助"
                loop

    showMenu = do
        putStrLn ""
        putStrLn "┌─────────────────────────────────┐"
        putStrLn "│  命令菜单                       │"
        putStrLn "├─────────────────────────────────┤"
        putStrLn "│  1 - 总召唤 (C_IC_NA)           │"
        putStrLn "│  2 - 心跳测试 (TESTFR)          │"
        putStrLn "│  3 - 单点遥控 (C_SC_NA)         │"
        putStrLn "│  4 - 电度总召 (C_CI_NA)         │"
        putStrLn "│  5 - 停止传输 (STOPDT)          │"
        putStrLn "│  6 - 启动传输 (STARTDT)         │"
        putStrLn "│  h - 显示帮助                   │"
        putStrLn "│  q - 退出                       │"
        putStrLn "└─────────────────────────────────┘"
        hFlush stdout

-- | 发送总召唤命令 C_IC_NA (TypeID=100)
sendInterrogation :: Socket -> IORef Word16 -> IORef Word16 -> IO ()
sendInterrogation sock sendSeqRef recvSeqRef = do
    ssn <- nextSeq sendSeqRef
    rsn <- readIORef recvSeqRef
    -- ASDU: TypeID=100, SQ=0, NumObj=1, COT=6(激活), CA=1
    -- 信息体: IOA=0x000000, QOI=20(站总召)
    let asduBytes = BS.pack
            [ 100         -- TypeID: C_IC_NA
            , 0x01        -- SQ=0, NumObj=1
            , 0x06        -- COT=6 (激活)
            , 0x00        -- ORG=0
            , 0x01, 0x00  -- CA=1 (公共地址)
            , 0x00, 0x00, 0x00  -- IOA=0
            , 20          -- QOI=20 (站总召)
            ]
    let frame = IFrame ssn rsn asduBytes
    putStrLn $ "[Master] 发送总召唤 (SSN=" ++ show ssn ++ ", RSN=" ++ show rsn ++ ")"
    sendAll sock (encodeFrame frame)
    hFlush stdout

-- | 发送单点遥控命令 C_SC_NA (TypeID=45)
sendSingleCommand :: Socket -> IORef Word16 -> IORef Word16 -> IO ()
sendSingleCommand sock sendSeqRef recvSeqRef = do
    putStr "  IOA (信息对象地址, 如 1001): "
    hFlush stdout
    ioaStr <- getLine
    putStr "  值 (0=分, 1=合): "
    hFlush stdout
    valStr <- getLine

    let ioa = read ioaStr :: Int
        val = read valStr :: Int
        sco = if val == 1 then 0x01 else 0x00 :: Word8  -- SCO: 值 + 选择/执行

    ssn <- nextSeq sendSeqRef
    rsn <- readIORef recvSeqRef

    let asduBytes = BS.pack
            [ 45          -- TypeID: C_SC_NA
            , 0x01        -- SQ=0, NumObj=1
            , 0x06        -- COT=6 (激活)
            , 0x00        -- ORG=0
            , 0x01, 0x00  -- CA=1
            , fromIntegral (ioa .&. 0xFF)           -- IOA 低
            , fromIntegral ((ioa `shiftR` 8) .&. 0xFF)  -- IOA 中
            , fromIntegral ((ioa `shiftR` 16) .&. 0xFF) -- IOA 高
            , sco          -- SCO
            ]
    let frame = IFrame ssn rsn asduBytes
    putStrLn $ "[Master] 发送单点遥控 IOA=" ++ show ioa
        ++ " 值=" ++ show val
        ++ " (SSN=" ++ show ssn ++ ")"
    sendAll sock (encodeFrame frame)
    hFlush stdout

-- | 发送电度总召命令 C_CI_NA (TypeID=101)
sendCounterInterrogation :: Socket -> IORef Word16 -> IORef Word16 -> IO ()
sendCounterInterrogation sock sendSeqRef recvSeqRef = do
    ssn <- nextSeq sendSeqRef
    rsn <- readIORef recvSeqRef
    let asduBytes = BS.pack
            [ 101         -- TypeID: C_CI_NA
            , 0x01        -- SQ=0, NumObj=1
            , 0x06        -- COT=6 (激活)
            , 0x00        -- ORG=0
            , 0x01, 0x00  -- CA=1
            , 0x00, 0x00, 0x00  -- IOA=0
            , 0x05        -- QCC=5 (总请求)
            ]
    let frame = IFrame ssn rsn asduBytes
    putStrLn $ "[Master] 发送电度总召 (SSN=" ++ show ssn ++ ")"
    sendAll sock (encodeFrame frame)
    hFlush stdout

-- | 获取并递增发送序号
nextSeq :: IORef Word16 -> IO Word16
nextSeq ref = atomicModifyIORef' ref (\n -> ((n + 1) `mod` 32768, n))

-- | IORef 写入
writeRef :: IORef a -> a -> IO ()
writeRef ref val = val `seq` atomicModifyIORef' ref (\_ -> (val, ()))

-- | 右填充
padRight :: Int -> String -> String
padRight n s
    | length s >= n = s
    | otherwise = s ++ replicate (n - length s) ' '
