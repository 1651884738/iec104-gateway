-- | APCI 帧编解码 单元测试
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word16)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

import Protocol.APCI
import Protocol.Parser

-- ============================================================
-- 测试框架（轻量级，不依赖外部测试库）
-- ============================================================

data TestResult = Pass String | Fail String String
    deriving (Show)

runTest :: String -> Bool -> TestResult
runTest name True  = Pass name
runTest name False = Fail name "断言失败"

runTestEq :: (Show a, Eq a) => String -> a -> a -> TestResult
runTestEq name expected actual
    | expected == actual = Pass name
    | otherwise = Fail name $ "期望: " ++ show expected ++ ", 实际: " ++ show actual

printResult :: TestResult -> IO Bool
printResult (Pass name) = do
    putStrLn $ "  ✓ " ++ name
    hFlush stdout
    return True
printResult (Fail name msg) = do
    putStrLn $ "  ✗ " ++ name ++ " — " ++ msg
    hFlush stdout
    return False

-- ============================================================
-- APCI 编解码测试
-- ============================================================

-- | 往返测试：encode → decode 后应该得到原始帧
roundTrip :: APCIFrame -> Bool
roundTrip frame =
    case decodeFrame (encodeFrame frame) of
        Right decoded -> decoded == frame
        Left _        -> False

testIFrameRoundTrip :: TestResult
testIFrameRoundTrip =
    let frame = IFrame 0 0 (BS.pack [0x01, 0x02, 0x03])
    in runTest "I 帧往返 (SSN=0, RSN=0)" (roundTrip frame)

testIFrameSeqNumbers :: TestResult
testIFrameSeqNumbers =
    let frame = IFrame 1234 5678 (BS.pack [0xAA, 0xBB])
    in runTest "I 帧往返 (SSN=1234, RSN=5678)" (roundTrip frame)

testIFrameMaxSeq :: TestResult
testIFrameMaxSeq =
    let frame = IFrame 32767 32767 BS.empty
    in runTest "I 帧往返 (最大序号 32767)" (roundTrip frame)

testSFrameRoundTrip :: TestResult
testSFrameRoundTrip =
    let frame = SFrame 42
    in runTest "S 帧往返 (RSN=42)" (roundTrip frame)

testSFrameZero :: TestResult
testSFrameZero =
    let frame = SFrame 0
    in runTest "S 帧往返 (RSN=0)" (roundTrip frame)

testUFrameStartDTAct :: TestResult
testUFrameStartDTAct =
    runTest "U 帧往返 (STARTDT Act)" (roundTrip (UFrame StartDTAct))

testUFrameStartDTCon :: TestResult
testUFrameStartDTCon =
    runTest "U 帧往返 (STARTDT Con)" (roundTrip (UFrame StartDTCon))

testUFrameStopDTAct :: TestResult
testUFrameStopDTAct =
    runTest "U 帧往返 (STOPDT Act)" (roundTrip (UFrame StopDTAct))

testUFrameStopDTCon :: TestResult
testUFrameStopDTCon =
    runTest "U 帧往返 (STOPDT Con)" (roundTrip (UFrame StopDTCon))

testUFrameTestFRAct :: TestResult
testUFrameTestFRAct =
    runTest "U 帧往返 (TESTFR Act)" (roundTrip (UFrame TestFRAct))

testUFrameTestFRCon :: TestResult
testUFrameTestFRCon =
    runTest "U 帧往返 (TESTFR Con)" (roundTrip (UFrame TestFRCon))

-- | 测试已知的字节序列
testKnownStartDTAct :: TestResult
testKnownStartDTAct =
    -- STARTDT Act 的标准字节序列: 68 04 07 00 00 00
    let expected = BS.pack [0x68, 0x04, 0x07, 0x00, 0x00, 0x00]
        encoded  = encodeFrame (UFrame StartDTAct)
    in runTestEq "STARTDT Act 编码 = 68 04 07 00 00 00" expected encoded

testKnownStartDTCon :: TestResult
testKnownStartDTCon =
    -- STARTDT Con: 68 04 0B 00 00 00
    let expected = BS.pack [0x68, 0x04, 0x0B, 0x00, 0x00, 0x00]
        encoded  = encodeFrame (UFrame StartDTCon)
    in runTestEq "STARTDT Con 编码 = 68 04 0B 00 00 00" expected encoded

testKnownTestFRAct :: TestResult
testKnownTestFRAct =
    -- TESTFR Act: 68 04 43 00 00 00
    let expected = BS.pack [0x68, 0x04, 0x43, 0x00, 0x00, 0x00]
        encoded  = encodeFrame (UFrame TestFRAct)
    in runTestEq "TESTFR Act 编码 = 68 04 43 00 00 00" expected encoded

testKnownTestFRCon :: TestResult
testKnownTestFRCon =
    -- TESTFR Con: 68 04 83 00 00 00
    let expected = BS.pack [0x68, 0x04, 0x83, 0x00, 0x00, 0x00]
        encoded  = encodeFrame (UFrame TestFRCon)
    in runTestEq "TESTFR Con 编码 = 68 04 83 00 00 00" expected encoded

testKnownSFrame :: TestResult
testKnownSFrame =
    -- S 帧 RSN=0: 68 04 01 00 00 00
    let expected = BS.pack [0x68, 0x04, 0x01, 0x00, 0x00, 0x00]
        encoded  = encodeFrame (SFrame 0)
    in runTestEq "S 帧(RSN=0) 编码 = 68 04 01 00 00 00" expected encoded

-- | 测试 I 帧编码的具体字节
testKnownIFrame :: TestResult
testKnownIFrame =
    -- I 帧 SSN=0, RSN=0, 无载荷: 68 04 00 00 00 00
    let expected = BS.pack [0x68, 0x04, 0x00, 0x00, 0x00, 0x00]
        encoded  = encodeFrame (IFrame 0 0 BS.empty)
    in runTestEq "I 帧(SSN=0,RSN=0,空载荷) 编码 = 68 04 00 00 00 00" expected encoded

-- | 测试解码错误情况
testDecodeTooShort :: TestResult
testDecodeTooShort =
    case decodeFrame (BS.pack [0x68, 0x04]) of
        Left _  -> Pass "太短的数据应该返回错误"
        Right _ -> Fail "太短的数据应该返回错误" "意外的成功解码"

testDecodeWrongStartByte :: TestResult
testDecodeWrongStartByte =
    case decodeFrame (BS.pack [0x69, 0x04, 0x00, 0x00, 0x00, 0x00]) of
        Left _  -> Pass "错误的起始字节应该返回错误"
        Right _ -> Fail "错误的起始字节应该返回错误" "意外的成功解码"

-- ============================================================
-- 帧解析器测试
-- ============================================================

testParserSingleFrame :: TestResult
testParserSingleFrame =
    let frame = encodeFrame (UFrame StartDTAct)
        (result, _) = feedBytes newParser frame
    in case result of
        Parsed f _ -> runTestEq "解析器: 单帧解析" frame f
        _          -> Fail "解析器: 单帧解析" (show result)

testParserTwoFrames :: TestResult
testParserTwoFrames =
    let f1 = encodeFrame (UFrame StartDTAct)
        f2 = encodeFrame (SFrame 5)
        combined = f1 <> f2
        (r1, p1) = feedBytes newParser combined
    in case r1 of
        Parsed parsed rest ->
            if parsed == f1
            then case feedBytes newParser rest of
                (Parsed parsed2 _, _) ->
                    runTestEq "解析器: 两帧粘包" f2 parsed2
                other -> Fail "解析器: 两帧粘包 (第2帧)" (show other)
            else Fail "解析器: 两帧粘包 (第1帧)" $ "期望: " ++ show f1 ++ ", 实际: " ++ show parsed
        _ -> Fail "解析器: 两帧粘包" (show r1)

testParserIncomplete :: TestResult
testParserIncomplete =
    let partial = BS.take 3 (encodeFrame (UFrame StartDTAct))
        (result, _) = feedBytes newParser partial
    in case result of
        NeedMore -> Pass "解析器: 不完整帧返回 NeedMore"
        _        -> Fail "解析器: 不完整帧返回 NeedMore" (show result)

testParserGarbage :: TestResult
testParserGarbage =
    let garbage = BS.pack [0xAA, 0xBB, 0xCC]
        (result, _) = feedBytes newParser garbage
    in case result of
        NeedMore -> Pass "解析器: 垃圾数据返回 NeedMore"
        _        -> Fail "解析器: 垃圾数据返回 NeedMore" (show result)

-- ============================================================
-- 主测试入口
-- ============================================================

main :: IO ()
main = do
    putStrLn "========================================="
    putStrLn " IEC 104 APCI 编解码测试"
    putStrLn "========================================="
    putStrLn ""

    putStrLn "--- 往返测试 (encode → decode) ---"
    results1 <- mapM printResult
        [ testIFrameRoundTrip
        , testIFrameSeqNumbers
        , testIFrameMaxSeq
        , testSFrameRoundTrip
        , testSFrameZero
        , testUFrameStartDTAct
        , testUFrameStartDTCon
        , testUFrameStopDTAct
        , testUFrameStopDTCon
        , testUFrameTestFRAct
        , testUFrameTestFRCon
        ]

    putStrLn ""
    putStrLn "--- 已知字节序列验证 ---"
    results2 <- mapM printResult
        [ testKnownStartDTAct
        , testKnownStartDTCon
        , testKnownTestFRAct
        , testKnownTestFRCon
        , testKnownSFrame
        , testKnownIFrame
        ]

    putStrLn ""
    putStrLn "--- 错误处理测试 ---"
    results3 <- mapM printResult
        [ testDecodeTooShort
        , testDecodeWrongStartByte
        ]

    putStrLn ""
    putStrLn "--- 帧解析器测试 ---"
    results4 <- mapM printResult
        [ testParserSingleFrame
        , testParserTwoFrames
        , testParserIncomplete
        , testParserGarbage
        ]

    let allResults = results1 ++ results2 ++ results3 ++ results4
        passed = length (filter id allResults)
        total  = length allResults

    putStrLn ""
    putStrLn "========================================="
    putStrLn $ " 结果: " ++ show passed ++ "/" ++ show total ++ " 通过"
    putStrLn "========================================="

    if passed == total
        then exitSuccess
        else exitFailure
