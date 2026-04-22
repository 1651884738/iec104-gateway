-- | IEC 60870-5-104 帧解析器
--
-- 从 TCP 字节流中切分出完整的 APCI 帧。
-- 处理粘包和拆包的情况。
--
module Protocol.Parser
    ( -- * 解析器
      FrameParser
    , newParser
    , feedBytes
    , ParseResult(..)
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Protocol.APCI (startByte)

-- | 解析结果
data ParseResult
    = NeedMore                          -- ^ 需要更多数据
    | Parsed ByteString ByteString      -- ^ 解析出一个完整帧 + 剩余数据
    | ParseError String                 -- ^ 解析错误
    deriving (Show, Eq)

-- | 帧解析器，持有未处理的缓冲区
newtype FrameParser = FrameParser
    { parserBuffer :: ByteString
    } deriving (Show)

-- | 创建一个新的解析器
newParser :: FrameParser
newParser = FrameParser BS.empty

-- | 向解析器喂入数据，尝试解析出帧
-- 返回 (解析结果, 更新后的解析器)
feedBytes :: FrameParser -> ByteString -> (ParseResult, FrameParser)
feedBytes parser input =
    let buf = parserBuffer parser <> input
    in tryParse buf

-- | 尝试从缓冲区中解析一个帧
tryParse :: ByteString -> (ParseResult, FrameParser)
tryParse buf
    -- 不够最小帧长度（2 字节头 + 4 字节控制域 = 6）
    | BS.length buf < 2 = (NeedMore, FrameParser buf)
    | otherwise =
        -- 查找起始字节 0x68
        case BS.elemIndex startByte buf of
            Nothing ->
                -- 没有找到起始字节，丢弃所有数据
                (NeedMore, FrameParser BS.empty)
            Just idx ->
                let buf' = BS.drop idx buf  -- 跳到起始字节位置
                in parseFromStart buf'

-- | 从起始字节开始解析
parseFromStart :: ByteString -> (ParseResult, FrameParser)
parseFromStart buf
    | BS.length buf < 2 = (NeedMore, FrameParser buf)
    | otherwise =
        let len = fromIntegral (BS.index buf 1) :: Int  -- APDU 长度（不含起始字节和长度字节本身）
            totalLen = len + 2  -- 完整帧长度
        in if len < 4
            then (ParseError "帧长度小于 4（最小控制域长度）", FrameParser (BS.drop 1 buf))
            else if len > 253
            then (ParseError "帧长度超过 253（IEC 104 最大长度）", FrameParser (BS.drop 1 buf))
            else if BS.length buf < totalLen
            then (NeedMore, FrameParser buf)
            else
                let frame = BS.take totalLen buf
                    rest  = BS.drop totalLen buf
                in (Parsed frame rest, FrameParser rest)
