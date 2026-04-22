-- | IEC 60870-5-104 ASDU (Application Service Data Unit)
--
-- ASDU = 数据单元标识符 (6字节) + 信息对象 (可变)
--
-- 数据单元标识符:
-- +--------+----+-----+----+--------+
-- | TypeID | SQ | Num | COT | CA    |
-- +--------+----+-----+----+--------+
--   1B      1B(含SQ+Num) 2B    2B
--
module Protocol.ASDU
    ( -- * ASDU 类型
      ASDU(..)
    , COT(..)
      -- * 编解码
    , decodeASDUHeader
    , encodeASDUHeader
      -- * 传送原因
    , cotToWord
    , cotFromWord
    ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16)

import Protocol.TypeID (TypeID, typeIDToWord, typeIDFromWord)
import Protocol.InfoObject (InfoObject)

-- | 传送原因 (Cause of Transmission)
data COT
    = Periodic             -- ^ 1:  周期/循环
    | Background           -- ^ 2:  背景扫描
    | Spontaneous          -- ^ 3:  突发/自发
    | Initialized          -- ^ 4:  初始化完成
    | Request              -- ^ 5:  请求/被请求
    | Activation           -- ^ 6:  激活
    | ActivationCon        -- ^ 7:  激活确认
    | Deactivation         -- ^ 8:  停止激活
    | DeactivationCon      -- ^ 9:  停止激活确认
    | ActivationTerm       -- ^ 10: 激活终止
    | ReturnRemote         -- ^ 11: 远程命令引起的返回
    | ReturnLocal          -- ^ 12: 当地命令引起的返回
    | Interrogated         -- ^ 20: 响应站召唤
    | InterrogatedGroup1   -- ^ 21: 响应组1召唤
    | CounterInterrogated  -- ^ 37: 响应电度总召
    | UnknownCOT !Word8    -- ^ 未知传送原因
    deriving (Show, Eq, Ord)

-- | COT → Word8
cotToWord :: COT -> Word8
cotToWord Periodic            = 1
cotToWord Background          = 2
cotToWord Spontaneous         = 3
cotToWord Initialized         = 4
cotToWord Request             = 5
cotToWord Activation          = 6
cotToWord ActivationCon       = 7
cotToWord Deactivation        = 8
cotToWord DeactivationCon     = 9
cotToWord ActivationTerm      = 10
cotToWord ReturnRemote        = 11
cotToWord ReturnLocal         = 12
cotToWord Interrogated        = 20
cotToWord InterrogatedGroup1  = 21
cotToWord CounterInterrogated = 37
cotToWord (UnknownCOT w)      = w

-- | Word8 → COT
cotFromWord :: Word8 -> COT
cotFromWord 1  = Periodic
cotFromWord 2  = Background
cotFromWord 3  = Spontaneous
cotFromWord 4  = Initialized
cotFromWord 5  = Request
cotFromWord 6  = Activation
cotFromWord 7  = ActivationCon
cotFromWord 8  = Deactivation
cotFromWord 9  = DeactivationCon
cotFromWord 10 = ActivationTerm
cotFromWord 11 = ReturnRemote
cotFromWord 12 = ReturnLocal
cotFromWord 20 = Interrogated
cotFromWord 21 = InterrogatedGroup1
cotFromWord 37 = CounterInterrogated
cotFromWord w  = UnknownCOT w

-- | ASDU 数据单元
data ASDU = ASDU
    { asduTypeID      :: !TypeID         -- ^ 类型标识
    , asduSQ          :: !Bool           -- ^ SQ 位：连续寻址标志
    , asduNumObjects  :: !Int            -- ^ 信息对象数量
    , asduCOT         :: !COT            -- ^ 传送原因
    , asduPN          :: !Bool           -- ^ P/N 位：肯定/否定确认
    , asduTest        :: !Bool           -- ^ T 位：试验标志
    , asduOrigAddr    :: !Word8          -- ^ 源发站地址 (ORG)
    , asduCommonAddr  :: !Word16         -- ^ 公共地址 (CA)
    , asduObjects     :: ![InfoObject]   -- ^ 信息对象列表
    } deriving (Show, Eq)

-- | 解码 ASDU 头部（6 字节），返回头部信息和剩余数据
decodeASDUHeader :: ByteString -> Maybe (ASDU, ByteString)
decodeASDUHeader bs
    | BS.length bs < 6 = Nothing
    | otherwise =
        let typeId  = typeIDFromWord (BS.index bs 0)
            sqNum   = BS.index bs 1
            sq      = testBit sqNum 7
            numObj  = fromIntegral (sqNum .&. 0x7F) :: Int
            cotByte = BS.index bs 2
            cot     = cotFromWord (cotByte .&. 0x3F)
            pn      = testBit cotByte 6
            test    = testBit cotByte 7
            orgAddr = BS.index bs 3
            caLow   = BS.index bs 4
            caHigh  = BS.index bs 5
            ca      = fromIntegral caLow .|. (fromIntegral caHigh `shiftL` 8) :: Word16
            asdu    = ASDU
                { asduTypeID     = typeId
                , asduSQ         = sq
                , asduNumObjects = numObj
                , asduCOT        = cot
                , asduPN         = pn
                , asduTest       = test
                , asduOrigAddr   = orgAddr
                , asduCommonAddr = ca
                , asduObjects    = []  -- 信息对象需要后续根据 TypeID 解析
                }
        in Just (asdu, BS.drop 6 bs)

-- | 编码 ASDU 头部为 6 字节
encodeASDUHeader :: ASDU -> ByteString
encodeASDUHeader asdu = BS.pack
    [ typeIDToWord (asduTypeID asdu)
    , (fromIntegral (asduNumObjects asdu) .&. 0x7F)
        .|. (if asduSQ asdu then 0x80 else 0x00)
    , (cotToWord (asduCOT asdu) .&. 0x3F)
        .|. (if asduPN asdu then 0x40 else 0x00)
        .|. (if asduTest asdu then 0x80 else 0x00)
    , asduOrigAddr asdu
    , fromIntegral (asduCommonAddr asdu .&. 0xFF)
    , fromIntegral ((asduCommonAddr asdu `shiftR` 8) .&. 0xFF)
    ]
