-- | IEC 60870-5-104 TypeID 定义
--
-- TypeID 标识 ASDU 中信息对象的类型。
-- 本模块定义了常用的 TypeID 及其分类。
--
module Protocol.TypeID
    ( -- * TypeID 类型
      TypeID(..)
    , typeIDToWord
    , typeIDFromWord
      -- * 分类查询
    , isMonitorDirection
    , isControlDirection
    , hasTimestamp
    ) where

import Data.Word (Word8)

-- | IEC 104 TypeID 定义
-- 优先支持最常用的 9 种类型
data TypeID
    -- 监视方向（子站 → 主站）
    = M_SP_NA     -- ^ 1:  单点遥信（不带时标）
    | M_DP_NA     -- ^ 3:  双点遥信（不带时标）
    | M_ME_NA     -- ^ 9:  归一化遥测值
    | M_ME_NC     -- ^ 13: 短浮点遥测值
    | M_SP_TB     -- ^ 30: 单点遥信（带 CP56Time2a 时标）
    | M_DP_TB     -- ^ 31: 双点遥信（带 CP56Time2a 时标）
    | M_ME_TF     -- ^ 36: 短浮点遥测（带 CP56Time2a 时标）
    -- 控制方向（主站 → 子站）
    | C_SC_NA     -- ^ 45: 单点遥控命令
    | C_DC_NA     -- ^ 46: 双点遥控命令
    | C_IC_NA     -- ^ 100: 站召唤命令
    | C_CI_NA     -- ^ 101: 电度总召唤命令
    | C_CS_NA     -- ^ 103: 时钟同步命令
    -- 未知类型
    | UnknownTypeID !Word8
    deriving (Show, Eq, Ord)

-- | TypeID → Word8
typeIDToWord :: TypeID -> Word8
typeIDToWord M_SP_NA          = 1
typeIDToWord M_DP_NA          = 3
typeIDToWord M_ME_NA          = 9
typeIDToWord M_ME_NC          = 13
typeIDToWord M_SP_TB          = 30
typeIDToWord M_DP_TB          = 31
typeIDToWord M_ME_TF          = 36
typeIDToWord C_SC_NA          = 45
typeIDToWord C_DC_NA          = 46
typeIDToWord C_IC_NA          = 100
typeIDToWord C_CI_NA          = 101
typeIDToWord C_CS_NA          = 103
typeIDToWord (UnknownTypeID w) = w

-- | Word8 → TypeID
typeIDFromWord :: Word8 -> TypeID
typeIDFromWord 1   = M_SP_NA
typeIDFromWord 3   = M_DP_NA
typeIDFromWord 9   = M_ME_NA
typeIDFromWord 13  = M_ME_NC
typeIDFromWord 30  = M_SP_TB
typeIDFromWord 31  = M_DP_TB
typeIDFromWord 36  = M_ME_TF
typeIDFromWord 45  = C_SC_NA
typeIDFromWord 46  = C_DC_NA
typeIDFromWord 100 = C_IC_NA
typeIDFromWord 101 = C_CI_NA
typeIDFromWord 103 = C_CS_NA
typeIDFromWord w   = UnknownTypeID w

-- | 是否是监视方向（子站上送）
isMonitorDirection :: TypeID -> Bool
isMonitorDirection M_SP_NA = True
isMonitorDirection M_DP_NA = True
isMonitorDirection M_ME_NA = True
isMonitorDirection M_ME_NC = True
isMonitorDirection M_SP_TB = True
isMonitorDirection M_DP_TB = True
isMonitorDirection M_ME_TF = True
isMonitorDirection _       = False

-- | 是否是控制方向（主站下发）
isControlDirection :: TypeID -> Bool
isControlDirection C_SC_NA = True
isControlDirection C_DC_NA = True
isControlDirection C_IC_NA = True
isControlDirection C_CI_NA = True
isControlDirection C_CS_NA = True
isControlDirection _       = False

-- | 是否带时标
hasTimestamp :: TypeID -> Bool
hasTimestamp M_SP_TB = True
hasTimestamp M_DP_TB = True
hasTimestamp M_ME_TF = True
hasTimestamp _       = False
