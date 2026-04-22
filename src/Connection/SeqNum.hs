-- | IEC 104 序号管理
--
-- 管理发送序号 V(S) 和接收序号 V(R)，
-- 以及基于 k/w 参数的滑动窗口流控。
--
module Connection.SeqNum
    ( -- * 序号管理器
      SeqManager(..)
    , newSeqManager
      -- * 序号操作
    , nextSendSeq
    , updateRecvSeq
    , confirmSendSeq
      -- * 窗口查询
    , canSend
    , needAck
    , unackedCount
    ) where

import Data.Word (Word16)

-- | 序号管理器
data SeqManager = SeqManager
    { smSendSeq      :: !Word16   -- ^ V(S): 下一个发送序号
    , smRecvSeq      :: !Word16   -- ^ V(R): 下一个期望接收的序号
    , smAckedSeq     :: !Word16   -- ^ 已被对端确认的最大发送序号
    , smRecvSinceAck :: !Int      -- ^ 自上次发送 S 帧以来接收的 I 帧数
    , smK            :: !Int      -- ^ k 参数：最大未确认 I 帧数
    , smW            :: !Int      -- ^ w 参数：接收 w 个 I 帧后需确认
    } deriving (Show, Eq)

-- | 创建新的序号管理器
newSeqManager :: Int -> Int -> SeqManager
newSeqManager k w = SeqManager
    { smSendSeq      = 0
    , smRecvSeq      = 0
    , smAckedSeq     = 0
    , smRecvSinceAck = 0
    , smK            = k
    , smW            = w
    }

-- | 获取下一个发送序号并递增
-- 序号范围 0-32767，循环使用
nextSendSeq :: SeqManager -> (Word16, SeqManager)
nextSendSeq sm =
    let ssn = smSendSeq sm
        next = (ssn + 1) `mod` 32768
    in (ssn, sm { smSendSeq = next })

-- | 收到 I 帧时更新接收序号
updateRecvSeq :: SeqManager -> SeqManager
updateRecvSeq sm = sm
    { smRecvSeq      = (smRecvSeq sm + 1) `mod` 32768
    , smRecvSinceAck = smRecvSinceAck sm + 1
    }

-- | 收到 S 帧或 I 帧中的 RSN 时，确认发送序号
confirmSendSeq :: Word16 -> SeqManager -> SeqManager
confirmSendSeq ackSeq sm = sm { smAckedSeq = ackSeq }

-- | 是否还可以发送（未确认数 < k）
canSend :: SeqManager -> Bool
canSend sm = unackedCount sm < smK sm

-- | 是否需要发送 S 帧确认（接收数 >= w）
needAck :: SeqManager -> Bool
needAck sm = smRecvSinceAck sm >= smW sm

-- | 未确认的 I 帧数量
unackedCount :: SeqManager -> Int
unackedCount sm =
    let diff = fromIntegral (smSendSeq sm) - fromIntegral (smAckedSeq sm) :: Int
    in if diff >= 0 then diff else diff + 32768
