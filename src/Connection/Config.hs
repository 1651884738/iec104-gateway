-- | IEC 104 连接配置参数
--
-- 定义连接的超时参数和流控参数。
-- 符合 IEC 60870-5-104 标准的默认值。
--
module Connection.Config
    ( -- * 配置类型
      ConnectionConfig(..)
      -- * 默认配置
    , defaultConfig
    ) where

-- | 连接配置
data ConnectionConfig = ConnectionConfig
    { cfgT0    :: !Int   -- ^ t0: TCP 连接建立超时（秒），默认 30
    , cfgT1    :: !Int   -- ^ t1: 发送帧后等待确认超时（秒），默认 15
    , cfgT2    :: !Int   -- ^ t2: 收到 I 帧后发送 S 帧确认的最大延迟（秒），默认 10
    , cfgT3    :: !Int   -- ^ t3: 空闲时发送 TESTFR 的间隔（秒），默认 20
    , cfgK     :: !Int   -- ^ k: 未确认 I 帧的最大数量，默认 12
    , cfgW     :: !Int   -- ^ w: 收到 w 个 I 帧后必须发送 S 帧确认，默认 8
    , cfgPort  :: !Int   -- ^ 监听/连接端口，默认 2404
    } deriving (Show, Eq)

-- | IEC 104 标准默认配置
defaultConfig :: ConnectionConfig
defaultConfig = ConnectionConfig
    { cfgT0   = 30
    , cfgT1   = 15
    , cfgT2   = 10
    , cfgT3   = 20
    , cfgK    = 12
    , cfgW    = 8
    , cfgPort = 2404
    }
