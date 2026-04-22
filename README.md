# IEC 60870-5-104 Gateway

<p align="center">
  <strong>基于 Haskell 实现的 IEC 60870-5-104 规约转发网关</strong>
</p>

<p align="center">
  <img src="https://img.shields.io/badge/language-Haskell-5e5086?style=flat-square" alt="Haskell">
  <img src="https://img.shields.io/badge/protocol-IEC%2060870--5--104-blue?style=flat-square" alt="IEC 104">
  <img src="https://img.shields.io/badge/license-BSD--3--Clause-green?style=flat-square" alt="License">
  <img src="https://img.shields.io/badge/tests-23%2F23%20passed-brightgreen?style=flat-square" alt="Tests">
</p>

---

## 📖 简介

IEC 60870-5-104（简称 IEC 104）是电力系统调度自动化中广泛使用的远动通信协议，基于 TCP/IP 网络传输。

本项目实现了一个 **IEC 104 网关**，能够：
- 对**上级主站**扮演受控站（Slave）角色，接收总召唤、遥控命令
- 对**下级子站/RTU** 扮演控制站（Master）角色，主动采集数据
- 在两者之间进行**数据转发**和**地址映射**

```
┌─────────┐                    ┌──────────────┐                    ┌─────────┐
│  主 站   │◄── IEC 104 ──►│   本 网 关    │◄── IEC 104 ──►│  RTU-01  │
│ (SCADA)  │   Server 端     │ (iec104-gw)  │   Client 端     │  子站 1  │
└─────────┘   端口 2404      ├──────────────┤                    ├─────────┤
                              │  地址映射     │◄── IEC 104 ──►│  RTU-02  │
                              │  数据缓存     │   Client 端     │  子站 2  │
                              │  命令路由     │                    └─────────┘
                              └──────────────┘
```

## ✨ 功能特性

### 已实现 ✅
- **APCI 帧编解码** — I 帧 / S 帧 / U 帧完整支持
- **TCP 流帧解析器** — 处理粘包、半包、垃圾数据
- **ASDU 应用层解析** — TypeID、COT、公共地址解析
- **连接状态机** — 完整的 IEC 104 连接状态迁移
- **序号管理** — 发送/接收序号、滑动窗口 (k/w 参数)
- **超时管理** — t1/t2/t3 定时器
- **Server 端** — TCP 监听，STARTDT/STOPDT/TESTFR 握手
- **实时数据库** — STM 无锁并发，变化事件检测
- **地址映射** — 子站 IOA ↔ 网关 IOA 双向映射
- **命令路由** — 主站命令按地址映射路由到子站
- **主站测试客户端** — 交互式 IEC 104 主站模拟器
- **TCP 压力测试** — 1000 并发连接测试

### 开发中 🚧
- [ ] Client 端数据采集循环
- [ ] 子站数据 → 网关 → 主站全链路转发
- [ ] 总召唤响应（生成遥信遥测数据）
- [ ] 遥控命令 Select-Before-Operate
- [ ] CP56Time2a 时标编解码
- [ ] YAML 配置文件加载
- [ ] 日志系统
- [ ] Web 监控界面

## 📦 项目结构

```
iec104-gateway/
├── app/                          # 可执行程序入口
│   ├── Main.hs                   #   网关主程序 (Server 端)
│   ├── IEC104Master.hs           #   主站测试客户端 (交互式)
│   ├── TcpServer.hs              #   TCP 压力测试 Server
│   └── TcpClient.hs              #   TCP 压力测试 Client (1000 连接)
│
├── src/                          # 库源码
│   ├── Protocol/                 #   ┌ 协议层 ─────────────────
│   │   ├── APCI.hs               #   │ APCI 帧编解码 (I/S/U)
│   │   ├── Parser.hs             #   │ TCP 流帧解析器 (粘包处理)
│   │   ├── ASDU.hs               #   │ ASDU 头部编解码
│   │   ├── TypeID.hs             #   │ 类型标识 (M_SP_NA 等)
│   │   ├── InfoObject.hs         #   │ 信息对象编解码
│   │   ├── Time.hs               #   │ CP56Time2a 时标
│   │   └── Quality.hs            #   └ 品质描述词 (QDS)
│   │
│   ├── Connection/               #   ┌ 连接管理层 ──────────────
│   │   ├── StateMachine.hs       #   │ 连接状态机
│   │   ├── SeqNum.hs             #   │ 序号管理 (SSN/RSN)
│   │   ├── Timeout.hs            #   │ 超时管理 (t1/t2/t3)
│   │   └── Config.hs             #   └ 连接参数 (k/w/t0~t3)
│   │
│   ├── Server/                   #   ┌ Server 端 ──────────────
│   │   ├── Listener.hs           #   │ TCP 监听器
│   │   └── Session.hs            #   └ 会话管理
│   │
│   ├── Client/                   #   ┌ Client 端 ──────────────
│   │   ├── Connector.hs          #   │ TCP 连接器 (自动重连)
│   │   └── Station.hs            #   └ 子站管理
│   │
│   └── Core/                     #   ┌ 核心逻辑 ───────────────
│       ├── DataPoint.hs          #   │ 数据点模型
│       ├── DataStore.hs          #   │ 实时数据库 (STM)
│       ├── AddressMap.hs         #   │ 地址映射
│       └── CommandRouter.hs      #   └ 命令路由
│
├── test/                         # 测试
│   └── Main.hs                   #   APCI 编解码测试 (23 用例)
│
├── config/                       # 配置
│   └── gateway.yaml              #   网关配置文件
│
├── docs/                         # 文档
│   └── iec104-protocol.html      #   IEC 104 协议帧解析参考
│
├── iec104-gateway.cabal          # Cabal 构建配置
├── CHANGELOG.md                  # 变更日志
└── LICENSE                       # BSD-3-Clause 许可证
```

## 🚀 快速开始

### 前置要求

- [GHCup](https://www.haskell.org/ghcup/) (GHC 9.6+ & Cabal 3.10+)

```bash
# 安装 GHCup (如未安装)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

### 编译

```bash
git clone https://github.com/YOUR_USERNAME/iec104-gateway.git
cd iec104-gateway

# 编译全部
cabal build all

# 运行测试
cabal test test-iec104
```

### 运行

#### 1. 启动网关（Server 端）

```bash
cabal run iec104-gateway
```

输出：
```
╔═══════════════════════════════════════════╗
║     IEC 60870-5-104 Gateway  v0.1.0      ║
╠═══════════════════════════════════════════╣
║  网关名称: IEC104-GW-01                   ║
║  监听端口: 2404                           ║
╚═══════════════════════════════════════════╝

[Gateway] 服务已启动，监听端口 2404
[Gateway] 等待主站连接...
```

#### 2. 启动主站测试客户端（另一个终端）

```bash
cabal run iec104-master
```

输出：
```
╔═══════════════════════════════════════════╗
║   IEC 104 主站测试客户端  v0.1.0         ║
╚═══════════════════════════════════════════╝

[Master] ✓ TCP 连接成功
[Master] 发送 STARTDT Act...
[收到] ✓ STARTDT Con — 数据传输已激活

┌─────────────────────────────────┐
│  命令菜单                       │
├─────────────────────────────────┤
│  1 - 总召唤 (C_IC_NA)           │
│  2 - 心跳测试 (TESTFR)          │
│  3 - 单点遥控 (C_SC_NA)         │
│  4 - 电度总召 (C_CI_NA)         │
│  5 - 停止传输 (STOPDT)          │
│  6 - 启动传输 (STARTDT)         │
│  q - 退出                       │
└─────────────────────────────────┘
```

#### 3. TCP 压力测试

```bash
# 终端 1
cabal run tcp-server

# 终端 2
cabal run tcp-client
# → 1000 个并发连接，验证 TCP 通信基础设施
```

## ⚙️ 配置

配置文件位于 `config/gateway.yaml`：

```yaml
# Server 端（面向主站）
server:
  listen_port: 2404
  max_connections: 10

# Client 端（面向子站）
clients:
  - name: "RTU-01"
    host: "192.168.1.101"
    port: 2404
    common_address: 1

# 地址映射
address_mapping:
  - source: { station: "RTU-01", ioa_start: 1, ioa_count: 100 }
    target: { ioa_start: 1001 }

# IEC 104 超时参数
timeouts:
  t0: 30    # TCP 连接超时
  t1: 15    # I 帧确认超时
  t2: 10    # S 帧发送延迟上限
  t3: 20    # TESTFR 间隔
  k: 12     # 未确认 I 帧上限
  w: 8      # 触发 S 帧的 I 帧计数
```

## 📊 测试

```bash
# 运行所有测试
cabal test test-iec104

# 测试输出
=========================================
 IEC 104 APCI 编解码测试
=========================================
  ✓ I 帧往返 (SSN=0, RSN=0)
  ✓ I 帧往返 (SSN=1234, RSN=5678)
  ✓ I 帧往返 (最大序号 32767)
  ✓ S 帧往返 (RSN=42)
  ✓ U 帧往返 (STARTDT/STOPDT/TESTFR)
  ✓ 已知字节序列验证
  ✓ 错误处理测试
  ✓ 帧解析器测试 (粘包/半包/垃圾数据)
=========================================
 结果: 23/23 通过
=========================================
```

## 📚 协议参考

- **标准**: IEC 60870-5-104 (Telecontrol — Network access for IEC 60870-5-101)
- **传输层**: TCP/IP, 默认端口 **2404**
- **帧格式**: APCI (I 帧 / S 帧 / U 帧) + ASDU (应用数据)
- **详细帧结构**: 参见 [`docs/iec104-protocol.html`](docs/iec104-protocol.html)

### APCI 帧格式速查

| 帧类型 | 控制域 (4 字节) | 用途 |
|--------|-----------------|------|
| I 帧 | SSN + RSN | 传输 ASDU 数据 |
| S 帧 | `01` + RSN | 确认接收序号 |
| U 帧 | 功能码 | STARTDT / STOPDT / TESTFR |

### 常用 TypeID

| TypeID | 名称 | 方向 | 说明 |
|--------|------|------|------|
| 1 | M_SP_NA | M→S | 单点遥信 |
| 3 | M_DP_NA | M→S | 双点遥信 |
| 9 | M_ME_NA | M→S | 归一化遥测 |
| 13 | M_ME_NC | M→S | 短浮点遥测 |
| 30 | M_SP_TB | M→S | 带时标单点遥信 |
| 45 | C_SC_NA | S→M | 单点遥控 |
| 100 | C_IC_NA | S→M | 站总召唤 |
| 101 | C_CI_NA | S→M | 电度总召唤 |

## 🏗️ 架构设计

```
                    ┌─────────────────────────────────────┐
                    │           应用层 (app/)              │
                    │  Main.hs │ IEC104Master.hs          │
                    └──────────┬──────────────────────────┘
                               │
          ┌────────────────────┼────────────────────┐
          │                    │                    │
    ┌─────┴─────┐     ┌───────┴───────┐     ┌──────┴──────┐
    │  Server/   │     │    Core/      │     │  Client/    │
    │  Listener  │     │  DataStore    │     │  Connector  │
    │  Session   │     │  AddressMap   │     │  Station    │
    │            │     │  CommandRouter│     │             │
    └─────┬─────┘     │  DataPoint    │     └──────┬──────┘
          │            └───────┬───────┘            │
          │                    │                    │
    ┌─────┴────────────────────┴────────────────────┴─────┐
    │              Connection/                             │
    │  StateMachine │ SeqNum │ Timeout │ Config            │
    └──────────────────────────┬───────────────────────────┘
                               │
    ┌──────────────────────────┴───────────────────────────┐
    │              Protocol/                               │
    │  APCI │ Parser │ ASDU │ TypeID │ InfoObject │ ...    │
    └──────────────────────────────────────────────────────┘
```

## 📝 License

[BSD-3-Clause](LICENSE)
