# Miva 快速开始

## 简介

Miva 是一个**内存安全、可验证、可预测**的**系统级编程语言**。

> 比 Rust 简单，比 Python 快，比 C 安全。

---

## 安装

### Linux

1. 克隆仓库到任意目录
2. 从 [Release](https://github.com/hunter-hongg/miva-lang/releases) 下载预编译二进制文件
3. 将二进制文件添加到 `$PATH`

### Windows

> ⚠️ **注意**：Windows 预编译工具链将在 `v0.0.4` 版本提供，目前需要手动编译。

1. 安装 MinGW 或 WSL，确保 `g++` 命令可用
2. 参考下方「手动编译」步骤

### macOS

> ⚠️ **注意**：macOS 预编译工具链将在 `v0.0.4` 版本提供，目前需要手动编译。

1. 安装 Xcode Command Line Tools，确保 `g++` 命令可用
2. 参考下方「手动编译」步骤

### 手动编译（所有平台）

**前置条件**：
- 工具链：`ocaml` 与 `dune`
- C++ 编译器：`g++` 或 `clang++`

**步骤**：
```bash
# 1. 安装 OCaml 依赖
opam install dune menhir toml cmdliner

# 2. 构建项目
dune build

# 3. 安装到系统路径
# Linux/macOS:
cp _build/default/bin/main.exe /usr/local/bin/miva

# Windows:
# copy _build\default\bin\main.exe C:\Windows\System32\miva.exe
```

---

## 环境配置

设置 Miva 标准库路径环境变量：

```bash
# Linux/macOS (添加到 ~/.bashrc 或 ~/.zshrc)
export MIVA_STD="/path/to/your/miva/repo/util"

# Windows (PowerShell)
$env:MIVA_STD = "C:\path\to\your\miva\repo\util"
```

> 💡 **提示**：将上述命令添加到 shell 配置文件中，避免每次启动终端都需重新设置。

---

## 快速开始

### 1. 验证安装

```bash
miva --version
```

期望输出类似：
```
0.0.3
```

### 2. 创建项目

```bash
# 创建并进入项目目录
mkdir miva-project-name && cd miva-project-name

# 初始化项目（自动生成 Hello World 示例）
miva init miva-project-name --type=bin

# 运行项目
miva run
```

### 3. 期望输出

```
Hello, World! # 或类似
```

---

## 常见问题

| 问题 | 解决方案 |
|------|---------|
| `miva: command not found` | 检查二进制文件是否已添加到 `$PATH` |
| `g++: command not found` | 安装对应平台的 C++ 编译器 |
| `opam: command not found` | 参考 [OCaml 官方安装指南](https://ocaml.org/docs/install) |

---

## 下一步

- 📖 阅读 [语言教程](./tutorial.md)
- 💬 加入 [社区讨论](https://github.com/hunter-hongg/miva-lang/discussions)
