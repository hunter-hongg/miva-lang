# Miva Quick Start

## Introduction

Miva is a **memory-safe, verifiable, and predictable** **systems programming language**.

> Simpler than Rust, faster than Python, safer than C.

---

## Installation

### Linux

1. Clone the repository to any directory
2. Download precompiled binaries from [Releases](https://github.com/hunter-hongg/miva-lang/releases)
3. Add the binary to your `$PATH`

### Windows

> ⚠️ **Note**: Precompiled toolchain for Windows will be available in version `v0.0.4`. Manual compilation is required for now.

1. Install MinGW or WSL to ensure the `g++` command is available
2. Follow the "Manual Compilation" steps below

### macOS

> ⚠️ **Note**: Precompiled toolchain for macOS will be available in version `v0.0.4`. Manual compilation is required for now.

1. Install Xcode Command Line Tools to ensure the `g++` command is available
2. Follow the "Manual Compilation" steps below

### Manual Compilation (All Platforms)

**Prerequisites**:
- Toolchain: `ocaml` and `dune`
- C++ Compiler: `g++` or `clang++`

**Steps**:
```bash
# 1. Install OCaml dependencies
opam install dune menhir toml cmdliner

# 2. Build the project
dune build

# 3. Install to system path
# Linux/macOS:
cp _build/default/bin/main.exe /usr/local/bin/miva

# Windows:
# copy _build\default\bin\main.exe C:\Windows\System32\miva.exe
```

---

## Environment Configuration

Set the Miva standard library path environment variable:

```bash
# Linux/macOS (add to ~/.bashrc or ~/.zshrc)
export MIVA_STD="/path/to/your/miva/repo/util"

# Windows (PowerShell)
$env:MIVA_STD = "C:\path\to\your\miva\repo\util"
```

> 💡 **Tip**: Add the above commands to your shell configuration file to avoid setting them every time you start a terminal.

---

## Quick Start

### 1. Verify Installation

```bash
miva --version
```

Expected output:
```
0.0.3
```

### 2. Create a Project

```bash
# Create and enter the project directory
mkdir miva-project-name && cd miva-project-name

# Initialize the project (automatically generates a Hello World example)
miva init miva-project-name --type=bin

# Run the project
miva run
```

### 3. Expected Output

```
Hello, World! # or similar
```

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| `miva: command not found` | Check if the binary has been added to `$PATH` |
| `g++: command not found` | Install a C++ compiler for your platform |
| `opam: command not found` | Refer to the [OCaml Official Installation Guide](https://ocaml.org/docs/install) |

---

## Next Steps

- 📖 Read the [Language Tutorial](./tutorial.md)
- 💬 Join the [Community Discussions](https://github.com/hunter-hongg/miva-lang/discussions)