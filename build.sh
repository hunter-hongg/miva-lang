#!/bin/bash
# ── Miva 一键构建脚本 ──────────────────────────────────────────────
# 构建前端 (miva-frontend-rs) + 编译器 (miva)
#
# 用法:
#   ./build.sh               # debug 构建
#   ./build.sh --release     # release 构建
#   ./build.sh --test        # debug 构建 + 运行测试
#   ./build.sh --release --test  # release 构建 + 测试
# ──────────────────────────────────────────────────────────────────

set -euo pipefail
cd "$(dirname "$0")"

MODE="debug"
RUN_TESTS=false
CARGO_ARGS=""

for arg in "$@"; do
    case "$arg" in
        --release) MODE="release" ;;
        --test)    RUN_TESTS=true ;;
        --help)
            echo "用法: $0 [--release] [--test]"
            echo "  --release     release 构建"
            echo "  --test        debug 构建并运行测试"
            exit 0
            ;;
        *)
            # 其他参数直接传给 cargo
            CARGO_ARGS="$CARGO_ARGS $arg"
            ;;
    esac
done

CARGO_FLAGS=""
[ "$MODE" = "release" ] && CARGO_FLAGS="--release"

# ── 前端: miva-frontend-rs ───────────────────────────────────────
echo "━━━ Building frontend (miva-frontend-rs) [$MODE] ━━━"
(cd miva-frontend-rs && cargo build $CARGO_FLAGS $CARGO_ARGS)

# ── 编译器: miva ──────────────────────────────────────────────────
echo "━━━ Building compiler (miva) [$MODE] ━━━"
(cd miva && cargo build $CARGO_FLAGS)

# ── 测试 ──────────────────────────────────────────────────────────
if [ "$RUN_TESTS" = true ]; then
    echo ""
    echo "━━━ Running frontend tests ━━━"
    (cd miva-frontend-rs && cargo test $CARGO_FLAGS)

    echo ""
    echo "━━━ Running compiler tests ━━━"
    (cd miva && cargo test $CARGO_FLAGS)
fi

# ── 完成 ──────────────────────────────────────────────────────────
echo ""
echo "✓ 构建完成 [$MODE]"
echo "  前端: miva-frontend-rs/target/$MODE/miva-frontend"
echo "  编译器: miva/target/$MODE/miva"

if [ "$MODE" = "release" ]; then
    echo ""
    echo "  提示: 将 miva-frontend-rs/target/release/miva-frontend 加入 PATH"
    echo "        miva 编译器的 frontend.rs 会自动查找它"
fi
