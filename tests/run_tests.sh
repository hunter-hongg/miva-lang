#!/usr/bin/env zsh
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
VERIFY="${ROOT}/miva-verify/target/release/miva-verify"
MIVA="${ROOT}/miva/target/debug/miva"
FRONTEND="${ROOT}/miva-frontend"

ensure_frontend() {
  if command -v miva-frontend &>/dev/null; then
    return
  fi
  if [[ -x "${FRONTEND}/_build/default/bin/main.exe" ]]; then
    return
  fi
  echo "Building miva-frontend..."
  (cd "$FRONTEND" && dune build) || {
    echo "FAIL: could not build miva-frontend"
    exit 1
  }
}

ensure_miva() {
  if [[ -x "$MIVA" ]]; then
    return
  fi
  echo "Building miva (Rust backend)..."
  (cd "${ROOT}/miva" && cargo build --release --quiet) || {
    echo "FAIL: could not build miva"
    exit 1
  }
}

ensure_verify() {
  if [[ -x "$VERIFY" ]]; then
    return
  fi
  echo "Building miva-verify..."
  (cd "${ROOT}/miva-verify" && cargo build --release --quiet) || {
    echo "FAIL: could not build miva-verify"
    exit 1
  }
}

run_test() {
  local miva_file="$1"
  local expected="$2"
  local name="${miva_file:t:r}"
  local tmpdir=$(mktemp -d "/tmp/miva-test-${name}.XXXXX")
  local exe="${tmpdir}/${name}.exe"

  echo "=== TEST: ${name} ==="

  # Compile
  if ! "$MIVA" sin-build "$miva_file" -o "$exe" &>/dev/null; then
    echo "  FAIL (compilation error)"
    "$MIVA" sin-build "$miva_file" -o "$exe" 2>&1 | sed 's/^/  /'
    rm -rf "$tmpdir"
    return 1
  fi

  # Run and verify
  if ! "$VERIFY" "$exe" "$expected"; then
    rm -rf "$tmpdir"
    return 1
  fi

  rm -rf "$tmpdir"
}

main() {
  ensure_frontend
  ensure_miva
  ensure_verify

  local failures=0
  local total=0

  # Test: Hello World
  total=$((total + 1))
  run_test "${ROOT}/tests/test_hello.miva" "Hello, World" || failures=$((failures + 1))

  # Test: Arithmetic
  total=$((total + 1))
  run_test "${ROOT}/tests/test_arithmetic.miva" "3 7 20 7" || failures=$((failures + 1))

  # Test: Functions
  total=$((total + 1))
  run_test "${ROOT}/tests/test_functions.miva" "7 30 7" || failures=$((failures + 1))

  # Test: For loop
  total=$((total + 1))
  run_test "${ROOT}/tests/test_for.miva" $'0 \n1 \n2' || failures=$((failures + 1))

  echo ""
  if (( failures == 0 )); then
    echo "All ${total} test(s) passed!"
  else
    echo "${failures}/${total} test(s) FAILED"
    exit 1
  fi
}

main "$@"
