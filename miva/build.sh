#!/bin/bash
set -e
cd "$(dirname "$0")/miva"
cargo build "$@" 2>&1 | tail -3
