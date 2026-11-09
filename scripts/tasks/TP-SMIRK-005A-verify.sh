#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-SMIRK-005A Verification Gate] ==="

echo "  [1/2] Running physical verification suite (bin/smirk-verify)..."
bin/smirk-verify

echo "  [2/2] Running full test suite..."
cabal test smirk-penelope --test-show-details=streaming

echo "=== [TP-SMIRK-005A Verification Gate PASSED] ==="
