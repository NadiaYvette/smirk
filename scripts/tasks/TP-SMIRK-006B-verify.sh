#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-SMIRK-006B Verification Gate] ==="

echo "  [1/2] Running physical verification suite (bin/smirk-verify)..."
bin/smirk-verify

echo "  [2/2] Running full test suite..."
cabal test smirk --test-show-details=streaming

echo "=== [TP-SMIRK-006B Verification Gate PASSED] ==="
