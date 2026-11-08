#!/usr/bin/env bash
set -euo pipefail

echo "=== [yamaarashi-verify] Running verification gate for TP-SMIRK-002D ==="
bin/smirk-verify
