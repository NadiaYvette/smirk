#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCH_FILE="${SCRIPT_DIR}/TP-SMIRK-002D.patch"

if [ -f "${PATCH_FILE}" ]; then
  echo "=== [yamaarashi-task] Applying snapshot patch: ${PATCH_FILE} ==="
  git apply --whitespace=nowarn "${PATCH_FILE}"
fi
