#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCH_FILE="${SCRIPT_DIR}/TP-SMIRK-002E.patch"

if [ -f "${PATCH_FILE}" ]; then
  echo "=== [yamaarashi-task] Applying snapshot patch: ${PATCH_FILE} ==="
  if git apply --check "${PATCH_FILE}" >/dev/null 2>&1; then
    git apply --whitespace=nowarn "${PATCH_FILE}"
  else
    echo "=== [yamaarashi-task] Patch already present in branch/worktree ==="
  fi
fi
