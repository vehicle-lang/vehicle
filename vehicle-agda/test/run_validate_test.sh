#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
FIXTURE_ROOT="$REPO_ROOT/vehicle/tests/golden/specifications/andGate"
AGDA_BIN="${AGDA_BIN:-$(command -v agda || true)}"
VEHICLE_BIN="${VEHICLE_BIN:-$(command -v vehicle || true)}"

if [[ -z "$AGDA_BIN" || ! -x "$AGDA_BIN" ]]; then
  printf 'Agda executable not found. Set AGDA_BIN=/absolute/path/to/agda.\n' >&2
  exit 2
fi

if [[ -z "$VEHICLE_BIN" || ! -x "$VEHICLE_BIN" ]]; then
  printf 'Vehicle executable not found. Set VEHICLE_BIN=/absolute/path/to/vehicle.\n' >&2
  exit 2
fi

AGDA_BIN="$(realpath "$AGDA_BIN")"
VEHICLE_BIN="$(realpath "$VEHICLE_BIN")"
WORK_ROOT="$(mktemp -d)"
trap 'rm -rf "$WORK_ROOT"' EXIT

prepare_case() {
  local name="$1"
  local case_root="$WORK_ROOT/$name"
  local cache_root="$case_root/Marabou.queries"

  mkdir -p "$cache_root"
  cp "$FIXTURE_ROOT/spec.vcl" "$case_root/spec.vcl"
  cp "$FIXTURE_ROOT/fake.onnx" "$case_root/fake.onnx"
  cp "$FIXTURE_ROOT/Marabou.queries/.vcl-cache-index" \
    "$cache_root/.vcl-cache-index"
  cp "$FIXTURE_ROOT/Marabou.queries/andGateCorrect.vcl-result" \
    "$cache_root/andGateCorrect.vcl-result"

  printf '%s\n' "$case_root"
}

generate_agda() {
  local case_root="$1"

  (
    cd "$case_root"
    "$VEHICLE_BIN" compile itp \
      -s spec.vcl \
      -t Agda \
      -o Agda.agda \
      -c Marabou.queries
  )
}

typecheck_agda() {
  local case_root="$1"

  (
    cd "$case_root"
    PATH="$(dirname "$VEHICLE_BIN"):$PATH" \
      "$AGDA_BIN" \
        -WnoUnsupportedIndexedMatch \
        -i . \
        -l vehicle-0.1.0 \
        Agda.agda
  )
}

expect_success() {
  local name="$1"
  local case_root="$2"
  local log="$case_root/agda.log"

  if typecheck_agda "$case_root" >"$log" 2>&1; then
    printf '%-12s passed as expected\n' "$name"
  else
    printf '%-12s unexpectedly failed:\n' "$name" >&2
    tail -80 "$log" >&2
    return 1
  fi
}

expect_failure() {
  local name="$1"
  local case_root="$2"
  local expected="$3"
  local log="$case_root/agda.log"

  if typecheck_agda "$case_root" >"$log" 2>&1; then
    printf '%-12s unexpectedly passed\n' "$name" >&2
    return 1
  elif grep -Fq "$expected" "$log"; then
    printf '%-12s failed as expected\n' "$name"
  else
    printf '%-12s failed for an unexpected reason:\n' "$name" >&2
    tail -80 "$log" >&2
    return 1
  fi
}

failures=0

verified_root="$(prepare_case verified)"
generate_agda "$verified_root"
expect_success verified "$verified_root" || failures=$((failures + 1))

unverified_root="$(prepare_case unverified)"
printf 'False\n' \
  >"$unverified_root/Marabou.queries/andGateCorrect.vcl-result"
generate_agda "$unverified_root"
expect_failure unverified "$unverified_root" "Status: unverified" ||
  failures=$((failures + 1))

missing_root="$(prepare_case missing)"
rm "$missing_root/Marabou.queries/.vcl-cache-index"
generate_agda "$missing_root"
expect_failure missing "$missing_root" ".vcl-cache-index" ||
  failures=$((failures + 1))

stale_root="$(prepare_case stale)"
generate_agda "$stale_root"
printf '\n-- changed after cache generation\n' >>"$stale_root/spec.vcl"
expect_failure stale "$stale_root" "Status: unknown" ||
  failures=$((failures + 1))

malformed_root="$(prepare_case malformed)"
printf 'not-a-boolean\n' \
  >"$malformed_root/Marabou.queries/andGateCorrect.vcl-result"
generate_agda "$malformed_root"
expect_failure malformed "$malformed_root" "Prelude.read" ||
  failures=$((failures + 1))

if ((failures > 0)); then
  printf '\n%d validation case(s) failed.\n' "$failures" >&2
  exit 1
fi

printf '\nAll Agda validation cases behaved as expected.\n'
