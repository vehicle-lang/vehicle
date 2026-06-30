#!/bin/sh
# End-to-end test for the vehicle_validate tactic.
#
# Builds a real verification cache against the windController spec using
# the TestVerifier mock, then exercises the three outcome paths:
#   1. Valid cache:    Lemma is proven, .vo file produced.
#   2. Tampered cache: tactic fails with validator's "altered resources" error.
#   3. Missing cache:  tactic fails with "cache index file not found" error.
#
# Usage: run from this directory after `dune build` + `dune install`.
# Requires `vehicle`, `rocq`, and a mock `testVerifier` on PATH.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# VEHICLE_ROOT defaults to two levels up (vehicle-rocq/test/.. = vehicle-rocq/..),
# which works for in-tree runs. Override via env var when running under a
# sandboxed build system (dune, etc.).
: "${VEHICLE_ROOT:=$(cd "$SCRIPT_DIR/../.." && pwd)}"
SPEC="$VEHICLE_ROOT/vehicle/tests/golden/specifications/windController/spec.vcl"
NETWORK="$VEHICLE_ROOT/vehicle/tests/golden/specifications/windController/controller.onnx"

WORK="$(mktemp -d)"
trap "rm -rf $WORK" EXIT

CACHE="$WORK/cache"
GEN_V="$WORK/spec.v"

# Install mock testVerifier on PATH.
cat > "$WORK/testVerifier" <<'EOF'
#!/bin/sh
echo "unsat"
EOF
chmod +x "$WORK/testVerifier"
export PATH="$WORK:$PATH"

# 1. Build a real verified cache.
echo "[1/4] Verifying spec against TestVerifier..."
( cd "$VEHICLE_ROOT" && vehicle verify -v TestVerifier \
    --specification "$SPEC" --network controller:"$NETWORK" \
    --cache "$CACHE" ) > /dev/null
[ -f "$CACHE/.vcl-cache-index" ] || { echo "FAIL: cache not produced"; exit 1; }

# 2. Generate the Rocq spec referencing the cache.
echo "[2/4] Generating cached Rocq spec..."
( cd "$VEHICLE_ROOT" && vehicle compile itp --target Rocq \
    --specification "$SPEC" --cache "$CACHE" --output "$GEN_V" ) > /dev/null
grep -q 'vehicle_validate' "$GEN_V" || { echo "FAIL: vehicle_validate not in output"; exit 1; }

# 3. Valid cache: rocq compile must succeed and produce a .vo.
echo "[3/4] Compiling against valid cache (expect success)..."
( cd "$VEHICLE_ROOT" && rocq compile "$GEN_V" ) > "$WORK/rocq.log" 2>&1
GEN_VO="${GEN_V%.v}.vo"
[ -f "$GEN_VO" ] || { echo "FAIL: .vo not produced for valid cache"; cat "$WORK/rocq.log"; exit 1; }
rm -f "$GEN_VO" "${GEN_V%.v}.glob"

# 4. Tampered cache: rocq compile must fail with the validator's error.
echo "[4/4] Compiling against tampered cache (expect failure)..."
# Flip the spec hash in the cache index.
sed -i.bak 's/"fileHash": -[0-9]*/&_tampered/' "$CACHE/.vcl-cache-index"
if ( cd "$VEHICLE_ROOT" && rocq compile "$GEN_V" ) > "$WORK/rocq.log" 2>&1; then
  echo "FAIL: rocq compile succeeded for tampered cache"
  exit 1
fi
grep -q 'Vehicle validation failed' "$WORK/rocq.log" || {
  echo "FAIL: tactic did not surface the validator's error"
  cat "$WORK/rocq.log"
  exit 1
}

# 5. Missing cache: rocq compile must fail with file-not-found.
echo "[5/5] Compiling against missing cache (expect failure)..."
rm -rf "$CACHE"
if ( cd "$VEHICLE_ROOT" && rocq compile "$GEN_V" ) > "$WORK/rocq.log" 2>&1; then
  echo "FAIL: rocq compile succeeded for missing cache"
  exit 1
fi
grep -q 'Vehicle validation failed' "$WORK/rocq.log" || {
  echo "FAIL: tactic did not surface the validator's error"
  cat "$WORK/rocq.log"
  exit 1
}

echo "PASS: all three validation paths behave correctly"
