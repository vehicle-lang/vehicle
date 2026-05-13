# Build the Rocq proof for the windController example.
#
# Run from this directory so that the relative paths inside
# `verificationResult/.vcl-cache-index` (windController.vcl, controller.onnx)
# resolve correctly when the `vehicle_validate` tactic invokes
# `vehicle validate --cache=verificationResult` at proof-checking time.

ROCQ ?= rocq
SPEC := rocqProof/WindControllerSpec.v
PROOF := rocqProof/SafetyProof.v

all: $(SPEC:.v=.vo) $(PROOF:.v=.vo)

# The spec depends on the cache (via vehicle_validate) — Rocq won't see this
# dependency itself, so we encode it here.
$(SPEC:.v=.vo): $(SPEC) verificationResult/.vcl-cache-index windController.vcl controller.onnx
	$(ROCQ) compile -Q rocqProof windController $<

$(PROOF:.v=.vo): $(PROOF) $(SPEC:.v=.vo)
	$(ROCQ) compile -Q rocqProof windController $<

clean:
	rm -f rocqProof/*.vo rocqProof/*.vos rocqProof/*.vok rocqProof/*.glob

.PHONY: all clean
