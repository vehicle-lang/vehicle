GHC_VERSION := 8.10.3
SRC_DIR_BNFC := src/bnfc
GEN_DIR_HS := gen/hs

#################################################################################
# Default
#################################################################################

.PHONY: default
default: build


#################################################################################
# Initialise project
#################################################################################

# NOTE:
#
#   The init command sets up a few things which only need

.PHONY: init
init:
	@echo "Create stack.yaml for GHC $(GHC_VERSION)"
	@cp stack-$(GHC_VERSION).yaml stack.yaml

#################################################################################
# Format code within project
#################################################################################

.PHONY: format
format: require-ormolu
	@ormolu --mode inplace $(git ls-files '*.hs')

#################################################################################
# Build parsers for Frontend and Core languages using BNFC
#################################################################################

.PHONY: bnfc
bnfc: bnfc-core bnfc-frontend

$(GEN_DIR_HS):
	mkdir -p $(GEN_DIR_HS)


# NOTE:
#
#   The call to BNFC creates multiple files, so we're using a multi-target task.
#   To keep things readable, we first compute the targets for the Frontend and
#   the Core languages, and then define a task for each. The phony bnfc task
#   builds all parsers.
#

BNFC_TARGETS_CORE := Lex.x Par.y ErrM.hs
BNFC_TARGETS_CORE := $(addprefix $(GEN_DIR_HS)/Vehicle/Core/,$(BNFC_TARGETS_CORE))

BNFC_GARBAGE_CORE := Print.hs Test.hs Skel.hs Doc.txt
BNFC_GARBAGE_CORE := $(addprefix $(GEN_DIR_HS)/Vehicle/Core/,$(BNFC_GARBAGE_CORE))

.PHONY: bnfc-core
bnfc-core: $(BNFC_TARGETS_CORE)

$(BNFC_TARGETS_CORE): $(SRC_DIR_BNFC)/Core.cf | require-bnfc $(GEN_DIR_HS)
	bnfc -d --haskell --generic --text-token \
	     --name-space Vehicle \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/Core.cf
	rm -f $(BNFC_GARBAGE_CORE)

BNFC_TARGETS_FRONTEND := Abs.hs Lex.x Layout.hs Par.y ErrM.hs
BNFC_TARGETS_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_TARGETS_FRONTEND))

BNFC_GARBAGE_FRONTEND := Test.hs Skel.hs Doc.txt Print.hs
BNFC_GARBAGE_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_GARBAGE_FRONTEND))

.PHONY: bnfc-frontend
bnfc-frontend: $(BNFC_TARGETS_FRONTEND)

$(BNFC_TARGETS_FRONTEND): $(SRC_DIR_BNFC)/Frontend.cf | require-bnfc $(GEN_DIR_HS)
	bnfc -d --haskell --generic --text-token \
	     --name-space Vehicle \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/Frontend.cf
	rm -f $(BNFC_GARBAGE_FRONTEND)


#################################################################################
# Build type-checker and compiler for Vehicle
#################################################################################

.PHONY: build
build: \
		require-stack require-stack-yaml \
		$(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND)
	stack build


#################################################################################
# Test Vehicle
#################################################################################

.PHONY: test
test: \
		require-stack require-stack-yaml \
		$(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND)
	stack test

# TODO make this into a profile flag
.PHONY: test-profile
test: \
		require-stack require-stack-yaml \
		$(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND)
	stack test --profile --ghc-options="-fexternal-interpreter"

# See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html#using-template-haskell-with-profiling
# for why using template Haskell requires us to pass this extra flag when
# profiling.

#################################################################################
# Test Vehicle
#################################################################################

.PHONY: clean
clean:
	rm -rf $(GEN_DIR_HS)


#################################################################################
# Dependencies with reasonable error messages
#################################################################################

.PHONY: require-stack
require-stack:
ifeq (,$(wildcard $(shell which stack)))
	@echo "The command you called requires the Haskell Tool Stack"
	@echo "See: https://docs.haskellstack.org/en/stable/README/"
	@exit 1
endif

.PHONY: require-stack-yaml
require-stack-yaml:
ifeq (,$(wildcard stack.yaml))
	@echo "The command you called requires a stack.yaml file"
	@echo "Please run 'make init' or create one from one of:"
	@ls stack-*.yaml
	@exit 1
endif

.PHONY: require-bnfc
require-bnfc:
ifeq (,$(wildcard $(shell which bnfc)))
	@echo "The command you called requires the BNF Converter"
	@echo "See: https://bnfc.digitalgrammars.com/"
	@exit 1
endif

.PHONY: require-ormolu
require-ormolu:e
ifeq (,$(wildcard $(shell which ormolu)))
	@echo "The command you called requires the ormolu Haskel formatter"
	@echo "See: https://github.com/tweag/ormolu"
	@exit 1
endif
