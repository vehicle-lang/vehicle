#################################################################################
# Configuration
#################################################################################

SRC_DIR_BNFC := src/bnfc
GEN_DIR_HS   := gen/hs

GHC_VERSION := 8.10.3

STACK  ?= stack
ORMOLU ?= ormolu
BNFC   ?= bnfc


# Add profiling flags if STACK_PROFILE is set:
ifneq (,$(wildcard $(STACK_PROFILE)))
# To profile code with Template Haskell, you must to pass -fexternal-interpreter.
# For details, see:
# https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/template_haskell.html#using-template-haskell-with-profiling
STACK := $(STACK) --profile --ghc-options="-fexternal-interpreter"
endif


#################################################################################
# Default
#################################################################################

.PHONY: default
default: build

#################################################################################
# Initialise project
#################################################################################

.PHONY: init
init: stack.yaml

stack.yaml: stack-$(GHC_VERSION).yaml
	@echo "Using stack configuration for GHC $(GHC_VERSION)"
	@cp stack-$(GHC_VERSION).yaml stack.yaml


#################################################################################
# Format code within project
#################################################################################

.PHONY: format
format: require-ormolu
	@echo "Format Haskell code using Ormolu"
	@$(ORMOLU) --mode inplace --cabal-default-extensions $(shell git ls-files '*.hs')

.PHONY: format-check
format-check: require-ormolu
	@echo "Check Haskell code using Ormolu"
	@$(ORMOLU) --mode check --cabal-default-extensions $(shell git ls-files '*.hs')

#################################################################################
# Build parsers for Frontend and Core languages using BNFC
#################################################################################

.PHONY: bnfc
bnfc: bnfc-core bnfc-frontend

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

$(BNFC_TARGETS_CORE): $(SRC_DIR_BNFC)/Core.cf | require-bnfc
	@mkdir -p $(GEN_DIR_HS)
	@$(BNFC)										\
		-d												\
		--haskell									\
		--generic									\
		--text-token							\
		--name-space Vehicle			\
		--outputdir=$(GEN_DIR_HS) \
		$(SRC_DIR_BNFC)/Core.cf
	@rm -f $(BNFC_GARBAGE_CORE)

.PHONY: bnfc-core-info
bnfc-core-info: $(GEN_DIR_HS)/Vehicle/Core/Par.info

$(GEN_DIR_HS)/Vehicle/Core/Par.info: $(GEN_DIR_HS)/Vehicle/Core/Par.y
	$(STACK) exec happy -- gen/hs/Vehicle/Core/Par.y --info=gen/hs/Vehicle/Core/Par.info

BNFC_TARGETS_FRONTEND := Abs.hs Lex.x Layout.hs Par.y ErrM.hs
BNFC_TARGETS_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_TARGETS_FRONTEND))

BNFC_GARBAGE_FRONTEND := Test.hs Skel.hs Doc.txt Print.hs
BNFC_GARBAGE_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_GARBAGE_FRONTEND))

.PHONY: bnfc-frontend
bnfc-frontend: $(BNFC_TARGETS_FRONTEND)

$(BNFC_TARGETS_FRONTEND): $(SRC_DIR_BNFC)/Frontend.cf | require-bnfc
	@mkdir -p $(GEN_DIR_HS)
	@$(BNFC)										\
		-d												\
		--haskell									\
		--generic									\
		--text-token							\
		--name-space Vehicle			\
		--outputdir=$(GEN_DIR_HS) \
		$(SRC_DIR_BNFC)/Frontend.cf
	@rm -f $(BNFC_GARBAGE_FRONTEND)

.PHONY: bnfc-frontend-info
bnfc-frontend-info: $(GEN_DIR_HS)/Vehicle/Frontend/Par.info

$(GEN_DIR_HS)/Vehicle/Frontend/Par.info: $(GEN_DIR_HS)/Vehicle/Frontend/Par.y
	$(STACK) exec happy -- gen/hs/Vehicle/Frontend/Par.y --info=gen/hs/Vehicle/Frontend/Par.info

#################################################################################
# Build type-checker and compiler for Vehicle
#################################################################################

.PHONY: build
build: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND) stack.yaml | require-stack
	@$(STACK) build


#################################################################################
# Test Vehicle
#################################################################################

.PHONY: test
test: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND) stack.yaml | require-stack
	@$(STACK) test


#################################################################################
# Test Vehicle
#################################################################################

.PHONY: clean
clean:
	@rm -rf $(GEN_DIR_HS)


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

.PHONY: require-bnfc
require-bnfc:
ifeq (,$(wildcard $(shell which bnfc)))
	@echo "The command you called requires the BNF Converter"
	@echo "See: https://bnfc.digitalgrammars.com/"
	@exit 1
endif

.PHONY: require-ormolu
require-ormolu:
ifeq (,$(wildcard $(shell which ormolu)))
	@echo "The command you called requires the ormolu Haskel formatter"
	@echo "See: https://github.com/tweag/ormolu"
	@exit 1
endif
