#################################################################################
# Configuration
#################################################################################

SRC_DIR_BNFC := src/bnfc
GEN_DIR_HS   := gen/hs

GHC_VERSION    := 9.0.1
BNFC_VERSION   := 2.9.3
ORMOLU_VERSION := 0.3.1.0

CABAL  ?= cabal
HAPPY  ?= happy
ORMOLU ?= ormolu
BNFC   ?= bnfc


#################################################################################
# Default
#################################################################################

.PHONY: default
default: build


#################################################################################
# Initialise project
#################################################################################

.PHONY: init
init: require-all setup-git-hooks

.PHONY: setup-git-hooks
setup-git-hooks:
	@git config --local core.hooksPath hooks/


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

BNFC_GARBAGE_CORE := Test.hs Skel.hs Doc.txt
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
	$(HAPPY) gen/hs/Vehicle/Core/Par.y --info=gen/hs/Vehicle/Core/Par.info

BNFC_TARGETS_FRONTEND := Abs.hs Lex.x Layout.hs Par.y ErrM.hs
BNFC_TARGETS_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_TARGETS_FRONTEND))

BNFC_GARBAGE_FRONTEND := Test.hs Skel.hs Doc.txt
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
	$(HAPPY) gen/hs/Vehicle/Frontend/Par.y --info=gen/hs/Vehicle/Frontend/Par.info


#################################################################################
# Build type-checker and compiler for Vehicle
#################################################################################

.PHONY: build
build: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND) | require-haskell
	@$(CABAL) build


#################################################################################
# Test Vehicle
#################################################################################

.PHONY: test
test: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND) | require-haskell
	@$(CABAL) test --test-show-details=always --test-options="--color=always"

.PHONY: test-accept
test-accept: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND) | require-haskell
	@$(CABAL) test --test-show-details=always --test-options="--accept --color=always"

#################################################################################
# Test Vehicle
#################################################################################

.PHONY: clean
clean:
	@rm -rf $(GEN_DIR_HS)


#################################################################################
# Dependencies with reasonable error messages
#################################################################################

.PHONY: require-all
require-all: require-haskell require-bnfc require-ormolu

# Haskell
.PHONY: require-haskell
require-haskell:
ifeq (,$(wildcard $(shell which ghc)))
	@echo "Vehicle requires GHC"
	@echo "See: https://www.haskell.org/ghcup/"
	@exit 1
endif
ifeq (,$(wildcard $(shell which cabal)))
	@echo "Vehicle requires Cabal"
	@echo "See: https://www.haskell.org/ghcup/"
	@exit 1
endif

# BNFC - a generator for parsers and printers
.PHONY: require-bnfc
require-bnfc:
ifeq (,$(wildcard $(shell which bnfc)))
	@echo ""
	@echo "Vehicle requires the BNF Converter"
	@echo "See: https://bnfc.digitalgrammars.com/"
	@echo ""
	@echo -n "Would you like to install BNFC? [y/N] " \
		&& read ans && [ $${ans:-N} = y ] \
		&& $(CABAL) v2-install --ignore-project BNFC-$(BNFC_VERSION)
endif

# Ormolu - a Haskell formatter
.PHONY: require-ormolu
require-ormolu:
ifeq (,$(wildcard $(shell which ormolu)))
	@echo ""
	@echo "Vehicle requires the Ormolu Haskell formatter"
	@echo "See: https://github.com/tweag/ormolu"
	@echo ""
	@echo -n "Would you like to install Ormolu? [y/N] " \
		&& read ans && [ $${ans:-N} = y ] \
		&& $(CABAL) v2-install --ignore-project ormolu-$(ORMOLU_VERSION)
endif

