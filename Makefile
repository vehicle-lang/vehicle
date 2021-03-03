SRC_DIR_BNFC := src/bnfc
GEN_DIR_HS := gen/hs

#################################################################################
# Build parsers for Frontend and Core languages using BNFC
#################################################################################

BNFC_TARGETS := Abs.hs Print.hs Lex.x Layout.hs Par.y Test.hs ErrM.hs Skel.hs Doc.txt

.PHONY: bnfc
bnfc: bnfc-core bnfc-frontend


# NOTE:
#
#   The call to BNFC creates multiple files, so we're using a multi-target task.
#   To keep things readable, we first compute the targets for the Frontend and
#   the Core languages, and then define a task for each. The phony bnfc task
#   builds all parsers.
#

BNFC_TARGETS_CORE := $(addprefix $(GEN_DIR_HS)/Vehicle/Core/,$(BNFC_TARGETS))

.PHONY: bnfc-core
bnfc-core: $(BNFC_TARGETS_CORE)

$(BNFC_TARGETS_CORE): $(SRC_DIR_BNFC)/Core.cf
	bnfc -m -d --haskell --generic --text-token \
	     --name-space Vehicle \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/Core.cf

BNFC_TARGETS_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_TARGETS))

.PHONY: bnfc-frontend
bnfc-frontend: $(BNFC_TARGETS_FRONTEND)

$(BNFC_TARGETS_FRONTEND): $(SRC_DIR_BNFC)/Frontend.cf
	bnfc -m -d --haskell --generic --text-token \
	     --name-space Vehicle \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/Frontend.cf


#################################################################################
# Build type-checker and compiler for Vehicle
#################################################################################

.PHONY: build
build: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND)
	stack build


#################################################################################
# Test Vehicle
#################################################################################

.PHONY: test
test: $(BNFC_TARGETS_CORE) $(BNFC_TARGETS_FRONTEND)
	stack test


#################################################################################
# Test Vehicle
#################################################################################

.PHONY: clean
clean:
	rm -rf $(GEN_DIR_HS)
