SRC_DIR_BNFC := src/bnfc
GEN_DIR_HS := gen/hs

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

BNFC_TARGETS_CORE := Print.hs Lex.x Par.y ErrM.hs
BNFC_TARGETS_CORE := $(addprefix $(GEN_DIR_HS)/Vehicle/Core/,$(BNFC_TARGETS_CORE))

BNFC_GARBAGE_CORE := Abs.hs Test.hs Skel.hs Doc.txt
BNFC_GARBAGE_CORE := $(addprefix $(GEN_DIR_HS)/Vehicle/Core/,$(BNFC_GARBAGE_CORE))

.PHONY: bnfc-core
bnfc-core: $(BNFC_TARGETS_CORE)

$(BNFC_TARGETS_CORE): $(SRC_DIR_BNFC)/Core.cf
	bnfc -m -d --haskell --generic --text-token \
	     --name-space Vehicle \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/Core.cf
	rm -f $(BNFC_GARBAGE_CORE)

BNFC_TARGETS_FRONTEND := Abs.hs Print.hs Lex.x Layout.hs Par.y ErrM.hs
BNFC_TARGETS_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_TARGETS_FRONTEND))

BNFC_GARBAGE_FRONTEND := Test.hs Skel.hs Doc.txt
BNFC_GARBAGE_FRONTEND := $(addprefix $(GEN_DIR_HS)/Vehicle/Frontend/,$(BNFC_GARBAGE_FRONTEND))

.PHONY: bnfc-frontend
bnfc-frontend: $(BNFC_TARGETS_FRONTEND)

$(BNFC_TARGETS_FRONTEND): $(SRC_DIR_BNFC)/Frontend.cf
	bnfc -m -d --haskell --generic --text-token \
	     --name-space Vehicle \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/Frontend.cf
	rm -f $(BNFC_GARBAGE_FRONTEND)


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
