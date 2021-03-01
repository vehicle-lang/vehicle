SRC_DIR_BNFC := src/bnfc
GEN_DIR_HS := gen/hs

# NOTE:
#
#   PROJECT_NAME is used in the call to BNFC. The grammar description is assumed
#   to be in SRC_DIR_BNFC/PROJECT_NAME.cf, and the Haskell files are written to
#   GEN_HS/PROJECT_NAME.
#
PROJECT_NAME := Vehicle

# NOTE:
#
#   The call to BNFC creates a number of files, so we're using a multi-target
#   rule. To keep things readable, we first compute a list of the targets, and
#   then prepend the appropriate path.
#
BNFC_TARGETS := Makefile Abs.hs Print.hs Lex.x Layout.hs Par.y Test.hs ErrM.hs Skel.hs Doc.txt
BNFC_TARGETS := $(addprefix $(GEN_DIR_HS)/$(PROJECT_NAME)/,$(BNFC_TARGETS))

$(BNFC_TARGETS): $(SRC_DIR_BNFC)/$(PROJECT_NAME).cf
	bnfc -m -d --haskell --generic --text-token \
	     --outputdir=$(GEN_DIR_HS) \
	     $(SRC_DIR_BNFC)/$(PROJECT_NAME).cf

.PHONY: bnfc
bnfc: $(BNFC_TARGETS)
