# Makefile for Emacs config in ~/.emacs.d

EMACS   = emacs
ELDIRS  = core engine modes
EXCLUDE = engine/rh-capitalize.el
SRC     = $(filter-out $(EXCLUDE), $(filter-out init.el early-init.el, $(wildcard init.el early-init.el) $(shell find $(ELDIRS) -type f -name "*.el")))

.PHONY: all native clean rebuild clean-eln clean-elc

all: native

native:
	@echo "Native compiling all Emacs Lisp files..."
	$(EMACS) --batch -l init.el --eval \
	"(mapc #'native-compile '($(patsubst %, \"%\", $(SRC))))"
	@echo "Native compilation complete."

clean: clean-eln clean-elc

clean-eln:
	@echo "Deleting .eln files from eln-cache..."
	find ~/.emacs.d/eln-cache -type f -name "*.eln" -delete
	@echo ".eln files deleted."

clean-elc:
	@echo "Deleting all .elc files..."
	find . -type f -name "*.elc" -delete
	@echo ".elc files deleted."

rebuild: clean native
