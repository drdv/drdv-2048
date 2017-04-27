EMACS=emacs
CASK=cask
PACKAGE-NAME=drdv-2048.el

DELAY=10
OUTPUT_DIR=./output

all: build

checkdoc:
	@$(EMACS) -Q --batch -f "checkdoc" ${PACKAGE-NAME}

package-lint: cask
	${CASK} exec $(EMACS) -Q --batch -l "package-lint.el" \
	-f "package-lint-batch-and-exit" ${PACKAGE-NAME}

build: checkdoc package-lint
	${CASK} exec  $(EMACS) -Q --batch \
	--eval "(progn \
	           (setq byte-compile-error-on-warn t)  \
	           (batch-byte-compile))" ${PACKAGE-NAME}

cask:
	${CASK} install

test: cask build
	${CASK} exec ert-runner

gif:
	convert -delay ${DELAY} ${OUTPUT_DIR}/image-*.jpg ${OUTPUT_DIR}/animated.gif

clean :
	@rm -f *.elc
	@rm -rf .cask
	@cd ${OUTPUT_DIR} && make clean

.PHONY:	all checkdoc package-lint build cask gif clean
