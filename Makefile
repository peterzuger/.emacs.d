ifeq ($(VERBOSE),1)
	Q =
	RM   = rm -v
	MAKE = make
else
	Q = @
	RM   = @rm
	MAKE = $(Q)make -s
endif

EMACS = $(Q)emacs
MU    = $(Q)mu
ECHO  = @echo -e


all: init.elc

%.elc: %.el
	$(ECHO) "EMACS\t$@"
	$(EMACS) --batch --eval '(byte-compile-file "$<")'

mu:
	$(ECHO) "MU \tinit"
	$(MU) init --my-address=${EMAIL} --maildir ${HOME}/Mail

.PHONY: clean
clean:
	$(RM) -f *.elc
