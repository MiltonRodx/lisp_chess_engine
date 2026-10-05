SBCL ?= /usr/bin/sbcl

.PHONY: test build repl uci perft clean

test:
	$(SBCL) --non-interactive --load run-tests.lisp

build:
	$(SBCL) --non-interactive --load build.lisp

repl:
	$(SBCL) --eval "(progn (require :asdf) (push (truename \"./\") asdf:*central-registry*) (asdf:load-system \"lisp-chess-engine\") (chess:play-repl))"

uci: build
	./lisp-chess-engine --uci

perft:
	./lisp-chess-engine --perft 4

clean:
	rm -f lisp-chess-engine *.fasl
	rm -rf ~/.cache/common-lisp/*lisp_chess_engine*
