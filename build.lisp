;;; build.lisp -- build a standalone executable.
;;; Usage: /usr/bin/sbcl --non-interactive --load build.lisp
(require :asdf)

(push (truename "./") asdf:*central-registry*)

(asdf:load-system "lisp-chess-engine")

(sb-ext:save-lisp-and-die "lisp-chess-engine"
                          :toplevel #'chess:main
                          :executable t)
