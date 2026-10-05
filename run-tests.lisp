;;; run-tests.lisp -- non-interactive test entry point.
;;; Usage: /usr/bin/sbcl --non-interactive --load run-tests.lisp
(require :asdf)

;; Register this repo so ASDF can find the .asd files.
(push (truename "./") asdf:*central-registry*)

(asdf:load-system "lisp-chess-engine/tests")

(multiple-value-bind (passed failed) (chess-tests:run-all-tests)
  (declare (ignore passed))
  (finish-output)
  (sb-ext:exit :code (if (zerop failed) 0 1)))
