(defsystem "lisp-chess-engine"
  :description "A chess engine in Common Lisp (SBCL): legal movegen, alpha-beta search, UCI."
  :author "lisp_chess_engine"
  :license "MIT"
  :depends-on ()
  :serial t
  :components ((:file "src/package")
               (:file "src/board")
               (:file "src/state")
               (:file "src/movegen")
               (:file "src/legal")
               (:file "src/eval")
               (:file "src/search")
               (:file "src/uci")
               (:file "src/main"))
  :in-order-to ((test-op (test-op "lisp-chess-engine/tests"))))

(defsystem "lisp-chess-engine/tests"
  :description "Tests for lisp-chess-engine (dependency-free harness, FiveAM later)."
  :depends-on ("lisp-chess-engine")
  :serial t
  :components ((:file "tests/test-framework")
               (:file "tests/test-board")
               (:file "tests/test-state")
               (:file "tests/test-movegen")
               (:file "tests/test-search")))
