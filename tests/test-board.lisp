(in-package #:chess-tests)

;; Tests for src/board.lisp: constants, setup, coords, copy.

(deftest test-piece-constants
  (is (= +empty+ 0))
  (is (= +w-pawn+ 1))
  (is (= +b-pawn+ -1))
  (is (= +w-king+ 6))
  (is (= +b-king+ -6))
  (is (= +castle-all+ 15))
  (is (= +castle-none+ 0)))

(deftest test-initial-position-pieces
  (let ((g (make-chess-game)))
    (setup-initial-position g)
    (let ((b (chess-game-board g)))
      ;; black back rank
      (is (= (aref b 0 0) +b-rook+))
      (is (= (aref b 0 1) +b-knight+))
      (is (= (aref b 0 2) +b-bishop+))
      (is (= (aref b 0 3) +b-queen+))
      (is (= (aref b 0 4) +b-king+))
      (is (= (aref b 0 5) +b-bishop+))
      (is (= (aref b 0 6) +b-knight+))
      (is (= (aref b 0 7) +b-rook+))
      ;; black pawns
      (loop for c from 0 below 8 do (is (= (aref b 1 c) +b-pawn+)))
      ;; empty middle
      (loop for r from 2 to 5 do
        (loop for c from 0 below 8 do (is (= (aref b r c) +empty+))))
      ;; white pawns + back rank
      (loop for c from 0 below 8 do (is (= (aref b 6 c) +w-pawn+)))
      (is (= (aref b 7 4) +w-king+))
      (is (= (aref b 7 3) +w-queen+))
      ;; full state reset
      (is (eq (chess-game-side-to-move g) :white))
      (is (= (chess-game-castling-rights g) +castle-all+))
      (is (null (chess-game-en-passant g)))
      (is (= (chess-game-halfmove-clock g) 0))
      (is (= (chess-game-fullmove-number g) 1)))))

(deftest test-clear-board
  (let ((g (make-chess-game)))
    (setup-initial-position g)
    (clear-board g)
    (loop for i from 0 below 64 do
      (is (= (row-major-aref (chess-game-board g) i) +empty+)))
    (is (= (chess-game-castling-rights g) +castle-none+))
    (is (null (chess-game-en-passant g)))))

(deftest test-coords-roundtrip
  ;; every square round-trips algebraic <-> coords
  (loop for r from 0 below 8 do
    (loop for c from 0 below 8 do
      (let* ((sq (coords-to-algebraic r c)))
        (multiple-value-bind (rr cc) (algebraic-to-coords sq)
          (is (and (= rr r) (= cc c)) sq)))))
  ;; known squares
  (multiple-value-bind (r c) (algebraic-to-coords "e4")
    (is (= r 4)) (is (= c 4)))
  (multiple-value-bind (r c) (algebraic-to-coords "a8")
    (is (= r 0)) (is (= c 0)))
  (multiple-value-bind (r c) (algebraic-to-coords "h1")
    (is (= r 7)) (is (= c 7)))
  (is-equal "e1" (coords-to-algebraic 7 4))
  (is (null (algebraic-to-coords "i9")))
  (is (null (algebraic-to-coords "e")))
  (is (not (valid-coords-p 8 0)))
  (is (not (valid-coords-p 0 8)))
  (is (valid-coords-p 0 0)))

(deftest test-piece-helpers
  (is (eq (piece-color +w-pawn+) :white))
  (is (eq (piece-color +b-queen+) :black))
  (is (null (piece-color +empty+)))
  (is (= (piece-type +b-knight+) 2))
  (is (eq (opposite-color :white) :black))
  (is (eq (opposite-color :black) :white))
  (is (castling-rights-has-p +castle-all+ +castle-wk+))
  (is (not (castling-rights-has-p +castle-none+ +castle-wk+))))

(deftest test-copy-game-is-deep
  (let* ((g (make-game-from-fen
             "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))
         (h (copy-game g)))
    (is-equal (game-to-fen g) (game-to-fen h))
    ;; mutating the copy must not affect the original board or EP
    (setf (aref (chess-game-board h) 6 4) +empty+)
    (is (= (aref (chess-game-board g) 6 4) +w-pawn+))
    (is (= (aref (chess-game-board h) 6 4) +empty+))))
