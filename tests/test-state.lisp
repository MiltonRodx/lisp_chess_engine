(in-package #:chess-tests)

;; Tests for src/state.lisp: FEN, attack detection, check.

(defparameter *startpos-fen*
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
(defparameter *kiwipete-fen*
  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
(defparameter *ep-fen*
  "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")
(defparameter *empty-fen*
  "8/8/8/8/8/8/8/8 w - - 0 1")

(deftest test-fen-startpos-roundtrip
  (let ((g (make-game-from-fen *startpos-fen*)))
    (is (eq (chess-game-side-to-move g) :white))
    (is (= (chess-game-castling-rights g) +castle-all+))
    (is (null (chess-game-en-passant g)))
    (is-equal *startpos-fen* (game-to-fen g))))

(deftest test-fen-variants-roundtrip
  (is-equal *kiwipete-fen* (game-to-fen (make-game-from-fen *kiwipete-fen*)))
  (is-equal *empty-fen* (game-to-fen (make-game-from-fen *empty-fen*)))
  ;; black to move, no castling, clocks preserved
  (let ((fen "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"))
    (is-equal fen (game-to-fen (make-game-from-fen fen))))
  ;; partial castling rights
  (let ((g (make-game-from-fen "r3k2r/8/8/8/8/8/8/R3K2R w Kq - 0 1")))
    (is (castling-rights-has-p (chess-game-castling-rights g) +castle-wk+))
    (is (not (castling-rights-has-p (chess-game-castling-rights g) +castle-wq+)))
    (is (not (castling-rights-has-p (chess-game-castling-rights g) +castle-bk+)))
    (is (castling-rights-has-p (chess-game-castling-rights g) +castle-bq+))
    (is-equal "r3k2r/8/8/8/8/8/8/R3K2R w Kq - 0 1" (game-to-fen g))))

(deftest test-fen-en-passant
  (let ((g (make-game-from-fen *ep-fen*)))
    ;; d6 = row 2, col 3
    (is (equal (chess-game-en-passant g) '(2 . 3)))
    (is-equal *ep-fen* (game-to-fen g))))

(deftest test-fen-malformed-signals-error
  (is (signals-error-p (make-game-from-fen "8/8/8/8/8/8/8/8 w - - 0")))
  (is (signals-error-p (make-game-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR x KQkq - 0 1")))
  (is (signals-error-p (make-game-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNX w KQkq - 0 1")))
  (is (signals-error-p (make-game-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1")))
  (is (signals-error-p (make-game-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 0"))))

(deftest test-square-attacked-pawn
  ;; White pawn on e4 attacks d5 and f5, not e5.
  (let ((g (make-game-from-fen "8/8/8/8/4P3/8/8/8 w - - 0 1")))
    (multiple-value-bind (rd cd) (algebraic-to-coords "d5")
      (multiple-value-bind (rf cf) (algebraic-to-coords "f5")
        (multiple-value-bind (re ce) (algebraic-to-coords "e5")
          (is (square-attacked-p (chess-game-board g) rd cd :white))
          (is (square-attacked-p (chess-game-board g) rf cf :white))
          (is (not (square-attacked-p (chess-game-board g) re ce :white)))
          (is (not (square-attacked-p (chess-game-board g) rd cd :black))))))
  ;; Black pawn on e5 attacks d4 and f4.
  (let ((g (make-game-from-fen "8/8/8/4p3/8/8/8/8 b - - 0 1")))
    (multiple-value-bind (rd cd) (algebraic-to-coords "d4")
      (is (square-attacked-p (chess-game-board g) rd cd :black))
      (is (not (square-attacked-p (chess-game-board g) rd cd :white)))))))

(deftest test-square-attacked-knight-king
  ;; Knight on f3 attacks e5, g5, h4, h2, g1, e1, d2, d4.
  (let ((g (make-game-from-fen "8/8/8/8/8/5N2/8/8 w - - 0 1")))
    (dolist (sq '("e5" "g5" "h4" "h2" "g1" "e1" "d2" "d4"))
      (multiple-value-bind (r c) (algebraic-to-coords sq)
        (is (square-attacked-p (chess-game-board g) r c :white) sq)))
    (multiple-value-bind (r c) (algebraic-to-coords "f4")
      (is (not (square-attacked-p (chess-game-board g) r c :white))))))

(deftest test-square-attacked-sliding-blocked
  ;; Rook e8 vs king e1: open file => attacked; pawn on e5 blocks => not attacked.
  (let ((open (make-game-from-fen "4r3/8/8/8/8/8/8/4K3 w - - 0 1"))
        (blocked (make-game-from-fen "4r3/8/8/8/4P3/8/8/4K3 w - - 0 1")))
    (multiple-value-bind (r c) (algebraic-to-coords "e1")
      (is (square-attacked-p (chess-game-board open) r c :black))
      (is (not (square-attacked-p (chess-game-board blocked) r c :black)))))
  ;; Bishop c4 attacks f7 diagonally; an enemy pawn on e6 blocks it
  ;; (note: a *white* pawn on e6 would itself attack f7, so use a black pawn).
  (let ((open (make-game-from-fen "8/8/8/8/2B5/8/8/8 w - - 0 1")))
    (multiple-value-bind (r c) (algebraic-to-coords "f7")
      (is (square-attacked-p (chess-game-board open) r c :white))))
  (let ((blocked (make-game-from-fen "8/8/4p3/8/2B5/8/8/8 w - - 0 1")))
    (multiple-value-bind (r c) (algebraic-to-coords "f7")
      ;; black pawn sits on e6 which blocks the diagonal c4-f7
      ;; (and a black pawn on e6 attacks d5/f5, not f7)
      (is (not (square-attacked-p (chess-game-board blocked) r c :white))))))

(deftest test-in-check
  ;; startpos: nobody in check
  (let ((g (make-game-from-fen *startpos-fen*)))
    (is (not (in-check-p g :white)))
    (is (not (in-check-p g :black))))
  ;; open rook file: white in check, black not
  (let ((g (make-game-from-fen "4r3/8/8/8/8/8/8/4K3 w - - 0 1")))
    (is (in-check-p g :white))
    (is (not (in-check-p g :black))))
  ;; blocked: no check
  (let ((g (make-game-from-fen "4r3/8/8/8/8/4P3/8/4K3 w - - 0 1")))
    (is (not (in-check-p g :white))))
  ;; knight check: black knight d3 checks white king e1
  (let ((g (make-game-from-fen "8/8/8/8/8/3n4/8/4K3 w - - 0 1")))
    (is (in-check-p g :white)))
  ;; fool's mate final: white mated (in check)
  (let ((g (make-game-from-fen
            "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 3")))
    (is (in-check-p g :white))
    (is (not (in-check-p g :black))))
  ;; empty board: no king => NIL (not an error)
  (let ((g (make-game-from-fen *empty-fen*)))
    (is (not (in-check-p g :white)))
    (is (not (in-check-p g :black)))))
