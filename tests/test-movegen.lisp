(in-package #:chess-tests)

;; Tests for movegen (Phase 2) and make/unmake + perft (Phase 3).

(defun %legal-strings (game)
  (sort (mapcar #'chess:move-to-string (chess:generate-legal-moves game))
        #'string<))

(deftest test-startpos-move-count
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))
    (is (= (length (chess:generate-legal-moves g)) 20))
    ;; every root move is a pawn push or knight jump
    (dolist (m (chess:generate-legal-moves g))
      (is (member (abs (aref (chess:chess-game-board g)
                             (chess:move-from-row m) (chess:move-from-col m)))
                  '(1 2))))))

(deftest test-perft-startpos
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))
    (is (= (chess:perft g 1) 20))
    (is (= (chess:perft g 2) 400))
    (is (= (chess:perft g 3) 8902))
    (is (= (chess:perft g 4) 197281))))

(deftest test-perft-kiwipete
  (let ((g (chess:make-game-from-fen
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")))
    (is (= (chess:perft g 1) 48))
    (is (= (chess:perft g 2) 2039))
    (is (= (chess:perft g 3) 97862))))

(deftest test-perft-endgame-ep
  ;; Position 3 (en passant + pins): perft 1-3.
  (let ((g (chess:make-game-from-fen
            "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")))
    (is (= (chess:perft g 1) 14))
    (is (= (chess:perft g 2) 191))
    (is (= (chess:perft g 3) 2812))))

(deftest test-make-unmake-restores-fen
  (let ((start "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
        (g (chess:make-game-from-fen
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")))
    ;; play the first 6 legal moves in a line, then unmake all: FEN must match
    (let ((line (subseq (chess:generate-legal-moves g) 0 6))
          (played nil))
      (dolist (m line)
        (chess:make-move-on-board g m)
        (push m played))
      (is (not (string= (chess:game-to-fen g) start)))
      (dolist (m played)
        (chess:unmake-move-on-board g m))
      (is-equal start (chess:game-to-fen g)))))

(deftest test-en-passant-capture
  ;; White pawn e5, black just pushed d7-d5: exd6 e.p. must exist.
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")))
    (let* ((strs (%legal-strings g)))
      (is (member "e5d6" strs :test #'string=)))
    ;; playing it removes the d5 pawn
    (let* ((parsed (chess:parse-move-string "e5d6" :white))
           (found (find-if (lambda (m) (chess:move-matches-p m parsed))
                           (chess:generate-legal-moves g))))
      (is (not (null found)))
      (is (chess:move-en-passant-p found))
      (chess:make-move-on-board g found)
      (multiple-value-bind (r c) (chess:algebraic-to-coords "d5")
        (is (= (aref (chess:chess-game-board g) r c) chess:+empty+)))
      (multiple-value-bind (r c) (chess:algebraic-to-coords "d6")
        (is (= (aref (chess:chess-game-board g) r c) chess:+w-pawn+))))))

(deftest test-castling-generation
  ;; Open back rank, full rights: both castles available.
  (let ((g (chess:make-game-from-fen
            "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1")))
    (let ((strs (%legal-strings g)))
      (is (member "e1g1" strs :test #'string=))
      (is (member "e1c1" strs :test #'string=))))
  ;; Pieces between -> no castling.
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))
    (let ((strs (%legal-strings g)))
      (is (not (member "e1g1" strs :test #'string=)))
      (is (not (member "e1c1" strs :test #'string=)))))
  ;; King in check -> no castling even with rights and space.
  (let ((g (chess:make-game-from-fen "4r3/8/8/8/8/8/8/R3K2R w KQ - 0 1")))
    (is (chess:in-check-p g :white))
    (let ((strs (%legal-strings g)))
      (is (not (member "e1g1" strs :test #'string=)))
      (is (not (member "e1c1" strs :test #'string=))))))

(deftest test-promotion-moves
  ;; Pawn a7 with empty a8/b8: 4 quiet promotions.
  (let ((g (chess:make-game-from-fen "8/P7/8/8/8/1k6/8/4K3 w - - 0 1")))
    (let ((promos (remove-if-not #'chess:move-promotion
                                 (chess:generate-legal-moves g))))
      (is (= (length promos) 4))
      (is (member "a7a8q" (mapcar #'chess:move-to-string promos) :test #'string=))))
  ;; Pawn captures to promote: a7xb8=N style (black knight on b8).
  (let ((g (chess:make-game-from-fen "1n6/P7/8/8/8/1k6/8/4K3 w - - 0 1")))
    (let ((strs (%legal-strings g)))
      (is (member "a7b8n" strs :test #'string=))
      (is (member "a7b8q" strs :test #'string=)))))

(deftest test-pinned-piece-cannot-move
  ;; Knight e2 pinned to king e1 by rook e8: no legal knight moves.
  (let ((g (chess:make-game-from-fen "4r3/8/8/8/8/8/4N3/4K3 w - - 0 1")))
    (let ((knight-moves (remove-if-not
                         (lambda (m)
                           (and (= (chess:move-from-row m) 6)
                                (= (chess:move-from-col m) 4)))
                         (chess:generate-legal-moves g))))
      (is (null knight-moves)))))

(deftest test-check-evasion-count
  ;; King e1 checked by rook e8 down an open file: Kd2, Ke-file illegal...
  ;; legal: d2, d1, f1, f2.
  (let ((g (chess:make-game-from-fen "4r3/8/8/8/8/8/8/4K3 w - - 0 1")))
    (is (chess:in-check-p g :white))
    (is-equal '("e1d1" "e1d2" "e1f1" "e1f2") (%legal-strings g))))

(deftest test-move-string-roundtrip
  (is-equal "e2e4" (chess:move-to-string (chess:parse-move-string "e2e4" :white)))
  (is-equal "e7e8q" (chess:move-to-string (chess:parse-move-string "e7e8q" :black)))
  (is-equal "e1g1" (chess:move-to-string (chess:parse-move-string "e1g1" :white)))
  ;; promotion sign follows color
  (let ((w (chess:parse-move-string "a7a8q" :white))
        (b (chess:parse-move-string "a2a1q" :black)))
    (is (= (chess:move-promotion w) chess:+w-queen+))
    (is (= (chess:move-promotion b) chess:+b-queen+))))
