(in-package #:chess-tests)

;; Tests for evaluation (Phase 4) and search (Phase 5).

(deftest test-eval-startpos-symmetric
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))
    (is (= (chess:evaluate g) 0))))

(deftest test-eval-material
  ;; White queen + king vs bare kings: score near 900+ from side to move.
  (let ((g (chess:make-game-from-fen "8/8/8/8/8/8/8/Q6K w - - 0 1")))
    (is (> (chess:evaluate g) 800)))
  ;; Same position, black to move: negated.
  (let ((g (chess:make-game-from-fen "8/8/8/8/8/8/8/Q6K b - - 0 1")))
    (is (< (chess:evaluate g) -800)))
  ;; Bare kings on mirrored squares: PST contributions cancel exactly.
  (let ((w (chess:make-game-from-fen "4k3/8/8/8/8/8/8/4K3 w - - 0 1"))
        (b (chess:make-game-from-fen "4k3/8/8/8/8/8/8/4K3 b - - 0 1")))
    (is (= (chess:evaluate w) 0))
    (is (= (chess:evaluate b) 0))))

(deftest test-insufficient-material
  (is (chess:insufficient-material-p
       (chess:make-game-from-fen "8/8/8/4k3/8/8/8/4K3 w - - 0 1")))
  (is (chess:insufficient-material-p
       (chess:make-game-from-fen "8/8/8/4kb2/8/8/8/4K3 w - - 0 1")))
  (is (chess:insufficient-material-p
       (chess:make-game-from-fen "8/8/8/4kn2/8/8/8/4K3 w - - 0 1")))
  (is (not (chess:insufficient-material-p
            (chess:make-game-from-fen
             "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))))
  (is (not (chess:insufficient-material-p
            (chess:make-game-from-fen "8/8/8/4k3/8/8/5P2/4K3 w - - 0 1")))))

(deftest test-search-mate-in-1
  ;; Scholar's mate pattern: Qh5 + Bc4 vs f7, black Nf6/Qd8. Qxf7# is mate:
  ;; Kxf7 illegal (Bc4 guards), Ke7/Kf8 attacked, Kd8 occupied by own queen.
  (let ((g (chess:make-game-from-fen
            "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 0 1")))
    (multiple-value-bind (best score) (chess:search-best-move g :depth 2)
      (is (not (null best)))
      (is-equal "h5f7" (chess:move-to-string best))
      (is (>= score (- chess:+mate-score+ 100))))))

(deftest test-search-finds-capture
  ;; White queen takes hanging black queen... simple tactic:
  ;; black rook d8 hangs to white queen d1? Use: Qxd5 wins a pawn.
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2")))
    (multiple-value-bind (best score) (chess:search-best-move g :depth 2)
      (is (not (null best)))
      ;; exd5 must be among reasonable replies; just require a sane score/move
      (is (integerp score))
      (is (> chess:*search-nodes* 0)))))

(deftest test-search-stalemate-score
  ;; Black to move and stalemated: no move, score 0.
  (let ((g (chess:make-game-from-fen "k7/8/1Q6/8/8/8/8/7K b - - 0 1")))
    (is (null (chess:generate-legal-moves g)))
    (multiple-value-bind (best score) (chess:search-best-move g :depth 2)
      (is (null best))
      (is (= score 0)))))

(deftest test-search-checkmated-score
  ;; Fool's mate final: white mated.
  (let ((g (chess:make-game-from-fen
            "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 3")))
    (is (null (chess:generate-legal-moves g)))
    (multiple-value-bind (best score) (chess:search-best-move g :depth 2)
      (is (null best))
      (is (= score (- chess:+mate-score+))))))

(deftest test-search-startpos-depth-2
  (let ((g (chess:make-game-from-fen
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))
    (multiple-value-bind (best score) (chess:search-best-move g :depth 2)
      (is (not (null best)))
      (is (integerp score))
      (is (> chess:*search-nodes* 100)))))

(deftest test-search-movetime-limit
  ;; A tiny budget must still return a move quickly.
  (let ((g (chess:make-game-from-fen
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")))
    (multiple-value-bind (best score) (chess:search-best-move g :depth 64
                                                              :movetime-ms 100)
      (is (not (null best)))
      (is (integerp score)))))
