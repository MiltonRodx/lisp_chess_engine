(in-package #:chess)

;; ==========================================
;; Phase 4: static evaluation (material + piece-square tables).
;; Tables are row-major with row 0 = rank 8 (Michniewski values).
;; White indexes (row col) directly; black mirrors vertically (7-row).
;; EVALUATE returns centipawns from the side-to-move's perspective.
;; ==========================================

(defparameter +pst-pawn+
  #(0 0 0 0 0 0 0 0
    50 50 50 50 50 50 50 50
    10 10 20 30 30 20 10 10
    5 5 10 25 25 10 5 5
    0 0 0 20 20 0 0 0
    5 -5 -10 0 0 -10 -5 5
    5 10 10 -20 -20 10 10 5
    0 0 0 0 0 0 0 0))

(defparameter +pst-knight+
  #(-50 -40 -30 -30 -30 -30 -40 -50
    -40 -20 0 0 0 0 -20 -40
    -30 0 10 15 15 10 0 -30
    -30 5 15 20 20 15 5 -30
    -30 0 15 20 20 15 0 -30
    -30 5 10 15 15 10 5 -30
    -40 -20 0 5 5 0 -20 -40
    -50 -40 -30 -30 -30 -30 -40 -50))

(defparameter +pst-bishop+
  #(-20 -10 -10 -10 -10 -10 -10 -20
    -10 0 0 0 0 0 0 -10
    -10 0 5 10 10 5 0 -10
    -10 5 5 10 10 5 5 -10
    -10 0 10 10 10 10 0 -10
    -10 10 10 10 10 10 10 -10
    -10 5 0 0 0 0 5 -10
    -20 -10 -10 -10 -10 -10 -10 -20))

(defparameter +pst-rook+
  #(0 0 0 0 0 0 0 0
    5 10 10 10 10 10 10 5
    -5 0 0 0 0 0 0 -5
    -5 0 0 0 0 0 0 -5
    -5 0 0 0 0 0 0 -5
    -5 0 0 0 0 0 0 -5
    -5 0 0 0 0 0 0 -5
    0 0 0 5 5 0 0 0))

(defparameter +pst-queen+
  #(-20 -10 -10 -5 -5 -10 -10 -20
    -10 0 0 0 0 0 0 -10
    -10 0 5 5 5 5 0 -10
    -5 0 5 5 5 5 0 -5
    0 0 5 5 5 5 0 -5
    -10 5 5 5 5 5 0 -10
    -10 0 5 0 0 0 0 -10
    -20 -10 -10 -5 -5 -10 -10 -20))

(defparameter +pst-king+
  #(-30 -40 -40 -50 -50 -40 -40 -30
    -30 -40 -40 -50 -50 -40 -40 -30
    -30 -40 -40 -50 -50 -40 -40 -30
    -30 -40 -40 -50 -50 -40 -40 -30
    -20 -30 -30 -40 -40 -30 -30 -20
    -10 -20 -20 -20 -20 -20 -20 -10
    20 20 0 0 0 0 20 20
    20 30 10 0 0 10 30 20))

(declaim (inline %piece-score))

(defun %piece-score (piece row col)
  "Signed material+PST value of PIECE on (ROW COL), from White's perspective."
  (declare (type fixnum piece row col)
           (optimize (speed 3) (safety 1)))
  (let ((white-p (plusp piece)))
    (multiple-value-bind (base table)
        (case (abs piece)
          (1 (values 100 +pst-pawn+))
          (2 (values 320 +pst-knight+))
          (3 (values 330 +pst-bishop+))
          (4 (values 500 +pst-rook+))
          (5 (values 900 +pst-queen+))
          (6 (values 20000 +pst-king+))
          (otherwise (values 0 nil)))
      (declare (type fixnum base))
      (if (null table)
          0
          (let* ((idx (if white-p
                          (+ (* row 8) col)
                          (+ (* (- 7 row) 8) col)))
                 (pst (aref table idx)))
            (declare (type fixnum idx pst))
            (if white-p (+ base pst) (- (+ base pst))))))))

(defun evaluate (game)
  "Static score in centipawns from GAME's side-to-move perspective."
  (declare (type chess-game game) (optimize (speed 3) (safety 1)))
  (let ((board (chess-game-board game))
        (score 0))
    (declare (type fixnum score))
    (loop for r from 0 below 8 do
      (loop for c from 0 below 8 do
        (let ((p (aref board r c)))
          (unless (zerop p)
            (incf score (%piece-score p r c))))))
    (if (eq (chess-game-side-to-move game) :white)
        score
        (- score))))

(defun insufficient-material-p (game)
  "True for KvK, K+minor vs K (no pawns/rooks/queens, at most one minor)."
  (declare (type chess-game game))
  (let ((board (chess-game-board game))
        (minors 0))
    (loop for i from 0 below 64 do
      (let ((p (row-major-aref board i)))
        (case (abs p)
          ((1 4 5) (return-from insufficient-material-p nil))
          ((2 3) (incf minors)
                 (when (> minors 1)
                   (return-from insufficient-material-p nil))))))
    t))
