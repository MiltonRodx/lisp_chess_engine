(in-package #:chess)

;; ==========================================
;; Phase 2: move representation + pseudo-legal move generation.
;; Coordinate convention: row 0 = rank 8, col 0 = file a.
;; White moves toward decreasing row; black toward increasing row.
;; Note: captures of the enemy KING are never generated (such positions
;; are illegal; the legal filter relies on this).
;; ==========================================

(defstruct move
  (from-row 0 :type (integer 0 7))
  (from-col 0 :type (integer 0 7))
  (to-row 0 :type (integer 0 7))
  (to-col 0 :type (integer 0 7))
  ;; promotion: NIL or the SIGNED piece value the pawn promotes to
  (promotion nil :type (or null integer))
  ;; capture: value of the captured piece (0 when quiet; pawn for EP)
  (capture 0 :type integer)
  (en-passant-p nil :type boolean)
  ;; castle: NIL, :kingside or :queenside
  (castle nil :type (or null (member :kingside :queenside)))
  (double-push-p nil :type boolean))

(defun move-to-string (move)
  "Long algebraic notation: e.g. e2e4, e7e8q."
  (concatenate 'string
               (coords-to-algebraic (move-from-row move) (move-from-col move))
               (coords-to-algebraic (move-to-row move) (move-to-col move))
               (let ((p (move-promotion move)))
                 (if p
                     (string (char-downcase
                              (case (abs p)
                                (2 #\N) (3 #\B) (4 #\R) (5 #\Q)
                                (otherwise (error "move-to-string: bad promotion ~a" p)))))
                     ""))))

(defun parse-move-string (string color)
  "Parse a UCI-style move STRING (e2e4, e7e8q) into a MOVE with only
   from/to/promotion filled in. COLOR determines promotion piece sign.
   Use MOVE-MATCHES-P (legal.lisp) to find the full generated move."
  (declare (type string string) (type (member :white :black) color))
  (unless (>= (length string) 4)
    (error "parse-move-string: too short ~s" string))
  (multiple-value-bind (fr fc) (algebraic-to-coords (subseq string 0 2))
    (multiple-value-bind (tr tc) (algebraic-to-coords (subseq string 2 4))
      (unless (and fr tr)
        (error "parse-move-string: bad squares ~s" string))
      (let ((promo nil))
        (when (> (length string) 4)
          (let ((ch (char-downcase (char string 4))))
            (setf promo (case ch
                          (#\q (if (eq color :white) +w-queen+ +b-queen+))
                          (#\r (if (eq color :white) +w-rook+ +b-rook+))
                          (#\b (if (eq color :white) +w-bishop+ +b-bishop+))
                          (#\n (if (eq color :white) +w-knight+ +b-knight+))
                          (otherwise (error "parse-move-string: bad promotion ~s" string))))))
        (make-move :from-row fr :from-col fc :to-row tr :to-col tc :promotion promo)))))

(defun piece-value (piece)
  "Midgame material value in centipawns (king = 20000)."
  (declare (type fixnum piece))
  (case (abs piece)
    (0 0) (1 100) (2 320) (3 330) (4 500) (5 900) (6 20000)
    (otherwise 0)))

;; ---- pawn promotion pieces, signed by color ----
(declaim (inline promotion-pieces))
(defun promotion-pieces (white-p)
  (if white-p
      (list +w-queen+ +w-rook+ +w-bishop+ +w-knight+)
      (list +b-queen+ +b-rook+ +b-bishop+ +b-knight+)))

(defun generate-pseudo-legal-moves (game &optional captures-only)
  "All pseudo-legal moves for GAME's side to move (king captures excluded).
   When CAPTURES-ONLY is true, generate captures, en-passant captures and
   promotions only (for quiescence search). No castling in that mode."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((board (chess-game-board game))
         (side (chess-game-side-to-move game))
         (white-p (eq side :white)))
    (let ((moves nil))
      (loop for fr from 0 below 8 do
        (loop for fc from 0 below 8 do
          (let ((p (aref board fr fc)))
            (when (if white-p (plusp p) (minusp p))
              (setf moves (%generate-piece-moves board moves game fr fc p white-p
                                                 captures-only))))))
      moves)))

(defun %generate-piece-moves (board moves game fr fc piece white-p captures-only)
  (declare (type (simple-array (signed-byte 8) (8 8)) board)
           (type fixnum fr fc piece))
  (case (abs piece)
    (1 (%pawn-moves board moves game fr fc white-p captures-only))
    (2 (%jump-moves board moves fr fc white-p +knight-offsets+ captures-only))
    (3 (%slide-moves board moves fr fc white-p +bishop-dirs+ captures-only))
    (4 (%slide-moves board moves fr fc white-p +rook-dirs+ captures-only))
    (5 (%slide-moves board moves fr fc white-p
                     (append +rook-dirs+ +bishop-dirs+) captures-only))
    (6 (%king-moves board moves game fr fc white-p captures-only))
    (otherwise moves)))

(defun %enemy-present-p (target white-p)
  "True if TARGET is an enemy non-king piece (capturable)."
  (declare (type fixnum target))
  (and (not (zerop target))
       (if white-p (minusp target) (plusp target))
       (/= (abs target) 6)))

(defun %pawn-moves (board moves game fr fc white-p captures-only)
  (let* ((dir (if white-p -1 1))
         (start-row (if white-p 6 1))
         (promo-row (if white-p 0 7))
         (ep (chess-game-en-passant game)))
    ;; pushes (skipped in captures-only, except promotions)
    (unless captures-only
      (let ((one-r (+ fr dir)))
        (when (and (<= 0 one-r 7) (zerop (aref board one-r fc)))
          (if (= one-r promo-row)
              (dolist (p (promotion-pieces white-p))
                (push (make-move :from-row fr :from-col fc
                                 :to-row one-r :to-col fc :promotion p)
                      moves))
              (progn
                (push (make-move :from-row fr :from-col fc
                                 :to-row one-r :to-col fc)
                      moves)
                (let ((two-r (+ fr (* 2 dir))))
                  (when (and (= fr start-row) (zerop (aref board two-r fc)))
                    (push (make-move :from-row fr :from-col fc
                                     :to-row two-r :to-col fc
                                     :double-push-p t)
                          moves))))))))
    ;; captures (always generated, even in captures-only)
    ;; (rewritten below for clarity)
    (dolist (dc '(-1 1) moves)
      (let ((tr (+ fr dir))
            (tc (+ fc dc)))
        (when (<= 0 tr 7)
          (when (<= 0 tc 7)
            (let ((target (aref board tr tc)))
              (cond
                ;; normal capture (never the king)
                ((%enemy-present-p target white-p)
                 (if (= tr promo-row)
                     (dolist (p (promotion-pieces white-p))
                       (push (make-move :from-row fr :from-col fc
                                        :to-row tr :to-col tc
                                        :promotion p :capture target)
                             moves))
                     (push (make-move :from-row fr :from-col fc
                                      :to-row tr :to-col tc :capture target)
                           moves)))
                ;; en passant: target square empty and equals EP square
                ((and (zerop target) ep (= tr (car ep)) (= tc (cdr ep)))
                 (push (make-move :from-row fr :from-col fc
                                  :to-row tr :to-col tc
                                  :capture (if white-p +b-pawn+ +w-pawn+)
                                  :en-passant-p t)
                       moves))
                ;; quiet promotion push in captures-only mode
                ((and captures-only (zerop target) (= tr promo-row))
                 (dolist (p (promotion-pieces white-p))
                   (push (make-move :from-row fr :from-col fc
                                    :to-row tr :to-col tc :promotion p)
                         moves)))))))))))

(defun %jump-moves (board moves fr fc white-p offsets captures-only)
  (dolist (off offsets moves)
    (let ((tr (+ fr (first off))) (tc (+ fc (second off))))
      (when (and (<= 0 tr 7) (<= 0 tc 7))
        (let ((target (aref board tr tc)))
          (cond ((%enemy-present-p target white-p)
                 (push (make-move :from-row fr :from-col fc :to-row tr :to-col tc
                                              :capture target)
                       moves))
                ((and (zerop target) (not captures-only))
                 (push (make-move :from-row fr :from-col fc :to-row tr :to-col tc)
                       moves))))))))

(defun %slide-moves (board moves fr fc white-p dirs captures-only)
  (dolist (dir dirs moves)
    (let ((dr (first dir)) (dc (second dir)))
      (loop for tr = (+ fr dr) then (+ tr dr)
            for tc = (+ fc dc) then (+ tc dc)
            while (and (<= 0 tr 7) (<= 0 tc 7)) do
              (let ((target (aref board tr tc)))
                (cond ((%enemy-present-p target white-p)
                       (push (make-move :from-row fr :from-col fc
                                        :to-row tr :to-col tc :capture target)
                             moves)
                       (return))
                      ((zerop target)
                       (unless captures-only
                         (push (make-move :from-row fr :from-col fc
                                          :to-row tr :to-col tc)
                               moves)))
                      (t (return)))))))) ; own piece or enemy king: blocked

(defun %king-moves (board moves game fr fc white-p captures-only)
  (setf moves (%jump-moves board moves fr fc white-p +king-offsets+ captures-only))
  ;; castling: never in captures-only mode
  (unless captures-only
    (let ((rights (chess-game-castling-rights game))
          (enemy (if white-p :black :white))
          (home (if white-p 7 0)))
      (when (and (= fr home) (= fc 4)
                 (not (square-attacked-p board home 4 enemy)))
        ;; kingside: squares f,g empty; e,f,g unattacked; rook present
        (when (and (if white-p (castling-rights-has-p rights +castle-wk+)
                         (castling-rights-has-p rights +castle-bk+))
                   (zerop (aref board home 5)) (zerop (aref board home 6))
                   (= (aref board home 7) (if white-p +w-rook+ +b-rook+))
                   (not (square-attacked-p board home 5 enemy))
                   (not (square-attacked-p board home 6 enemy)))
          (push (make-move :from-row fr :from-col fc
                           :to-row home :to-col 6 :castle :kingside)
                moves))
        ;; queenside: squares b,c,d empty; e,d,c unattacked; rook present
        (when (and (if white-p (castling-rights-has-p rights +castle-wq+)
                         (castling-rights-has-p rights +castle-bq+))
                   (zerop (aref board home 1))
                   (zerop (aref board home 2)) (zerop (aref board home 3))
                   (= (aref board home 0) (if white-p +w-rook+ +b-rook+))
                   (not (square-attacked-p board home 3 enemy))
                   (not (square-attacked-p board home 2 enemy)))
          (push (make-move :from-row fr :from-col fc
                           :to-row home :to-col 2 :castle :queenside)
                moves)))))
  moves)

(defun generate-capture-moves (game)
  "Pseudo-legal captures (+ promotions) for quiescence search."
  (generate-pseudo-legal-moves game t))
