(in-package #:chess)

;; ==========================================
;; Phase 3: make/unmake, legal filtering, perft.
;; History entries are plists:
;;   (:moved M :captured C :cap-row R :cap-col O
;;    :castling N :ep E :half H :full F)
;; where E is a fresh cons or NIL (never aliases game state).
;; ==========================================

(defconstant +mate-score+ 100000)
(defconstant +infinite-score+ 1000000)

(declaim (inline %clear-right-for-square))

(defun %clear-right-for-square (rights row col)
  "Clear the castling right associated with original rook square (ROW COL)."
  (declare (type (integer 0 15) rights) (type fixnum row col))
  (cond ((and (= row 7) (= col 0)) (logand rights (lognot +castle-wq+)))
        ((and (= row 7) (= col 7)) (logand rights (lognot +castle-wk+)))
        ((and (= row 0) (= col 0)) (logand rights (lognot +castle-bq+)))
        ((and (= row 0) (= col 7)) (logand rights (lognot +castle-bk+)))
        (t rights)))

(defun make-move-on-board (game move)
  "Play MOVE on GAME, pushing an undo record. Returns MOVE."
  (declare (type chess-game game) (type move move)
           (optimize (speed 3) (safety 1)))
  (let* ((board (chess-game-board game))
         (fr (move-from-row move)) (fc (move-from-col move))
         (tr (move-to-row move)) (tc (move-to-col move))
         (white-p (eq (chess-game-side-to-move game) :white))
         (moved (aref board fr fc))
         ;; capture square differs for en passant
         (cap-r (if (move-en-passant-p move) fr tr))
         (cap-c (if (move-en-passant-p move) tc tc))
         (captured (aref board cap-r cap-c))
         (old-ep (chess-game-en-passant game)))
    ;; 1. push undo record
    (push (list :moved moved :captured captured
                :cap-row cap-r :cap-col cap-c
                :castling (chess-game-castling-rights game)
                :ep (when old-ep (cons (car old-ep) (cdr old-ep)))
                :half (chess-game-halfmove-clock game)
                :full (chess-game-fullmove-number game))
          (chess-game-history game))
    ;; 2. move the piece (promotion replaces the pawn)
    (setf (aref board fr fc) +empty+)
    (setf (aref board tr tc) (or (move-promotion move) moved))
    ;; 3. en passant: remove the bypassed pawn
    (when (move-en-passant-p move)
      (setf (aref board cap-r cap-c) +empty+))
    ;; 4. castling: hop the rook too
    (case (move-castle move)
      (:kingside
       (let ((home (if white-p 7 0)))
         (setf (aref board home 7) +empty+)
         (setf (aref board home 5) (if white-p +w-rook+ +b-rook+))))
      (:queenside
       (let ((home (if white-p 7 0)))
         (setf (aref board home 0) +empty+)
         (setf (aref board home 3) (if white-p +w-rook+ +b-rook+))))
      ((nil))
      (otherwise (error "make-move-on-board: bad castle flag ~a" (move-castle move))))
    ;; 5. castling rights: king move clears both; rook from/to clears one
    (let ((rights (chess-game-castling-rights game)))
      (when (= (abs moved) 6)
        (setf rights (if white-p
                         (logand rights (lognot (logior +castle-wk+ +castle-wq+)))
                         (logand rights (lognot (logior +castle-bk+ +castle-bq+))))))
      (setf rights (%clear-right-for-square rights fr fc))
      (setf rights (%clear-right-for-square rights tr tc))
      (setf (chess-game-castling-rights game) rights))
    ;; 6. en passant target: only after a double push
    (setf (chess-game-en-passant game)
          (if (move-double-push-p move)
              (cons (if white-p (1+ tr) (1- tr)) tc)
              nil))
    ;; 7. clocks
    (setf (chess-game-halfmove-clock game)
          (if (or (= (abs moved) 1) (not (zerop captured)))
              0
              (1+ (chess-game-halfmove-clock game))))
    (when (not white-p)
      (incf (chess-game-fullmove-number game)))
    ;; 8. flip side
    (setf (chess-game-side-to-move game) (if white-p :black :white))
    move))

(defun unmake-move-on-board (game move)
  "Undo MOVE on GAME using the top history record. Returns MOVE."
  (declare (type chess-game game) (type move move)
           (optimize (speed 3) (safety 1)))
  (let* ((board (chess-game-board game))
         (undo (pop (chess-game-history game)))
         (fr (move-from-row move)) (fc (move-from-col move))
         (tr (move-to-row move)) (tc (move-to-col move)))
    (unless undo
      (error "unmake-move-on-board: empty history"))
    (let ((white-p (eq (chess-game-side-to-move game) :black)))
      ;; 1. flip side back
      (setf (chess-game-side-to-move game) (if white-p :white :black))
      ;; 2. restore clocks / rights / EP
      (setf (chess-game-castling-rights game) (getf undo :castling))
      (setf (chess-game-en-passant game) (getf undo :ep))
      (setf (chess-game-halfmove-clock game) (getf undo :half))
      (setf (chess-game-fullmove-number game) (getf undo :full))
      ;; 3. undo castling rook hop
      (case (move-castle move)
        (:kingside
         (let ((home (if white-p 7 0)))
           (setf (aref board home 5) +empty+)
           (setf (aref board home 7) (if white-p +w-rook+ +b-rook+))))
        (:queenside
         (let ((home (if white-p 7 0)))
           (setf (aref board home 3) +empty+)
           (setf (aref board home 0) (if white-p +w-rook+ +b-rook+)))))
      ;; 4. restore moving piece (promotion -> pawn) and captured piece
      (setf (aref board fr fc) (getf undo :moved))
      (setf (aref board tr tc) +empty+)
      (let ((captured (getf undo :captured)))
        (unless (zerop captured)
          (setf (aref board (getf undo :cap-row) (getf undo :cap-col))
                captured))))
    move))

(defun generate-legal-moves (game)
  "Pseudo-legal moves filtered by king safety (pins, check evasions, castling)."
  (let ((side (chess-game-side-to-move game))
        (legal nil))
    (dolist (m (generate-pseudo-legal-moves game) (nreverse legal))
      (make-move-on-board game m)
      (unless (in-check-p game side)
        (push m legal))
      (unmake-move-on-board game m))))

(defun has-legal-move-p (game)
  "True if GAME's side to move has at least one legal move."
  (not (null (generate-legal-moves game))))

(defun move-matches-p (generated parsed)
  "True if GENERATED move matches PARSED from/to plus promotion type.
   Used to resolve UCI input against the generated move list."
  (and (= (move-from-row generated) (move-from-row parsed))
       (= (move-from-col generated) (move-from-col parsed))
       (= (move-to-row generated) (move-to-row parsed))
       (= (move-to-col generated) (move-to-col parsed))
       (let ((gp (move-promotion generated))
             (pp (move-promotion parsed)))
         (cond ((and (null gp) (null pp)) t)
               ((or (null gp) (null pp)) nil)
               (t (= (abs gp) (abs pp)))))))

(defun perft (game depth)
  "Count leaf nodes from GAME to DEPTH (0 = 1). Uses make/unmake."
  (declare (type chess-game game) (type (integer 0 32) depth))
  (if (zerop depth)
      1
      (let ((nodes 0))
        (dolist (m (generate-legal-moves game) nodes)
          (make-move-on-board game m)
          (incf nodes (perft game (1- depth)))
          (unmake-move-on-board game m)))))

(defun perft-divide (game depth)
  "Alist of (MOVE-STRING . COUNT) for each root move at DEPTH."
  (let ((result nil))
    (dolist (m (generate-legal-moves game) (nreverse result))
      (make-move-on-board game m)
      (let ((n (perft game (1- depth))))
        (unmake-move-on-board game m)
        (push (cons (move-to-string m) n) result)))))
