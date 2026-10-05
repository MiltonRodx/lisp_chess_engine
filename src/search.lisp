(in-package #:chess)

;; ==========================================
;; Phase 5: negamax + alpha-beta + quiescence + iterative deepening.
;; Scores are from the side-to-move's perspective.
;; Mate scores: being mated = (- +mate-score+ ply); mating via negation.
;; ==========================================

(defvar *search-nodes* 0
  "Nodes visited by the current/last search.")

(defvar *search-stop-time* nil
  "Quietus time in internal-time units, or NIL for no limit.")

(defvar *search-max-ply* 64
  "Absolute ply ceiling (mate scores stay well below +MATE-SCORE+).")

(defvar *search-max-qply* 12
  "Quiescence depth cap (plies from quiescence entry). Check evasions and
   capture chains beyond this return the static score. Keeps the q-tree
   (especially check-evasion branches) bounded.")

(defvar *search-killers* (make-array 128 :initial-element nil)
  "Two killer (quiet) moves per ply: index 2*ply, 2*ply+1.")

(defvar *search-history* (make-array '(64 64) :element-type 'fixnum :initial-element 0)
  "History heuristic for quiet moves, indexed by (from-sq to-sq).")

(declaim (inline %sq64 %time-up-p %check-time))

(defun %sq64 (row col)
  (declare (type fixnum row col))
  (+ (* row 8) col))

(defun %time-up-p ()
  (and *search-stop-time*
       (>= (get-internal-real-time) *search-stop-time*)))

(defun %check-time ()
  (when (%time-up-p)
    (throw 'search-abort nil)))

;; ---- move ordering ----

(defun %order-score (game move ply)
  "Heuristic score for ordering (higher = search first)."
  (declare (ignore game))
  (let ((score 0))
    (declare (type fixnum score))
    ;; captures: MVV-LVA
    (let ((cap (move-capture move)))
      (unless (zerop cap)
        (let* ((board (chess-game-board game))
               (attacker (abs (aref board (move-from-row move) (move-from-col move)))))
          (incf score (+ 100000 (- (* 10 (abs cap)) attacker))))))
    ;; promotions
    (when (move-promotion move)
      (incf score (+ 90000 (abs (move-promotion move)))))
    ;; killers (quiet moves that caused cutoffs before)
    (let ((k1 (aref *search-killers* (min (* 2 ply) 126)))
          (k2 (aref *search-killers* (min (1+ (* 2 ply)) 127))))
      (when (or (eq move k1) (eq move k2))
        (incf score 50000)))
    ;; history heuristic for quiets
    (unless (or (not (zerop (move-capture move))) (move-promotion move))
      (incf score (aref *search-history*
                       (%sq64 (move-from-row move) (move-from-col move))
                       (%sq64 (move-to-row move) (move-to-col move)))))
    score))

(defun %ordered-moves (game moves ply)
  (sort (copy-list moves) #'> :key (lambda (m) (%order-score game m ply))))

(defun %record-cutoff (move ply depth)
  "Remember a quiet move that caused a beta cutoff."
  (unless (or (not (zerop (move-capture move))) (move-promotion move))
    ;; shift killers, store history
    (let ((i (min (* 2 ply) 126)))
      (unless (eq move (aref *search-killers* i))
        (setf (aref *search-killers* (1+ i)) (aref *search-killers* i))
        (setf (aref *search-killers* i) move)))
    (incf (aref *search-history*
                (%sq64 (move-from-row move) (move-from-col move))
                (%sq64 (move-to-row move) (move-to-col move)))
          (* depth depth))))

;; ---- quiescence ----

(defun %quiescence (game alpha beta ply qply)
  (declare (type chess-game game) (type fixnum alpha beta ply qply)
           (optimize (speed 3) (safety 1)))
  (incf *search-nodes*)
  (when (zerop (logand *search-nodes* 2047))
    (%check-time))
  ;; hard horizon: capture/check trees are capped from quiescence entry.
  (when (>= qply *search-max-qply*)
    (return-from %quiescence (evaluate game)))
  ;; draw by fifty moves / dead material
  (when (or (>= (chess-game-halfmove-clock game) 100)
            (insufficient-material-p game))
    (return-from %quiescence 0))
  (let ((side (chess-game-side-to-move game)))
    ;; if in check, search all evasions (can't stand pat)
    (when (in-check-p game side)
      (return-from %quiescence
        (%full-no-stand-pat game alpha beta (1+ ply) (1+ qply))))
    (let ((stand-pat (evaluate game)))
      (when (>= stand-pat beta)
        (return-from %quiescence beta))
      (when (> stand-pat alpha)
        (setf alpha stand-pat))
      (%quiescence-captures game alpha beta ply side qply))))

(defun %quiescence-captures (game alpha beta ply side qply)
  (dolist (m (%ordered-moves game (generate-capture-moves game) ply) alpha)
    (make-move-on-board game m)
    (if (in-check-p game side) ; own king left in check: illegal, skip
        (unmake-move-on-board game m)
        (let ((score (- (%quiescence game (- beta) (- alpha)
                                     (1+ ply) (1+ qply)))))
          (unmake-move-on-board game m)
          (when (>= score beta)
            (return-from %quiescence-captures beta))
          (when (> score alpha)
            (setf alpha score)))))
  alpha)

(defun %full-no-stand-pat (game alpha beta ply qply)
  "Full-width search used from quiescence when in check."
  (let ((side (chess-game-side-to-move game))
        (moves (generate-legal-moves game)))
    (cond ((null moves)
           (if (in-check-p game side)
               (- ply +mate-score+) ; mated (unreachable here, but safe)
               0))
           (t
           (dolist (m (%ordered-moves game moves ply) alpha)
             (make-move-on-board game m)
             (let ((score (- (%quiescence game (- beta) (- alpha)
                                          (1+ ply) (1+ qply)))))
               (unmake-move-on-board game m)
               (when (>= score beta)
                 (return-from %full-no-stand-pat beta))
               (when (> score alpha)
                 (setf alpha score))))
           alpha))))

;; ---- main negamax ----

(defun %negamax (game depth alpha beta ply)
  (declare (type chess-game game) (type fixnum depth alpha beta ply)
           (optimize (speed 3) (safety 1)))
  (incf *search-nodes*)
  (when (zerop (logand *search-nodes* 2047))
    (%check-time))
  ;; draws
  (when (or (>= (chess-game-halfmove-clock game) 100)
            (insufficient-material-p game))
    (return-from %negamax 0))
  (when (<= depth 0)
    (return-from %negamax (%quiescence game alpha beta ply 0)))
  (let* ((side (chess-game-side-to-move game))
         (moves (%ordered-moves game (generate-legal-moves game)
                                (min ply *search-max-ply*)))
         (best (- +infinite-score+)))
    (declare (type fixnum best))
    (cond ((null moves)
           (if (in-check-p game side)
               (- ply +mate-score+) ; checkmated
               0)) ; stalemate
          (t
           (dolist (m moves best)
             (make-move-on-board game m)
             (let ((score (- (%negamax game (1- depth) (- beta) (- alpha)
                                       (1+ ply)))))
               (unmake-move-on-board game m)
               (when (> score best)
                 (setf best score))
               (when (> best alpha)
                 (setf alpha best))
               (when (>= alpha beta)
                 (%record-cutoff m (min ply *search-max-ply*) depth)
                 (return beta))))
           best))))

(defun %search-root (game depth alpha beta)
  "One full-width iteration at DEPTH. Returns (values best-move best-score)."
  (let ((best-move nil)
        (best (- +infinite-score+))
        (a alpha))
    (dolist (m (%ordered-moves game (generate-legal-moves game) 0) (values best-move best))
      (%check-time)
      (make-move-on-board game m)
      (let ((score (- (%negamax game (1- depth) (- beta) (- a) 1))))
        (unmake-move-on-board game m)
        (when (or (null best-move) (> score best))
          (setf best score best-move m))
        (when (> best a)
          (setf a best))
        (when (>= a beta)
          (%record-cutoff m 0 depth)
          (return (values best-move best)))))))

(defun search-with-limits (game &key (depth 4) (movetime-ms nil) (info-callback nil))
  "Iterative-deepening search. Returns (values best-move score depth nodes).
   INFO-CALLBACK, if given, is called after each completed iteration as
   (funcall cb iteration-depth score nodes elapsed-ms)."
  (declare (type chess-game game))
  (setf *search-nodes* 0)
  (fill *search-killers* nil)
  ;; NOTE: FILL only works on sequences (vectors), not rank-2 arrays.
  (loop for i from 0 below 64 do
    (loop for j from 0 below 64 do
      (setf (aref *search-history* i j) 0)))
  (let* ((start (get-internal-real-time))
         (units internal-time-units-per-second)
         (root-moves (generate-legal-moves game)))
    (setf *search-stop-time*
          (when movetime-ms
            (+ start (max 1 (round (* movetime-ms (/ units 1000)))))))
    (cond ((null root-moves)
           (values nil
                   (if (in-check-p game (chess-game-side-to-move game))
                       (- +mate-score+) ; already mated
                       0)
                   0 0))
          (t
           (let ((best (first root-moves)) (score 0) (done 0))
             (catch 'search-abort
               (loop for d from 1 to (max 1 depth) do
                 (multiple-value-bind (m s) (%search-root game d
                                                          (- +infinite-score+)
                                                          +infinite-score+)
                   (when m (setf best m score s))
                   (setf done d)
                   (when info-callback
                     (let ((elapsed (round (* (- (get-internal-real-time) start)
                                             (/ 1000 units)))))
                       (funcall info-callback d s *search-nodes* elapsed)))
                   ;; stop early on forced mate found at this depth
                   (when (>= (abs score) (- +mate-score+ 1000))
                     (return)))))
             (setf *search-stop-time* nil)
             (values best score done *search-nodes*))))))

(defun search-best-move (game &key (depth 4) (movetime-ms nil))
  "Convenience wrapper returning (values best-move score)."
  (multiple-value-bind (m s d n) (search-with-limits game :depth depth
                                                     :movetime-ms movetime-ms)
    (declare (ignore d n))
    (values m s)))
