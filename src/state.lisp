(in-package #:chess)

;; ==========================================
;; Phase 1: FEN + attack detection + check
;; ==========================================

(defun %piece-char-to-value (ch)
  (case ch
    (#\P +w-pawn+) (#\N +w-knight+) (#\B +w-bishop+)
    (#\R +w-rook+) (#\Q +w-queen+)  (#\K +w-king+)
    (#\p +b-pawn+) (#\n +b-knight+) (#\b +b-bishop+)
    (#\r +b-rook+) (#\q +b-queen+)  (#\k +b-king+)
    (otherwise (error "parse-fen: invalid piece char ~s" ch))))

(defun %piece-value-to-char (v)
  (case v
    (1 #\P) (2 #\N) (3 #\B) (4 #\R) (5 #\Q) (6 #\K)
    (-1 #\p) (-2 #\n) (-3 #\b) (-4 #\r) (-5 #\q) (-6 #\k)
    (otherwise (error "game-to-fen: invalid piece value ~a" v))))

(defun parse-fen (fen-string &optional (game (make-chess-game)))
  "Parse FEN-STRING into GAME (or a fresh game). Signals an error on malformed FEN.
   Supports the 6 standard fields: placement, side, castling, EP, halfmove, fullmove."
  (declare (type string fen-string))
  (let* ((parts (split-fen-fields fen-string)))
    (unless (= (length parts) 6)
      (error "parse-fen: expected 6 FEN fields, got ~a in ~s" (length parts) fen-string))
    (destructuring-bind (placement side castling ep half full) parts
      (let ((board (chess-game-board game)))
        ;; 1. piece placement
        (loop for i from 0 below 64 do (setf (row-major-aref board i) +empty+))
        (let ((row 0) (col 0))
          (loop for ch across placement do
            (cond
              ((char= ch #\/)
               (unless (= col 8) (error "parse-fen: short rank in ~s" placement))
               (incf row) (setf col 0)
               (when (> row 7) (error "parse-fen: too many ranks in ~s" placement)))
              ((char<= #\1 ch #\8)
               (let ((n (- (char-code ch) (char-code #\0))))
                 (loop repeat n do
                   (when (>= col 8) (error "parse-fen: rank overflow in ~s" placement))
                   (setf (aref board row col) +empty+)
                   (incf col))))
              (t
               (when (>= col 8) (error "parse-fen: rank overflow in ~s" placement))
               (setf (aref board row col) (%piece-char-to-value ch))
               (incf col))))
          (unless (and (= row 7) (= col 8))
            (error "parse-fen: incomplete placement ~s" placement)))
        ;; 2. side to move
        (setf (chess-game-side-to-move game)
              (cond ((string= side "w") :white)
                    ((string= side "b") :black)
                    (t (error "parse-fen: bad side field ~s" side))))
        ;; 3. castling
        (setf (chess-game-castling-rights game)
              (cond ((string= castling "-") +castle-none+)
                    (t
                     (let ((r +castle-none+))
                       (loop for ch across castling do
                         (case ch
                           (#\K (setf r (logior r +castle-wk+)))
                           (#\Q (setf r (logior r +castle-wq+)))
                           (#\k (setf r (logior r +castle-bk+)))
                           (#\q (setf r (logior r +castle-bq+)))
                           (otherwise (error "parse-fen: bad castling field ~s" castling))))
                       r))))
        ;; 4. en passant
        (setf (chess-game-en-passant game)
              (cond ((string= ep "-") nil)
                    ((= (length ep) 2)
                     (multiple-value-bind (r c) (algebraic-to-coords ep)
                       (unless r (error "parse-fen: bad EP square ~s" ep))
                       (cons r c)))
                    (t (error "parse-fen: bad EP field ~s" ep))))
        ;; 5/6. clocks
        (setf (chess-game-halfmove-clock game) (parse-integer half))
        (setf (chess-game-fullmove-number game) (parse-integer full))
        (when (minusp (chess-game-halfmove-clock game))
          (error "parse-fen: negative halfmove ~s" half))
        (when (< (chess-game-fullmove-number game) 1)
          (error "parse-fen: fullmove must be >= 1, got ~s" full))
        (setf (chess-game-history game) nil)
        game))))

(defun split-fen-fields (s)
  "Split string S on spaces, dropping empties."
  (let ((fields nil) (start 0) (n (length s)))
    (loop for i from 0 to n do
      (when (or (= i n) (char= (if (< i n) (char s i) #\Space) #\Space))
        (when (> i start)
          (push (subseq s start i) fields))
        (setf start (1+ i))))
    (nreverse fields)))

(defun make-game-from-fen (fen-string)
  "Convenience: parse FEN into a fresh game."
  (parse-fen fen-string (make-chess-game)))

(defun game-to-fen (game)
  "Serialize GAME to a FEN string."
  (let ((board (chess-game-board game)))
    (with-output-to-string (out)
      ;; 1. placement
      (loop for row from 0 below 8 do
        (let ((empty-run 0))
          (labels ((flush ()
                     (when (> empty-run 0)
                       (write-char (code-char (+ (char-code #\0) empty-run)) out)
                       (setf empty-run 0))))
            (loop for col from 0 below 8 do
              (let ((v (aref board row col)))
                (if (zerop v)
                    (incf empty-run)
                    (progn (flush) (write-char (%piece-value-to-char v) out)))))
            (flush)))
        (when (< row 7) (write-char #\/ out)))
      ;; 2. side
      (format out " ~a" (if (eq (chess-game-side-to-move game) :white) "w" "b"))
      ;; 3. castling
      (let ((r (chess-game-castling-rights game)))
        (format out " ~a"
                (if (zerop r) "-"
                    (concatenate 'string
                                 (if (castling-rights-has-p r +castle-wk+) "K" "")
                                 (if (castling-rights-has-p r +castle-wq+) "Q" "")
                                 (if (castling-rights-has-p r +castle-bk+) "k" "")
                                 (if (castling-rights-has-p r +castle-bq+) "q" "")))))
      ;; 4. EP
      (let ((ep (chess-game-en-passant game)))
        (format out " ~a" (if ep (coords-to-algebraic (car ep) (cdr ep)) "-")))
      ;; 5/6. clocks
      (format out " ~a ~a" (chess-game-halfmove-clock game) (chess-game-fullmove-number game)))))

;; ------------------------------------------
;; Attack detection
;; ------------------------------------------

(defparameter +knight-offsets+ '((-2 -1) (-2 1) (-1 -2) (-1 2) (1 -2) (1 2) (2 -1) (2 1)))
(defparameter +king-offsets+ '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))
(defparameter +rook-dirs+ '((-1 0) (1 0) (0 -1) (0 1)))
(defparameter +bishop-dirs+ '((-1 -1) (-1 1) (1 -1) (1 1)))

(defun square-attacked-p (board target-row target-col attacker)
  "True if square (TARGET-ROW TARGET-COL) is attacked by color ATTACKER (:white/:black).
   BOARD is the raw 8x8 array. Handles pawns, knights, kings, and sliding pieces."
  (declare (type (simple-array (signed-byte 8) (8 8)) board)
           (type fixnum target-row target-col)
           (type (member :white :black) attacker)
           (optimize (speed 3) (safety 1)))
  (unless (valid-coords-p target-row target-col)
    (error "square-attacked-p: target off board ~a ~a" target-row target-col))
  (let ((by-white (eq attacker :white)))
    ;; 1. Pawn attacks. A white pawn on (r,c) attacks (r-1,c±1), so a square
    ;;    (tr,tc) is attacked by white iff a white pawn sits at (tr+1,tc±1).
    (let ((pr (if by-white (1+ target-row) (1- target-row)))
          (wanted (if by-white +w-pawn+ +b-pawn+)))
      (when (<= 0 pr 7)
        (when (and (>= (1- target-col) 0)
                   (= (aref board pr (1- target-col)) wanted))
          (return-from square-attacked-p t))
        (when (and (<= (1+ target-col) 7)
                   (= (aref board pr (1+ target-col)) wanted))
          (return-from square-attacked-p t))))
    ;; 2. Knight attacks
    (let ((wanted (if by-white +w-knight+ +b-knight+)))
      (dolist (off +knight-offsets+)
        (let ((r (+ target-row (first off))) (c (+ target-col (second off))))
          (when (and (<= 0 r 7) (<= 0 c 7) (= (aref board r c) wanted))
            (return-from square-attacked-p t)))))
    ;; 3. King attacks
    (let ((wanted (if by-white +w-king+ +b-king+)))
      (dolist (off +king-offsets+)
        (let ((r (+ target-row (first off))) (c (+ target-col (second off))))
          (when (and (<= 0 r 7) (<= 0 c 7) (= (aref board r c) wanted))
            (return-from square-attacked-p t)))))
    ;; 4. Sliding attacks: orthogonal (rook/queen)
    (let ((rook (if by-white +w-rook+ +b-rook+))
          (queen (if by-white +w-queen+ +b-queen+)))
      (dolist (dir +rook-dirs+)
        (let ((r (+ target-row (first dir))) (c (+ target-col (second dir))))
          (loop while (and (<= 0 r 7) (<= 0 c 7)) do
            (let ((p (aref board r c)))
              (cond ((zerop p)) ; empty: keep sliding
                    ((or (= p rook) (= p queen))
                     (return-from square-attacked-p t))
                    (t (return)))) ; blocked by other piece
            (incf r (first dir)) (incf c (second dir))))))
    ;; 5. Sliding attacks: diagonal (bishop/queen)
    (let ((bishop (if by-white +w-bishop+ +b-bishop+))
          (queen (if by-white +w-queen+ +b-queen+)))
      (dolist (dir +bishop-dirs+)
        (let ((r (+ target-row (first dir))) (c (+ target-col (second dir))))
          (loop while (and (<= 0 r 7) (<= 0 c 7)) do
            (let ((p (aref board r c)))
              (cond ((zerop p))
                    ((or (= p bishop) (= p queen))
                     (return-from square-attacked-p t))
                    (t (return))))
            (incf r (first dir)) (incf c (second dir))))))
    nil))

(defun find-king (board color)
  "Return (values row col) of COLOR's king on BOARD, or NIL NIL if absent."
  (declare (type (simple-array (signed-byte 8) (8 8)) board))
  (let ((wanted (if (eq color :white) +w-king+ +b-king+)))
    (loop for r from 0 below 8 do
      (loop for c from 0 below 8 do
        (when (= (aref board r c) wanted)
          (return-from find-king (values r c)))))
    (values nil nil)))

(defun in-check-p (game color)
  "True if COLOR's king is under attack by the opponent. NIL if king missing."
  (declare (type (member :white :black) color))
  (multiple-value-bind (kr kc) (find-king (chess-game-board game) color)
    (when (and kr kc)
      (square-attacked-p (chess-game-board game) kr kc (opposite-color color)))))
