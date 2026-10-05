(in-package #:chess)

;; ==========================================
;; 1. PIECE CONSTANTS (kept compatible with core.lisp)
;; ==========================================
(defconstant +empty+ 0)
(defconstant +w-pawn+ 1)   (defconstant +b-pawn+ -1)
(defconstant +w-knight+ 2)  (defconstant +b-knight+ -2)
(defconstant +w-bishop+ 3)  (defconstant +b-bishop+ -3)
(defconstant +w-rook+ 4)    (defconstant +b-rook+ -4)
(defconstant +w-queen+ 5)   (defconstant +b-queen+ -5)
(defconstant +w-king+ 6)    (defconstant +b-king+ -6)

;; Castling rights bitmask: WK=1 WQ=2 BK=4 BQ=8
(defconstant +castle-none+ 0)
(defconstant +castle-wk+ 1)
(defconstant +castle-wq+ 2)
(defconstant +castle-bk+ 4)
(defconstant +castle-bq+ 8)
(defconstant +castle-all+ 15)

;; ==========================================
;; 2. GAME STATE STRUCTURE (Phase 1: full position state)
;; Coordinate convention (same as core.lisp):
;;   row 0 = rank 8 (black back rank), row 7 = rank 1 (white back rank)
;;   col 0 = file a, col 7 = file h
;;   White pawns move toward DECREASING row. Black toward INCREASING row.
;; ==========================================
(defstruct chess-game
  (board (make-array '(8 8)
                     :element-type '(signed-byte 8)
                     :initial-element +empty+)
   :type (simple-array (signed-byte 8) (8 8)))
  (side-to-move :white :type (member :white :black))
  (castling-rights +castle-all+ :type (integer 0 15))
  ;; en-passant: NIL or a cons (ROW . COL) naming the capturable target square
  ;; (the empty square behind a pawn that just double-pushed).
  (en-passant nil :type (or null cons))
  (halfmove-clock 0 :type (unsigned-byte 32))
  (fullmove-number 1 :type (unsigned-byte 32))
  ;; history: stack of undo records for make/unmake (Phase 3). Each entry
  ;; is a plist/opaque record pushed by make-move, popped by unmake-move.
  (history nil :type list))

;; ==========================================
;; 3. SMALL HELPERS
;; ==========================================
(declaim (inline valid-coords-p opposite-color piece-color white-piece-p black-piece-p))

(defun valid-coords-p (row col)
  "True if ROW/COL are on the board."
  (declare (type fixnum row col)
           (optimize (speed 3) (safety 1)))
  (and (<= 0 row 7) (<= 0 col 7)))

(defun opposite-color (color)
  (ecase color
    (:white :black)
    (:black :white)))

(defun piece-color (piece)
  "Return :white, :black, or NIL for empty."
  (declare (type fixnum piece))
  (cond ((plusp piece) :white)
        ((minusp piece) :black)
        (t nil)))

(defun white-piece-p (piece)
  (plusp (the fixnum piece)))

(defun black-piece-p (piece)
  (minusp (the fixnum piece)))

(defun piece-type (piece)
  "Absolute piece kind: 0 empty, 1 pawn, 2 knight, 3 bishop, 4 rook, 5 queen, 6 king."
  (declare (type fixnum piece))
  (abs piece))

(defun castling-rights-has-p (rights flag)
  "True if RIGHTS bitmask includes FLAG."
  (declare (type (integer 0 15) rights flag))
  (not (zerop (logand rights flag))))

(defun algebraic-to-coords (square)
  "Convert algebraic string like \"e4\" to (values row col). Returns NIL NIL if invalid."
  (declare (type string square))
  (when (= (length square) 2)
    (let ((f (char-downcase (char square 0)))
          (r (char square 1)))
      (when (and (char<= #\a f #\h) (char<= #\1 r #\8))
        (let ((col (- (char-code f) (char-code #\a)))
              (rank (- (char-code r) (char-code #\1)))) ; 0-based rank, 0 = rank 1
          (values (- 7 rank) col))))))

(defun coords-to-algebraic (row col)
  "Convert ROW/COL to algebraic string like \"e4\"."
  (declare (type fixnum row col))
  (unless (valid-coords-p row col)
    (error "coords-to-algebraic: off-board coords ~a ~a" row col))
  (format nil "~c~c"
          (code-char (+ (char-code #\a) col))
          (code-char (+ (char-code #\1) (- 7 row)))))

(defun clear-board (game)
  "Empty the board and reset all state to a blank position (White to move, no rights)."
  (let ((board (chess-game-board game)))
    (loop for i from 0 below 64
          do (setf (row-major-aref board i) +empty+))
    (setf (chess-game-side-to-move game) :white
          (chess-game-castling-rights game) +castle-none+
          (chess-game-en-passant game) nil
          (chess-game-halfmove-clock game) 0
          (chess-game-fullmove-number game) 1
          (chess-game-history game) nil))
  game)

(defun copy-game (game)
  "Deep copy GAME (board is copied, history list is copied shallowly)."
  (let ((new (make-chess-game)))
    (replace (make-array 64 :element-type '(signed-byte 8)
                            :displaced-to (chess-game-board new))
             (make-array 64 :element-type '(signed-byte 8)
                            :displaced-to (chess-game-board game)))
    (setf (chess-game-side-to-move new) (chess-game-side-to-move game)
          (chess-game-castling-rights new) (chess-game-castling-rights game)
          (chess-game-en-passant new) (let ((ep (chess-game-en-passant game)))
                                        (when ep (cons (car ep) (cdr ep))))
          (chess-game-halfmove-clock new) (chess-game-halfmove-clock game)
          (chess-game-fullmove-number new) (chess-game-fullmove-number game)
          (chess-game-history new) (copy-list (chess-game-history game)))
    new))

;; ==========================================
;; 4. SETUP FUNCTION (compatible with core.lisp)
;; ==========================================
(defun setup-initial-position (game)
  "Fill the board with the standard starting chess position and reset all state."
  (let ((board (chess-game-board game)))
    ;; 1. Clear the board first
    (loop for i from 0 below 64
          do (setf (row-major-aref board i) +empty+))

    ;; 2. Black pieces (Row 0 and Row 1)
    (setf (aref board 0 0) +b-rook+)
    (setf (aref board 0 1) +b-knight+)
    (setf (aref board 0 2) +b-bishop+)
    (setf (aref board 0 3) +b-queen+)
    (setf (aref board 0 4) +b-king+)
    (setf (aref board 0 5) +b-bishop+)
    (setf (aref board 0 6) +b-knight+)
    (setf (aref board 0 7) +b-rook+)
    (loop for col from 0 below 8
          do (setf (aref board 1 col) +b-pawn+))

    ;; 3. White pieces (Row 6 and Row 7)
    (setf (aref board 7 0) +w-rook+)
    (setf (aref board 7 1) +w-knight+)
    (setf (aref board 7 2) +w-bishop+)
    (setf (aref board 7 3) +w-queen+)
    (setf (aref board 7 4) +w-king+)
    (setf (aref board 7 5) +w-bishop+)
    (setf (aref board 7 6) +w-knight+)
    (setf (aref board 7 7) +w-rook+)
    (loop for col from 0 below 8
          do (setf (aref board 6 col) +w-pawn+))

    ;; 4. Reset full state
    (setf (chess-game-side-to-move game) :white
          (chess-game-castling-rights game) +castle-all+
          (chess-game-en-passant game) nil
          (chess-game-halfmove-clock game) 0
          (chess-game-fullmove-number game) 1
          (chess-game-history game) nil)
    game))

;; ==========================================
;; 5. GLOBAL STATE & PRINTER
;; ==========================================
(defparameter *game* (make-chess-game))

(defun print-board (game)
  "Print the current board state to the REPL in a readable ASCII format."
  (let* ((board (chess-game-board game))
         (pieces '#(#\. #\P #\N #\B #\R #\Q #\K)))
    (format t "~&  a b c d e f g h~%")
    (loop for row from 0 below 8 do
      (format t "~a " (- 8 row))
      (loop for col from 0 below 8 do
        (let ((val (aref board row col)))
          (if (zerop val)
              (format t ". ")
              (let ((char (aref pieces (abs val))))
                (if (> val 0)
                    (format t "~c " (char-upcase char))
                    (format t "~c " (char-downcase char)))))))
      (format t "~%"))
    (format t "Turn: ~a | Castling: ~a | EP: ~a | Half: ~a Full: ~a~%~%"
            (chess-game-side-to-move game)
            (let ((r (chess-game-castling-rights game)))
              (if (zerop r)
                  "-"
                  (concatenate 'string
                               (if (castling-rights-has-p r +castle-wk+) "K" "")
                               (if (castling-rights-has-p r +castle-wq+) "Q" "")
                               (if (castling-rights-has-p r +castle-bk+) "k" "")
                               (if (castling-rights-has-p r +castle-bq+) "q" ""))))
            (let ((ep (chess-game-en-passant game)))
              (if ep (coords-to-algebraic (car ep) (cdr ep)) "-"))
            (chess-game-halfmove-clock game)
            (chess-game-fullmove-number game))))

;; Initialize the game immediately (as core.lisp did)
(setup-initial-position *game*)
