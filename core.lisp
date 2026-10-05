;; ==========================================
;; SO, LET'S MAKE A CHESS ENGINE IN COMMON LISP!
;; ==========================================

;; ==========================================
;; 1. PIECE CONSTANTS
;; ==========================================
(defconstant +empty+ 0)
(defconstant +w-pawn+ 1)   (defconstant +b-pawn+ -1)
(defconstant +w-knight+ 2) (defconstant +b-knight+ -2)
(defconstant +w-bishop+ 3) (defconstant +b-bishop+ -3)
(defconstant +w-rook+ 4)   (defconstant +b-rook+ -4)
(defconstant +w-queen+ 5)  (defconstant +b-queen+ -5)
(defconstant +w-king+ 6)   (defconstant +b-king+ -6)

;; ==========================================
;; 2. GAME STATE STRUCTURE
;; ==========================================
(defstruct chess-game
  (board (make-array '(8 8)
                     :element-type '(signed-byte 8)
                     :initial-element +empty+)
   :type (simple-array (signed-byte 8) (8 8)))
  (side-to-move :white)
  ;; castling and en-passant will be added in phase 2
  )

;; ==========================================
;; 3. SETUP FUNCTION
;; ==========================================
(defun setup-initial-position (game)
  "Fills the board with the standard starting chess position."
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
    
    ;; 4. Reset turn to White
    (setf (chess-game-side-to-move game) :white)
    
    game))

;; ==========================================
;; 4. GLOBAL STATE & PRINTER
;; ==========================================
(defparameter *game* (make-chess-game))

(defun print-board (game)
  "Prints the current board state to the REPL in a readable ASCII format."
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
    (format t "Turn: ~a~%~%" (chess-game-side-to-move game))))

;; Initialize the game immediately
(setup-initial-position *game*)