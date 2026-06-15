;; ==========================================
;; SO, LET'S MAKE A CHESS ENGINE IN CLISP!
;; ==========================================

;; ***************
;; I need it to return a graphic of a table i guess, to stdout

;; So, rules:
;; Integer Piece             Integer Piece
;; 0       Empty             0       Empty
;; 1       White Pawn       -1       Black Pawn
;; 2       White Knight     -2       Black Knight
;; 3       White Bishop     -3       Black Bishop
;; 4       White Rook       -4       Black Rook
;; 5       White Queen      -5       Black Queen
;; 6       White King       -6       Black King


;; DEFINE a chess-board which will be a 2D array, just integers. (maybe I shouldn't use defconstant)...
(defconstant +chess-board+ (make-array '(8 8) :element-type 'integer :initial-element 0))