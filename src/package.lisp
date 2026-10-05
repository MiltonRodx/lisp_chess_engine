(defpackage #:chess
  (:use #:cl)
  (:export
   ;; piece constants
   #:+empty+
   #:+w-pawn+ #:+b-pawn+
   #:+w-knight+ #:+b-knight+
   #:+w-bishop+ #:+b-bishop+
   #:+w-rook+ #:+b-rook+
   #:+w-queen+ #:+b-queen+
   #:+w-king+ #:+b-king+
   ;; castling bits
   #:+castle-wk+ #:+castle-wq+ #:+castle-bk+ #:+castle-bq+
   #:+castle-all+ #:+castle-none+
   ;; mate score bounds
   #:+mate-score+ #:+infinite-score+
   ;; game struct + accessors
   #:chess-game
   #:make-chess-game
   #:chess-game-board
   #:chess-game-side-to-move
   #:chess-game-castling-rights
   #:chess-game-en-passant
   #:chess-game-halfmove-clock
   #:chess-game-fullmove-number
   #:chess-game-history
   #:*game*
   ;; board API
   #:setup-initial-position
   #:clear-board
   #:print-board
   #:valid-coords-p
   #:opposite-color
   #:piece-color
   #:piece-type
   #:piece-value
   #:white-piece-p #:black-piece-p
   #:algebraic-to-coords
   #:coords-to-algebraic
   #:copy-game
   ;; state API (src/state.lisp)
   #:parse-fen
   #:game-to-fen
   #:make-game-from-fen
   #:square-attacked-p
   #:in-check-p
   #:find-king
   #:castling-rights-has-p
   #:insufficient-material-p
   ;; move API (src/movegen.lisp)
   #:move
   #:make-move
   #:move-from-row #:move-from-col
   #:move-to-row #:move-to-col
   #:move-promotion
   #:move-capture #:move-en-passant-p
   #:move-castle #:move-double-push-p
   #:move-to-string
   #:parse-move-string
   #:generate-pseudo-legal-moves
   #:generate-capture-moves
   ;; legal API (src/legal.lisp)
   #:make-move-on-board
   #:unmake-move-on-board
   #:generate-legal-moves
   #:has-legal-move-p
   #:perft
   #:perft-divide
   #:move-matches-p
   ;; eval API (src/eval.lisp)
   #:evaluate
   ;; search API (src/search.lisp)
   #:search-best-move
   #:search-with-limits
   #:*search-nodes*
   ;; uci API (src/uci.lisp)
   #:run-uci-loop
   ;; main API (src/main.lisp)
   #:play-repl
   #:main))
