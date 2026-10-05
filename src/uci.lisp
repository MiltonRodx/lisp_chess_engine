(in-package #:chess)

;; ==========================================
;; Phase 6: UCI protocol loop.
;; Supported: uci, isready, ucinewgame, position, go, stop, quit.
;; go supports: depth N, movetime MS, infinite, wtime/btime/winc/binc[/movestogo].
;; ==========================================

(defparameter *uci-game* nil
  "Position held by the UCI loop.")

(defparameter *engine-name* "lisp-chess-engine")
(defparameter *engine-version* "0.1.0")
(defparameter *engine-author* "milton")

(defun %uci-tokens (line)
  "Split LINE on whitespace."
  (let ((toks nil) (start nil))
    (loop for i from 0 to (length line) do
      (let ((ch (when (< i (length line)) (char line i))))
        (cond ((or (null ch) (char= ch #\Space) (char= ch #\Tab) (char= ch #\Return))
               (when start (push (subseq line start i) toks) (setf start nil)))
              (t (unless start (setf start i))))))
    (nreverse toks)))

(defun %uci-apply-position-moves (game moves)
  "Play UCI long-algebraic MOVES on GAME. Signals an error on illegal input."
  (dolist (ms moves game)
    (let* ((parsed (parse-move-string ms (chess-game-side-to-move game)))
           (found (find-if (lambda (m) (move-matches-p m parsed))
                           (generate-legal-moves game))))
      (unless found
        (error "Illegal move in position command: ~a (FEN ~a)" ms (game-to-fen game)))
      (make-move-on-board game found))))

(defun %uci-handle-position (tokens)
  "Handle tokens after the 'position' keyword. Sets *UCI-GAME*."
  (cond
    ((and (>= (length tokens) 1) (string= (first tokens) "startpos"))
     (setf *uci-game* (make-chess-game))
     (setup-initial-position *uci-game*)
     (let ((i (position "moves" tokens :test #'string=)))
       (when i
         (%uci-apply-position-moves *uci-game* (subseq tokens (1+ i))))))
    ((and (>= (length tokens) 1) (string= (first tokens) "fen"))
     ;; position fen <6 fields> [moves ...]
     (let ((moves-idx (position "moves" tokens :test #'string=)))
       (unless (>= (length tokens) 7)
         (error "position fen needs 6 FEN fields"))
       (setf *uci-game*
             (make-game-from-fen
              (format nil "~a ~a ~a ~a ~a ~a"
                      (nth 1 tokens) (nth 2 tokens) (nth 3 tokens)
                      (nth 4 tokens) (nth 5 tokens) (nth 6 tokens))))
       (when moves-idx
         (%uci-apply-position-moves *uci-game* (subseq tokens (1+ moves-idx))))))
    (t (error "Bad position command"))))

(defun %uci-allocate-time (tokens side)
  "Compute a thinking budget in ms from wtime/btime/winc/binc tokens.
   Simple rule: time-left/25 + increment/2, reserving 50ms."
  (let ((wtime nil) (btime nil) (winc 0) (binc 0))
    (loop for (k v) on tokens by #'cddr do
      (cond ((string= k "wtime") (setf wtime (parse-integer v)))
            ((string= k "btime") (setf btime (parse-integer v)))
            ((string= k "winc") (setf winc (parse-integer v)))
            ((string= k "binc") (setf binc (parse-integer v)))))
    (let* ((mine (if (eq side :white) wtime btime))
           (inc (if (eq side :white) winc binc)))
      (when (null mine)
        (return-from %uci-allocate-time nil))
      (max 10 (min (- mine 50)
                   (+ (floor mine 25) (floor inc 2)))))))

(defun %uci-handle-go (tokens)
  "Run a search per the 'go' parameters and print 'bestmove ...'."
  (let ((depth 4) (depth-given nil) (movetime nil) (infinite nil))
    ;; parse tokens
    (loop for rest on tokens do
      (let ((k (first rest)))
        (cond ((string= k "depth")
               (setf depth (parse-integer (second rest)) depth-given t))
              ((string= k "movetime")
               (setf movetime (parse-integer (second rest))))
              ((string= k "infinite")
               (setf infinite t))
              ((or (string= k "wtime") (string= k "btime")
                   (string= k "winc") (string= k "binc")
                   (string= k "movestogo"))
               ;; time allocation handled below; skip value token
               nil))))
    (when (and (not movetime) (not infinite)
               (or (member "wtime" tokens :test #'string=)
                   (member "btime" tokens :test #'string=)))
      (setf movetime (%uci-allocate-time tokens (chess-game-side-to-move *uci-game*))))
    ;; a clock (but no explicit depth) means: think for the whole budget
    (when (and movetime (not depth-given))
      (setf depth 64))
    (when infinite
      (setf depth 64 movetime nil))
    (unless *uci-game*
      (setf *uci-game* (make-chess-game))
      (setup-initial-position *uci-game*))
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (best score done nodes)
          (search-with-limits *uci-game* :depth depth :movetime-ms movetime
                              :info-callback
                              (lambda (d s n elapsed)
                                (format t "info depth ~a score cp ~a nodes ~a time ~a~%"
                                        d s n elapsed)
                                (finish-output)))
        (declare (ignore done))
        (let ((elapsed (round (* (- (get-internal-real-time) start)
                                 (/ 1000 internal-time-units-per-second)))))
          (format t "info nodes ~a time ~a score cp ~a~%" nodes elapsed score)
          (if best
              (format t "bestmove ~a~%" (move-to-string best))
              ;; no legal moves: report none (GUI detects mate/stalemate)
              (format t "bestmove 0000~%"))
          (finish-output))))))

(defun %uci-handle-line (line)
  "Process one UCI input LINE. Returns :quit when the session should end."
  (let ((tokens (%uci-tokens line)))
    (when (null tokens)
      (return-from %uci-handle-line nil))
    (let ((cmd (first tokens))
          (args (rest tokens)))
      (cond ((string= cmd "uci")
             (format t "id name ~a ~a~%id author ~a~%uciok~%"
                     *engine-name* *engine-version* *engine-author*)
             (finish-output))
            ((string= cmd "isready")
             (format t "readyok~%") (finish-output))
            ((string= cmd "ucinewgame")
             (setf *uci-game* (make-chess-game))
             (setup-initial-position *uci-game*))
            ((string= cmd "position")
             (%uci-handle-position args))
            ((string= cmd "go")
             (%uci-handle-go args))
            ((string= cmd "stop")
             ;; single-threaded: searches run to completion; nothing to stop.
             nil)
            ((string= cmd "quit")
             (return-from %uci-handle-line :quit))
            ;; best-effort extras (not part of strict UCI)
            ((string= cmd "d")
             (unless *uci-game*
               (setf *uci-game* (make-chess-game))
               (setup-initial-position *uci-game*))
             (print-board *uci-game*))
            ((string= cmd "perft")
             (unless *uci-game*
               (setf *uci-game* (make-chess-game))
               (setup-initial-position *uci-game*))
             (let ((d (if args (parse-integer (first args)) 3)))
               (format t "perft(~a) = ~a~%" d (perft *uci-game* d))
               (finish-output)))
            (t
             (format t "info string unknown command: ~a~%" cmd)
             (finish-output)))))
  nil)

(defun run-uci-loop ()
  "Read UCI commands from *STANDARD-INPUT* until 'quit'."
  (setf *uci-game* (make-chess-game))
  (setup-initial-position *uci-game*)
  (loop for line = (read-line *standard-input* nil nil)
        while line do
          (when (eq (%uci-handle-line line) :quit)
            (return)))
  (values))
