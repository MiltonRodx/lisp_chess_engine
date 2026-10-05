(defpackage #:chess-tests
  (:use #:cl #:chess)
  (:export #:run-all-tests #:*tests* #:deftest #:is #:is-equal #:signals-error-p))

(in-package #:chess-tests)

(defvar *tests* nil
  "Registry of test thunks: list of (NAME . FUNCTION).")

(defvar *test-pass-count* 0)
(defvar *test-fail-count* 0)
(defvar *current-test* nil)

(defmacro deftest (name &body body)
  "Define and register a test named NAME."
  `(progn
     (defun ,name () ,@body)
     (pushnew (cons ',name #',name) *tests* :key #'car)
     ',name))

(defmacro is (form &optional msg)
  "Assert FORM is true. Records a failure instead of aborting the suite."
  `(let ((val ,form)
         (m ,msg))
     (if val
         (incf *test-pass-count*)
         (progn
           (incf *test-fail-count*)
           (format t "~&  FAIL [~a]: ~a => ~a~@[ (~a)~]~%"
                   *current-test* ',form val m)))))

(defmacro is-equal (expected actual &optional msg)
  "Assert EXPECTED equals ACTUAL via EQUAL."
  `(let ((exp ,expected) (act ,actual))
     (if (equal exp act)
         (incf *test-pass-count*)
         (progn
           (incf *test-fail-count*)
           (format t "~&  FAIL [~a]: expected ~s, got ~s~@[ (~a)~]~%"
                   *current-test* exp act ,msg)))))

(defmacro signals-error-p (form)
  "True if evaluating FORM signals an error."
  `(handler-case (progn ,form nil)
     (error (c) (declare (ignore c)) t)))

(defun run-all-tests ()
  "Run every registered test. Returns (values passed failed). Prints a summary."
  (setf *test-pass-count* 0 *test-fail-count* 0)
  (format t "~&=== lisp-chess-engine tests: ~a test(s) ===~%" (length *tests*))
  (dolist (entry (reverse *tests*))
    (let ((*current-test* (car entry)))
      (format t "~&-- ~a~%" *current-test*)
      (handler-case (funcall (cdr entry))
        (error (c)
          (incf *test-fail-count*)
          (format t "~&  ERROR [~a]: ~a~%" *current-test* c)))))
  (format t "~&=== Result: ~a assertion(s) passed, ~a failed ===~%"
          *test-pass-count* *test-fail-count*)
  (values *test-pass-count* *test-fail-count*))
