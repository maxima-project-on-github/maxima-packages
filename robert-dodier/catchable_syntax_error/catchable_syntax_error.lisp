;; catchable_syntax_error.lisp
;; copyright 2019 by Robert Dodier
;; I release this work under terms of the GNU General Public License v2

;; Helper for MREAD-SYNERR.
;; Adapted from local function PRINTER in built-in MREAD-SYNERR.

(defun mread-synerr-printer (x)
  (cond ((symbolp x)
         (print-invert-case (stripdollar x)))
        ((stringp x)
         (maybe-invert-string-case x))
        (t x)))

;; Punt to Maxima function 'error' so that syntax errors can be caught by 'errcatch'.
;; This definition replaces the built-in MREAD-SYNERR
;; which throws to the top level of the interpreter in a way which cannot
;; be intercepted by 'errcatch'.
;;
;; After a syntax error is detected, the global variable 'error'
;; contains the error message (which is also printed on the console
;; when the error occurs).
;;
;; Aside from punting to 'error', this implementation doesn't try to
;; do anything else which the built-in MREAD-SYNERR does. In particular
;; this implementation doesn't try to output any input-line information.

(defun mread-synerr (format-string &rest l)
  (let*
    ((format-string-1 (concatenate 'string "syntax error: " format-string))
     (format-string-args (mapcar #'mread-synerr-printer l))
     (message-string (apply #'format nil format-string-1 format-string-args)))
    (declare (special *parse-stream*))
    (when (eql *parse-stream* *standard-input*)
      (read-line *parse-stream* nil nil))
    ($error message-string)))
