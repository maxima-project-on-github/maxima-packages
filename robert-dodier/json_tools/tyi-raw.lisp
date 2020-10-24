;; work-around for bug in SBCL;
;; see: https://bugs.launchpad.net/sbcl/+bug/690408
;; This TYI-RAW is identical to the one in src/commac.lisp
;; except that the call to READ-CHAR-NO-HANG has been replaced by READ-CHAR.

(defun tyi-raw (&optional (stream *standard-input*) eof-option)
  ;; Adding this extra EOF test, because the testsuite generates
  ;; unexpected end of input-stream with Windows XP and GCL 2.6.8.
  #+gcl
  (when (eql (peek-char nil stream nil eof-option) eof-option)
    (return-from tyi-raw eof-option))

  (let ((ch (read-char stream nil eof-option)))
    (if ch
	ch
	(progn
	  (when (and *prompt-on-read-hang* *read-hang-prompt*)
	    (princ *read-hang-prompt*)
	    (finish-output *standard-output*))
	  (read-char stream nil eof-option)))))
