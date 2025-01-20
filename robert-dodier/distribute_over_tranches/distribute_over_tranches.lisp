(defun divide-list-into-tranches (l n)
  (let (ll)
    (dotimes (i n) (push nil ll))
    (dotimes (i (length l)) (push (nth i l) (nth (mod i n) ll)))
    (mapcar 'reverse ll)))

(defun seq-1-length (n)
  (let (ii)
    (dotimes (i n) (push (1+ i) ii))
    (reverse ii)))

(defmfun $distribute_over_tranches (expr expr-loop-var m n)

  (let*
    ((ii (seq-1-length m))
     (ii-tranches (divide-list-into-tranches ii n))
     child-pids
     fds-for-parent-to-read)

    (dotimes (i n)
      (let
        ((indices-to-process (nth i ii-tranches))
         (fd-pair (multiple-value-list (sb-posix:pipe))))
        (format t "for tranche ~d, fd pair = ~a~%" i fd-pair)
        (let ((child-pid (sb-posix:fork)))
          (if (eql child-pid 0)
            (progn
              (sb-posix:close (first fd-pair))
              (let*
                ((f (lambda (j) (meval (maxima-substitute j expr-loop-var expr))))
                 (g (lambda (j) (mfuncall '$string (funcall f j))))
                 (g-f-j (mapcar g indices-to-process)))
                (mapcar (lambda (s) (sb-posix:write (second fd-pair) (sb-sys:vector-sap s) (* 4 (length s)))) g-f-j))
              (sb-posix:close (second fd-pair))
              (sb-posix:exit 0))
            (progn
              (format t "process ~d given indices [~{~a~^, ~}] to process~%" child-pid indices-to-process)
              (push child-pid child-pids)
              (sb-posix:close (second fd-pair))
              (format t "save fd ~d to read output of process ~d~%" (first fd-pair) child-pid)
              (push (first fd-pair) fds-for-parent-to-read))))))

    (setq fds-for-parent-to-read (reverse fds-for-parent-to-read))

    (dotimes (i n)
      (format t "read from fd ~d for process ~d~%" (nth i fds-for-parent-to-read) (nth i child-pids))
      (let*
        ((buf (make-array 16384 :element-type 'standard-char))
         (n-bytes (sb-posix:read (nth i fds-for-parent-to-read) (sb-sys:vector-sap buf) 16384)))
        (loop while (> n-bytes 0) do
              (format t "read ~d bytes from process ~d: ~a~%" n-bytes (nth i child-pids) buf)
              (setq n-bytes (sb-posix:read (nth i fds-for-parent-to-read) (sb-sys:vector-sap buf) 16384))))
      (sb-posix:close (nth i fds-for-parent-to-read)))

    (dotimes (i n)
      (sb-posix:waitpid (nth i child-pids) 0))))
