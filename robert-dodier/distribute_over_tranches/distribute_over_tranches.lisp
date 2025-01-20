(defun divide-list-into-tranches (l n)
  (let (ll)
    (dotimes (i n) (push nil ll))
    (dotimes (i (length l)) (push (nth i l) (nth (mod i n) ll)))
    (mapcar 'reverse ll)))

(defun seq-length (n)
  (let (ii)
    (dotimes (i n) (push i ii))
    (reverse ii)))

(defmfun $distribute_over_tranches (expr expr-loop-var m n)

  (let*
    ((ii (seq-length m))
     (ii-tranches (divide-list-into-tranches ii n))
     child-pids
     read-fds
     child-values-strings
     child-values)

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
                ((f (lambda (j) (meval (maxima-substitute (1+ j) expr-loop-var expr))))
                 (g (lambda (j) (mfuncall '$string (funcall f j))))
                 (g-f-j (mapcar g indices-to-process))
                 (write-s (lambda (s) (sb-posix:write (second fd-pair) (sb-sys:vector-sap s) (* 4 (length s)))))
                 (write-$ (lambda () (sb-posix:write (second fd-pair) (sb-sys:vector-sap "$") 4))))
                (mapcar (lambda (s) (funcall write-s s) (funcall write-$)) g-f-j))
              (sb-posix:close (second fd-pair))
              (sb-posix:exit 0))
            (progn
              (format t "process ~d given indices [~{~a~^, ~}] to process~%" child-pid indices-to-process)
              (push child-pid child-pids)
              (sb-posix:close (second fd-pair))
              (format t "save fd ~d to read output of process ~d~%" (first fd-pair) child-pid)
              (push (first fd-pair) read-fds))))))

    (setq child-pids (reverse child-pids))
    (setq read-fds (reverse read-fds))

    (dotimes (i n)
      (format t "read from fd ~d for process ~d~%" (nth i read-fds) (nth i child-pids))
      (let*
        ((buf (make-array 1024 :element-type 'extended-char :initial-element #\α))
         (n-bytes-total 0)
         (bufs-list nil)
         (n-bytes (sb-posix:read (nth i read-fds) (sb-sys:vector-sap buf) 1024)))
        (loop while (> n-bytes 0) do
                (format t "obtained ~d bytes from process ~d: ~s~%" n-bytes (nth i child-pids) buf)
                (setq n-bytes-total (+ n-bytes-total n-bytes))
                (push (make-array (/ n-bytes 4) :element-type 'extended-char :displaced-to buf)  bufs-list)
                #+nil (push (make-array n-bytes :element-type 'extended-char :displaced-to buf)  bufs-list)
                #+nil (push buf bufs-list)
                (setq buf (make-array 1024 :element-type 'extended-char :initial-element #\α))
                (setq n-bytes (sb-posix:read (nth i read-fds) (sb-sys:vector-sap buf) (* 1024 4))))
        (setq bufs-list (reverse bufs-list))
        (format t "obtained ~d total bytes (~d characters) from process ~d~%" n-bytes-total (/ n-bytes-total 4) (nth i child-pids))
        (format t "bufs list from process ~d: ~a~%" (nth i child-pids) bufs-list)
        (format t "LENGTH as applied to items on bufs list for process ~d: ~a~%" (nth i child-pids) (mapcar 'length bufs-list))
        (let ((concatenated-bufs (apply 'concatenate (cons 'string bufs-list))))
          (format t "concatenated (LENGTH = ~d) from process ~d: ~s~%" (length concatenated-bufs) (nth i child-pids) concatenated-bufs)
          (push concatenated-bufs child-values-strings)))
      (sb-posix:close (nth i read-fds)))

    (setq child-values-strings (reverse child-values-strings))

    (dotimes (i n)
      (sb-posix:waitpid (nth i child-pids) 0))
    
    (dotimes (i n)
      (let ((string-input (make-string-input-stream (nth i child-values-strings))) child-values-1)
        (dotimes (j (length (nth i ii-tranches)))
          (push (third (mread-raw string-input)) child-values-1))
        (setq child-values-1 (reverse child-values-1))
        (push child-values-1 child-values)))

    (setq child-values (reverse child-values))

    (let ((all-values-array (make-array m)))
      (dotimes (i n)
        (dotimes (j (length (nth i ii-tranches)))
          (setf (aref all-values-array (nth j (nth i ii-tranches))) (nth j (nth i child-values)))))
      (cons '(mlist simp) (coerce all-values-array 'list)))))
