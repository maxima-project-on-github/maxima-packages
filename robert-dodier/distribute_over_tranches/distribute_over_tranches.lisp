(defun divide-into-tranches (m n)
  (let ((ll (make-array n :initial-element nil)))
    (dotimes (i m) (push i (aref ll (mod i n))))
    (dotimes (i n) (setf (aref ll i) (reverse (aref ll i))))
    ll))

(defmfun $distribute_over_tranches (expr expr-loop-var m n)

  (let*
    ((ii-tranches (divide-into-tranches m n))
     (child-pids (make-array n))
     (read-fds (make-array n))
     (child-values-strings (make-array n))
     (child-values (make-array n)))

    (dotimes (i n)
      (let
        ((indices-to-process (aref ii-tranches i))
         (fd-pair (multiple-value-list (sb-posix:pipe))))

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
              (setf (aref child-pids i) child-pid)
              (sb-posix:close (second fd-pair))
              (setf (aref read-fds i) (first fd-pair)))))))

    (dotimes (i n)
      (let*
        ((buf (make-array 1024 :element-type 'extended-char :initial-element #\α))
         (n-bytes-total 0)
         (bufs-list nil)
         (n-bytes (sb-posix:read (aref read-fds i) (sb-sys:vector-sap buf) 1024)))
        (loop while (> n-bytes 0) do
                (setq n-bytes-total (+ n-bytes-total n-bytes))
                (push (make-array (/ n-bytes 4) :element-type 'extended-char :displaced-to buf)  bufs-list)
                (setq buf (make-array 1024 :element-type 'extended-char :initial-element #\α))
                (setq n-bytes (sb-posix:read (aref read-fds i) (sb-sys:vector-sap buf) (* 1024 4))))
        (setq bufs-list (reverse bufs-list))
        (let ((concatenated-bufs (apply 'concatenate (cons 'string bufs-list))))
          (setf (aref child-values-strings i) concatenated-bufs)))
      (sb-posix:close (aref read-fds i)))

    (dotimes (i n)
      (sb-posix:waitpid (aref child-pids i) 0))
    
    (dotimes (i n)
      (let
        ((string-input (make-string-input-stream (aref child-values-strings i)))
         (child-values-1 (make-array (length (aref ii-tranches i)))))
        (dotimes (j (length (aref ii-tranches i)))
          (setf (aref child-values-1 j) (third (mread-raw string-input))))
        (setf (aref child-values i) child-values-1)))

    (let ((all-values-array (make-array m)))
      (dotimes (i n)
        (dotimes (j (length (aref ii-tranches i)))
          (setf (aref all-values-array (nth j (aref ii-tranches i))) (aref (aref child-values i) j))))
      (cons '(mlist) (coerce all-values-array 'list)))))
