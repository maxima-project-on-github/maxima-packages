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
     child-pids)

    (dotimes (i n)
      (let ((indices-to-process (nth i ii-tranches)))
        (let ((child-pid (sb-posix:fork)))
          (if (eql child-pid 0)
            (progn
              (mapcar (lambda (j) (meval (maxima-substitute j expr-loop-var expr))) indices-to-process)
              (sb-posix:exit 0))
            (progn
              (format t "process ~d given indices [~{~a~^, ~}] to process~%" child-pid indices-to-process)
              (push child-pid child-pids))))))

    (dotimes (i n)
      (sb-posix:waitpid (nth i child-pids) 0))))
