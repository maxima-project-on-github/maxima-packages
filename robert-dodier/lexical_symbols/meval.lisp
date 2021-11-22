(setf (symbol-function 'prev-meval) (symbol-function 'meval)) ;; traceable
(defun meval (e)
  (if (and (consp e) (eq (caar e) '$closure))
    (with-lexical-environment (rest (second e)) (prev-meval (third e)))
    (prev-meval e)))
