;; intercept assignments and enclose values if a lexical environment is active
;; and the left-hand side is outside the environment.

(defun get-innermost-op (x)
  ;; ASSUME WITHOUT CHECKING THAT X IS A CONS !!
  ;; MQAPPLY WITH ATOMIC ARGUMENT SIMPLIFIES TO NON-MQAPPLY !!
  ;; SO IT SEEMS IMPOSSIBLE THAT X IS NOT A CONS UNLESS X IS UNSIMPLIFIED !!
  ;; PROBABLY BEST TO GUARD AGAINST IT !!
  (if (eq (caar x) 'mqapply)
    (get-innermost-op (second x))
    (caar x)))

(let ((prev-mset (symbol-function 'mset)))
  (defun mset (x y)
    (declare (special *active-lexical-environments*))
    (if *active-lexical-environments*
      (let ((x-symbol (if (symbolp x) x (get-innermost-op x))))
        (if (every #'(lambda (e) (freeof-env (get e 'env) x-symbol)) *active-lexical-environments*)
          (funcall prev-mset x `(($closure) ((mlist) ,@*active-lexical-environments*) ,y))
          (funcall prev-mset x y)))
      (funcall prev-mset x y))))

