;; List of active lexical environments.
;; Each environment is a hash table.
;; These are stored as the ENV property on a symbol.

(defvar *active-lexical-environments* nil)

(defmacro with-lexical-environment (env-list &rest body)
  `(let (symbols vals)
    (mapcar #'(lambda (env) (maphash #'(lambda (s v) (push s symbols) (push v vals)) env)) ,env-list)
    (mbind symbols vals nil)
    (unwind-protect (meval (list '(mprogn) ,@body))
      (mapcar #'(lambda (env) (maphash #'(lambda (s v) (setf (gethash s env) (symbol-value s))) env)) ,env-list)
      (munbind symbols))))

(defun extract-env-list (car-expr)
  (let (env-list)
    (mapcar #'(lambda (x) (if (and (symbolp x) (hash-table-p (get x 'env))) (push (get x 'env) env-list))) car-expr)
    env-list))

(let ((mlambda-prev (symbol-function 'mlambda)))
  (defun mlambda (fn args fnname noeval form)
    (let ((env-list (extract-env-list (car fn))))
      (with-lexical-environment env-list (funcall mlambda-prev fn args fnname noeval form)))))

(defun mdefine1 (args body)
  (list (append '(lambda) *active-lexical-environments*) (cons '(mlist) args) body))

(let ((mbind-doit-prev (symbol-function 'mbind-doit)))
  (defun mbind-doit (lamvars fnargs fnname)
    (let
      ((new-env (make-hash-table))
       (new-env-id (gensym "ENV")))
      ;; EXCLUDE NON-LEXICAL VARIABLES HERE ?? I DUNNO !!
      (mapcar #'(lambda (s v) (setf (gethash s new-env) v)) lamvars fnargs)
      (setf (get new-env-id 'env) new-env)
      (push new-env-id *active-lexical-environments*))
    (funcall mbind-doit-prev lamvars fnargs fnname)))

(let ((munbind-prev (symbol-function 'munbind)))
  (defun munbind (vars)
    ;; COULD COMPARE VARS AGAINST (CAR *ACTIVE-LEXICAL-ENVIRONMENTS*) HERE !!
    ;; FOR NOW JUST POP WITHOUT INSPECTING IT !!
    (pop *active-lexical-environments*)
    (funcall munbind-prev vars)))

;; example

(defvar env-1 (make-hash-table))
(setf (gethash '$a456 env-1) 1111)
(defvar env-id-1 (gensym))
(setf (get env-id-1 'env) env-1)

(defvar $mylambda)
(setq $mylambda (list (list 'lambda env-id-1) (list '(mlist) '$x '$y) '((msetq) $a456 ((mtimes) $a456 ((mplus) $x $y)))))

(mputprop '$foo $mylambda 'mexpr) ;; now you can say foo(x, y) and it's evaluated with ENV-1
