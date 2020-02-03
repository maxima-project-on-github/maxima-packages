;; List of active lexical environments;
;; the innermost environment is the first element.
;; Each environment is a hash table.
;; These are stored as the ENV property on a symbol.

(defvar *active-lexical-environments* nil)

(defmacro with-lexical-environment (env-name-list &rest body)
  `(let (symbols vals)
    ;; If some of the environments named by ENV-NAME-LIST are already active,
    ;; we don't need to bind those symbols. 
    ;; Hpwever, that's doesn't change the behavior of WITH-LEXICAL-ENVIRONMENT,
    ;; so let's do it the simpler way for now. !!
    (mapcar #'(lambda (env-name) (maphash #'(lambda (s v) (push s symbols) (push v vals)) (get env-name 'env))) ,env-name-list)
    (when symbols (mbind symbols vals nil))
    (unwind-protect
      (let ((result ,@body))
        (list '($closure) (cons '(mlist) ,env-name-list) result))
      (mapcar #'(lambda (env-name)
                  (let ((env (get env-name 'env)))
                    (maphash #'(lambda (s v) (setf (gethash s env) (if (boundp s) (symbol-value s) s))) env))) ,env-name-list)
      (when symbols (munbind symbols)))))

;; NOT SURE IF FREEOF IS THE APPROPRIATE TEST HERE !!
(defun freeof-env (e x)
  (let (symbols)
    (maphash #'(lambda (s v) (push s symbols)) e)
    ($lfreeof (cons '(mlist) symbols) x)))

;; HOW IS Z SUPPOSED TO BE USED HERE ??
(defun simplify-$closure (x vestigial z)
  (declare (ignore vestigial))
  (let
    ((env-name-list (rest (second x)))
     (result (third x)))
    ;; PROBABLY NEED TO LOOK AT VALUES IN INNER ENVIROMENTS BEFORE SAYING OUTER ENVIRONMENT CAN GO AWAY !!
    (let ((new-env-name-list (remove-if #'(lambda (e) (freeof-env (get e 'env) result)) env-name-list)))
      (if (null new-env-name-list)
        result
        (cond
          ((and (consp result) (eq (caar result) 'lambda))
           ;; Smash list of environments into expression car
           ;; and throw away $CLOSURE.
           ;; DOES NEW-ENV-NAME-LIST NEED TO GO BEFORE OR AFTER ANY ENVIRONMENTS ALREADY PRESENT IN CAR ??
           ;; OR MAYBE IT DOESN'T MATTER ??
           (cons (append (car result) new-env-name-list) (cdr result)))
          ((and (consp result) (eq (caar result) '$closure))
           ;; Nested closure -- flatten.
           (let ((inner-env-name-list (rest (second result))))
             (cons '($closure) (list (cons '(mlist) (append new-env-name-list inner-env-name-list)) (third result)))))
          ;; Otherwise RESULT is a general expression, wrap it in $CLOSURE.
          (t (list '($closure simp) (cons '(mlist simp) new-env-name-list) result)))))))

(setf (get '$closure 'operators) 'simplify-$closure)

(defun extract-env-name-list (car-expr)
  (let (env-name-list)
    (mapcar #'(lambda (x) (if (and (symbolp x) (hash-table-p (get x 'env))) (push x env-name-list))) car-expr)
    (reverse env-name-list)))

(let ((mlambda-prev (symbol-function 'mlambda)))
  (defun mlambda (fn args fnname noeval form)
    (let ((env-name-list (extract-env-name-list (car fn))))
      (with-lexical-environment env-name-list (funcall mlambda-prev fn args fnname noeval form)))))

(defun mdefine1 (args body)
  (list (append '(lambda) *active-lexical-environments*) (cons '(mlist) args) body))

(let ((mbind-doit-prev (symbol-function 'mbind-doit)))
  (defun mbind-doit (lamvars fnargs fnname)
    (when lamvars
      (let
        ((new-env (make-hash-table))
         (new-env-id (gensym "ENV")))
        ;; EXCLUDE NON-LEXICAL VARIABLES HERE ?? I DUNNO !!
        (mapcar #'(lambda (s v) (setf (gethash s new-env) v)) lamvars fnargs)
        (setf (get new-env-id 'env) new-env)
        (push new-env-id *active-lexical-environments*)))
    (funcall mbind-doit-prev lamvars fnargs fnname)))

(let ((munbind-prev (symbol-function 'munbind)))
  (defun munbind (vars)
    ;; COULD COMPARE VARS AGAINST (CAR *ACTIVE-LEXICAL-ENVIRONMENTS*) HERE !!
    ;; FOR NOW JUST POP WITHOUT INSPECTING IT !!
    (when vars (pop *active-lexical-environments*))
    (funcall munbind-prev vars)))

(defun extract-local-vars (e)
  (if (and (rest e) ($listp (second e)))
    (let ((vars+var-inits (rest (second e))))
      (mapcar #'(lambda (e1) (if (consp e1) (list (second e1) (third e1)) (list e1 e1))) vars+var-inits))))

(defun extract-body (e)
  (if (and (rest e) ($listp (second e)))
    (rest (rest e))
    (rest e)))

(let ((prev-mfexpr* (get 'mprog 'mfexpr*)))
  (setf (get 'mprog 'mfexpr*)
        #'(lambda (e)
            (let
              ((vars+var-inits-pairs (extract-local-vars e))
               (body (extract-body e)))
              (if vars+var-inits-pairs
                (let
                  ((new-env (make-hash-table))
                   (new-env-id (gensym "ENV")))
                  ;; EXCLUDE NON-LEXICAL VARIABLES HERE ?? I DUNNO !!
                  (mapcar #'(lambda (vv) (setf (gethash (first vv) new-env) (second vv))) vars+var-inits-pairs)
                  (setf (get new-env-id 'env) new-env)
                  (with-lexical-environment (list new-env-id) (funcall (get 'mprogn 'mfexpr*) (cons '(mprogn) body))))
                (funcall prev-mfexpr* e))))))

;; example

(defvar env-1 (make-hash-table))
(setf (gethash '$a456 env-1) 1111)
(defvar env-id-1 (gensym))
(setf (get env-id-1 'env) env-1)

(defvar $mylambda)
(setq $mylambda (list (list 'lambda env-id-1) (list '(mlist) '$x '$y) '((msetq) $a456 ((mtimes) $a456 ((mplus) $x $y)))))

(mputprop '$foo $mylambda 'mexpr) ;; now you can say foo(x, y) and it's evaluated with ENV-1
