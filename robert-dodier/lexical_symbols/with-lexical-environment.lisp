;; List of active lexical environments;
;; the innermost environment is the first element.
;; Each environment is a hash table.
;; These are stored as the ENV property on a symbol.

(defvar *active-lexical-environments* nil)

(defun get-lexical-environments-symbols+values (env-name-list)
  (let ((not-yet-active (remove-if #'(lambda (e) (member e *active-lexical-environments*)) env-name-list)))
    (get-lexical-environments-symbols+values-1 not-yet-active)))

(defun get-lexical-environments-symbols+values-1 (env-name-list)
  (let (symbols vals)
    (mapcar #'(lambda (env-name) (maphash #'(lambda (s v) (push s symbols) (push v vals)) (get env-name 'env))) env-name-list)
    (values (reverse symbols) (reverse vals))))

(defun update-lexical-environment-symbol-value (env s)
  (setf (gethash s env) (if (boundp s) (symbol-value s) s)))

(defun update-lexical-environments (env-name-list)
  (mapcar #'(lambda (env-name)
              (let ((env (get env-name 'env)))
                (maphash #'(lambda (s v)
                             (declare (ignore v))
                             (update-lexical-environment-symbol-value env s)) env))) env-name-list))

(defmacro with-lexical-environment (env-name-list &rest body)
  `(multiple-value-bind (symbols vals) (get-lexical-environments-symbols+values ,env-name-list)
     (let ((*active-lexical-environments* *active-lexical-environments*))
       (dolist (e ,env-name-list) (push e *active-lexical-environments*))
       (when symbols (mbind symbols vals nil))
       (unwind-protect
         (let ((result ,@body))
           (simplifya (list '($closure) (cons '(mlist) ,env-name-list) result) t))
         (update-lexical-environments ,env-name-list)
         (when symbols (munbind symbols))))))

;; NOT SURE IF FREEOF IS THE APPROPRIATE TEST HERE !!
(defun freeof-env (e x)
  (let (symbols)
    (maphash #'(lambda (s v) (push s symbols)) e)
    ($lfreeof (cons '(mlist) symbols) x)))

(defun $get_env (e)
  (let ((e-env (get e 'env)))
    (if e-env
      (let (k-equals-v)
        (maphash #'(lambda (k v) (push (list '(mequal) k v) k-equals-v)) e-env)
        (list '(mequal) e ($sort (cons '(mlist) k-equals-v))))
      (merror "get_env: argument must be an environment; found: ~M" e))))

(defun $get_envs (c)
  (unless (and (consp c) (eq (caar c) '$closure))
    (merror "get_envs: argument must be a closure; found: ~M" c))
  (cons '(mlist) (mapcar #'$get_env (rest (first (rest c))))))

(defun simplify-$closure (x vestigial z)
  (declare (ignore vestigial))
  (let
    ((env-name-list (rest (second x)))
     (result (third x)))
    ;; PROBABLY NEED TO LOOK AT VALUES IN INNER ENVIROMENTS BEFORE SAYING OUTER ENVIRONMENT CAN GO AWAY !!
    (let ((new-env-name-list (remove-if #'(lambda (e)
                                            ;; Return NIL if E doesn't have an ENV property,
                                            ;; which means E won't be simplified away.
                                            ;; I guess that's questionable !!
                                            (let ((e-env (get e 'env)))
                                              (if e-env (freeof-env e-env result))))
                                        env-name-list)))
      (if (null new-env-name-list)
        (simplifya result z)
        (cond
          #+nil ;; DISABLE CLOSURE(LAMBDA) --> LAMBDA W/ ENV IN EXPRESSION CAR !!
          ((and (consp result) (eq (caar result) 'lambda))
           ;; Smash list of environments into expression car
           ;; and throw away $CLOSURE.
           ;; DOES NEW-ENV-NAME-LIST NEED TO GO BEFORE OR AFTER ANY ENVIRONMENTS ALREADY PRESENT IN CAR ??
           ;; OR MAYBE IT DOESN'T MATTER ??
           (cons (append (car result) new-env-name-list) (cdr result)))
          ((and (consp result) (eq (caar result) '$closure))
           ;; Nested closure -- flatten.
           (let ((inner-env-name-list (rest (second result))))
             (simplify-$closure (cons '($closure) (list (cons '(mlist) (append new-env-name-list inner-env-name-list)) (third result))) nil z)))
          ;; Otherwise RESULT is a general expression, wrap it in $CLOSURE.
          (t (list '($closure simp) (cons '(mlist simp) new-env-name-list) result)))))))

(setf (get '$closure 'operators) 'simplify-$closure)

;; SINCE ENV LIST IS NO LONGER IN EXPRESSION CAR, THIS FUNCTION ALWAYS RETURNS NIL !!
;; THAT WON'T BREAK ANYTHING SO LET IT BE FOR NOW !!
(defun extract-env-name-list (car-expr)
  (let (env-name-list)
    (mapcar #'(lambda (x) (if (and (symbolp x) (hash-table-p (get x 'env))) (push x env-name-list))) car-expr)
    (reverse env-name-list)))

(let ((mlambda-prev (symbol-function 'mlambda)))
  (defun mlambda (fn args fnname noeval form)
    (let ((env-name-list (extract-env-name-list (car fn))))
      (with-lexical-environment env-name-list (funcall mlambda-prev fn args fnname noeval form)))))

;; MAPPLY1 looks for a hook attached to the operator, let's use that.

(defun mapply1-extension-$closure (fn args fnname form)
  (let*
    ((is-array-ref (and (consp form) (member 'array (car form)))) ;; I DUNNO; IS THIS REALLY GUARANTEED TO DETECT ARRAY REFS ??
     (array-foo (if is-array-ref (list 'array))))
    (with-lexical-environment (rest (second fn)) (meval `((mqapply ,@ array-foo) ,(third fn) ,@ args)))))

(setf (get '$closure 'mapply1-extension) 'mapply1-extension-$closure)

;; adapted from MQAPPLY1 in src/mlisp.lisp.

(defun mqapply1 (form)
  (declare (special aryp))
  (destructuring-let (((fn . argl) (cdr form)) (aexprp))
    (unless (mquotep fn) (setq fn (meval fn)))
    (cond ((atom fn)
           (meval (cons (cons (amperchk fn) aryp) argl)))
          ((eq (caar fn) '$closure) ;; NEW
           (with-lexical-environment (rest (second fn)) (meval `((mqapply) ,(third fn) ,@ argl)))) ;; NEW
          ((eq (caar fn) 'lambda)
           (if aryp
               (merror (intl:gettext "lambda: cannot apply lambda as an array function."))
               (mlambda fn argl (cadr form) noevalargs form)))
          (t
           (mapply1 fn (mevalargs argl) (cadr form) form)))))

(defun mdefine1 (args body)
  #+nil (list (append '(lambda) *active-lexical-environments*) (cons '(mlist) args) body)
  #+nil `((lambda) ((mlist) ,@args) (($closure) ((mlist) ,@*active-lexical-environments*) ,body))
  #-nil `(($closure) ((mlist) ,@*active-lexical-environments*) ((lambda) ((mlist) ,@args) ,body)))

#+nil
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

#+nil
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
                  ;; OH LOOK, THERE'S A CALL TO MEVAL !!
                  (mapcar #'(lambda (vv) (setf (gethash (first vv) new-env) (meval (second vv)))) vars+var-inits-pairs)
                  (setf (get new-env-id 'env) new-env)
                  (with-lexical-environment (list new-env-id) (funcall (get 'mprogn 'mfexpr*) (cons '(mprogn) body))))
                (funcall prev-mfexpr* e))))))

;; example
#|
(defvar env-1 (make-hash-table))
(setf (gethash '$a456 env-1) 1111)
(defvar env-id-1 (gensym))
(setf (get env-id-1 'env) env-1)

(defvar $mylambda)
(setq $mylambda (list (list 'lambda env-id-1) (list '(mlist) '$x '$y) '((msetq) $a456 ((mtimes) $a456 ((mplus) $x $y)))))

(mputprop '$foo $mylambda 'mexpr) ;; now you can say foo(x, y) and it's evaluated with ENV-1
 |#
