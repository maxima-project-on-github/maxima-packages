;; lexical_symbols.lisp -- lexical symbols for Maxima
;; copyright 2012, 2019 by Robert Dodier
;; I release this work under terms of the GNU GPL
;;
;; Constructs which have local variables, namely
;; block, function definitions (named and unnamed), and for loops,
;; are redefined so that the local variables are lexical symbols.
;; That is, two local variables which have the same name in
;; different constructs are distinct.
;;
;; Local variables are made lexical, via gensym substitution,
;; when the construct (block, function, or for loop) is parsed.
;;
;; This file, by itself, only implements lexical local variables.
;; Other files in this directory implement closures.
;;
;; This file defines a new feature, global,
;; which makes symbols dynamic instead of lexical.
;; One can say declare(foo, global) and then featurep(foo, global) => true,
;; or, equivalently, (KINDP '$FOO '$GLOBAL) => T.
;; Any symbols which have been declared global by the time
;; the construct is parsed are excluded from gensym substitution.

;; Some share functions have been redefined with lexical scope.
;; Assign MEXPR property for symbols which have MEXPR at present.
;; This part can go away when the lexical symbol implementation
;; is in the Maxima image.

($auto_mexpr '$trigrat "trigrat.lisp")
($auto_mexpr '$trigsimp "trgsmp.mac")
($auto_mexpr '$facsum "facexp.mac")
($auto_mexpr '$factorfacsum "facexp.mac")
($auto_mexpr '$collectterms "facexp.mac")

(mfuncall '$declare '$global '$feature)
;; Ensure that global feature isn't removed by reset()
;; by saving current list of features.
(setf (gethash '$features *variable-initial-values*) $features)

;; Declare all DEFMVAR symbols as global.
;; Assume the keys of the hash table *VARIABLE-INITIAL-VALUES*
;; is exactly the set of such symbols.
;; Make these declarations in the global context to protect them from kill.

(let (defmvars-list (save-$props (copy-list $props)) (save-$context $context))
  (maphash #'(lambda (k v) (push k defmvars-list)) *variable-initial-values* )
  (mset '$context '$global)
  (declare1 defmvars-list t '$global 'kind)
  (mset '$context save-$context)
  (setq $props save-$props))
 
;; Variables defined by define_variable are also declared global.

(let ((prev-mfexpr* (get '$define_variable 'mfexpr*)))
  (setf (get '$define_variable 'mfexpr*)
        #'(lambda (e)
            (let ((var (second e)))
              (declare1 (list var) t '$global 'kind)
              (funcall prev-mfexpr* e)))))

;; Lexicalize MPROG (i.e., block([a, b, c, ...], ...))

(defun maybe-subst-lexical-symbols-into-mprog (e)
  (if (or (null (cdr e)) (not ($listp (cadr e))))
    e
    (let*
      ((mprog-op (car e))
       (mprog-args (cdr e))
       (vars+var-inits (cdr (car mprog-args)))
       (exprs (cdr mprog-args)))
      (subst-lexical-symbols-into-mprog mprog-op vars+var-inits exprs))))

(defun make-lexical-gensym (s)
  (let*
    ((s1 (gensym (symbol-name s)))
     (%s ($nounify s))
     (%s1 ($nounify s1)))
    (setf (get s1 'reversealias) (or (get s 'reversealias) s))
    (setf (get %s1 'reversealias) (or (get %s 'reversealias) %s))
    s1))

(defun subst-lexical-symbols-into-mprog (mprog-op vars+var-inits exprs)
  (let*
   ((vars+var-inits-lexical (remove-if #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$global)) vars+var-inits))
    (vars+var-inits-global (remove-if-not #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$global)) vars+var-inits))
    (vars-only-lexical (remove-if-not #'symbolp vars+var-inits-lexical))
    (var-inits-lexical (remove-if #'symbolp vars+var-inits-lexical))
    (var-inits-vars-lexical (mapcar #'second var-inits-lexical))
    (vars-all-lexical (append vars-only-lexical var-inits-vars-lexical))
    (vars-only-gensyms (mapcar #'make-lexical-gensym vars-only-lexical))
    (var-inits-gensyms (mapcar #'make-lexical-gensym var-inits-vars-lexical))
    (gensyms-all (append vars-only-gensyms var-inits-gensyms))
    (subst-eqns (apply #'append (mapcar #'(lambda (x y)
                                            (list `((mequal) ,x ,y)
                                                  `((mequal) ,($nounify x) ,($nounify y))))
                                        vars-all-lexical gensyms-all)))
    (gensym-mprogn (let (($simp nil)) ($substitute `((mlist) ,@ subst-eqns) `((mprogn) ,@ exprs))))
    (gensym-inits (mapcar #'(lambda (e y) (list (first e) y (third e))) var-inits-lexical var-inits-gensyms))
    (gensym-mprog `(,mprog-op ((mlist) ,@ (append vars-only-gensyms gensym-inits vars+var-inits-global)) ,@ (cdr gensym-mprogn))))
   gensym-mprog))

(defvar right-paren-symbol '|$)|)

(def-nud (mprog) (op)
  (pop-c) ;; eat the opening parenthesis
  (let
    ((right (prsmatch right-paren-symbol '$any))
     (header (mheader 'mprog)))
    (cons '$any (maybe-subst-lexical-symbols-into-mprog (cons header right)))))

;; Lexicalize MDEFINE (i.e. f(a, b, c, ...) := ..., also f[a, b, c, ...] := ..., and f[a, b, c, ...](x, y, z, ...) := ...)

(defun subst-lexical-symbols-into-mdefine-or-lambda (e)
  (let*
    ((args (remove-if #'(lambda (x) (kindp x '$global)) (extract-arguments-symbols e)))
     (args-gensyms (mapcar
                     #'(lambda (s)
                         (let ((s1 (gensym (symbol-name s))))
                           (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) args))
     (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) args args-gensyms))
     (substituted-definition (let (($simp nil)) ($substitute `((mlist) ,@ subst-eqns) e)))
     (function-header (first (second e)))
     (function-header-new (cons function-header (rest (second substituted-definition)))))
    (append (list (first e) function-header-new) (cddr substituted-definition))))

(def-led (|$:=| 180. 20.) (op left)
  (let ((e (parse-infix op left)))
    (cons (first e) (subst-lexical-symbols-into-mdefine-or-lambda (rest e)))))

(defun extract-arguments-symbols (e)
  (let*
    ((e-lhs (second e))
     (is-mqapply (eq (caar e-lhs) 'mqapply))
     (e-lhs-op ($op e-lhs))
     (e-lhs-args ($args e-lhs)))
    (if is-mqapply
      (rest ($listofvars ($append ($args e-lhs-op) e-lhs-args)))
      (rest ($listofvars e-lhs-args)))))

;; Lexicalize LAMBDA (i.e., lambda([a, b, c, ...], ...))

(def-nud (lambda) (op)
  (if (eq (first-c) '|$(|)
    (progn
      (pop-c) ;; eat the opening parenthesis
      (let
        ((right (prsmatch right-paren-symbol '$any))
         (header (mheader 'lambda)))
        (cons '$any (subst-lexical-symbols-into-mdefine-or-lambda (cons header right)))))
    `($any . ,op)))

;; Lexicalize MDO and MDOIN (i.e., for x: ... do ..., and for x in ... do ...)

(defparameter parse-$do-prev #'parse-$do)

(defun parse-$do (&rest a)
  (let*
    ((do-expr (apply parse-$do-prev a))
     (var (third do-expr))
     (var-subst (gensym (symbol-name var)))
     (next (sixth do-expr))
     (unless (eighth do-expr))
     (body (ninth do-expr)))
    (setf (get var-subst 'reversealias) (or (get var 'reversealias) var))
    (setf (third do-expr) var-subst)
    (let (($simp nil))
      (setf (sixth do-expr) (maxima-substitute var-subst var next))
      (setf (eighth do-expr) (maxima-substitute var-subst var unless))
      (setf (ninth do-expr) (maxima-substitute var-subst var body)))
    do-expr))

(setf (get '$for 'nud) #'parse-$do)

