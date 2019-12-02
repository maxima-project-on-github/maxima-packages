;; lexical_symbols.lisp -- lexical symbols for Maxima
;; copyright 2012, 2019 by Robert Dodier
;; I release this work under terms of the GNU GPL
;;
;; examples:
;; with_lexical_symbols ([n:100], f(x) := n : n + x, g() := display(n));
;; with_lexical_symbols ([f], f(x) := 2*x);
;; with_lexical_symbols ([a], h(x) := a[x] : 1, i() := arrayinfo(a));

;; After this, one can say declare(foo, special) and then featurep(foo, special) => true,
;; or, equivalently, (KINDP '$FOO '$SPECIAL) => T.
;; Any symbols declared special are excluded from gensym substitution.

(mfuncall '$declare '$special '$feature)

;; Declare all DEFMVAR symbols as special.
;; Assume the keys of the hash table *VARIABLE-INITIAL-VALUES*
;; is exactly the set of such symbols.

(let (defmvars-list (save-$props (copy-list $props)))
  (maphash #'(lambda (k v) (push k defmvars-list)) *variable-initial-values* )
  (declare1 defmvars-list t '$special 'kind)
  (setq $props save-$props))

(let ((prev-mfexpr* (get '$define_variable 'mfexpr*)))
  (setf (get '$define_variable 'mfexpr*)
        #'(lambda (e)
            (let ((var (second e)))
              (declare1 (list var) t '$special 'kind)
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

(defun subst-lexical-symbols-into-mprog (mprog-op vars+var-inits exprs)
  (let*
   ((vars+var-inits-lexical (remove-if #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$special)) vars+var-inits))
    (vars+var-inits-special (remove-if-not #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$special)) vars+var-inits))
    (vars-only-lexical (remove-if-not #'symbolp vars+var-inits-lexical))
    (var-inits-lexical (remove-if #'symbolp vars+var-inits-lexical))
    (var-inits-vars-lexical (mapcar #'second var-inits-lexical))
    (vars-all-lexical (append vars-only-lexical var-inits-vars-lexical))
    (vars-only-gensyms (mapcar #'(lambda (s) (let ((s1 (gensym))) (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) vars-only-lexical))
    (var-inits-gensyms (mapcar #'(lambda (s) (let ((s1 (gensym))) (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) var-inits-vars-lexical))
    (gensyms-all (append vars-only-gensyms var-inits-gensyms))
    (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) vars-all-lexical gensyms-all))
    (gensym-mprogn (let (($simp nil)) ($substitute `((mlist) ,@ subst-eqns) `((mprogn) ,@ exprs))))
    (gensym-inits (mapcar #'(lambda (e y) (list (first e) y (third e))) var-inits-lexical var-inits-gensyms))
    (gensym-mprog `(,mprog-op ((mlist) ,@ (append vars-only-gensyms gensym-inits vars+var-inits-special)) ,@ (cdr gensym-mprogn))))
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
    ((args (remove-if #'(lambda (x) (kindp x '$special)) (extract-arguments-symbols e)))
     (args-gensyms (mapcar
                     #'(lambda (s)
                         (let ((s1 (gensym)))
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

(let ((parse-$do-prev #'parse-$do))
  (defun parse-$do (&rest a)
    (let*
      ((do-expr (apply parse-$do-prev a))
       (var (third do-expr))
       (var-subst (gensym))
       (next (sixth do-expr))
       (unless (eighth do-expr))
       (body (ninth do-expr)))
      (setf (get var-subst 'reversealias) (or (get var 'reversealias) var))
      (setf (third do-expr) var-subst)
      (let (($simp nil))
        (setf (sixth do-expr) (maxima-substitute var-subst var next))
        (setf (eighth do-expr) (maxima-substitute var-subst var unless))
        (setf (ninth do-expr) (maxima-substitute var-subst var body)))
      do-expr)))

(setf (get '$for 'nud) #'parse-$do)

