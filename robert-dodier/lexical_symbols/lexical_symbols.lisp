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

;; Lexicalize MPROG (i.e., block([a, b, c, ...], ...))

(defun subst-lexical-symbols-into-mprog (e)
 (let*
   ((mprog-op (car e))
    (mprog-args (cdr e))
    (vars+var-inits (cdr (car mprog-args)))
    (vars+var-inits-lexical (remove-if #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$special)) vars+var-inits))
    (vars+var-inits-special (remove-if-not #'(lambda (x) (kindp (if (symbolp x) x (second x)) '$special)) vars+var-inits))
    (vars-only-lexical (remove-if-not #'symbolp vars+var-inits-lexical))
    (var-inits-lexical (remove-if #'symbolp vars+var-inits-lexical))
    (var-inits-vars-lexical (mapcar #'second var-inits-lexical))
    (vars-all-lexical (append vars-only-lexical var-inits-vars-lexical))
    (exprs (cdr mprog-args))
    (vars-only-gensyms (mapcar #'(lambda (s) (let ((s1 (gensym))) (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) vars-only-lexical))
    (var-inits-gensyms (mapcar #'(lambda (s) (let ((s1 (gensym))) (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) var-inits-vars-lexical))
    (gensyms-all (append vars-only-gensyms var-inits-gensyms))
    (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) vars-all-lexical gensyms-all))
    (gensym-mprogn ($psubstitute `((mlist) ,@ subst-eqns) `((mprogn) ,@ exprs)))
    (gensym-inits (mapcar #'(lambda (e y) (list (first e) y (third e))) var-inits-lexical var-inits-gensyms))
    (gensym-mprog `(,mprog-op ((mlist) ,@ (append vars-only-gensyms gensym-inits vars+var-inits-special)) ,@ (cdr gensym-mprogn))))
   gensym-mprog))

(defvar right-paren-symbol '|$)|)

(def-nud (mprog) (op)
  (pop-c) ;; eat the opening parenthesis
  (let
    ((right (prsmatch right-paren-symbol '$any))
     (header (mheader 'mprog)))
    (cons '$any (subst-lexical-symbols-into-mprog (cons header right)))))

;; Lexicalize MDEFINE (i.e. f(a, b, c, ...) := ...)

(defun subst-lexical-symbols-into-mdefine-or-lambda (e)
  (let*
    ((args (remove-if #'(lambda (x) (kindp x '$special)) (rest ($listofvars (second e)))))
     (args-gensyms (mapcar
                     #'(lambda (s)
                         (let ((s1 (gensym)))
                           (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) args))
     (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) args args-gensyms)))
    ($psubstitute `((mlist) ,@ subst-eqns) e)))

(def-led (|$:=| 180. 20.) (op left)
  (let ((e (parse-infix op left)))
    (cons (first e) (subst-lexical-symbols-into-mdefine-or-lambda (rest e)))))

;; Lexicalize LAMBDA (i.e., lambda([a, b, c, ...], ...))

(def-nud (lambda) (op)
  (pop-c) ;; eat the opening parenthesis
  (let
    ((right (prsmatch right-paren-symbol '$any))
     (header (mheader 'lambda)))
    (cons '$any (subst-lexical-symbols-into-mdefine-or-lambda (cons header right)))))