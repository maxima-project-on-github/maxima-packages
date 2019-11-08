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
(mfuncall '$declare '$special '$feature)

(defmspec $with_lexical_symbols (x)
 (let*
   ((args (cdr x))
    (vars+var-inits (cdr (car args)))
    (vars-only (remove-if-not #'symbolp vars+var-inits))
    (var-inits (remove-if #'symbolp vars+var-inits))
    (var-inits-vars (mapcar #'second var-inits))
    (vars-all (append vars-only var-inits-vars))
    (exprs (cdr args))
    (vars-only-gensyms (mapcar #'(lambda (s) (let ((s1 (gensym))) (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) vars-only))
    (var-inits-gensyms (mapcar #'(lambda (s) (let ((s1 (gensym))) (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) var-inits-vars))
    (gensyms-all (append vars-only-gensyms var-inits-gensyms))
    (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) vars-all gensyms-all))
    (gensym-mprogn ($psubstitute `((mlist) ,@ subst-eqns) `((mprogn) ,@ exprs)))
    (gensym-inits (mapcar #'(lambda (e y) (list (first e) y (third e))) var-inits var-inits-gensyms))
    (gensym-mprog `((mprog) ((mlist) ,@ (append vars-only-gensyms gensym-inits)) ,@ (cdr gensym-mprogn))))
   (meval gensym-mprog)))

;; Redefine function definition operator ":=" so that gensyms are substituted for function arguments.
;; This is a simple-minded experiment to investigate lexical scope for function arguments.
;; Any symbols declared special are excluded from gensym substitution.

(let ((default-mdefine-mfexpr* (get 'mdefine 'mfexpr*)))
  (setf (get 'mdefine 'mfexpr*)
        #'(lambda (e)
            ;; (format t "HEY E = ~S~%" e)
            (let*
              ((args (remove-if #'(lambda (x) (kindp x '$special)) (rest ($listofvars (second e)))))
               (args-gensyms (mapcar
                               #'(lambda (s)
                                   (let ((s1 (gensym)))
                                     (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) args))
               (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) args args-gensyms))
               (gensym-expr ($psubstitute `((mlist) ,@ subst-eqns) e)))
              ;; (format t "HEY NOW CALL DEFAULT MDEFINE MFEXPR* ON ~S~%" gensym-expr)
              (funcall default-mdefine-mfexpr* gensym-expr)))))
