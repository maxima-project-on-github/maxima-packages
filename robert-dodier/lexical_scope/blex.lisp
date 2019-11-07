;; blex.lisp -- lexical block for Maxima
;; copyright 2012, 2019 by Robert Dodier
;; I release this work under terms of the GNU GPL
;;
;; examples:
;; blex ([n:100], f(x) := n : n + x, g() := display(n));
;; blex ([f], f(x) := 2*x);
;; blex ([a], h(x) := a[x] : 1, i() := arrayinfo(a));

(defmspec $blex (x)
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

(let ((default-mdefine-mfexpr* (get 'mdefine 'mfexpr*)))
  (setf (get 'mdefine 'mfexpr*)
        #'(lambda (e)
            ;; (format t "HEY E = ~S~%" e)
            (let*
              ((args (rest ($listofvars (second e))))
               (args-gensyms (mapcar
                               #'(lambda (s)
                                   (let ((s1 (gensym)))
                                     (setf (get s1 'reversealias) (or (get s 'reversealias) s)) s1)) args))
               (subst-eqns (mapcar #'(lambda (x y) `((mequal) ,x ,y)) args args-gensyms))
               (gensym-expr ($psubstitute `((mlist) ,@ subst-eqns) e)))
              ;; (format t "HEY NOW CALL DEFAULT MDEFINE MFEXPR* ON ~S~%" gensym-expr)
              (funcall default-mdefine-mfexpr* gensym-expr)))))
