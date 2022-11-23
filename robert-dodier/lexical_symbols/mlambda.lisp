;; MLAMBDA COPIED VERBATIM FROM SRC/MLISP.LISP
;; WITH CHANGES FOR LEXICAL SYMBOLS AS NOTED

(defun mlambda (lambda-or-closure args fnname noeval form)
  ; We assume that the lambda expression handed to us has been simplified,
  ; or at least that it's well-formed.  This is because various checks are
  ; performed during simplification instead of every time lambda expressions
  ; are applied to arguments.
  (setq noevalargs nil)
  (let* ((closure-envs (if (eq (caar lambda-or-closure) '$closure) (rest (second lambda-or-closure))))
         (fn (if (eq (caar lambda-or-closure) '$closure) (third lambda-or-closure) lambda-or-closure))
         (params (cdadr fn))
         (mlocp t))
    (setq loclist (cons nil loclist))
    (do ((a) (p))
        ((or (null params) (and (null args) (not (mdeflistp params))))
         (setq args (nreconc a args) params (nreconc p params)))
      (cond ((mdeflistp params)
             (setq params (cdar params) args (ncons (cons '(mlist) args)))))
      (cond ((and mfexprp (mquotep (car params)))
             (setq a (cons (car args) a) p (cons (cadar params) p)))
            ((atom (car params))
             (setq p (cons (car params) p)
                   a (cons (cond (noeval (car args))
                                 (t (meval (car args)))) a)))
            (t (merror (intl:gettext "lambda: formal argument must be a symbol or quoted symbol; found: ~M") (car params))))
      (setq args (cdr args) params (cdr params)))
    (let (finish2033 #+nil (finish2032 params) (ar *mlambda-call-stack*))
      (declare (type (vector t) ar))
      (unwind-protect
           (progn
             (unless (> (array-total-size ar) (+ (fill-pointer ar) 10))
               (setq ar (adjust-array ar (+ (array-total-size ar) 50) :fill-pointer (fill-pointer ar))))
             (vector-push bindlist ar)
             ;; rather than pushing all on *baktrcl* it might be good
             ;; to make a *last-form* global that is set in meval1
             ;; and is pushed here.
             (vector-push form ar)
             (vector-push params ar)
             (vector-push args ar)
             (vector-push fnname ar)
             #+nil (mbind finish2032 args fnname)
             (setq finish2033 t)
             (let ((aexprp (and aexprp (not (atom (caddr fn))) (eq (caar (caddr fn)) 'lambda))))
               (let
                 ((new-env (make-hash-table))
                  (new-env-id (gensym "ENV")))
                 ;; EXCLUDE NON-LEXICAL VARIABLES HERE ?? I DUNNO !!
                 ;; DON'T BOTHER WITH NEW-ENV IF PARAMS IS EMPTY ?? MAYBE !!
                 (mapcar #'(lambda (s v) (setf (gethash s new-env) v)) params args)
                 (setf (get new-env-id 'env) new-env)
                 (with-lexical-environment (cons new-env-id closure-envs)
                   (cond ((null (cddr fn)) (merror (intl:gettext "lambda: no body present.")))
                         ((cdddr fn) (mevaln (cddr fn)))
                         (t (meval (caddr fn))))))))
        (if finish2033
            (progn
              (incf (fill-pointer *mlambda-call-stack*) -5)
              (munlocal)
              #+nil (munbind finish2032)))))))
