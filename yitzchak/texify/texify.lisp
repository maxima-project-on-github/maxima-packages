(in-package :maxima)

(defvar $texify_styles '((mlist simp) $latex $tex_prefix_functions $tex))
(defparameter texify-styles (make-hash-table :test 'equal))

#|
  Modes

  a - Array row
  m - Display math mode
  i - Inline math mode
  s - Superscript/Subscript
  t - mtext mode, strings are placed text boxes.
  u - ezunits
  n - siunitx number
|#
(defparameter *texify-modes* '(#\m))

(defparameter *texify-op* 'mparen)
(defparameter *texify-lop* 'mparen)
(defparameter *texify-rop* 'mparen)
(defparameter *texify-styles* nil)

(defparameter +texify-prefix-functions+ '(%acos
                                          %asin
                                          %atan
                                          %cos
                                          %cosh
                                          %cot
                                          %coth
                                          %csc
                                          %log
                                          %sin
                                          %sinh
                                          %tan
                                          %tanh))

(defmacro texify-bp (sym left &optional right)
  `(setf (get ',sym 'texify-lbp) ',left)
  `(setf (get ',sym 'texify-rbp) ',(or right left)))

(defun texify-lbp (x)
  (or (get x 'texify-lbp) (lbp x)))

(defun texify-rbp (x)
  (or (get x 'texify-rbp) (rbp x)))

(defun texify-mminusp (expr)
  (and expr
       (listp expr)
       (listp (car expr))
       (eql 'mminus (caar expr))))

(defun texify-mlabelp (expr)
  (and expr
       (listp expr)
       (listp (car expr))
       (eql 'mlabel (caar expr))))

(defclass texify-style ()
  ((functions
     :initarg :functions
     :initform (make-hash-table :test 'equal)
     :reader texify-style-functions)
   (function-default
     :initarg :function-default
     :initform nil
     :accessor texify-style-function-default)
   (normalizers
     :initarg :normalizers
     :initform (make-hash-table :test 'equal)
     :reader texify-style-normalizers)
   (string-default
     :initarg :string-default
     :initform nil
     :accessor texify-style-string-default)
   (symbols
     :initarg :symbols
     :initform (make-hash-table :test 'equal)
     :reader texify-style-symbols)
   (symbol-default
     :initarg :symbol-default
     :initform nil
     :accessor texify-style-symbol-default)))

(defun initialize-hash (table source)
  (loop for keyval on source by #'cddr
        do (if (listp (first keyval))
               (loop for key in (first keyval)
                     do (setf (gethash key table) (second keyval)))
               (setf (gethash (first keyval) table) (second keyval)))))

(defun make-texify-style (name &key (functions nil)
                                    (function-default nil)
                                    (normalizers nil)
                                    (symbols nil)
                                    (string-default nil)
                                    (symbol-default nil))
  (let ((style (make-instance 'texify-style :function-default function-default
                                            :string-default string-default
                                            :symbol-default symbol-default)))
    (when functions
      (initialize-hash (texify-style-functions style) functions))
    (when normalizers
      (initialize-hash (texify-style-normalizers style) normalizers))
    (when symbols
      (initialize-hash (texify-style-symbols style) symbols))
    (setf (gethash name texify-styles) style)))

(defun texify-mode-get (values modes)
  (some (lambda (mode) (cdr (assoc mode values)))
        modes))

; Turn floats into texify-float, use mminus unary for numbers and look for
; symbols that probably should be strings.
(defun texify-normalize-args (expr)
  (mapcar (lambda (x)
            (cond
              ((floatp x)
                (let* ((rep (format nil "~A" x))
                       (pos (position #\e rep)))
                  (if pos
                    `((texify-float) ,(subseq rep 0 pos) ,(subseq rep (1+ pos)))
                    x)))
              ((and (numberp x) (minusp x))
                `((mminus) ,(- x)))
              ((and (symbolp x)
                    (find #\Space (string x))
                    (not (equal '| --> | x)))
                (maybe-invert-string-case (string x)))
              (t
                x)))
          expr))

; Wrap with parenthesis if needed. Otherwise call nformat then
; texify-normalize-function.
(defun texify-normalize (expr modes lop rop)
  (if (and (listp expr)
           (not (equalp (caar expr) 'mparen))
           (or (< (texify-lbp (caar expr)) (texify-rbp lop))
               (> (texify-lbp rop) (texify-rbp (caar expr)))))
    `((mparen simp) ,expr)
    (let* ((n (nformat expr))
           (m (if (and (listp n) (find 'array (car n)))
                `((array simp) ,(caar n) ,@(cdr n))
                n)))
      (if (listp m)
        (or
          (dolist (style *texify-styles*)
            (let ((normalizer (texify-mode-get (gethash (caar m) (texify-style-normalizers style)) modes)))
              (when normalizer
                (let ((result (funcall normalizer m modes lop rop)))
                  (when result
                    (return result))))))
          m)
        m))))

(defun texify-quote (str &key (text nil))
  (apply #'concatenate 'string
         (map 'list (lambda (ch)
                      (case ch
                        (#\& "\\&")
                        (#\% "\\%")
                        (#\$ "\\$")
                        (#\# "\\#")
                        (#\_ "\\_")
                        (#\{ "\\{")
                        (#\} "\\}")
                        (#\\ "\\textbackslash ")
                        (#\~ "\\textasciitilde ")
                        (#\^ "\\textasciicircum ")
                        (#\< (if text "\\textless " (string ch)))
                        (#\> (if text "\\textgreater " (string ch)))
                        (otherwise (string ch))))
                    (string str))))

(defgeneric apply-style (expr style modes lop rop))
(defgeneric apply-default-style (expr style modes lop rop))

(defun texify (expr &optional (modes '(#\m)) (lop 'mparen) (rop 'mparen))
  (let ((nexpr (texify-normalize expr modes lop rop)))
    (or
      (dolist (style *texify-styles*)
        (let ((value (apply-style nexpr style modes lop rop)))
          (when value
            (return value))))
      (dolist (style *texify-styles*)
        (let ((value (apply-default-style nexpr style modes lop rop)))
          (when value
            (return value)))))))

(defun do-texify (expr modes styles)
  (let ((*texify-styles* (mapcar (lambda (x) (gethash x texify-styles))
                                 (append styles (cdr $texify_styles)))))
    (texify (if (texify-mlabelp expr)
              expr
              `((mlabel simp) nil ,expr))
            modes)))

(defun $texify (expr &rest styles)
  (do-texify expr '(#\m) styles))

(defun $texify_inline (expr &rest styles)
  (do-texify expr '(#\i #\m) styles))

(defun $texify_available_styles ()
  (cons '(mlist)
        (loop for key being the hash-keys of texify-styles collect key)))

(defun $texify_add_styles (&rest styles)
  (setq $texify_styles (append '((mlist))
                               styles
                               (cdr $texify_styles))))

(defun $texify_remove_styles (&rest styles)
  (setq $texify_styles (remove-if (lambda (x) (find x styles)) $texify_styles)))

(defmethod apply-style (expr style modes lop rop))

(defmethod apply-default-style (expr style modes lop rop)
  (texify-quote (format nil "~A" expr)))

(defun texify-parse-symbol (sym)
  (let* ((nn-list (extract-trailing-digits (symbol-name sym)))
         (name (maybe-invert-string-case (texify-quote (stripdollar (if nn-list (first nn-list) sym))))))
    (list (length name) name (cdr nn-list))))

(defmethod apply-style ((expr symbol) style modes lop rop)
  (texify-mode-get (gethash expr (texify-style-symbols style)) modes))

(defmethod apply-default-style ((expr symbol) style modes lop rop)
  (let ((symbol-default (texify-mode-get (texify-style-symbol-default style) modes)))
    (when symbol-default
      (apply #'format nil symbol-default (texify-parse-symbol expr)))))

(defmethod apply-default-style ((expr string) style modes lop rop)
  (let ((string-default (texify-mode-get (texify-style-string-default style) modes)))
    (when string-default
      (format nil string-default (texify-quote expr :text t) (texify-quote (with-output-to-string (f) (mgrind expr f)) :text t)))))

(defun texify-function (control expr style modes lop rop)
  (declare (ignore style))
  (when control
    (let ((*texify-modes* modes)
          (*texify-op* (caar expr))
          (*texify-lop* lop)
          (*texify-rop* rop))
      (apply #'format nil control
                      (caar expr)
                      (cdr expr)))))

(defmethod apply-style ((expr list) style modes lop rop)
  (texify-function (texify-mode-get (gethash (caar expr) (texify-style-functions style)) modes)
                   expr style modes lop rop))

(defmethod apply-default-style ((expr list) style modes lop rop)
  (texify-function (texify-mode-get (texify-style-function-default style) modes)
                   expr style modes lop rop))

(defun get-texify-function (name modes)
  (dolist (style *texify-styles*)
    (let ((value (texify-mode-get (gethash name (texify-style-functions style)) modes)))
      (when value
        (return value)))))

(defun get-texify-function-default (modes)
  (dolist (style *texify-styles*)
    (let ((value (texify-mode-get (texify-style-function-default style) modes)))
      (when value
        (return value)))))

#|

Format control functions

|#

(defun texify-format (output expr prefix postfix paren mode)
  (write-string (texify expr
                        (if mode
                          (cons mode *texify-modes*)
                          *texify-modes*)
                        (cond
                          (prefix *texify-op*)
                          (paren 'mparen)
                          (t *texify-lop*))
                        (cond
                          (postfix *texify-op*)
                          (paren 'mparen)
                          (t *texify-rop*)))
                output))

; Operand with the main operator in both a prefix and postfix location.
(defun cl-user::perifix (output expr colon at &optional mode)
  (declare (ignore at))
  (texify-format output expr t t colon mode))

; Operand with the main operator in a prefix location.
(defun cl-user::prefix (output expr colon at &optional mode)
  (declare (ignore at))
  (texify-format output expr t nil colon mode))

; Operand with the main operator in a postfix location.
(defun cl-user::postfix (output expr colon at &optional mode)
  (declare (ignore at))
  (texify-format output expr nil t colon mode))

; Operand not surrounded by the main operator.
(defun cl-user::nullfix (output expr colon at &optional mode)
  (declare (ignore at))
  (texify-format output expr nil nil colon mode))

#|

texify specific binding power settings. Mostly here to account to for treating
the trig functions, sum, product, etc. as prefix operators.

|#

(texify-bp %acos 200. 130.)
(texify-bp %asin 200. 130.)
(texify-bp %atan 200. 130.)
(texify-bp %cos 200. 130.)
(texify-bp %cosh 200. 130.)
(texify-bp %cot 200. 130.)
(texify-bp %coth 200. 130.)
(texify-bp %csc 200. 130.)
(texify-bp %log 200. 130.)
(texify-bp %product 200. 115.)
(texify-bp %sin 200. 130.)
(texify-bp %sinh 200. 130.)
(texify-bp %sqrt 140. 115.)
(texify-bp %sum 200. 110.)
(texify-bp %tan 200. 130.)
(texify-bp %tanh 200. 130.)
(texify-bp texify-diff-euler 200. 125.)
(texify-bp texify-diff-leibniz 200. 125.)
(texify-bp texify-float 115.)

#|

Normalization Functions

|#

(defun tex-normalize-diff-euler (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  (let ((vars (loop for rest on (cddr expr) by #'cddr
                    for var = (first rest)
                    for deg = (second rest)
                    append (list var (unless (onep deg) deg)))))
    `((texify-diff-euler simp) ,(second expr) ,@vars)))

(defun tex-normalize-diff-lagrange (expr modes l-op r-op)
  (declare (ignore l-op r-op))
  (when (and (equalp (length expr) 4)
             (or (symbolp (second expr))
                 (and (listp (second expr))
                      (equalp 2 (length (second expr)))
                      (equalp (second (second expr)) (third expr))
                      (not (get-texify-function (caar (second expr)) modes)))))
    (let* ((base (if (listp (second expr)) (caar (second expr)) (second expr)))
           (order (fourth expr)))
      `((texify-diff-lagrange simp) ,base
                                    ,(when (numberp order) order)
                                    ,(unless (numberp order) order)
                                    ,@(when (listp (second expr)) (cdr (second expr)))))))

(defun tex-normalize-diff-leibniz (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  (let ((deg (simplifya `((mplus) ,@(loop for rest on (cddr expr) by #'cddr
                                          collect (second rest)))
                        nil))
        (op (second expr))
        (vars (loop for rest on (cddr expr) by #'cddr
                    for var = (first rest)
                    for deg = (second rest)
                    append (list var (unless (onep deg) deg)))))
    `((texify-diff-leibniz simp) ,(unless (symbolp op) op) ,(unless (onep deg) deg) ,(when (symbolp op) op) ,@vars)))

(defun tex-normalize-diff-newton (expr modes l-op r-op)
  (declare (ignore l-op r-op))
  (when (and (equalp (length expr) 4)
             (equalp (third expr) '$t)
             (or (symbolp (second expr))
                 (and (listp (second expr))
                      (equalp 2 (length (second expr)))
                      (equalp (second (second expr)) (third expr))
                      (not (get-texify-function (caar (second expr)) modes)))))
    (let* ((base (if (listp (second expr)) (caar (second expr)) (second expr)))
           (order (fourth expr)))
      `((texify-diff-newton simp) ,base
                                  ,(when (numberp order) order)
                                  ,(unless (numberp order) order)
                                  ,@(when (listp (second expr)) (cdr (second expr)))))))

; Make the limit direction an index so we can use a format conditional.
(defun tex-normalize-limit (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  `(,(first expr)
    ,(second expr)
    ,(third expr)
    ,(fourth expr)
    ,(case (fifth expr)
      ($plus 1)
      ($minus 0)
      (otherwise 2))))

; Remove empty else clauses and set the test of else classes to nil to make
; using a format conditional possible.
(defun tex-normalize-mcond (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  (cons (first expr)
    (loop for test-body on (cdr expr) by #'cddr
          when (not (equalp '$false (second test-body)))
          append (list (if (equalp t (first test-body)) nil (first test-body)) (second test-body)))))

; Look for roots and push the exponent into the function arguments for prefix
; functions.
(defun tex-normalize-mexpt (expr modes l-op r-op)
  (declare (ignore l-op r-op))
  (let* ((n `(,(first expr) ,(second expr) ,(texify-normalize (third expr) modes 'mparen 'mparen)))
         (base (second n))
         (exponent (third n)))
    (cond
      ((and (listp exponent)
            (or (equalp 'rat (caar exponent))
                (equalp 'mquotient (caar exponent)))
            (= 1 (second exponent)))
        `((texify-root simp) ,(third exponent) ,base))
      ((and (listp base)
            (or (not (numberp exponent))
                (> exponent 0))
            (find (caar base) +texify-prefix-functions+))
        `(,(first base) ,exponent ,@(cdr base)))
      (t
        n))))

; Respect *display-labels-p*
(defun tex-normalize-mlabel (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  `(,(first expr) ,(when *display-labels-p* (second expr))  ,(third expr)))

(defun tex-no-label-normalize-mlabel (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  `(,(first expr) nil ,(third expr)))

(defun tex-eq-number-normalize-mlabel (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  `((texify-math simp) ,(third expr)))

; Look for mminus unary operators that are not in the first slot and turn them
; into n-ary mminus to avoid results like z+x+-y.
(defun tex-normalize-mplus (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  (loop with previous = nil
        for arg in (cdr expr)
        if (and previous (texify-mminusp arg)) do
          (if (and (texify-mminusp previous) (> (length previous) 2))
            (setq previous (append previous (cdr arg)))
            (setq previous `((mminus simp) ,previous ,@(cdr arg))))
        else
          when previous
            collect previous into args
          end
          and do
            (setq previous arg)
        finally
          (return (if previous
                    `((mplus simp) ,@args ,previous)
                    `((mplus simp) ,@args)))))

; Look for an argument with a low right binding power that can avoid parenthesis
; by moving it to the end of the product. This will move roots to the end.
(defun tex-normalize-mtimes (expr modes l-op r-op)
  (declare (ignore modes l-op r-op))
  (let ((mtimes-rbp (texify-rbp 'mtimes))
        (min-rbp (texify-lbp 'mtimes))
        (min-pos nil))
    (dotimes (pos (length expr))
      (let ((arg (nth pos expr)))
        (when (and (listp arg) (listp (car arg)))
          (let ((my-lbp (texify-lbp (caar arg)))
                (my-rbp (texify-rbp (caar arg))))
            (when (and (< my-rbp min-rbp) (<= mtimes-rbp my-lbp))
              (setq min-rbp my-rbp)
              (setq min-pos pos))))))
    (if min-pos
      (append (subseq expr 0 min-pos)
              (subseq expr (1+ min-pos))
              (subseq expr min-pos (1+ min-pos)))
      expr)))

(make-texify-style '$tex :functions '((%at $at) ((#\m . "~*\\left.~/postfix/\\right|_{~'s:/nullfix/}"))
                                      %binomial ((#\m . "~*{{~:/nullfix/}\\choose{~:/nullfix/}}"))
                                      texify-diff-euler ((#\m . "~2*~@{\\mathop{D}_~:/nullfix/~@[^{~:/nullfix/}~] ~}~1@*~/prefix/"))
                                      texify-diff-lagrange ((#\m . "~*~:/nullfix/~@[~[~;'~;''~;'''~:;^{\\left(~:*~:/nullfix/\\right)}~]~]~@[^{\\left(~:/nullfix/\\right)}~]~#[~:;\\left(~@{~:/nullfix/~^, ~}\\right)~]"))
                                      texify-diff-leibniz ((#\m . "~*~*{d~@[^{~:/nullfix/}~]~@[~:/nullfix/~]}\\over{~@{\\mathop{d~:/nullfix/}~@[^{~:/nullfix/}~]~}}~1@*~@[~/prefix/~]"))
                                      texify-diff-newton ((#\m . "~2*{~@[~[~;\\dot~;\\ddot~;\\buildrel{\\cdots}\\over~:;\\buildrel{\\scriptscriptstyle\\left(~:*~'s:/nullfix/\\right)}\\over~]~]~@[\\buildrel{\\scriptscriptstyle\\left(~'s:/nullfix/\\right)}\\over~]{~1@*~:/nullfix/}}~2*~#[~:;\\left(~@{~:/nullfix/~^, ~}\\right)~]"))
                                      %del ((#\m . "~*\\mathop{d~:/nullfix/}"))
                                      (%integrate $integrate) ((#\m . "~*\\int~2*~#[~:;_{~'s:/nullfix/}^{~'s:/nullfix/}~]~1@*~:/nullfix/\\mathop{d~:/nullfix/}"))
                                      %limit ((#\m . "~2*\\lim_{~:/nullfix/ \\rightarrow ~:/nullfix/~[^{-}~;^{+}~]} ~1@*~/prefix/"))
                                      (%product $product) ((#\m . "~*\\prod~*_{~'s:/nullfix/=~'s:/nullfix/}^{~'s:/nullfix/}~1@*~/prefix/"))
                                      %sqrt ((#\m . "~*\\sqrt{~:/nullfix/}"))
                                      |$`| ((#\m . "~*~:/nullfix/\\,~'u:/nullfix/"))
                                      $bern ((#\m . "~*B_{~'s:/nullfix/}"))
                                      $matrix ((#\m . "~*\\left[\\matrix{~@{~'a:/nullfix/~^\\cr~%  ~}}\\right]"))
                                      %sum ((#\m . "~*\\sum~*_{~'s:/nullfix/=~'s:/nullfix/}^{~'s:/nullfix/}~1@*~/prefix/"))
                                      array ((#\m . "~*{~:/nullfix/}_{~@{~'s:/nullfix/~^,~}}"))
                                      mabs ((#\m . "~*\\left|~:/nullfix/\\right|"))
                                      mand ((#\m . "~*~/postfix/~@{~#[~; \\land ~/prefix/~:; \\land ~/perifix/~]~}"))
                                      marrow ((#\m . "~*~/postfix/ \\rightarrow ~/prefix/"))
                                      mdefine ((#\m . "~*~/postfix/ := ~/prefix/"))
                                      mdefmacro ((#\m . "~*~/postfix/ ::= ~/prefix/"))
                                      (%mdo mdo) ((#\m . "~*\\mathop{\\bf for}\\;~:/nullfix/~@[\\;{\\bf from}\\;~:/nullfix/~]~@[{\\bf step}\]\\;~:/nullfix/~]~@[\\;{\\bf next}\\;~:/nullfix/~]~@[\\;{\\bf thru}\\;~:/nullfix/~]~@[\\;{\\bf unless}\\;~:/nullfix/~]\\;{\\bf do}\\;~/prefix/"))
                                      (%mdoin mdoin) ((#\m . "~*\\mathop{\\bf for}\\;~:/nullfix/\\;{\\bf in}\\;~:/nullfix/\\;~*~*~*~@[{\\bf unless}\\;~:/nullfix/\\;~]{\\bf do}\\;~/prefix/"))
                                      mequal ((#\m . "~*~/postfix/ = ~/prefix/"))
                                      mexpt ((#\m . "~*{~/postfix/}^{~'s:/nullfix/}"))
                                      mfactorial ((#\m . "~*~/postfix/!"))
                                      mgeqp ((#\m . "~*~/postfix/ \\geq ~/prefix/"))
                                      mgreaterp ((#\m . "~*~/postfix/ > ~/prefix/"))
                                      mlabel ((#\m . "$$~*~:[~;~:*$~:/nullfix/$\\;~] ~:/nullfix/$$")
                                              (#\i . "$~*~*~:/nullfix/$"))
                                      mleqp ((#\m . "~*~/postfix/ \\leq ~/prefix/"))
                                      mlessp ((#\m . "~*~/postfix/ < ~/prefix/"))
                                      mlist ((#\m . "~*\\left[~@{~:/nullfix/~^, ~}\\right]")
                                             (#\s . "~*~@{~:/nullfix/~^,~}")
                                             (#\a . "~*~@{~'m:/nullfix/~^ & ~}"))
                                      mminus ((#\m . "~*~#[~;-~/prefix/~:;~/postfix/~@{~#[~;-~/prefix/~:;-~/perifix/~]~}~]"))
                                      mnctimes ((#\m . "~*~/postfix/~@{~#[~;\\cdot ~/prefix/~:;\\cdot ~/perifix/~]~}"))
                                      mnot ((#\m . "~*\\neg ~/prefix/"))
                                      mnotequal ((#\m . "~*~/left \\neg ~/prefix/"))
                                      mor ((#\m . "~*~/postfix/~@{~#[~; \\lor ~/prefix/~:; \\lor ~/perifix/~]~}"))
                                      mparen ((#\m . "~*\\left(~:/nullfix/\\right)"))
                                      mplus ((#\m . "~*~/postfix/~@{~#[~;+~/prefix/~:;+~/perifix/~]~}"))
                                      ; mprog ((#\m . "\\eqalign{~A\\left(~@{&~:/nullfix/~^,\\cr~%  ~}\\right)}"))
                                      mquote ((#\m . "~*'~/prefix/"))
                                      (mquotient rat) ((#\m . "~*{{~:/nullfix/}\\over{~:/nullfix/}}")
                                                       (#\s . "~*~/postfix//~/prefix/")
                                                       (#\u . "~*~/postfix//~/prefix/"))
                                      mset ((#\m . "~*~/postfix/ :: ~/prefix/"))
                                      msetq ((#\m . "~*~/postfix/ : ~/prefix/"))
                                      mtext ((#\m . "~*~@{~'t:/nullfix/~}"))
                                      mtimes ((#\m . "~*~/postfix/~@{~#[~; ~/prefix/~:; ~/perifix/~]~}"))
                                      spaceout ((#\m . "~*\\hspace{~:/nullfix/mm}"))
                                      (mcond %mcond) ((#\m . "~*\\mathop{\\bf if}\\;~:/nullfix/\\;\\mathop{\\bf then}\\;~:/nullfix/~@{\\;~:[\\mathop{\\bf else}~;~:*\\mathop{\\bf elseif}\\;~:/nullfix/\\;\\mathop{\\bf then}~]\\;~:/nullfix/~}"))
                                      texify-math ((#\m . "$$~*~:/nullfix/\\eqnum$$")
                                                   (#\i . "$~*~:/nullfix/$"))
                                      texify-float ((#\m . "~*~A \\times 10^{~A}"))
                                      texify-root ((#\m . "~*\\sqrt[~:/nullfix/]{~:/nullfix/}")))
                         :function-default '((#\m . "~:/nullfix/\\left(~@{~:/nullfix/~^, ~}\\right)"))
                         :normalizers `(mexpt ((#\m . ,#'tex-normalize-mexpt))
                                        (mcond %mcond) ((#\m . ,#'tex-normalize-mcond))
                                        (%derivative $diff) ((#\m . ,#'tex-normalize-diff-leibniz))
                                        %limit ((#\m . ,#'tex-normalize-limit))
                                        mlabel ((#\m . ,#'tex-normalize-mlabel))
                                        mplus ((#\m . ,#'tex-normalize-mplus))
                                        mtimes ((#\m . ,#'tex-normalize-mtimes)))
                         :string-default '((#\m . "\\hbox{\\rm ~*~A}")
                                           (#\t . "\\hbox{\\rm ~A}"))
                         :symbol-default '((#\m . "~[~;{~A}~:[~;~:*_{~{~A~^, ~}}~]~:;\\mathop{{\\rm ~A}~:[~;~:*_{~{~A~^, ~}}~]}~]")
                                           (#\u . "~*\\mathop{{\\rm ~A}~:[~;~:*_{~@{~A~^, ~}}~]}"))
                         :symbols '(| --> | ((#\m . "\\longrightarrow"))
                                    %acos ((#\m . "\\arccos"))
                                    %asin ((#\m . "\\arcsin"))
                                    %atan ((#\m . "\\arctan"))
                                    %cos ((#\m . "\\cos"))
                                    %cosh ((#\m . "\\cosh"))
                                    %cot ((#\m . "\\cot"))
                                    %coth ((#\m . "\\coth"))
                                    %csc ((#\m . "\\csc"))
                                    %determinant ((#\m . "\\det"))
                                    %dim ((#\m . "\\dim")) ; Not used?
                                    %exp ((#\m . "\\exp"))
                                    %gamma ((#\m . "\\gamma"))
                                    %gamma_incomplete ((#\m . "\\Gamma"))
                                    %gamma_incomplete_generalized ((#\m . "\\Gamma"))
                                    %gamma_incomplete_regularized ((#\m . "Q"))
                                    %gcd ((#\m . "\\gcd")) ; Not used?
                                    %laplace ((#\m . "{\\rm L}"))
                                    %log ((#\m . "\\ln"))
                                    %max ((#\m . "\\max"))
                                    %min ((#\m . "\\min"))
                                    %sec ((#\m . "\\sec"))
                                    %sin ((#\m . "\\sin"))
                                    %sinh ((#\m . "\\sinh"))
                                    %tan ((#\m . "\\tan"))
                                    %tanh ((#\m . "\\tanh"))
                                    |$Alpha| ((#\m . "{\\rm A}"))
                                    |$Beta| ((#\m . "{\\rm B}"))
                                    |$Chi| ((#\m . "{\\rm X}"))
                                    |$Delta| ((#\m . "\\Delta"))
                                    |$Epsilon| ((#\m . "{\\rm E}"))
                                    |$Eta| ((#\m . "{\\rm H}"))
                                    |$Gamma| ((#\m . "\\Gamma"))
                                    |$Iota| ((#\m . "{\\rm I}"))
                                    |$Kappa| ((#\m . "{\\rm K}"))
                                    |$Lambda| ((#\m . "\\Lambda"))
                                    |$Mu| ((#\m . "{\\rm M}"))
                                    |$Nu| ((#\m . "{\\rm N}"))
                                    |$Omega| ((#\m . "\\Omega"))
                                    |$Omicron| ((#\m . "{\\rm O}"))
                                    |$Phi| ((#\m . "\\Phi"))
                                    |$Pi| ((#\m . "\\Pi"))
                                    |$Psi| ((#\m . "\\Psi"))
                                    |$Rho| ((#\m . "{\\rm P}"))
                                    |$Sigma| ((#\m . "\\Sigma"))
                                    |$Tau| ((#\m . "{\\rm T}"))
                                    |$Theta| ((#\m . "\\Theta"))
                                    |$Upsilon| ((#\m . "\\Upsilon"))
                                    |$Xi| ((#\m . "\\Xi"))
                                    |$Zeta| ((#\m . "{\\rm Z}"))
                                    $%e ((#\m . "e"))
                                    $%gamma ((#\m . "\\gamma"))
                                    $%i ((#\m . "i"))
                                    $%phi ((#\m . "\\varphi"))
                                    $%pi ((#\m . "\\pi"))
                                    $beta ((#\m . "\\beta"))
                                    $chi ((#\m . "\\chi"))
                                    $delta ((#\m . "\\delta"))
                                    $done ((#\m . "\\mathop{\\bf done}"))
                                    $epsilon ((#\m . "\\varepsilon"))
                                    $eta ((#\m . "\\eta"))
                                    ($false nil) ((#\m . "\\mathop{\\bf false}"))
                                    $gamma ((#\m . "\\gamma"))
                                    $gamma_incomplete_lower ((#\m . "\\gamma"))
                                    $inf ((#\m . "\\infty"))
                                    $iota ((#\m . "\\iota"))
                                    $kappa ((#\m . "\\kappa"))
                                    $lambda ((#\m . "\\lambda"))
                                    $minf ((#\m . "-\\infty"))
                                    $mu ((#\m . "\\mu"))
                                    $nu ((#\m . "\\nu"))
                                    $omega ((#\m . "\\omega"))
                                    $omicron ((#\m . "o"))
                                    $partial ((#\m . "\\partial"))
                                    $phi ((#\m . "\\phi"))
                                    $pi ((#\m . "\\pi"))
                                    $psi ((#\m . "\\psi"))
                                    $rho ((#\m . "\\rho"))
                                    $sigma ((#\m . "\\sigma"))
                                    $tau ((#\m . "\\tau"))
                                    $theta ((#\m . "\\vartheta"))
                                    ($true t)  ((#\m . "\\mathop{\\bf true}"))
                                    $upsilon ((#\m . "\\upsilon"))
                                    $xi ((#\m . "\\xi"))
                                    $zeta ((#\m . "\\zeta"))
                                    lambda ((#\m . "\\lambda"))
                                    mprog ((#\m . "\\mathop{\\bf block}"))
                                    ;; Ampere
                                    ($ampere $amp) ((#\u . "\\mathop{\\rm A}"))
                                    (|$microA| $microampere) ((#\u . "\\mathop{\\micro{\\rm A}}"))
                                    ;; AU
                                    $astronomical_unit ((#\u . "\\mathop{\\rm AU}"))
                                    ;; becquerel
                                    $becquerel ((#\u . "\\mathop{\\rm Bq}"))
                                    ;; candela
                                    $candela ((#\u . "\\mathop{\\rm cd}"))
                                    ;; Coulomb
                                    $coulomb ((#\u . "\\mathop{\\rm C}"))
                                    (|$microC| $microcoulomb) ((#\u . "\\mathop{\\micro{\\rm C}}"))
                                    ;; day
                                    $day ((#\u . "\\mathop{\\rm d}"))
                                    ;; degree
                                    $degree ((#\u . "^{\\circ}"))
                                    ;; Farad
                                    $farad ((#\u . "\\mathop{\\rm F}"))
                                    (|$microF| $microfarad) ((#\u . "\\mathop{\\micro{\\rm F}}"))
                                    ;; feet
                                    $feet ((#\u . "\\mathop{\\rm ft}"))
                                    ;; Gram
                                    $kilogram ((#\u . "\\mathop{\\rm kg}"))
                                    ($microg $microgram) ((#\u . "\\mathop{\\micro{\\rm g}}"))
                                    ;; Gray
                                    $gray ((#\u . "\\mathop{\\rm Gy}"))
                                    ;; Hectare
                                    $hectare ((#\u . "\\mathop{\\rm Ha}"))
                                    ;; Henry
                                    $henry ((#\u . "\\mathop{\\rm H}"))
                                    (|$microH| $microhenry) ((#\u . "\\mathop{\\micro{\\rm Hs}}"))
                                    ;; Hertz
                                    $hertz ((#\u . "\\mathop{\\rm Hz}"))
                                    (|$microHz| $microhertz) ((#\u . "\\mathop{\\micro{\\rm Hz}}"))
                                    ;; hour
                                    $hour ((#\u . "\\mathop{\\rm hr}"))
                                    ;; inch
                                    $inch ((#\u . "\\mathop{\\rm in}"))
                                    ;; Joule
                                    ($joule |$j|) ((#\u . "\\mathop{\\rm J}"))
                                    (|$microJ| $microjoule) ((#\u . "\\mathop{\\micro{\\rm J}}"))
                                    ;; Katal
                                    ($katal $kat) ((#\u . "\\mathop{\\rm kat}"))
                                    ;; Kelvin
                                    ('$kelvin |$k|) ((#\u . "\\mathop{\\rm K}"))
                                    (|$microK| $microkelvin) ((#\u . "\\mathop{\\micro{\\rm K}}"))
                                    ;; light year
                                    $light_year ((#\u . "\\mathop{\\rm lyr}"))
                                    ;; Liter
                                    ($liter $l |$l|) ((#\u . "\\mathop{\\rm L}"))
                                    ;; lumen
                                    $lumen ((#\u . "\\mathop{\\rm lm}"))
                                    ;; lux
                                    $lux ((#\u . "\\mathop{\\rm lx}"))
                                    ;; Meter
                                    $kilometer ((#\u . "\\mathop{\\rm km}"))
                                    $meter ((#\u . "\\mathop{\\rm m}"))
                                    ($micrometer $micron $microm) ((#\u . "\\mathop{\\micro{\\rm m}}"))
                                    ;; metric ton
                                    $metric_ton ((#\u . "\\mathop{\\rm t}"))
                                    ;; minute
                                    $minute ((#\u . "\\mathop{\\rm min}"))
                                    ;; Mole
                                    $mole ((#\u . "\\mathop{\\rm mol}"))
                                    ($micromole $micromol) ((#\u . "\\mathop{\\micro{\\rm mol}}"))
                                    ;; Newton
                                    $newton ((#\u . "\\mathop{\\rm N}"))
                                    (|$microN| $micronewton) ((#\u . "\\mathop{\\micro{\\rm N}}"))
                                    ;; Ohm
                                    |$GOhm| ((#\u . "\\mathop{{\\rm G}\\Omega}"))
                                    |$MOhm| ((#\u . "\\mathop{{\\rm M}\\Omega}"))
                                    |$kOhm| ((#\u . "\\mathop{{\\rm k}\\Omega}"))
                                    ($ohm |$Ohm|) ((#\u . "\\mathop{\\Omega}"))
                                    |$mOhm| ((#\u . "\\mathop{{\\rm m}\\Omega}"))
                                    $microOhm ((#\u . "\\mathop{\\micro\\Omega}"))
                                    |$pOhm| ((#\u . "\\mathop{{\\rm p}\\Omega}"))
                                    |$nOhm| ((#\u . "\\mathop{{\\rm n}\\Omega}"))
                                    |$fOhm| ((#\u . "\\mathop{{\\rm f}\\Omega}"))
                                    ;; Pascal
                                    $pascal ((#\u . "\\mathop{\\rm Pa}"))
                                    (|$microPa| $micropascal) ((#\u . "\\mathop{\\micro{\\rm Pa}}"))
                                    ;; parsec
                                    $parsec ((#\u . "\\mathop{\\rm pc}"))
                                    ;; Second
                                    $second ((#\u . "\\mathop{\\rm s}"))
                                    ($micros $microsecond) ((#\u . "\\mathop{\\micro{\\rm s}}"))
                                    ;; Siemens
                                    $siemens ((#\u . "\\mathop{\\rm S}"))
                                    (|$microS| $microsiemens) ((#\u . "\\mathop{\\micro{\\rm S}}"))
                                    ;; Tesla
                                    $tesla ((#\u . "\\mathop{\\rm T}"))
                                    (|$microT| $microtesla) ((#\u . "\\mathop{\\micro{\\rm T}}"))
                                    ;; Volt
                                    $volt ((#\u . "\\mathop{\\rm V}"))
                                    (|$microV| $microvolt) ((#\u . "\\mathop{\\micro{\\rm V}}"))
                                    ;; Watt
                                    $watt ((#\u . "\\mathop{\\rm W}"))
                                    (|$microW| $microwatt) ((#\u . "\\mathop{\\micro{\\rm W}}"))
                                    ;; Weber
                                    $weber ((#\u . "\\mathop{\\rm Wb}"))
                                    (|$microWb| $microweber) ((#\u . "\\mathop{\\micro{\\rm Wb}}"))
                                    ;; yard
                                    $yard ((#\u . "\\mathop{\\rm yd}"))
                                    ;; physical_constants
                                    $%c ((#\m . "c"))
                                    $%mu_0 ((#\m . "\\mu_0"))
                                    $%e_0 ((#\m . "\\varepsilon_0"))
                                    $%Z_0 ((#\m . "Z_0"))
                                    $%G ((#\m . "G"))
                                    $%h ((#\m . "h"))
                                    $%h_bar ((#\m . "\\hbar"))
                                    $%m_P ((#\m . "m_P"))
                                    $%%k ((#\m . "k"))
                                    $%T_P ((#\m . "T_P"))
                                    $%l_P ((#\m . "l_P"))
                                    $%t_P ((#\m . "t_P"))
                                    $%%e ((#\m . "e"))
                                    $%Phi_0 ((#\m . "\\Phi_0"))
                                    $%G_0 ((#\m . "G_0"))
                                    $%R_K ((#\m . "R_K"))
                                    $%mu_B ((#\m . "\\mu_B"))
                                    $%mu_N ((#\m . "\\mu_N"))
                                    $%alpha ((#\m . "\\alpha"))
                                    $%R_inf ((#\m . "R_{\\infty}"))
                                    $%a_0 ((#\m . "a_0"))
                                    $%E_h ((#\m . "E_{\\rm h}"))
                                    $%ratio_h_me ((#\m . "(h/m_{\\rm e})"))
                                    $%m_e ((#\m . "m_{\\rm e}"))
                                    $%N_A ((#\m . "N_A"))
                                    $%m_u ((#\m . "\\mu_{\\rm u}"))
                                    $%F ((#\m . "F"))
                                    $%R ((#\m . "R"))
                                    $%V_m ((#\m . "V_m"))
                                    $%n_0 ((#\m . "n_0"))
                                    $%ratio_S0_R ((#\m . "(S_0/R)"))
                                    $%sigma ((#\m . "\\sigma"))
                                    $%c_1 ((#\m . "c_1"))
                                    $%c_1L ((#\m . "c_{\\rm 1L}"))
                                    $%c_2 ((#\m . "c_2"))
                                    $%b ((#\m . "b"))
                                    $%b_prime ((#\m . "b'"))))

(make-texify-style '$tex_no_label :normalizers `(mlabel ((#\m . ,#'tex-no-label-normalize-mlabel))))

(make-texify-style '$tex_eq_number :normalizers `(mlabel ((#\m . ,#'tex-eq-number-normalize-mlabel))))

(let ((funs (texify-style-functions (gethash '$tex texify-styles))))
  (dolist (fun +texify-prefix-functions+)
    (setf (gethash fun funs) '((#\m . "~#[~;~;~:/nullfix/\\left(~:/nullfix/\\right)~;\\left(~:/nullfix/\\left(~*~:/nullfix/\\right)\\right)^{~:*~:*~'s:/nullfix/}~]")))))

(make-texify-style '$tex_pmatrix :functions '($matrix ((#\m . "~*\\pmatrix{~@{~'a:/nullfix/~^ & ~}~}~^\\cr~%  ~}}"))))

(make-texify-style '$tex_diff_lagrange :normalizers `((%derivative $diff) ((#\m . ,#'tex-normalize-diff-lagrange))))

(make-texify-style '$tex_diff_newton :normalizers `((%derivative $diff) ((#\m . ,#'tex-normalize-diff-newton))))

(make-texify-style '$tex_diff_euler :normalizers `((%derivative $diff) ((#\m . ,#'tex-normalize-diff-euler))))

(make-texify-style '$tex_prefix_functions
  :functions (mapcan (lambda (x) (list x '((#\m . "~:/nullfix/~#[~; ~/prefix/~;^{~'s:/nullfix/} ~/prefix/~]"))))
                     +texify-prefix-functions+))

(make-texify-style '$latex :functions '((mquotient rat) ((#\m . "~*\\frac{~:/nullfix/}{~:/nullfix/}")
                                                         (#\s . "~*~/postfix//~/prefix/")
                                                         (#\u . "~*~/postfix//~/prefix/"))
                                        texify-diff-leibniz ((#\m . "~*~*\\frac{d~@[^{~:/nullfix/}~]~@[~:/nullfix/~]}{~@{\\mathop{d~:/nullfix/}~@[^{~:/nullfix/}~]~}}~1@*~@[~/prefix/~]"))
                                        texify-diff-newton ((#\m . "~2*{~@[~[~;\\dot~;\\ddot~;\\overset{\\cdots}~:;\\overset{\\scriptscriptstyle\\left(~:*~'s:/nullfix/\\right)}~]~]~@[\\overset{\\scriptscriptstyle\\left(~'s:/nullfix/\\right)}~]{~1@*~:/nullfix/}}~2*~#[~:;\\left(~@{~:/nullfix/~^, ~}\\right)~]"))
                                        (%mdo mdo) ((#\m . "~*\\mathop{\\mathbf{for}}\\;~:/nullfix/~@[\\;{\\mathbf{from}}\\;~:/nullfix/~]~@[{\\mathbf{step}}\]\\;~:/nullfix/~]~@[\\;{\\mathbf{next}}\\;~:/nullfix/~]~@[\\;{\\mathbf{thru}}\\;~:/nullfix/~]~@[\\;{\\mathbf{unless}}\\;~:/nullfix/~]\\;{\\mathbf{do}}\\;~/prefix/"))
                                        (%mdoin mdoin) ((#\m . "~*\\mathop{\\mathbf{for}}\\;~:/nullfix/\\;{\\mathbf{in}}\\;~:/nullfix/\\;~*~*~*~@[{\\mathbf{unless}}\\;~:/nullfix/\\;~]{\\mathbf{do}}\\;~/prefix/"))
                                        (mcond %mcond) ((#\m . "~*\\mathop{\\mathbf{if}}\\;~:/nullfix/\\;\\mathop{\\mathbf{then}}\\;~:/nullfix/~@{\\;~:[\\mathop{\\mathbf{else}}~;~:*\\mathop{\\mathbf{elseif}}\\;~:/nullfix/\\;\\mathop{\\mathbf{then}}~]\\;~:/nullfix/~}"))
                                        mlabel ((#\m . "\\[~*~:[~;~:*\\(~:/nullfix/\\)\\;~] ~:/nullfix/\\]")
                                                (#\i . "\\(~*~*~:/nullfix/\\)"))
                                        texify-math ((#\m . "\\begin{equation}~%~*~:/nullfix/~%\\end{equation}")
                                                     (#\i . "\\(~*~:/nullfix/\\)")))
                           :string-default '((#\m . "\\mbox{~*~A}")
                                             (#\t . "\\mbox{~A}"))
                           :symbol-default '((#\m . "~[~;~A~:[~;~:*_{~{~A~^, ~}}~]~:;\\mathop{\\mathrm{~A}~:[~;~:*_{~{~A~^, ~}}~]}~]")
                                             (#\u . "~*\\mathop{\\mathrm{~A}~:[~;~:*_{~@{~A~^, ~}}~]}"))
                           :symbols '(%laplace ((#\m . "\\mathcal{L}"))
                                      |$Alpha| ((#\m . "\\mathrm{A}"))
                                      |$Beta| ((#\m . "\\mathrm{B}"))
                                      |$Chi| ((#\m . "\\mathrm{X}"))
                                      |$Epsilon| ((#\m . "\\mathrm{E}"))
                                      |$Eta| ((#\m . "\\mathrm{H}"))
                                      |$Iota| ((#\m . "\\mathrm{I}"))
                                      |$Kappa| ((#\m . "\\mathrm{K}"))
                                      |$Mu| ((#\m . "\\mathrm{M}"))
                                      |$Nu| ((#\m . "\\mathrm{N}"))
                                      |$Omicron| ((#\m . "\\mathrm{O}"))
                                      |$Rho| ((#\m . "\\mathrm{P}"))
                                      |$Tau| ((#\m . "\\mathrm{T}"))
                                      |$Zeta| ((#\m . "\\mathrm{Z}"))
                                      $done ((#\m . "\\mathop{\\mathbf{done}}"))
                                      ($false nil) ((#\m . "\\mathop{\\mathbf{false}}"))
                                      ($true t)  ((#\m . "\\mathop{\\mathbf{true}}"))
                                      mprog ((#\m . "\\mathop{\\mathbf{block}}"))
                                      ;; Ampere
                                      ($ampere $amp) ((#\u . "\\mathop{\\mathrm{A}}"))
                                      (|$microA| $microampere) ((#\u . "\\mathop{\\micro\\mathrm{A}}"))
                                      ;; AU
                                      $astronomical_unit ((#\u . "\\mathop{\\mathrm{AU}}"))
                                      ;; becquerel
                                      $becquerel ((#\u . "\\mathop{\\mathrm{Bq}}"))
                                      ;; candela
                                      $candela ((#\u . "\\mathop{\\mathrm{cd}}"))
                                      ;; Coulomb
                                      $coulomb ((#\u . "\\mathop{\\mathrm{C}}"))
                                      (|$microC| $microcoulomb) ((#\u . "\\mathop{\\micro\\mathrm{C}}"))
                                      ;; day
                                      $day ((#\u . "\\mathop{\\mathrm{d}}"))
                                      ;; Farad
                                      $farad ((#\u . "\\mathop{\\mathrm{F}}"))
                                      (|$microF| $microfarad) ((#\u . "\\mathop{\\micro\\mathrm{F}}"))
                                      ;; feet
                                      $feet ((#\u . "\\mathop{\\mathrm{ft}}"))
                                      ;; Gram
                                      $kilogram ((#\u . "\\mathop{\\mathrm{kg}}"))
                                      ($microg $microgram) ((#\u . "\\mathop{\\micro\\mathrm{g}}"))
                                      ;; Gray
                                      $gray ((#\u . "\\mathop{\\mathrm{Gy}}"))
                                      ;; Hectare
                                      $hectare ((#\u . "\\mathop{\\mathrm{Ha}}"))
                                      ;; Henry
                                      $henry ((#\u . "\\mathop{\\mathrm{H}}"))
                                      (|$microH| $microhenry) ((#\u . "\\mathop{\\micro\\mathrm{Hs}}"))
                                      ;; Hertz
                                      $hertz ((#\u . "\\mathop{\\mathrm{Hz}}"))
                                      (|$microHz| $microhertz) ((#\u . "\\mathop{\\micro\\mathrm{Hz}}"))
                                      ;; hour
                                      $hour ((#\u . "\\mathop{\\mathrm{hr}}"))
                                      ;; inch
                                      $inch ((#\u . "\\mathop{\\mathrm{in}}"))
                                      ;; Joule
                                      ($joule |$j|) ((#\u . "\\mathop{\\mathrm{J}}"))
                                      (|$microJ| $microjoule) ((#\u . "\\mathop{\\micro\\mathrm{J}}"))
                                      ;; Katal
                                      ($katal $kat) ((#\u . "\\mathop{\\mathrm{kat}}"))
                                      ;; Kelvin
                                      ('$kelvin |$k|) ((#\u . "\\mathop{\\mathrm{K}}"))
                                      (|$microK| $microkelvin) ((#\u . "\\mathop{\\micro\\mathrm{K}}"))
                                      ;; light year
                                      $light_year ((#\u . "\\mathop{\\mathrm{lyr}}"))
                                      ;; Liter
                                      ($liter $l |$l|) ((#\u . "\\mathop{\\mathrm{L}}"))
                                      ;; lumen
                                      $lumen ((#\u . "\\mathop{\\mathrm{lm}}"))
                                      ;; lux
                                      $lux ((#\u . "\\mathop{\\mathrm{lx}}"))
                                      ;; Meter
                                      $kilometer ((#\u . "\\mathop{\\mathrm{km}}"))
                                      $meter ((#\u . "\\mathop{\\mathrm{m}}"))
                                      ($micrometer $micron $microm) ((#\u . "\\mathop{\\micro\\mathrm{m}}"))
                                      ;; metric ton
                                      $metric_ton ((#\u . "\\mathop{\\mathrm{t}}"))
                                      ;; minute
                                      $minute ((#\u . "\\mathop{\\mathrm{min}}"))
                                      ;; Mole
                                      $mole ((#\u . "\\mathop{\\mathrm{mol}}"))
                                      ($micromole $micromol) ((#\u . "\\mathop{\\micro\\mathrm{mol}}"))
                                      ;; Newton
                                      $newton ((#\u . "\\mathop{\\mathrm{N}}"))
                                      (|$microN| $micronewton) ((#\u . "\\mathop{\\micro\\mathrm{N}}"))
                                      ;; Ohm
                                      |$GOhm| ((#\u . "\\mathop{{\\mathrm{G}}\\Omega}"))
                                      |$MOhm| ((#\u . "\\mathop{{\\mathrm{M}}\\Omega}"))
                                      |$kOhm| ((#\u . "\\mathop{{\\mathrm{k}}\\Omega}"))
                                      ($ohm |$Ohm|) ((#\u . "\\mathop{\\Omega}"))
                                      |$mOhm| ((#\u . "\\mathop{{\\mathrm{m}}\\Omega}"))
                                      $microOhm ((#\u . "\\mathop{\\micro\\Omega}"))
                                      |$pOhm| ((#\u . "\\mathop{{\\mathrm{p}}\\Omega}"))
                                      |$nOhm| ((#\u . "\\mathop{{\\mathrm{n}}\\Omega}"))
                                      |$fOhm| ((#\u . "\\mathop{{\\mathrm{f}}\\Omega}"))
                                      ;; Pascal
                                      $pascal ((#\u . "\\mathop{\\mathrm{Pa}}"))
                                      (|$microPa| $micropascal) ((#\u . "\\mathop{\\micro\\mathrm{Pa}}"))
                                      ;; parsec
                                      $parsec ((#\u . "\\mathop{\\mathrm{pc}}"))
                                      ;; Second
                                      $second ((#\u . "\\mathop{\\mathrm{s}}"))
                                      ($micros $microsecond) ((#\u . "\\mathop{\\micro\\mathrm{s}}"))
                                      ;; Siemens
                                      $siemens ((#\u . "\\mathop{\\mathrm{S}}"))
                                      (|$microS| $microsiemens) ((#\u . "\\mathop{\\micro\\mathrm{S}}"))
                                      ;; Tesla
                                      $tesla ((#\u . "\\mathop{\\mathrm{T}}"))
                                      (|$microT| $microtesla) ((#\u . "\\mathop{\\micro\\mathrm{T}}"))
                                      ;; Volt
                                      $volt ((#\u . "\\mathop{\\mathrm{V}}"))
                                      (|$microV| $microvolt) ((#\u . "\\mathop{\\micro\\mathrm{V}}"))
                                      ;; Watt
                                      $watt ((#\u . "\\mathop{\\mathrm{W}}"))
                                      (|$microW| $microwatt) ((#\u . "\\mathop{\\micro\\mathrm{W}}"))
                                      ;; Weber
                                      $weber ((#\u . "\\mathop{\\mathrm{Wb}}"))
                                      (|$microWb| $microweber) ((#\u . "\\mathop{\\micro\\mathrm{Wb}}"))
                                      ;; yard
                                      $yard ((#\u . "\\mathop{\\mathrm{yd}}"))
                                      ;; physical_constants
                                      $%E_h ((#\m . "E_{\\mathrm{h}}"))
                                      $%ratio_h_me ((#\m . "(h/m_{\\mathrm{e}})"))
                                      $%m_e ((#\m . "m_{\\mathrm{e}}"))
                                      $%m_u ((#\m . "\\mu_{\\mathrm{u}}"))
                                      $%c_1L ((#\m . "c_{\\mathrm{1L}}"))))

(make-texify-style '$amsmath :functions '((%at $at) ((#\m . "~*\\left.~/postfix/\\rvert_{~'s:/nullfix/}"))
                                          $binomial ((#\m . "~*\\binom{~:/nullfix/}{~/nullfix/}"))
                                          $matrix ((#\m . "~*\\begin{bmatrix}~%~@{  ~'a:/nullfix/~^\\\\~%~}~%\\end{bmatrix}"))
                                          texify-diff-newton ((#\m . "~2*{~@[~[~;\\dot~;\\ddot~;\\dddot~:;\\overset{\\scriptscriptstyle\\left(~:*~'s:/nullfix/\\right)}~]~]~@[\\overset{\\scriptscriptstyle\\left(~'s:/nullfix/\\right)}~]{~1@*~:/nullfix/}}~2*~#[~:;\\left(~@{~:/nullfix/~^, ~}\\right)~]"))
                                          mabs ((#\m . "~*\\lvert~:/nullfix/\\rvert"))
                                          mlabel ((#\m . "~*~:[\\begin{equation*}~%~:/nullfix/~%\\end{equation*}~;~:*\\begin{equation}~%\\tag{\\(~:/nullfix/\\)}~:/nullfix/~%\\end{equation}~]")
                                                  (#\i . "\\(~*~*~:/nullfix/\\)")))
                             :string-default '((#\m . "\\text{~*~A}")
                                               (#\t . "\\text{~A}")))

(make-texify-style '$amsmath_pmatrix :functions '($matrix ((#\m . "~*\\begin{pmatrix}~%~@{~{~*~@{~:/nullfix/~^ & ~}~}~^\\\\~%  ~}~%\\end{pmatrix}"))))

(make-texify-style '$breqn :functions '(mdefine ((#\m . "~*~/postfix/ \\hiderel{:=} ~/prefix/"))
                                        mdefmacro ((#\m . "~*~/postfix/ \\hiderel{::=} ~/prefix/"))
                                        mlabel ((#\m . "~*~:[\\begin{dmath*}~%~:/nullfix/~%\\end{dmath*}~;~:*\\begin{dmath}[number={\\(~:/nullfix/\\)}]~%~:/nullfix/~%\\end{dmath}~]")
                                                (#\i . "\\(~*~*~:/nullfix/\\)"))
                                        mset ((#\m . "~*~/postfix/ \\hiderel{::} ~/prefix/"))
                                        msetq ((#\m . "~*~/postfix/ \\hiderel{:} ~/prefix/"))
                                        texify-math ((#\m . "~*\\begin{dmath}~%~:/nullfix/~%\\end{dmath)")
                                                     (#\i . "\\(~*~:/nullfix/\\)"))))

(make-texify-style '$mathtools :functions '(mdefine ((#\m . "~*~/postfix/ \\coloneqq ~/prefix/"))
                                            mdefmacro ((#\m . "~*~/postfix/ \\Coloneqq ~/prefix/"))
                                            mset ((#\m . "~*~/postfix/ \\dblcolon ~/prefix/"))
                                            msetq ((#\m . "~*~/postfix/ \\vcentcolon ~/prefix/"))))

(make-texify-style '$nicefrac :functions '((mquotient rat) ((#\u . "~*\\nicefrac{~/postfix/}{~/prefix/}"))))

(make-texify-style '$siunitx
  :functions '(|$`| ((#\m . "~*\\SI{~'n:/nullfix/}{~'u:/nullfix/}"))
               mexpt ((#\u . "~*~:/nullfix/\\tothe{~:/nullfix/}"))
               mquotient ((#\u . "~*~/postfix//~/prefix/"))
               mtimes ((#\u . "~*~@{~:/nullfix/~^.~}"))
               texify-float ((#\n . "~*~Ae~A")))
  :symbols '(;; Ampere
             |$GA| ((#\u . "\\giga\\ampere"))
             |$MA| ((#\u . "\\mega\\ampere"))
             |$kA| ((#\u . "\\kilo\\ampere"))
             ($ampere $amp |$a|) ((#\u . "\\ampere"))
             |$mA| ((#\u . "\\milli\\ampere"))
             (|$microA| $microampere) ((#\u . "\\micro\\ampere"))
             |$nA| ((#\u . "\\nano\\ampere"))
             |$pA| ((#\u . "\\pico\\ampere"))
             |$fA| ((#\u . "\\femto\\ampere"))
             ;; AU
             ($astronomical_unit |$au|) ((#\u . "\\astronomicalunit"))
             ;; becquerel
             ($becquerel |$Bq|) ((#\u . "\\becquerel"))
             ;; candela
             $candela ((#\u . "\\candela"))
             ;; Coulomb
             |$GC| ((#\u . "\\giga\\coulomb"))
             |$MC| ((#\u . "\\mega\\coulomb"))
             |$kC| ((#\u . "\\kilo\\coulomb"))
             ($coulomb |$c|) ((#\u . "\\coulomb"))
             |$mC| ((#\u . "\\milli\\coulomb"))
             (|$microC| $microcoulomb) ((#\u . "\\micro\\coulomb"))
             |$pC| ((#\u . "\\pico\\coulomb"))
             |$nC| ((#\u . "\\nano\\coulomb"))
             |$fC| ((#\u . "\\femto\\coulomb"))
             ;; day
             $day ((#\u . "\\day"))
             ;; degree
             $degree ((#\u . "\\degree"))
             ;; Farad
             |$GF| ((#\u . "\\giga\\farad"))
             |$MF| ((#\u . "\\mega\\farad"))
             |$kF| ((#\u . "\\kilo\\farad"))
             ($farad |$f|) ((#\u . "\\farad"))
             |$mF| ((#\u . "\\milli\\farad"))
             (|$microF| $microfarad) ((#\u . "\\micro\\farad"))
             |$pF| ((#\u . "\\pico\\farad"))
             |$nF| ((#\u . "\\nano\\farad"))
             |$fF| ((#\u . "\\femto\\farad"))
             ;; feet
             $feet ((#\u . "ft"))
             ;; Gram
             |$Gg| ((#\u . "\\giga\\gram"))
             |$Mg| ((#\u . "\\mega\\gram"))
             ($kilogram $kg) ((#\u . "\\kilogram"))
             $g ((#\u . "\\gram"))
             $mg ((#\u . "\\milli\\gram"))
             ($microg $microgram) ((#\u . "\\micro\\gram"))
             $ng ((#\u . "\\nano\\gram"))
             $pg ((#\u . "\\pico\\gram"))
             $fg ((#\u . "\\femto\\gram"))
             ;; Gray
             ($gray |$Gy|) ((#\u . "\\gray"))
             ;; Hectare
             ($hectare $ha) ((#\u . "\\hectare"))
             ;; Henry
             |$GH| ((#\u . "\\giga\\henry"))
             |$MH| ((#\u . "\\mega\\henry"))
             |$kH| ((#\u . "\\kilo\\henry"))
             ($henry |$h|) ((#\u . "\\henry"))
             |$mH| ((#\u . "\\milli\\henry"))
             (|$microH| $microhenry) ((#\u . "\\micro\\henry"))
             |$nH| ((#\u . "\\nano\\henry"))
             |$pH| ((#\u . "\\pico\\henry"))
             |$fH| ((#\u . "\\femto\\henry"))
             ;; Hertz
             |$GHz| ((#\u . "\\giga\\hertz"))
             |$MHz| ((#\u . "\\mega\\hertz"))
             |$kHz| ((#\u . "\\kilo\\hertz"))
             ($hertz |$Hz|) ((#\u . "\\hertz"))
             |$mHz| ((#\u . "\\milli\\hertz"))
             (|$microHz| $microhertz) ((#\u . "\\micro\\hertz"))
             |$pHz| ((#\u . "\\pico\\hertz"))
             |$nHz| ((#\u . "\\nano\\hertz"))
             |$fHz| ((#\u . "\\femto\\hertz"))
             ;; hour
             $hour ((#\u . "\\hour"))
             ;; inch
             $inch ((#\u . "in"))
             ;; Joule
             |$GJ| ((#\u . "\\giga\\joule"))
             |$MJ| ((#\u . "\\mega\\joule"))
             |$kJ| ((#\u . "\\kilo\\joule"))
             ($joule |$j|) ((#\u . "\\joule"))
             |$mJ| ((#\u . "\\milli\\joule"))
             (|$microJ| $microjoule) ((#\u . "\\micro\\joule"))
             |$pJ| ((#\u . "\\pico\\joule"))
             |$nJ| ((#\u . "\\nano\\joule"))
             |$fJ| ((#\u . "\\femto\\joule"))
             ;; Katal
             ($katal $kat) ((#\u . "\\katal"))
             ;; Kelvin
             |$GK| ((#\u . "\\giga\\kelvin"))
             |$MK| ((#\u . "\\mega\\kelvin"))
             |$kK| ((#\u . "\\kilo\\kelvin"))
             ('$kelvin |$k|) ((#\u . "\\kelvin"))
             |$mK| ((#\u . "\\milli\\kelvin"))
             (|$microK| $microkelvin) ((#\u . "\\micro\\kelvin"))
             |$pK| ((#\u . "\\pico\\kelvin"))
             |$nK| ((#\u . "\\nano\\kelvin"))
             |$fK| ((#\u . "\\femto\\kelvin"))
             ;; light year
             $light_year ((#\u . "lyr"))
             ;; Liter
             ($liter $l |$l|) ((#\u . "\\liter"))
             $ml ((#\u . "\\milli\\liter"))
             ;; lumen
             $lumen ((#\u . "\\lumen"))
             ;; lux
             $lux ((#\u . "\\lux"))
             ;; Meter
             |$Gm| ((#\u . "\\giga\\meter"))
             |$Mm| ((#\u . "\\mega\\meter"))
             ($kilometer $km) ((#\u . "\\kilo\\meter"))
             ($meter $m) ((#\u . "\\meter"))
             $cm ((#\u . "\\centi\\meter"))
             $mm ((#\u . "\\milli\\meter"))
             ($micrometer $micron $microm) ((#\u . "\\micro\\meter"))
             $nm ((#\u . "\\nano\\meter"))
             $pm ((#\u . "\\pico\\meter"))
             $fm ((#\u . "\\femto\\meter"))
             ;; metric ton
             $metric_ton ((#\u . "\\tonne"))
             ;; minute
             $minute ((#\u . "\\minute"))
             ;; Mole
             |$Gmol| ((#\u . "\\giga\\mole"))
             |$Mmol| ((#\u . "\\mega\\mole"))
             $kmol ((#\u . "\\kilo\\mole"))
             ($mole $mol) ((#\u . "\\mole"))
             $mmol ((#\u . "\\milli\\mole"))
             ($micromole $micromol) ((#\u . "\\micro\\mole"))
             $nmol ((#\u . "\\nano\\mole"))
             $pmol ((#\u . "\\pico\\mole"))
             $fmol ((#\u . "\\femto\\mole"))
             ;; Newton
             |$GN| ((#\u . "\\giga\\newton"))
             |$MN| ((#\u . "\\mega\\newton"))
             |$kN| ((#\u . "\\kilo\\newton"))
             ($newton |$n|) ((#\u . "\\newton"))
             |$mN| ((#\u . "\\milli\\newton"))
             (|$microN| $micronewton) ((#\u . "\\micro\\newton"))
             |$pN| ((#\u . "\\pico\\newton"))
             |$nN| ((#\u . "\\nano\\newton"))
             |$fN| ((#\u . "\\femto\\newton"))
             ;; Ohm
             |$GOhm| ((#\u . "\\giga\\ohm"))
             |$MOhm| ((#\u . "\\mega\\ohm"))
             |$kOhm| ((#\u . "\\kilo\\ohm"))
             ($ohm |$Ohm|) ((#\u . "\\ohm"))
             |$mOhm| ((#\u . "\\milli\\ohm"))
             $microOhm ((#\u . "\\micro\\ohm"))
             |$pOhm| ((#\u . "\\pico\\ohm"))
             |$nOhm| ((#\u . "\\nano\\ohm"))
             |$fOhm| ((#\u . "\\femto\\ohm"))
             ;; Pascal
             |$GPa| ((#\u . "\\giga\\pascal"))
             |$MPa| ((#\u . "\\mega\\pascal"))
             |$kPa| ((#\u . "\\kilo\\pascal"))
             ($pascal |$Pa|) ((#\u . "\\pascal"))
             |$mPa| ((#\u . "\\milli\\pascal"))
             (|$microPa| $micropascal) ((#\u . "\\micro\\pascal"))
             |$pPa| ((#\u . "\\pico\\pascal"))
             |$nPa| ((#\u . "\\nano\\pascal"))
             |$fPa| ((#\u . "\\femto\\pascal"))
             ;; parsec
             ($parsec $pc) ((#\u . "pc"))
             ;; Second
             |$Gs| ((#\u . "\\giga\\second"))
             |$Ms| ((#\u . "\\mega\\second"))
             $ks ((#\u . "\\kilo\\second"))
             ($second $s) ((#\u . "\\second"))
             $ms ((#\u . "\\milli\\second"))
             ($micros $microsecond) ((#\u . "\\micro\\second"))
             $ns ((#\u . "\\nano\\second"))
             $ps ((#\u . "\\pico\\second"))
             $fs ((#\u . "\\femto\\second"))
             ;; Siemens
             |$GS| ((#\u . "\\giga\\siemens"))
             |$MS| ((#\u . "\\mega\\siemens"))
             |$kS| ((#\u . "\\kilo\\siemens"))
             ($siemens |$s|) ((#\u . "\\siemens"))
             |$mS| ((#\u . "\\milli\\siemens"))
             (|$microS| $microsiemens) ((#\u . "\\micro\\siemens"))
             |$nS| ((#\u . "\\nano\\siemens"))
             |$pS| ((#\u . "\\pico\\siemens"))
             |$fS| ((#\u . "\\femto\\siemens"))
             ;; Tesla
             |$GT| ((#\u . "\\giga\\tesla"))
             |$MT| ((#\u . "\\mega\\tesla"))
             |$kT| ((#\u . "\\kilo\\tesla"))
             ($tesla |$t|) ((#\u . "\\tesla"))
             |$mT| ((#\u . "\\milli\\tesla"))
             (|$microT| $microtesla) ((#\u . "\\micro\\tesla"))
             |$nT| ((#\u . "\\nano\\tesla"))
             |$pT| ((#\u . "\\pico\\tesla"))
             |$fT| ((#\u . "\\femto\\tesla"))
             ;; Volt
             |$GV| ((#\u . "\\giga\\volt"))
             |$MV| ((#\u . "\\mega\\volt"))
             |$kV| ((#\u . "\\kilo\\volt"))
             ($volt |$v|) ((#\u . "\\volt"))
             |$mV| ((#\u . "\\milli\\volt"))
             (|$microV| $microvolt) ((#\u . "\\micro\\volt"))
             |$pV| ((#\u . "\\pico\\volt"))
             |$nV| ((#\u . "\\nano\\volt"))
             |$fV| ((#\u . "\\femto\\volt"))
             ;; Watt
             |$GW| ((#\u . "\\giga\\watt"))
             |$MW| ((#\u . "\\mega\\watt"))
             |$kW| ((#\u . "\\kilo\\watt"))
             ($watt |$w|) ((#\u . "\\watt"))
             |$mW| ((#\u . "\\milli\\watt"))
             (|$microW| $microwatt) ((#\u . "\\micro\\watt"))
             |$pW| ((#\u . "\\pico\\watt"))
             |$nW| ((#\u . "\\nano\\watt"))
             |$fW| ((#\u . "\\femto\\watt"))
             ;; Weber
             |$GWb| ((#\u . "\\giga\\weber"))
             |$MWb| ((#\u . "\\mega\\weber"))
             |$kWb| ((#\u . "\\kilo\\weber"))
             ($weber |$Wb|) ((#\u . "\\weber"))
             |$mWb| ((#\u . "\\milli\\weber"))
             (|$microWb| $microweber) ((#\u . "\\micro\\weber"))
             |$nWb| ((#\u . "\\nano\\weber"))
             |$pWb| ((#\u . "\\pico\\weber"))
             |$fWb| ((#\u . "\\femto\\weber"))
             ;; yard
             $yard ((#\u . "yd")))
  :symbol-default '((#\u . "~*~A~:[~;~:*~@{_~A~}~]")))
