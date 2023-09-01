(defmfun $declared_arrayp (a)
  (and (symbolp a) (mget a 'array) (symbolp (mget a 'array)) (arrayp (get (mget a 'array) 'array))))

(defmfun $get_array_from_declared_array (x)
   (if ($declared_arrayp x)
     (get (mget x 'array) 'array)
     (merror "get_array_from_declared_array: argument must be a declared array; found: ~M" x)))

(defmfun $reshape_array_by_rows (a new-dims)
  (cond
    ((arrayp a) (reshape-lisp-array-by-rows a (rest new-dims)))
    (($declared_arrayp a)
     (reshape-declared-maxima-array a (rest new-dims) 'by-rows))
    (t
      (merror "reshape_array_by_rows: first argument must be an array value or declared array symbol; found: ~M" a))))

(defun reshape-lisp-array-by-rows (a new-dims &optional b)
  (let
    ((n-elements (apply '* new-dims)))
    (when (null b)
      (setq b (apply '$make_array '$any new-dims)))
    (dotimes (i n-elements) (setf (row-major-aref b i) (row-major-aref a i)))
    b))

(defun reshape-declared-maxima-array (a new-dims rows-or-columns)
  ;; A is a declared Maxima array.
  ;; Construct another declared array which is the reshaped array.
  ;; This involves putting a gensym on the arrays infolist,
  ;; which seems slightly terrible. I guess an alternative is to
  ;; return an array value. I've chosen to go down the road of
  ;; constructing a declared array for consistency with the idea
  ;; that the return value is the same kind of thing as the argument.
  (let*
    ((reshaped-a-symbol ($gensym "a"))
     (a-array ($get_array_from_declared_array a))
     reshaped-a-array)
    (meval `(($array) ,reshaped-a-symbol ,@(mapcar #'1- new-dims)))
    (setq reshaped-a-array ($get_array_from_declared_array reshaped-a-symbol))
    (if (eq rows-or-columns 'by-rows)
      (reshape-lisp-array-by-rows a-array new-dims reshaped-a-array)
      (reshape-lisp-array-by-columns a-array new-dims reshaped-a-array))
    reshaped-a-symbol))

(defmfun $reshape_array_by_columns (a new-dims)
  (cond
    ((arrayp a) (reshape-lisp-array-by-columns a (rest new-dims)))
    (($declared_arrayp a)
     (reshape-declared-maxima-array a (rest new-dims) 'by-columns))
    (t
      (merror "reshape_array_by_columns first argument must be an array value or declared array symbol; found: ~M" a))))

(defun reshape-lisp-array-by-columns (a new-dims &optional b)
  (let
    ((n-elements (apply '* new-dims)))
    (when (null b)
      (setq b (apply '$make_array '$any new-dims)))
    (dotimes (i n-elements)
      (let (ii (j i) (n n-elements))
        (loop for k in (reverse new-dims) do (setq n (/ n k)) (push (floor j n) ii) (setq j (mod j n)))
        #+nil (format t "HEY n-elements = ~d, i = ~d, ii = ~d~%" n-elements i ii)
        (setf (apply #'aref b ii) (row-major-aref a i))))
    b))

(defmfun $flatten_array (x)
  (cond
    ((arrayp x)
     (let ((y ($make_array '$any (array-total-size x))))
       (dotimes (i (array-total-size x)) (setf (aref y i) (row-major-aref x i)))
       y))
    (($declared_arrayp x)
     (reshape-declared-maxima-array x (list (array-total-size ($get_array_from_declared_array x))) 'by-rows))
    (t
      (merror "flatten_array: argument must be an array; found: ~M" x))))
