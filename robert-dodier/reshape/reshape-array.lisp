(defmfun $reshape_array_by_rows (a new-dims)
  (let
    ((b (apply '$make_array '$any (rest new-dims)))
     (n-elements (apply '* (rest new-dims))))
    (dotimes (i n-elements) (setf (row-major-aref b i) (row-major-aref a i)))
    b))

(defmfun $reshape_array_by_columns (a new-dims)
  (let
    ((b (apply '$make_array '$any (rest new-dims)))
     (n-elements (apply '* (rest new-dims))))
    (dotimes (i n-elements)
      (let (ii (j i) (n n-elements))
        (loop for k in (reverse (rest new-dims)) do (setq n (/ n k)) (push (floor j n) ii) (setq j (mod j n)))
        #+nil (format t "HEY n-elements = ~d, i = ~d, ii = ~d~%" n-elements i ii)
        (setf (apply #'aref b ii) (row-major-aref a i))))
    b))

(defmfun $flatten_array (x)
  (if (arrayp x)
    (let ((y ($make_array '$any (array-total-size x))))
      (dotimes (i (array-total-size x)) (setf (aref y i) (row-major-aref x i)))
      y)
    (merror "flatten_array: argument must be an array; found: ~M" x)))
