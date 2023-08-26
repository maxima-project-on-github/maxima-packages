(defun reshape-array-by-rows (a new-dims)
  (let
    ((b (apply '$make_array '$any new-dims))
     (n-elements (apply '* new-dims)))
    (dotimes (i n-elements) (setf (row-major-aref b i) (row-major-aref a i)))
    b))

(defun reshape-array-by-columns (a new-dims)
  (let
    ((b (apply '$make_array '$any new-dims))
     (n-elements (apply '* new-dims)))
    (dotimes (i n-elements)
      (let (ii (j i) (n n-elements))
        (loop for k in (reverse new-dims) do (setq n (/ n k)) (push (floor j n) ii) (setq j (mod j n)))
        #+nil (format t "HEY n-elements = ~d, i = ~d, ii = ~d~%" n-elements i ii)
        (setf (apply #'aref b ii) (row-major-aref a i))))
    b))
