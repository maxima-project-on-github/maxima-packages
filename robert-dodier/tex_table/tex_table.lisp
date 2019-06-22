;; tex_table -- define table structure and LaTeX output
;; copyright 2019 by Robert Dodier
;; I release this work under terms of the GNU General Public License, version 2

(defun dimension-$table (a b)
  (let*
    ((a-header ($@-function a '$header))
     (a-rows ($@-function a '$rows))
     (m (if a-header `(($matrix) ,a-header ,@(cdr a-rows)) `(($matrix) ,@(cdr a-rows)))))
    (dim-\$matrix m b)))

(setf (get '$table 'dimension) 'dimension-$table)

