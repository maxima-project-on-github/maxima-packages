;; superq.lisp -- "super" quoting for Maxima
;; Copyright 2008 by Robert Dodier
;; I release this work under the terms of the GNU General Public License

;; Examples:

;; load (superq);

;; '(1 + 1);
;;  => 2
;; superq (1 + 1);
;;  => superq(1 + 1)
;; '(5!);
;;  => 120
;; superq (5!);
;;  => superq(5!)
;; '(sin (0));
;;  => 0
;; superq (sin (0));
;;  => superq(sin(0))
;; '('diff (x, x, 1));
;;  => 1
;; superq('diff (x, x, 1));
;;  => superq('diff(x,x,1))

;; tex ('(1 + 1));
;;  => $$2$$
;; tex (superq (1 + 1));
;;  => $$1+1$$
;; tex ('(5!));
;;  => $$120$$
;; tex (superq (5!));
;;  => $$5!$$
;; tex ('(sin (0)));
;;  => $$0$$
;; tex (superq (sin (0)));
;;  => $$\sin 0$$
;; tex ('('diff (x, x, 1)));
;;  => $$1$$
;; tex (superq('diff (x, x, 1)));
;;  => $${{d}\over{d\,x}}\,x$$

(defmspec $superq (e) e)

(defun simp-$superq (x y z) (declare (ignore y z)) x)

(setf (get '$superq 'operators) 'simp-$superq)

(defun tex-$superq (x l r) (tex (second x) l r 'mparen 'mparen))

(setf (get '$superq 'tex) 'tex-$superq)
