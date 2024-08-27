;; combining_diacritics.lisp -- create symbol containing a combining diacritic and an input alias for it
;; copyright 2024 by Robert Dodier
;; I release this work under terms of the GNU General Public License, Version 2.

(defun stringify (c) (coerce (list c) 'string))

(defconstant $combining_circumflex (stringify #\combining_circumflex_accent))
(defconstant $combining_macron     (stringify #\combining_macron))
(defconstant $combining_dot        (stringify #\combining_dot_above))
(defconstant $combining_arrow      (stringify #\combining_right_arrow_above))

(mfuncall '$declare $combining_circumflex '$alphabetic)
(mfuncall '$declare $combining_macron     '$alphabetic)
(mfuncall '$declare $combining_dot        '$alphabetic)
(mfuncall '$declare $combining_arrow      '$alphabetic)

(defmfun $define_input_alias (y x diacritic)
  (let*
    ((x-diacritic-name ($sconcat x diacritic))
     (x-diacritic-symbol (mfuncall '$parse_string x-diacritic-name)))
    (putprop y x-diacritic-symbol 'alias)))
