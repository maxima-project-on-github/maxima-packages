;; expressions_from_xml.lisp -- construct Maxima expressions from XML
;; copyright 2021 by Robert Dodier
;; I release this work under terms of the GNU General Public License

(ql:quickload "xmls")

;; about elements vs nodes, see for example:
;; https://stackoverflow.com/questions/132564/whats-the-difference-between-an-element-and-a-node-in-xml
;;
;; apparently element is the only kind of node which can contain child nodes and attributes.
;; probably need to think more carefully about elements, nodes, children, etc.

(defmfun $expressions_from_xml (x)
  (cond
    ((stringp x) x)
    (x
     (let*
       ((op (mfuncall '$parse_string (xmls:node-name x)))
        (attribute-eqs (mapcar (lambda (pq) (cons '(mequal) pq)) (xmls:node-attrs x)))
        (children-exprs (mapcar (symbol-function '$expressions_from_xml) (xmls:node-children x))))
       `((mqapply) ((,op) ,@attribute-eqs) ,@children-exprs)))
     (t x)))

(defmfun $parse_xml (f)
  (cond
    ((stringp f)
     (xmls:parse f))
    ((streamp f)
     (xmls:parse f))
    ((pathnamep f)
     (with-open-file (s f)
       (xmls:parse s)))
    (t
      (merror "parse_xml: argument must be a string, stream, or Lisp pathname; found ~M~%" f))))

(ql:quickload "zippy")
(ql:quickload "flexi-streams")

(defun $open_zip_input_stream (zipfile-name entry-name)
  (let*
    ((z (org.shirakumo.zippy:open-zip-file zipfile-name))
     (ee (mapcar (lambda (e) (list (org.shirakumo.zippy:file-name e) e)) (coerce (org.shirakumo.zippy:entries z) 'list)))
     (a (second (assoc entry-name ee :test #'string=)))
     (c (org.shirakumo.zippy:entry-to-vector a))
     (s0 (flexi-streams:make-in-memory-input-stream c)))
    (flexi-streams:make-flexi-stream s0 :external-format :utf-8)))

