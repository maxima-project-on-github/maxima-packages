;; expressions_from_xml.lisp -- construct Maxima expressions from XML
;; copyright 2021 by Robert Dodier
;; I release this work under terms of the GNU General Public License

(ql:quickload :plump)

;; By default, plump assumes some stuff applicable to HTML tags -- avoid that.
;; (In particular, it is assumed that <input> is a self-closing tag.)

(setq plump:*tag-dispatchers* plump:*xml-tags*)

;; about elements vs nodes, see for example:
;; https://stackoverflow.com/questions/132564/whats-the-difference-between-an-element-and-a-node-in-xml
;;
;; apparently element is the only kind of node which can contain child nodes and attributes.
;; probably need to think more carefully about elements, nodes, children, etc.

(defmfun $expressions_from_xml (x)
  (cond
    ((plump:textual-node-p x)
     (plump:text x))
    ((plump:nesting-node-p x)
     (let*
       ((op (mfuncall '$parse_string (plump:tag-name x)))
        (attribute-keys (loop for k being the hash-keys of (plump:attributes x) collect k))
        (attribute-values (mapcar #'(lambda (a) (plump:get-attribute x a)) attribute-keys))
        (children-list (coerce (plump:child-elements x) 'list))
        (children-exprs (mapcar (symbol-function '$expressions_from_xml) children-list)))
       (if children-exprs
         `((mqapply) ((,op) ,@attribute-values) ,@children-exprs)
         (if (plump:first-child x)
           (let ((first-child-expr (funcall (symbol-function '$expressions_from_xml) (plump:first-child x))))
             `((mqapply) ((,op) ,@attribute-values) ,first-child-expr))
           `((mqapply) ((,op) ,@attribute-values))))))
     (x
       (merror "expressions_from_xml: nonnull argument ~A is not a text or nesting node in an XML parse tree." x))
     (t
       (merror "expressions_from_xml: null argument; how did that happen?"))))

(defmfun $parse_xml (f)
  (plump:first-child
    (cond
      ((stringp f)
       (plump:parse f))
      ((streamp f)
       (plump:parse (plump:slurp-stream f)))
      ((pathnamep f)
       (with-open-file (s f)
         (plump:parse (plump:slurp-stream s))))
      (t
        (plump:parse (string f))))))
