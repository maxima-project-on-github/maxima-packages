(defsystem superq
  :defsystem-depends-on ("info-index")
  :name "superq"
  :maintainer "Robert Dodier"
  :author "Robert Dodier"
  :licence "GNU General Public License"
  :description "'super' quoting mechanism"
  :long-description "Maxima package for 'super' quoting, that is to say, preventing both simplification and evaluation."
  
  :components
  ((:file "superq")
   (:info-index "superq-index")))
