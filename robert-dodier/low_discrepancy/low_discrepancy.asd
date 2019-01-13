(defsystem low_discrepancy
  :defsystem-depends-on ("maxima-file" "info-index")
  :name "low_discrepancy"
  :maintainer "Robert Dodier"
  :author "Robert Dodier"
  :licence "ACM License"
  :description "Low-discrepancy sequences"
  :long-description "Maxima package for generating low-discrepancy (quasirandom) sequences, based on CACM Algorithm 647."
  
  :components
  ((:maxima-file "low_discrepancy")
   (:info-index "low_discrepancy-index")))
