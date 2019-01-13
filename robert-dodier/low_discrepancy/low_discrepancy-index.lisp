(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("low_discrepancy_makelist_faure" . ("low_discrepancy.info" 1749 174 "Definitions for package low_discrepancy"))
("low_discrepancy_makelist_halton" . ("low_discrepancy.info" 1924 178 "Definitions for package low_discrepancy"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
("Definitions for package low_discrepancy" . ("low_discrepancy.info" 1661 441))
("Introduction to package low_discrepancy" . ("low_discrepancy.info" 781 708))
)))
(load-info-hashtables (maxima::maxima-load-pathname-directory) deffn-defvr-pairs section-pairs))
