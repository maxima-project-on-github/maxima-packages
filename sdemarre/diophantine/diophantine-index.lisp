(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("diophantine_instantiate_solutions" . ("diophantine.info" 3144 1271 "Functions and Variables for package diophantine"))
("diophantine_solve" . ("diophantine.info" 1265 1878 "Functions and Variables for package diophantine"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
("Functions and Variables for package diophantine" . ("diophantine.info" 1161 3254))
("Introduction to package diophantine" . ("diophantine.info" 734 289))
)))
(load-info-hashtables (maxima::maxima-load-pathname-directory) deffn-defvr-pairs section-pairs))
