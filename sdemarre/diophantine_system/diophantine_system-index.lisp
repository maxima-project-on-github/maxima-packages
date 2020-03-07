(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("smith_normal_form" . ("diophantine_system.info" 2565 1275 "Functions and Variables for package diophantine_system"))
("solve_diophantine_system" . ("diophantine_system.info" 1410 1154 "Functions and Variables for package diophantine_system"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
("Functions and Variables for package diophantine_system" . ("diophantine_system.info" 1292 2548))
("Introduction to package diophantine_system" . ("diophantine_system.info" 850 283))
)))
(load-info-hashtables (maxima::maxima-load-pathname-directory) deffn-defvr-pairs section-pairs))
