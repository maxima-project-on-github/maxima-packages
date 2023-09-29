(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("flatten_array" . ("reshape.info" 7420 1490 "Definitions for package reshape"))
("get_array_from_declared_array" . ("reshape.info" 6654 753 "Definitions for package reshape"))
("reshape" . ("reshape.info" 995 5564 "Definitions for package reshape"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
("Definitions for package reshape" . ("reshape.info" 923 7881))
("Introduction to package reshape" . ("reshape.info" 242 527))
)))
(load-info-hashtables (maxima::maxima-load-pathname-directory) deffn-defvr-pairs section-pairs))
