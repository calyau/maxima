(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("frotz" . ("MYTOPIC.info" 867 226 "Definitions for MYTOPIC"))
("transmogrify" . ("MYTOPIC.info" 732 134 "Definitions for MYTOPIC"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
("Definitions for MYTOPIC" . ("MYTOPIC.info" 676 417))
("Introduction to MYTOPIC" . ("MYTOPIC.info" 423 121))
)))
(load-info-hashtables (maxima::maxima-load-pathname-directory) deffn-defvr-pairs section-pairs))
