(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("plot_vector_field" . ("drawutils.info" 768 766 "Vector fields"))
("plot_vector_field3d" . ("drawutils.info" 1535 858 "Vector fields"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
)))
(load-info-hashtables (pathname-directory #-gcl *load-pathname* #+gcl sys:*load-pathname*) deffn-defvr-pairs section-pairs))
