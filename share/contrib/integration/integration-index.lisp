(in-package :cl-info)
(let (
(deffn-defvr-pairs '(
; CONTENT: (<INDEX TOPIC> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS> <NODE NAME>))
("conditional_integrate" . ("abs_integrate.info" 9670 806 "Definitions for abs_integrate"))
("convert_to_signum" . ("abs_integrate.info" 10477 582 "Definitions for abs_integrate"))
("extra_definite_integration_methods" . ("abs_integrate.info" 5744 1135 "Definitions for abs_integrate"))
("extra_integration_methods" . ("abs_integrate.info" 3710 2033 "Definitions for abs_integrate"))
("intfudu(<e>," . ("abs_integrate.info" 6880 1541 "Definitions for abs_integrate"))
("intfugudu" . ("abs_integrate.info" 8422 826 "Definitions for abs_integrate"))
("signum_to_abs" . ("abs_integrate.info" 9249 420 "Definitions for abs_integrate"))
))
(section-pairs '(
; CONTENT: (<NODE NAME> . (<FILENAME> <BYTE OFFSET> <LENGTH IN CHARACTERS>))
("Definitions for abs_integrate" . ("abs_integrate.info" 3642 7417))
("Introduction to abs_integrate" . ("abs_integrate.info" 489 3003))
)))
(load-info-hashtables (maxima::maxima-load-pathname-directory) deffn-defvr-pairs section-pairs))
