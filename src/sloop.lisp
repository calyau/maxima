(in-package :cl-sloop)
(defmacro sloop (&rest body)
  (warn "Using deprecated macro 'sloop'. Use 'loop' instead.")
  `(loop ,@body))

