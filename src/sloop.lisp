(in-package :cl-sloop)
(defmacro sloop (&rest body) `(loop ,@body))

