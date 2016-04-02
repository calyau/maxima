;; -*- Lisp -*-

(in-package :maxima)

(mk:defsystem maxima-fft
  :source-pathname (maxima::maxima-load-pathname-directory)
  :binary-pathname (maxima::maxima-objdir "share" "numeric")
  :source-extension "lisp"
  :components
  ((:file "fft-package")
   (:file "fft-core" :depends-on ("fft-package"))
   (:file "fft-interface" :depends-on ("fft-core" "fft-package"))))

(mk:oos "maxima-fft" :compile)
