(in-package #:maxima)

#+nil
(progn
  (format t "path = ~A~%" (combine-path *maxima-sharedir* "lapack"))
  (format t "(maxima-load-pathname-directory) = ~A~%" (maxima-load-pathname-directory))
  (format t "sys = ~A~%" (merge-pathnames (make-pathname :name "lapack" :type "system") (maxima-load-pathname-directory))))

(unless (member :mk-defsystem *features*) ($load "lisp-utils/defsystem.lisp"))

;; Compiling this package needs an unusual amount of heap.  dgesvd.lisp is
;; f2cl's translation of dgesvd.f as a single 7,483-line function, and
;; compiling it alone peaks well over a gigabyte -- more than the dynamic
;; space Debian and Ubuntu build SBCL with.  When that runs out, SBCL dies
;; in the garbage collector with "Heap exhausted, game over.", which no
;; handler-case can catch, so say so beforehand rather than after several
;; minutes of compiling.
;;
;; Only the first load pays this: the compiled files are cached in the
;; objdir, and loading them afterwards costs a few tens of megabytes.
#+sbcl
(let* ((have (sb-ext:dynamic-space-size))
       (want (* 2048 1024 1024))
       ;; Only the compiling load is expensive, so keep quiet once the
       ;; objdir holds the fasl for the file that needs the room.
       (already-compiled
         (probe-file
           (merge-pathnames
             (make-pathname
               :directory '(:relative "share" "lapack" "lapack")
               :name "dgesvd"
               :type (pathname-type (compile-file-pathname "foo.lisp")))
             (pathname (concatenate 'string *maxima-objdir* "/"))))))
  (when (and (< have want) (not already-compiled))
    ;; MTELL takes MFORMAT directives, not CL FORMAT ones: ~A and ~% work,
    ;; ~D and ~,0F and ~T signal an error.  Hence the pre-formatted number
    ;; and the one-line-at-a-time message.
    (let ((mb (round have (* 1024 1024))))
      (mtell "~%Note: compiling share/lapack needs more heap than this~%")
      (mtell "Lisp has.  SBCL was given ~A MB, and dgesvd.lisp alone~%" mb)
      (mtell "needs over 1 GB.  If this fails with \"Heap exhausted\",~%")
      (mtell "quit and restart Maxima as~%~%")
      (mtell "    MAXIMA_LISP_OPTIONS=\"--dynamic-space-size 2048\" maxima~%~%")
      (mtell "then load(lapack) again.  Only this first, compiling load~%")
      (mtell "needs the larger heap; the result is cached afterwards.~%~%"))))

(load (merge-pathnames (make-pathname :name "lapack" :type "system") (maxima-load-pathname-directory)))

;; Maxima errored out when any lapack function was used which
;; most certainly was an ECL bug: Seems like the definition of the
;; MAXIMA package shadows the array symbol from the COMMON-LISP package.
;; Bugfix by Marius Gerbershagen:
#+ecl (in-package #:common-lisp)

#-abcl (mk:oos "lapack-interface" :compile)

#+abcl (require "asdf")
#+abcl (push (maxima-load-pathname-directory) asdf:*central-registry*)
#+abcl (asdf:operate 'asdf:load-source-op "lapack")
