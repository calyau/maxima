(in-package :maxima)
;; Cheaper genvar creation and numbering in ORDERPOINTER.
;; GENSYM formats a counter into every name, MAKE-SYMBOL with the
;; readable base name does not. SET goes through
;; ABOUT-TO-MODIFY-SYMBOL-VALUE, but genvars are never declared, constant or
;; dynamically bound, so the global value can be written directly.
(defun gensym-readable (symname)
 (if (or (and (eq *use-readable-gensyms* :debug) (not *mdebug*) *debugger-hook*)
         (not *use-readable-gensyms*))
  (make-symbol "G")
  (cond ((symbolp symname)
	 (make-symbol (string-trim "$" (string symname))))
	(t
	 (setq symname (aformat nil "~:M" symname))
	 (if symname (make-symbol symname) (make-symbol "G"))))))
(defun orderpointer (l)
  (loop for v in l
	 for i below (- (length l) (length genvar))
	 collecting  (gensym-readable v) into tem
	 finally (setq genvar (nconc tem genvar))
       (return (prenumber genvar 1))))
(defun prenumber (v n)
  (do ((vl v (cdr vl))
       (i n (1+ i)))
      ((null vl) nil)
    (sb-kernel:%set-symbol-global-value (car vl) i)))
