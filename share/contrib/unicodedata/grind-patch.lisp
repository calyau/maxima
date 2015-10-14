;; -*- mode: lisp -*-
;; Copyright Leo Butler (leo.butler@ndsu.edu) 2015
;; Released under the terms of GPLv3+
(in-package :maxima)

(defun slash (x)
  (do ((l (cdr x) (cdr l))) ((null l))
    ;; Following test is the same (except backslash is not included,
    ;; so backslash is preceded by backslash) as in SCAN-TOKEN (src/nparse.lisp).
    ;; Any wide-char should not be escaped
    (if (or (ascii-numberp (car l)) (alphabetp (car l)) (> (char-code (car l)) 128.))
	nil
	(progn (rplacd l (cons (car l) (cdr l)))
	       (rplaca l #\\) (setq l (cdr l)))))
  (if (or (alphabetp (car x)) (> (char-code (car x)) 128.)) x (cons #\\ x)))


; end of grind-patch.lisp 
