;; noninteractive.lisp -- some functions to be used by noninteractive.mac
;; copyright 2007 by Robert Dodier
;; I release this file under the terms of the GNU General Public License, version 2.

;; Redefine MERROR to throw something.
(defun merror (s &rest l) (meval `(($throw) '((merror) ,s ,@l))))

;; Expose GENSYM as a Maxima function.
(defun $gensym () (cadr (dollarify `(,(gensym)))))

