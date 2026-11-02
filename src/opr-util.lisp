;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;
;;; This file is part of the Maxima computer algebra project
;;; (https://sourceforge.net/projects/maxima/)
;;; SPDX-License-Identifier: GPL-2.0-or-later
;;;
;;; Maxima is copyrighted by its authors and licensed under the GNU
;;; General Public License.  This program is distributed WITHOUT ANY
;;; WARRANTY. See COPYING and AUTHORS for details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)

;; op and opr properties

(defvar *opr-table* (make-hash-table :test #'equal))

(defun getopr0 (x)
  (or
    (and (symbolp x) (get x 'opr))
    (and (stringp x) (gethash x *opr-table*))))

(defun getopr (x)
  (or (getopr0 x) x))

(defun putopr (x y)
  (or
    (and (symbolp x) (setf (get x 'opr) y))
    (and (stringp x) (setf (gethash x *opr-table*) y))))

(defun remopr (x)
  (or
    (and (symbolp x) (remprop x 'opr))
    (and (stringp x) (remhash x *opr-table*))))


