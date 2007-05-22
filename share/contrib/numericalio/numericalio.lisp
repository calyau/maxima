;;  Author: Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

;; Read functions:
;;   M: read_matrix (file_name, sep_ch_flag)$
;;   read_lisp_array (file_name, A, sep_ch_flag)$
;;   read_maxima_array (file_name, A, sep_ch_flag)$
;;   read_hashed_array (file_name, A, sep_ch_flag)$
;;   L: read_nested_list (file_name, sep_ch_flag)$
;;   L: read_list (file_name, sep_ch_flag)$
;;
;; Write function:
;;   write_data (X, file_name, sep_ch_flag)$

;; See numericalio.texi for a lengthier description.

;; -------------------- read functions --------------------

(defun $read_matrix (file-name &optional sep-ch-flag)
  `(($matrix) ,@(cdr ($read_nested_list file-name sep-ch-flag))))


(defun $read_lisp_array (file-name A &optional sep-ch-flag)
  ($fillarray A ($read_list file-name sep-ch-flag))
  '$done)


(defun $read_maxima_array (file-name A &optional sep-ch-flag)
  ($fillarray A ($read_list file-name sep-ch-flag))
  '$done)


(defun $read_hashed_array (file-name A &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file (in file-name :if-does-not-exist nil)
    (cond
      ((not (null in))
        (let (key L (sep-ch (get-input-sep-ch sep-ch-flag file-name)))
          (loop
            (setq L (read-line in nil 'eof))
            (if (eq L 'eof) (return))
            (setq L (make-mlist-from-string L sep-ch))
            (cond
              ((> ($length L) 0)
               (setq key ($first L))
               (if (= ($length L) 1)
                 (arrstore (list (list A 'array) key) nil)
                 (arrstore (list (list A 'array) key) ($rest L))))))))
      (t (merror "read_hashed_array: ~S: no such file" file-name))))
  '$done)


(defun $read_nested_list (file-name &optional sep-ch-flag)
  (let ((A '()) (L))
    (setq file-name (require-string file-name))
    (with-open-file (in file-name :if-does-not-exist nil)
      (cond ((not (null in))
          (let ((sep-ch (get-input-sep-ch sep-ch-flag file-name)))
            (loop
              (setq L (read-line in nil 'eof))
              (if (eq L 'eof) (return (cons '(mlist simp) A)))
              (setq A (append A (list (make-mlist-from-string L sep-ch)))))))
        (t (merror "read_nested_list: ~S: no such file" file-name))))))


(defun $read_list (file-name &optional sep-ch-flag)
  (let ((A '()) (L))
    (setq file-name (require-string file-name))
    (with-open-file (in file-name :if-does-not-exist nil)
      (cond
       ((not (null in))
	(let ((sep-ch (get-input-sep-ch sep-ch-flag file-name)))
	  (loop
	   (setq L (read-line in nil 'eof))
	   (if (eq L 'eof)
	       (return (cons '(mlist simp) (nreverse A))))
	   ;; use nreconc accumulation to avoid n^2 cons's
	   (setq A (nreconc (cdr (make-mlist-from-string L sep-ch)) A)))))
       (t (merror "read_list: ~S: no such file" file-name))))))

;; Usage: (make-mlist-from-string "1 2 3 foo bar baz")

(defun make-mlist-from-string (s sep-ch)
  ; scan-one-token-g isn't happy with symbol at end of string.
  (setq s (concatenate 'string s " "))

  (with-input-from-string (*parse-stream* s)
    (let ((token) (L) (LL) (sign))
      (loop
        (setq token (scan-one-token-g t 'eof))
        (cond
          ((eq token 'eof)
           (cond
             ((not (null sign))
              (format t "numericalio: trailing sign (~S) at end of line; strange, but just eat it.~%" sign)))
           (cond
             ((eq sep-ch #\space)
              (return (cons '(mlist) LL)))
             (t
               (return (cons '(mlist) (appropriate-append L LL)))))))
        (cond
          ((or (eq token '$-) (eq token '$+))
           (setq sign (cond ((eq token '$-) -1) (t 1))))
          (t
            (cond
              ((not (null sign))
               (setq token (m* sign token))
               (setq sign nil)))
            (cond
              ((eq sep-ch #\space)
               (setq LL (append LL (list token))))
              (t
                (cond
                  ((eq token sep-ch)
                   (setq L (appropriate-append L LL))
                   (setq LL nil))
                  (t
                    (setq LL (append LL (list token)))))))))))))


(defun appropriate-append (L LL)
  (cond
    ((null LL) (append L '(nil)))
    ((= (length LL) 1) (append L LL))
    (t (append L (list (append '((mlist)) LL))))))


;; -------------------- write functions -------------------

(defun $write_data (X file-name &optional sep-ch-flag)
  (cond (($matrixp X)
      (write-matrix X file-name sep-ch-flag))
    ((arrayp X)
      (write-lisp-array X file-name sep-ch-flag))
    ((mget X 'array)
      (write-maxima-array X file-name sep-ch-flag))
    ((mget X 'hashar)
      (write-hashed-array X file-name sep-ch-flag))
    (($listp X)
      (write-list X file-name sep-ch-flag))
    (t (merror "write_data: don't know what to do with a ~M" (type-of X))))
  '$done)

; Thanks to William Bland on comp.lang.lisp for help writing this macro.
(defmacro with-open-file-appropriately ((out fname) &body body)
  `(with-open-file (,out ,fname :direction :output
    :if-exists (if (or (eq $file_output_append '$true) (eq $file_output_append t)) :append :supersede)
    :if-does-not-exist :create)
    ,@body))

(defun write-matrix (M file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file-appropriately (out file-name)
    (let ((sep-ch (get-output-sep-ch sep-ch-flag file-name)))
      (mapcar #'(lambda (x) (write-list-lowlevel (cdr x) out sep-ch)) (cdr M)))))


(defun write-lisp-array (A file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file-appropriately (out file-name)
    (let ((sep-ch (get-output-sep-ch sep-ch-flag file-name)) (d (array-dimensions A)))
      (write-lisp-array-helper A d '() out sep-ch))))


(defun write-lisp-array-helper (A d indices out sep-ch)
  (cond ((equalp (length d) 1)
      (let ((L '()))
        (sloop for i from 0 to (- (car d) 1) do
          (let ((x (apply 'aref (append (list A) (reverse (cons i indices))))))
            (setq L (cons x L))))
        (write-list-lowlevel (reverse L) out sep-ch)))
    (t
      (sloop for i from 0 to (- (car d) 1) do
        (write-lisp-array-helper A (cdr d) (cons i indices) out sep-ch)
        (cond ((> (length d) 2) (terpri out)))))))


(defun write-maxima-array (A file-name &optional sep-ch-flag)
  (write-lisp-array (symbol-array (mget A 'array)) file-name sep-ch-flag))


(defun write-hashed-array (A file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (let ((keys (cdddr (meval (list '($arrayinfo) A)))) (L))
    (with-open-file-appropriately (out file-name)
      (let ((sep-ch (get-output-sep-ch sep-ch-flag file-name)))
        (loop
          (if (not keys) (return))
          (setq L ($arrayapply A (car keys)))
          (cond ((listp L) (pop L))
            (t (setq L (list L))))
          (write-list-lowlevel (append (cdr (pop keys)) L) out sep-ch))))))


(defun write-list (L file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file-appropriately (out file-name)
    (let ((sep-ch (get-output-sep-ch sep-ch-flag file-name)))
      (write-list-lowlevel (cdr L) out sep-ch))))


(defun write-list-lowlevel (L stream sep-ch)
  (setq sep-ch (cond ((symbolp sep-ch) (cadr (exploden sep-ch))) (t sep-ch)))
  (cond ((not (null L))
      (loop 
        (if (not L) (return))
        (let ((e (pop L)))
          (cond (($listp e)
              (write-list-lowlevel (cdr e) stream sep-ch))
            (t (mgrind e stream)
              (cond ((null L) (terpri stream))
                (t (write-char sep-ch stream)))))))))
  (finish-output stream))


(defun get-input-sep-ch (sep-ch-flag file-name)
  (cond
    ((eq sep-ch-flag '$tab)
     (format t "numericalio: separator flag ``tab'' not recognized for input; assume ``space'' instead.~%")
     #\space)
    (t (get-output-sep-ch sep-ch-flag file-name))))


(defun get-output-sep-ch (sep-ch-flag file-name)
  (cond
    ((eq sep-ch-flag '$space) #\space)
    ((eq sep-ch-flag '$tab) #\tab)
    ((or (eq sep-ch-flag '$comma) (eq sep-ch-flag '$csv)) '$\,) ; '$csv is backwards compatibility ... sigh
    ((eq sep-ch-flag '$pipe) '$\|)
    ((eq sep-ch-flag '$semicolon) '$\;)

    ((null sep-ch-flag)
      (cond ((equal (pathname-type file-name) "csv") '$\,)
        (t #\space)))
    (t
      (format t "numericalio: separator flag ~S not recognized; assume ``space''.~%" (stripdollar sep-ch-flag))
      #\space)))


(defun require-string (s)
  (cond
    ((stringp s)
     s)
    ((mstringp s)
     (print-invert-case (stripdollar s)))
    (t
      (merror "numericalio: expected a string, instead found a ~:M" (type-of s)))))
