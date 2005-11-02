;;  Author: Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package "MAXIMA")

;; This file contains some functions to read and write data files.
;; Data files can contain integers, rationals, floats, complex,
;; strings (in double quotes), and symbols. The case of a symbol
;; (upper, lower, or mixed) is preserved, thus ``FOO'', ``Bar'',
;; and ``baz'' in a data file are ``FOO'', ``Bar'', and ``baz'' in
;; Maxima. The entire file is read to construct one object;
;; partial reads are not supported.
;;
;; Read functions:
;;   M: read_matrix (file_name, sep_ch_flag)$
;;   read_lisp_array (file_name, A, sep_ch_flag)$
;;   read_maxima_array (file_name, A, sep_ch_flag)$
;;   read_hashed_array (file_name, A, sep_ch_flag)$
;;   L: read_nested_list (file_name, sep_ch_flag)$
;;   L: read_list (file_name, sep_ch_flag)$
;;
;; Write function:
;;   write_data(X, file_name, sep_ch_flag)$
;;
;; Notes: (1) sep_ch_flag tells what to use to separate elements.
;; It is an optional argument for all read & write functions.
;; Two values are understood: 'CSV for comma separated values,
;; and 'SPACE for space separated values. If the file name ends
;; in ".csv", 'CSV is assumed unless sep_ch_flag is explicitly specified.
;; If not ".csv" and not specified, 'SPACE is assumed.
;;
;; (2) READ_MATRIX infers the size of the matrix from the input data.
;;
;; (3) READ_LISP_ARRAY and READ_MAXIMA_ARRAY require that the array
;; be declared by MAKE_ARRAY or ARRAY (respectively) before calling
;; the read function. (This obviates the need to infer the array 
;; dimensions, which could be a hassle for arrays with multiple dimensions.)
;;
;; (4) READ_LISP_ARRAY and READ_MAXIMA_ARRAY do not check to see that the 
;; input file conforms in some way to the array dimensions; the input
;; is read as a flat list, then the array is filled using FILLARRAY.
;;
;; (5) READ_HASHED_ARRAY treats the first item on a line as a
;; hash key, and makes the remainder of the line into a list:
;; reading "567 12 17 32 55" is equivalent to A[567]: [12, 17, 32, 55]$
;; Lines need not have the same numbers of elements.
;;
;; (6) READ_NESTED_LIST makes a list which has a sublist for each
;; line of input. Lines need not have the same numbers of elements.
;; Empty lines are -not- ignored: an empty line yields an empty sublist.
;; 
;; (7) READ_LIST reads all input into a flat list.
;; READ_LIST ignores end-of-line characters.
;;
;; (8) WRITE_DATA figures out what kind of object is X and
;; hands it off to an appropriate function to write it out.
;;
;; (9) WRITE_DATA writes matrices in row-major form,
;; with one line per row.
;;
;; (10) WRITE_DATA writes Lisp and Maxima declared arrays in
;; row-major form, with a new line at the end of every slab.
;; Higher-dimensional slabs are separated by additional new lines.
;;
;; (11) WRITE_DATA writes hashed arrays with a key followed by
;; the associated list on each line.
;;
;; (12) WRITE_DATA writes a nested list with each sublist on one line.
;;
;; (13) WRITE_DATA writes a flat list all on one line.

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
        (let (key L (sep-ch (get-sep-ch sep-ch-flag file-name)))
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
          (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
            (loop
              (setq L (read-line in nil 'eof))
              (if (eq L 'eof) (return (cons '(mlist simp) A)))
              (setq A (append A (list (make-mlist-from-string L sep-ch)))))))
        (t (merror "read_nested_list: ~S: no such file" file-name))))))


(defun $read_list (file-name &optional sep-ch-flag)
  (let ((A '()) (L))
    (setq file-name (require-string file-name))
    (with-open-file (in file-name :if-does-not-exist nil)
      (cond ((not (null in))
          (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
            (loop
              (setq L (read-line in nil 'eof))
              (if (eq L 'eof) (return (cons '(mlist simp) A)))
              (setq A (append A (cdr (make-mlist-from-string L sep-ch)))))))
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
              (format t "numericalio: trailing sign (~S) at end of line; strange, but just eat it." sign)))
           (cond
             ((eq sep-ch #\space)
              (return (cons '(mlist) LL)))
             (t
               (return (cons '(mlist) (appropriate-append L LL)))))))
        (cond
          ((or (eq token '|$-|) (eq token '|$+|))
           (setq sign (cond ((eq token '|$-|) -1) (t 1))))
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

(defun lisp-to-maxima (x)
  (cond ((null x) x)
    ((eq x t) x)
    ((symbolp x) (makealias x))
    ((and (rationalp x) (not (integerp x)))
      `((rat simp) ,(numerator x) ,(denominator x)))
    ((complexp x)
      `((mplus simp) ,(lisp-to-maxima (realpart x)) 
        ((mtimes simp) ,(lisp-to-maxima (imagpart x)) $%i)))
    (t x)))


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
    (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
      (mapcar #'(lambda (x) (write-list-lowlevel (cdr x) out sep-ch)) (cdr M)))))


(defun write-lisp-array (A file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file-appropriately (out file-name)
    (let ((sep-ch (get-sep-ch sep-ch-flag file-name)) (d (array-dimensions A)))
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
      (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
        (loop
          (if (not keys) (return))
          (setq L ($arrayapply A (car keys)))
          (cond ((listp L) (pop L))
            (t (setq L (list L))))
          (write-list-lowlevel (append (cdr (pop keys)) L) out sep-ch))))))


(defun write-list (L file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file-appropriately (out file-name)
    (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
      (write-list-lowlevel (cdr L) out sep-ch))))


(defun write-list-lowlevel (L stream sep-ch)
  (setq sep-ch (cond ((symbolp sep-ch) (cadr (exploden sep-ch))) (t sep-ch)))
  (cond ((not (null L))
      (loop 
        (if (not L) (return))
        (let ((e (pop l)))
          (cond (($listp e)
              (write-list-lowlevel (cdr e) stream sep-ch))
            (t (my-mgrind e stream)
              (cond ((null L) (terpri stream))
                (t (write-char sep-ch stream)))))))))
  (finish-output stream))


(defun my-mgrind (x fs)
  (setq x (maybe-convert-complex x))
  (cond ((complexp x) (format fs "~S" x))
    (t (mgrind x fs))))


(defun maybe-convert-complex (x)
  (cond ((not ($freeof '$%i x))
      (let ((xr ($realpart x)) (xi ($imagpart x)))
        (setq xr (maybe-convert-rat xr))
        (setq xi (maybe-convert-rat xi))
        (if (and (numberp xr) (numberp xi))
          (setq x (complex xr xi))))))
  x)


(defun maybe-convert-rat (x)
  (cond ((and ($ratnump x) (not (integerp x)))
      (setq x (/ (nth 1 x) (nth 2 x)))))
  x)


(defun maybe-convert-symbol (x)
  (cond ((and (symbolp x) (not (null x)) (not (eq x t)))
      (setq x (stripdollar x))))
  x)
  

(defun get-sep-ch (sep-ch-flag file-name)
  (cond
    ((eq sep-ch-flag '$csv) '|$,|)
    ((eq sep-ch-flag '$pipe) '$\|)
    ((eq sep-ch-flag '$space) '#\space)

    ((null sep-ch-flag)
      (cond ((equal (pathname-type file-name) "csv") '|$,|)
        (t '#\space)))
    (t
      (format t "numericalio: separator flag ~S not recognized; assume ``space''" (stripdollar sep-ch-flag))
      '#\space)))


(defun require-string (s)
  (cond ((mstringp s)
      (print-invert-case (stripdollar s)))
    (t
      (merror "numericalio: expected a string, instead found a ~:M" (type-of s)))))
