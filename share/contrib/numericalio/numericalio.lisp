;;  Author: Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License,
;;  http://www.gnu.org/copyleft/gpl.html.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package "MAXIMA")

;; This file contains some functions to read and write data files.
;; Data files can contain integers, rationals, floats, complex,
;; strings (in double quotes), and symbols. Symbols are translated
;; to uppercase when read in, thus ``foo'' in a data file is
;; ``FOO'' in Maxima. The entire file is read to construct one
;; object, i.e., no partial reads.
;;
;; Read functions:
;;   M: read_matrix(file_name, sep_ch_flag)$
;;   read_lisp_array(file_name, A, sep_ch_flag)$
;;   read_maxima_array(file_name, A, sep_ch_flag)$
;;   read_hashed_array(file_name, A, sep_ch_flag)$
;;   L: read_nested_list(file_name, sep_ch_flag)$
;;   L: read_list(file_name, sep_ch_flag)$
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
  (mapply-tr '$matrix ($read_nested_list file-name sep-ch-flag)))


(defun $read_lisp_array (file-name A &optional sep-ch-flag)
  ($fillarray A ($read_list file-name sep-ch-flag)))


(defun $read_maxima_array (file-name A &optional sep-ch-flag)
  ($fillarray A ($read_list file-name sep-ch-flag)))


(defun $read_hashed_array (file-name A &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file (in file-name :if-does-not-exist nil)
    (cond ((not (null in))
        (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
          (loop
            (let ((key (read in nil 'eof)) (L))
              (if (eq key 'eof) (return t))
              (if (symbolp key) (setq key (makealias key)))
              (setq L (read-line in nil 'eof))
              (setq L (make-mlist-from-string L sep-ch))
              (arrstore (list (list A 'simp 'array) key) L)))))
      (t (merror "read_hashed_array: ~S: no such file" file-name)))))


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

  ;; Append a separator at the end to make read-delimited-list happy. Ugh.
  ;; But not if the line is emtpy -- otherwise pasting on a delimiter has
  ;; the effect of creating an null element. Double ugh.

  (cond ((> (length (string-trim '(#\space #\tab) s)) 0)
      (setq s (concatenate 'string s (string sep-ch)))))

  (let ((L '()) (in (make-string-input-stream s)) (x) (pc)) 
    (loop 

      ;; Two different methods of reading are needed, because:
      ;; read-delimited-list is undefined if sep-ch is whitespace,
      ;; plus it wants to see a delimiter character at the end,
      ;; plus it throws an error at eof instead of returning 'eof. (sigh)

      (cond ((eq sep-ch '#\space)
          (setq x (read in nil 'eof)))
        (t 
          (setq pc (peek-char t in nil 'eof))
          (cond ((eq (peek-char t in nil 'eof) 'eof)
              (setq x 'eof))
            (t

              ;; Ugh. GCL needs to see the "si::" bit, Clisp needs to not see it.
              #+gcl (setq x (car (si::ignore-errors (read-delimited-list sep-ch in))))
              #-gcl (setq x (car (ignore-errors (read-delimited-list sep-ch in))))))))

      (if (eq x 'eof) (return  (cons '(mlist simp) L)))
      (setq x (lisp-to-maxima x))
      (setq L (append L (list x))))))


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
    (t (merror "write_data: don't know what to do with a ~M" (type-of X)))))


(defun write-matrix (M file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
      (mapcar #'(lambda (x) (write-list-lowlevel (cdr x) out sep-ch)) (cdr M)))))


(defun write-lisp-array (A file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file (out file-name :direction :output :if-exists :supersede)
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
  (let ((keys (cdddr (meval (list '($arrayinfo) A)))) (L))
  (setq file-name (require-string file-name))
    (with-open-file (out file-name :direction :output :if-exists :supersede)
      (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
        (loop
          (if (not keys) (return))
          (setq L ($arrayapply A (car keys)))
          (cond ((listp L) (pop L))
            (t (setq L (list L))))
          (write-list-lowlevel (append (cdr (pop keys)) L) out sep-ch))))))


(defun write-list (L file-name &optional sep-ch-flag)
  (setq file-name (require-string file-name))
  (with-open-file (out file-name :direction :output :if-exists :supersede)
    (let ((sep-ch (get-sep-ch sep-ch-flag file-name)))
      (write-list-lowlevel (cdr L) out sep-ch))))


(defun write-list-lowlevel (L stream sep-ch)
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
  (setq x (maybe-convert-rat x))
  (setq x (maybe-convert-symbol x))
  (format fs (cond ((floatp x) "~F")
    (t "~S")) x))


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
    ((eq sep-ch-flag '$CSV) '#\,)
    ((eq sep-ch-flag '$SPACE) '#\space)

    ((null sep-ch-flag)
      (cond ((equal (pathname-type file-name) "csv") '#\,)
        (t '#\space)))
    (t
      (format t "numericalio: separator flag ~S not recognized; assume SPACE" (stripdollar sep-ch-flag))
      '#\space)))


(defun require-string (s)
  (cond ((mstringp s)
      (symbol-name (stripdollar s)))
    (t
      (merror "numericalio: expected a string, instead found a ~:M" (type-of s)))))
