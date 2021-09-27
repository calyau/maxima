;;  Copyright 2005 by Robert Dodier

;;  This program is free software; you can redistribute it and/or
;;  modify it under the terms of the GNU General Public License.

;;  This program has NO WARRANTY, not even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

(in-package :maxima)

;; Read functions:
;;
;;   M: read_matrix (source, sep_ch_flag)
;;   read_matrix (source, M, sep_ch_flag)
;;   A : read_array (source, sep_ch_flag)
;;   read_array (source, A, sep_ch_flag)
;;   read_hashed_array (source, A, sep_ch_flag)
;;   L: read_nested_list (source, sep_ch_flag)
;;   L: read_list (source, sep_ch_flag)
;;   read_list (source, L, sep_ch_flag)
;;
;;   read_binary_matrix (source, M)
;;   A : read_binary_array (source)
;;   read_binary_array (source, A)
;;   L: read_binary_list (source)
;;   read_binary_list (source, L)
;;
;; `source' is a file name or input stream.
;;
;; Write functions:
;;
;; `sink' is a file name or output stream.
;;
;;   write_data (X, sink, sep_ch_flag)
;;   write_binary_data (X, sink)
;;
;; Helpers:
;;
;; byte_order_flag recognized values: msb, lsb
;;
;;   assume_external_byte_order (byte_order_flag)
;;

(defun $assume_external_byte_order (x)
  (cond
    ((eq x '$lsb)
     (define-external-byte-order :lsb))
    ((eq x '$msb)
     (define-external-byte-order :msb))
    (t
      (merror "assume_external_byte_order: unrecognized byte order flag: ~a" x))))

(defun lisp-or-declared-maxima-array-p (x)
  (or (arrayp x) (mget x 'array)))

;; THESE FILE-OPENING FUNCTIONS WANT TO BE MOVED TO STRINGPROC (HOME OF OTHER SUCH FUNCTIONS) !!

(defun $openw_binary (file)
   (open
      #+sbcl (sb-ext:native-namestring file)
      #-sbcl file
      :direction :output
      :if-exists :supersede
      :element-type '(unsigned-byte 8)
      :if-does-not-exist :create))

(defun $opena_binary (file)
   (open
      #+sbcl (sb-ext:native-namestring file)
      #-sbcl file
      :direction :output
      :if-exists :append
      :element-type '(unsigned-byte 8)
      :if-does-not-exist :create))

(defun $openr_binary (file) (open
			     #+sbcl (sb-ext:native-namestring file)
			     #-sbcl file
			     :element-type '(unsigned-byte 8)))

;; -------------------- read functions --------------------

;; ---- functions to read a matrix

(defun $read_matrix (stream-or-filename &rest args)
  (if ($matrixp (car args))
    (let*
      ((M (car args))
       (sep-ch-flag (cadr args))
       (nrow (length (cdr M)))
       (ncol (if (> nrow 0) (length (cdadr M)) 0))
       (L ($read_list stream-or-filename sep-ch-flag (* nrow ncol))))
      ;; COPYING DATA HERE !!
      (fill-matrix-from-list L M nrow ncol))
    (let*
      ((sep-ch-flag (car args))
       (rows-list (cdr ($read_nested_list stream-or-filename sep-ch-flag)))
       (rows-list-nonempty (remove-if #'(lambda (x) (= ($length x) 0)) rows-list)))
      `(($matrix) ,@rows-list-nonempty))))

(defun $read_binary_matrix (stream-or-filename M)
  (if ($matrixp M)
    (let*
      ((nrow (length (cdr M)))
       (ncol (if (> nrow 0) (length (cdadr M)) 0))
       (L ($read_binary_list stream-or-filename (* nrow ncol))))
      ;; COPYING DATA HERE !!
      (fill-matrix-from-list L M nrow ncol))
    (merror "read_binary_matrix: expected a matrix, found ~a instead" (type-of M))))

(defun fill-matrix-from-list (L M nrow ncol)
  (let ((k 0))
    (dotimes (i nrow)
      (let ((row (nth (1+ i) M)))
        (dotimes (j ncol)
          (setf (nth (1+ j) row) (nth (1+ k) L))
          (setq k (1+ k))))))
  M)

;; ---- functions to read a Lisp array or Maxima declared array

(defun $read_array (stream-or-filename &rest args)
  (if (and args (lisp-or-declared-maxima-array-p (car args)))
    (let
      ((A (car args))
       (sep-ch-flag (and (cdr args) (cadr args)))
       (mode 'text))
      (read-into-existing-array stream-or-filename A sep-ch-flag mode))
    (let
      ((sep-ch-flag (and args (car args)))
       (mode 'text))
      (read-and-return-new-array stream-or-filename sep-ch-flag mode))))

(defun $read_binary_array (file-name &rest args)
  (if (car args)
    (read-into-existing-array file-name (car args) nil 'binary)
    (read-and-return-new-array file-name nil 'binary)))

(defun read-into-existing-array (file-name A sep-ch-flag mode)
  (if (not (arrayp A))
    (setq A (get (mget A 'array) 'array)))
  (let*
    ((dimensions (array-dimensions A))
     (n (apply #'* dimensions)))
    (read-into-existing-array-size-known file-name A sep-ch-flag mode n)
    '$done))

(defun read-into-existing-array-size-known (stream-or-filename A sep-ch-flag mode n)
  (if (streamp stream-or-filename)
    (read-into-existing-array-size-known-from-stream stream-or-filename A sep-ch-flag mode n)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file
        (in 
            #+sbcl (sb-ext:native-namestring file-name)
            #-sbcl file-name
            :if-does-not-exist nil
            :element-type (if (eq mode 'text) 'character '(unsigned-byte 8)))
        (if (not (null in))
          (read-into-existing-array-size-known-from-stream in A sep-ch-flag mode n)
          (merror "read_array: no such file `~a'" file-name))))))

(defun read-into-existing-array-size-known-from-stream (in A sep-ch-flag mode n)
  (let (x (sep-ch (get-input-sep-ch sep-ch-flag in)))
    (if (eq mode 'text) (reset-for-parse-next-element))
    (dotimes (i n)
      (if (eq (setq x (if (eq mode 'text) (parse-next-element in sep-ch) (read-float-64 in))) 'eof)
        (return A))
      (setf (row-major-aref A i) x))))

(defun read-into-existing-array-size-unknown-from-stream (in A sep-ch mode)
  (let (x)
    (if (eq mode 'text) (reset-for-parse-next-element))
    (loop
      (if (eq (setq x (if (eq mode 'text) (parse-next-element in sep-ch) (read-float-64 in))) 'eof)
        (return A))
      (vector-push-extend x A))))

(defun read-and-return-new-array (stream-or-filename sep-ch-flag mode)
  (if (streamp stream-or-filename)
    (read-and-return-new-array-from-stream stream-or-filename sep-ch-flag mode)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file
        (in
            #+sbcl (sb-ext:native-namestring file-name)
            #-sbcl file-name
            :if-does-not-exist nil
            :element-type (if (eq mode 'text) 'character '(unsigned-byte 8)))
        (if (not (null in))
          (read-and-return-new-array-from-stream in sep-ch-flag mode)
          (merror "read_array: no such file `~a'" file-name))))))

(defun read-and-return-new-array-from-stream (in sep-ch-flag mode)
  (let ((A (make-array 0 :adjustable t :fill-pointer t))
        (sep-ch (if (eq mode 'text) (get-input-sep-ch sep-ch-flag in))))
    (read-into-existing-array-size-unknown-from-stream in A sep-ch mode)))

;; ---- functions to read a Maxima undeclared array

(defun $read_hashed_array (stream-or-filename A &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-hashed-array-from-stream stream-or-filename A sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in
		       #+sbcl (sb-ext:native-namestring file-name)
		       #-sbcl file-name
		       :if-does-not-exist nil)
        (if (not (null in))
          (read-hashed-array-from-stream in A sep-ch-flag)
          (merror "read_hashed_array no such file `~a'" file-name))))))

(defun read-hashed-array-from-stream (in A sep-ch-flag)
  (let (key L (sep-ch (get-input-sep-ch sep-ch-flag in)))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof) (return))
      (setq L (make-mlist-from-string L sep-ch))
      (cond
        ((> ($length L) 0)
         (setq key ($first L))
         (if (= ($length L) 1)
           (arrstore (list (list A 'array) key) nil)
           (arrstore (list (list A 'array) key) ($rest L)))))))
  A)

;; ---- functions to read a list or nested list

(defun $read_nested_list (stream-or-filename &optional sep-ch-flag)
  (if (streamp stream-or-filename)
    (read-nested-list-from-stream stream-or-filename sep-ch-flag)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file (in
		       #+sbcl (sb-ext:native-namestring file-name)
		       #-sbcl file-name
		       :if-does-not-exist nil)
        (if (not (null in))
          (read-nested-list-from-stream in sep-ch-flag)
          (merror "read_nested_list: no such file `~a'" file-name))))))

(defun read-nested-list-from-stream (in sep-ch-flag)
  (let (A L (sep-ch (get-input-sep-ch sep-ch-flag in)))
    (loop
      (setq L (read-line in nil 'eof))
      (if (eq L 'eof)
        (return (cons '(mlist simp) (nreverse A))))
      (setq A (cons (make-mlist-from-string L sep-ch) A)))))

(defun $read_list (stream-or-filename &rest args)
  (if ($listp (car args))
    (let*
      ((L (car args))
       (sep-ch-flag (cadr args))
       (n (or (caddr args) ($length L))))
      (read-into-existing-list stream-or-filename L sep-ch-flag 'text n))
    (if (integerp (car args))
      (let ((n (car args)))
        (read-list stream-or-filename nil 'text n))
      (let ((sep-ch-flag (car args)) (n (cadr args)))
        (read-list stream-or-filename sep-ch-flag 'text n)))))

(defun read-into-existing-list (stream-or-filename L sep-ch-flag mode n)
  (if (streamp stream-or-filename)
    (read-into-existing-list-from-stream stream-or-filename L sep-ch-flag mode n)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file
        (in 
            #+sbcl (sb-ext:native-namestring file-name)
            #-sbcl file-name
            :if-does-not-exist nil
            :element-type (if (eq mode 'text) 'character '(unsigned-byte 8)))
        (if (not (null in))
          (read-into-existing-list-from-stream in L sep-ch-flag mode n)
          (merror "read_list: no such file `~a'" file-name))))))

(defun read-into-existing-list-from-stream (in L sep-ch-flag mode n)
  (let (x (sep-ch (if (eq mode 'text) (get-input-sep-ch sep-ch-flag in))))
    (if (eq mode 'text) (reset-for-parse-next-element))
    (dotimes (i n)
      (if (eq (setq x (if (eq mode 'text) (parse-next-element in sep-ch) (read-float-64 in))) 'eof)
        (return))
      (setf (nth (1+ i) L) x))
    L))

(defun read-list (stream-or-filename sep-ch-flag mode n)
  (if (streamp stream-or-filename)
    (read-list-from-stream stream-or-filename sep-ch-flag mode n)
    (let ((file-name (require-string stream-or-filename)))
      (with-open-file
       (in
            #+sbcl (sb-ext:native-namestring file-name)
            #-sbcl file-name
            :if-does-not-exist nil
            :element-type (if (eq mode 'text) 'character '(unsigned-byte 8)))
        (if (not (null in))
          (read-list-from-stream in sep-ch-flag mode n)
          (merror "read_list: no such file `~a'" file-name))))))

(defun read-list-from-stream (in sep-ch-flag mode n)
  (let (A x (sep-ch (if (eq mode 'text) (get-input-sep-ch sep-ch-flag in))))
    (if (eq mode 'text) (reset-for-parse-next-element))
    (loop
      (if
        (or
          (and n (eql n 0))
          (eq (setq x (if (eq mode 'text) (parse-next-element in sep-ch) (read-float-64 in)))
              'eof))
        (return (cons '(mlist simp) (nreverse A))))
      (setq A (nconc (list x) A))
      (if n (decf n)))))

(defun $read_binary_list (stream-or-filename &rest args)
  (if ($listp (car args))
    (let*
      ((L (car args))
       (n (or (cadr args) ($length L))))
      (read-into-existing-list stream-or-filename L nil 'binary n))
    (let ((n (car args)))
      (read-list stream-or-filename nil 'binary n))))

(defun make-mlist-from-string (s sep-ch)
  ; scan-one-token-g isn't happy with symbol at end of string.
  (setq s (concatenate 'string s " "))

  (with-input-from-string (*parse-stream* s)
    (let ((token) (L) (LL) (sign) (found-token) (found-sep))
      (loop
        (setq token (scan-one-token-g t 'eof))
        (cond
          ((eq token 'eof)
           (cond
             ((not (null sign))
              (format t "numericalio: trailing sign (~S) at end of line; strange, but just eat it.~%" sign)))
           (cond
             ((eql sep-ch #\space)
              (return (cons '(mlist) LL)))
             (t
               (if (or found-token found-sep)
                 (return (cons '(mlist) (appropriate-append L LL)))
                 ;; We reached EOF without encountering a token or a separator;
                 ;; this is an empty line.
                 (return '((mlist))))))))
        (cond
          ((or (eq token '$-) (eq token '$+))
           (setq sign (cond ((eq token '$-) -1) (t 1))))
          (t
            (cond
              ((not (null sign))
               (setq token (m* sign token))
               (setq sign nil)))
            (cond
              ((eql sep-ch #\space)
               (setq found-token token)
               (setq LL (append LL (list token))))
              (t
                (cond
                  ((eql token sep-ch)
                   (setq found-sep token)
                   (setq L (appropriate-append L LL))
                   (setq LL nil))
                  (t
                    (setq found-token token)
                    (setq LL (append LL (list token)))))))))))))

(defun appropriate-append (L LL)
  (cond
    ((null LL) (append L '(nil)))
    ((= (length LL) 1) (append L LL))
    (t (append L (list (append '((mlist)) LL))))))

;; ----- begin backwards compatibility stuff ... sigh -----
(defun $read_lisp_array (file-name A &optional sep-ch-flag)
  ($read_array file-name A sep-ch-flag))

(defun $read_maxima_array (file-name A &optional sep-ch-flag)
  ($read_array file-name A sep-ch-flag))
;; -----  end backwards compatibility stuff ... sigh  -----

;; ---- read one element

(defvar newline-symbol (intern (coerce '(#\$ #\newline) 'string)))
(defvar whitespace-sans-newline (remove #\newline *whitespace-chars*))

(let (prev-token-sep-ch sign start-of-line)

  (defun reset-for-parse-next-element ()
    (setq prev-token-sep-ch nil)
    (setq sign 1)
    (setq start-of-line t))

  (defun parse-next-element (in sep-ch)
    (let
      ((*parse-stream* in)
       ;; Treat newline as a token, so leading/trailing separators can be detected,
       ;; when separator is anything other than a space.
       (*whitespace-chars* (if (eql sep-ch #\space) *whitespace-chars* whitespace-sans-newline))
       token)
      (loop
        (setq token (scan-one-token-g t 'eof))
        (cond
          ((eq token newline-symbol)
           (setq start-of-line t)
           (when prev-token-sep-ch
             (setq prev-token-sep-ch nil)
             (return nil)))
          ((eq token 'eof)
           (if prev-token-sep-ch
             (progn
               (setq prev-token-sep-ch nil)
               (return nil))
             (return 'eof)))
          ((and (eql token sep-ch) (not (eql sep-ch #\space))) ;; TEST FOR #\SPACE IS REDUNDANT
           ;; We have a separator token.
           ;; If the preceding token was also a separator,
           ;; or we're at the start of a line, return NIL.
           (if (or prev-token-sep-ch start-of-line)
             (progn
               (setq start-of-line nil)
               (return nil))
             (setq prev-token-sep-ch token)))
          ((prog nil (setq start-of-line nil)))
          ((prog nil (setq prev-token-sep-ch nil)))
          ((member token '($- $+))
           (setq sign (* sign (if (eq token '$-) -1 1))))
          (t
            (let ((return-value (m* sign token)))
              (setq sign 1)
              (return return-value))))))))


;; -------------------- write functions -------------------

(defun open-file-appropriately (file-name mode)
  (open
        #+sbcl (sb-ext:native-namestring file-name)
        #-sbcl file-name
        :direction :output
        :element-type (if (eq mode 'text) 'character '(unsigned-byte 8))
        :if-exists (if (or (eq $file_output_append '$true) (eq $file_output_append t)) :append :supersede)
        :if-does-not-exist :create))

(defun $write_data (X stream-or-filename &optional sep-ch-flag)
  (write-data X stream-or-filename sep-ch-flag 'text))

(defun $write_binary_data (X stream-or-filename)
  (write-data X stream-or-filename nil 'binary))

(defun write-data (X stream-or-filename sep-ch-flag mode)
  (let
    ((out
       (if (streamp stream-or-filename)
         stream-or-filename
         (open-file-appropriately (require-string stream-or-filename) mode))))
    (cond
      (($matrixp X)
        (write-matrix X out sep-ch-flag mode))
      ((arrayp X)
        (write-lisp-array X out sep-ch-flag mode))
      ((mget X 'array)
        (write-maxima-array X out sep-ch-flag mode))
      ((mget X 'hashar)
        (write-hashed-array X out sep-ch-flag mode))
      (($listp X)
        (write-list X out sep-ch-flag mode))
      (t (merror "write_data: don't know what to do with a ~M" (type-of X))))
    (if (streamp stream-or-filename)
      (finish-output out)
      (close out))
    '$done))

(defun write-matrix (M out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag out)))
    (mapcar #'(lambda (x) (write-list-lowlevel (cdr x) out sep-ch mode)) (cdr M))))

(defun write-lisp-array (A out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag out)) (d (array-dimensions A)))
    (write-lisp-array-helper A d '() out sep-ch mode)))

(defun write-lisp-array-helper (A d indices out sep-ch mode)
  (cond ((equalp (length d) 1)
      (let ((L '()))
        (loop for i from 0 to (- (car d) 1) do
          (let ((x (apply 'aref (append (list A) (reverse (cons i indices))))))
            (setq L (cons x L))))
        (write-list-lowlevel (reverse L) out sep-ch mode)))
    (t
      (loop for i from 0 to (- (car d) 1) do
        (write-lisp-array-helper A (cdr d) (cons i indices) out sep-ch mode)
        (if (and (eq mode 'text) (> (length d) 2))
          (terpri out))))))

(defun write-maxima-array (A out sep-ch-flag mode)
  (write-lisp-array (symbol-array (mget A 'array)) out sep-ch-flag mode))

(defun write-hashed-array (A out sep-ch-flag mode)
  (let
    ((keys (cdddr (meval (list '($arrayinfo) A))))
     (sep-ch (get-output-sep-ch sep-ch-flag out))
     L)
    (loop
      (if (not keys) (return))
      (setq L ($arrayapply A (car keys)))
      (cond ((listp L) (pop L))
            (t (setq L (list L))))
      (write-list-lowlevel (append (cdr (pop keys)) L) out sep-ch mode))))

(defun write-list (L out sep-ch-flag mode)
  (let ((sep-ch (get-output-sep-ch sep-ch-flag out)))
    (write-list-lowlevel (cdr L) out sep-ch mode)))

(defun write-list-lowlevel (L out sep-ch mode)
  (setq sep-ch (cond ((symbolp sep-ch) (cadr (exploden sep-ch))) (t sep-ch)))
  (cond
    ((null L) (terpri out))
    (t
      (loop 
        (if (not L) (return))
        (let ((e (pop L)))
          (cond (($listp e)
              (write-list-lowlevel (cdr e) out sep-ch mode))
            (t
              (cond
                ((eq mode 'text)
                 (let
                   (($lispdisp t))
                   (declare (special $lispdisp))
                   (mgrind e out))
                 (cond
                   ((null L) (terpri out))
                   (t (write-char sep-ch out))))
                ((eq mode 'binary)
                 (if ($numberp e)
                   (write-float ($float e) out)
                   (merror "write_data: encountered non-numeric data in binary output")))
                (t
                  (merror "write_data: unrecognized mode"))))))))))

(defun get-input-sep-ch (sep-ch-flag my-stream)
  (cond
    ((eq sep-ch-flag '$tab)
     (format t "numericalio: separator flag ``tab'' not recognized for input; assume ``space'' instead.~%")
     #\space)
    (t (get-output-sep-ch sep-ch-flag my-stream))))

(defun get-output-sep-ch (sep-ch-flag my-stream)
  (cond
    ((eq sep-ch-flag '$space) #\space)
    ((eq sep-ch-flag '$tab) #\tab)
    ((or (eq sep-ch-flag '$comma) (eq sep-ch-flag '$csv)) '$\,) ; '$csv is backwards compatibility ... sigh
    ((eq sep-ch-flag '$pipe) '$\|)
    ((eq sep-ch-flag '$semicolon) '$\;)

    ((stringp sep-ch-flag)
     (if (/= (length sep-ch-flag) 1)
       (progn (format t "numericalio: unrecognized separator; assume ``space''.~%")
              #\space)
       (let ((would-be-sep-ch (aref sep-ch-flag 0)))
         (cond
           ((eq would-be-sep-ch #\space) #\space)
           ((eq would-be-sep-ch #\tab) #\tab)
           ((eq would-be-sep-ch #\,) '$\,)
           ((eq would-be-sep-ch #\|) '$\|)
           ((eq would-be-sep-ch #\;) '$\;)
           (t
             (format t "numericalio: separator flag ~S not recognized; assume ``space''.~%" would-be-sep-ch)
             #\space)))))
    ((null sep-ch-flag)
      (cond
        ((ignore-errors (equal (pathname-type (truename my-stream)) "csv"))
         '$\,)
        (t #\space)))
    (t
      (format t "numericalio: separator flag ~S not recognized; assume ``space''.~%" (stripdollar sep-ch-flag))
      #\space)))

(defun require-string (s)
  (cond
    ((stringp s)
     s)
    (t
      (merror "numericalio: expected a string, instead found a ~:M" (type-of s)))))
