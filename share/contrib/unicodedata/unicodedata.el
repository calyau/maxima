;; -*- Mode: emacs-lisp; lexical-binding: t -*- ;;
;; Copyright Leo Butler (leo.butler@member.fsf.org) 2015
;; Released under terms of GPLv3+
;;
;; Generate the unicodedata-txt.lisp file from UnicodeData.txt
;;
;; Batch mode:
;; emacs -nw -q --eval "(progn (byte-compile-file \"unicodedata.el\" t) (unicode-init) (unicode-print-lisp))" 
;;

(defun unicode->list (file)
  (interactive "sFile? ")
  (cl-labels ((string->number (x)
			      (string-to-number x 16))
	      (char (n)
		    (if (characterp n)
			(format "%c" n)
		      n))
	      (assemble-list (x)
			     (let ((n (string->number (car x))))
			       (cons (car x) (cons n (cons (char n) (cdr x)))))))
    (let ((u (save-excursion
	       (or (get-buffer file) (find-file-literally file))
	       (buffer-substring-no-properties (point-min) (point-max)))))
      (mapcar #'assemble-list
	      (mapcar (lambda(s) (split-string s ";" nil nil))
		      (split-string u "\n" t))))))

(defun unicode-hashtable->list (ht)
  (let (l)
    (maphash #'(lambda(k v)
		 (push (list k v) l))
	     ht)
    l))

(eval-when (compile load)
  (defvar unicode-code-value-hex)
  (defvar unicode-code-value)
  (defvar unicode-code-character)
  (defvar unicode-character-name)
  (defvar unicode-general-category))

(defun unicode-find (predicate)
  "Creates a function that applies PREDICATE(K V) to the key K and value V of each element
in a hashtable HT. Analogous to CL-REMOVE-IF-NOT."
  (let ((this `(lambda (ht)
		 (let ((oht (make-hash-table :test #'eq)))
		   (maphash #'(lambda (k v)
				(if (funcall #',predicate k v)
				    (puthash k v oht))) ht)
		   oht))))
    (cl-coerce this 'function)))

(defun unicode->hashtable (file)
  (interactive "sFile? ")
  (let ((ht (make-hash-table :size 29215 :test #'equal))
	(l (unicode->list file)))
    (cl-loop for c in l
	     for h = (funcall unicode-code-value-hex c)
	     for v = (funcall unicode-code-value c)
	     for ch = (funcall unicode-character-name c)
	     for g = (funcall unicode-general-category c)
	     for gc = (gethash g ht)
	     do
	     (puthash h c ht)
	     (puthash v c ht)
	     (puthash ch c ht)
	     (puthash g (push c gc) ht))
    ht))

(eval-when (compile load)
  (defvar unicode-data-hashtable )	;; (unicode->hashtable unicode-data-txt-buffer))
  (defvar unicode-data-list )		;; (unicode->list unicode-data-txt-buffer))
  (defvar unicode-data-txt-url "http://www.unicode.org/Public/UNIDATA/UnicodeData.txt"
    "URL of UnicodeData.txt file.")
  (defvar unicode-data-txt-buffer "UnicodeData.txt" "Name of buffer created by UNICODEDATA.TXT.GET."))

(defun unicodedata.txt.get (&optional url proceed further-callbacks)
  (interactive "sURL? ")
  (setq url (or url unicode-data-txt-url))
  (let* ((transform (lambda (buffer)
		      (with-current-buffer buffer
			(setq unicode-data-hashtable (unicode->hashtable (buffer-name)))
			(message "Done processing %s." unicode-data-txt-buffer))))
	 (callback (lambda (status)
		     (unless status
		       (search-forward-regexp "^0000;" (point-max))
		       (forward-line -1)
		       (kill-whole-line (- (1+ (line-number-at-pos))))
		       (kill-line)
		       (rename-buffer unicode-data-txt-buffer)
		       (funcall transform unicode-data-txt-buffer)
		       (cl-loop for f in further-callbacks do (funcall f))))))
    (cond ((find-file-literally unicode-data-txt-buffer)
	   (funcall transform unicode-data-txt-buffer))
	  ((or proceed (y-or-n-p "Retrieve UNICODE DATA online? "))
	   (url-retrieve url callback))
	  (t
	   (message "Stopped.")))))

(defun unicode-search-character-name (regex &optional ht)
  (interactive)
  (let ((ht (or ht unicode-data-hashtable))
	(l))
    (cl-loop for k being the hash-keys of ht
	     if (and (stringp k) (string-match regex k))
	     do (push (gethash k ht) l))
    l))
(defun unicode-search-value (n &optional ht)
  (interactive)
  (let ((ht (or ht unicode-data-hashtable)))
    (gethash n ht)))
(defun unicode-search-value-hex (n &optional ht)
  (interactive)
  (unicode-search-value (string-to-number n 16) ht))
(defun unicode-search-category (c &optional ht)
  (interactive)
  (let ((ht (or ht unicode-data-hashtable)))
    (gethash c ht)))

;; see http://www.unicode.org/Public//3.0-Update1/UnicodeData-3.0.1.html#General%20Category
(defun unicode-math-characters ()
  (interactive)
  (let ((ht (make-hash-table :size 8000 :test #'eq))
	(l (append (unicode-search-category "Sm") ;; math symbol
		   (unicode-search-category "Mn") ;; non-spacing mark
		   ;;(unicode-search-category "Me") ;; enclosing mark
		   (unicode-search-category "No") ;; number other
		   (unicode-search-category "Nl") ;; number letter
		   (unicode-search-category "Nd") ;; number decimal digit
		   (unicode-search-category "Pc") ;; number decimal digit
		   (unicode-search-category "Pc") ;; Punctuation, Connector
		   (unicode-search-category "Pd") ;; Punctuation, Dash
		   (unicode-search-category "Ps") ;; Punctuation, Open
		   (unicode-search-category "Pe") ;; Punctuation, Close
		   (unicode-search-category "Pi") ;; Punctuation, Initial quote (may behave like Ps or Pe depending on usage)
		   (unicode-search-category "Pf") ;; Punctuation, Final quote (may behave like Ps or Pe depending on usage)
		   ;;(unicode-search-category "Po") ;; Punctuation, Other
		   (unicode-search-character-name (regexp-opt '("APL FUNCTIONAL SYMBOL" "MATHEMATICAL" "GREEK" "LATIN"))))))
    (cl-loop for i in l
	     do (puthash (funcall unicode-code-value i) i ht))
    ht))

(eval-when (compile load)
  (defvar unicode-math-characters nil) ;; (unicode-math-characters))
  (defvar unicode-data-output-file "unicodedata-txt.lisp"))

(defun unicode-print-lisp (&optional data-file ht)
  (interactive)
  (setq ht (or ht (unicode-math-characters))
	data-file (or data-file unicode-data-output-file))
  (if (get-buffer data-file) (kill-buffer data-file))
  (if (file-exists-p data-file) (delete-file data-file))
  (let (s)
  (with-temp-buffer
    (let* ((standard-output (current-buffer))
	   (coding-system-for-read 'utf-8-unix)
	   (coding-system-for-write 'utf-8-unix)
	   (print-as-symbol (lambda (x) (if (equal "" x) (format " nil") (format " |%s|" x))))
	   (print-as-hex (lambda (x)  (if (equal "" x) (format " nil") (progn (with-output-to-string (princ " #x")(princ x))))))
	   (print-car (lambda (x)
			(princ "'(#x")(princ (car x))))
	   (print-rest (lambda (x)
			 (insert (concat
				  (funcall print-as-symbol (nth 2 x))
				  (funcall print-as-symbol (nth 3 x))
				  (funcall print-as-symbol (nth 4 x))
				  (funcall print-as-symbol (nth 5 x))
				  (funcall print-as-symbol (nth 6 x))
				  (funcall print-as-symbol (nth 7 x))
				  (funcall print-as-symbol (nth 8 x))
				  (funcall print-as-symbol (nth 9 x))
				  (funcall print-as-symbol (nth 10 x))
				  (funcall print-as-symbol (nth 11 x))
				  (funcall print-as-symbol (nth 12 x))
				  (funcall print-as-symbol (nth 13 x))
				  (funcall print-as-hex (nth 14 x))
				  (funcall print-as-hex (nth 15 x))
				  ")\n"))))
	   (printer (lambda (x)
		      (funcall print-car x) (funcall print-rest x))))
      (insert ";; -*- mode:lisp; coding: utf-8 -*-\n")
      (insert (format ";; Derived from %s\n#.(list\n" unicode-data-txt-url))
      (mapc printer (cl-loop for k being the hash-keys of ht
			     for v = (gethash k ht)
			     if (and (numberp k) (> k 127))
			     collect v))
      (insert ")\n")
      (setq s (buffer-substring-no-properties (point-min) (point-max)))))
  (with-current-buffer (find-file-literally data-file)
    (insert (encode-coding-string s 'utf-8-unix))
    (basic-save-buffer)
    (kill-buffer))))

(defun unicode-init ()
  (interactive)
  (mapc #'(lambda(x) (set (car x) `(lambda (l) (nth ,(cadr x) l))))
	'((unicode-code-value-hex 0)
	  (unicode-code-value 1)
	  (unicode-code-character 2)
	  (unicode-character-name 3) 
	  (unicode-general-category 4)))
  (unicodedata.txt.get nil t)
  (unicode->hashtable unicode-data-txt-buffer)
  )

;; end of unicodedata.el ;;
