
(load "../gcl-tk/convert.el")
;(let ((i 2000)) (while (> i 0) (do-one) (setq i (- i 1))))

(defun get-match (i) (buffer-substring (match-beginning i) (match-end i)))
(defun list-matches (l)
  (let (ans)
  (while l
    (setq ans (cons (get-match (car l)) ans)))
  (nreverse ans)))
(defun do-one ()
  (interactive)
  
  ()
  (beginning-of-line)
  (re-search-forward "" nil t)
  (let ((beg (point))
	def
	(end (save-excursion   (re-search-forward "" nil t) (point))))
    (cond ((looking-at "F\\([^\n]+\\)\n\\([^\n]+\\) in \\([A-Z_a-z]+\\) package[:]?[\n ]\\(Args\\|Syntax\\): ")
	   (let ((fun (get-match 1))
		 (type (get-match 2))
		 (package (get-match 3))
		 args body)
	     (goto-char (match-end 0))
	     (cond ((equal (get-match 4) "Syntax")
		    (setq args "")
		    (beginning-of-line))
		   (t
		     (setq args
		       (progn (let ((beg (point)))
				(forward-sexp 1)
				(buffer-substring beg (point)))))))
	     
	     (setq body (buffer-substring (point) (- end 1)))
	     (delete-region beg end )
	     (save-excursion
	       (get-buffer-create package)
	       (set-buffer package)
	       (goto-char (point-max))
	       (insert
		(if (equal type "Function")
		    (setq def "@defun")
		  (concat (setq def "@deffn") " {" type "}"))
		" "
		 fun " " args "\nPackage:" package "\n"
		       body)
	       (insert "\n@end " (substring def 1) "\n")
	       )))
	  ((looking-at "V\\([^\n]+\\)\n\\([^\n]+\\) in \\([A-Z_a-z]+\\) package:\n")
	   (let ((fun (get-match 1))
		 (type (get-match 2))
		 (package (get-match 3))
		 args body)
	     (goto-char (match-end 0))
	     (setq body (buffer-substring (point) (- end 1)))
	     (delete-region beg end )
	     (save-excursion
	       (get-buffer-create package)
	       (set-buffer package)
	       (goto-char (point-max))
	       (insert (if (string-match "^\\*" fun)
			   (setq def "@defvar")
			 (concat (setq def "@defvr")" {Constant}"))
		       " "
		       fun " " "\nPackage:" package "\n"
		       body )
	       (insert "\n@end " (substring def 1) "\n")))))))
	       

(defun do-some ()
  (interactive)
  (while (re-search-forward "{Constant}" nil t)
    (let* ((tem (read-char ))
	   (u
	    (cdr (assoc  tem
			'((?s . "{Special Variable}")
			  (?d .  "{Declaration}"))))))
      (if u (replace-match u)))))

(setq b-alist '((?n . "number.texi")
		(?s . "sequence.texi")
		(?c . "character.texi")
		(?l . "list.texi")
		(?i . "io.texi")
		(?a . "internal.texi")
		(?f . "form.texi")
		(?C . "compile.texi")
		(?S . "symbol.texi")
		(?t . "system.texi")
		(?d . "structure.texi")
		(?I . "iteration.texi")
		(?u . "user-interface.texi")
		(?d . "doc.texi")
		(?b . "type.texi")
		))
(defun try1 ()
  (interactive)
  (while (re-search-forward "\n@def" nil t)
    (let ((beg (match-beginning 0)) me tem
	  (end (save-excursion (re-search-forward "\n@end def[a-z]+" nil t)
			       (point))))
      (sit-for 0 300)
      (setq tem (read-char ))
      (cond ((setq tem (cdr (assoc tem b-alist)))
	     (setq  me (buffer-substring beg end))
	     (delete-region beg end)
	     (forward-char -2)
	     (save-excursion
	       (get-buffer-create tem)
	       (set-buffer tem)
	       (goto-char (point-max))
	       (insert me "\n")))))))
      

      
(setq xall	  (mapcar 'cdr  b-alist))

;(let ((all xall)) (while all (set-buffer (car all))  (write-file (car all)) (setq all (cdr all))))
;(let ((all xall)) (while all   (find-file (car all)) (setq all (cdr all))))	
(let ((all xall) x) (while all (set-buffer (car all)) (goto-char (point-min)) (insert "@node " (setq x (capitalize (car all))) "\n@chapter "x"\n")  (write-file (car all)) (set-buffer "gcl-si.texi")(goto-char (point-max)) (insert "\\n@include " (car all) "\n") (setq all (cdr all))))
	  
	   
(let ((all xall) x) (while all (switch-to-buffer (car all)) (goto-char (point-min)) (insert "@node " (setq x (capitalize (car all))) "\n@chapter "x"\n")  (save-buffer) (set-buffer "gcl-si.texi")(goto-char (point-max)) (insert "\\n@include " (car all) "\n") (setq all (cdr all))))	
    
(let ((all xall) x) (while all (switch-to-buffer (car all)) (goto-char (point-min)) (insert "@node " (setq x (capitalize (car all))) "\n@chapter "x"\n")  (save-buffer) (set-buffer "gcl-si.texi")(goto-char (point-max)) (insert "\\n@include " (car all) "\n") (setq all (cdr all))))	
