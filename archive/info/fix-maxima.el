;(find-file "/softs/maxima/doc/DOC")
;(goto-char 15644)

(setq chaps
      '(("Lists")
    ("Differentiation")
    ("Integration")
    ("Polynomials")
    ("Simplification")
    ("Expressions")
    ("Function Definition")
    ("Input and Output")
    ("Program Flow")
    ("Plotting")
    ("NonCommutative")
    ("Floating Point")
    ("Numerical")
    ("Trigonometric")
    ("Equations")
    ("Arrays and Tables")
    ("Series")

    ("Symmetries")
    ("Groups")
    ("Operators")
    ("Constants")
    ("Special Functions")
    ("Command Line")
    ("Contexts")
    ("Matrices and Linear Algebra")
    ("Runtime Environment")
    ("Miscellaneous Options")
    ("Rules and Patterns")
    ("Number Theory")
    ("Help")
    ("Differential Equations")
    ("xrefs")
    ("Statistics")
    ("Limits")
    ("Logarithms")
    ("Debugging")
    ("Affine")
    ("Tensor")
    ("Ctensor")
    ("None")
    ))

(defvar chapter-selected nil)
(defvar doc-dir "/home/wfs/max-doc")
(defun name-of-file (tem) (substring  tem 0 (string-match " " tem)))
(defun get-chapter (prompt &optional type)
  (let ((tem  (completing-read prompt chaps nil t)))
    (setq chapter-selected tem)
    (switch-to-buffer doc-buffer)
    (goto-char begin-expr)
    (other-window 1)
    (find-file
     (concat doc-dir "/" (name-of-file tem) ".texi"))
    (goto-char (point-min))
    (cond ((> (point-max) 5)
	   (or (search-forward "@c end concepts" nil t)
	       (progn
		 (search-forward "@chapter")
		 (forward-line 1)
		  (insert "@c end concepts "tem "\n")))))
    (goto-char (point-max))
    (cond ((< (point) 5)
	   (insert "@node "tem "\n"
		   "@chapter "tem "\n"
		   "@c end concepts "tem "\n"
		   )))
    (cond ((equal type "section")
	   (goto-char (point-min))
	   (search-forward "@c end concepts" )
	   (beginning-of-line)
	   (insert "\n")
	   (forward-line -1)

	   ))
    ))


(defun do-maxima-texi()
  (interactive)
  (find-file "maxima.texi")
  (goto-char (point-min))
  (search-forward "@c includes")
  (delete-region (point)  (progn (search-forward "end includes")
				 (point)))
  (insert "\n")
  (let ((tem chaps))
    (while tem
      (insert "\n@include " (name-of-file (car (car tem))) ".texi\n")
      (setq tem (cdr tem)))
    (insert "\n@c end includes\n"))
  (save-buffer)
  )
  

(defvar begin-expr nil)
(defvar end-expr nil)

(defun rest-of-it ()
  (save-excursion
    (buffer-substring (point)
		      (progn (search-forward "\n" nil t)
			     (forward-char -2)
			     (setq end-expr (point))))))
(defvar doc-buffer nil)

(defvar first-time nil)
(defun doit()
  (interactive)
  (setq doc-buffer (current-buffer))
  (let ((completion-ignore-case t))
  (while (search-forward "\n")
    (setq begin-expr (point))
;    (sit-for 0 2)
    (recenter 1)
;    (sit-for 1 2)
    
    (cond ((looking-at "[A-Z0-9_a-z%]+(")
	   (let* ((fun (buffer-substring (point) (- (match-end 0) 1)))
		  (arg-begin (- (match-end 0) 1))
		  (args (buffer-substring arg-begin
					  (progn (forward-sexp 1)
						 (point))))
		  (description (rest-of-it)))
	     (progn
	       (get-chapter (concat "Fun: " fun  " args: " args ": "))
	       (insert "@defun " fun " " args "\n" description
		       "\n@end defun\n"))))
	  ((looking-at "[A-Z0-9_a-z%]+ default:")
	   (let* ((var (buffer-substring (point)
					 (progn (forward-sexp 1)
						(point))))
		  (description (rest-of-it)))
	     (progn
	       (get-chapter (concat "Var: " var ": "))
	       (insert "@defvar " var "\n" description
		       "\n@end defvar\n"))))

	  (t
	   (let* ((var (buffer-substring (point)
					 (progn (forward-sexp 1)
						(point))))
		  (description (rest-of-it))
		  (cat "")
		  )
	     (let* ((com (completing-read
			  (concat "@ command for `"var"': ")
					 '(("defun")
					    ("defvar")
					    ("defopt")
					    ("deffn")
					    ("defmac")
					    ("defvr")
					    ("decl")
					    ("property")
					    
					    ("section")
					    )
					 nil t
					 )))
	       
	       (progn
		 (get-chapter "chap: " com)
		 (cond ((equal com "deffn")
			(cond ((equal  chapter-selected "Operators")
			       (setq cat " operator" ))))
		       ((equal com "decl")
			(setq com "defvr")
			(setq cat " declaration"))
		       ((equal com "property")
			(setq com "defvr")
			(setq cat " property"))
		       )
		 
		 (cond ((and (equal cat "")
			     (member com '("deffn" "defvr")))
			(let ((tem(completing-read
				       (concat com " type:")
				       '(("operator")
					  ("constant")
					  ("keyword")
					  ("special operator")
					  ("special symbol")
					  ("declaration")
					  
					  )
				       nil t
				       )))
			  (cond ((string-match " " tem)
				 (setq tem (concat "{" tem "}"))))
			(setq cat
			      (concat " " tem))
				      )))
		 (insert "@" com cat
			 " "var"\n" description)
		 (or (member com '("section"))
		     (insert "\n@end " com"\n"))))))
	  )

    (other-window 1)
    (switch-to-buffer doc-buffer)
    (goto-char end-expr)
;    (sit-for 1)
    (write-region (format "(setq end-expr %d)" end-expr)
		  nil (concat doc-dir "/pos.el"))
    )))

;(setq tags-loop-operate '(fix-defun))
;(setq tags-loop-scan '(fix-defun))
;(setq tags-loop-scan '(progn (save-excursion (search-forward "@defun" nil t))))
(setq tags-loop-scan 't)
						      
				
(defun my-add-node ()
  (interactive)
  (cond ((re-search-forward "\n@def" nil t)
	 (save-excursion
	   (forward-line -1)
	   (cond ((not (looking-at "@node"))
		  (progn 	   (forward-line 1)
				   (beginning-of-line)
				   (insert "@node\n")))))
	 (forward-line 1)
					;(y-or-n-p "ok? ")
	 t
	 )))

(global-set-key "\M-]" 'example-region)
(defvar after (make-marker))
(defun example-region ()
  (interactive)
  (goto-char  (region-end))
  (insert "@end example\n")
  (set-marker after (point))
  (goto-char  (region-beginning))
  (insert "@example\n")
  (goto-char after)
  (set-marker after nil)
  
  )


(defun check-example()
  (forward-line 1)
  (let ((end (- (point-max) 5))
	(in-example nil))
    (while (and (< (point) end))
      (cond ((looking-at "@example")
	     (setq in-example t))
	    (in-example
	     (if (looking-at "@end example")
		 (setq in-example nil)))
	    ((looking-at "  ")
	     (barf))
	    (t nil))
      (forward-line 1)
      (beginning-of-line))))

    

    
  

(defun my-add-example ()
  (interactive)
  (let (beg)
    (cond ((re-search-forward "\n(C[0-9]" nil t)
	   (beginning-of-line)
	   (setq beg (point))
	   (forward-line -1)
	   (cond ((not (looking-at "@example"))
		  (goto-char beg)
		  (insert "@example\n")
		  (search-forward "@end" nil t)
		  (re-search-backward "\n([CD]" nil t)
		  (forward-line 2)
		  (while (looking-at "[ \t\n]")
		    (forward-line 1))
		  (insert "\n@end example\n")
		  )
		 (t (forward-line 2))
		 )
	   ;(recenter 1)
	   (barf)
	   ;(y-or-n-p "next")
	   ))))



(defun my-fix-node ()
  (interactive)
  (cond ((re-search-forward "\n@node" nil t)
	 (save-excursion
	   (cond ((looking-at "[ \t]*\n")
		  (let ((pt (point))
			beg end
			)
		    (forward-sexp 2)
		    (setq end (point))
		    (forward-sexp -1)
		    (setq item (buffer-substring (point) end))
		    (goto-char pt)
		    (insert " " item)))))
	 t
	 )))



(defvar my-operate-remaining-files nil  )
(defvar my-operate-current-file nil)
(defvar my-operate-function nil)


(defun my-operate-setup (lis func)
  "func returns nil when done with this file"
  (setq my-operate-current-file nil)
  (setq my-operate-function func)
  (setq my-operate-on-remaining-files lis))

(defun do-all-files (func)
  (my-operate-setup (directory-files "/home/wfs/max-doc/" t ".*[.]texi$" ) func))


(global-set-key "\M-[" 'my-operate-continue)

(do-all-files '(lambda () (while (and (search-forward "@def" nil t)
				      (not (save-excursion (forward-line -1)
						  (looking-at "@unnum"))))
			    ( beginning-of-line)
			    (insert "@unnumberedsec phony\n")
			    (forward-line 2)
			    ))) 
						
;(do-all-files 'my-add-node)
;(do-all-files 'my-fix-node)
;(do-all-files 'my-add-example)
;(texinfo-multiple-files-update "maxima.texi" t t)
;(do-all-files 'insert-section-definitions)
(do-all-files 'foo)
(defun foo ()
  (cond ((search-forward "\n@node " nil t)
	 (cond ((save-excursion (forward-line 1)
				(looking-at "@c @unnumber"))
		(beginning-of-line)
		(insert "@c ")))
	 (forward-line 1)
	 t)
	))
		
	   

(defun insert-section-definitions ()
  (interactive)
  (cond ((search-forward "@chapter " nil t )
	 (let ((chap (buffer-substring (point) (progn (end-of-line) (point)))))
	   (forward-line 1)
	   (cond ((not (looking-at "@c end concepts"))
		  (insert "@node Introduction to " chap
			  "\n@section Introduction to " chap "\n")))
	   (search-forward "@c end concepts")
	   (forward-line 1)
	   (insert "@node Definitions for " chap "\n@section Definitions for "
		   chap "\n")
	   nil))))


	  
	  
    
    
    

;(setq texinfo-section-types-regexp "section\\|unnumberedsec\\|heading")

;(setq texinfo-subsection-level-regexp "subsection\\|unnumberedsubsec\\|subheading\\|appendixsubsec")

(defun my-operate-continue ()
  (interactive)
  (while my-operate-on-remaining-files
    (cond (my-operate-current-file)
	  (t (setq my-operate-current-file (car my-operate-on-remaining-files)
		   my-operate-on-remaining-files (cdr my-operate-on-remaining-files))
	     (find-file my-operate-current-file) (goto-char (point-min))))
    (find-file my-operate-current-file)
    (message (format "operating on %s" my-operate-current-file))
    (while (funcall my-operate-function))
    
    (setq   my-operate-current-file nil)
    )
  (message "Done")
  )


(defun try ()      
  (re-search-forward "(C[0-9]" nil t)
  (beginning-of-line)
  (switch-to-buffer (current-buffer))
  (recenter 1)
  (cond ((y-or-n-p "Do this one?")
	 (insert "@example\n")
	 (progn (search-forward "@" nil t)
		(re-search-backward "([CD][0-9]" nil t)
		(forward-line 1)
		(insert "@end example\n"))))
					;(y-or-n-p "continue? ")
  )
	 
;(setq tags-loop-scan '(fix-defun))

(defun fix-defun ()
  (interactive)
 (cond ((search-forward "@defun" nil t)
       (switch-to-buffer (current-buffer))
       (recenter 1)
       (forward-sexp 1)
       (cond ((looking-at "[ ]*\n(")
	      (kill-line) (insert " ")
	      (forward-sexp 1)
	      (insert "\n")
	      (while (looking-at "[ ---]")(delete-char 1))
	      ))
       ;(barf)
       ))
  )



(defun fix-defun ()
  (interactive)
 (cond ((search-forward "@defun" nil t)
	(switch-to-buffer (current-buffer))
	(recenter 1)
	(forward-sexp 1)
	(cond ((looking-at "[ ]*\n(")
					;(barf)
	       (kill-line) (insert " ")
	       (forward-sexp 1)
	       (insert "\n")
	       (while (looking-at "[ ---]")(delete-char 1))
	       ;(barf)
	       ))
	t)
       (t nil)))

(defun fix-defun ()
  (interactive)
  (cond ((search-forward "@defun" nil t)
	 (switch-to-buffer (current-buffer))
	 (recenter 1)
	 (forward-sexp 1)
	 (cond ((looking-at "[ ]*\n ")
		(barf)
		(kill-line) (insert " ")
		(forward-sexp 1)
		(insert "\n")
		(while (looking-at "[ ---]")(delete-char 1))
					;(barf)
		))
	 t)))

	 
		      


