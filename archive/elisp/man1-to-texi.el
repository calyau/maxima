;;;;if you are in a buffer which has a man page you can try
;; M-x doit, to do an at least partial conversion of tcl tk man pages to
;; texinfo

;; file for converting the tcl/tk man pages to texinfo and suitable for gcl/tk
;          .bp     begin new page
;          .br     break output line here
;          .sp n   insert n spacing lines
;          .ls n   (line spacing) n=1 single, n=2 double space
;          .na     no alignment of right margin
;          .ce n   center next n lines
;          .ul n   underline next n lines
;          .sz +n  add n to point size
;
;  Requests
;     Request     Cause   If no     Explanation
;                 Break   Argument
;
;     .B t        no      t=n.t.l.* Text is in bold font.
;     .BI t       no      t=n.t.l.  Join words,  alternating  bold
;                 and italic.
;     .BR t       no      t=n.t.l.  Join words,  alternating  bold
;                 and roman.
;     .DT         no      .5i 1i... Restore default tabs.
;     .HP i       yes     i=p.i.*   Begin paragraph  with  hanging
;                 indent.  Set prevailing indent to i.
;     .I t        no      t=n.t.l.  Text is italic.
;     .IB t       no      t=n.t.l.  Join words, alternating italic
;                 and bold.
;
;     .IP x i     yes     x=""      Same as .TP with tag x.
;     .IR t       no      t=n.t.l.  Join words, alternating italic
;                 and roman.
;     .IX t       no      -         Index macro, for Sun  internal
;                 use.
;     .LP         yes     -         Begin left-aligned  paragraph.
;                 Set prevailing indent to .5i.
;     .PD d       no      d=.4v     Set vertical distance  between
;                 paragraphs.
;     .PP         yes     -         Same as .LP.
;     .RE         yes     -         End   of   relative    indent.
;                 Restores prevailing indent.
;     .RB t       no      t=n.t.l.  Join words, alternating  roman
;                 and bold.
;     .RI t       no      t=n.t.l.  Join words, alternating  roman
;                 and italic.
;     .RS i       yes     i=p.i.    Start     relative     indent,
;                 increase indent by i.  Sets prevailing indent to
;                 .5i                   for nested indents.
;     .SB t       no      -         Reduce  size  of  text  by   1
;                 point, make text boldface.
;     .SH t       yes     -         Section Heading.
;     .SM t       no      t=n.t.l.  Reduce  size  of  text  by   1
;                 point.
;     .SS t       yes     t=n.t.l.  Section Subheading.
;     .TH n s d f m
;                 yes     -         Begin  reference  page  n,  of
;                 section   s;   d   is   the  date  of  the  most
;                                   recent change.  If present,  f
;                 is    the   left   page   footer;   m   is   the
;                                   main  page  (center)   header.
;                 Sets prevailing indent and tabs to .5i.
;     .TP i       yes     i=p.i.    Begin indented paragraph, with
;                 the    tag    given    on    the    next    text
;                                   line.  Set  prevailing  indent
;                 to i.
;
;     .TX t p     no      -         Resolve the title abbreviation
;                 t;  join  to  punctuation  mark  (or text) p.  *
;                 n.t.l. =  next  text  line;  p.i.  =  prevailing
;                 indent
; .HS name section [date [version]]
;	Replacement for .TH in other man pages.  See below for valid
;	section names.
;
; .AP type name in/out [indent]
;	Start paragraph describing an argument to a library procedure.
;	type is type of argument (int, etc.), in/out is either "in", "out",
;	or "in/out" to describe whether procedure reads or modifies arg,
;	and indent is equivalent to second arg of .IP (shouldn't ever be
;	needed;  use .AS below instead)
;
; .AS [type [name]]
;	Give maximum sizes of arguments for setting tab stops.  Type and
;	name are examples of largest possible arguments that will be passed
;	to .AP later.  If args are omitted, default tab stops are used.
;
; .BS
;	Start box enclosure.  From here until next .BE, everything will be
;	enclosed in one large box.
;
; .BE
;	End of box enclosure.
;
; .VS
;	Begin vertical sidebar, for use in marking newly-changed parts
;	of man pages.
;
; .VE
;	End of vertical sidebar.
;
; .DS
;	Begin an indented unfilled display.
;
; .DE
;	End of indented unfilled display.
; 

(defun do-replace (lis &optional not-in-string)
  (let (x case-fold-search)
    (while lis
      (setq x (car lis)) (setq lis (cdr lis))
      (goto-char (point-min))
      (message "doing %s " x)
      (while (re-search-forward (nth 0 x) nil t)
	(and not-in-string
	     (progn (forward-char -1)
		    (not (in-a-string))))
	(let ((f (nth 1 x)))
	  (cond ((stringp f)
		 (replace-match f t))
		(t (let ((i 0) ans)
		     (while (match-beginning i)
		       (setq ans (cons (buffer-substring
					(match-beginning i)
					(match-end i)) ans))
		       (setq i (+ i 1)))
		     (setq ans (nreverse ans))
		     (goto-char (match-beginning 0))
		     (delete-region (match-beginning 0)
				    (match-end 0))
		     (apply f ans)))))))))




(defun doit ()
  (interactive)
  (texinfo-mode)
  (goto-char (point-min))
  (do-replace '(("@" "@@")
		("^[.]VS\n" "")
		("^[.]VE\n" "")
		))
  (goto-char (point-min))
  (insert "@setfilename foo.info")
  (insert "\n")
  (do-tables)
;  (do-nf)
 (do-replace
  '(
    (".SH \"SEE ALSO\"\n\\([^\n]*\\)" "@xref{\\1}")
    ("^[.]SH NAME" "")
    ("^'[\\]\"[^\n]*\n" "")
    ("^'[/]\"[^\n]*\n" "")
    ("^[.]so[^\n]+\n" "")
    ("[.]HS \\([^ \n]+\\)\\([^\n]*\\)\n"
     "@node \\1\n@subsection \\1\n")
    ("^[.]VS\n" "")
    ("^[.]VE\n" "")
    (".nf\nName:\t\\([^\n]*\\)\nClass:\t\\([^\n]*\\)\nCommand-Line Switch:\t\\([^\n]*\\)\n.fi\n" do-keyword)
    ("Name:\t\\([^\n]*\\)\nClass:\t\\([^\n]*\\)\nCommand-Line Switch:\t\\([^\n]*\\)\n" do-keyword)
    ("Name:\t\\([^\n]*\\)\n" "@*@w{  Name: @code{\\1}}\n")
    ("Class:\t\\([^\n]*\\)\n" "@*@w{  Class: @code{\\1}}\n")
    ("Command-Line Switch:\t\\([^\n]*\\)\n" "@*@w{  Keyword: @code{\\1}}\n")
    ("[\\]-\\([a-z]\\)" ":\\1")
    ("^[.]nf\n" "@example\n")
    ("^[.]fi\n" "@end example\n")
    ("^[.]ta[^\n]*\n" do-ta)
    ("^[.]IP\n" "\n")
    ("[\\]f\\([A-Z]\\)\\([^\\\n]*\\)[\\]f"
     do-font)
    ("^\\([^\n]+\\)\n[.]br" "@*@w{\\1}@*")
    ("^[.]SH \\([^\n]*\\)"
     (lambda (a0 a1)
       (insert  "@unnumberedsubsec " (capitalize a1))))
    ("[\\]fR" "")
    
    ("^[.]BS" "@cartouche")
    ("^[.]BE" "@end cartouche")
    ("^[.]sp \\([0-9]\\)" "@sp \\1")
    ("^[.]sp" "@sp 1")
    ("^[.]LP\n" "\n\n")
    ("^[.][LP]P" "")
    ("^[.]DS[^\n]*\n" "\n@example\n")
    ("^[.]DE[^\n]*\n" "@end example\n\n")
    ("^[.]DS[^\n]*\n" "\n@example\n")
    ("^[.]DE[^\n]*\n" "@end example\n\n")
    ("^[.]RS\n" "")  ; relative indent increased..
    ("^[.]rE\n" "")
    ("^[\\]&\\([^\n]*\\)\n" "@*@w{   \\1}\n")
;    ("Command-Line Switch" "Keyword")
    ("pathName }@b{\\([a-z]\\)" "pathName }@b{:\\1")
    ("[\\]0" " ")
    ("%\\([a-z#]\\)\\([^a-zA-Z0-9%]\\)" "|%\\1|\\2")
    ("^[.]TP[^\n]*\n" "@item ")
    ))
 (add-keywords)
 )

(defun do-font (ign a b)
  (let ((ch (assoc (aref a 0)
		   '((?R . "@r{")
		     (?I . "@i{")
		     (?B . "@b{")))))
    (cond (ch (insert (cdr ch) b "}\\f")
	      (forward-char -2)
	      )
	  (t     (error "unknown leter %s" a)))))

(defun do-keyword (ign name class key)
  (insert "@table  \n@item @code{"key "}"
	  "\n@flushright\nName=@code{\""name"\"} Class=@code{\""class "\"}\n"
	  "@end flushright\n@sp 1\n")
  (save-excursion
    (cond ((re-search-forward "[.]LP\\|[.]BE\\|[.]SH" nil t)
	   (beginning-of-line)
	   (insert "@end table\n")))))
  
  
	  
	  
(defun try ()
  (interactive)
  (if (get-buffer "foo.texi")
      (kill-buffer (get-buffer "foo.texi")))

  (if (get-buffer "foo.info")
      (kill-buffer (get-buffer "foo.info")))

  (find-file "foo.n")
  (toggle-read-only 0)
  (doit)
  (write-file "foo.texi")
  (makeinfo-buffer ))

(defun foo ()
  (re-search-forward "\n\\|\\([\\]f[a-zA-Z]\\)" nil t)
  (list (match-beginning 0) (match-beginning 1) (match-beginning 2)))
	
(defun list-current-line ()
  (beginning-of-line)
  (let (ans at-end (beg (point)))
    (save-excursion
      (while (not at-end)
	(re-search-forward "\n\\|\\([\\]f[a-zA-Z]\\)" nil t)
	(if (match-beginning 1) (replace-match "")
	  (setq at-end t))))
    (setq at-end nil)
    (beginning-of-line)
    (while (not at-end)
      (re-search-forward "[\t\n]" nil t)
      (let ((x (buffer-substring beg (- (point) 1))))
	(or (equal x "")
	    (setq ans (cons x     ans))))
      
      (setq beg (point))
      (setq at-end (equal (char-after (- (point) 1)) ?\n)))
    (nreverse ans)
    ))

(defun do-ta (a0)
  (let ((beg (point))
	items (vec (make-vector 10 0)) i (tot 0) surplus)
    (while (not (looking-at "[.][LDI]"))
      (cond ((looking-at "[.]")(forward-line 1))
	    (t
	     (setq items (cons (list-current-line) items))
	     (let ((tem (car items))
		   (i 0))
	       (while tem
		 (aset vec i (max (real-length (car tem)) (aref vec i)))
		 (setq i (+ i 1))
		 (setq tem (cdr tem)))
	       ))))
;    (message "%s" (list beg (point)))
;    (sit-for 1)
    
    (delete-region beg (point))
;     (forward-line -2)
;    (message "%s" vec)
;    (sit-for 2)
    (setq items (nreverse items))
    (setq i 0)
    (while (< i (length vec)) (setq tot (+ (aref vec i) tot)) (setq i (+ i 1)))
    (setq surplus (/ (- 70 tot) (+ 1 (length (car items)))))
    (while items
      (setq tem (car items))
      (setq i 0)
      (let (ans x)
	(insert "")
	(while tem
	  (insert (tex-center (car tem) (+ (aref vec i) surplus) 'left
			      (real-length (car tem))))
	  (setq tem (cdr tem)) (setq i (+ i 1)))
	(insert "\n"))
      (setq items (cdr items)))
    )
  )
	
	
      
    

  
  
(defun real-length (item)
  (let* ((n (length item)) (m (- n 1)) (start 0))
    (while (setq start (string-match "[\\]f" item start))
      (setq n (- n 3))
      (if (<  start m) (setq start (+ start 1))))
    n))


(defun do-tables ()
  (goto-char (point-min))
  (while (re-search-forward "^[.]TP" nil t)
    (beginning-of-line)
    (insert "\n@table @asis\n")
    (forward-line 2)
    (re-search-forward "^[.]\\(LP\\|BE\\|SH\\)" nil t)
    (beginning-of-line)
    (insert "@end table\n")
    ))
(defun do-nf ()
  (goto-char (point-min))
  (while (re-search-forward "^[.]nf" nil t)
    (forward-line 1) (beginning-of-line)
      (while (not (looking-at "[.]fi"))
	(insert "@w{" ) (end-of-line) (insert "}")
	    (forward-line 1) (beginning-of-line))))

(defun add-keywords ()
  (let ((tem tk-control-options)x lis l y)
    (while tem
      (setq l (car tem))
      (setq tem (cdr tem))
      (setq x (symbol-name (car l )))
      (setq lis (car (cdr l)))
      (while lis
	(cond ((atom lis) (setq lis nil))
	      (t (setq y (symbol-name (car lis)))
		 (do-replace (list (list (concat x  " "y "")
					 (concat x " :"y "")
					 )))))
	(setq lis (cdr lis))))))

(setq tk-control-options
      '((after fixnum) 
	(exit fixnum) 
	(lower window) 
	(place pathName (-anchor -bordermode -height
				 -in -relheight -relwidth
				 -relx -rely -width -x  -y))
	(send interpreter )
					;(TKVARS "invalid command name \"tkvars\"") 
	(winfo  (atom atomname cells children class containing
		      depth exists fpixels geometry height id
		      interps ismapped name parent pathname pixels
		      reqheight reqwidth rgb rootx rooty screen
		      screencells screendepth screenheight screenmmheight
		      screenmmwidth screenvisual screenwidth toplevel
		      visual vrootheight vrootwidth vrootx vrooty width x y) )
	(focus (default  none) )
	(option (add clear get readfile)) 
	(raise pathname)
	(tk  colormodel) 
	(tkwait  ( variable visible window) ) 
	(wm  (aspect client command deiconify focusmodel frame geometry grid group iconbitmap iconify iconmask iconname iconposition iconwindow maxsize minsize overrideredirect positionfrom protocol sizefrom state title trace transient  withdraw))
	(destroy window) 
	(grab (current release set  status))
	(pack window (-after, -anchor, -before, -expand, -fill, -in, -ipadx, -ipady, -padx, -pady, -side) argggg)
	(selection (clear get handle own))
	(tkerror "") 
	(update (idletasks)) 
	))

(setq tk-widget-options
      '(
	(button (activate configure deactivate flash invoke)) 
	(listbox ( configure curselection delete get insert nearest
			     scan select size xview yview)) 
	(scale ( configure get set)) 
	(canvas ( addtag bbox bind canvasx canvasy configure coords
			 create dchars delete dtag find focus gettags
			 icursor index insert itemconfigure lower move
			 postscript raise scale scan select type xview yview)) 
	(menu ( activate add configure delete disable enable
			 entryconfigure index invoke post unpost yposition)) 
	(scrollbar ( configure get set)) 
	(checkbutton
	 (     activate configure deactivate deselect flash
			invoke select toggle)) 
	(menubutton
	 (     activate configure deactivate)) 
	(text ( compare configure debug delete get index insert
			mark scan tag yview)) 
	(entry ( configure delete get icursor index insert scan select view)) 
	(message ( configure)) 
	(frame ( configure)) 
	(label ( configure)) 
	(radiobutton
	 (     activate configure deactivate deselect flash invoke  select)) 
	(toplevel ( configure)) 
	))

(setq manual-sections
      '(after bind button canvas checkbutton destroy  entry exit focus foo frame grab label lbSingSel listbox lower menu menubar menubutton message option options pack-old pack place radiobutton raise scale scrollbar selection send text tk tkerror tkvars tkwait toplevel update winfo wm))

;(setq widgets (sort (mapcar 'car tk-widget-options) 'string-lessp))
;(let ((m manual-sections)(tem widgets)) (while tem  (setq manual-sections (delete (car tem) manual-sections))(setq tem (cdr tem))))

			

