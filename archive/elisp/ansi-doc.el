;; Copyright  William F. Schelter.   1994
;; Licensed by GNU public license.

;; This file contains function find-ansi-doc which finds documentation in the
;; standard common lisp ansi documentation (1350 pages!), and puts it on
;; the screen at the correct page using xdvi.  If there is more than one
;; reference it successively finds them.  You need dpANS2/*.dvi
;; dpANS2/index.idx from parcftp.xerox.com (13.1.64.94) You also need
;; xdvi.   You may gzip the .dvi files and it will unzip them into tmp
;; as needed.


(defvar ansi-doc-dir "/usr/local/doc/dpANS2")
(defvar ansi-doc-alist nil)

(defun create-index-el-from-index-idx ()
  (interactive)
  (let (tem)
    (cond ((not ansi-doc-alist)
	   (setq tem (concat ansi-doc-dir "/index.el"))
	   (or (file-exists-p tem)
	       (progn
		 (shell-command
		  (concat "echo '(setq ansi-doc-alist (quote (( ' > " tem))
		 (shell-command
		  (concat "cat " ansi-doc-dir "/index.idx "
			  "| sed "
			  " -e 's/\\!9\\([A-Z]\\):\\([^\\!]*\\)\\!\\!/)(\"\\2\" \\1/g' "
			  " -e 's:{$\\\\spLT \\$}:<:g' "
			  " -e 's:{$\\\\spGT $}:>:g' "
			  " -e 's:\\\\&:\\&:g' "
			  " -e 's:\\([0-9]\\),:\\1:g'"
			  " -e 's:\\([A0-9][0-9]*\\)--\\([0-9][0-9]*\\):(\\1 . \\2):g'"
			  " | sort -r  "
			  " >> " tem))
		 (shell-command (concat "echo '))))' >>  " tem))))
	   
	   ))))
(defun maybe-gzip-to-tmp (file &optional dir)
  "If file exists with  .gz  added to it, then unzip it to /tmp and
return that file otherwise return file"
  (let (tmp-file)
    (cond  ((file-exists-p (concat file ".gz"))
	    (setq tmp-file
			  (file-name-nondirectory file))
	    (or (file-exists-p tmp-file)
		(progn (message "gzipping %s in /tmp for future use" file)
		       (shell-command (concat "gzip -dc < " file ".gz > "
					      tmp-file ))))
	    tmp-file)
	   (t file))))

(defun find-ansi-doc ()
  "Find the documentation in the ansi draft on a particular function
or topic.   If there are several pieces of documentation then go through
them successively.   Requires copying the "
  (interactive )
  (let (x tem name lis first chap tmp-chap)
    (or ansi-doc-alist
	(progn
	  (create-index-el-from-index-idx )
	  (load (concat ansi-doc-dir "/index.el"))))
    (setq name (completing-read "Doc on: " ansi-doc-alist nil t))
    (progn  (setq ans nil)   (setq lis ansi-doc-alist)
	    (while lis
	      (cond ((equal (car  (car lis)) name)
		     (setq ans (append ans (cdr  (cdr (car lis)))))))
	      (setq lis (cdr lis)))
	    )	    
    (setq tem ans)
    (if (cdr tem) (setq first "First") (setq first ""))
    (while tem
      (setq x (car tem))
      (setq chap (concat ansi-doc-dir
		  (downcase (format "/chap-%s.dvi"  (car x)))))
      (setq chap (maybe-gzip-to-tmp chap))
      (message "%s Doc in Chapter %s page %s) %s .." first (car x) (cdr x))
      (if (cdr tem) (setq first "Next") (setq next "Final"))
      (shell-command (concat "xdvi  -expert -xoffset .2 -yoffset -.2 "
			     " -paper 7.2x8.5 "
			     " -display "
			     (or x-display-name ":0")
			     "  -geometry -2-2 +" (+ (cdr x) 2)" "
			     chap
			     ))
      (setq tem (cdr tem))

      )
    )
  (message nil)
  
  )
