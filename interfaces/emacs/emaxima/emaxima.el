;; emaxima.el  Mode for interaction with Maxima from TeX buffer
;; Written 2/12/1991 by Dan Dill dan@chem.bu.edu
;; Modified for Maxima by Jay Belanger

;; Copyright (C) 1991, 1993 Dan Dill (dan@chem.bu.edu) 
;;               1999-2001 Jay Belanger (belanger@truman.edu)

;; Author: Dan Dill
;;         Jay Belanger
;; Maintainer: Jay Belanger <belanger@truman.edu>
;; $Name:  $
;; $Revision: 1.1 $
;; $Date: 2001-11-08 22:21:30 $
;; Keywords: maxima, emaxima

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;          
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;          
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;
;; Please send suggestions and bug reports to <belanger@truman.edu>. 
;; The latest version of this package should be available at
;; ftp://vh213601.truman.edu/pub/Maxima
;; You will need, in addition to this file,
;; maxima.el, maxima-font-lock.el, maxima-symbols.el, and emaxima.sty

;;; Commentary:

;;  See the file EMintro.ps for a quick introduction.

;;; Change Log:

;; $Log: emaxima.el,v $
;; Revision 1.1  2001-11-08 22:21:30  belanger
;; Initial commit of EMaxima elisp file.
;;
;; Revision 1.12  2001/11/06 16:13:32  jay
;; I added the keybinding M-C-k for maxima-stop.
;;
;; Revision 1.11  2001/11/06 04:31:07  jaycvs
;; I fixed the update to return to the original position when the update
;; if finished.
;;
;; Revision 1.10  2001/11/05 13:40:45  jaycvs
;; I fixed a minor problem with a regexp.
;;
;; Revision 1.9  2001/11/03 19:59:13  jaycvs
;; I added some functions to replace single lines,
;; and some general tidying up.
;;
;; Revision 1.8  2001/11/03 16:07:30  jaycvs
;; I made some of the variables customizable,
;; and a few other minor changes.
;;


(require 'maxima)
(provide 'emaxima)

;;;; The variables that the user may wish to change

(defgroup emaxima nil
  "Maxima mode"
  :prefix "emaxima-"
  :tag    "EMaxima")

(defcustom emaxima-use-tex 'auctex
  "Possible modes to use within EMaxima.
Possible choices are 'auctex, 'tex or nil"
  :group 'emaxima
  :type '(choice :menu-tag "TeX style"
                 :tag      "TeX style"
                 (const auctex)
                 (const tex) 
                 (const nil)))

(defcustom emaxima-abbreviations-allowed t
  "If non-nil, then `...' abbreviations are allowed in cell labels 
and references. Note that enabling this options will slow cell and 
package assembly."
  :group 'emaxima
  :type '(boolean))

(defcustom emaxima-max-references 5
  "Number of references in a cell below which cell references are fetched
as needed, scanning the entire document for each reference.  At or above this
number, all cells in a document for the given filename are precollated in a
single scan of the document."
  :group 'emaxima
  :type '(integer))

(defcustom emaxima-temp-dir "/tmp/"
  "Directory for temporary files.
Specify \"\" to use the directory of the Emaxima document buffer."
  :group 'emaxima
  :type '(directory))

;;; Other variables and constants

(defconst emaxima-output-marker "\\output"
  "Contents of line separating input and output portion of cell.")


(defconst emaxima-tex-string
  "tex(%);"
  "A string to send to Maxima to set output to TeX form")

(defvar emaxima-zap-file nil
  "Temporary file name used for text being sent as input to Maxima.")

(defvar emaxima-dereference-path nil
  "List of buffers referenced in cell assembly.
Used by `emaxima-dereference-buffer' to detect self-reference.")

(defvar emaxima-zap-file-prefix nil
  "Global variable used as prefix to make unique buffer names for cell 
and package assembly.")

(defvar emaxima-error-point nil
  "Buffer position where error detected.")

(defvar emaxima-buffer-alist nil
  "Alist of temporary buffers associate with cells `file:part'.
The buffers are used in package and cell assembly.")

(defvar emaxima-source-buffer nil
  "Buffer from which emaxima-collate-cells works.")

(defconst emaxima-temp-suffix 0
  "Temporary filename suffix.  Incremented by 1 for each filename.")

(defun emaxima-make-temp-name ()
  "Return a unique filename."
  (setq emaxima-temp-suffix (+ emaxima-temp-suffix 1))
  (concat (concat (make-temp-name "#mz") "-")
          (int-to-string emaxima-temp-suffix)
          ".mu"))

(defun emaxima-mark-file-as-emaxima ()
  "Mark the file as an EMaxima buffer.
The next time the file is loaded, it will then be in EMaxima mode"
  (interactive)
  (save-excursion
    (goto-line 1)
    (beginning-of-line)
    (if (looking-at ".*-\\*-EMaxima-\\*-")
	()
	(open-line 1)
      (insert "%-*-EMaxima-*-"))))

;(defun emaxima-set-output-to-tex ()
;  "Arrange to have Maxima output in TeX form."
;  (maxima-send-string "oldPrefoutput := Pref::output():")
;  (maxima-send-string "Pref::output(x->print(Unquoted,generate::TeX(x))):")
;  (maxima-wait))

;(defun emaxima-restore-output-form ()
;  "Restore the output form to the original form."
;  (maxima-send-string "Pref::output(oldPrefoutput):")
;  (maxima-wait))

(defun emaxima-replace-assoc (alist key val)
  "Replace ALIST KEY VALUE, if KEY present, else add KEY VALUE.
Return modified alist."
  (if (assoc key alist)
      (setcdr (assoc key alist) val)
    (setcdr alist (cons (cons key val) (cdr alist))))
  alist)

(defun emaxima-toggle-init ()
  "Toggle initialization marker of TYPE cell containing point."
  (interactive)
  (save-excursion
    (re-search-backward "^\\\\maxima")
    (goto-char (match-end 0))
    (if (looking-at "\\[\\* Initialization Cell \\*\\]")
        (delete-region (match-beginning 0) (match-end 0))
      (insert "[* Initialization Cell *]"))))

(defun emaxima-package-part ()
  "Toggle initialization marker of TYPE cell containing point."
  (interactive)
  (save-excursion
    (let ((package (read-string "Package: " "...:")))
      (re-search-backward "^\\\\maxima")
      (goto-char (match-end 0))
      (insert (concat "<" package ">")))))

(defun emaxima-update-all (arg)
  "Optionally update all cells.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (save-excursion
    (if arg
        (emaxima-update nil nil nil)
      (emaxima-update nil (y-or-n-p "Interactive update? ") nil))))

(defun emaxima-tex-update-all (arg)
  "Optionally update all cells and return output in TeX form.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
;  (emaxima-set-output-to-tex)
  (if arg
      (emaxima-update nil nil t)
    (emaxima-update nil (y-or-n-p "Interactive update? ") t)))
;  (emaxima-restore-output-form))

(defun emaxima-update-init (arg)
  "Optionally update all initialization cells.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
  (if arg
      (emaxima-update "\\[\\* Initialization Cell \\*\\]" nil nil)
    (emaxima-update "\\[\\* Initialization Cell \\*\\]" 
		   (y-or-n-p "Interactive update? ") nil)))

(defun emaxima-tex-update-init (arg)
  "Optionally update all initialization cells and return output in TeX form.
With C-u prefix, update without confirmation at each cell."
  (interactive "P")
;  (emaxima-set-output-to-tex)
  (if arg
      (emaxima-update "\\[\\* Initialization Cell \\*\\]" nil t)
    (emaxima-update "\\[\\* Initialization Cell \\*\\]" 
		   (y-or-n-p "Interactive update? ") t)))
;  (emaxima-restore-output-form))

(defun emaxima-create-cell ()
  "Insert cell in buffer."
  (interactive)
  (if (emaxima-cell-p)
      (error "Cell already exists")
    (if (not (bolp))
        (progn
          (open-line 1)
          (forward-line 1)))
    (insert "\\maxima\n\n\\endmaxima")
    (unless (looking-at " *$")
	(insert "\n")
	(forward-line -1))
    (beginning-of-line)
    (previous-line 1)))

(defun emaxima-forward-cell ()
  "Move to next cell."
  (interactive)
    (let ((cur-pos (point))
          (cell-pos (point-max))
          new-pos)
        (setq new-pos (emaxima-next-cell-start))
        (if (not (equal new-pos cur-pos))
            (if (> new-pos cell-pos)
                nil
              (setq cell-pos new-pos)))
      (if (equal cell-pos (point-max))
          nil; No more cells
        (goto-char cell-pos))))

(defun emaxima-backward-cell ()
  "Move to previous cell."
  (interactive)
    (let ((cur-pos (point))
          (cell-pos (point-min))
          new-pos)
        (setq new-pos (emaxima-previous-cell-start))
        (if (not (equal new-pos cur-pos))
            (if (< new-pos cell-pos)
                nil
              (setq cell-pos new-pos)))
      (if (equal cell-pos (point-min))
          nil ; No more cells
        (goto-char cell-pos))))

(defun emaxima-update (kind ask tex)
  "Optionally update all KIND cells.
If ASK is non-nil, then ask whether each KIND cell is to be updated,
else update each KIND cell.  If KIND is nil, update all cells.
If TEX is non-nil, then insert \\outputtex instead of \\output."
  (let (bypass display-start display-end cur-pos)
    (save-excursion
      (goto-char (point-min))
      (while (emaxima-forward-cell)
        (forward-line -1)
        (if (not (looking-at (concat "^\\\\maxima" kind)))
            (progn
              (forward-line 1) ; Don't want the same cell next time
              nil) ; Wrong kind of cell
          ;; We have a cell of the right kind
          (setq display-start (point))
          (goto-char (emaxima-cell-end))
          (forward-line 1) ; We need to include cell trailer in narrowed region
          (end-of-line)    ; ..
          (setq display-end (point))
          (forward-line 0)
          (unwind-protect
              (progn
                (narrow-to-region display-start display-end)
                (goto-char (point-min))
                (recenter 1) ; force display, just in case...
                (forward-line 1)
                (if (and ask (not (y-or-n-p "Update this cell? ")))
                    t
                  (emaxima-update-type-cell tex)))
            (widen) ; If user aborts evaluation at prompt
            ) ; unwind-protect
          ) ; if in a valid cell
        ) ; while still types to check
      (widen)
      (sit-for 1)) ; save-excursion
;    (beep)
    (message "Update of cells finished")))

(defun emaxima-eval-init ()
  "Evaluate all initialization cells, without returning the output."
  (interactive)
  (let (bypass display-start display-end cur-pos)
    (save-excursion
      (goto-char (point-min))
      (while (emaxima-forward-cell)
        (forward-line -1)
        (if (not (looking-at "^\\\\maxima\\[\\* Initialization Cell \\*\\]"))
            (progn
              (forward-line 1) ; Don't want the same cell next time
              nil) ; Wrong kind of cell
          ;; We have a cell of the right kind
          (setq display-start (point))
          (goto-char (emaxima-cell-end))
          (forward-line 1) ; We need to include cell trailer in narrowed region
          (end-of-line)    ; ..
          (setq display-end (point))
          (forward-line 0)
          (unwind-protect
              (progn
                (narrow-to-region display-start display-end)
                (goto-char (point-min))
                (recenter 1) ; force display, just in case...
                (forward-line 1)
                (emaxima-send-cell))
            (widen) ; If user aborts evaluation at prompt
            ) ; unwind-protect
          ) ; if in a valid cell
        ) ; while still types to check
      ) ; save-excursion
    (widen)
;    (beep)
    (message "Evaluation of initialization cells finished")))

(defun emaxima-cell-start ()
  "Return position of start of cell containing point."
  (let ((begin-re "^\\\\maxima"))
  (save-excursion
    (if (not (looking-at begin-re))
        (re-search-backward begin-re))
    (forward-line 1)
    (point))))

(defun emaxima-cell-end ()
  "Return position of end of cell containing point."
  (let ((end-re "^\\\\endmaxima"))
    (save-excursion
      (re-search-forward end-re)
      (forward-line -1)
      (end-of-line)
      (point))))

(defun emaxima-previous-cell-start ()
  "Get start of preceding cell.  If none, return current position."
  (let ((cur-pos (point))
        (start nil)
        (begin-re "^\\\\maxima")
        (end-re "^\\\\endmaxima"))
    (save-excursion
      (if (not (re-search-backward end-re (point-min) t))
          cur-pos
        (if (emaxima-cell-p)
            (progn
              (re-search-backward begin-re)
              (forward-line 1)
              (point))
          cur-pos)))))
              
(defun emaxima-next-cell-start ()
  "Get start of next cell.  If none, return current position."
  (let ((cur-pos (point))
        (start nil)
        (begin-re "^\\\\maxima")
        (end-re "^\\\\endmaxima"))
    (save-excursion
      (if (re-search-forward begin-re (point-max) t)
          (progn
            (if (not (emaxima-cell-p))
                cur-pos)
            (forward-line 1)
            (point))
        cur-pos))))

(defun emaxima-cell-p ()
  "Returns t if point is in a Emaxima cell, else returns nil."
  (let ((begin-re "^\\\\maxima")
        (end-re "^\\\\endmaxima")
        (found nil))
    (save-excursion
      (if (re-search-backward begin-re (point-min) t) ; \maxima
            (setq found (point))))
    (save-excursion
      (if (and found
               (re-search-backward end-re found t)) ; Intervening \endmaxima
          (setq found nil)))
    (save-excursion
      (if (and found
               (re-search-forward end-re (point-max) t)) ;\endmaxima
          (setq found (point))))
    (save-excursion
        (if (and found 
                 (re-search-forward begin-re found t)) ; Intervening \maxima
            (setq found nil)))
    (if found t nil)))
        
(defun emaxima-delete-output ()
  "Delete current output (if any).  Assumes point in cell.
Output assumed to follow input, separated by a emaxima-output-marker line.
Input *may* contain blank lines."
  (interactive)
  (let ((out-start (emaxima-output-p)))
    (if out-start
        (delete-region out-start (emaxima-cell-end))
      t)))

(defun emaxima-output-p ()
  "Return start of output text if present, else return nil.  Assumes
point in cell.  Output assumed to follow input, separated by a
\output."
  (save-excursion
    (goto-char (emaxima-cell-start))
    (if (re-search-forward "^\\\\output"
         (emaxima-cell-end) t)
        (progn
          (forward-line -1)
          (end-of-line)
          (point))
      nil)))

;;; @@ EMaxima functions for package assembly

(defun emaxima-assemble (arg)
 "Assemble package (see emaxima-assemble-package), or, with C-u prefix,
assemble references within a cell (see emaxima-assemble-cell)."
  (interactive "P")
  (if arg
      (emaxima-assemble-package)
    (emaxima-assemble-cell)))

(defun emaxima-assemble-cell (&optional delete)
  "Assemble references in cell to file with unique name.  The buffer used to
write the file is not deleted, unless optional DELETE is non-nil.
Return the filename."

  ;; Here is how this function works:

  ;; The text of the cell is written to a buffer with key `file:part'.  Then
  ;; the number of references in the cell is counted.  If the number of
  ;; references in the cell is less than emaxima-max-references, then the cell
  ;; references are resolved by successive calls to emaxima-dereference-buffer
  ;; which collates the text for cell references as needed, using
  ;; emaxima-collate-cells.  If the number of references is equal to or
  ;; greater than emaxima-max-references, then all cells in the document
  ;; correpsonding to the current cell type and filename are collated into
  ;; buffers, using emaxima-collate-cells, and then the all cell references
  ;; are are resolved by successive calls to emaxima-dereference-buffer.

  ;; The global `emaxima-buffer-alist' associates buffer names with keys.
  ;; Buffer names are unique.  The names of all buffers are constructed with
  ;; `emaxima-make-temp-name' and are unique.  All buffers except possibly the
  ;; cell-buffer are deleted on exit.

  (interactive)
  (let ((home-buffer (current-buffer))
        files parts file part 
        ref-count
        cell-key cell-buffer tmp-alist tmp-buffer)
    (if (not (emaxima-cell-p)) (error "Not in a cell"))
    (if (not (emaxima-reference-p)) (error "Cell contains no references"))
    (save-excursion
      (goto-char (emaxima-cell-start))
      (forward-line -1)
      (if (not (looking-at "^\\\\maxima.*<.*:.*>"))
          (error "Cell is not marked"))

      (setq emaxima-error-point (point))
      (if emaxima-abbreviations-allowed
          (unwind-protect ; In case filename errors
              ;; This can take some seconds
              (progn
                (message "Getting filenames...")
                (setq files (emaxima-get-filenames))
                (message "")
                )
            (goto-char emaxima-error-point)))

      (setq file (emaxima-get-filename files))
      (if (not file) (error "Ambiguous filename"))

      (if emaxima-abbreviations-allowed
          ;; This can take several seconds for a document with many cells
          (progn
            (message "Getting partnames")
            (setq parts (emaxima-get-partnames file files))
            (message "")
            ))

      (setq part (emaxima-get-partname parts))
      (if  (not part) (error "Ambiguous partname"))

      ) ; save-excursion

    (setq cell-key (concat file ":"))
    (if (not (equal part "")) (setq cell-key (concat cell-key part)))
    (message "Assembling `%s' ..." cell-key) ; (sleep-for 1)
    (setq cell-buffer (emaxima-make-temp-name))
    (setq emaxima-buffer-alist (list (cons cell-key cell-buffer)))
    (unwind-protect
        (save-excursion
          (emaxima-append-cell-to-buffer cell-buffer)
          (setq emaxima-source-buffer (current-buffer)) ; Collate from here

          (if (< (emaxima-reference-count cell-buffer) emaxima-max-references)
              ;; Build reference buffers as needed
                (while (emaxima-dereference-buffer cell-key files parts nil))
            ;; Prebuild all reference buffers
            (emaxima-collate-cells file part files parts nil)
            (while (emaxima-dereference-buffer cell-key files parts nil nil))
            )
          (set-buffer cell-buffer)
          (write-file (concat emaxima-temp-dir cell-buffer))
          (set-buffer home-buffer)
          )
      ;; unwind-protect forms: deleted cell buffers
      (setq tmp-alist emaxima-buffer-alist)
      (while (setq tmp-buffer (cdr (car tmp-alist)))
        (setq tmp-alist (cdr tmp-alist))
        (condition-case nil ; In case buffer not actually created
            (if (and (not delete) (equal tmp-buffer cell-buffer))
                nil ; Don't delete the assembly buffer
              (kill-buffer tmp-buffer))
          (error nil)))
      ) ; unwind-protect
    (message "`%s' assembled in file `%s%s'" 
	     cell-key emaxima-temp-dir cell-buffer)
    (concat emaxima-temp-dir cell-buffer)))

(defun emaxima-assemble-package (&optional file overwrite)
  "Assemble text into a package buffer and write that buffer to a file.
The buffer is *not* deleted.  Return the filename.

Optional arguments (useful for batch processing):

FILE package filename;
OVERWRITE, if not nil package filename buffer will be overwritten 
without asking."

  ;; Here is how this function works:

  ;; The entire buffer is scanned for marked cells matching TYPE and FILE and
  ;; these are collated by `file' and `part' into buffers with keys
  ;; `file:part' and, for `part' = "" (a package cell), into a buffer with key
  ;; `FILE'.

  ;; Once the cell buffers have been created, then all cell references in the
  ;; package buffer, with key `FILE', are replaced by the contents of the
  ;; corresponding buffers with keys `file:part', by successive calls to
  ;; emaxima-dereference-buffer.

  ;; The global `emaxima-buffer-alist' associates buffer names with keys.
  ;; Buffer names are unique.  The names of all buffers are constructed with
  ;; `emaxima-make-temp-name' and are unique.    All buffers
  ;; except the package buffer `FILE' are deleted on exit.

  (interactive)
  (let ((home-buffer (current-buffer))
        files parts prompt
        tmp-buffer tmp-alist file-buffer
        )

    (if (not file)
        ;; If file has not been specifed, prompt
        (progn
              ;; Get default file from cell label, if any
              (save-excursion
                (goto-char (emaxima-cell-start))
                (forward-line -1)
                (if (looking-at "^\\\\maxima.*<.*:.*>")
                    (progn
                      (setq emaxima-error-point (point))
                      (unwind-protect ; In case filename errors
                          (if emaxima-abbreviations-allowed
                              ;; This can take some seconds
                              (progn
                                (message "Getting filenames...")
                                (if (not (setq files (emaxima-get-filenames)))
                                    (error 
                                       "No complete package filenames found"))
                                (message "")
                                ))
                        (goto-char emaxima-error-point))
                      (setq file (emaxima-get-filename files)))))
          (setq file (read-from-minibuffer "Package file: " file))
          (if (or (not file) (equal file "")) (error "No file specified"))))

    (if (not overwrite)
        (if (file-exists-p file)
            (progn
              (setq prompt (concat
                            "Package file `"
                            file
                            "' exists. Overwrite it ? "))
              (if (not (y-or-n-p prompt))
                  (error "Package assembly cancelled")))))
    
    (if (get-buffer file) (kill-buffer file))

    (if emaxima-abbreviations-allowed
        ;; This can take several seconds for a document with many cells
        (progn
          (message "Getting partnames...")
          (setq parts (emaxima-get-partnames file files))
          (message "")))

    (message "Assembling package `%s' ..." file) ;(sleep-for 1)

    ;; Set where assembly will occur
    (setq file-buffer (emaxima-make-temp-name))
    (setq emaxima-buffer-alist (list (cons file file-buffer)))

    (unwind-protect ; So buffer can be deleted even if errors or abort
        (progn
          (setq emaxima-source-buffer (current-buffer)) ; Collate from here
          (emaxima-collate-cells file nil files parts nil)
          (or (get-buffer (cdr (assoc file emaxima-buffer-alist)))
              (error "No `%s' cell `%s:' found" file))
          
          ;; OK, here we go:  Recursively dereference the cell buffer:
          (while (emaxima-dereference-buffer file files parts))

          (set-buffer file-buffer)
          (write-file file)
          (set-buffer home-buffer))
      ;; unwind-protect tail:  Delete part files
      (setq tmp-alist emaxima-buffer-alist)
      (while (setq tmp-buffer (cdr (car tmp-alist)))
        (setq tmp-alist (cdr tmp-alist))
        (condition-case nil ; In case buffer not actually created
            (if (equal tmp-buffer file-buffer)
                nil ; Don't delete the package buffer
              (kill-buffer tmp-buffer))
          (error nil)))
      ) ; unwind-protect
    (message "Package `%s' assembled" file)
;    file
    (switch-to-buffer-other-window file)))

(defun emaxima-reference-count (buffer)
  "Return the number of references in BUFFER."
  (let ((count 0)
        (home-buffer (current-buffer)))
    (save-excursion
          (set-buffer buffer)
          (goto-char (point-min))
          (while (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (point-max) t)
            (setq count (+ count 1)))
          (set-buffer home-buffer))
    count))

(defun emaxima-append-cell-to-buffer (buffer)
  "Append text of cell containing point to BUFFER.
Create BUFFER if it does not exist."
  (if (not (emaxima-cell-p))
      (error "Not in a cell.")
    (let ((home-buffer (current-buffer))
          (start (emaxima-cell-start))
          end)
      (save-excursion
	(goto-char start)
	(beginning-of-line)
        (while (looking-at "^ *$") (forward-line 1))
	(setq start (point))
	(if (not (setq end (emaxima-output-p)))
	    (progn
	      (goto-char (emaxima-cell-end))
	      (while (looking-at "^ *$") (forward-line -1))
	      (end-of-line)
	      (setq end (point)))
	  (progn
	    (goto-char end)
	    (while (looking-at "^ *$") (forward-line -1))
	    (end-of-line)
	    (setq end (point))))
        (set-buffer (get-buffer-create buffer))
        (goto-char (point-max))
        (insert-buffer-substring home-buffer start end)
        (insert "\n")))))

(defun emaxima-collate-cells (file part files parts &optional single)

  "Assemble cells marked with filename FILE in buffers with keys
`file:part' or, for part = null string (package cells), with key `file'.  The
names of all buffers are constructed with `emaxima-make-temp-name' and are
unique.  If PART is non-nil then do not collate cells with keys `FILE:PART'
and `FILE' (package cells).  Use FILES and PARTS for name completion \(see
`emaxima-get-filename' and `emaxima-get-partname'\).  If optional SINGLE is
non-nil, then collate just cells `FILE:PART' (PART must be non-nil).

The global `emaxima-buffer-alist' associates buffer names with keys.  It must
be initialized, typically with the buffer for key `FILE' or `FILE:PART',
according to whether PART is nil or not."

  (let ((home-buffer (current-buffer))
        this-part this-file key)
    (unwind-protect ; For error location
        (setq emaxima-error-point (point)) ; Go here if no error
        (progn

          ;; Scan buffer to construct buffers for all `file:part'
          (save-excursion
            (set-buffer emaxima-source-buffer) ; Collate from here
            (goto-char (point-min))
            (while (emaxima-forward-cell)
                   ;; We have a cell of the right type
                (forward-line -1) ; Move to \begin{...
                (if (not (looking-at "^\\\\maxima.*<.*:.*>"))
                    (forward-line 1) ; So we go to next cell next time through

                  ;; We have a marked cell
                  (setq this-file (emaxima-get-filename files))
                  (cond
                   ((not this-file)
                    (setq emaxima-error-point (point))
                    (error "Ambiguous filename"))
                   ((not (equal file this-file))
                    (forward-line 1)) ; So we go to next cell next time through
                   (t

                    ;; We have a cell of the right package filename
                    (setq this-part (emaxima-get-partname parts))
                    (cond
                     ((not this-part)
                      (setq emaxima-error-point (point))
                      (error "Ambiguous partname"))
                     ((and single (not (equal this-part part)))
                      (forward-line 1));Do only `file:part' for SINGLE non-nil
                     ((and part (equal this-part ""))
                      (forward-line 1));Cell assembly, 
                                       ;ignore package cell `FILE:'
                     ((and (not single) (equal this-part part))
                      (forward-line 1));Cell assembly, ignore cell `FILE:PART'
                     (t

                      ;; We have a cell with a valid partname
                      (forward-line 1) ; Move into cell
                      (if (equal this-part "")
                          (setq key file)
                        (setq key (concat file ":" this-part)))
                      (or
                       (assoc key emaxima-buffer-alist) ; buffer already created
                       (emaxima-replace-assoc
                        emaxima-buffer-alist
                        key (emaxima-make-temp-name)))

                      ;; Append cell contents to its buffer
                      (emaxima-append-cell-to-buffer
                       (cdr (assoc key emaxima-buffer-alist)))
                      
                      ) ; t on valid partname
                     ) ; cond on partname
                    ) ; t on right filename (package)
                   ) ; cond on filename
                  ) ; if a marked cell
              ) ; while still cells to process
            (set-buffer home-buffer)
            ) ; save excursion
          ) ; progn of unwind-protect body
      
      ;; unwind-protect tail:  Delete part files
      (goto-char emaxima-error-point))))

(defun emaxima-dereference-buffer (key files parts &optional noinit)
  "Resolve all references in buffer corresponding to KEY in alist
emaxima-buffer-alist, using FILES and PARTS for name completion.  If optional
NOINIT is nil, initialize global variable `emaxima-dereference-path' with KEY.
If NOINIT is non-nil, add KEY to `emaxima-dereference-path'. 
then references are collated in buffers and added to emaxima-buffer-alist if
necessary.  Use `emaxima-dereference-path' to check for self-reference and
report error if detected,"
  (let ((ref-found nil)
        (home-buffer (current-buffer))
        path-to-here
        ref-indent ref-key ref-buffer
        (key-buffer (cdr (assoc key emaxima-buffer-alist)))
        file part
        re-found
        )
    (or key-buffer (error "No cell `%s'" key))
    (set-buffer key-buffer)
    (goto-char (point-min))
    (if noinit
        t
      (setq noinit t)
      (setq emaxima-dereference-path (list key))
      )
    (setq path-to-here emaxima-dereference-path)
    (while (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (point-max) t)
      (setq re-found 1)
      (beginning-of-line)
      (setq ref-indent (emaxima-get-reference-indentation))
      (setq file (emaxima-get-filename files))
      (setq part (emaxima-get-partname parts))
      (setq ref-key (concat file ":" part))
      (if (emaxima-string-mem ref-key path-to-here)
            (emaxima-dereference-error (cons ref-key path-to-here)))
      (setq emaxima-dereference-path (cons ref-key path-to-here))
      (if (not (assoc ref-key emaxima-buffer-alist))
          ;; Construct buffer on the fly
          (progn
            (setq ref-buffer (emaxima-make-temp-name))
            (emaxima-replace-assoc emaxima-buffer-alist ref-key ref-buffer)
            (emaxima-collate-cells file part files parts t)
            )
        (setq ref-buffer (cdr (assoc ref-key emaxima-buffer-alist)))
        )
      (while (emaxima-dereference-buffer ref-key files parts noinit))
      (kill-line 1) ; Remove reference line
      (insert-buffer ref-buffer)
      (let ((indent-start (point))
            indent-end)
        (exchange-point-and-mark)
        (setq indent-end (point))
        (exchange-point-and-mark)
        (if ref-indent (indent-rigidly indent-start indent-end ref-indent))))
    (setq emaxima-dereference-path path-to-here)
    (set-buffer home-buffer)
    ref-found))

(defun emaxima-dereference-error (path)
  "Report package self-reference error, in PATH"
  (let ((cell (car path))
        (home-buffer (current-buffer))
        to-cell from-cell)
    (setq to-cell cell)
    (with-output-to-temp-buffer "*Help*" (message ""))
    (pop-to-buffer "*Help*")
    (insert "Self-reference detected assembling Maxima/TeX cell\n\n")
    (insert (concat "\t\t" to-cell "\n\n"))
    (insert "Here is how the self-reference happened:\n\n")
    (setq path (reverse path))
    (setq from-cell (car path))
    (insert (concat "\t" from-cell "\n"))
    (while (setq path (cdr path))
      (setq to-cell (car path))
      (if (equal cell to-cell)
          (insert (concat " !!! ->\t   -->\t" to-cell "\n"))
        (insert (concat "\t   -->\t" to-cell "\n")))
      (setq from-cell to-cell)
      )
    (pop-to-buffer home-buffer)
    (error "Self-reference detected")))

(defun emaxima-get-reference-indentation ()
  "Return indentation of reference on current line.
Line assumed tabified."
  (let (start end)
    (save-excursion
      (beginning-of-line)
      (setq start (point))
      (search-forward "<")
      (untabify start (point))
      (setq end (point))
      (beginning-of-line)
      (tabify (point) end)
      (- end start 1))))

(defun emaxima-insert-complete-name ()
  "Insert complete name in buffer for cell.
Return t if successful, else nil."
  (interactive)
  (let ((here (point))
        start end name text files parts
        )
    (save-excursion
      (beginning-of-line)
      (cond
       ((and ; partname
         (or
          (re-search-forward "^\\\\maxima<.*:[^\t]*" here t)
          (re-search-forward "^[ \t]*<.*:[^\t]*" here t))
         (equal here (point)))

        ;; This can take a second or two
        (message "Getting filenames...")
        (if (not (setq files (emaxima-get-filenames)))
            (error "No package filenames in document"))
        (message "")

        (search-backward "<")
        (forward-char 1)
        (setq start (point))
        (search-forward ":")
        (forward-char -1)
        (setq text (buffer-substring start (point)))
        (if (not (setq name (emaxima-complete-name text files)))
            (error "No matching package filename found"))

        ;; This can take several seconds for a document with many cells
        (message "Getting partnames")
        (setq parts (emaxima-get-partnames name files))
        (message "")

        (forward-char 1)
        (setq start (point)) ; New start, for partname deletion
        (setq text (buffer-substring (point) here))
        (if (not (setq name (emaxima-complete-name
                             (concat text "...")
                             parts)))
            (error "No matching package partname found"))
        (cond
         ((equal t name) ; Text is complete
          (setq name text))
         ((equal t (try-completion name parts)))
         (t ; Else, get completion
          (setq name
                (completing-read
                 "Partname (<space> to see partnames): "
                 parts nil t name))
          )
         ) ; cond: what kind of partname completion was done
        (delete-region start here)
        (insert (concat name ">"))) ; End of partname completion
       ((and ; filename
         (or (re-search-forward "^\\\\maxima<[^ \t]*" here t)
             (re-search-forward "^[ \t]*<[^ \t]*" here t))
         (equal here (point)))

        ;; This can take a second or two
        (message "Getting filenames...")
        (if (not (setq files (emaxima-get-filenames)))
            (error "No package filenames in document"))
        (message "")

        (re-search-backward "<")
        (forward-char 1)
        (setq start (point))
        (setq text (buffer-substring start here))
        (if (not (setq name (emaxima-complete-name
                             (concat text "...") ; completion form
                             files)))
            (error "No matching package filename found"))
        (cond
         ((equal t name) ; Text is complete
          (setq name text))
         ((equal t (try-completion name files)))
         (t ; Else, get completion
          (setq name
                (completing-read
                 "Filename (<space> to see filenames): "
                 files nil t name))
          (if (equal "" name) (error ""))))
        (delete-region start here)
        (insert (concat name ":")))
       (t
        ;;(error "Nothing to complete")
        nil))) ; save-excursion
    (if (not name)
        nil
      (goto-char (+ (point) (length name) 1))
      t)))

(defun emaxima-get-filenames ()
  "Return alist of package filenames for cells."
  (let (file files)
    (save-excursion
      (goto-char (point-min))
      (while (emaxima-forward-cell)
          (forward-line -1)
          (if (not (looking-at (concat "^\\\\maxima.*<.*>")))
              (forward-line 1) ; Cell not marked.  Get set for next one
            (if (setq file (emaxima-get-filename)) ; Only unabbreviated names
                (if files
                    (if (assoc file files)
                        nil ; already only
                      (setq files (cons (list file) files))) ; Add to alist
                  (setq files (list (list file))))) ; Start alist
            (forward-line 1)
            ) ; if a marked cell
        ) ; while cell to look at
      ) ; save-excursion
    files))

(defun emaxima-complete-name (text alist &optional exact)
  "Get full name corresponding to TEXT.
If text is a string ending in `...',
then the substring preceding the `...' is used with try-completion on ALIST.
An exact match is required if optional EXACT is t.
If text is just `...' and alist is length 1, then the car of its single element
is returned.
Oherwise nil is returned."
  (let (name try-name)
    (if (not (string-match "\\(\\.\\.\\.$\\)" text))
        (setq name text) ; don't do completion on full names
      (if (and
           (eq 0 (match-beginning 1)) ; just "..."
           (eq 1 (length alist))) ; a single package filename
          (setq name (car (car alist)))
        (setq try-name (substring text 0 (match-beginning 1)))
        (setq name (try-completion try-name alist)))
      (cond
       ((equal t name)
        (setq name try-name))
       ((and
         exact
         (not (equal t (try-completion name alist))))
        (setq name nil)))) ; Not an exact match, so error
    name))

(defun emaxima-get-partnames (file files)
  "Return alist of partnames for package FILE, using FILES for
filename completion."
  (let (cell-end cell-file part parts)
    (setq emaxima-error-point (point))
    (unwind-protect
        (save-excursion
          (goto-char (point-min))
          (while (emaxima-forward-cell)
              (setq cell-end (emaxima-cell-end))
              (forward-line -1)
              (if (not (looking-at
                       "^\\\\maxima.*<[^:].*:.*>"))
                  (forward-line 1) ; Not a marked cell
                (setq cell-file (emaxima-get-filename files))
                (if (not (equal file cell-file))
                    (forward-line 1) ; Wrong file
                  (while (and
                          (<= (point) cell-end)
                          (or
                           (re-search-forward
                            "^\\\\maxima.*<[^:].*:.*>" cell-end t)
                           (re-search-forward
                            "^ *\t*<[^:].*:.*>" cell-end t)))
                    (beginning-of-line) ; We have a filename-partname reference
                    (if (not (setq file (emaxima-get-filename files)))
                        (progn
                          (setq emaxima-error-point (point))
                          (error "Ambiguous filename")))
                    (if (not (equal cell-file file))
                        (progn
                          (setq emaxima-error-point (point))
                          (error "Reference must match cell filename: `%s'"
                                 cell-file)))
                    (setq part (emaxima-get-partname))
                    (if (not part)
                        nil ; Need full (unabbreviated) parts only, for alist
                      (if parts ; Update alist
                          (if (or
                               (equal part "")
                               (emaxima-string-mem part parts))
                              nil; already on list
                            (setq parts (append (list part) parts))) 
					; Add to alist
                        (if (not (equal part ""))
                            (setq parts (list part)))) ; Create alist
                      ) ; if an unabbreviated part                    
                    (forward-line 1)
                    ) ; while references to process in this cell
                  ) ; if a marked cell of this FILE
                ) ; if a marked cell
            ) ; while cells to process
          ); save-excursion
      (goto-char emaxima-error-point) ; unwind-protect form
      ) ; unwind-protect
    (setq parts (mapcar 'list parts)) ; Make list into an alist
    parts))

(defun emaxima-get-filename (&optional alist)
  "Get filename in package reference on current line.
If optional ALIST is supplied, use it for name completion.
Return nil if no name or error in name."
  (let ((match-re "\\(<\\)[^:]*\\(:\\)")
        (abbrev-re "\\.\\.\\.")
        beg text)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq text (buffer-substring beg (point)))
      (string-match match-re text)
      (setq text 
	    (substring text (+ 1 (match-beginning 1)) (+ -1 (match-end 2)))))
    (if alist
        (emaxima-complete-name text alist t)
      (if (string-match abbrev-re text)
          (if emaxima-abbreviations-allowed
              nil
            (setq emaxima-error-point (point))
	    (error 
  "Set emaxima-abbreviations-allowed (M-x set-variable) to use abbreviations")
            )
        text))))

(defun emaxima-get-partname (&optional alist)
  "Get partname in package reference on current line.
If optional ALIST is supplied, use it for name completion.
Return nil if no name or error in name."
  (let ((match-re "\\(:\\)\\([^>]*\\)")
        (abbrev-re "\\.\\.\\.")
        beg text)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq text (buffer-substring beg (point)))
      (string-match match-re text)
      (setq text (substring text (+ 1 (match-beginning 1)) (match-end 2))))
    (if alist
        (emaxima-complete-name text alist t)
      (if (string-match abbrev-re text)
          (if emaxima-abbreviations-allowed
              nil
            (setq emaxima-error-point (point))
	    (error 
   "Set emaxima-abbreviations-allowed (M-x set-variable) to use abbreviations")
            )
        text))))

(defun emaxima-string-mem (element list) ; memq doesn't work for strings
  "Returns t if string ELEMENT is in LIST of strings, else returns nil."
  (let (try
        (found nil))
    (while (and (setq try (car list)) (not found))
      (setq list (cdr list))
      (if (equal element try)
          (setq found t)))
    found))

(defun emaxima-reference-p ()
  "Return t if cell contains a cell reference, else retrun nil."
  (save-excursion
    (goto-char (emaxima-cell-start))
    (if (re-search-forward "^ *\t*<[^:].*:[^>].*>$" (emaxima-cell-end) t)
        t
      nil)))

;;; @@ EMaxima functions for "maxima" cells

(defun emaxima-get-cell-contents ()
  "Return the cell contents as a string."
  (if (not (emaxima-cell-p))
      (error "Not in Maxima cell"))
  (let ((home-buffer (current-buffer))
        assembled-file start end)
    (if (emaxima-reference-p)
          (save-excursion
            (widen) ; So cell references will be found
            (set-buffer (find-file-noselect
                         (emaxima-assemble-cell t)))
            (buffer-substring-no-properties (point-min) (point-max)))
      (save-excursion
	(goto-char (emaxima-cell-start))
        ;; Now I want to skip over any blank lines at the beginning of the cell
	(beginning-of-line)
	(while (looking-at "^ *$") (forward-line 1))
	(setq start (point))
        ;; as well as at the end of the cell
	(if (not (setq end (emaxima-output-p)))
	    (progn
	      (goto-char (emaxima-cell-end))
	      (while (looking-at "^ *$") (forward-line -1))
	      (end-of-line)
	      (setq end (point)))
	  (progn
	    (goto-char end)
	    (while (looking-at "^ *$") (forward-line -1))
	    (end-of-line)
	    (setq end (point)))))
      (buffer-substring-no-properties start end))))

(defun emaxima-send-cell ()
  "Send the current cell's contents to Maxima."
  (interactive)
  (if (not (emaxima-cell-p))
      (message "Not in cell.")
    (maxima-region (emaxima-cell-start) (emaxima-cell-end))))

(defun emaxima-update-cell (&optional tex)
  "Send the current cell's contents to Maxima, and return the results."
  (interactive "P")
  (save-excursion
    (emaxima-delete-output)
    (let ((end)
          (cell (emaxima-get-cell-contents)))
      (goto-char (emaxima-cell-end))
      (forward-line 1)
      (if tex
          (insert "\\outputtex\n")
        (insert "\\output\n"))
      (while (setq end (string-match "[$;]" cell))
        (maxima-string-nodisplay (substring cell 0 (1+ end)))
        (setq cell (substring cell (1+ end)))
        (if tex
            (progn
              (maxima-string-nodisplay emaxima-tex-string)
              (insert (maxima-last-output-tex-noprompt)))
          (insert (maxima-last-output)))))))

(defun emaxima-tex-update-cell ()
  "Send input to maxima and replace output with the result in TeX form.
Point must be in cell.  Output assumed to follow input,
separated by a emaxima-output-marker line."
  (interactive)
  (emaxima-update-cell t))

(defun emaxima-update-type-cell (tex)
  "Send input to Maxima and replace output with result.
Point must be in cell.  Output assumed to follow input,
separated by a emaxima-output-marker line."
  (if (not (emaxima-cell-p))
      (error "Not in Maxima cell"))
  (emaxima-update-cell tex))

(defun emaxima-replace-line-with-tex ()
  "Sends the current line to Maxima, and then replaces it with the Maxima
output in TeX form."
  (interactive)
  (maxima-send-line-nodisplay)
  (maxima-wait)
  (maxima-string-nodisplay emaxima-tex-string)
  (beginning-of-line)
  (insert "% ")
  (end-of-line)
  (newline)
  (insert (maxima-last-output-tex-noprompt)))

(defun emaxima-replace-line ()
  "Sends the current line to Maxima, and then replaces it with the Maxima
output."
  (interactive)
  (maxima-send-line-nodisplay)
  (maxima-wait)
  (beginning-of-line)
  (insert "% ")
  (end-of-line)
  (newline)
  (insert (maxima-last-output-noprompt)))

;;; @@ The mode

;;; First of all, I want to be able to change the keymap depending 
;;; on whether the point is in a cell or not.
;;; So I need one keymap for when in a cell, and one for when 
;;; not in a cell.
;;; Changing the keymaps doesn't seem to work, so I'll have to do 
;;; it on a key by key basis.
;;; maxima-add-keys will add the maxima keys.


;; First, find out what kind of TeX mode is being used.
(cond
 ((eq emaxima-use-tex 'auctex)
  (require 'tex-site)
  ;; I don't think this is the best thing to do...
  (load "latex")
  (setq texmode-map LaTeX-mode-map)
  (defun texmode () (latex-mode)))
 ((eq emaxima-use-tex 'tex)
  (require 'tex-mode)
  (setq texmode-map tex-mode-map)
  (defun texmode () (tex-mode)))
 (t
  (autoload 'text-mode "text-mode")
  (setq texmode-map text-mode-map)
  (defun texmode () (text-mode))))

;;; Now, define the keymap
(defvar emaxima-mode-map nil
  "The keymap for emaxima-mode")

(if emaxima-mode-map
    nil
  (let ((map (copy-keymap texmode-map)))
    (define-key map "\C-c\C-u" nil)
    (define-key map "\C-c+" 'emaxima-forward-cell)
    (define-key map "\C-c-" 'emaxima-backward-cell)
    (define-key map "\C-c\C-ua" 'emaxima-update-all)
    (define-key map "\C-c\C-uA" 'emaxima-tex-update-all)
    (define-key map "\C-c\C-uq" 'emaxima-eval-init)
    (define-key map "\C-c\C-ui" 'emaxima-update-init)
    (define-key map "\C-c\C-uI" 'emaxima-tex-update-init)
    (define-key map "\C-c\C-o" 'emaxima-create-cell)
    (define-key map "\C-c\C-ul" 'emaxima-replace-line)
    (define-key map "\C-c\C-uL" 'emaxima-replace-line-with-tex)
    (define-key map "\M-\C-k"  'maxima-stop)
    ;; And some emaxima keys that make sense in cells
    (define-key map "\C-c\C-s" 'emaxima-send-cell)
    (define-key map "\C-c\C-uc" 'emaxima-update-cell)
    (define-key map "\C-c\C-uC" 'emaxima-tex-update-cell)
    (define-key map "\C-c\C-d" 'emaxima-delete-output)
    (define-key map "\C-c\C-q" 'emaxima-toggle-init)
    (define-key map "\C-c\C-x" 'emaxima-package-part)
    (define-key map "\C-c@" 'emaxima-assemble)
    (define-key map "\C-c\C-h" 'maxima-help)
    (define-key map "\C-c\C-i" 'maxima-info)
    (define-key map [(control c) (control tab)] 'emaxima-insert-complete-name)
    (setq emaxima-mode-map map)))

(define-derived-mode emaxima-mode tex-mode  "EMaxima"
  "This is a mode intended to allow the user to write documents that
include Maxima code.  The file can be LaTeXed to produce nice 
looking output (although that isn't necessary, of course), and so the
mode is an extension of tex-mode (AucTeX).
The units of Maxima code that are worked with are \"cells\", which are 
delimited by \"\\maxima\" and \"\\endmaxima\". The cells can be evaluated 
individually, as a group, and the output can be returned as Maxima output 
or in TeX form.  Evaluating a cell and returning the output is called 
\"updating\" the cell.  This mode also supports some literate programming 
constructs.  (See the file \"EMaximaIntro.tex\" for more 
information.)
The commands for working with cells are:
 \\[emaxima-create-cell]  create a cell         
 \\[emaxima-update-all] update all the cells 
 \\[emaxima-tex-update-all] update all the cells in TeX form 
 \\[emaxima-forward-cell] go to the next cell 
 \\[emaxima-backward-cell] go to the previous cell
 \\[emaxima-eval-init] evaluate all  initialization cells
 \\[emaxima-update-init] update all the initialization cells
 \\[emaxima-tex-update-init] update all the initialization cells in TeX form

(With a prefix, C-u \\[emaxima-update-all] and C-u \\[emaxima-tex-update-all] will update the cells 
without prompting)
Since the Maxima output can be returned to the EMaxima buffer,
the buffer which runs the Maxima process is not shown.

Single lines can be evaluated:
 \\[emaxima-replace-line] replace the current line with Maxima output
 \\[emaxima-replace-line-with-tex] replace the current line with Maxima output in TeX form.

Within a cell, the following commands are available:\\<emaxima-maxima-map>
 \\[emaxima-delete-output]  delete the cell's output
 \\[emaxima-update-cell]  update a cell 
 \\[emaxima-tex-update-cell] update a cell in TeX form
 \\[emaxima-toggle-init] toggle initialization cells
 \\[emaxima-assemble]  assemble a cell which defines a package
 C-u \\[emaxima-assemble]  assemble a cell with references

Finally, the command \\[emaxima-mark-file-as-emaxima] will insert a 
%-*-EMaxima-*- at the beginning of the file (if there isn't one there 
already) so the file will begin in emaxima-mode next time it's opened.

\\{emaxima-mode-map}
"
  (when (or (eq emaxima-use-tex 'auctex) (eq emaxima-use-tex 'tex))
    (make-local-variable 'ispell-parser)
    (setq ispell-parser 'tex)
    (make-local-variable 'ispell-tex-p)
    (setq ispell-tex-p t))
  (if (eq emaxima-use-tex 'auctex)
    (progn
      (require 'font-latex)
      (add-hook 'emaxima-mode-hook 'font-latex-setup)))
  (run-hooks 'emaxima-mode-hook))

(if (eq emaxima-use-tex 'auctex)
    (put 'latex-mode 'font-lock-defaults 'emaxima-mode))
;; Now, some more font-locking
;; Some more fontlocking
;; First, fontify the \maxima and \endmaxima

(if (fboundp 'font-lock-add-keywords)
    (progn
      (defun emaxima-font-lock-cell (limit)
	"Used to fontify whatever's between \\maxima and \\endmaxima."
	(when (re-search-forward "\\\\maxima" 
				 limit t)
	  (let ((beg (match-end 0)) end)
	    (if (search-forward "\\endmaxima"
				limit 'move)
		(setq end (match-beginning 0))
	      (setq end (point)))
	    (store-match-data (list beg end))
	    t)))

      (font-lock-add-keywords 'emaxima-mode 
			      '((emaxima-font-lock-cell
				 (0 font-lock-function-name-face append t))))

      (font-lock-add-keywords 'emaxima-mode 
	    '(("\\(\\\\\\(endmaxima\\|output\\(tex\\)?\\|maxima\\)\\)"
	       . font-lock-keyword-face)))))


;;; Now, the menu.
(easy-menu-define emaxima-menu emaxima-mode-map
  "EMaxima mode menu"
  '("EMaxima"
    ("Cells"
     ["Create cell"  emaxima-create-cell (not (emaxima-cell-p))]
     ["Send cell"  emaxima-send-cell (emaxima-cell-p)]
     ["Update cell"   emaxima-update-cell (emaxima-cell-p)]
     ["TeX update cell"  emaxima-tex-update-cell (emaxima-cell-p)]
     ["Delete output"   emaxima-delete-output (emaxima-cell-p)]
     ["Toggle initialization"  emaxima-toggle-init (emaxima-cell-p)]
     ["Mark as package part" emaxima-package-part (emaxima-cell-p)]
     ["Insert complete name" emaxima-insert-complete-name (emaxima-cell-p)]
     ["Forward cell"  emaxima-forward-cell]
     ["Backwards cell"  emaxima-backward-cell])
    ("Update"
     ["Update line"   emaxima-replace-line (not (emaxima-cell-p))]
     ["Update all cells"  emaxima-update-all]
     ["Update initialization cells"  emaxima-update-init]
     "---"
     ["TeX update line"   emaxima-replace-line-with-tex (not (emaxima-cell-p))]
     ["TeX update all cells"  emaxima-tex-update-all]
     ["TeX update initialization cells"  emaxima-tex-update-init])
    ("Process"
     ["Start a Maxima process"   maxima-start
      (not (processp maxima-process))]
     ["Run Maxima on region"  maxima-region]
     ["Run Maxima on initialization cells"  emaxima-eval-init]
     ["Send string to Maxima" maxima-eval-string t]
     ["Reset the Maxima process" maxima-reset t]
     ["Kill Maxima process"  maxima-stop (processp maxima-process)])
    ("Misc"
     ["Indent region"   maxima-indent-region (emaxima-cell-p)]
     ["Short comment"  maxima-short-comment (emaxima-cell-p)]
     ["Long comment" maxima-long-comment (emaxima-cell-p)]
     ["Mark file as EMaxima"  emaxima-mark-file-as-emaxima])
    ("Web"
     ["Assemble cell"  emaxima-assemble-cell 
      (and (emaxima-cell-p) (emaxima-reference-p))]
     ["Assemble package"  emaxima-assemble-package 
      (and (emaxima-cell-p) (emaxima-reference-p))])
    ("Help"
     ["Apropos"   maxima-apropos]
     ["Help"  maxima-help])))

;;; The next line is necessary for XEmacs
(easy-menu-add emaxima-menu emaxima-mode-map)
  
;;; emaxima.el ends here
