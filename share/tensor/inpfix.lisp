(DEFUN MREAD (&REST READ-ARGS)
  #+NIL (let ((*mread-prompt-internal* *mread-prompt*)
          (si:*ttyscan-dispatch-table *macsyma-ttyscan-operators*))
      (declare (special *mread-prompt-internal*))
      (SI:READ-APPLY ':MREAD #'MREAD-RAW (coerce READ-ARGS 'sys:vector)
             '(:prompt mread-prompter)
             '(:reprompt mread-prompter)))
  #+cl (progn
     (when *mread-prompt*
           (and *parse-window* (setf (car *parse-window*) nil
                     *parse-window* (cdr *parse-window*)))
           (princ *mread-prompt*) (FORCE-OUTPUT *standard-output*))
     (#+lispm read-apply #-lispm apply 'mread-raw-NL read-args)
            )
  #-(or NIL cl)
  (READ-APPLY #'MREAD-RAW READ-ARGS)
)


(DEFUN MREAD-RAW-NL (*PARSE-STREAM* &OPTIONAL *MREAD-EOF-OBJ*)
  (LET ((SCAN-BUFFERED-TOKEN (LIST NIL))
    *parse-tyi*
    )
    (IF (EQ SCAN-BUFFERED-TOKEN ;; a handly unique object for the EQ test.
        (PEEK-ONE-TOKEN-G T SCAN-BUFFERED-TOKEN))
    *MREAD-EOF-OBJ*
    (DO ((LABELS ())
         (INPUT (PARSE '$ANY 0.) (PARSE '$ANY 0.)))
        (NIL)
      (CASE (FIRST-C)
        ((|$;| |$$|)
          ;force a separate line info structure
         (fresh-line *standard-output*)
         (SETF *CURRENT-LINE-INFO* NIL)
         (RETURN (LIST (MHEADER (POP-C))
               (IF LABELS (CONS (MHEADER '|$[|) (NREVERSE LABELS)))
               INPUT)))
        ((|$&&|)
         (POP-C)
         (IF (SYMBOLP INPUT)
         (PUSH INPUT LABELS)
         (MREAD-SYNERR "Invalid && tag. Tag must be a symbol")))
        (T
         (PARSE-BUG-ERR 'MREAD-RAW)))))))
