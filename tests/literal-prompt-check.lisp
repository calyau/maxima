;;; Regression helpers for flat literal prompt fragments.
(in-package :maxima)

(defun literal-prompt-format-check (pieces expected)
  (let ((*prompt-prefix* "<prompt>") (*prompt-suffix* "</prompt>")
        (*print-base* 16) (*print-radix* t) (*print-circle* t))
    (let ((wanted (concatenate 'string "<prompt>" expected "</prompt>"))
          (before (copy-tree pieces))
          (stream-result :unwritten))
      (and (string= (default-format-prompt nil "~{~A~}" (list pieces)) wanted)
           (string= (with-output-to-string (output)
                      (setq stream-result
                            (default-format-prompt output "~{~A~}" (list pieces)))) wanted)
           (null stream-result)
           (= *print-base* 16) *print-radix* *print-circle*
           (equal pieces before)))))

(defun $literal_prompt_default_check ()
  (let ((shared (list 1 2)))
    (every (lambda (entry) (literal-prompt-format-check (first entry) (second entry)))
           (list (list nil "")
                 (list '("Choose " 42 ": ") "Choose 42: ")
                 (list '("~{~A~} ~~ ~M" -123456789) "~{~A~} ~~ ~M-123456789")
                 (list '("Größe α: " 2.5) "Größe α: 2.5")
                 (list '($x " " foo " " nil " " t " " 1/3) "$x foo nil t 1/3")
                 (list (list (list shared shared)) "((1 2) (1 2))")))))

(defun literal-prompt-observe (message input-text &optional formatter)
  (with-input-from-string (input input-text)
    (let* ((output (make-string-output-stream))
           (*query-io* (make-two-way-stream input output))
           (*standard-output* (make-broadcast-stream))
           (*prompt-prefix* "<prompt>") (*prompt-suffix* "</prompt>")
           (*general-display-prefix* "<resume>")
           ($alt_format_prompt formatter)
           ($errormsg nil) ($error nil)
           (answer (errcatch (retrieve message t)))
           (question-error $error)
           (following (and answer (mread-noprompt *query-io* nil))))
      (list answer following (get-output-stream-string output) question-error
            (file-position input)))))

(defun $literal_prompt_read_check ()
  (and
   (equal (subseq (literal-prompt-observe '((mtext) "Value " 42 "?") "123;456;") 0 4)
          (list '(123) 456 (format nil "<prompt>Value 42?</prompt>~%<resume>") nil))
   (equal (subseq (literal-prompt-observe '((mtext)) "123;456;") 0 4)
          (list '(123) 456 (format nil "<prompt></prompt>~%<resume>") nil))
   ;; Atomic and NIL messages retain their existing newline behavior.
   (equal (subseq (literal-prompt-observe "Question" "123;456;") 0 4)
          (list '(123) 456 (format nil "<prompt>Question</prompt>~%<resume>") nil))
   (equal (subseq (literal-prompt-observe nil "123;456;") 0 4)
          (list '(123) 456 "<prompt></prompt><resume>" nil))))

(defun $literal_prompt_custom_check (&optional delegate-p)
  (let* ((parts (list "Value " 42 "?"))
         (message (cons '(mtext) parts))
         (calls 0) (protocol-ok nil)
         (*print-base* 16) (*print-radix* nil)
         (observation
          (literal-prompt-observe
           message "123;456;"
           (lambda (destination control arguments)
             (incf calls)
             (setq protocol-ok
                   (and (streamp destination) (string= control "~{~A~}")
                        (= (length arguments) 1) (eq (first arguments) parts)))
             (let ((text (if delegate-p
                             (default-format-prompt nil control arguments)
                             (concatenate 'string "<custom>"
                                          (apply #'format nil control arguments)
                                          "</custom>"))))
               (write-string text destination)
               text)))))
    (and (= calls 1) protocol-ok (= *print-base* 16) (not *print-radix*)
         (equal (subseq observation 0 4)
                (list '(123) 456
                      (if delegate-p
                          (format nil "<prompt>Value 42?</prompt>~%<resume>")
                          (format nil "<custom>Value 2A?</custom>~%<resume>"))
                      nil)))))

(defun $literal_prompt_refusal_check ()
  (let ((calls 0))
    (let (($alt_format_prompt
           (lambda (destination control arguments)
             (declare (ignore destination control arguments))
             (incf calls) "unexpected callback")))
      (and
       (every
        (lambda (execution-mode)
          (parallel-input-refused-p
           (parallel-input-observe
            (lambda ()
              (parallel-input-run
               (lambda () (retrieve '((mtext) "Do not ask " 42) t)) execution-mode))
            "111;" "222;")))
        (append '($caller $fallback $public)
                (when (parallel-threads-p) '($worker))))
       (zerop calls)))))

(defun $literal_prompt_error_check ()
  (let ((eof (literal-prompt-observe '((mtext) "EOF?") "")))
    (and (null (first eof))
         (string= (third eof) (format nil "<prompt>EOF?</prompt>~%<resume>"))
         (equal (fourth eof) '((mlist simp) "RETRIEVE: End of file encountered."))
         (zerop (fifth eof))
         (every
          (lambda (arguments)
            (let ((*print-base* 16) (*print-radix* t) (*print-circle* t)
                  (*prompt-prefix* "<prompt>") (*prompt-suffix* "</prompt>")
                  (output (make-string-output-stream)))
              (and (handler-case
                       (progn (default-format-prompt output "~{~A~}" arguments) nil)
                     (error () t))
                   (string= (get-output-stream-string output) "")
                   (= *print-base* 16) *print-radix* *print-circle*)))
          '(nil (42) (("prefix" . 42)))))))

(defun $literal_prompt_random_check ()
  ;; Independent references use CL's explicit decimal directive for numbers.
  ;; Fragment text stays data even when it contains formatting instructions.
  (loop with state = 3781 repeat 128 always
        (let ((pieces nil) (expected (make-string-output-stream)))
          (dotimes (index (mod state 12))
            (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
            (let ((piece (if (oddp index) " ~A{α} "
                             (* (- state (expt 2 31)) (expt 10 (mod state 30))))))
              (push piece pieces)
              (if (integerp piece) (format expected "~D" piece)
                  (write-string piece expected))))
          (setq state (mod (+ (* state 1664525) 1013904223) (expt 2 32)))
          (literal-prompt-format-check (nreverse pieces) (get-output-stream-string expected)))))

(defun $literal_prompt_long_check ()
  (let ((pieces (make-list 2048 :initial-element "x")))
    (literal-prompt-format-check pieces (make-string 2048 :initial-element #\x))))
