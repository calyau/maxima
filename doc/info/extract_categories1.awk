BEGIN {
   print "(defvar *categories* (make-hash-table :test 'equal))"
   print "(defvar *filenamebase*)"
   print "(defvar items)"
   print "(defvar fn)"
   print "(defun foo (lst)"
   print "  (loop :for x :in lst"
   print "        :collect (list *filenamebase* x)))"
}

{ print; }

END {
   print "(loop :for key :being :the :hash-keys of *categories* :using (hash-value value) :do"
   print "      (setq fn (concatenate 'string \"Category-\" key \".texi\"))"
   print "      (with-open-file (out-stream fn :direction :output :if-exists :overwrite :if-does-not-exist :create)"
   print "        (format out-stream \"~&@anchor{Category: ~A}\" key)"
   print "        (format out-stream \"~&@opencatbox\")"
   print "        (format out-stream \"~&@b{Category: ~A}~%~%\" key)"
   print "        (setq items value)"
   print "        (cond ((> (length items) 0)"
   print "               (setq items (sort items #'string< :key #'third))"
   print "               (loop :for x :in items"
   print "                     :for y :from 1"
   print "                     :when (> y 1)"
   print "                     :do (format out-stream \"~&@html~%&middot;~%@end html\")"
   print "                     :do (format out-stream \"~&@ref{Item: ~A/~A/~A, ~A}\" (first x) (first (second x)) (second (second x)) (second (second x))))))"
   print "        (format out-stream \"~&@closecatbox\")))"
   print ""
}

