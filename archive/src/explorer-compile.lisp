(load "sysdef.lisp")
(fs::set-logical-pathname-host "maxima-source"
			       :physical-host "rascal"
			       :translations
			       '(("maxima" "/usr2/maxima/src/")))
(fs::set-logical-pathname-host "cl-maxima-object"
			       :physical-host "clug"
			       :translations
			       '(("maxima" "maxima.ojbect;")))

(setf (get :maxima :object-path) #+ti "clug:maxima.object;foo.xld"
      #+symbolics "cl-maxima-object:maxima;foo.bin")
(setf (get :maxima :source-path)
      "maxima-source:maxima;foo.lisp")

(make:make :maxima :compile t :batch t)
