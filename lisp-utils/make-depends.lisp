(in-package :mk)

(defvar *oos-dependency-stream* t
  "Output stream for list-dependencies-operation")

(defvar *oos-dependency-targets* nil
  "Output targets for list-dependencies-operation")

(defun list-dependencies-operation (component force)
  (declare (ignore force))
  (dolist (target *oos-dependency-targets*)
    (let ((dep-name (format nil "~A.~A"
                            (component-source-pathname component)
                            (component-source-extension component))))
      (unless (string= target dep-name)
        (format *oos-dependency-stream* "~A : ~A~%" target dep-name)))))

(component-operation :list-dependencies 'list-dependencies-operation)

(defun create-dependency-file (targets file)
  (with-open-file (dependency-file file :direction :output :if-exists :supersede)
    (setq *oos-dependency-targets*
          (if (stringp targets)
              (list targets)
              targets))
    (setq *oos-dependency-stream* dependency-file)
    (operate-on-system 'maxima :list-dependencies)
    (format dependency-file "~%")))
