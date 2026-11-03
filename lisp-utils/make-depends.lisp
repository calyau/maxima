(in-package :mk)

(defvar *oos-dependency-stream* t
  "Output stream for list-dependencies-operation")

(defvar *oos-dependency-targets* nil
  "Output targets for list-dependencies-operation")

(defvar *oos-dependency-root* nil
  "Directory of maxima.system. Sources are listed relative to it, where
make finds them in the build directory or, out of tree, through VPATH.")

(defun list-dependencies-operation (component force)
  (declare (ignore force))
  (dolist (target *oos-dependency-targets*)
    (let ((dep-name (enough-namestring
                     (component-full-pathname component :source)
                     *oos-dependency-root*)))
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
    (find-system 'maxima :load)
    (setq *oos-dependency-root*
          (make-pathname :name nil :type nil :version nil
                         :defaults (system-definition-pathname 'maxima)))
    (operate-on-system 'maxima :list-dependencies)
    (format dependency-file "~%")))
