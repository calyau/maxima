;;;; check-dependencies.lisp -- verify src/maxima.system's dependency graph
;;;;
;;;; DEFSYSTEM normally assumes that anything compiled after a file may
;;;; have been compiled against it, and rebuilds accordingly.  A component
;;;; marked :DEPENDENCIES-COMPLETE T switches that off and trusts the
;;;; declared :DEPENDS-ON edges instead, so an edge that is real but
;;;; undeclared makes an incremental build load a stale fasl.  Nothing in
;;;; the testsuite can notice that: the answers stay right until someone
;;;; changes the file that was never rebuilt.
;;;;
;;;; This checks the claim against what the compiler actually recorded.
;;;; Run it in a built Maxima image; it needs SBCL's xref data, so it is
;;;; SBCL-only.
;;;;
;;;;     sbcl --core src/binary-sbcl/maxima.core   ; or ./maxima-local
;;;;     (load "lisp-utils/check-dependencies.lisp")
;;;;     (maxima-depcheck:check-and-exit "/path/to/maxima/")
;;;;
;;;; A dependency counts only when it can change how the dependent file is
;;;; COMPILED.  Three things do that: a macro, an inline function or
;;;; structure accessor, and -- the one the module comments in
;;;; maxima.system tend to miss -- a call to a DEFMFUN.  DEFMFUN defines a
;;;; compiler-macro that rewrites ($foo ...) into (FOO-IMPL ...) at the
;;;; call site, so an ordinary Maxima-level call compiled after the callee
;;;; is a compile-time dependency, not just a run-time one.  A plain DEFUN
;;;; call, or a read of a special variable, is late-bound and does not
;;;; count.

(require :sb-introspect)

(defpackage #:maxima-depcheck
  (:use #:common-lisp)
  (:export #:check #:check-and-exit))

(in-package #:maxima-depcheck)

;;; ------------------------------------------------------------------
;;; Reaching into DEFSYSTEM.  Its package does not exist when this file
;;; is read, so every name is looked up at run time.

(defun mkf (name)
  (or (and (find-package "MK") (find-symbol name "MK"))
      (error "check-dependencies: MK::~A not found; is defsystem.lisp loaded?"
             name)))

(defun cname (c) (funcall (mkf "COMPONENT-NAME") c))
(defun ctype (c) (funcall (mkf "COMPONENT-TYPE") c))
(defun ckids (c) (funcall (mkf "COMPONENT-COMPONENTS") c))
(defun cdeps (c) (funcall (mkf "COMPONENT-DEPENDS-ON") c))
(defun ccomplete-p (c) (funcall (mkf "COMPONENT-DEPENDENCIES-COMPLETE") c))
(defun cfile (c)
  (ignore-errors (funcall (mkf "COMPONENT-FULL-PATHNAME") c :source)))

(defun dep-name (d)
  "A :DEPENDS-ON entry is a string, a symbol, or an already-resolved component."
  (cond ((stringp d) d)
        ((symbolp d) (string d))
        (t (ignore-errors (string (cname d))))))

(defun load-maxima-system (root)
  "Load defsystem from ROOT and return the maxima system's component tree."
  (unless (find-package "MK")
    (load (merge-pathnames "lisp-utils/defsystem.lisp" root)))
  (funcall (mkf "ADD-REGISTRY-LOCATION") (merge-pathnames "src/" root))
  (funcall (mkf "FIND-SYSTEM") 'maxima :load))

;;; ------------------------------------------------------------------
;;; The tree

(defstruct (node (:conc-name n-))
  component parent kids (files '()) (order nil))

(defun build-tree (root-component)
  "Return (values root-node all-nodes file->node compile-order).
DEFSYSTEM walks COMPONENT-COMPONENTS in order, so a depth-first walk in
listed order is the compile order; :DEPENDS-ON decides what is rebuilt,
never the sequence."
  (let ((all '()) (file->node (make-hash-table :test 'equal)) (counter 0))
    (labels ((key (path) (and path (namestring path)))
             (walk (c parent)
               (let ((n (make-node :component c :parent parent)))
                 (push n all)
                 (setf (n-kids n) (mapcar (lambda (k) (walk k n)) (ckids c)))
                 (when (eq (ctype c) :file)
                   (let ((k (key (cfile c))))
                     (when k
                       (setf (n-order n) (incf counter))
                       (setf (n-files n) (list k))
                       (setf (gethash k file->node) n))))
                 ;; a module's files are its children's files
                 (unless (n-files n)
                   (setf (n-files n)
                         (mapcan (lambda (k) (copy-list (n-files k)))
                                 (n-kids n))))
                 n)))
      (let ((root (walk root-component nil)))
        (values root (nreverse all) file->node)))))

(defun earliest-order (node)
  (let ((best nil))
    (labels ((walk (n)
               (when (n-order n)
                 (setf best (if best (min best (n-order n)) (n-order n))))
               (mapc #'walk (n-kids n))))
      (walk node))
    best))

(defun latest-order (node)
  (let ((best nil))
    (labels ((walk (n)
               (when (n-order n)
                 (setf best (if best (max best (n-order n)) (n-order n))))
               (mapc #'walk (n-kids n))))
      (walk node))
    best))

;;; ------------------------------------------------------------------
;;; Compile-time edges, from the compiler's own cross-reference data

(defun compile-affecting-callee-p (sym)
  (or (and (compiler-macro-function sym) t)
      (ignore-errors
        (and (member (sb-int:info :function :inlinep sym) '(:inline inline)) t))
      ;; DEFSTRUCT accessors and anything else the compiler open-codes
      ;; through a source transform.
      (ignore-errors
        (and (sb-int:info :function :source-transform sym) t))))

(defun xref-sanity-check ()
  "Fail loudly rather than reporting a vacuous pass if xref data is absent."
  (let ((probe (ignore-errors
                 (sb-introspect:who-calls (find-symbol "SIMPLIFYA" "MAXIMA")))))
    (unless probe
      (error "check-dependencies: no xref data in this image (who-calls ~
              MAXIMA::SIMPLIFYA returned nothing).  The check cannot run; ~
              it would pass without examining anything."))))

(defun collect-edges (file->node)
  "FROM-node -> TO-node -> list of (kind . symbol).
Compile-time edges only."
  (let ((edges (make-hash-table :test 'eq))
        (seen (make-hash-table :test 'eq)))
    (flet ((note (from-path to-node kind sym)
             (let ((from-node (gethash (and from-path (namestring from-path))
                                       file->node)))
               (when (and from-node to-node (not (eq from-node to-node)))
                 (let ((per-from (or (gethash from-node edges)
                                     (setf (gethash from-node edges)
                                           (make-hash-table :test 'eq)))))
                   (push (cons kind sym) (gethash to-node per-from)))))))
      (do-all-symbols (sym)
        (unless (gethash sym seen)
          (setf (gethash sym seen) t)
          (when (fboundp sym)
            (let* ((macro-p (and (macro-function sym) t))
                   (def (ignore-errors
                          (sb-introspect:find-definition-sources-by-name
                           sym (if macro-p :macro :function))))
                   (to (and def
                            (gethash
                             (let ((p (sb-introspect:definition-source-pathname
                                       (first def))))
                               (and p (namestring p)))
                             file->node))))
              (when to
                (cond
                  (macro-p
                   (dolist (r (ignore-errors
                                (sb-introspect:who-macroexpands sym)))
                     (note (sb-introspect:definition-source-pathname (cdr r))
                           to :macro sym)))
                  ((compile-affecting-callee-p sym)
                   (dolist (r (ignore-errors (sb-introspect:who-calls sym)))
                     (note (sb-introspect:definition-source-pathname (cdr r))
                           to :call sym))))))))))
    edges))

(defun hard-refs (edges from to)
  "Compile-time references in FROM to things defined in TO, but only when
FROM is actually compiled after TO -- otherwise the compiler had not seen
the definition and no coupling was created."
  (let ((fo (earliest-order from)) (to-o (latest-order to)))
    (when (and fo to-o (> fo to-o))
      (let ((per-from (gethash from edges)))
        (and per-from (gethash to per-from))))))

;;; ------------------------------------------------------------------
;;; Global compiler policy
;;;
;;; A top-level (DECLAIM (OPTIMIZE ...)) is global: it stays in force for
;;; every file compiled after it, not just the rest of its own file.  That
;;; is a compile-time dependency on every later file at once, which no
;;; :DEPENDS-ON can express and which no cross-reference records -- a
;;; proclamation is not a reference to a symbol, so the xref pass above is
;;; blind to it.  It has to be checked directly.
;;;
;;; The convention this enforces is that a file which changes the policy
;;; restores it before its end, so the proclamations come in pairs.

(defun optimize-proclamations (path)
  "Line numbers of top-level OPTIMIZE proclamations in PATH.
Scanned textually rather than with READ: every top-level form in src/
starts at column 0, and reading these files needs the Maxima readtable."
  (let ((hits '()) (n 0))
    (with-open-file (s path :if-does-not-exist nil)
      (when s
        (loop for line = (read-line s nil nil)
              while line
              do (incf n)
                 (when (and (plusp (length line))
                            (char= (char line 0) #\()
                            (or (search "(declaim (optimize" line)
                                (search "(proclaim '(optimize" line)
                                (search "(proclaim (quote (optimize" line)))
                   (push n hits)))))
    (nreverse hits)))

(defun policy-leaks (root)
  "Files that change the global policy an odd number of times, i.e. leave
it changed for whatever the build compiles next."
  (let ((out '()))
    (labels ((walk (n)
               (when (and (eq (ctype (n-component n)) :file) (n-files n))
                 (let* ((path (first (n-files n)))
                        (hits (optimize-proclamations path)))
                   (when (oddp (length hits))
                     (push (list (cname (n-component n)) path hits) out))))
               (mapc #'walk (n-kids n))))
      (walk root))
    (nreverse out)))

;;; ------------------------------------------------------------------
;;; The invariant

(defun declared-closure (node siblings)
  "Names NODE depends on, transitively through its sibling set."
  (let ((by-name (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (todo '()))
    (dolist (s siblings)
      (setf (gethash (string (cname (n-component s))) by-name) s))
    (dolist (d (cdeps (n-component node)))
      (let ((n (dep-name d))) (when n (push n todo))))
    (loop while todo
          for d = (pop todo)
          unless (gethash d seen)
            do (setf (gethash d seen) t)
               (let ((s (gethash d by-name)))
                 (when s
                   (dolist (dd (cdeps (n-component s)))
                     (let ((n (dep-name dd))) (when n (push n todo)))))))
    seen))

(defun implicit-edge-p (parent earlier)
  "Would DEFSYSTEM still add its own 'compiled after a changed file' edge
from EARLIER to a later sibling?  See OPERATE-ON-COMPONENTS: the parent
being complete removes them for the whole child list, and a complete child
does not mark itself as changed for the siblings that follow it."
  (and (not (ccomplete-p (n-component parent)))
       (not (ccomplete-p (n-component earlier)))))

(defun violations (root edges)
  (let ((out '()))
    (labels ((walk (parent)
               (let ((kids (n-kids parent)))
                 (dolist (a kids)
                   (let ((declared (declared-closure a kids)))
                     (dolist (b kids)
                       (unless (or (eq a b)
                                   (gethash (string (cname (n-component b)))
                                            declared)
                                   (implicit-edge-p parent b))
                         (let ((refs (hard-refs edges a b)))
                           (when refs
                             (push (list (cname (n-component parent))
                                         (cname (n-component a))
                                         (cname (n-component b))
                                         refs)
                                   out)))))))
                 (mapc #'walk kids))))
      (walk root))
    (nreverse out)))

;;; ------------------------------------------------------------------
;;; Reporting

(defun report-policy-leaks (leaks stream)
  (when leaks
    (format stream "~&check-dependencies: ~D file~:P change~:[~;s~] the global ~
                    compiler policy without restoring it.~2%"
            (length leaks) (= 1 (length leaks)))
    (dolist (l leaks)
      (destructuring-bind (name path lines) l
        (declare (ignore path))
        (format stream "  ~A.lisp proclaims OPTIMIZE at line~P ~{~D~^, ~}~%"
                name (length lines) lines)
        (format stream "      DECLAIM is global: every file compiled after ~
                        this one inherits~%      the policy, and no ~
                        :DEPENDS-ON can express that.  Restore it with a~%~
                        ~6Tsecond proclamation at the end of the file.~2%")))))

(defun report (violations stream)
  (if (null violations)
      (format stream "~&check-dependencies: no undeclared compile-time ~
                      dependencies found.~%")
      (progn
        (format stream "~&check-dependencies: ~D undeclared compile-time ~
                        dependenc~:@P found.~2%" (length violations))
        (dolist (v violations)
          (destructuring-bind (parent from to refs) v
            (format stream "  module ~A: ~A is compiled against ~A ~
                            but does not declare it~%" parent from to)
            (let ((shown (sort (remove-duplicates (copy-list refs)
                                                  :test #'equal)
                               #'string<
                               :key (lambda (r) (string (cdr r))))))
              (dolist (r (subseq shown 0 (min 6 (length shown))))
                (format stream "        ~(~6A~) ~A~%" (car r) (cdr r)))
              (when (> (length shown) 6)
                (format stream "        ... and ~D more~%"
                        (- (length shown) 6))))
            (format stream "      fix: give ~A  :depends-on (\"~A\")~2%"
                    from to))))))

(defun check (root &key (stream *standard-output*))
  "Verify maxima.system under ROOT.  Returns the list of violations."
  (xref-sanity-check)
  (let ((system (load-maxima-system (pathname root))))
    (multiple-value-bind (tree nodes file->node) (build-tree system)
      (declare (ignore nodes))
      (when (zerop (hash-table-count file->node))
        (error "check-dependencies: no source files resolved from ~
                maxima.system."))
      (let ((edges (collect-edges file->node)))
        ;; If maxima.system's pathnames and the ones the compiler recorded
        ;; do not agree -- a VPATH build, a symlinked tree -- every lookup
        ;; misses and the check passes without having examined anything.
        (when (zerop (hash-table-count edges))
          (error "check-dependencies: resolved ~D source files but found no ~
                  compile-time references between any of them.  The source ~
                  pathnames in maxima.system and the ones recorded by the ~
                  compiler are probably not the same; the check would pass ~
                  vacuously."
                 (hash-table-count file->node)))
        (let ((v (violations tree edges))
              (leaks (policy-leaks tree)))
          (report v stream)
          (report-policy-leaks leaks stream)
          (append v leaks))))))

(defun check-and-exit (root)
  (let ((v (handler-case (check root)
             (error (e)
               (format *error-output* "~&check-dependencies: ~A~%" e)
               (sb-ext:exit :code 2 :abort t)))))
    (sb-ext:exit :code (if v 1 0) :abort t)))
