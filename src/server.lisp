;; Connect Maxima to a socket which has been opened by
;; some third party, typically a GUI program which supplies
;; input to Maxima.
;; Note that this code DOES NOT create a Maxima server:
;; Maxima is the client!

(in-package :maxima)

#+(or ecl sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (require 'sb-posix)
  (require 'sb-bsd-sockets))

(defvar $in_netmath nil)
(defvar $show_openplot t)
(defvar *socket-connection*)
(defvar $old_stdout)
(defvar $old_stderr)

#+ecl (defvar *old-stdin*)
#+ecl (defvar *old-stdout*)
#+ecl (defvar *old-sterr*)
#+ecl (defvar *old-term-io*)
#+ecl (defvar *old-debug-io*)

(defun setup-client (port &optional (host "localhost"))
  ;; The following command has to be executed on windows before
  ;; the connection is opened. If it isn't the first unicode 
  ;; character maxima wants to send causes sbcl to wait indefinitely.
  #+sbcl (setf sb-impl::*default-external-format* :utf-8)
  (multiple-value-bind (sock condition) (ignore-errors (open-socket host port))
    (unless sock
      ; It appears that we were unable to open a socket or connect to the
      ; specified port.
      (mtell (intl:gettext "~%Unable to connect Maxima to port ~:M.~%") port)
      (mtell (intl:gettext "Error: ~A~%") condition)
      ($quit))
    ;; Some lisps if the front-end dies by default don't quit but output an
    ;; error message to the front-end that (as the front-end doesn't exist
    ;; any more) causes an error message that...
    #+gcl (setq si::*sigpipe-action* 'si::bye)
    #+ecl (ext:set-signal-handler EXT:+SIGPIPE+ 'ext:quit)
    
    (setq *socket-connection* sock)
    (setq $old_stderr *error-output*
	  $old_stdout *standard-output*)
    #+ecl (setq *old-stdin*    *standard-input*
		*old-stdout*   *standard-output*
		*old-sterr*    *error-output*
		*old-term-io*  *terminal-io*
		*old-debug-io* *debug-io*)
    (setq *standard-input* sock)
    (setq *standard-output* sock)
    (setq *error-output* sock)
    (setq *terminal-io* sock)
    (setq *trace-output* sock)
    (format t "pid=~a~%" (getpid))
    (finish-output sock)
    (setq *debug-io* sock))
  (values))

(defun close-client ()
  #+ecl (setq *standard-input*  *old-stdin*
	      *standard-output* *old-stdout*
	      *error-output*    *old-sterr*
	      *terminal-io*     *old-term-io*
	      *debug-io*        *old-debug-io*)
  #+ecl (close *socket-connection*))

;;; from CLOCC: <http://clocc.sourceforge.net>
(defun open-socket (host port &optional bin)
  "Open a socket connection to `host' at `port'."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  #+(or gcl ccl)
  (declare (ignore bin))

  (let ((host (etypecase host
                (string host)
                ;; Can't actually handle this case for lack of HOSTENT-NAME and RESOLVE-HOST-IPADDR.
                ;;(integer (hostent-name (resolve-host-ipaddr host)))))
                (integer (merror (intl:gettext "OPEN-SOCKET: can't handle integer host argument (host=~M)~%") host))))
	#+(and ccl openmcl-unicode-strings)
	(ccl:*default-socket-character-encoding* :utf-8))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (socket:socket-connect port host :element-type
				   (if bin '(unsigned-byte 8) 'character))
    #+scl (sys:make-fd-stream (ext:connect-to-inet-socket host port)
			      :input t :output t :element-type
			      (if bin '(unsigned-byte 8) 'character))
    #+cmu (sys:make-fd-stream (ext:connect-to-inet-socket host port)
			      :input t :output t :element-type
			      (if bin '(unsigned-byte 8) 'character)
			      #+unicode :external-format #+unicode :utf-8
			      :buffering :line)
    #+(or ecl sbcl) (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
					:type :stream :protocol :tcp)))
	     (sb-bsd-sockets:socket-connect
	      socket (sb-bsd-sockets:host-ent-address
		      (sb-bsd-sockets:get-host-by-name host)) port)
	     (sb-bsd-sockets:socket-make-stream
	      socket :input t :output t :buffering (if bin :none :line)
	      :element-type (if bin '(unsigned-byte 8) 'character)
              #+sb-unicode :external-format #+sb-unicode :utf-8))
    #+gcl (si::socket port :host host)
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    #+ccl (ccl::make-socket :remote-host host :remote-port port)
    #-(or allegro clisp cmu scl sbcl gcl lispworks ecl ccl)
    (error 'not-implemented :proc (list 'open-socket host port bin))))



(defun start-client (port &optional (host "localhost"))
  (format t (intl:gettext "Connecting Maxima to server on port ~a~%") port)
  (setq $in_netmath t)
  (setq $show_openplot nil)
  (setup-client port host))

#-gcl
(defun getpid-from-environment ()
  (handler-case
      (values (parse-integer (maxima-getenv "PID")))
    ((or type-error parse-error) () -1)))

;;; For gcl, getpid imported from system in maxima-package.lisp
#-gcl
(defun getpid ()
#+clisp (os:process-id)
#+(or cmu scl) (unix:unix-getpid)
#+sbcl (sb-unix:unix-getpid)
#+gcl (system:getpid)
#+openmcl (ccl::getpid)
#+lispworks (system::getpid)
#+ecl (si:getpid)
#+ccl (ccl::getpid)
#+allegro (excl::getpid)
#-(or clisp cmu scl sbcl gcl openmcl lispworks ecl ccl allegro)
  (getpid-from-environment)
)

#+(or gcl clisp cmu scl sbcl lispworks ecl ccl allegro)
(defun xchdir (w)
  #+clisp (ext:cd w)
  #+gcl (si::chdir w)
  #+(or cmu scl) (unix::unix-chdir w)
  #+sbcl (sb-posix:chdir w)
  #+lispworks (hcl:change-directory w)
  #+ecl (si:chdir w)
  #+ccl (ccl:cwd w)
  #+allegro (excl:chdir w)
  )
