;; very simple server started on port

(in-package :maxima)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)		    ;not needed here for a recent SBCL
  (require 'sb-posix)
  (require 'sb-bsd-sockets))

(defvar $in_netmath nil)
(defvar $show_openplot t)
(defvar *socket-connection*)

(defun setup-server (port &optional (host "localhost"))
  (let* ((sock (open-socket host port)))
    #+gcl (setq si::*sigpipe-action* 'si::bye)
    (setq *socket-connection* sock)
    (setq *standard-input* sock)
    (setq *standard-output* sock)
    (setq *error-output* sock)
    (setq *terminal-io* sock)
    (setq *trace-output* sock)
    (format t "pid=~a~%" (getpid))
    (force-output sock)
    (setq *debug-io* sock))
  (values))

;;; from CLOCC: <http://clocc.sourceforge.net>
(defun open-socket (host port &optional bin)
  "Open a socket connection to `host' at `port'."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (socket:socket-connect port host :element-type
				   (if bin '(unsigned-byte 8) 'character))
    #+(or cmu scl) (sys:make-fd-stream (ext:connect-to-inet-socket host port)
				       :input t :output t :element-type
				       (if bin '(unsigned-byte 8) 'character))
    #+sbcl (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
					:type :stream :protocol :tcp)))
	     (sb-bsd-sockets:socket-connect
	      socket (sb-bsd-sockets:host-ent-address
		      (sb-bsd-sockets:get-host-by-name host)) port)
	     (sb-bsd-sockets:socket-make-stream
	      socket :input t :output t :buffering (if bin :none :line)
	      :element-type (if bin '(unsigned-byte 8) 'character)))
    #+gcl (si::socket port :host host)
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    #-(or allegro clisp cmu scl sbcl gcl lispworks)
    (error 'not-implemented :proc (list 'open-socket host port bin))))



(defun start-server (port &optional (host "localhost"))
  (format t "jfa: starting server on port ~a~%" port)
  (setq $in_netmath t)
  (setq $show_openplot nil)
  (setup-server port host))

#-gcl
(defun getpid-from-environment ()
  (handler-case
      (values (parse-integer (maxima-getenv "PID")))
    ((or type-error parse-error) () -1)))

(defun getpid ()
#+clisp (os:process-id)
#+(or cmu scl) (unix:unix-getpid)
#+sbcl (sb-unix:unix-getpid)
#+gcl (system:getpid)
#+openmcl (ccl::getpid)
#-(or clisp cmu scl sbcl gcl openmcl) (getpid-from-environment)
)

#+(or gcl clisp cmu scl sbcl)
(defun xchdir (w)
  #+clisp (ext:cd w)
  #+gcl (si::chdir w)
  #+(or cmu scl) (unix::unix-chdir w)
  #+sbcl (sb-posix:chdir w)
  )
