
;; very simple server started on port

(in-package "MAXIMA")

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf)		    ;not needed here for a recent SBCL
  (require 'sb-posix)
  (require 'sb-bsd-sockets))

(defun setup-server (port &optional (host "localhost"))
  (let* ((sock (open-socket host port)))
    (setq me sock)
   #+gcl (setq si::*sigpipe-action* 'si::bye)
    (setq *socket-connection* sock)
    (setq *standard-input* sock)
    (setq *standard-output* sock)
    (setq *error-output* sock)
    (setq *terminal-io* sock)
    (format t "pid=~a~%"        (getpid))
    (force-output sock)
    (setq *debug-io* sock)
    (values)
    ))

;;; from CLOCC: <http://clocc.sourceforge.net>
(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (socket-connect port host :element-type
                                 (if bin '(unsigned-byte 8) 'character))

    #+cmu (sys:make-fd-stream (ext:connect-to-inet-socket host port)
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
    #-(or allegro clisp cmu sbcl gcl lispworks)
    (error 'not-implemented :proc (list 'open-socket host port bin))))



(defun start-server (port &optional (host "localhost"))
  (format t "jfa: starting server on port ~a~%" port)
  (setq $in_netmath t)
  (setq $show_openplot nil)
  (setup-server port host))

#+clisp
(defun getpid ( &aux tem)
  (cond #-win32((fboundp 'sys::program-id)
		(sys::program-id))
	((consp (setq tem (errset (system::getenv "PID"))))
	 (read-from-string (car tem)))
	(t (format t "using fake value for pid") -1)))
#+cmu
(defun getpid () (unix:unix-getpid))

#+sbcl
(defun getpid () (sb-unix:unix-getpid))

#+(or gcl clisp cmu sbcl)
(defun xchdir (w)
  #+clisp (cd w)
  #+gcl (si::chdir w)
  #+cmu (unix::unix-chdir w)
  #+sbcl (sb-posix:chdir w)
  )
 
  
