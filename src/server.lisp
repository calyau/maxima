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
    #+(and ecl (not windows)) (ext:set-signal-handler EXT:+SIGPIPE+ 'ext:quit)
    
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
    (setq *debug-io* sock)
    ;; A frontend that wants to interrupt us without signals says so by
    ;; passing a token; see "The interrupt channel" below.
    (let ((token (maxima-getenv "MAXIMA_INTERRUPT_TOKEN")))
      (when (and token (plusp (length token)))
        (open-interrupt-channel host port token))))
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
    #+abcl (ext:get-socket-stream (ext:make-socket host port))
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
    #-(or allegro abcl clisp cmu scl sbcl gcl lispworks ecl ccl)
    (error 'not-implemented :proc (list 'open-socket host port bin))))



;;; ---------------------------------------------------------------------
;;; The interrupt channel
;;;
;;; How a frontend interrupts a computation without depending on signals.
;;; POSIX has kill(SIGINT), but MS Windows has nothing like it, so there
;;; this channel is the only way a frontend can interrupt a computation
;;; without the user losing the session.
;;;
;;; So a frontend that passes a token in the environment variable
;;; MAXIMA_INTERRUPT_TOKEN gets a second connection to the port it already
;;; listens on. The first line Maxima sends there is the token, so the
;;; frontend can tell this connection from anybody else's. A thread then
;;; waits on it; each time the frontend writes the line "interrupt" the
;;; thread interrupts the thread that evaluates Maxima's commands. No pid,
;;; no named objects, no helper program, the same on every operating system.
;;; A Lisp without threads (GCL, a clisp built without them) doesn't open the
;;; channel, and the frontend falls back to what it did before.
;;;
;;; wxMaxima uses the same scheme, with the Lisp side in its own
;;; wxMathML.lisp; xmaxima uses this one.
;;;
;;; How safe is it? As safe as a Ctrl+C, and no safer. The interrupt runs
;;; in the interrupted thread itself, so nothing is accessed concurrently:
;;; SBCL's own SIGINT handler (SIGINT-HANDLER in SBCL's
;;; src/code/target-signal.lisp) just calls INTERRUPT-THREAD on the
;;; foreground thread, and on ECL, CCL and clisp this code uses those
;;; Lisps' own thread-interrupt primitives, which likewise run the
;;; function in the target thread. Each Lisp defers interrupts inside its
;;; own critical sections (WITHOUT-INTERRUPTS), which protects the Lisp's
;;; internals, hash table rehashing included -- but not Maxima's: an
;;; interrupt can land between two updates Maxima means to make together
;;; and leave its state inconsistent. That is a lack of atomic sections in
;;; Maxima and affects every way of interrupting it, this one included.
;;; ---------------------------------------------------------------------

(define-condition user-interrupt (condition) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "User interrupt"))))

(defvar *interrupt-channel-thread* nil
  "The thread waiting for the frontend's interrupt requests, if any.")

(defun interrupt-channel-available-p ()
  "True if this Lisp can run the interrupt channel's thread."
  #+(or sb-thread (and ecl threads) (and clisp mt) openmcl ccl) t
  #-(or sb-thread (and ecl threads) (and clisp mt) openmcl ccl) nil)

(defun interrupt-channel-current-thread ()
  #+sb-thread sb-thread:*current-thread*
  #+(and ecl threads) mp:*current-process*
  #+(and clisp mt) (mt:current-thread)
  #+(or openmcl ccl) ccl:*current-process*
  #-(or sb-thread (and ecl threads) (and clisp mt) openmcl ccl) nil)

(defun interrupt-channel-thread-alive-p (thread)
  (and thread
       #+sb-thread (sb-thread:thread-alive-p thread)
       #+(and ecl threads) (mp:process-active-p thread)
       #+(and clisp mt) (mt:thread-active-p thread)
       #+(or openmcl ccl) (not (ccl:process-exhausted-p thread))
       #-(or sb-thread (and ecl threads) (and clisp mt) openmcl ccl) nil))

(defun interrupt-channel-make-thread (name function)
  #+sb-thread (sb-thread:make-thread function :name name)
  #+(and ecl threads) (mp:process-run-function name function)
  #+(and clisp mt) (mt:make-thread function :name name)
  #+(or openmcl ccl) (ccl:process-run-function name function)
  #-(or sb-thread (and ecl threads) (and clisp mt) openmcl ccl)
  (progn name function nil))

(defun interrupt-channel-interrupt-thread (thread function)
  "Makes THREAD run FUNCTION, interrupting whatever it is doing."
  #+sb-thread (sb-thread:interrupt-thread thread function)
  #+(and ecl threads) (mp:interrupt-process thread function)
  #+(and clisp mt) (mt:thread-interrupt thread :function function)
  #+(or openmcl ccl) (ccl:process-interrupt thread function)
  #-(or sb-thread (and ecl threads) (and clisp mt) openmcl ccl)
  (progn thread function nil))

(defun interrupt-main-thread (main)
  "Makes the thread MAIN abandon its computation the way a Ctrl+C would.

The interrupt goes through INVOKE-DEBUGGER, not ERROR: Maxima's *debugger-hook*
reports it and returns to Maxima's top level, exactly as it does for a
SIGINT. ERROR would first look for handlers, and a computation running inside
errcatch() or ignore-errors would catch the interrupt and carry on."
  (interrupt-channel-interrupt-thread
   main
   (lambda ()
     ;; SBCL runs an interrupt with further interrupts disabled. The debugger
     ;; hook prints and unwinds, both of which want them back on.
     #+sb-thread (sb-sys:with-interrupts
                   (invoke-debugger (make-condition 'user-interrupt)))
     #-sb-thread (invoke-debugger (make-condition 'user-interrupt)))))

(defun interrupt-channel-loop (stream main)
  "Interrupts MAIN each time the frontend writes \"interrupt\" to STREAM."
  (unwind-protect
       ;; Nothing that goes wrong in here may reach the debugger hook: it
       ;; would print on the shared output stream and then throw to a catch
       ;; tag that only exists in the main thread. Losing the channel just
       ;; means the frontend falls back to its other ways of interrupting.
       (ignore-errors
        (loop for line = (read-line stream nil nil)
              while line
              when (string= (string-trim '(#\Space #\Return) line) "interrupt")
                do (interrupt-main-thread main)))
    (ignore-errors (close stream))))

(defun open-interrupt-channel (host port token)
  "Connects to the frontend at HOST:PORT as the interrupt channel.

Must be called from the thread that evaluates Maxima's commands -- which is
therefore the one that gets interrupted. Does nothing if the Lisp has no
threads, if the channel is already open, or if the connection fails. Returns
T if a channel thread is running afterwards."
  (when (interrupt-channel-available-p)
    (unless (interrupt-channel-thread-alive-p *interrupt-channel-thread*)
      (let ((main (interrupt-channel-current-thread))
            (stream (ignore-errors (open-socket host port))))
        (when stream
          (ignore-errors
           (write-line token stream)
           (finish-output stream)
           (setq *interrupt-channel-thread*
                 (interrupt-channel-make-thread
                  "Maxima interrupt channel"
                  (lambda () (interrupt-channel-loop stream main)))))))))
  (and (interrupt-channel-thread-alive-p *interrupt-channel-thread*) t))

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
