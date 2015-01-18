;;;
;;; This is inter-process communication support for MAXIMA.
;;;
;;; The main idea is copied from gcl/maxima. We have a library
;;; winkill_lib.dll which sets up a shared memory file mapping.
;;; The program winkill.exe writes interrupts into the shared memory.
;;; Maxima should then regularly check this memory to see if it
;;; should interrupt current computation or terminate. Other
;;; signals are not supported yet.
;;;
;;; Currently there is only support for ccl and sbcl. Should add
;;; support for all lisps other than gcl which run on windows.
;;;

(in-package :maxima)

#+openmcl
(progn
  ;; The main maxima thread
  (defvar *main-maxima-process*)
  (defvar *signal-monitor-process*)
  
  (defun load-library ()
    (ccl:open-shared-library "winkill_lib")
    (ccl:external-call "init_shared_memory"))
  
  (defvar *run-monitor* nil)
  
  (defun monitor-shared-memory ()
    (loop while *run-monitor* do
       ;; Check for SIGINT signals
         (when (= 1 (ccl:external-call "read_sm_sigint" :int))
           (ccl:external-call "reset_sm_sigint")
           (ccl:process-interrupt *main-maxima-process* #'(lambda () (error "interrupt signal"))))
       ;; Check for SIGTERM signals
         (when (= 1 (ccl:external-call "read_sm_sigterm" :int))
           (ccl:external-call "reset_sm_sigterm")
           (ccl:process-interrupt *main-maxima-process* #'(lambda () ($quit))))
       ;; Wait a little
         (sleep 0.1)))
  
  ;; Starts the monitor thread
  (defun start-monitor ()
    (setq *run-monitor* t)
    (setq *main-maxima-process* ccl:*current-process*)
    (setq *signal-monitor-process* (ccl:process-run-function "monitor" #'monitor-shared-memory)))
  
  (defun $quit ()
    (when *signal-monitor-process*
      (ccl:process-kill *signal-monitor-process*))
    (ccl:quit))
  
  (defun start-shared-memory-monitor ()
    (load-library)
    (start-monitor)))

#+sbcl
(progn
  (sb-alien:load-shared-object "winkill_lib.dll")
  
  (defun load-library ()
    (sb-alien:alien-funcall (sb-alien:extern-alien "init_shared_memory" (sb-alien:function sb-alien:void))))
  
  (defvar *run-monitor* nil)
  
  (defun monitor-shared-memory ()
    (loop while *run-monitor* do
	  ;; Check for SIGINT signals
	  (when (= 1 (sb-alien:alien-funcall (sb-alien:extern-alien "read_sm_sigint" (sb-alien:function sb-alien:int))))
	    (sb-alien:alien-funcall (sb-alien:extern-alien "reset_sm_sigint" (function sb-alien:void)))
	    (sb-thread:interrupt-thread (sb-thread:main-thread) #'(lambda () (error "interrupt signal"))))
	  ;; Check for SIGTERM signals
	  (when (= 1 (sb-alien:alien-funcall (sb-alien:extern-alien"read_sm_sigterm" (sb-alien:function sb-alien:int))))
	    (sb-alien:alien-funcall (sb-alien:extern-alien "reset_sm_sigterm" (sb-alien:function sb-alien:void)))
	    (sb-thread:interrupt-thread (sb-thread:main-thread) #'(lambda () ($quit))))
	  ;; Wait a little
	  (sleep 0.1)))
  
  ;; Starts the monitor thread
  (defun start-monitor ()
    (setq *run-monitor* t)
    (sb-thread:make-thread #'monitor-shared-memory :name "monitor"))
  
  (defun start-shared-memory-monitor ()
    (load-library)
    (start-monitor)))


#-(or openmcl sbcl)
(defun start-shared-memory-monitor ())


;; This function should be called when Maxima starts.
(start-shared-memory-monitor)
