;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;*******************************************************************;;

;;; $Id$
;;


(in-package "OPAL")

;;;  Global variables
;;
(defparameter *main-event-loop-process* nil
  "The variable which is a handle to the main-event-loop process.")

(defvar *inside-main-event-loop* nil)

;;; Define opal:launch-main-event-loop-process
;;

(defun discard-all-pending-events ()
  (gem:discard-pending-events (g-value gem:device-info :current-root)))

;; Main event process loop. Define here so we don't have multiple pieces of code
;; doing the same thing.
;; XXX Actually this gets smashed when the code in protected-process.lisp gets loaded.
;; This function is duplicated there except that the event handler is wrapped with
;; the with-garnet-error-handling macro.
(defun m-e-l ()
  ;; first, throw away any pending events
  (discard-all-pending-events)

  ;;  I don't know if this should be there.... [2006/01/10:rpg]
  #-NO-K-READER
  (set-dispatch-macro-character #\# #\k (function kr::k-reader))

  (let ((root-window (gv gem:device-info :current-root)))
    (unwind-protect
	 (catch 'exit-main-loop-exception
	   (loop
	      ;; RGA added an abort restart to the main event loop.
	      (restart-case
		  (inter::default-event-handler root-window)
		(abort () :report "Discard pending X events, restart loop"
		       (discard-all-pending-events)))))
      (setf *inside-main-event-loop* nil))))


#+allegro
   ;; RGA --- The optional tty parameter ensures that processes will not
   ;; clobber each other's I/O.
(defun launch-main-event-loop-process (&optional (tty excl:*initial-terminal-io*))
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
  (when (eq (type-of *main-event-loop-process*) 'mp:process)
    (mp:process-kill *main-event-loop-process*))
  (setf *main-event-loop-process*
	(mp:process-run-restartable-function
	 "Garnet event loop"
	 #'(lambda (t-io)
	     ;; RGA --- This gets around a "feature" of Allegro 4.1
	     ;; which does not allow background processes to do io.
	     ;; Note that the main process function is now a function
	     ;; of one arg which is bound to *terminal-io*
	     (setq *terminal-io* t-io)
	     (setq *query-io* t-io)
	     (setq *standard-input* t-io)
	     (setq *standard-output* t-io)
	     (setq *error-output* t-io)
	     (setq *trace-output* t-io)
	     ;; Don't bind *debug-io* because RGA suggests other problems
	     ;; might arise
	     ;;(setq *debug-io* t-io)
	     ;; The actual loop.
	     (m-e-l))
	 tty))
  (setf (mp:process-priority *main-event-loop-process*) 1)
  *main-event-loop-process*)

;;; SBCL main-event-loop-process
;;
#+sb-thread
(defun launch-main-event-loop-process ()
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
  (when (and (typep *main-event-loop-process* 'sb-thread:thread)
	     (sb-thread:thread-alive-p *main-event-loop-process*))
    (sb-thread:terminate-thread *main-event-loop-process*))
  (setf *main-event-loop-process*
	(sb-thread:make-thread #'m-e-l :name "Garnet event loop"))
  *main-event-loop-process*)

#+(and cmu mp)
(defun launch-main-event-loop-process ()
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
  (when (mp:processp *main-event-loop-process*)
    (mp:destroy-process *main-event-loop-process*))
  (setf *main-event-loop-process*
	(mp:make-process #'m-e-l :name "Garnet event loop"))
  (setf mp::*idle-process* mp::*initial-process*)
  ;; Lower the timeout for serve-event to give good process response.
  (setf lisp::*max-event-to-usec* 50000)
  (setf lisp::*max-event-to-sec* 0)
  *main-event-loop-process*)

#+ccl
(defun launch-main-event-loop-process ()
  "Spawn a process which is doing Garnet interaction all of the time.
   RETURN the process."
  (when (ccl::processp *main-event-loop-process*)
    (ccl:process-kill *main-event-loop-process*))
  (setf *main-event-loop-process*
	(ccl:process-run-function "Garnet event loop" #'m-e-l))
  *main-event-loop-process*)

#-(or allegro (and cmu mp) ccl sb-thread)
(defun launch-main-event-loop-process ())


;;;  Define opal:kill-main-event-loop-process
;;

#+allegro
(defun kill-main-event-loop-process ()
  "
  Kill the current main-event-loop process.
  "
  (when (eq (type-of *main-event-loop-process*) 'mp:process)
    (mp:process-kill *main-event-loop-process*)
    (setf *main-event-loop-process* nil)))

#+(and cmu mp)
(defun kill-main-event-loop-process ()
  "
  Kill the current main-event-loop process.
  "
  (when (and *main-event-loop-process*
             (mp:processp *main-event-loop-process*))
    (mp:destroy-process *main-event-loop-process*)
    (setf *main-event-loop-process* nil)))

#+sb-thread
(defun kill-main-event-loop-process ()
  "Kill the current main-event-loop process."
  (let ((p *main-event-loop-process*))
    (when (and (typep p 'sb-thread:thread)
	       (sb-thread:thread-alive-p p))
      (setf *main-event-loop-process* nil)
      (sb-thread:terminate-thread p))))

#+ccl
(defun kill-main-event-loop-process ()
  "Kill the current main-event-loop process."
  (let ((p *main-event-loop-process*))
    (when (and p (ccl::processp p))
      (setf *main-event-loop-process* nil)
      (ccl:process-kill p))))


#-(or allegro (and cmu mp) ccl sb-thread)
(defun kill-main-event-loop-process ())

;;; Define running-p functions
;;

(defun main-event-loop-process-running-p ()
  (and *main-event-loop-process*
       #+allegro
       (mp:process-active-p *main-event-loop-process*)
       #+(and cmu mp)
       (equal "Run" (mp:process-whostate *main-event-loop-process*))
       #+ccl
       (ccl::process-active-p *main-event-loop-process*)
       #+sb-thread
       (sb-thread:thread-alive-p *main-event-loop-process*)
       ))

(defun running-main-event-loop-process-elsewhere-p ()
  (and *main-event-loop-process*
       (not (eq *main-event-loop-process*
		#+(or allegro (and cmu mp)) mp:*current-process*
		#+sb-thread sb-thread:*current-thread*
		#+ccl ccl:*current-process*
		#-(or allegro (and cmu mp) ccl sb-thread) T)
	    )))

;;;  Define process lock functions
;;

#-(and) ;;ccl version; doesn't seem to work
(defmacro with-update-lock-held (&body body)
  `(ccl:with-lock-grabbed (gem:*update-lock*)
     ,@body))

#-(and) ;; cmu verstion also doesn't seem to work right
(defmacro with-update-lock-held (&body body)
  `(mp:with-lock-held (gem:*update-lock*)
     ,@body))

#+(and)
(defmacro with-update-lock-held (&body body)
  `(unwind-protect
	(progn
	  (update-start-fn nil)
	  ,@body)
     (update-stop-fn nil)))

(defun update-start-fn (win)
  (declare (ignore win))
  #+ALLEGRO
  (if common-lisp-user::update-locking-p
      (unless (eq (mp:process-lock-locker gem:*update-lock*) mp:*current-process*)
	;; Lock only if lock is held by a different process, or unlocked.
	(mp:process-lock gem:*update-lock*)))
  #+ccl
  (when common-lisp-user::update-locking-p
    (ccl:without-interrupts
      (unless (eq (ccl::%%lock-owner gem:*update-lock*) ccl:*current-process*)
	(ccl:grab-lock gem:*update-lock*))))
  #+sb-thread
  (when common-lisp-user::update-locking-p
    (sb-sys:without-interrupts
      (unless (sb-thread:holding-mutex-p gem:*update-lock*)
	(sb-sys:allow-with-interrupts
	  (sb-thread:grab-mutex gem:*update-lock*))))))


(defun update-stop-fn (win)
  (declare (ignore win))
  #+ALLEGRO
  (if (and common-lisp-user::update-locking-p
	   (eq (mp:process-lock-locker gem:*update-lock*) mp:*current-process*))
      (mp:process-unlock gem:*update-lock*))
  #+ccl
  (when common-lisp-user::update-locking-p
    (ccl:without-interrupts
      (when (eq (ccl::%%lock-owner gem:*update-lock*) ccl:*current-process*)
	(ccl:release-lock gem:*update-lock*))))
  #+sb-thread
  (when common-lisp-user::update-locking-p
    (sb-sys:without-interrupts
      (when (sb-thread:holding-mutex-p gem:*update-lock*)
	(sb-thread:release-mutex gem:*update-lock*)))))
