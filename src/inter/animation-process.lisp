;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;


;;; Change log:
;;    ??/??/?? RGA --- Added code to make this work under MCL.
;;    10/1/93 Brad Myers - Workaround bug in HP CL where vector-push-extend
;;                         returns the wrong value
;;    9/06/93 Clive Tong - Added LispWorks stuff
;;   10/22/92 Brad Myers - Test if main-event-loop crashed into debugger
;;   10/11/92 Brad Myers - declare (ignore) so fewer warnings in CMU CL -Sjolin
;;   5/22/92 Brad Myers - started, based on code from R J Williams
;;                           <rodw@cbl.leeds.ac.uk> and code from
;;                           opal:process.lisp


(in-package "INTERACTORS")

;;; A window waiting for a timer event

(defparameter *Inters-With-Timers*
  (make-array 5 :fill-pointer 0 :adjustable T)
"An array of all interactors that have timers.  None are ever removed
from this array.  An index into this array is sent with the timer
event so that we will know which interactor to wake up.")

;; set by default-event-handler.
(defparameter *process-with-main-event-loop* nil)

(defparameter *All-Timer-Processes* NIL)

(defun Reset-All-Timer-Processes ()
  (dolist (p (copy-list *All-Timer-Processes*))
    (internal-kill-timer-process p)))

(defun Listener-Process-Broken-P ()
  "Returns T if the process *Process-With-Main-Event-Loop* is in the
   debugger, otherwise NIL"
  ;; (when *Process-With-Main-Event-Loop*
    ;; A thread that is broken will bind the variable
    ;; sb-debug:*debug-condition* whereas in a running thread it will
    ;; be unbound.
    ;;
    ;; Maybe bordeaux-threads-2:*default-special-bindings* could be used
    ;; for this?
    ;;
    ;; (ignore-errors
    ;;   (sb-thread:symbol-value-in-thread
    ;;    'sb-debug:*debug-condition*
    ;;    *process-with-main-event-loop*))
    NIL)

(defun send-timer-event (inter)
  (let* ((wins (Get-Interactor-Windows inter))
	 (win (if (listp wins) (car wins) wins)))
    (if-debug inter (Format T "Posting Timer event for ~s~%" inter))
    (when win
      (let ((indx (g-value inter :timer-array-index)))
	(unless indx
	  (setq indx (vector-push-extend inter *Inters-With-Timers* 10))
	  (s-value inter :timer-array-index indx))
	#+garnet-debug			; only test when debugging
	(unless (eq (g-value inter :timer-array-index)
		    (position inter *inters-with-timers*))
	  (error "Interactor timer index not eq to position for ~s" inter))
	(gem:inject-event win indx)
	(gem:flush-output win)))))


(defun Timer-Process-Main-Loop (inter time once)
  "Sleep for appropriate time (in seconds), and then wake up and send event"
  (loop
     ;; All the process-wait-with-timeout implementations seem
     ;; eventually to boil down to calls to some version of sleep, so
     ;; this should be OK.
     (sleep time)
     (unless (schema-p inter)		; if inter destroyed somehow
       (return))
     (when (Listener-Process-Broken-P)	; if main-event-loop crashed
       (if-debug inter
		 (Format T "Main event loop in debugger so aborting anim for ~s~%" inter))
       (return))
     (send-timer-event inter)
     (when once (return))
     ;; now, make sure other processes run
     (bt2:thread-yield)
     ))

(defun kill-timer-process (inter)
  "Kills the timer process for the interactor, if any"
  (let ((timer-process (g-value inter :timer-event-process)))
    (when timer-process (internal-kill-timer-process timer-process))
    (s-value inter :timer-event-process NIL)))

(defun launch-timer-process (inter time once)
  "Spawn a process which is waiting for timer events"

  (let ((timer-process (g-value inter :timer-event-process)))
    (when timer-process (internal-kill-timer-process timer-process))
    ;;; DZG (xlib:intern-atom opal::*default-x-display* ':TIMER_EVENT)
    (setf timer-process
	  (bordeaux-threads-2:make-thread
	   #'(lambda ()
	       (Timer-Process-Main-Loop inter time once))
	   :name "Garnet Timer")
	  )
    (if-debug inter (format T "Launching process ~s for ~s~%" timer-process
			    inter))
    (s-value inter :timer-event-process timer-process)
    (push timer-process *All-Timer-Processes*)
    timer-process))


(defun Handle-Timer-Event (inter-index)
  "This is called when a timer event occurs for the interactor"
  (let ((inter (aref *Inters-With-Timers* inter-index)))
    (when (and inter (schema-p inter))
      (if-debug inter (Format T "Timer event for ~s~%" inter))
      (if (eq (g-value inter :current-state) :start)
	  (kill-timer-process inter)	; whoops, process shouldn't be running
	  (progn
	    (kr-send inter :timer-handler inter)
	    (opal:update-all))))))


;;;-------------------------------------------------------------------------

(defun internal-kill-timer-process (timer-process)
  (when (and (eq (type-of timer-process) 'bt2:thread)
             (bt2:thread-alive-p timer-process))
    (deleteplace timer-process *All-Timer-Processes*)
    (bt2:destroy-thread timer-process)))

