;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.

(in-package :opal)

(defparameter *main-event-loop-process* nil
  "The variable which is a handle to the main-event-loop process.")

(defvar *inside-main-event-loop* nil)

(defun discard-all-pending-events ()
  (gem:discard-pending-events (g-value gem:device-info :current-root)))


;; Commenting out since protected-process.lisp is no longer
;; conditionally loaded under ASDF.
;;
;; ;; "Main Event Loop" process. Actually this gets smashed when the code
;; ;; in protected-process.lisp gets loaded.  This function is duplicated
;; ;; there except that the event handler is wrapped with the
;; ;; with-garnet-error-handling macro.
;; (defun m-e-l ()
;;   (break)
;;   ;; first, throw away any pending events
;;   (discard-all-pending-events)
;;   (set-dispatch-macro-character #\# #\k (function kr::k-reader))
;;   (let ((root-window (gv gem:device-info :current-root)))
;;     (unwind-protect
;; 	 (catch 'exit-main-loop-exception
;; 	   (loop
;; 	      ;; RGA added an abort restart to the main event loop.
;; 	      (restart-case
;; 		  (inter::default-event-handler root-window)
;; 		(abort () :report "Discard pending X events, restart loop"
;; 		       (discard-all-pending-events)))))
;;       (setf *inside-main-event-loop* nil))))

(defun launch-main-event-loop-process ()
  "Spawn a process which is doing Garnet interaction all of the time.
   return the process."
  (break)
  (when (and (bordeaux-threads:threadp  *main-event-loop-process*)
	     (bordeaux-threads:thread-alive-p *main-event-loop-process*))
    (bordeaux-threads::destroy-thread *main-event-loop-process*))
  (setf *main-event-loop-process*
	(bordeaux-threads:make-thread #'m-e-l :name "Garnet event loop"))
  *main-event-loop-process*)

;;;  Define opal:kill-main-event-loop-process
(defun kill-main-event-loop-process ()
  "Kill the current main-event-loop process."
  (let ((p *main-event-loop-process*))
    (when (and (bordeaux-threads:threadp p)
	       (bordeaux-threads:thread-alive-p p))
      (setf *main-event-loop-process* nil)
      (bordeaux-threads:destroy-thread p))))

;;; Define running-p functions
(defun main-event-loop-process-running-p ()
  (and *main-event-loop-process*
       (bordeaux-threads:thread-alive-p *main-event-loop-process*)))

(defun running-main-event-loop-process-elsewhere-p ()
  (and *main-event-loop-process*
       (not (eq *main-event-loop-process*
		(bordeaux-threads:current-thread)))))




;; (defmacro with-update-lock-held (&body body)
;;   `(unwind-protect
;; 	(progn
;; 	  (update-start-fn nil)
;; 	  ,@body)
;;      (update-stop-fn nil)))

;;; Note, the only safe way to use without-interrupts is in conjuction
;;; with sb-sys:interupt-thread, i.e. it doesn't block ordinary os
;;; interrupts.  No aspect of the
;; (defun update-start-fn (win)
;;   (declare (ignore win))
;;     (sb-sys:without-interrupts
;;       (unless (sb-thread:holding-mutex-p gem:*update-lock*)
;; 	(sb-sys:allow-with-interrupts
;; 	  (sb-thread:grab-mutex gem:*update-lock*)))))

;; (defun update-stop-fn (win)
;;   (declare (ignore win))
;;     (sb-sys:without-interrupts
;;       (when (sb-thread:holding-mutex-p gem:*update-lock*)
;; 	(sb-thread:release-mutex gem:*update-lock*))))
