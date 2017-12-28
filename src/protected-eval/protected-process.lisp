;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

;; The Garnet User Interface Development Environment.
;;
;; This code was written as part of the Garnet project at ;; Carnegie
;; Mellon University, and has been placed in the public domain.
;;
;; Protected eval in at a low level in the garnet code so it should be
;; able to catch any error resulting from computations initiating in
;; the UI.

(in-package :opal)

;; The below documentation is speculative about the 'intent' of the
;; evalueation loop.
;;
;; The Garnet protected eval subsystem, has two loop layers with
;; exception processing for each loop.  The innermost loop is the
;; event processing loop that runs within the context of the Garnet
;; GUI.  If an exception occurs.  Garnet presents a GUI dialog box
;; iquiring whether the user wants to abort or debug.  Debugging
;; theoretically allows the user to resume their session within the
;; current GUI context, while

(defvar *debug-protected-eval-mode* t)

;; "Main Event Loop"
;; 'Lowlevel' main event loop.
(defun m-e-l ()
  ;; first, throw away any pending events
  (discard-all-pending-events)
  (set-dispatch-macro-character #\# #\k (function kr::k-reader))
  (let ((root-window (gv gem:*device-info* :current-root)))
    (unwind-protect
	 (catch 'exit-main-loop-exception
	   (loop
	      ;; Abort restart exists in the main event loop.
	      (restart-case
		  (gg:with-garnet-error-handling "Main Interaction Loop"
		    (loop
		       (inter::default-event-handler root-window)))
		(abort () :report "Discard pending X events, restart loop"
		       (discard-all-pending-events)))))
      (setf *inside-main-event-loop* nil))))
