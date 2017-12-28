;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

;; The Garnet User Interface Development Environment.
;;
;; This code was written as part of the Garnet project at ;; Carnegie
;; Mellon University, and has been placed in the public domain.
;;
;; Protected eval in at a low level in the garnet code so it should be
;; able to catch any error resulting from computations initiating in
;; the UI.

;; split from protected-process.lisp
(in-package :inter)


;; wait interaction complete interactive loop
(defun w-i-c-i-l ()
  "wait-interaction-complete-inner-loop; protected version."
  (catch 'exit-wait-interaction-complete
    (incf *waiting-for-exit-wait-interaction-complete*)
    (if *trans-from-file*
	;; Read from transcript (in a loop in case event handler exits)
	(loop
	   (Read-All-Transcript-Events))
	;; else get event from event handler
	(gg:with-garnet-error-handling "Wait Interaction Complete"
	  (loop
	     (inter::default-event-handler
		 (g-value gem:*device-info* :current-root)))))))
