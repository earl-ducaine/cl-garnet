;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;; This file contains definitions of objects needed by lapidary

(in-package "LAPIDARY")
 
;; interactor for moving objects between windows

(create-instance 'multi-win-interactor inter:move-grow-interactor
	 (:running-where T)
	 (:old-feedback NIL)
	 (:start-where nil)
	 (:waiting-priority inter:high-priority-level)
	 (:attach-point :where-hit)
	 (:running-action
	  #'(lambda (inter obj new-box)
	     (let ((old-f (g-value inter :old-feedback))
		   (cur-f (g-value inter :feedback-obj)))
	       (when (not (eq old-f cur-f))
		 (when (and old-f (schema-p old-f))
		       (s-value old-f :obj-over NIL))
		 (when cur-f (s-value cur-f :obj-over obj))
		 (s-value inter :old-feedback cur-f)))
	     (call-prototype-method inter obj new-box))))
