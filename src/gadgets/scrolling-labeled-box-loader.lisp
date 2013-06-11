;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

#|
==================================================================
Change log:
   06/07/90 Brad Myers - created
==================================================================
|#

(in-package :COMMON-LISP-USER)

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Now load the Scrolling-Labeled-Box module
;;;
(unless (get :garnet-modules :Scrolling-Labeled-Box)
  (format t "Loading Scrolling-Labeled-Box...~%")
  (dolist (pair '((:scrolling-input-string "scrolling-input-string-loader")
		  (:scrolling-labeled-box "scrolling-labeled-box")))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))
  (format t "...Done Scrolling-Labeled-Box.~%"))

(setf (get :garnet-modules :Scrolling-Labeled-Box) t)

