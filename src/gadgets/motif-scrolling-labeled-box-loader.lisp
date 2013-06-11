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
;;;  MOTIF-SCROLLING-LABELED-BOX-LOADER:  Loads the "motif-scrolling-labeled-box"
;;;  and "parts" modules if required.

#|
==================================================================
Change log:
   2/8/91 Andrew Mickish - Created
==================================================================
|#

(in-package :COMMON-LISP-USER)

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before
  loading Gadgets."))

(unless (get :garnet-modules :motif-scrolling-labeled-box)
  (format t "Loading Motif-Scrolling-Labeled-Box...~%")
  (dolist (pair '((:motif-parts "motif-parts")
		  (:scrolling-input-string "scrolling-input-string-loader")
		  (:motif-scrolling-labeled-box "motif-scrolling-labeled-box")))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))
  (format t "...Done Motif-Scrolling-Labeled-Box.~%"))


(setf (get :garnet-modules :motif-scrolling-labeled-box) t)
