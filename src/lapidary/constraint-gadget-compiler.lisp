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
;;; 10/2/03 RGA --- New compile/load protocol
    4/22/93 Brad Vander Zanden - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

(defvar *debug-constraint-gadget-mode* nil)

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Load the gadgets that the constraint gadget needs
(unless (get :garnet-modules :constraint-gadget)
  (force-output *error-output*)
  (format t "~&Loading Constraint Gadget...~%")
  (dolist (pair '((:error-gadget "error-gadget")
		  (:arrow-line "arrow-line-loader")
		  (:labeled-box "labeled-box-loader")))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair))))))

(garnet-load "gilt:gilt-functions-loader")
(garnet-mkdir-if-needed Garnet-Constraint-Gadget-Pathname)
(setf (get :garnet-modules :constraint-gadget) t)
