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
    8/06/93 Andrew Mickish - Added defvar of Garnet-Constraint-Gadget-Pathname;
              Loaded error-gadget-loader instead of error-gadget.
    4/22/93 Brad Vander Zanden - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Load the gadgets that the constraint gadget needs
;;;
(unless (get :garnet-modules :constraint-gadget)
  (force-output *error-output*)
  (format t "~&Loading Constraint Gadget...~%")
  (dolist (pair '((:error-gadget "error-gadget-loader")
		  (:arrow-line "arrow-line-loader")
		  (:labeled-box "labeled-box-loader")))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

  ;;; load c32
  (when (not (get :garnet-modules :c32))
    (load garnet-c32-loader)))

;;;
;;;     Functions needed from Gilt
(garnet-load "gilt:gilt-functions-loader")


;; ---- Load the constraint gadget itself

(Defparameter Garnet-Constraint-Gadget-Files
  '(
    "cg-defs"
    "support-constraints"
    "custom"
    "attach-constraints"
    "support-box-constraints" "box-parts" "box"
    "line-constraint-defs" "line-constraint-objs" "line-constraint"
    "set-feedback"))

(defvar Garnet-Constraint-Gadget-Pathname
  Garnet-Lapidary-Pathname)

(dolist (file Garnet-Constraint-Gadget-Files)
  (load (merge-pathnames file Garnet-Constraint-Gadget-PathName)
	:verbose T))

(setf (get :garnet-modules :constraint-gadget) t)


