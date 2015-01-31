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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-constraint-gadget-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Load the gadgets that the constraint gadget needs
;;;
(unless (get :garnet-modules :constraint-gadget)
  (force-output *error-output*)
  (format t "~&Loading Constraint Gadget...~%")
  (dolist (pair '((:error-gadget "error-gadget")
		  (:arrow-line "arrow-line-loader")
		  (:labeled-box "labeled-box-loader")))
    (unless (get :garnet-modules (car pair))
      (garnet-load (concatenate 'string "gadgets:" (cadr pair))))))

;;; load c32
(when (not (get :garnet-modules :c32))
      (load garnet-c32-loader))

;;;
;;;     Functions needed from Gilt
(garnet-load "gilt:gilt-functions-loader")

(garnet-mkdir-if-needed Garnet-Constraint-Gadget-Pathname)

;; ---- Compile the constraint gadget itself

(Defparameter Garnet-Constraint-Gadget-Files
  '(
    "cg-defs"
    "support-constraints"
    "custom"
    "attach-constraints"
    "support-box-constraints" "box-parts" "box"
    "line-constraint-defs" "line-constraint-objs" "line-constraint"
    "set-feedback"))

(dolist (file Garnet-Constraint-Gadget-Files)
  (garnet-compile (concatenate 'string "lapidary:" file))
  (garnet-load (concatenate 'string "lapidary:" file)))

(garnet-copy-files Garnet-Constraint-Gadget-Src Garnet-Constraint-Gadget-Pathname
		   '("constraint-gadget-loader.lisp"))

(setf (get :garnet-modules :constraint-gadget) t)


