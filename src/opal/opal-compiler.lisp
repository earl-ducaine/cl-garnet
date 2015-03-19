;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; $Id$
;;;

(in-package "COMMON-LISP-USER")

(defvar *debug-opal-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-opal-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-opal-Pathname))

(Defparameter Garnet-Opal-Files
  '(
    "exports"
    "types"
    "update-constants"
    "macros"
    "defs"
    "new-defs"
    "utils"
    "text-fonts"
    "create-instances"
    "text-functions"
    "text"
    "update-basics"
    "halftones"
    "objects"
    "roundtangles"
    "basics"
    "aggregates"
    "process"
    "clean-up"
    "windows"
    "update"
    "fast-redraw"
    "update-window"
    "multifont"
    "virtual-aggregates"
    "pixmaps"
    "open-and-close"
    ))

#+ALLEGRO
(proclaim '(optimize (debug 0)))

(dolist (file Garnet-Opal-Files)
  (let ((gfile (concatenate 'string "opal:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Opal-Src Garnet-Opal-Pathname
		   '("opal-loader.lisp"
		     "multifont-loader.lisp"))
(setf (get :garnet-modules :opal) T)

(gem:init-device :X NIL)
