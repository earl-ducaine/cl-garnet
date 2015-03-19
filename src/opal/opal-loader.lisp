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
;;; $Id::                                                             $

(in-package "COMMON-LISP-USER")

(defparameter Opal-Version-Number "1.3")

(format t "Loading Opal...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Opal-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Opal-PathName before loading opal."))

;;;  Load Opal  ...
(Defvar Garnet-Opal-Files
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
    "virtual-aggregates"
    "pixmaps"
    "open-and-close"
    ))

(dolist (file Garnet-Opal-Files)
  (load (merge-pathnames file Garnet-Opal-PathName)
	:verbose T))

(setf (get :garnet-modules :opal) t)
(format t "...Done Opal.~%")

(gem:init-device :X NIL)

