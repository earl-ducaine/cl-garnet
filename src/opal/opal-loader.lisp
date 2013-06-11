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

;;; Changes:
;;; 10/2/03 RGA --- Changed from switching on #+apple to #+(and apple (not clx))
;;; 11/8/93 AMICKISH Added x.lisp and called gem:init-device
;;;  3/5/93 AMICKISH Added utils.lisp
;;; 2/10/93 AMICKISH Added types.lisp
;;; 7/20/92 AMICKISH Changed Defparameter of opal files to Defvar
;;; 6/11/92 ECP Added pixmaps.lisp.
;;; 04/02/92 RGM Released new version of multifont.
;;; 03/20/92 ECP Added process.lisp.
;;; 02/20/92 AMICKISH - Moved OPAL package defintion into Garnet-Loader
;;; 12/10/91 ECP Added virtual-aggregates.
;;; 6/18/91 ECP Added multifont.
;;; 3/4/91  D'Souza Removed nickname "MO" of package Opal.
;;; 8/15/90 ECP Moved clean-up to after open-and-close
;;; 6/6/90  ECP Removed *twm-bug*
;;; 3/22/90 Robert Cook - Define the package "OPAL" for the TI Explorer
;;; 3/19/90 ECP Added *twm-bug*
;;; 3/9/90  ECP Added open-and-close
;;; 2/13/90 ECP Merged objects.lisp and eds-objects.lisp
;;; 1/4/90  ECP Added version number
;;;
(in-package "COMMON-LISP-USER")

(defparameter Opal-Version-Number "1.3")

(format t "Loading Opal...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Opal-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Opal-PathName before loading opal."))

;;;  Load Opal  ...
(Defvar Garnet-Opal-Files
  '(
    "types"
    "update-constants"
    "defs"
    "macros"
    "new-defs"
    "utils"
    "text-fonts"
    "create-instances"
    "create-instances2"
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
    "x"))

(dolist (file Garnet-Opal-Files)
  (load (merge-pathnames file Garnet-Opal-PathName)
	:verbose T))

(setf (get :garnet-modules :opal) t)
(format t "...Done Opal.~%")

(gem:init-device :X NIL)

