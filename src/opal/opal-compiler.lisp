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
;;; Changes:
;;; 10/2/03 RGA --- New compile/load protocol
;;;        7/28/96 RGA --- changed to use garnet-compile/load
;;;  8-Nov-93 Mickish     Added x.lisp and called gem:init-device
;;;  5-Mar-93 Mickish     Added utils.lisp
;;; 10-Feb-93 Mickish     Added types.lisp
;;; 11-Jun-92 Pervin	  Added pixmaps.lisp.
;;;  7-Apr-92 Pervin      Moved clean-up before windows to eliminate warning.
;;; 20-Mar-92 Pervin	  Added process.lisp.
;;; 20-Jan-92 Mickish     Moved make-package call into Garnet-Loader
;;; 10-Dec-91 Pervin	  Added virtual-aggregates.
;;; 18-Jun-91 Pervin      Added multifont.
;;; 26-Mar-91 Pervin      Load compiled files in Lucid
;;; 22-Mar-91 Pervin      Added #-cmu before setf and provide.
;;;  4-Mar-91 D'Souza     Removed nickname "MO" of Opal
;;; 15-Aug-90 Pervin      Moved clean-up to after open-and-close.
;;;  6-Jun-90 Pervin      Removed *twm-bug*
;;;  5-Jun-90 Richardson  Added lispworks
;;; 12-Apr-90 Mitchell    Added #+allegro (gc t)
;;;  2-Apr-90 Cook/Pervin Added #+explorer part.

(in-package "COMMON-LISP-USER")

(defvar *debug-opal-mode* t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-opal-mode*
       (and (boundp '*garnet-compile-debug-settings*)
	    *garnet-compile-debug-settings*)
       ;; Global default settings.
       (and (boundp '*default-garnet-proclaim*) 
	    *default-garnet-proclaim*))))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-opal-Pathname))

(Defparameter Garnet-Opal-Files
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
	"multifont"
        "virtual-aggregates"
	"pixmaps"
        "open-and-close"
        "x"
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
