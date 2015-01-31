;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changes:
;;; 10/2/03 RGA --- New compile/load protocol
;;; 10/2/03 RGA --- Changed this back, is it really necessary to explicitly call 
;;;                 gc during compilation?
;;;  6/26/03 rpg         Figured that the +allegro-3.1 should be +allegro.
;;;  7/28/96 RGA --- changed to use garnet-compile/load
;;;  8/27/93 Mickish     Added suggest-constants
;;;  1/10/93 Brad Myers  Added inspector
;;;  2/20/92 Mickish     Moved make-package call into Garnet-Loader
;;;  4/12/90 Mitchell    Added #+allegro (gc t)
;;;  3/22/90 Robert Cook Define the "GARNET-DEBUG" package for the TI Explorer

(in-package "COMMON-LISP-USER")

(defvar *debug-debug-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-debug-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))


;; load utilities needed by inspector, unless already loaded

(unless (get :garnet-modules :multifont)
  (load (merge-pathnames "multifont-loader" common-lisp-user::Garnet-Opal-PathName)))
(unless (get :garnet-modules :text-buttons)
  (load (merge-pathnames "text-buttons-loader" common-lisp-user::Garnet-Gadgets-PathName)))
(unless (get :garnet-modules :error-gadget-utils)
  (common-lisp-user::garnet-load "gg:error-gadget-utils"))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-debug-Pathname))

(Defvar Garnet-Debug-Files   ;; defvar rather than defparameter so can setq
			     ;; this variable before loading if only want
			     ;; to compile some of these files
  '(
    "debug-fns"
    "objsize"
    "inspector"
    "suggest-constants"))

(dolist (file Garnet-Debug-Files)
  (let ((gfile (concatenate 'string "debug:" file)))
    (garnet-compile gfile)))

(garnet-copy-files Garnet-debug-Src Garnet-debug-Pathname
		   '("debug-loader.lisp"))

(setf (get :garnet-modules :debug) T)
