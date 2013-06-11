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
;;; 02/24/93  Andrew Mickish - Removed references to compile-opal/inter-p
;;; 04/15/92  Andrew Mickish - Added load of ps-multifont
;;; 02/20/92  Andrew Mickish - Moved make-package calls into Garnet-Loader
;;; 08/06/91  Andrew Mickish - Created
;;;
(in-package "COMMON-LISP-USER")

(defparameter PS-Version-Number "1.0")

(format t "Loading PS...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-PS-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-PS-PathName before loading PS."))


;;;  Load PS  ...

(Defparameter Garnet-PS-Files  ; NOT USED
  '(
    "ps"
    "ps-multifont"
    ))

(unless (get :garnet-modules :ps)
  (load (merge-pathnames "ps" Garnet-PS-PathName)
	:verbose T))

; Load printing functions for multifont if multifont files already loaded
; (otherwise they will be loaded by the multifont-loader when needed).
(if (get :garnet-modules :multifont)
    (load (merge-pathnames "ps-multifont" Garnet-PS-Pathname)
	  :verbose T))

(setf (get :garnet-modules :ps) t)
(format t "...Done PS.~%")
