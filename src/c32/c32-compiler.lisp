;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;; 
;;;___________________________________________________________________
;;; The Garnet User Interface Development Environment
;;; Copyright (c) 1991 Carnegie Mellon University
;;; All rights reserved.  The CMU software License Agreement specifies
;;; the terms and conditions for use and redistribution.
;;;
;;; If you want to use this code or anything developed as part of the
;;; Garnet project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;;___________________________________________________________________
;;;
;;; $Id:: c32-compiler.lisp 114 2017-03-23 17:07:47Z rotgut           $	


#|
==================================================================
Change log:
;;; 10/2/03 RGA --- New compile/load protocol
    5/25/93 Dave Kosbie - Removed references to "kr-extra" (placed
                          #'kr::i-depend-on inside actual KR code)
    2/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
    4/14/91 Brad Myers - created
==================================================================
|#

(in-package "COMMON-LISP-USER")

(defvar *debug-c32-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-c32-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim)
	    Default-Garnet-Proclaim))))


(format t "Compiling C32...~%")

;; check first to see if place is set
(unless (boundp 'Garnet-C32-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-C32-PathName before loading this file."))

;; Load files that C32 depends on, only if not compiling the rest of Garnet
;; already

(unless (get :garnet-modules :multifont)
  (garnet-load "opal:multifont-loader"))

(unless (get :garnet-modules :gilt-functions)
  (garnet-load "gilt:gilt-functions-loader"))

(garnet-mkdir-if-needed Garnet-c32-Pathname)


(dolist (gadget '(
		  "labeled-box-loader"
		  "motif-check-buttons-loader"
		  "arrow-line-loader"
		  "motif-text-buttons-loader"
		  "motif-scrolling-window-loader"
		  "scrolling-input-string-loader"
		  "motif-scrolling-menu-loader"		; for pop-up-functions
		  "motif-error-gadget-loader"  		; for C32error
		  "scrolling-labeled-box-loader"	; for package name
		  ))
  (load (merge-pathnames gadget garnet-gadgets-pathname)))

(defvar C32-files '(
		    "c32"
		    "c32formula"
		    "c32ref"
		    "pop-up-generalize"
		    "pop-up-copy-formula"
		    "pop-up-ask-object"
		    "pop-up-functions"
		    "c32dialog"
		    "c32-lapidary"
		    ))


(dolist (file c32-files)
  (garnet-compile (concatenate 'string "c32:" file))
  (garnet-load (concatenate 'string "c32:" file))
  )

(garnet-copy-files Garnet-c32-Src Garnet-c32-Pathname
		   '("c32-loader.lisp"))

(setf (get :garnet-modules :C32) t)

(format t "... Done Compiling C32~%")
