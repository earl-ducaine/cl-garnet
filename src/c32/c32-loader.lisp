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
;;; $Id:: c32-loader.lisp 114 2017-03-23 17:07:47Z rotgut             $

#|
==================================================================
Change log:
   05/25/93 Dave Kosbie - Removed references to "kr-extra" (placed
                          #'kr::i-depend-on inside actual KR package)
   07/30/92 Dario Giuse - added loading of gg:scrolling-labeled-box
   06/15/92 Dario Giuse - added loading of opal:multifont
   06/12/92 Dario Giuse - added loading of gg:error-gadget
   05/21/92 Dario Giuse - Converted to Garnet version 2.0
   ??/??/90 Brad Myers  - created
==================================================================
|#

(in-package "COMMON-LISP-USER")

(format t "Loading C32...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-C32-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-C32-PathName before loading this file."))

(dolist (gadget '("labeled-box-loader"
		  "motif-check-buttons-loader"
		  "arrow-line-loader"
		  "motif-text-buttons-loader"
		  "motif-scrolling-window-loader"
		  "scrolling-input-string-loader"
		  "motif-scrolling-menu-loader" 		; for pop-up-functions
		  "motif-error-gadget-loader"	  		; for C32error
		  "scrolling-labeled-box-loader"		; for package name
		  ))
  (garnet-load (concatenate 'string "gadgets:" gadget)))

(garnet-load "gilt:gilt-functions-loader")
(garnet-load "gilt:path-functions-loader")
(garnet-load "opal:multifont-loader")

(defvar C32-files '("c32"
		    "c32formula"
		    "c32ref"
		    "pop-up-generalize"
		    "pop-up-copy-formula"
		    "pop-up-ask-object"
		    "pop-up-functions"
		    "c32dialog"
		    "c32-lapidary"))

(dolist (file C32-files)
  (garnet-load (concatenate 'string "c32:" file)))


(setf (get :garnet-modules :c32) T)

(format T "Type (c32:do-go) to start~%")
