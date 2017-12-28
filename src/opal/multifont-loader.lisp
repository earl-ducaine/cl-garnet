;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")



;;; Load modules required for multifont
(unless (get :garnet-modules :multifont)
   (format t "Loading Multifont...~%")
   (load (merge-pathnames "multifont" Garnet-Opal-PathName)
         :verbose T)
   (dolist (file (list
		  ;; "lispkeyhandling"
		  ;;      "multifont-textinter"
		  ;;      "focus-multifont-textinter"
		  ;;      "selection-interactor"
		       ))
     (load (merge-pathnames file Garnet-Inter-PathName)
	   :verbose T))
   ; Load special printing functions for multifont if PS module
   ; already loaded (otherwise they will be loaded by the PS loader
   ; when needed).
   ;; (if (get :garnet-modules :ps)
   ;;     (load (merge-pathnames "ps-multifont" Garnet-PS-Pathname)
   ;; 	     :verbose T))
   ; Load special scrolling functions for multifont if PS module
   ; already loaded (otherwise they will be loaded by the PS loader
   ; when needed).
   ;; (if (get :garnet-modules :scrolling-window)
   ;;     (load (merge-pathnames "scrolling-window-multifont" Garnet-Gadgets-Pathname)
   ;; 	     :verbose T))
   (format t "...Done Multifont.~%"))
