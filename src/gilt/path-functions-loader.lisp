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

#|
==================================================================
Change log:
   11/05/90 Andrew Mickish - created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Gilt-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gilt-PathName before loading this file."))

;;; Now load the Path-Functions module
;;;
(unless (get :garnet-modules :Path-Functions)
  (format t "Loading Path functions...~%")
  (load (merge-pathnames "path-functions" Garnet-Gilt-PathName)
	:verbose T)
  (format t "...Done Path Functions.~%"))

(setf (get :garnet-modules :Path-Functions) t)


