;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                             $

#|
==================================================================
Change log:
    1/07/93 Rajan Parthasarathy - Added menubar-functions.lisp
    5/06/91 Andrew Mickish - Created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Now load the menubar module
;;;
(unless (get :garnet-modules :menubar)
  (format t "Loading Menubar...~%")
  (dolist (pair '((:menubar-functions "menubar-functions")
		  (:menubar "menubar")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done Menubar.~%"))

(setf (get :garnet-modules :menubar) t)
(setf (get :garnet-modules :menubar-functions) t)
