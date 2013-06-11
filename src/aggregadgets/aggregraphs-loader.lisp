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
=========================================================================
Change log:
;;; 10/2/03 RGA --- New compile/load protocol
      03/12/91 Andrew Mickish - Load aggregadgets if necessary; don't load
                 aggregraphs if already loaded.
      03/07/91 Andrew Mickish - Created
=========================================================================
|#

(in-package "COMMON-LISP-USER")

(format t "Loading Aggregraphs ...~%")

;; check first to see if place is set
(unless (boundp 'Garnet-Aggregadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Aggregadgets-PathName before loading Aggregraphs."))

;; ---- Load aggregraphs themselves
(Defvar Garnet-Aggregraphs-Files
  '(
    "rectangle-conflict-object"
    "aggregraphs"
    "scalable-aggregraph"
    "scalable-aggregraph-image"
    ))

;; Now load aggregraphs if not already loaded
;;
(unless (get :garnet-modules :aggregraphs)
  (dolist (file Garnet-Aggregraphs-Files)
    (load (merge-pathnames file Garnet-Aggregadgets-PathName)
	  :verbose T)))

(setf (get :garnet-modules :aggregraphs)  t)
(format t "...Done Aggregraphs.~%")



