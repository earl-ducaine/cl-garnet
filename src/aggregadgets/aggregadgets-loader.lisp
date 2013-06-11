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
         5/14/93 Andrew Mickish - Added new files, split off from others
         8/05/90 Roger Dannenberg - update for new aggregadgets version
         8/23/89 Ed Pervin - Removed load of aggregates
         7/20/89 Philippe Marchal - Added aggrelists, removed aggregitems
         6/29/89 Philippe Marchal - Added aggregitems
         6/21/89 Brad Myers - Made to work with Sun Lucid Lisp also
		 Philippe Marchal -- created
=======================================================================++
|#

(in-package "COMMON-LISP-USER")

(format t "Loading Aggregadgets ...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Aggregadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Aggregadgets-PathName before loading Aggregadgets."))

;; ---- Load aggregadgets themselves

(Defvar Garnet-Aggregadgets-Files
  '(
    "agg-macros"
    "agg-utils"
    "aggregadgets"
    "aggrelists"
    "add-agg"
    "agg-fix-slots"
    "copy-agg"
    "save-agg"
    "string-edit"
    "agg-labels"
    ))

(dolist (file Garnet-Aggregadgets-Files)
  (load (merge-pathnames file Garnet-Aggregadgets-PathName)
	:verbose T))

(setf (get :garnet-modules :aggregadgets)  t)
(format t "...Done Aggregadgets.~%")



