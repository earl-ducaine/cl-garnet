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
   06/15/93 Andrew Mickish - Removed references to compile-opal/inter-p
   02/22/93 Brad Myers - Only one gilt.
   11/05/92 Andrew Mickish - Added filter extension
   02/20/92 Andrew Mickish - Moved make-package call into Garnet-Loader
   02/18/92 Brad Myers - add gilt-gadget-utils
   03/11/91 Osamu Hashimoto - load only motif's stuff
   12/05/90 Brad Myers - created
==================================================================
|#

(in-package "COMMON-LISP-USER")

(format t "Loading Gilt...~%")

;; check first to see if place is set
(unless (boundp 'Garnet-Gilt-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gilt-PathName before loading this file."))

;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (garnet-load "opal:multifont-loader"))

(defparameter gilt-files
  '(
    "gilt-functions-loader" ; only load if not already loaded
    "filter-functions-loader"
    "path-functions-loader"
    "gilt-gadget-utils"
    "motif-gilt-gadgets"
    "gilt-gadgets"
    "gilt"
    "line-imp"  "motif-line-props"
    "fill-imp"  "motif-fill-props"
    "align"
    "motif-gilt-save" "motif-gilt-read"
    "gilt-font-imp"  "motif-gilt-font-props"
    "color-imp"  "motif-color-props"
    "value-control"
    "enable-control"
    "error-check"
    ))

(dolist (file gilt-files)
  (load (merge-pathnames file Garnet-Gilt-PathName)
	:verbose T))


(format T "Type (gilt:do-go :motif) or (gilt:do-go :garnet) to start~%")


