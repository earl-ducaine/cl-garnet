;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;
;; The Garnet User Interface Development Environment.
;;
;; This code was written as part of the Garnet project at Carnegie
;; Mellon University, and has been placed in the public domain.


;;  This file contains the export list for Opal.

(in-package :opal)

(defvar *debug-opal-mode* nil)

;;; This is the export list for as much of OPAL as I could find
;;; Import some stuff from GEM that used to be in OPAL.
(eval-when (:execute :load-toplevel :compile-toplevel)
  (import '(gem:Display-Info
	    gem:Make-Display-Info gem:Copy-Display-Info
	    gem:Display-Info-Display gem:Display-Info-Screen gem:Display-Info-Root-Window
	    gem:Display-Info-Line-Style-GC gem:Display-Info-Filling-Style-GC
;;;	    gem:*update-lock*
;;;	    gem:*screen-width* gem:*screen-height*

	    gem:*Fixed-Font-Family* gem:*Serif-Font-Family* gem:*Sans-Serif-Font-Family*
	    gem:*Small-Font-Size* gem:*Medium-Font-Size*
	    gem:*Large-Font-Size* gem:*Very-Large-Font-Size*
	    gem:*Small-Font-Point-Size* gem:*Medium-Font-Point-Size*
	    gem:*Large-Font-Point-Size* gem:*Very-Large-Font-Point-Size*
	    gem:default-font-from-file)
	  (find-package "OPAL")))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(

)))
