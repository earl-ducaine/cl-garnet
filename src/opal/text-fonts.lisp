;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Changes:
;;;12/17/93 AMICKISH  Make-XFont-Name ---> gem:Make-Font-Name
;;; 9/28/92 AMICKISH  Added Point-Size variables so that make-xfont-name
;;;              selects fonts based on point size, not pixel size
;;;  8/6/91 AMICKISH  Added ps-font-name (used by postscript module)
;;;  3/4/91 d'souza Removed nickname "MO" of package Opal.
;;; 3/18/90 ECP  Oops, I had "times" and "helvetica" switched.
;;;		 Actually, Times has the serifs, not Helvetica.
;;; 1/25/90 ECP  Total rewrite to take advantage of standard font files.
;;;              using X font naming conventions.
;;;

(in-package "OPAL")

;; "/Courier", etc. are names of postscript fonts used by the printer.
;;
(defun ps-font-name (family face)
  (let* ((serif-p (eq family :serif))
	 (ps-font1 (case family
		     (:fixed "/Courier")
		     (:serif "/Times")
		     (:sans-serif "/Helvetica")))
	 (ps-font2 (case face
		     (:roman (if serif-p "-Roman" ""))
		     (:bold "-Bold")
		     (:italic (if serif-p "-Italic" "-Oblique"))
		     (:bold-italic (if serif-p
				       "-BoldItalic"
				       "-BoldOblique")))))
    (concatenate 'string ps-font1 ps-font2)))

(defun ps-font-size (size)
  (case size
    (:small *Small-Font-Size*)
    (:medium *Medium-Font-Size*)
    (:large *Large-Font-Size*)
    (:very-large *Very-Large-Font-Size*)))

