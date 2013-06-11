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
;;; PS-Multifont
;;;
;;; This file contains functions for the multifont-text object used to
;;; generate PostScript files with the PS module.
;;;
;;; Multifont-text functions written by Rich McDaniel

;;; Change Log:
;;;
;;; 08/20/93 Matt Goldberg - Added colors and marks to multifont text.
;;; 09/15/93 Don Hopkins - Added printing of objects in multifont text
;;; 06/23/92 Andrew Mickish - Only print cursor when :visible is T
;;; 04/21/92 Andrew Mickish - Sent :ps-object message to multifont's cursor
;;; 04/15/92 Andrew Mickish - Moved multifont-text stuff here from ps.lisp
;;;

(in-package "OPAL")

(define-method :ps-register-fn OPAL:MULTIFONT-TEXT (obj)
  (pushnew *text-fn* *required-ps-fns*)
  (pushnew *rectangle-fn* *required-ps-fns*)
  (and *color-p*
       (or *file-uses-color-p* (check-ls-color obj) (check-fs-color obj))))

(defun print-given-color-info (style color)
  (if style
      (let* ((stipple (g-value style :stipple))
	     (gray (if stipple (g-value stipple :percent))))
	(if (arbitrary-pattern-p style)
	    (handle-arbitrary-pattern style)
	    (if gray
	        (format t "[~A dup dup] "
			(ps-number
			 (float (/ (- 100 gray) 100))))
		(let ((red (g-value color :red))
		      (green (g-value color :green))
		      (blue (g-value color :blue)))
		(if *color-p*
		    (format t "[~A ~A ~A] " (ps-number red)
			                    (ps-number green)
					    (ps-number blue))
		    ;(format t "[0 0 0] ")
		    (format t "[~A dup dup] " (float (/ (+ red green blue) 3)))
		    )))))
      ; Should we just print a gray scale? "[~A dup dup] "
      (format t "null ")))


(defun ps-frag (frag line-x top base-y height line-style fill-p)
   (if (opal::frag-object-p frag)
       ;;; is it a nested object or a mark?
       (let ((obj (opal::frag-object frag)))
	 (if (mark-p obj)
	   ;; it's a mark.
	   (let* ((sticky-left (mark-sticky-left obj))
		  (string (if sticky-left "<" ">"))
	          (left (- line-x (if sticky-left 3 5)))
		  (opal-width 6)
		  (font (opal:get-standard-font :fixed :bold :small))
		  (font-name (ps-font-name (g-value font :family)
					   (g-value font :face)))
		  (font-size (ps-font-size (g-value font :size))))
	     (add-font-to-list font-name)
	     (format t "~S ~S ~S ~S ~S " left (- top 4) (- base-y 4) opal-width height)
	     (if fill-p
		 (format t "true ")
		 (format t "false ")
		 )
	     (print-color-info line-style :foreground-color)
	     (print-color-info line-style :background-color)
	     (format t "(~A) ~A ~A DrawText~%"
		     string (prin1-to-string font-size) font-name))
	   ;; it's an object
	   (when obj
	     (kr-send obj :ps-object obj))))
       ;;; else it's a string
       (let ((string (convert-parentheses (opal::frag-string frag))))
	  (unless (or (null string) (string= string ""))
	     (let* ((left line-x)
		    (opal-width (opal::frag-width frag))
		    (font (opal::frag-font frag))
		    (font-name (ps-font-name (g-value font :family)
				     (g-value font :face)))
		    (font-size (ps-font-size (g-value font :size)))
		    (fcolor (opal::frag-fcolor frag))
		    (bcolor (opal::frag-bcolor frag)))
	       (add-font-to-list font-name)
	       (format t "~S ~S ~S ~S ~S " left top base-y opal-width height)
		(if fill-p
		   (format t "true ")
		   (format t "false ")
		)
		(if fcolor
		    (print-given-color-info line-style fcolor)
		    (print-color-info line-style :foreground-color))
		(if bcolor
		    (print-given-color-info line-style bcolor)
		    (print-color-info line-style :background-color))
		(format t "(~A) ~A ~A DrawText~%"
			string (prin1-to-string font-size) font-name))))))

(defun ps-multifont-line (obj line-style fill-p show-marks)
   (let* ((line-x (g-value obj :left))
          (top (convert-y (g-value obj :top)))
          (line-y (- top (g-value obj :ascent)))
          (height (g-value obj :height)))
      (do ((frag (g-value obj :first-frag) (opal::frag-next frag)))
          ((null frag))
	(unless (and (not show-marks) (opal::mark-p (opal::frag-object frag)))
	  (ps-frag frag line-x top line-y height line-style fill-p))
	(incf line-x (opal::frag-width frag)))))

(define-method :ps-object OPAL:MULTIFONT-TEXT (obj)
   (let ((line-style (g-value obj :line-style))
	 (show-marks (g-value obj :show-marks)))
     (if line-style
	 (let ((fill-p (g-value obj :fill-background-p))
	       (cursor (g-value obj :cursor)))
	   (do ((line (g-value obj :first-line) (g-value line :next-line)))
	       ((null line))
	     (ps-multifont-line line line-style fill-p show-marks))
	   (if (g-value cursor :visible)
	       (kr-send cursor :ps-object cursor))))))

