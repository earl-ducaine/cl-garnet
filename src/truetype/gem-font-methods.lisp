;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GEM; Base: 10 -*-
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;-------------------------------------------------------------------;;

;;; $Id$

;;; This file includes truetype versions of font-related GEM methods.
;;; They will be smashed over the bitmap font methods when the truetype
;;; code is enabled.

(in-package "GEM")


;;; Font loader interface.
;;

(defun x-delete-font (root-window font)
  (declare (ignore root-window))
  (zpb-ttf:close-font-loader (kr:g-cached-value font :xfont)))


(defun x-font-name-p (root-window arg)
  (declare (ignore root-window))
  (stringp arg))

(defun x-make-font-name (root-window key)
  "Returns either a string which describes the font 
using X conventions, or a cons of the bad value and slot."
  (declare (ignore root-window))
  (let ((family-part
          (case (first key)
            (:fixed      opal::*Fixed-Font-Family*)
            (:serif      opal::*Serif-Font-Family*)
            (:sans-serif opal::*Sans-Serif-Font-Family*)
	    (otherwise   nil)))
        (face-part
         (let ((face-spec (if (consp (second key))
                              (second key)
                              (list (second key)))))
           (when (subsetp face-spec *x-font-faces*)
	     face-spec)))
        (size-part
	  (case (third key)
            (:small      (princ-to-string opal::*Small-Font-Point-Size*))
            (:medium     (princ-to-string opal::*Medium-Font-Point-Size*))
            (:large      (princ-to-string opal::*Large-Font-Point-Size*))
            (:very-large (princ-to-string opal::*Very-Large-Font-Point-Size*))
            (otherwise   nil))))
    (cond ((null family-part)
           (cons (first key) :family)) ;; for reporting error
          ((null face-part)
           (cons (second key) :face))
          ((null size-part)
           (cons (third key) :size))
          (t
	   (let ((adjusted-face-part
		  (cond ((equal '(:roman) face-part) "medium-r")
			((equal '(:bold) face-part) "bold-r")
			((equal '(:italic) face-part)
			 (if (eq (first key) :serif) "medium-i" "medium-o"))
			((or (equal '(:bold-italic) face-part)
			     (equal '(:bold :italic) face-part)
			     (equal '(:italic :bold) face-part))
			 (if (eq (first key) :serif) "bold-i" "bold-o")))))
	     (concatenate 'string
	       "*-*-"
	       family-part
	       "-"
	       adjusted-face-part
	       "-*-*-*-" 
	       size-part
	       "-*-*-*-*-iso8859-1"))))))

(defun x-font-exists-p (root-window name)
  (declare (ignore root-window))
  (xlib:list-font-names opal::*default-x-display* name))


(defun x-font-to-internal (root-window font-from-file)
  (let ((dx-plist (g-value font-from-file :display-xfont-plist))
	(display (the-display root-window)))
    (or (getf dx-plist display)
	(let ((font-path (opal::fix-font-path
			  (g-value font-from-file :font-path)))
	      (font-name (g-value font-from-file :font-name)))
	  (when font-path
	    (let ((xfont-path (mapcar #'opal::remove-null-char
				      (xlib:font-path display))))
	      ;;; Add the font-path to the font-path, if necessary
	      (unless (member font-path xfont-path :test #'string=)
		(setf (xlib:font-path display)
		      (cons font-path xfont-path))
		;;; Now make sure it's there!
		(unless (member font-path (xlib:font-path display)
				:test #'string=)
		  (format t "WARNING: X did not add ~A to font-path!!~%"
			  font-path)))))
	  ;;; Open the font only if it's on the font-path
	  (if (xlib:list-font-names display font-name)
	    (let ((xfont (xlib:open-font display font-name)))
		(s-value font-from-file :display-xfont-plist
			 (cons display (cons xfont dx-plist)))
		xfont)
	      (progn
		(format t "WARNING: Font '~A' not on font path!~%"
			font-name)
		(format t "  ****   Resorting to Default Font!~%")
		(opal::fff-to-xfont opal::default-font-from-file
				    root-window)))))))


;;; Extents.
;;

(defun x-character-width (root-window opal-font the-char-code)
  (let ((glyph (zpb-ttf:index-glyph the-char-code (g-value opal-font :xfont))))
    (* (xft::font-units->pixels-x root-window opal-font) (zpb-ttf:advance-width glyph)))))

;;; RETURNS: the maximum character width for the font; if <min-too> is non-nil,
;;; returns both maximum and minimum width, as multiple values.  This function
;;; used to be called by opal::get-index, but was replaced by a simple g-value
;;; of the font's :char-width.
;;;
(defun x-font-max-min-width (root-window opal-font min-too)
  (declare (ignore root-window))
  (let ((font (g-value opal-font :xfont)))
    (if min-too
	(values (xlib:max-char-width font)
		(xlib:min-char-width font))
	(xlib:max-char-width font))))


(defun x-max-character-ascent (root-window opal-font)
  (declare (ignore root-window))
  (xlib:max-char-ascent (g-value opal-font :xfont)))


(defun x-max-character-descent (root-window opal-font)
  (declare (ignore root-window))
  (xlib:max-char-descent (g-value opal-font :xfont)))


(defun x-text-extents (root-window font string)
  "RETURNS: multiple values:
    width
    ascent
    descent
    left-bearing
    right-bearing
plus other information we do not use."
  (declare (ignore root-window))
  (xlib:text-extents (g-value font :xfont) string))


(defun x-text-width (root-window font string)
  "Returns the width of the <string> in the given font."
  (declare (ignore root-window))
  (xlib:text-width (g-value font :xfont) string))



;;; Draw the text.
;;

(defun x-draw-text (window x y string font function
			   line-style &optional fill-background invert-p)
  (setf font (g-value font :xfont))
  (setf function (get function :x-draw-function))
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (if (and line-style font)
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(set-gc line-style-gc xlib-gc-line :font font)
	(if fill-background
	  (let ((background (g-value line-style :background-color
				     :colormap-index))
		(foreground (if invert-p (g-value line-style :foreground-color
						  :colormap-index))))
	    (if invert-p
	      (progn
		(set-gc line-style-gc xlib-gc-line
			:foreground background)
		(set-gc line-style-gc xlib-gc-line
			:background foreground))
	      (set-gc line-style-gc xlib-gc-line :background background))
	    (xlib:draw-image-glyphs drawable xlib-gc-line x y string)
	    (when invert-p
	      ;; restore gc
	      (set-gc line-style-gc xlib-gc-line
		      :foreground foreground)
	      (set-gc line-style-gc xlib-gc-line
		      :background background)))
	  (xlib:draw-glyphs drawable xlib-gc-line x y string))))))


