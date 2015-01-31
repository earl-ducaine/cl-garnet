;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GEM; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(in-package "GEM")

;;; Define names and argument lists for all the Gem methods.  Define and
;;; export all Gem interface macros, such as (gem:create-window)



(gem-method :ALL-GARNET-WINDOWS (root-window))

(gem-method :BEEP (root-window))

(gem-method :BIT-BLIT (window source s-x s-y width height destination d-x d-y))

(gem-method :BLACK-WHITE-PIXEL (window))

(gem-method :CHARACTER-WIDTH (root-window font character))

(gem-method :CHECK-DOUBLE-PRESS (root-window state code time))

(gem-method :CLEAR-AREA (window &optional (x 0) (y 0) width height buffer-p))

(gem-method :COLOR-TO-INDEX (root-window a-color))

(gem-method :COLORMAP-PROPERTY (root-window property &optional a b c))

(gem-method :COPY-TO-PIXMAP (root-window to from width height))

(gem-method :CREATE-CURSOR (root-window source mask foreground background
					from-font-p x y))

(gem-method :CREATE-IMAGE (root-window width height depth from-data-p
				       &optional color-or-data properties
				       bits-per-pixel left-pad data-array))

(gem-method :CREATE-IMAGE-ARRAY (root-window width height depth))

(gem-method :CREATE-PIXMAP (root-window width height depth
					&optional image bitmap-p data-array))

(gem-method :BUILD-PIXMAP (window image width height bitmap-p))


(gem-method :CREATE-STATE-MASK (root-window modifier))

(gem-method :CREATE-WINDOW
	    (parent-window x y width height
	     title icon-name background border-width
	     save-under visible
	     min-width min-height max-width max-height
	     user-specified-position-p user-specified-size-p
	     override-redirect))

(gem-method :DELETE-FONT (root-window font))

(gem-method :DELETE-PIXMAP (root-window pixmap &optional buffer-too))

(gem-method :DELETE-WINDOW (root-window x-window))

(gem-method :DEVICE-IMAGE (root-window index))

(gem-method :DISCARD-MOUSE-MOVED-EVENTS (root-window))

(gem-method :DISCARD-PENDING-EVENTS (root-window &optional timeout))

(gem-method :DRAW-ARC (window x y width height angle1 angle2 function
			      line-style fill-style &optional pie-slice-p))

(gem-method :DRAW-IMAGE (window left top width height image function fill-style))

(gem-method :DRAW-LINE (window x1 y1 x2 y2 function line-style
			       &optional drawable))

(gem-method :DRAW-LINES (window point-list function line-style fill-style))

(gem-method :DRAW-POINTS (window point-list function line-style))

(gem-method :DRAW-RECTANGLE (window x y width height function
				    line-style fill-style))

(gem-method :DRAW-ROUNDTANGLE (window left top width height
				      corner-width corner-height function
				      line-style fill-style))

(gem-method :DRAW-TEXT (window x y string font function
			       line-style &optional fill-background invert-p))

(gem-method :DRAWABLE-TO-WINDOW (root-window drawable))

(gem-method :EVENT-HANDLER (root-window ignore-keys))

(gem-method :FLUSH-OUTPUT (window))

(gem-method :FONT-EXISTS-P (root-window name))

(gem-method :FONT-MAX-MIN-WIDTH (root-window font min-too))

(gem-method :FONT-NAME-P (root-window arg))

(gem-method :FONT-TO-INTERNAL (root-window opal-font))

(gem-method :GET-CUT-BUFFER (root-window))

(gem-method :IMAGE-BIT (root-window image x y))

(gem-method :IMAGE-FROM-BITS (root-window patterns))

(gem-method :IMAGE-HOT-SPOT (root-window image))

;;; Returns three values: width, height, depth
(gem-method :IMAGE-SIZE (a-window image))

(gem-method :IMAGE-TO-ARRAY (root-window image))

(gem-method :INITIALIZE-DEVICE (root-window))

(gem-method :INITIALIZE-WINDOW-BORDERS (window drawable))

(gem-method :INJECT-EVENT (window index))

(gem-method :MAKE-FONT-NAME (root-window key))

(gem-method :MAP-AND-WAIT (a-window drawable))

(gem-method :MAX-CHARACTER-ASCENT (root-window font))

(gem-method :MAX-CHARACTER-DESCENT (root-window font))

(gem-method :MOUSE-GRAB (window grab-p want-enter-leave &optional owner-p))

(gem-method :RAISE-OR-LOWER (window raise-p))

(gem-method :READ-AN-IMAGE (root-window pathname))

(gem-method :REPARENT (window new-parent drawable left top))

(gem-method :SET-CLIP-MASK (window clip-mask &optional lstyle-ogc fstyle-ogc))

(gem-method :SET-CUT-BUFFER (root-window string))

(gem-method :SET-DEVICE-VARIABLES (root-window full-display-name))

(gem-method :SET-DRAW-FUNCTION-ALIST (root-window))

(gem-method :SET-DRAW-FUNCTIONS (root-window))

(gem-method :SET-DRAWABLE-TO-WINDOW (window drawable))

(gem-method :SET-INTEREST-IN-MOVED (window interestedp))

(gem-method :SET-SCREEN-COLOR-ATTRIBUTE-VARIABLES (root-window))

(gem-method :SET-WINDOW-PROPERTY (window property value))

(gem-method :STIPPLED-P (root-window))

(gem-method :TEXT-EXTENTS (root-window opal-font string))

(gem-method :TEXT-WIDTH (root-window opal-font string))

(gem-method :TRANSLATE-CHARACTER (window x y state code time))

(gem-method :TRANSLATE-CODE (window scan-code shiftp))

(gem-method :TRANSLATE-COORDINATES (root-window window x y &optional
                                                other-window))

(gem-method :TRANSLATE-MOUSE-CHARACTER (root-window button-code modifier-bits
                                        event-key))

(gem-method :WINDOW-DEBUG-ID (window))

(gem-method :WINDOW-DEPTH (window))

(gem-method :WINDOW-FROM-DRAWABLE (root-window drawable))

(gem-method :WINDOW-HAS-GROWN (window width height))

(gem-method :WINDOW-TO-IMAGE (window left top width height))

(gem-method :WRITE-AN-IMAGE (root-window pathname image))



;;; This ends up being a macro, not a function; therefore, it is handled
;; specially.
;;
(defmacro batch-changes (drawable &body body)
  `(xlib:with-state ,drawable
     ,@body))

;; This macro is called in the update method, so it has to be defined before
;; update-window.lisp.  Also, this macro requires that the Gworld module has
;; already been loaded.
;;
(defmacro MAC-with-focused-view-or-gworld ((drawable buffer) &body body)
  (declare (ignore drawable buffer))
  `(progn ,@body))
