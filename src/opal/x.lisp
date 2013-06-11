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


;;; CHANGE LOG:
;;  
;;  [2003/11/20:rga]        - Added further patch to Robert's for same
;;                            problem.
;;  [2003/11/20:rpg]        - Fix remaining bugs in gem:create-image, where
;;                            depth and bits-per-pixel don't match.
;;  17-DEC-1999 Fred Gilham - Fix problem where pixmap format doesn't match
;;                            valid pixmap formats in displays where `depth'
;;                            and `bits-per-pixel' values differ
;;                            (see also pixmap.lisp).
;;  08/20/98 Fred Gilham    - Added CMU to x-map-and-wait conditioanl for
;;                            better updating.
;;  08/01/98 Fred Gilham    - Fixed problems with color displays deeper than
;;                            8 bits.
;;  01/30/95 Andrew Mickish - New destroy-notify-window for CMUCL
;;  12/02/94 Andrew Mickish - Removed stippled-p parameter from draw-image
;;  05/29/94 Andrew Mickish - Added ability to specify a list for font :face
;;  05/25/94 Andrew Mickish - Added optional drawable parameter to X-Draw-Line
;;  04/15/94 Andrew Mickish - Fixed tiny width and height for X-Draw-Arc
;;  04/13/94 Andrew Mickish - Fixed :QUERY-COLORS branch of X-Colormap-Property
;;  03/25/94 Andrew Mickish - Fixed title clause in x-set-window-property
;;  03/21/94 Andrew Mickish - Fixed corner fill of roundtangles
;;  01/21/94 Andrew Mickish - New Gem method color-to-index
;;  01/12/94 Andrew Mickish - New window-from-drawable
;;  01/07/94 Andrew Mickish - Fixed functions that destroy window from X menu
;;  01/03/94 Andrew Mickish - Moved PI variables to utils/general.lisp
;;  12/17/93 Andrew Mickish - New X-Device-Image
;;  12/15/93 Andrew Mickish - Moved macros to macros.lisp
;;  11/23/93 Andrew Mickish - Fixed line and fill placement in x-draw-arc,
;;                            line placement in x-draw-rectangle, and fill
;;                            placement in x-draw-roundtangle.
;;  11/18/93 Andrew Mickish - Removed "list" from call to xlib:free-colors
;;  11/11/93 Andrew Mickish - Put into CLTL2 form


;;; This is the device handler for the X window system.  It implements the
;;  Gem methods.

(in-package "GEM")

(defvar *default-x-display-number*)

;; The following two variables used to be in Inter/i-windows.lisp; they
;; have been moved here because nobody seems to be using them.
;;
(defvar *mouse-debug* nil
  "When true, *mouse-throw-aways* will increment each time a mouse-moved
   event is thrown away")

(defvar *mouse-throw-aways* 0)


;; Debugging only
(defparameter *debug-on* NIL)

(defmacro debug-print (&rest arguments)
  #-DEBUG
  (declare (ignore arguments))
  #-DEBUG
  NIL
  #+DEBUG
  `(if *debug-on*
    (format t "~%~A ~{ ~S~}" (car ,arguments) (cdr ,arguments))))

(defvar *x-font-faces* '(:roman :bold :italic :bold-italic))


;;; Set Styles Functions (from opal: new-defs.lisp)
;;


;;; This is called by with-*-styles, and it replaces the old :x-tiles slot.
;;  It gets the *-style's :stipple, and checks its :root-pixmap-plist slot for
;;  an entry for this Root.  If so, it returns it.  Else, it creates the
;;  entry and places it at the head of the plist.
;;  These were split into two macros because the draw method for opal:bitmap
;;  also needs to use the first macro now...

(defun get-stipple-schema-pixmap (stipple-schema root-window bitmap-p)
  (let ((root-plist (g-value stipple-schema :root-pixmap-plist)))
    (or (getf root-plist root-window)
	(let ((the-image (g-value stipple-schema :image))
	      roots-entry)
	  ;; (return-from get-stipple-schema-pixmap NIL)
	  (if the-image
	    (if (typep the-image 'xlib::image)
	      (multiple-value-bind (width height)
		  (x-image-size nil the-image)
		(setq roots-entry (opal::build-pixmap *root-window* the-image
						      width height
						      bitmap-p))
		(s-value stipple-schema :root-pixmap-plist
			 (cons root-window (cons roots-entry root-plist)))
		roots-entry)
	      (format
	       t
	       "WARNING -- :image entry in schema ~A is not of type xlib:image!~%"
	       stipple-schema))
	    (format
	     t "WARNING -- no :image slot in schema ~A~%" stipple-schema))))))


(defmacro get-x-stipple (style-schema root-window)
  `(let ((stipple-schema (g-value ,style-schema :stipple)))
    (if stipple-schema
      (get-stipple-schema-pixmap stipple-schema ,root-window nil))))




;;; With-styles works like xlib:with-gcontext except it takes a gob and
;;  extracts all the relevant things for you. This is a win for the simple
;;  draw methods, it will be a lose for performance. See below.
;; 
;;  This is a quick hack to get around the caching of various gcontext
;;  values, it will work until we understand how CLX and the RT X11 server
;;  cache gcontexts better.

(defmacro set-gc (opal-gcontext xlib-gcontext slot value)
  (case slot
    (:foreground
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-foreground ,opal-gcontext))
	 (setf (opal::opal-gc-foreground ,opal-gcontext)
	       (setf (xlib:gcontext-foreground ,xlib-gcontext) v)))))
    (:background
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-background ,opal-gcontext))
	 (setf (opal::opal-gc-background ,opal-gcontext)
	       (setf (xlib:gcontext-background ,xlib-gcontext) v)))))
    (:function
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-function ,opal-gcontext))
	 (setf (opal::opal-gc-function ,opal-gcontext)
	       (setf (xlib:gcontext-function ,xlib-gcontext) v)))))
    (:line-width
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-line-width ,opal-gcontext))
	 (setf (opal::opal-gc-line-width ,opal-gcontext)
	       (setf (xlib:gcontext-line-width ,xlib-gcontext) v)))))
    (:line-style
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-line-style ,opal-gcontext))
	 (setf (opal::opal-gc-line-style ,opal-gcontext)
	       (setf (xlib:gcontext-line-style ,xlib-gcontext) v)))))
    (:cap-style
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-cap-style ,opal-gcontext))
	 (setf (opal::opal-gc-cap-style ,opal-gcontext)
	       (setf (xlib:gcontext-cap-style ,xlib-gcontext) v)))))
    (:join-style
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-join-style ,opal-gcontext))
	 (setf (opal::opal-gc-join-style ,opal-gcontext)
	       (setf (xlib:gcontext-join-style ,xlib-gcontext) v)))))
    (:dashes
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-dashes ,opal-gcontext))
	 (setf (opal::opal-gc-dashes ,opal-gcontext)
	       (if v			; do not set to NIL
		 (setf (xlib:gcontext-dashes ,xlib-gcontext) v))))))
    (:font
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-font ,opal-gcontext))
	 (setf (opal::opal-gc-font ,opal-gcontext)
	       (if v			; do not set to NIL
		 (setf (xlib:gcontext-font ,xlib-gcontext) v))))))
    (:fill-style
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-fill-style ,opal-gcontext))
	 (setf (opal::opal-gc-fill-style ,opal-gcontext)
	       (setf (xlib:gcontext-fill-style ,xlib-gcontext) v)))))
    (:fill-rule
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-fill-rule ,opal-gcontext))
	 (setf (opal::opal-gc-fill-rule ,opal-gcontext)
	       (setf (xlib:gcontext-fill-rule ,xlib-gcontext) v)))))
    (:stipple
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-stipple ,opal-gcontext))
	 (setf (opal::opal-gc-stipple ,opal-gcontext)
	       (if v			; do not set to NIL
		 (setf (xlib:gcontext-stipple ,xlib-gcontext) v))))))
    (:clip-mask
     `(let* ((v ,value)
	     (s (opal::opal-gc-stored-clip-mask ,opal-gcontext))
	     do-copy?)
       (setf (opal::opal-gc-clip-mask ,opal-gcontext) v)
       (if (eq v :none)
	 (unless (eq (first s) :none)
	   (setf (xlib:gcontext-clip-mask ,xlib-gcontext) :none)
	   (setf (first s) :none))
	 (progn
	   (unless (nthcdr 4 v)
	     ;; special cases if v has only one clip-mask
	     (unless (eq (first s) :short)
	       (setq do-copy? T)
	       (setf (first s) :short))
	     (setq s (nthcdr 4 s)))
	   (when (or do-copy? (not (equal v s)))
	     (setf (xlib:gcontext-clip-mask ,xlib-gcontext) v)
	     ;; copy v into s.
	     (do nil
		 ((null v))
	       (setf (car s) (car v))
	       (setq s (cdr s))
	       (setq v (cdr v))))))))))



(defun set-line-style (line-style opal-gc xlib-gc root-window x-draw-fn)
  (declare (optimize (speed 3) (safety 1)))
  (when line-style
    (let ((draw-fn-changed? (set-gc opal-gc xlib-gc
				    :function x-draw-fn)))
      (unless (eq x-draw-fn opal::boole-2)
	(let ((x-stipple (get-x-stipple line-style root-window))
	      x-dash-pattern)

	  ;; If the draw-function is :xor and *black* = 0 (for instance
	  ;; on HP machines), then we must draw black as white and white
	  ;; as black.  But we must check the draw-function first.
	  ;; Set-gc returns non-NIL if draw-function changed.
	  (when (or draw-fn-changed?
		    (not (eq line-style (opal::opal-gc-opal-style opal-gc))))
	    (set-gc opal-gc xlib-gc :foreground
		    (opal::HP-XOR-hack
		     x-draw-fn
		     (g-value line-style :foreground-color :colormap-index)))
	    (set-gc opal-gc xlib-gc :background
		    (opal::HP-XOR-hack
		     x-draw-fn
		     (g-value line-style :background-color :colormap-index))))

	  (unless (eq line-style (opal::opal-gc-opal-style opal-gc))
	    (setf (opal::opal-gc-opal-style opal-gc) line-style)
	    (set-gc opal-gc xlib-gc :line-width
		    (g-value line-style :line-thickness))
	    (set-gc opal-gc xlib-gc :line-style
		    (g-value line-style :line-style))
	    (set-gc opal-gc xlib-gc :cap-style
		    (g-value line-style :cap-style))
	    (set-gc opal-gc xlib-gc :join-style
		    (g-value line-style :join-style))
	    (if (setq x-dash-pattern (g-value line-style :dash-pattern))
		(set-gc opal-gc xlib-gc :dashes x-dash-pattern)))

	  ;; This can't be in the "unless" since the same
	  ;; line-style can have different x-stipples
	  (if x-stipple
	      (progn
		(set-gc opal-gc xlib-gc :fill-style :opaque-stippled)
		(set-gc opal-gc xlib-gc :stipple x-stipple))
	      (set-gc opal-gc xlib-gc :fill-style :solid)))))))


(defun set-filling-style (filling-style opal-gc xlib-gc root-window x-draw-fn)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 2)))
  (when filling-style
    (unless (eq x-draw-fn boole-2)
      (let ((x-stipple (get-x-stipple filling-style root-window)))
	;; Set-gc returns non-NIL if draw-function changed.
	(when (or (set-gc opal-gc xlib-gc :function x-draw-fn)
		  (not (eq filling-style (opal::opal-gc-opal-style opal-gc))))
	  (set-gc opal-gc xlib-gc :foreground
		  (opal::HP-XOR-hack
		   x-draw-fn
		   (g-value filling-style :foreground-color :colormap-index)))
	  (set-gc opal-gc xlib-gc :background
		  (opal::HP-XOR-hack
		   x-draw-fn
		   (g-value filling-style :background-color :colormap-index))))

	(unless (eq filling-style (opal::opal-gc-opal-style opal-gc))
	  (setf (opal::opal-gc-opal-style opal-gc) filling-style)
	  (set-gc opal-gc xlib-gc :fill-style
		  (g-value filling-style :fill-style))
	  (set-gc opal-gc xlib-gc :fill-rule
		  (g-value filling-style :fill-rule)))
	(if x-stipple (set-gc opal-gc xlib-gc :stipple x-stipple))))
    (set-gc opal-gc xlib-gc :function x-draw-fn)))




;;; -------------------------------------------------- X Windows Handling


(defun do-all-garnet-windows (clx-window)
  "Iterates over all CLX windows.  Clean-Up calls
this function with the root CLX window."
  (let ((windows (if (member :garnet (xlib:drawable-plist clx-window))
		   (list clx-window))))
    (dolist (w (xlib:query-tree clx-window))
      (setf windows (append windows (do-all-garnet-windows w))))
    windows))


(defun x-all-garnet-windows (root-window)
  "Returns a list of all the X windows that were created by Garnet.  
These are raw windows, NOT Opal windows!"
  (declare (ignore root-window))
  (do-all-garnet-windows opal::*default-x-root*))


(defun x-beep (root-window)
  (declare (ignore root-window))
  (xlib:bell opal::*default-x-display*)
  (xlib:display-force-output opal::*default-x-display*))


;;; This assumes that the <window> has a gcontext.
;;;
(defun x-bit-blit (window source s-x s-y width height destination d-x d-y)
  (xlib:copy-area source
		  (g-value window :buffer-gcontext)
		  s-x s-y width height destination d-x d-y))


(defun x-black-white-pixel (window)
  "Returns: the black and white pixel for the screen of the <window>, as
multiple values."
  (let ((screen (opal::display-info-screen (g-value window :display-info))))
    (values (xlib:screen-black-pixel screen)
	    (xlib:screen-white-pixel screen))))


(defun x-character-width (root-window opal-font the-char-code)
  (declare (ignore root-window))
  (xlib:char-width (g-value opal-font :xfont) the-char-code))


(defun x-clear-area (window &optional (x 0) (y 0) width height clear-buffer-p)
  "Clears the visible area associated with a window.  If <clear-buffer-p>,
operate on the window's buffer instead."
  (if clear-buffer-p
      ;; Clear the window's buffer
      (let* ((gc (g-value window :buffer-gcontext))
	     (buffer (g-value window :buffer))
	     (background (xlib:gcontext-background gc)))
	(xlib:with-gcontext (gc :function opal::*copy* :foreground background)
	  (if x
	      ;; clear only a region
	      (xlib:draw-rectangle buffer gc x y width height t)
	      ;; clear the entire buffer
	      (xlib:draw-rectangle buffer gc 0 0
				   (xlib:drawable-width buffer)
				   (xlib:drawable-height buffer) t))))
      ;; Clear the window itself
      (xlib:clear-area (the-drawable window)
		       :x x :y y :width width :height height
		       :exposures-p NIL)))


(defun x-color-to-index (root-window a-color)
  (declare (ignore root-window))
  (if (g-value opal::color :color-p)
      (if a-color (g-value a-color :colormap-index) opal::*white*)
      (if (eq a-color opal::black)
          opal::*black*
          opal::*white*)))

(defun x-colormap-property (root-window property &optional a b c)
  "Returns various things, depending on which <property> is requested:
:COLOR-LOOKUP -- looks up <a> (a color name) and returns three values,
                 the R-G-B values in the color lookup table.
:MAKE-COLOR   -- creates and returns a color whose three RGB components
                 are given by <a, b, c>"
  (declare (ignore root-window))
  (case property
    (:ALLOC-COLOR
     (xlib:alloc-color opal::*default-x-colormap* a))
    (:ALLOC-COLOR-CELLS
     (xlib:alloc-color-cells opal::*default-x-colormap* 1))
    (:FIRST-ALLOCATABLE-INDEX
     (let* ((indices (xlib:alloc-color-cells opal::*default-x-colormap* 1))
	    (index (car indices)))
       (xlib:free-colors opal::*default-x-colormap* indices)
       index))
    (:FREE-COLORS
     (xlib:free-colors opal::*default-x-colormap* a))
    (:LOOKUP-COLOR
     (xlib:lookup-color opal::*default-x-colormap* a))
    (:LOOKUP-RGB
     (let* ((xcolor (xlib:lookup-color opal::*default-x-colormap* a)))
       ;; The PS module needs the RGB values
       (values (xlib:color-red xcolor)
	       (xlib:color-green xcolor)
	       (xlib:color-blue xcolor))))
    (:MAKE-COLOR
     (xlib:make-color :red a :green b :blue c))
    (:QUERY-COLORS
     ;; Returns three values: red, green, blue components
     (let ((color (car (xlib:query-colors opal::*default-x-colormap* (list a)))))
       (values (floor (* 65535 (xlib:color-red color)))
	       (floor (* 65535 (xlib:color-green color)))
	       (floor (* 65535 (xlib:color-blue color))))))
    (t
     (error "Unknown property ~S in gem::x-colormap-property~%"
	    property))))



(defun x-copy-to-pixmap (root-window to from width height)
  "Copy the cursor or bitmap in <from> to the pixmap <to>.  The operation
affects an area of <width> by <height>."
  (let* ((screen (opal::display-info-screen
		  (g-value root-window :display-info)))
	 (gc (xlib:create-gcontext
	      :drawable to :function boole-1
	      :foreground (xlib:screen-white-pixel screen)
	      :background (xlib:screen-black-pixel screen))))
    (xlib:put-image to gc from :x 0 :y 0 :width width :height height)
    (xlib:free-gcontext gc)))



(defun x-create-cursor (root-window source mask
			foreground background
			from-font-p x y)
  "If <from-font-p> is true, the <source> is a font; otherwise, it is
a pixmap.  Same for the <mask>.  <x> and <y> are a position when the
source is a pixmap; otherwise, they are the cursor-char and the mask-char
for the two fonts."
  (declare (ignore root-window))
  (if from-font-p
      (xlib:create-glyph-cursor :source-font source :mask-font mask
				:source-char x
				:mask-char y
				:foreground foreground
				:background background)
      (xlib:create-cursor :source source :mask mask
			  :x x :y y
			  :foreground (g-value opal:black :xcolor)
			  :background (g-value opal:white :xcolor))))
  

;; The following deals with cases where the display provides pixmaps
;; with depths different from bits-per-pixel.
;;
(defun get-pixmap-formats ()
  "Return valid pixmap formats for this display."
  (xlib:display-pixmap-formats opal::*default-x-display*))

(defun depth-pixmap-format (depth)
  "Return a list of all pixmap formats with a given depth supported by
this display."
  (remove-if-not
   #'(lambda (format)
       (= (xlib:pixmap-format-depth format) depth))
   (get-pixmap-formats)))

(defun depth-to-bits-per-pixel (depth)
  "Return a bits-per-pixel value valid for a given depth.  Prefer
depth = bits-per-pixel if possible.  Otherwise just use the first
pixmap format in the list of valid formats."
  (let ((valid-formats (depth-pixmap-format depth)))
    (dolist (format valid-formats (xlib:pixmap-format-bits-per-pixel (car valid-formats)))
      (when (= (xlib:pixmap-format-bits-per-pixel format) depth)
	(return-from depth-to-bits-per-pixel depth)))))



;; <color-or-data> is used as a color (if <from-data-p> is nil) or as
;; actual data
;;
(defun x-create-image (root-window width height depth from-data-p
		       &optional color-or-data properties
			 bits-per-pixel left-pad data-array)
  (declare (ignore root-window data-array))
  (unless bits-per-pixel
    (setf bits-per-pixel (depth-to-bits-per-pixel depth)))
  (if from-data-p
      (let* ((bits-per-line (xlib::index* width bits-per-pixel))
	     (padded-bits-per-line
	      (xlib::index* (xlib::index-ceiling bits-per-line 32) 32))
	     (padded-bytes-per-line
	      (xlib::index-ceiling padded-bits-per-line 8)))
	(xlib:create-image
	 :width width :height height
	 :depth depth
	 :format :z-pixmap
	 :data color-or-data
	 :unit 32 :pad 32
	 :byte-lsb-first-p t :bit-lsb-first-p t
	 :bits-per-pixel bits-per-pixel
	 :plist properties
	 :bytes-per-line padded-bytes-per-line :left-pad left-pad))
      (let* ((element-type
	      (case bits-per-pixel
		(1  'xlib::pixarray-1-element-type)
		(4  'xlib::pixarray-4-element-type)
		(8  'xlib::pixarray-8-element-type)
		(16 'xlib::pixarray-16-element-type)
		(24 'xlib::pixarray-24-element-type)
		(32 'xlib::pixarray-32-element-type)
		(t
		 (cerror
		  "Ignore"
		  "gem::x-create-image-array: bits-per-pixel ~S is not valid (1, 8, 16, 24 or 32)"
		  depth)
		 'xlib::pixarray-8-element-type)))
	     (data-array (make-array (list height width)
				     :element-type element-type
					    
				     :initial-element
				     (if color-or-data
					 (g-value color-or-data :colormap-index)
					 opal::*white*))))
	(xlib:create-image
	 :depth depth
	 :bits-per-pixel bits-per-pixel
	 :width width
	 :height height
	 :format :z-pixmap
	 :data data-array ))))
  

(defun x-create-image-array (root-window width height depth)
  "Create an array that's suitable for an X image.  <depth> should be
1 for a bitmap and the depth of the display for a pixmap."
  (declare (ignore root-window))
  (make-array (list height width)
	      :element-type
	      (case depth
		(1  'xlib::pixarray-1-element-type)
		(4  'xlib::pixarray-4-element-type)
		(8  'xlib::pixarray-8-element-type)
		(16 'xlib::pixarray-16-element-type)
		(24 'xlib::pixarray-24-element-type)
		(32 'xlib::pixarray-32-element-type)
		(t
		 (cerror
		  "Ignore"
		  "gem::x-create-image-array: depth ~S is not valid (1, 8, 16, 24 or 32)"
		  depth)
		 'xlib::pixarray-8-element-type))))


;;; Creates a state mask for keyboard events.
;;;
(defun x-create-state-mask (root-window modifier)
  (declare (ignore root-window))
  (xlib:make-state-mask modifier))



(defun x-create-pixmap (window width height depth
			&optional image bitmap-p data-array)
  (declare (ignore data-array))
  (let* ((drawable (g-value window :drawable))
	 (pixmap (xlib:create-pixmap :width width
				     :height height
				     :depth depth
				     :drawable drawable)))
    (if image
      (let ((gc (xlib:create-gcontext :drawable pixmap :function boole-1
		  ;;; Since this pixmap is going to be used as a stipple mask,
                  ;;; you must have 1's in foreground, regardless of whether
                  ;;; *black* is 1 on this machine or not (on HP's, it's 0).
				      :foreground 1  ; NOT opal::*black*
				      :background 0  ; NOT opal::*white*
				      )))
	(xlib:put-image pixmap gc image
			:x 0 :y 0 :width width :height height
			:bitmap-p bitmap-p)
	(xlib:free-gcontext gc)
	(xlib:set-wm-properties drawable :icon-pixmap pixmap)))
    pixmap))



;;; RETURNS:
;;; the newly-created drawable.
;;;
(defun x-create-window (parent-window x y width height
			title icon-name
			background border-width
			save-under visible
			min-width min-height max-width max-height
			user-specified-position-p user-specified-size-p
			override-redirect)
  (let* ((display-info (g-value parent-window :display-info))
	 (drawable (xlib:create-window
		    :parent (g-value parent-window :drawable)
		    :x x
		    :y y
		    :width width
		    :height height
		    :background background
		    :border-width border-width
		    :border (xlib:screen-black-pixel (opal::display-info-screen
						      display-info))
		    :override-redirect override-redirect
		    :event-mask opal::*exposure-event-mask*
		    :save-under save-under
		    :class :input-output)))
    (setf (xlib:wm-hints drawable)
          (xlib:make-wm-hints :input :on :initial-state visible))
    (setf (xlib:wm-normal-hints drawable)
	  (xlib:make-wm-size-hints
	   :width-inc 1
	   :height-inc 1
	   :x x
	   :y y
	   :min-width min-width
	   :min-height min-height
	   :max-width max-width
	   :max-height max-height
	   :user-specified-position-p user-specified-position-p
	   :user-specified-size-p user-specified-size-p))

    (xlib:set-wm-properties drawable
;;;			    :client-machine
;;;			    (or 
;;;			     #+allegro
;;;			     (sys:getenv "HOSTNAME")
;;;			     "")
			    :resource-name "Opal"
			    :resource-class :opal
			    :name title
			    :icon-name icon-name)

    ;;; The following allows you to destroy windows by hand using the
    ;;; window manager.  Unfortunately, this does not work in lispworks, but
    ;;; causes an error with mysterious message "#\U is not of type integer".
    ;;;
    ;;; The same error appeared in clisp before the addition of
    ;;; :TRANSFORM #'xlib:char->card8. I guess it is related to missing error
    ;;; error checking in other implementations than clisp and lispworks.
    ;;; B. Haible 20.9.1993
    (xlib:change-property drawable
			  :WM_CLIENT_MACHINE (short-site-name)
			  :STRING 8)
    
    (xlib:change-property drawable :WM_PROTOCOLS
			  (list (xlib:intern-atom
				 (opal::display-info-display display-info)
				 "WM_DELETE_WINDOW"))
			  :ATOM 32)
    drawable))



(defun x-delete-font (root-window font)
  (declare (ignore root-window))
  (xlib:close-font (kr:g-cached-value font :xfont)))


(defun x-delete-pixmap (window pixmap &optional buffer-too)
  (xlib:free-pixmap pixmap)
  (if buffer-too
    (xlib:free-gcontext (g-value window :buffer-gcontext))))


;;; Destroys the <x-window>, a raw window (NOT an Opal window!)
;;;
(defun x-delete-window (root-window x-window)
  (declare (ignore root-window))
  (setf (getf (xlib:drawable-plist x-window) :garnet) NIL)
  (let ((display (xlib:window-display x-window)))
    (xlib:destroy-window x-window)
    (if display
      (xlib:display-force-output display))))  



;;; RETURNS: multiple values:
;;; - x of the last mouse event that was discarded;
;;; - y of the last mouse event;
;;; - Opal window in which the last event happened.
;;;
(defun x-discard-mouse-moved-events (root-window)
  (declare (ignore root-window))
  (let (current-x current-y current-win)
    #-allegro
    (loop
     (unless
	 (xlib:event-case (opal::*default-x-display*
			   :discard-p nil :timeout 0)
			  (:motion-notify ((:x x-prime) (:y y-prime)
					   (:event-window win-prime))
					  (setf current-x x-prime)
					  (setf current-y y-prime)
					  (setf current-win win-prime)
					  (if *mouse-debug* 
					    (incf *mouse-throw-aways*))
					  t)
			  (t () nil))	; any other event, return nil (causes
					; event-case to terminate), which causes
					; loop to terminate
       (return)))
    #+ALLEGRO
    (block throw-away
      (xlib:event-case (opal::*default-x-display*
			:discard-p t :timeout 0)
		       (:motion-notify ((:x x-prime) (:y y-prime)
					(:event-window win-prime))
				       (setf current-x x-prime)
				       (setf current-y y-prime)
				       (setf current-win win-prime)
				       (if *mouse-debug* 
					 (incf *mouse-throw-aways*))
				       nil)
		       (t () (return-from throw-away))))
    (values current-x current-y
	    (if current-win
	      (getf (xlib:drawable-plist current-win) :garnet)))))

#-(and cmu mp)
(defun x-discard-pending-events (root-window &optional (timeout 1))
  (declare (ignore root-window))
  (xlib:event-case (opal::*default-x-display* :discard-p t :timeout timeout)
		   (:destroy-notify () NIL) ; get rid of warnings
		   (otherwise () t)))

#+(and cmu mp)
(defun x-discard-pending-events (root-window &optional (timeout 1))
  (declare (ignore root-window timeout))
  (ext:flush-display-events opal::*default-x-display*))


(defun x-draw-arc (window x y width height angle1 angle2 function
			  line-style fill-style &optional pie-slice-p)
  (declare (ignore pie-slice-p))
  (let* ((thickness (if line-style
			(max 1 (g-value line-style :line-thickness))
                        0))
	 (thickness2 (* thickness 2))
	 (fill-width (max 0 (- width thickness2)))
	 (fill-height (max 0 (- height thickness2)))
	 (display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (setf function (get function :x-draw-function))
    (if fill-style
      (let ((filling-style-gc (opal::display-info-line-style-gc display-info)))
	(set-filling-style
	 fill-style
	 filling-style-gc
	 (opal::opal-gc-gcontext filling-style-gc) root-window function)
	(xlib:draw-arc drawable (opal::opal-gc-gcontext filling-style-gc)
		       (+ x thickness) (+ y thickness)
		       fill-width fill-height angle1 angle2 T)))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	     (half-thickness (truncate thickness 2))
	     (diameter (min width height))
	     (d-mod-2 (mod diameter 2))
	     (t-mod-2 (mod thickness 2)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(xlib:draw-arc
	 drawable xlib-gc-line
	 (+ x half-thickness
	    (aref opal::*left-adjustment* d-mod-2 d-mod-2 t-mod-2))
	 (+ y half-thickness
	    (aref opal::*top-adjustment* d-mod-2 d-mod-2 t-mod-2))
	 (max 0 (- width thickness
		   (aref opal::*width-adjustment* d-mod-2 d-mod-2 t-mod-2)))
	 (max 0 (- height thickness
		   (aref opal::*height-adjustment* d-mod-2 d-mod-2 t-mod-2)))
	 angle1 angle2 NIL)))))



(defun x-draw-image (window left top width height image function fill-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window))
	 (bitmap-p (= (xlib:image-depth image) 1)))

    (setf function (get function :x-draw-function))
    (if fill-style
      (let* ((fill-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-fill (opal::opal-gc-gcontext fill-style-gc)))
	(set-filling-style fill-style fill-style-gc xlib-gc-fill
			   root-window function)
	(if (and (eq (xlib:gcontext-fill-style xlib-gc-fill) :stippled)
		 bitmap-p)
	  (let ((save-stipple (xlib:gcontext-stipple xlib-gc-fill)))
	    (setf (xlib:gcontext-stipple xlib-gc-fill)
		  (opal::build-pixmap window image width height bitmap-p))
	    (setf (xlib:gcontext-ts-x xlib-gc-fill) left)
	    (setf (xlib:gcontext-ts-y xlib-gc-fill) top)
	    (xlib:draw-rectangle drawable xlib-gc-fill
				 left top width height t)
	    (if save-stipple
	      (setf (xlib:gcontext-stipple xlib-gc-fill) save-stipple)))
	  (xlib:put-image drawable xlib-gc-fill image
			  :x left
			  :y top
			  :width width
			  :height height
			  :bitmap-p bitmap-p))))))


(defun x-draw-line (window x1 y1 x2 y2 function line-style &optional drawable)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info)))
    ;; Provide the actual drawable of the window if you want to bypass drawing
    ;; into the buffer.  This is used by the gesture-interactor to draw lines
    ;; directly into the window, not the buffer.
    (unless drawable
      (setf drawable (the-drawable window)))
    (setf function (get function :x-draw-function))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(xlib:draw-line drawable xlib-gc-line x1 y1 x2 y2)))))  



(defun x-draw-lines (window point-list function line-style fill-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (setf function (get function :x-draw-function))
    (if fill-style
      (let* ((filling-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-filling (opal::opal-gc-gcontext filling-style-gc)))
	(set-filling-style
	 fill-style filling-style-gc xlib-gc-filling root-window function)
	(xlib:draw-lines drawable xlib-gc-filling point-list :fill-p T)))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(xlib:draw-lines drawable xlib-gc-line point-list)))))



(defun x-draw-points (window point-list function line-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	   (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
      (set-line-style line-style
		      line-style-gc xlib-gc-line
		      root-window (get function :x-draw-function))
      (xlib:draw-points drawable xlib-gc-line point-list))))



(defun x-draw-rectangle (window left top width height function
				line-style fill-style)
  (if (< width 1)
    (setf width 1))
  (if (< height 1)
    (setf height 1))
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window))
	 (thickness (if line-style
			(max (g-value line-style :line-thickness) 1) 0)))
    (setf function (get function :x-draw-function))
    (if fill-style
      (let* ((filling-style-gc (opal::display-info-line-style-gc display-info))
	     (gc (opal::opal-gc-gcontext filling-style-gc))
	     (th2 (* 2 thickness)))
	(set-filling-style fill-style filling-style-gc gc
			   root-window function)
	(xlib:draw-rectangle drawable gc
			     (+ left thickness) (+ top thickness)
			     (- width th2) (- height th2)
			     t)))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	     (half-thickness (truncate thickness 2)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(xlib:draw-rectangle drawable xlib-gc-line
			     (+ left half-thickness)
			     (+ top half-thickness)
			     (- width thickness)
			     (- height thickness) NIL)))))



(defun x-draw-roundtangle (window left top width height
				  x-radius y-radius function
				  line-style fill-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window))
	 (th (if line-style (max 1 (g-value line-style :line-thickness)) 0))
	 (th\2 (ceiling th 2))
	 (th/2 (floor th 2))
         ;; The mnemonic for c-w and c-h is "corner-width" and "corner-height"
	 (c-w (+ x-radius x-radius))
	 (c-h (+ y-radius y-radius)))
    (setf function (get function :x-draw-function))
    (if fill-style
      (let* ((filling-style-gc (opal::display-info-line-style-gc
				display-info))
	     (gc (opal::opal-gc-gcontext filling-style-gc))
	     (r x-radius)
	     (top-l (+ top y-radius))
	     (top-f (+ top th))
	     (right-l (- (+ left width) r))
	     (side-w (max 0 (- x-radius th)))
	     (side-h (- height y-radius y-radius))
	     (bottom-t (+ top height (- c-h))))
	(set-filling-style fill-style filling-style-gc
			   gc root-window function)
	;; center rectangle, from top to bottom
	(xlib:draw-rectangle drawable gc
			     (+ left r) top-f
			     (- width r r) (max 0 (- height th th)) T)
	;; two side rectangles
	(xlib:draw-rectangle drawable gc
			     (+ left th) top-l side-w side-h T)
	(xlib:draw-rectangle drawable gc
			     right-l top-l side-w side-h T)
	;; four filled arcs at the corners
	;; The cruddy +1 and -1 is to get the filling-styles and line-styles
	;; to touch at the corners
	(let* ((right-f (+ (- right-l x-radius) th))
	       (left-f (+ left th))
	       (left-f-1 (- left-f (if line-style 1 0)))
	       (top-f-1 (- top-f (if line-style 1 0)))
	       (bottom-f (+ bottom-t th))
	       (th2 (+ th th))
	       (c-w-f (max 0 (- c-w th2)))
	       (c-h-f (max 0 (- c-h th2)))
	       (c-w-f+1 (+ c-w-f (if line-style 1 0)))
	       (c-h-f+1 (+ c-h-f (if line-style 1 0))))
	  (xlib:draw-arc drawable gc right-f top-f-1
			             c-w-f c-h-f+1 0.0 gu:pi/2 T)
	  (xlib:draw-arc drawable gc (- left-f 1) (- top-f 1)
			             (+ c-w-f 1)  (+ c-h-f 1)
				     gu:pi/2 gu:pi/2 T)
	  (xlib:draw-arc drawable gc left-f-1 bottom-f c-w-f+1 c-h-f
			             pi gu:pi/2 T)
	  (xlib:draw-arc drawable gc right-f bottom-f c-w-f c-w-f
			             gu:pi3/2 gu:pi/2 T))
	t))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	     (left-w (+ left x-radius))
	     (right (+ left width (- x-radius))))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	;; Top and bottom segments
	(let* ((y (+ top th/2))
	       (y1 (+ top height (- th\2)))
	       (l (+ left th/2))
	       (l1 (+ left width (- th\2)))
	       (up (+ top y-radius))
	       (down (+ top height (- y-radius)))
	       (c-w (max 0 (- c-w th)))
	       (c-h (max 0 (- c-h th))))
	  (xlib:draw-segments drawable xlib-gc-line
			      (list left-w y right y
				    left-w y1 right y1
				    l up l down
				    l1 up l1 down))
	  (let ((left (+ left th\2 (if (< th 2) -1 0)))
		(right (+ left width (- c-w) (- th\2) (if (< th 2) 0 -1)))
		(bottom (+ top height (- c-h) (- th\2) (if (<= th 3) 0 -1))))
	    ;; Four arcs
	    (xlib:draw-arc drawable xlib-gc-line
			   right (+ top th/2) c-w c-h 0.0 gu:pi/2)
	    (xlib:draw-arc drawable xlib-gc-line
			   left (+ top th/2) c-w c-h gu:pi/2 gu:pi/2)
	    (xlib:draw-arc drawable xlib-gc-line
			   left bottom c-w c-h pi gu:pi/2)
	    (xlib:draw-arc drawable xlib-gc-line
			   right bottom c-w c-h gu:pi3/2 gu:pi/2)))))))



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


;;; Given a drawable or pixmap, returns the associated Opal window.
;;;
(defun x-drawable-to-window (root-window drawable)
  (declare (ignore root-window))
  (getf (xlib:drawable-plist drawable) :garnet))


;;; From windows.lisp (eliminate the obsolete deleting-window-drop-events)
;;;
(defun destroy-notify-window (event-window)
  (let ((display (xlib:window-display event-window)))
    (xlib:display-finish-output display)
    #+cmu
    (let ((result nil))
      (xlib:process-event
       display :timeout 0
       :handler #'(lambda (&key cmu-event-window a-window &allow-other-keys)
		    (if (or (eq cmu-event-window event-window)
			    (eq a-window event-window))
		      (setf result t)
		      nil)))
      result)
    #-cmu
    (xlib:discard-current-event display)
    t))


(defun connected-window-p (event-window)
  (let ((a-window (getf (xlib:drawable-plist event-window) :garnet)))
    (if a-window
      (let ((drawable (g-value a-window :drawable)))
	(and drawable (= (xlib:window-id drawable)
			 (xlib:window-id event-window)))))))


;;; Taken from windows.lisp
;;;
(defun Delete-Notify (event-debug event-window)
  (if event-debug (format t " delete-notify ~s~%"
			  event-window;; DZG  (xlib:window-id event-window)
			  ))
  ;; Will be changed to take a-window as a parameter, rather than event-window.
  ;; Hence, the following will be unnecessary.
  (let ((a-window (getf (xlib:drawable-plist event-window) :garnet)))
    (if a-window
      (if (schema-p a-window)
	(let ((drawable (g-value a-window :drawable)))
	  (if (and drawable (= (xlib:window-id drawable)
			       (xlib:window-id event-window)))
	    (opal:destroy a-window)
	    ;; Then event-window is an orphaned window
	    (x-delete-window a-window event-window)))
	;; Then event-window is an orphaned window
	(x-delete-window NIL event-window)))))



;;; Used, for example, by the MouseLine.
;;;
(defun do-client-message (event-window type data format display)
  (cond ((and (eq format 32)
	      (eq type :WM_PROTOCOLS)
	      (eq (xlib:atom-name 
		   display
		   (aref (the (simple-array (unsigned-byte 32) (5)) data) 0))
		  :WM_DELETE_WINDOW))
	 (Delete-Notify NIL event-window))
	((and (eq format 32)
	      (eq type :TIMER_EVENT))
	 (if interactors::*trans-from-file*
	   T
	   ;; ignore events when read transcript
	   (interactors::Queue-Timer-Event
	    (aref (the (simple-array (unsigned-byte 32) (5)) data) 0)))))
  NIL)



;;; Returns list of drawable, parent, grandparent, ... , root.
;;;
(defun lineage-of-drawable (drawable)
  (multiple-value-bind (children parent root)
      (xlib:query-tree drawable)
    (declare (ignore children))
    (if (xlib:drawable-equal parent root)
      (list drawable root)
      (cons drawable (lineage-of-drawable parent)))))


#-debug-event-handler
(defmacro event-handler-debug (message &rest args)
  (declare (ignore message args))
  )

#+debug-event-handler
(defmacro event-handler-debug (message &rest args)
  `(format t "event-handler ~S   ~S~%" ,message ',args))



;;; Input event handling
;;;
(defun x-event-handler (root-window ignore-keys)
  (let ((display (the-display root-window)))
    (xlib:event-case
     (display :discard-p t :timeout (if ignore-keys 0 NIL))
     ;; this first one is for when a window is deleted by the wm
     (:CLIENT-MESSAGE
      (event-window type data format)
      (event-handler-debug :CLIENT-MESSAGE event-window type data format)
      (do-client-message event-window type data format display))
     (:MAP-NOTIFY
      (event-window)
      (event-handler-debug :MAP-NOTIFY)
      (interactors::do-map-notify (x-window-from-drawable root-window
							  event-window)))
     (:UNMAP-NOTIFY
      (event-window)
      (event-handler-debug :UNMAP-NOTIFY)
      (interactors::do-unmap-notify (x-window-from-drawable root-window
							    event-window)))
     (:REPARENT-NOTIFY
      (event-window)
      (event-handler-debug :REPARENT-NOTIFY)
      (if (connected-window-p event-window)
	(let ((window (x-window-from-drawable root-window event-window)))
	  (s-value window :already-initialized-border-widths nil)
	  (s-value window :lineage (lineage-of-drawable event-window)))))
     (:CIRCULATE-NOTIFY
      ()
      (event-handler-debug :CIRCULATE-NOTIFY)
      (interactors::do-circulate-notify))
     (:GRAVITY-NOTIFY
      ()
      (event-handler-debug :GRAVITY-NOTIFY)
      (interactors::do-gravity-notify))
     (:DESTROY-NOTIFY
      (event-window)
      (event-handler-debug :DESTROY-NOTIFY)
      (destroy-notify-window event-window))
     (:CONFIGURE-NOTIFY
      (event-window x y width height above-sibling)
      (event-handler-debug :CONFIGURE-NOTIFY)
      (if (connected-window-p event-window)
	(interactors::do-configure-notify (x-window-from-drawable root-window
								  event-window)
	  x y width height above-sibling)))
     (:EXPOSURE
      (event-window x y width height count)
      (event-handler-debug :EXPOSURE x y width height count)
      (if (connected-window-p event-window)
	(interactors::do-exposure (x-window-from-drawable root-window
							  event-window)
	  x y width height count display)))
     (:KEY-PRESS
      (event-window x y state code time)
      (event-handler-debug :KEY-PRESS event-window x y state code time)
      (if ignore-keys
	;; We don't want keys, but check if this is the abort key
	(let ((c (x-translate-character *root-window* 0 0 state code 0)))
	  (when (eq c interactors::*garnet-break-key*)
	    (format T "~%**Aborting transcript due to user command**~%")
	    (return-from x-event-handler :abort)))
	;; Normal case: we do want keys
	(interactors::do-key-press
	  (x-window-from-drawable root-window
				  event-window) x y state code time)))
     (:BUTTON-PRESS
      (event-window x y state code time event-key)
      (event-handler-debug :BUTTON-PRESS event-window x y state code time
			   event-key)
      (unless ignore-keys
	(interactors::do-button-press (x-window-from-drawable root-window
							      event-window)
	  x y state code time event-key)))
     (:BUTTON-RELEASE
      (event-window x y state code time event-key)
      (event-handler-debug :BUTTON-RELEASE event-window x y state code time
			   event-key)
      (unless ignore-keys
	(interactors::do-button-release (x-window-from-drawable root-window
								event-window)
	  x y state code time event-key)))
     (:MOTION-NOTIFY
      (event-window x y)
      (event-handler-debug :MOTION-NOTIFY event-window x y)
      (unless ignore-keys
	(interactors::do-motion-notify (x-window-from-drawable root-window
							       event-window)
	  x y display)))
     (:ENTER-NOTIFY
      (event-window x y time)
      (event-handler-debug :ENTER-NOTIFY event-window x y time)
      (unless ignore-keys
	(interactors::do-enter-notify (x-window-from-drawable root-window
							      event-window)
	  x y time)))
     (:LEAVE-NOTIFY
      (event-window x y time)
      (event-handler-debug :LEAVE-NOTIFY event-window x y time)
      (unless ignore-keys
	(interactors::do-leave-notify (x-window-from-drawable root-window
							      event-window)
	  x y time)))
     (:NO-EXPOSURE
      ()
      (event-handler-debug :NO-EXPOSURE)
      (unless ignore-keys
	t))
     (OTHERWISE () (format t "illegal event") t))))


#|
;;; Old version, for old-style interactors
(defun x-event-handler (root-window ignore-keys)
  (let ((display (the-display root-window)))
    (xlib:event-case
     (display :discard-p t :timeout (if ignore-keys 0 NIL))
     ;; this first one is for when a window is deleted by the wm
     (:CLIENT-MESSAGE
      (event-window type data format)
      (event-handler-debug :CLIENT-MESSAGE event-window type data format)
      (do-client-message event-window type data format display))
     (:MAP-NOTIFY
      (event-window)
      (event-handler-debug :MAP-NOTIFY)
      (interactors::do-map-notify (x-window-from-drawable root-window
							  event-window)))
     (:UNMAP-NOTIFY
      (event-window)
      (event-handler-debug :UNMAP-NOTIFY)
      (interactors::do-unmap-notify (x-window-from-drawable root-window
							    event-window)))
     (:REPARENT-NOTIFY
      (event-window)
      (event-handler-debug :REPARENT-NOTIFY)
      (if (connected-window-p event-window)
	(let ((window (x-window-from-drawable root-window event-window)))
	  (s-value window :already-initialized-border-widths nil)
	  (s-value window :lineage (lineage-of-drawable event-window)))))
     (:CIRCULATE-NOTIFY
      ()
      (event-handler-debug :CIRCULATE-NOTIFY)
      (interactors::do-circulate-notify))
     (:GRAVITY-NOTIFY
      ()
      (event-handler-debug :GRAVITY-NOTIFY)
      (interactors::do-gravity-notify))
     (:DESTROY-NOTIFY
      (event-window)
      (event-handler-debug :DESTROY-NOTIFY)
      (destroy-notify-window event-window))
     (:CONFIGURE-NOTIFY
      (event-window x y width height above-sibling)
      (event-handler-debug :CONFIGURE-NOTIFY)
      (if (connected-window-p event-window)
	(interactors::do-configure-notify (x-window-from-drawable root-window
								  event-window)
	  x y width height above-sibling)))
     (:EXPOSURE
      (event-window x y width height count)
      (event-handler-debug :EXPOSURE x y width height count)
      (if (connected-window-p event-window)
	(interactors::do-exposure (x-window-from-drawable root-window
							  event-window)
	  x y width height count display)))
     (:KEY-PRESS
      (event-window x y state code time)
      (event-handler-debug :KEY-PRESS event-window x y state code time)
      (if ignore-keys
	;; We don't want keys, but check if this is the abort key
	(let ((c (translate-character display code state)))
	  (when (eq c interactors::*garnet-break-key*)
	    (format T "~%**Aborting transcript due to user command**~%")
	    (return-from x-event-handler :abort)))
	;; Normal case: we do want keys
	(interactors::do-key-press event-window
	  ;; DZG - must be fixed here and below (x-window-from-drawable root-window event-window)
	  x y state code time)))
     (:BUTTON-PRESS
      (event-window x y state code time event-key)
      (event-handler-debug :BUTTON-PRESS event-window x y state code time
			   event-key)
      (unless ignore-keys
	(interactors::do-button-press event-window
	  x y state code time event-key)))
     (:BUTTON-RELEASE
      (event-window x y state code time event-key)
      (event-handler-debug :BUTTON-RELEASE event-window x y state code time
			   event-key)
      (unless ignore-keys
	(interactors::do-button-release event-window
	  x y state code time event-key)))
     (:MOTION-NOTIFY
      (event-window x y)
      (event-handler-debug :MOTION-NOTIFY event-window x y)
      (unless ignore-keys
	(interactors::do-motion-notify event-window x y display)))
     (:ENTER-NOTIFY
      (event-window x y time)
      (event-handler-debug :ENTER-NOTIFY event-window x y time)
      (unless ignore-keys
	(interactors::do-enter-notify event-window x y time)))
     (:LEAVE-NOTIFY
      (event-window x y time)
      (event-handler-debug :LEAVE-NOTIFY event-window x y time)
      (unless ignore-keys
	(interactors::do-leave-notify event-window x y time)))
     (:NO-EXPOSURE
      ()
      (event-handler-debug :NO-EXPOSURE)
      (unless ignore-keys
	t))
     (OTHERWISE () (format t "illegal event") t))))
|#


(defun x-flush-output (window)
  (xlib:display-force-output (the-display window)))



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

(defun x-font-name-p (root-window arg)
  (declare (ignore root-window))
  (stringp arg))

;; Returns either a string which describes the font using X conventions,
;; or a cons of the bad value and slot.
(defun x-make-font-name (root-window key)
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
           (if (subsetp face-spec *x-font-faces*)
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



;;; Sets the Cut buffer for X.  Note that this does NOT do a select, and
;;; therefore the cut buffer will not be affected if there already is a
;;; selection in some xterm window.
;;;
(defun x-get-cut-buffer (window)
  (xlib:cut-buffer
   (opal::display-info-display (g-value window :display-info))))



;;; -------------------------------------------------- Event masks

;;; pem = pointer-event-mask, used to change an active pointer grab
;;;   (having :key-press in here makes it crash)
;;; em = eventmask , used to change an event mask

;;; In the Gem interface, the following are referred to by keywords, whose
;;; names are encoded as follows:
;;; E if enter/leave events are to be reported;
;;; G if the mouse is to be grabbed;
;;; K if keyboard events are to be reported;
;;; M if mouse motions are to be reported.
;;; For example, :E-G-K refers to enter-leave-ignore-motion-grab-em


(defparameter *report-motion-pem*
  (xlib:make-event-mask :button-press :button-release
			:pointer-motion))

(defparameter *enter-leave-report-motion-pem*
  (xlib:make-event-mask :button-press :button-release
			:pointer-motion
			:enter-window
			:leave-window))


(defparameter *ignore-motion-grab-em*
  (xlib:make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify
			:owner-grab-button))

(defparameter *enter-leave-ignore-motion-grab-em*
  (xlib:make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify
			:enter-window
			:leave-window
			:owner-grab-button))

(defparameter *ignore-motion-em*
  (xlib:make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify))

(defparameter *enter-leave-ignore-motion-em*
  (xlib:make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify
			:enter-window
			:leave-window))


;;; em = eventmask , used to change an event mask
(defparameter *report-motion-em*
  (xlib:make-event-mask :button-press :button-release
			:key-press
			:exposure
			:pointer-motion
			:structure-notify))


(defparameter *enter-leave-report-motion-em*
  (xlib:make-event-mask :button-press :button-release
			:key-press
			:exposure
			:pointer-motion
			:structure-notify
			:enter-window
			:leave-window))



;;; RETURNS: a single pixel of an image.
;;;
(defun x-image-bit (root-window image x y)
  (declare (ignore root-window))
  (let* ((bytes-per-line (xlib::image-x-bytes-per-line image))
	 (byte-pos (+ (floor x 8) (* bytes-per-line y)))
	 (byte (aref (xlib::image-x-data image) byte-pos))
	 (bit-pos (mod x 8)))
    (logbitp bit-pos byte)))


;;; Create an X bitmap from a series of patterns (specified as integers)
;;;
(defun x-image-from-bits (root-window patterns)
  (declare (ignore root-window))
  (apply #'xlib:bitmap-image patterns))

;;; Create an X bitmap from a series of patterns (specified as bit-vectors)
;;;
(defun x-device-image (root-window index)
  (declare (ignore root-window))
  (let ((descriptor (opal::get-descriptor index)))
    (apply #'xlib:bitmap-image descriptor)))


;;; RETURNS: the <image>'s hot spot, as multiple values.
;;;
(defun x-image-hot-spot (root-window image)
  (declare (ignore root-window))
  (values (xlib:image-x-hot image)
	  (xlib:image-y-hot image)))


;;; Given an X image, returns its size as multiple values (width, height,
;;; depth).
;;;
(defun x-image-size (root-window image)
  (declare (ignore root-window))
  (values (xlib:image-width image)
	  (xlib:image-height image)
	  (xlib:image-depth image)))



;;; Given an image, returns its internal array.
;;;
(defun x-image-to-array (root-window image)
  (declare (ignore root-window))
  (xlib:image-z-pixarray image))



;;; Helper function for x-initialize-window-borders
;;;
(defun set-four-borders (window left &optional top right bottom)
  (unless top
    ;; Only left specified - use for all three.
    (setf top (setf right (setf bottom left))))
  (s-value window :window :left-border-width left)
  (s-value window :top-border-width top)
  (s-value window :right-border-width right)
  (s-value window :bottom-border-width bottom))



(defun x-initialize-device (root-window)
  (declare (ignore root-window))
  (let* ((x-line-style-gc
	  (xlib:create-gcontext :drawable opal::*default-x-root*
				:cache-p t
				:function 2
				:foreground opal::*black*
				:background opal::*white*
				:line-width 0
				:line-style :solid
				:cap-style :butt
				:join-style :miter
				:fill-style :solid
				:fill-rule :even-odd))
	 (x-filling-style-gc
	  (xlib:create-gcontext :drawable opal::*default-x-root*
				:cache-p t
				:function 2
				:foreground opal::*black*
				:background opal::*white*
				:line-width 0
				:line-style :solid
				:cap-style :butt
				:join-style :miter
				:fill-style :solid
				:fill-rule :even-odd))
	 (opal-line-style-gc
	  (opal::make-opal-gc	:gcontext x-line-style-gc
			:opal-style NIL
			:function 2
			:line-width 0
			:line-style :solid
			:cap-style  :butt
			:join-style :miter
			:dashes NIL
			:font   NIL
			:fill-style :solid
			:fill-rule  :even-odd
			:stipple   NIL
			:clip-mask :none
			:stored-clip-mask (make-list 8)))
	 (opal-filling-style-gc
	  (opal::make-opal-gc	:gcontext x-filling-style-gc
			:opal-style NIL
			:function 2
			:line-width 0
			:line-style :solid
			:cap-style  :butt
			:join-style :miter
			:dashes NIL
			:font   NIL
			:fill-style :solid
			:fill-rule  :even-odd
			:stipple   NIL
			:clip-mask :none
			:stored-clip-mask (make-list 8))))

    (opal::make-display-info :display opal::*default-x-display*
		       :screen  opal::*default-x-screen*
		       :root-window opal::*default-x-root*
		       :line-style-gc opal-line-style-gc
		       :filling-style-gc opal-filling-style-gc)))



;;; Set the border widths of the <window>.  This is quite complex, because of
;;; the differences among various window systems.
;;;
(defun x-initialize-window-borders (window drawable)
  ;; find out what borders really are
  (if (g-value window :parent);; window is really subwindow
    (set-four-borders window (xlib:drawable-border-width drawable))
    (let ((lineage (g-value window :lineage)))
      (case (length lineage)
	(2				; UWM or window without title
	 (set-four-borders window (xlib:drawable-border-width drawable)))
	(3				; TWM
	 (let ((border-width (xlib:drawable-border-width (second lineage))))
	   (set-four-borders
	    window
	    (+ border-width (xlib:drawable-x drawable))
	    (+ border-width (xlib:drawable-y drawable))
	    (- (xlib:drawable-width (second lineage))
	       (xlib:drawable-width (first lineage))
	       (xlib:drawable-x (first lineage))
	       (- border-width))
	    (- (xlib:drawable-height (second lineage))
	       (xlib:drawable-height (first lineage))
	       (xlib:drawable-y (first lineage))
	       (- border-width)))))
	((4 6)				; MWM and DECWindows, or possibly TVTWM
	 ;; if it is TVTWM, i.e. 3rd window is virtual root
	 (if (xlib:get-property (third lineage) :__SWM_VROOT)
	   (let* ((parent (second lineage))
		  (border-width (xlib:drawable-border-width parent)))
	     (set-four-borders
	      window
	      (+ border-width (xlib:drawable-x (first lineage)))
	      (+ border-width (xlib:drawable-y (first lineage)))
	      (- (xlib:drawable-width (second lineage))
		 (xlib:drawable-width (first lineage))
		 (xlib:drawable-x (first lineage))
		 (- border-width))
	      (- (xlib:drawable-height (second lineage))
		 (xlib:drawable-height (first lineage))
		 (xlib:drawable-y (first lineage))
		 (- border-width))))
	   (let* ((parent (second lineage))
		  (grandparent (third lineage))
		  (left-border-width
		   (MAX (xlib:drawable-x parent)	      ; MWM
			(xlib:drawable-border-width parent))) ; DECwindows
		  (top-border-width (xlib:drawable-y parent)))
	     (set-four-borders window
			       left-border-width
			       top-border-width
			       (- (xlib:drawable-width grandparent)
				  (xlib:drawable-width parent)
				  left-border-width)
			       (- (xlib:drawable-height grandparent)
				  (xlib:drawable-height parent)
				  top-border-width)))))))))


(defun x-inject-event (window index)
  (let ((drawable (g-value window :drawable)))
    (xlib:send-event drawable 
		     :client-message nil
		     :event-window drawable
		     :type :TIMER_EVENT
		     :format 32
		     :data (list index))))



;;; Does a map-window, and then waits for it to actually appear
;;; on the screen.  The waiting is necessary, because otherwise
;;; objects in the window won't appear in Lucid and Allegro
;;; (due to some race condition).
;;;
#-sb-thread
(defun x-map-and-wait (a-window drawable)
  (let ((display (the-display a-window)))
    #+(or allegro cmu)
    (when (eq (xlib:window-map-state drawable) :unmapped)
      (let ((suspend-process (opal::main-event-loop-process-running-p)))
	(when suspend-process
	  (opal::kill-main-event-loop-process))
	(xlib:map-window drawable)
	(xlib:display-force-output display)
	(xlib:event-case (display :discard-p nil :peek-p t :timeout 5)
			 (:map-notify (event-window)
				      (eq event-window drawable)))
	(when suspend-process
	  (opal::launch-main-event-loop-process))))
    #-(or allegro cmu)
    (progn
      (xlib:map-window drawable)
      (xlib:display-force-output display))))


#+sb-thread
(defun x-map-and-wait (a-window drawable)
  (let ((display (the-display a-window)))
    (sb-thread:with-recursive-lock (opal::*update-lock*)
      (xlib:map-window drawable)
      (xlib:display-force-output display)
      )
    (loop
	 (if (eq (xlib:window-map-state drawable) :unmapped)
	     (sleep .1)
	     (return t)))))


(defun x-max-character-ascent (root-window opal-font)
  (declare (ignore root-window))
  (xlib:max-char-ascent (g-value opal-font :xfont)))


(defun x-max-character-descent (root-window opal-font)
  (declare (ignore root-window))
  (xlib:max-char-descent (g-value opal-font :xfont)))


;;; If <grab-p>, this is a mouse grab or a change-active-pointer grab;
;;; otherwise, it is a mouse ungrab.
;;; If <owner-p> is a keyword, then do a change-active-pointer-grab; otherwise,
;;; do a regular grab-pointer.
;;;
(defun x-mouse-grab (window grab-p want-enter-leave &optional owner-p)
  (declare (ignore window))
  (if grab-p
    ;; Mouse grab.
    (if (keywordp owner-p)
      (xlib:change-active-pointer-grab opal::*default-x-display*
				       (if want-enter-leave
					 *enter-leave-report-motion-pem*
					 *report-motion-pem*))
      (xlib:grab-pointer opal::*default-x-display*
			 (if want-enter-leave
			   *enter-leave-report-motion-pem*
			   *report-motion-pem*)
			 :owner-p owner-p))
    ;; Mouse ungrab.
    (xlib:ungrab-pointer opal::*default-x-display*)))
    



;;; Move the <window> to the top (if <raise-p>) or to the bottom.
;;;
(defun x-raise-or-lower (window raise-p)
  (setf (xlib:window-priority (g-value window :drawable))
	(if raise-p :above :below)))



(defun x-read-an-image (root-window pathname)
  (declare (ignore root-window))
  (xlib:read-bitmap-file pathname))



;;; Reparent a window.
;;;
(defun x-reparent (window new-parent drawable left top)
  (if new-parent
    (if (is-a-p new-parent opal::window)
      (xlib:reparent-window drawable
			    (g-value new-parent :drawable)
			    left top)
      (error "Parent ~S of window ~S is not of type window~%"
	     new-parent window))
    (xlib:reparent-window drawable
			  (opal::display-info-root-window
			   (g-value window :display-info))
			  left top)))



(defun x-set-clip-mask (a-window clip-mask &optional lstyle-ogc fstyle-ogc)
  (declare (ignore a-window))
  (let ((lstyle-xgc (opal::opal-gc-gcontext lstyle-ogc))
	(fstyle-xgc (opal::opal-gc-gcontext fstyle-ogc)))
    (set-gc lstyle-ogc lstyle-xgc :clip-mask clip-mask)
    (set-gc fstyle-ogc fstyle-xgc :clip-mask clip-mask)))



(defun x-set-cut-buffer (window string)
  (setf (xlib:cut-buffer
	 (opal::display-info-display (g-value window :display-info)))
	string))



(defun get-display-number (display)
  (let* ((dlist (coerce display 'list))
         (numstr (progn
                   (do ((c (pop dlist) (pop dlist)))
                       ((or (eq c nil) (eq c '#\:))))
                   (do ((c (pop dlist) (pop dlist))
                        (numlist nil))
                       ((or (eq c nil) (eq c '#\.))
                        (coerce (reverse numlist) 'string))
                       (push c numlist))))
         (num (if (equal numstr "") 0 (read-from-string numstr))))
    num))



(defun x-set-device-variables (root-window &aux auth-name auth-data)
  (declare (ignore root-window)
	   #-allegro (ignore auth-name auth-data)
	   )
  (setq *default-x-display-number*
        opal::*default-x-display-number*)
;;	(get-display-number opal::*default-x-display-name*))

  (setq opal::*default-x-display*
	#-allegro
	(xlib:open-display opal::*default-x-display-name*
			   :display *default-x-display-number*)
	#+allegro
	(or
;;;	 (ignore-errors
;;;	   (common-windows::open-display-with-auth
;;;	    opal::*default-x-display-name* *default-x-display-number*))
	 (ignore-errors
	   (xlib:open-display opal::*default-x-display-name*
			   :display *default-x-display-number*)))
	)
  (setq opal::*default-x-screen*
        (nth opal::*default-x-screen-number*
             (xlib:display-roots opal::*default-x-display*)))
  (setq opal::*screen-width* (xlib:screen-width opal::*default-x-screen*))
  (setq opal::*screen-height* (xlib:screen-height opal::*default-x-screen*))
  (setq opal::*default-x-root* (xlib:screen-root opal::*default-x-screen*))

  ;;; We must call xlib:open-display a second time to get to the colormap,
  ;;; because it turns out that if we simply used the *default-x-display*
  ;;; to get at the colormap, then every time xlib:alloc-color was called
  ;;; it would cause an implicit xlib:display-force-output.
  ;;; (Except that in CMUCL you cannot use two displays at one time.)
  (setq opal::*default-x-colormap*
	(xlib:screen-default-colormap
	 #+cmu
	 opal::*default-x-screen*
	 #-cmu
	 (nth opal::*default-x-screen-number*
	      (xlib:display-roots
	       #-allegro
	       (xlib:open-display
		opal::*default-x-display-name*
		:display *default-x-display-number*)
	       #+allegro
	       (or
;;;		(ignore-errors
;;;		  (xcw::open-display-with-auth opal::*default-x-display-name* *default-x-display-number*))
		(ignore-errors
		  (xlib:open-display
		   opal::*default-x-display-name*
		   :display *default-x-display-number*)))
	       ))))
  (setq opal::*white* (xlib:screen-white-pixel opal::*default-x-screen*))
  (setq opal::*black* (xlib:screen-black-pixel opal::*default-x-screen*))
  (setf opal::*exposure-event-mask*
	(xlib:make-event-mask :exposure :structure-notify
			      :button-press :key-press)))



;;; Sets the pointer from a raw X <drawable> (a drawable or pixmap) to the
;;; Opal <window>.
;;;
(defun x-set-drawable-to-window (window drawable)
  (if (xlib:pixmap-p drawable)
      (setf (xlib:pixmap-plist drawable) (list :garnet window))
      (setf (xlib:window-plist drawable) (list :garnet window))))

(defun x-set-draw-function-alist (root-window)
  (declare (ignore root-window))
  (setq opal::*function-alist*
	(cond ((or t (zerop opal::*white*))	; Sparc
	       `((:clear . ,boole-clr)	; (color, opal::*white* = 0)
		 (:set . ,boole-set)
		 (:copy . ,boole-1)
		 (:no-op . ,boole-2)
		 (:copy-inverted . ,boole-c1)
		 (:invert . ,boole-c2)
		 (:and . ,boole-and)
		 (:or . ,boole-ior)
		 (:xor . ,boole-xor)
		 (:equiv . ,boole-eqv)
		 (:nand . ,boole-nand)
		 (:nor . ,boole-nor)
		 (:and-inverted . ,boole-andc1)
		 (:and-reverse . ,boole-andc2)
		 (:or-inverted . ,boole-orc1)
		 (:or-reverse . ,boole-orc2)))
	      (opal::*is-this-a-color-screen?* ; HP
	       `((:clear . ,boole-set)	       ; (color, opal::*white* = 1)
		 (:set . ,boole-clr)
		 (:copy . ,boole-1)
		 (:no-op . ,boole-2)
		 (:copy-inverted . ,boole-c1)
		 (:invert . ,boole-c2)
		 (:and . ,boole-ior)
		 (:or . ,boole-and)
		 (:xor . ,boole-xor)
		 (:equiv . ,boole-eqv)
		 (:nand . ,boole-nand)
		 (:nor . ,boole-nor)
		 (:and-inverted . ,boole-orc1)
		 (:and-reverse . ,boole-orc2)
		 (:or-inverted . ,boole-andc1)
		 (:or-reverse . ,boole-andc2)))
	      (t			; IBM-RT (black-and-white)
	       `((:clear . ,boole-set)	; (black-and-white, opal::*white* = 1)
		 (:set . ,boole-clr)
		 (:copy . ,boole-1)
		 (:no-op . ,boole-2)
		 (:copy-inverted . ,boole-c1)
		 (:invert . ,boole-c2)
		 (:and . ,boole-ior)
		 (:or . ,boole-and)
		 (:xor . ,boole-eqv)
		 (:equiv . ,boole-xor)
		 (:nand . ,boole-nor)
		 (:nor . ,boole-nand)
		 (:and-inverted . ,boole-orc1)
		 (:and-reverse . ,boole-orc2)
		 (:or-inverted . ,boole-andc1)
		 (:or-reverse . ,boole-andc2)))))
  ;; For erasing buffers
  (setq opal::*copy* (cdr (assoc :copy opal::*function-alist*))))


(defun x-set-window-property (window property value)
  "RETURNS:
     normally NIL; returns T if:
     - the property is :WIDTH or :HEIGHT and a new buffer is 
       required because the old one was too small; or
     - the property is :VISIBLE and the window needs to be mapped."
  (case property
    (:BACKGROUND-COLOR
     (let* ((gc (g-value window :buffer-gcontext))
	    (drawable (g-value window :drawable))
	    (index (x-color-to-index window value)))
       (setf (xlib:window-background drawable) index)
       (when gc (setf (xlib:gcontext-background gc) index))
       (when (g-value window :visible)
	 (xlib:map-window drawable)))
     nil)
    (:BUFFER-GCONTEXT
     ;; The <value> is a list of three elements: (buffer foregr. backgr.)
     (s-value window :buffer-gcontext
	      (xlib:create-gcontext :drawable (first value)
				    :foreground (second value)
				    :background (third value))))
    (:CURSOR
     (setf (xlib:window-cursor (g-value window :drawable)) value))
    (:EVENT-MASK
     ;; The <value> should be a keyword, encoded as explained above the ...em
     ;; defparameters.
     (let ((skip-force-output NIL))
       (setf (xlib:window-event-mask (g-value window :drawable))
	     (case value
	       (:E-K *enter-leave-ignore-motion-em*)
	       (:K *ignore-motion-em*)
	       (:E-G-K *enter-leave-ignore-motion-grab-em*)
	       (:G-K *ignore-motion-grab-em*)
	       (:E-K-M
		(setf skip-force-output T)
		*enter-leave-report-motion-em*)
	       (:K-M
		(setf skip-force-output T)
		*report-motion-em*)
	       (T
		(error
		 "Illegal keyword ~S in gem:set-window-property (:EVENT-MASK)"
		 value))))
       (if skip-force-output
	 ;; CMUCL does not call display-force-output automatically after the
	 ;; event-mask is changed (which is consistent with the CLX docs).
	 ;; But since this operation is expensive, only do it for CMUCL.
	 #+CMU (xlib:display-force-output opal::*default-x-display*)
	 #-CMU NIL

	 ;; Need to force-output when using the background m-e-l
	 ;; process,  otherwise this doesn't get noticed.
	 (xlib:display-force-output opal::*default-x-display*))))
    (:EVENT-POSITION
     ;; This is used after a :configure-notify event has given us what it
     ;; thinks are the X and Y coordinates for the window.  For certain
     ;; window managers, we have to do some massaging of the actual numbers.
     ;; In this case, the <value> is a list of X, Y, and the event window.
     (let ((lineage (or (g-value window :lineage)
			(s-value window :lineage
				 (lineage-of-drawable (third value)))))
	   (x (first value))
	   (y (second value)))
       ;; Use the length of the lineage to determine what window manager.
       (case (length lineage)
	 (2				; UWM or window without label.
	  (s-value window :left x)
	  (s-value window :top y))
	 (3				; TWM
	  (s-value window :left (xlib:drawable-x (second lineage)))
	  (s-value window :top (xlib:drawable-y (second lineage))))
	 ((4 6)				; MWM and DECWindows, or possibly TVTWM
	  (let ((3rd (third lineage)))
	    (if (xlib:get-property 3rd :__SWM_VROOT)
	      (let ((2nd (second lineage)))
		(s-value window :left (xlib:drawable-x 2nd))
		(s-value window :top (xlib:drawable-y 2nd)))
	      (progn
		(s-value window :left (xlib:drawable-x 3rd))
		(s-value window :top (xlib:drawable-y 3rd)))))))))
    (:HEIGHT
     (let ((old-buffer (g-value window :buffer)))
       (setf (xlib:drawable-height (g-value window :drawable))
	     (max 0 value))
       (setf (opal::win-update-info-height (g-value window :win-update-info))
	     value)
       ;; Does the buffer need to be recreated?
       (and old-buffer (> value (xlib:drawable-height old-buffer)))))
    (:ICON-TITLE
     (xlib:set-wm-properties (g-value window :drawable) :icon-name value)
;;;     (xlib:set-standard-properties (g-value window :drawable) :icon-name value)
     nil)
    (:LEFT
     (let* ((drawable (g-value window :drawable))
	    (hints (xlib:wm-normal-hints drawable)))
       (setf (xlib:drawable-x drawable) value
	     (xlib:wm-size-hints-x hints) value
	     (xlib:wm-normal-hints drawable) hints))
     nil)
    (:PARENT
     (let ((left (g-value window :left))
	   (top (g-value window :top))
	   (drawable (g-value window :drawable)))
       (if value
	 (xlib:reparent-window drawable (g-value window :drawable) left top)
	 (xlib:reparent-window drawable (opal::display-info-root-window
					 (g-value window :display-info))
			       left top)))
     nil)
    (:POINTER-POSITION
     ;; Warps the pointer to the position expressed by the <value>
     (xlib:warp-pointer (g-value window :drawable) (car value) (cdr value))
     (xlib:display-force-output opal::*default-x-display*))
    (:REPORT-ASYNCHRONOUS-ERRORS
     (setf (xlib:display-report-asynchronous-errors
	    opal::*default-x-display*)
	   value))
    (:SAVE-UNDER
     (setf (xlib:window-save-under (g-value window :drawable)) value)
     nil)
    (:SUBWINDOW-MODE
     (let ((display-info (g-value window :display-info)))
       (setf (xlib:gcontext-subwindow-mode
	      (opal::opal-gc-gcontext
	       (opal::display-info-line-style-gc display-info)))
	     value)
       (setf (xlib:gcontext-subwindow-mode
	      (opal::opal-gc-gcontext
	       (opal::display-info-filling-style-gc display-info)))
	     value)))
    (:TITLE
;;;     (let ((drawable (g-value window :drawable)))
;;;       (setf (xlib:wm-name drawable) value)
;;;       (xlib:set-standard-properties drawable :name value))
     (xlib:set-wm-properties (g-value window :drawable) :name value)
     nil)
    (:TOP
     (let* ((drawable (g-value window :drawable))
	    (hints (xlib:wm-normal-hints drawable)))
       (setf (xlib:drawable-y drawable) value
	     (xlib:wm-size-hints-y hints) value
	     (xlib:wm-normal-hints drawable) hints))
     nil)
    (:VISIBLE
     (let* ((drawable (g-value window :drawable))
	    (vis (g-value window :visible))
	    (map-window NIL))
       (cond ((eq vis t)
	      (setf map-window t))
	     ((eq vis :iconified)
	      (xlib:iconify-window drawable opal::*default-x-screen*))
	     ((eq vis nil)
	      (xlib:withdraw-window drawable opal::*default-x-screen*)))
       ;; Does the window need to be mapped?
       map-window))
    (:WIDTH
     (let ((old-buffer (g-value window :buffer)))
       (setf (xlib:drawable-width (g-value window :drawable))
	     (max 0 value))
       (setf (opal::win-update-info-width (g-value window :win-update-info))
	     value)
       ;; Does the buffer need to be recreated?
       (and old-buffer (> value (xlib:drawable-width old-buffer)))))
    (T
     (format t "Unknown property ~S in gem:x-set-window-property.~%"
	     property))))



;;; RETURNS: T if the filling style of the given display is stippled
;;;
(defun x-stippled-p (root-window)
  (eq (xlib:gcontext-fill-style
       (opal::opal-gc-gcontext
	(opal::display-info-filling-style-gc
	 (g-value root-window :display-info))))
      :stippled))



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


(defun x-translate-code (window scan-code shiftp)
  "Translates a keyboard scan"
  (xlib:keycode->keysym
   (opal::display-info-display
    (the opal::DISPLAY-INFO (g-value window :display-info)))
   scan-code (if shiftp 1 0)))


(defun x-translate-coordinates (root-window window1 x y &optional window2)
  "RETURNS: the coordinates of point <x,y> in the <window> relative to
the <window2>, or to the screen's origin if <window2> is not specified.
Returns multiple values."
  (declare (ignore root-window))
  (let ((draw1 (when window1 (g-value window1 :drawable)))
	(draw2 (when window2 (g-value window2 :drawable))))
    (when (and draw1 (null window2))
      (setf draw2 (xlib:drawable-root draw1)))
    (when (and draw2 (null window1))
      (setq draw1 (xlib:drawable-root draw2)))
    (when (and draw1 draw2)
      (xlib:translate-coordinates draw1 x y draw2))))


(defun x-window-debug-id (window)
  "RETURNS: a human-readable representation for the 
drawable associated with the <window>."
  (xlib:window-id (g-value window :drawable)))


(defun x-window-depth (window)
  "RETURNS: the depth in bits of the drawable 
associated with the window."
  (xlib:drawable-depth (g-value window :drawable)))

(defun x-window-from-drawable (root-window x-window)
  "Given an X drawable, returns the associated Opal window.

The documented way to get this functionality is through 
opal:drawable-to-window, which only takes one parameter: 
the X drawable."
  (declare (ignore root-window))
  (getf (xlib:drawable-plist x-window) :garnet))



(defun x-window-has-grown (window width height)
  "RETURNS: true if the <window>'s old buffer was smaller
(in at least one dimension) than the new <width> and <height>."
  (let ((old-buffer (g-value window :buffer)))
    (and old-buffer
	 (or (> height (xlib:drawable-height old-buffer))
	     (> width  (xlib:drawable-width old-buffer))))))


(defun x-window-to-image (window left top width height)
  "Create an image from (a piece of) a window."
  (let ((drawable (g-value window :drawable)))
    (if drawable
      (xlib:get-image drawable :format :z-pixmap :x left :y top
		      :width width :height height))))


(defun x-write-an-image (root-window pathname image)
  (declare (ignore root-window))
  (xlib:write-bitmap-file pathname image))



;;; --------------------------------------------------



(defun attach-X-methods (x-device)

  (attach-method x-device :all-garnet-windows #'x-all-garnet-windows)
  (attach-method x-device :beep #'x-beep)
  (attach-method x-device :bit-blit #'x-bit-blit)
  (attach-method x-device :black-white-pixel #'x-black-white-pixel)
  (attach-method x-device :character-width #'x-character-width)
  (attach-method x-device :clear-area #'x-clear-area)
  (attach-method x-device :color-to-index #'x-color-to-index)
  (attach-method x-device :colormap-property #'x-colormap-property)
  (attach-method x-device :copy-to-pixmap #'x-copy-to-pixmap)
  (attach-method x-device :create-cursor #'x-create-cursor)
  (attach-method x-device :create-image #'x-create-image)
  (attach-method x-device :create-image-array #'x-create-image-array)
  (attach-method x-device :create-pixmap #'x-create-pixmap)
  (attach-method x-device :create-state-mask #'x-create-state-mask)
  (attach-method x-device :create-window #'x-create-window)
  (attach-method x-device :delete-font #'x-delete-font)
  (attach-method x-device :delete-pixmap #'x-delete-pixmap)
  (attach-method x-device :delete-window #'x-delete-window)
  (attach-method x-device :discard-mouse-moved-events
		 #'x-discard-mouse-moved-events)
  (attach-method x-device :discard-pending-events #'x-discard-pending-events)
  (attach-method x-device :draw-arc #'x-draw-arc)
  (attach-method x-device :draw-image #'x-draw-image)
  (attach-method x-device :draw-line 'x-draw-line)
  (attach-method x-device :draw-lines #'x-draw-lines)
  (attach-method x-device :draw-points #'x-draw-points)
  (attach-method x-device :draw-rectangle #'x-draw-rectangle)
  (attach-method x-device :draw-roundtangle #'x-draw-roundtangle)
  (attach-method x-device :draw-text #'x-draw-text)
  (attach-method x-device :drawable-to-window #'x-drawable-to-window)
  (attach-method x-device :event-handler #'x-event-handler)
  (attach-method x-device :flush-output #'x-flush-output)
  (attach-method x-device :font-max-min-width #'x-font-max-min-width)
  (attach-method x-device :font-name-p #'x-font-name-p)
  (attach-method x-device :font-exists-p #'x-font-exists-p)
  (attach-method x-device :font-to-internal #'x-font-to-internal)
  (attach-method x-device :get-cut-buffer #'x-get-cut-buffer)
  (attach-method x-device :device-image #'x-device-image)
  (attach-method x-device :image-bit #'x-image-bit)
  (attach-method x-device :image-from-bits #'x-image-from-bits)
  (attach-method x-device :image-hot-spot #'x-image-hot-spot)
  (attach-method x-device :image-size #'x-image-size)
  (attach-method x-device :image-to-array #'x-image-to-array)
  (attach-method x-device :initialize-device #'x-initialize-device)
  (attach-method x-device :initialize-window-borders
		 #'x-initialize-window-borders)
  (attach-method x-device :inject-event #'x-inject-event)
  (attach-method x-device :make-font-name #'x-make-font-name)
  (attach-method x-device :map-and-wait #'x-map-and-wait)
  (attach-method x-device :max-character-ascent #'x-max-character-ascent)
  (attach-method x-device :max-character-descent #'x-max-character-descent)
  (attach-method x-device :mouse-grab #'x-mouse-grab)
  (attach-method x-device :raise-or-lower #'x-raise-or-lower)
  (attach-method x-device :read-an-image #'x-read-an-image)
  (attach-method x-device :reparent #'x-reparent)
  (attach-method x-device :set-clip-mask #'x-set-clip-mask)
  (attach-method x-device :set-cut-buffer #'x-set-cut-buffer)
  (attach-method x-device :set-device-variables #'x-set-device-variables)
  (attach-method x-device :set-draw-function-alist #'x-set-draw-function-alist)
  (attach-method x-device :set-drawable-to-window #'x-set-drawable-to-window)
  (attach-method x-device :set-window-property #'x-set-window-property)
  (attach-method x-device :stippled-p #'x-stippled-p)
  (attach-method x-device :text-extents #'x-text-extents)
  (attach-method x-device :text-width #'x-text-width)
  (attach-method x-device :translate-code #'x-translate-code)
  (attach-method x-device :translate-coordinates #'x-translate-coordinates)
  (attach-method x-device :window-debug-id #'x-window-debug-id)
  (attach-method x-device :window-depth #'x-window-depth)
  (attach-method x-device :window-from-drawable #'x-window-from-drawable)
  (attach-method x-device :window-has-grown #'x-window-has-grown)
  (attach-method x-device :window-to-image #'x-window-to-image)
  (attach-method x-device :write-an-image #'x-write-an-image)

  ;; Defined in inter/x-inter.lisp
  (attach-method x-device :check-double-press 'x-check-double-press)
  (attach-method x-device :compare-and-get-possible-stop-event
                          'x-compare-and-get-possible-stop-event)
  (attach-method x-device :set-interest-in-moved 'x-set-interest-in-moved)
  (attach-method x-device :translate-mouse-character
		          'x-translate-mouse-character)
  (attach-method x-device :translate-character 'x-translate-character)
  
  ;; now make all windows inherit Gem methods from the X device.
  ;;
  (set-window-methods opal::window x-device)
  )



(defun X-TOP-LEVEL-INITIALIZE (display-name)

  ;; This schema stands for the top-level root window for the X device.
  ;; We use create-schema to prevent any :initialize method from firing.
  ;;
  (create-schema '*root-window*
    (:is-a opal::window))

  ;; This schema points to the root window, and contains the slot :methods
  ;; which names all existing Gem method.  The slot is copied into the root
  ;; nodes of the windows and fonts hierarchies.
  ;;
  (create-schema 'X-DEVICE
    (:root-window *root-window*)
    (:device-type :X))

  (attach-X-methods X-DEVICE)

  (opal::initialize-x11-values (or display-name (opal::get-full-display-name))
                               *root-window*)
  (s-value opal::DEVICE-INFO :current-root *root-window*)
  (s-value opal::DEVICE-INFO :current-device X-DEVICE)
  (pushnew X-DEVICE (g-value opal::DEVICE-INFO :active-devices))

  (let ((display-info (x-initialize-device NIL)))
    (s-value *root-window* :drawable
	     (opal::display-info-root-window display-info))
    (s-value *root-window* :display-info display-info))

  (opal::set-draw-functions)
  (opal::initialize-halftones)

  *root-window*)


;;; Make the initializer function available to the outside world.
;;;
(push (cons :X #'X-TOP-LEVEL-INITIALIZE) *device-initializers*)
