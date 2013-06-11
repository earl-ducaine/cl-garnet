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
;;; XXXXXXX This code is not used in the current version.
;;;
;;; CHANGE LOG:
;;; I started this project  without any knowledge of garnet internals,
;;; no knowledge of gdi, and minimal knowledge of lisp.
;;; I expect that in most cases there are better ways of coding this.
;;; It is mostly a prototype, so, debug ONLY code ;)
;;;
;;; This is the device handler for the M$ window system.  It implements the
;;; Gem methods.  Initially, I am going to call the gdi functions directly.
;;; This may be a huge mistake.  I created this file by merging x.lisp and
;;; mac.lisp using emerge.  Garnet assumes that dimensions are in pixels,
;;; so I may have to set this.  I am going to store the background brush in
;;; The following may not be true.  I wrote this before I started
;;; coding this, and I dont think this was the way I did this.
;;; the kr object system in the :background slot.  I am assuming that the buffer
;;; slot is a drawable.  In gdi-copy-to-pixmap, from can be either a cursor or
;;; bitmap.  This information must be stored, so that copy image will know 
;;; what kind of image it is.  It may be better to only use bitmaps, because
;;; I think there is a function to create a cursor from a bitmap.  I am using
;;; bitblt to copy bitmaps, and drawiconex to for the cursor or icon.
;;;  Dan Stanger
 
(in-package "GEM")

(defvar *garnet-window-class* NIL)
(defvar *gdi-font-faces* '(:roman :bold :italic :bold-italic
                           :plain :condense :extend :outline
                           :shadow :underline))

(defvar *gdi-display-depth* 24) ; fixme This should be set by a gdi call
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GDI Drawable prototype and interface functions
;;; Following the article at motifzone.com,
;;; a device context corresponds to a drawable, and the triple of
;;; pen, brush, and font corresponds to a graphics context.
(defstruct drawable dc handle plist (depth *gdi-display-depth*))
(defstruct (gwindow (:include drawable))) 
(defstruct (bitmap (:include drawable)))

(defstruct (icon (:include drawable)))
(defstruct gdi-dc pen brush font)

(defun window-id (drawable)
   (drawable-dc drawable))
;;; NIY? -- Need to find unique identifier for a mac drawable, like
;;; in xlib:window-id.
(defun connected-gdi-drawable-p (event-window)
  (let ((opal-window (GDI-window-from-drawable NIL event-window)))
    (if opal-window
        (let ((drawable (g-value opal-window :drawable)))
          (and drawable (eq event-window drawable))))))

;;; The following two variables used to be in Inter/i-windows.lisp ; they
;;; have been moved here because nobody seems to be using them.
;;;
(defvar *mouse-debug* nil
  "When true, *mouse-throw-aways* will increment each time a mouse-moved
   event is thrown away")

(defvar *mouse-throw-aways* 0)

;;; Debugging only
;;;
(defparameter *debug-on* NIL)

(defmacro debug-print (&rest arguments)
  #-DEBUG
  (declare (ignore arguments))
  #-DEBUG
  NIL
  #+DEBUG
  `(if *debug-on*
    (format t "~%~A ~{ ~S~}" (car ,arguments) (cdr ,arguments))))

(defun color-device-attached? () T)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Styles Functions (from opal: new-defs.lisp)
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; It is possible to have a background brush for a class,
;;; but once it is set, I dont think it can be changed.
;;; And I dont think stipple is needed for garnet.
(defun get-gdi-stipple (style-schema root-window) nil)

;;; With-styles works like xlib:with-gcontext except it takes a gob and
;;; extracts all the relevant things for you. This is a win for the simple
;;; draw methods, it will be a lose for performance. See below.
;;;
;;; This is a quick hack to get around the caching of various gcontext
;;; values, it will work until we understand how CLX and the RT X11 server
;;; cache gcontexts better.
#+clx
(defmacro set-gc (opal-gcontext xlib-gcontext slot value)
  (case slot
    (:foreground
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-foreground ,opal-gcontext))
	 (setf (opal::opal-gc-foreground ,opal-gcontext)
	       (setf (gdi::gcontext-foreground ,xlib-gcontext) v)))))
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
	       (if v;; do not set to NIL
		 (setf (xlib:gcontext-dashes ,xlib-gcontext) v))))))
    (:font
     `(let ((v ,value))
       (unless (eq v (opal::opal-gc-font ,opal-gcontext))
	 (setf (opal::opal-gc-font ,opal-gcontext)
	       (if v;; do not set to NIL
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
	       (if v;; do not set to NIL
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



(defun set-line-style (line-style opal-gc xlib-gc root-window gdi-draw-fn)
  (declare (optimize (speed 3) (safety 0)))
  (when line-style
    (let ((draw-fn-changed? (set-gc opal-gc xlib-gc
					  :function gdi-draw-fn)))
      (unless (eq gdi-draw-fn opal::boole-2)
	(let ((gdi-stipple (get-gdi-stipple line-style root-window))
	      gdi-dash-pattern)

	  ;; If the draw-function is :xor and *black* = 0 (for instance
	  ;; on HP machines), then we must draw black as white and white
	  ;; as black.  But we must check the draw-function first.
	  ;; Set-gc returns non-NIL if draw-function changed.
	  (when (or draw-fn-changed?
		    (not (eq line-style (opal::opal-gc-opal-style opal-gc))))
	    (set-gc opal-gc xlib-gc :foreground
		    (opal::HP-XOR-hack
		     gdi-draw-fn
		     (g-value line-style :foreground-color :colormap-index)))
	    (set-gc opal-gc xlib-gc :background
		    (opal::HP-XOR-hack
		     gdi-draw-fn
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
	    (if (setq gdi-dash-pattern (g-value line-style :dash-pattern))
	      (set-gc opal-gc xlib-gc :dashes gdi-dash-pattern)))

	  ;; This can't be in the "unless" since the same
	  ;; line-style can have different gdi-stipples
	  (if gdi-stipple
	    (progn
	      (set-gc opal-gc xlib-gc :fill-style :opaque-stippled)
	      (set-gc opal-gc xlib-gc :stipple gdi-stipple))
	    (set-gc opal-gc xlib-gc :fill-style :solid)))))))



(defun set-filling-style (filling-style opal-gc xlib-gc root-window gdi-draw-fn)
  (declare (optimize (speed 3) (safety 0)))
  (when filling-style
    (unless (eq gdi-draw-fn boole-2)
      (let ((gdi-stipple (get-gdi-stipple filling-style root-window)))
	;; Set-gc returns non-NIL if draw-function changed.
	(when (or (set-gc opal-gc xlib-gc :function gdi-draw-fn)
		  (not (eq filling-style (opal::opal-gc-opal-style opal-gc))))
	  (set-gc opal-gc xlib-gc :foreground
		  (opal::HP-XOR-hack
		   gdi-draw-fn
		   (g-value filling-style :foreground-color :colormap-index)))
	  (set-gc opal-gc xlib-gc :background
		  (opal::HP-XOR-hack
		   gdi-draw-fn
		   (g-value filling-style :background-color :colormap-index))))

	(unless (eq filling-style (opal::opal-gc-opal-style opal-gc))
	  (setf (opal::opal-gc-opal-style opal-gc) filling-style)
	  (set-gc opal-gc xlib-gc :fill-style
		  (g-value filling-style :fill-style))
	  (set-gc opal-gc xlib-gc :fill-rule
		  (g-value filling-style :fill-rule)))
	(if gdi-stipple (set-gc opal-gc xlib-gc :stipple gdi-stipple))))
    (set-gc opal-gc xlib-gc :function gdi-draw-fn)))




;; Do-All-Progeny is used to iterate over all GDI windows.  Clean-Up calls
;; this function with the root GDI window.
;; for now, comment this out.
#+fixme
(defun do-all-garnet-windows (clx-window)
  (let ((windows (if (member :garnet (drawable-plist clx-window))
		   (list clx-window))))
    (dolist (w (gdi:query-tree clx-window))
      (setf windows (append windows (do-all-garnet-windows w))))
    windows))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   The STATE parameter
;;;
;;;    The modifier keys for the current keystroke or mouse-click are passed
;;; around in the STATE parameter.  This is a set of four bits that correspond
;;; to the shift, caps-lock, control, and option keys.  This parameter is
;;; generally set by the top-level event-handler that corresponds to a
;;; key or mouse event, and is passed to the support functions like
;;; GDI-translate-character.  The single byte STATE spec can be used as an
;;; index into the *prefixes* array, and is used analogously to index into the
;;; *mouse-down-translations* array (see garnet-keytrans.lisp and
;;; define-mouse-keys.lisp).
;;;
;;; These particular positions for each bit correspond to the state information
;;; generated by X windows.  The values were carried into the Mac version so
;;; that existing Interactors code could be shared by both implementations.
;;; 

;; Modifiers
(defconstant +shift-bit+     1)
(defconstant +caps-lock-bit+ 2)
(defconstant +control-bit+   4)
(defconstant +option-bit+    8)  ;; the meta key
#+fixme
(defun get-event-bits ()
  (+ (if (gdi:option-key-p)    *option-bit* 0)
     (if (gdi:control-key-p)   *control-bit* 0)
     (if (gdi:caps-lock-key-p) *caps-lock-bit* 0)
     (if (gdi:shift-key-p)     *shift-bit* 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENT HANDLING


;;; NIY:
;; The function gdi:find-view-containing-point does not always return the value
;; we expect.  If there are two overlapping subviews, it will return the deepest
;; one in the window hierarchy, even if the other one is obscuring it.  Consider
;; Agate, where you have both a scrolling-window and an error-gadget as
;; subwindows of the main window.  The error-gadget covers the scrolling-window,
;; but gdi:find-view-containing-point will return the scrolling-window's
;; inner-window when you try to click on the error-gadget button.
;;
;; Caveat to previous paragraph: After writing that comment, I finished
;; implementing show-drawable and hide-drawable so that subviews are removed
;; from their window hierarchy when they become invisible.  By reordering the
;; subviews of the parent view, I coincidentally "fixed" the error-gadget
;; problem in Agate.  Now the expected subview is being identified as the one
;; in which the event happened, but this is only by chance.  The MCL function
;; is still returning just the first subview that the event occurs in, rather
;; than the topmost subview.
;;
#+fixme
(defun find-overlapping-view-containing-point (event-window where)
  (do* ((subviews (gdi:subviews event-window) (cdr subviews))
        (subview (car subviews) (car subviews))
        (found? (if subview (gdi:view-contains-point-p subview where))
                (if subview (gdi:view-contains-point-p subview where)))
        (found-list (if found? (list subview))
                    (if found? (cons subview found-list))))
       ((null subview) found-list)
    ))
        
#+fixme
(defmethod gdi:view-key-event-handler ((event-window gem::GDI-DRAWABLE) char)
  (declare (ignore char))
  (let* ((time (gdi:rref gdi:*current-event* :EventRecord.when))
         (message (gdi:rref gdi:*current-event* :EventRecord.message))
         ;; The "where" is in screen coordinates
         (where (gdi::%global-to-local
                 (gdi:wptr event-window)
                 (gdi:rref gdi:*current-event* :EventRecord.where)))
         ;; CODE contains the "key code" in the high-order word, and the
         ;; "character code" in the low-order word
         (code (logand message #xFFFF))
         (state (get-event-bits)))
    ;; The key-event-handler is called on the "active (frontmost) window", as
    ;; stated on p. 376 of the MCL manual.  This means we have to determine
    ;; ourselves which subview the keystroke should actually go to -- argh!
    (let ((subwindow (gdi:find-view-containing-point event-window where)))
      (unless (eq event-window subwindow)
        (setf where (gdi::convert-coordinates where event-window subwindow))
        (setf event-window subwindow)))
    (let ((x (gdi:point-h where))
          (y (gdi:point-v where))
          (win (GDI-window-from-drawable *root-window* event-window)))
      ;(format t "DOWN code = ~S~%" code)
      (inter::Do-Key-Press win x y state code time)))
  #+comment
  (call-next-method))


;; Implemented in conjunction with GDI-translate-character.  The key-up-event-
;; handler perceives simulated mouse-up events generated by keyboard keys
;; (esp. middledown and rightdown).  GDI-translate-character perceives the
;; simulated mouse-down events.
;;
#+fixme
(defmethod gdi:window-key-up-event-handler ((event-window gem::GDI-DRAWABLE))
  (let* ((message (logand (gdi:rref gdi:*current-event* :EventRecord.message)
                          #xFFFF))
         (key-code (ash message -8))             ;; high-order word
         (code (key-code-to-button-code key-code)))
    (when code
      (let* ((time (gdi:rref gdi:*current-event* :EventRecord.when))
             ;; The "where" is in screen coordinates
             (where (gdi::%global-to-local
                     (gdi:wptr event-window)
                     (gdi:rref gdi:*current-event* :EventRecord.where)))
             (state (get-event-bits))
             (event-key :BUTTON-RELEASE))
    ;; The key-event-handler is called on the "active (frontmost) window", as
    ;; stated on p. 376 of the MCL manual.  This means we have to determine
    ;; ourselves which subview the keystroke should actually go to -- argh!
        (let ((subwindow (gdi:find-view-containing-point event-window where)))
          (unless (eq event-window subwindow)
            (setf where (gdi::convert-coordinates where event-window subwindow))
            (setf event-window subwindow)))
        (let ((x (gdi:point-h where))
              (y (gdi:point-v where))
              (win (GDI-window-from-drawable *root-window* event-window)))
          ;(format t "UP = ~S~%" code)
          (inter::Do-Button-Release win x y state code time event-key)))))
  #+comment
  (call-next-method))

#+fixme
(defmethod gdi:view-click-event-handler ((event-window gem::GDI-DRAWABLE) where)
  (let ((state (get-event-bits))
        (time (gdi:rref gdi:*current-event* :EventRecord.when))
        (code inter::*left-button*)
        (event-key :BUTTON-PRESS))
    ;(format t "code = ~S~%" code)
    ;; If we adhered to the object-oriented event-handler paradigm, we would
    ;; allow MCL to call the view-click-event-handler on each of the window's
    ;; subviews, giving each one a chance to handle the event.  However, the
    ;; implementation here bypasses the intervening views (including the top
    ;; window, if appropriate) to only allow the deepest view to handle the
    ;; event.  This is consistent with the X windows model.

    (let ((subwindow (gdi:find-view-containing-point event-window where)))
      (unless (eq event-window subwindow)
        (setf where (gdi::convert-coordinates where event-window subwindow))
        (setf event-window subwindow)))

    ;; Do bookkeeping required for mouse-move events that might be ahead...
    (process-activate-event event-window)
    (let ((x (gdi:point-h where))
          (y (gdi:point-v where))
          (win (GDI-window-from-drawable *root-window* event-window)))
      (inter::Do-Button-Press win x y state code time event-key)))
  (call-next-method))

#+fixme
(defmethod gdi:window-mouse-up-event-handler ((event-window gem::GDI-DRAWABLE))
  (let* ((state (get-event-bits))
         (time (gdi:rref gdi:*current-event* :EventRecord.when))
         (code inter::*left-button*)
         (event-key :BUTTON-RELEASE)
         (where (gdi::%global-to-local
                 (gdi:wptr event-window)
                 (gdi:rref gdi:*current-event* :EventRecord.where))))
    ;; See comments for gdi:view-click-event-handler
    (let ((subwindow (gdi:find-view-containing-point event-window where)))
      (unless (eq event-window subwindow)
        (setf where (gdi::convert-coordinates where event-window subwindow))
        (setf event-window subwindow)))
    (let ((x (gdi:point-h where))
          (y (gdi:point-v where))
          (win (GDI-window-from-drawable *root-window* event-window)))
      (inter::Do-Button-Release win x y state code time event-key)))
  (call-next-method))
(print "444")
#+fixme
(defmethod gdi:view-draw-contents ((event-window garnet-view-mixin))
  (gdi:with-back-color (background-color event-window)
    (let* ((opal-window (GDI-window-from-drawable *root-window* event-window))
           (view-size-point (gdi:view-size event-window))
           (left 0)
           (top 0)
           (right (gdi:point-h view-size-point))
           (bottom (gdi:point-v view-size-point)))

      ;; Draw the background color of the window.  Without this call, the
      ;; background color would still be drawn successfully by GDI-Clear-Area,
      ;; but only if the window has an aggregate.  The update method does not
      ;; focus on the view to draw anything unless there is an aggregate.
      (SetRectRgn *spare-region-1* 0 0 right bottom)
      (unless (g-value opal-window :omit-title-bar-p)
        (diff-with-grow-rgn *spare-region-1* event-window
                            (gdi:window-object (gdi:wptr event-window))))
      (EraseRgn *spare-region-1*)

      ;; Redraw the objects in the window
      (if (connected-gdi-drawable-p event-window)
          (inter::do-exposure opal-window left top right bottom
                              0 NIL)))) ; Count and Display are unused by Mac
  (call-next-method))

#+fixme
(defmethod (setf background-color) :after (color (view garnet-view-mixin))
  (declare (ignore color))
  (gdi:invalidate-view view))


#+fixme
(defmethod gdi:window-grow-event-handler :after
             ((event-window gem::GDI-DRAWABLE) where)
  (declare (ignore where))
  (when (gem::connected-gdi-drawable-p event-window)
    (let* ((opal-window (gem::GDI-window-from-drawable
                         NIL event-window))
           (view-pos-point (gdi:view-position event-window))
           (view-size-point (gdi:view-size event-window))
           (x (gdi:point-h view-pos-point))
           (y (gdi:point-v view-pos-point))
           (width (gdi:point-h view-size-point))
           (height (gdi:point-v view-size-point))
           (above-sibling NIL))
      (inter::do-configure-notify opal-window x y width height
                                  above-sibling))))
(print "493")
#+fixme
(defmethod gdi:window-drag-event-handler :after
             ((event-window gem::GDI-DRAWABLE) where)
  (declare (ignore where))
  (when (gem::connected-gdi-drawable-p event-window)
    (let* ((opal-window (gem::GDI-window-from-drawable
                         NIL event-window))
           (view-pos-point (gdi:view-position event-window))
           (view-size-point (gdi:view-size event-window))
           (x (gdi:point-h view-pos-point))
           (y (gdi:point-v view-pos-point))
           (width (gdi:point-h view-size-point))
           (height (gdi:point-v view-size-point))
           (above-sibling NIL))
      (inter::do-configure-notify opal-window x y width height
                                  above-sibling))))
  
;;; END Event Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; RETURNS:
;;; a list of all the X windows that were created by Garnet.  These are
;;; raw windows, NOT Opal windows!
;;;
(defun gdi-all-garnet-windows (root-window)
  (declare (ignore root-window))
  (do-all-garnet-windows opal::*default-gdi-root*))
(print 522)
;; From Inside Macintosh V: "During a CopyBits call, the foreground and back-
;; ground colors are applied to the image.  To avoid unwanted coloring of the
;; image, set the foreground to black and the background to white before
;; calling this routine." -- argh, ARGH!
#+fixme
(defmacro with-copybits-colors ((the-gworld) &body body)
  (let ((old-fore (gensym))
        (old-back (gensym))
        (old-rgb-fore (gensym))
        (old-rgb-back (gensym))
        (port (gensym)))
    `(let* ((,port ,the-gworld)
            (,old-fore (gdi:rref ,port windowrecord.fgcolor))
            (,old-back (gdi:rref ,port windowrecord.bkcolor))
            (,old-rgb-fore (gdi:rref ,port cgrafport.rgbfgcolor))
            (,old-rgb-back (gdi:rref ,port cgrafport.rgbbkcolor)))
       (rgbforecolor gdi:*black-rgb*)
       (rgbbackcolor gdi:*white-rgb*)
       ,@body
       (gdi:rset ,port windowrecord.fgcolor ,old-fore)
       (gdi:rset ,port windowrecord.bkcolor ,old-back)
       (gdi:rset ,port cgrafport.rgbfgcolor ,old-rgb-fore)
       (gdi:rset ,port cgrafport.rgbbkcolor ,old-rgb-back))))

;;; I hate beeps
(defun gdi-beep (root-window)
  (declare (ignore root-window))
  nil)

;;; This assumes that the <window> has a gcontext.
;;;
(defun gdi-bit-blit (window source s-x s-y width height destination d-x d-y)
  ; may have to modify the pen,brush,font here
  ;(g-value window :buffer-gcontext)
  (gdi::bitblt destination d-x d-y width height source SRCCOPY))

;;; Returns: the black and white pixel for the screen of the <window>, as
;;; multiple values.
;;;

(defun gdi-black-white-pixel (window)
  (declare (ignore window))
  (values gdi:*black* gdi:*white*))

(defun gdi-character-width (root-window opal-font the-char-code)
  (declare (ignore root-window))
  (gdi::char-width (g-value opal-font :xfont) the-char-code))

;;; Clears the visible area associated with a window.  If <clear-buffer-p>,
;;; operate on the window's buffer instead.
;;;
(defun gdi-clear-area (window &optional (x 0) (y 0) width height clear-buffer-p)
    ;; Get info for clearing
    (let* ((gc (g-value window :buffer-gcontext))
	   (buffer (if clear-buffer-p (g-value window :buffer) nil))
	   (background (g-value window :background))
           (oldROP (gdi::setrop2 window gdi:R2_COPYPEN))
           (oldBackground (multiple-value-bind (retval h)
              (gdi::selectobject window background)
              (if retval h (error "getdc ~S" h))))
           (rect (make-rect)))
      (multiple-value-bind (retval e) (gdi::getclientrect window rect)
         (unless retval (error "getclientrect ~S" e)))
      (when x (setf (rect-left rect) x (rect-top rect) y
               (rect-right rect) (+ x width 1) (rect-bottom rect) (+ y height 1))
      ;; clear the buffer if flagged
      (when buffer (gdi::fillrect buffer rect background))
      ;; Clear the window itself
      (gdi::fillrect buffer rect background))
      (gdi::selectobject window oldBackground)
      (gdi::setrop2 window oldROP)))

;; Nomenclature:  There is some inconsistency with the word "pixmaps" in Gem.
;; All of the functions concerned with pixmaps have "-pixmap-" in the function
;; name, when appropriate.  However, the object that is passed around is a
;; Mac GWORLD object, which has a gworld pointer in a slot, and from this gworld
;; pointer we can get the underlying pixmap.
;;
;; source -- a gworld CLOS object, the :buffer of the window
;; destination -- a view
;;
;; Note: On the Mac, we do not really have indices; we have six-digit
;; hexadecimal RGB values.  These values are stored in the :xcolor slot
;; of each Opal color object (the :colormap-index slot is unused).
;; On M$ Windows, use the :xcolor slot as on the Mac
(defun gdi-color-to-index (root-window a-color)
  (declare (ignore root-window))
  (if (g-value opal::color :color-p)
      (if a-color (g-value a-color :xcolor) gdi:*white*)
      (if (eq a-color opal::black)
          gdi:*black*
          gdi:*white*)))

;;; RETURNS: various things, depending on which <property> is requested:
;;; :COLOR-LOOKUP -- looks up <a> (a color name) and returns
;;;   three values, the R-G-B values in the color lookup table.
;;; :MAKE-COLOR   -- creates and returns a color whose three RGB components
;;;   are given by <a, b, c>
;;;
(defun gdi-colormap-property (root-window property &optional a b c) nil)
#+clx
(defun gdi-colormap-property (root-window property &optional a b c)
  (declare (ignore root-window))
  (case property
    (:ALLOC-COLOR
     (xlib:alloc-color opal::*default-gdi-colormap* a))
    (:ALLOC-COLOR-CELLS
     (xlib:alloc-color-cells opal::*default-gdi-colormap* 1))
    (:FIRST-ALLOCATABLE-INDEX
     (let* ((indices (xlib:alloc-color-cells opal::*default-gdi-colormap* 1))
	    (index (car indices)))
       (xlib:free-colors opal::*default-gdi-colormap* indices)
       index))
    (:FREE-COLORS
     (xlib:free-colors opal::*default-gdi-colormap* a))
    (:LOOKUP-COLOR
     (xlib:lookup-color opal::*default-gdi-colormap* a))
    (:LOOKUP-RGB
     (let* ((xcolor (xlib:lookup-color opal::*default-gdi-colormap* a)))
       ;; The PS module needs the RGB values
       (values (xlib:color-red xcolor)
	       (xlib:color-green xcolor)
	       (xlib:color-blue xcolor))))
    (:MAKE-COLOR
     (xlib:make-color :red a :green b :blue c))
    (:QUERY-COLORS
     ;; Returns three values: red, green, blue components
     (let ((color (car (xlib:query-colors opal::*default-gdi-colormap* (list a)))))
       (values (floor (* 65535 (xlib:color-red color)))
	       (floor (* 65535 (xlib:color-green color)))
	       (floor (* 65535 (xlib:color-blue color))))))
    (t
     (error "Unknown property ~S in gem::gdi-colormap-property~%"
	    property))))

;;; Copy the cursor or bitmap in <from> to the pixmap <to>.  The operation
;;; affects an area of <width> by <height>.
;;;
(trace gdi::bitblt)
(defun gdi-copy-to-pixmap (root-window to from width height)
  (if (bitmap-p from)
      (multiple-value-bind (retval code)
         (gdi::bitblt (drawable-handle to) 0 0 width height
             (drawable-handle from) 0 0 gdi::SRCCOPY)
         (unless retval (cerror "bitblt ~S" "~S" code)))
      (gdi::drawiconex to 0 0 from width height 0 gdi::null gdi::gi_normal)))

;;; If <from-font-p> is true, the <source> is a font; otherwise, it is
;;; a pixmap.  Same for the <mask>.  <x> and <y> are a position when the
;;; source is a pixmap; otherwise, they are the cursor-char and the mask-char
;;; for the two fonts.
;;;
(defun gdi-create-cursor (root-window source mask foreground background
				    from-font-p x y)
  (declare (ignore root-window))
  (if from-font-p
    (gdi::create-glyph-cursor :source-font source :mask-font mask
			      :source-char x
			      :mask-char y
			      :foreground foreground
			      :background background)
    (multiple-value-bind (retval h)
       (gdi::createiconindirect (gdi:make-iconinfo nil (/ x 2) (/ y 2)
           source mask))
       (if retval h (cerror "error" "createiconindirect ~S" h)))))

;;;
;;; The following deals with cases where the display provides pixmaps
;;; with depths different from bits-per-pixel.
;;;
(defun get-pixmap-formats ()
  "Return valid pixmap formats for this display."
  (gdi::display-pixmap-formats opal::*default-gdi-display*))

(defun depth-pixmap-format (depth)
  "Return a list of all pixmap formats with a given depth supported by
this display."
  (remove-if-not
   #'(lambda (format)
       (= (gdi::pixmap-format-depth format) depth))
   (get-pixmap-formats)))

(defun depth-to-bits-per-pixel (depth)
  "Return a bits-per-pixel value valid for a given depth.  Prefer
depth = bits-per-pixel if possible.  Otherwise just use the first
pixmap format in the list of valid formats."
  (let ((valid-formats (depth-pixmap-format depth)))
    (dolist (format valid-formats (gdi::pixmap-format-bits-per-pixel (car valid-formats)))
      (when (= (gdi::pixmap-format-bits-per-pixel format) depth)
	(return-from depth-to-bits-per-pixel depth)))))



(print "720")
;;; <color-or-data> is used as a color (if <from-data-p> is nil) or as
;;; actual data
;;;
(defun gdi-create-image (root-window width height depth from-data-p
				   &optional color-or-data properties
				   bits-per-pixel left-pad data-array)
  (declare (ignore root-window data-array))
  (unless bits-per-pixel
    (setf bits-per-pixel (depth-to-bits-per-pixel depth)))
  (if from-data-p
    (let* ((bits-per-line (gdi::index* width bits-per-pixel))
	   (padded-bits-per-line
	    (gdi::index* (gdi::index-ceiling bits-per-line 32) 32))
	   (padded-bytes-per-line
	    (gdi::index-ceiling padded-bits-per-line 8)))
      (gdi::createbitmap width height depth bits-per-pixel color-or-data))
    (gdi::createbitmap width height depth depth
           (make-array (list height width)
		       :element-type
		       (case depth
			 (1  'gdi::pixarray-1-element-type)
			 (4  'gdi::pixarray-4-element-type)
			 (8  'gdi::pixarray-8-element-type)
			 (16 'gdi::pixarray-16-element-type)
			 (24 'gdi::pixarray-24-element-type)
			 (32 'gdi::pixarray-32-element-type)
			 (t
			  (cerror
			   "Ignore"
			   "gem::gdi-create-image: depth ~S is not valid (1, 8, 16, 24 or 32)"
			   depth)
			  'gdi::pixarray-8-element-type))
		       :initial-element
		       (if color-or-data
			 (g-value color-or-data :colormap-index)
			 opal::*white*)))))
  


;;; Create an array that's suitable for an X image.  <depth> should be
;;; 1 for a bitmap and the depth of the display for a pixmap
;;;
(defun gdi-create-image-array (root-window width height depth)
  (declare (ignore root-window))
  (make-array (list height width)
	      :element-type
	      (case depth
		(1  'gdi::pixarray-1-element-type)
		(4  'gdi::pixarray-4-element-type)
		(8  'gdi::pixarray-8-element-type)
		(16 'gdi::pixarray-16-element-type)
		(24 'gdi::pixarray-24-element-type)
		(32 'gdi::pixarray-32-element-type)
		(t
		 (cerror
		  "Ignore"
		  "gem::gdi-create-image-array: depth ~S is not valid (1, 8, 16, 24 or 32)"
		  depth)
		 'gdi::pixarray-8-element-type))))


;;; Creates a state mask for keyboard events.
;;;
(defun gdi-create-state-mask (root-window modifier)
  (declare (ignore root-window))
  (gdi::make-state-mask modifier))



(defun gdi-create-pixmap (window width height depth
			&optional image bitmap-p data-array)
  (declare (ignore data-array))
  (let* ((drawable (g-value window :drawable))
         (hdc (multiple-value-bind (retval h) (gdi::getdc nil)
               (if retval h (error "getdc ~S" h))))
         (pixmap (multiple-value-bind (retval h)
                  (gdi::createcompatiblebitmap hdc width height)
               (if retval h (error "createcompatiblebitmap ~S" h))))
         (bitmap-struct (make-bitmap :depth depth)))
    (if image
      (let ((gc (gdi::create-gcontext :drawable pixmap :function boole-1
		  ;;; Since this pixmap is going to be used as a stipple mask,
                  ;;; you must have 1's in foreground, regardless of whether
                  ;;; *black* is 1 on this machine or not (on HP's, it's 0).
				      :foreground 1  ; NOT opal::*black*
				      :background 0  ; NOT opal::*white*
				      )))
	(gdi::put-image pixmap gc image
			:x 0 :y 0 :width width :height height
			:bitmap-p bitmap-p)
	(gdi::free-gcontext gc)
	(gdi::set-standard-properties drawable :icon-pixmap pixmap)))
    (setf (bitmap-handle bitmap-struct) pixmap)
    bitmap-struct))



;;; RETURNS:
;;; the newly-created drawable.
;;;
(defun gdi-create-window (parent-window x y width height
			title icon-name
			background border-width
			save-under visible
			min-width min-height max-width max-height
			user-specified-position-p user-specified-size-p
			override-redirect)
  (let* ((display-info (g-value parent-window :display-info))
         (drawable (multiple-value-bind (retval h)
                  (gdi::createwindowindirecta
                    (gdi::make-createstruct 0 *garnet-window-class* title
                        gdi:WS_POPUP x y width height
                        NIL NIL gdi::*hinstance* NIL))
                  (if retval h (error "createwindowindirect ~S" h))))
         (window-struct (make-gwindow)))
     (setf (gwindow-handle window-struct) drawable)
;    (push :pixmap (slot-value new-image 'plist))
;    (push :garnet (slot-value new-image 'plist))
    window-struct))

(defun GDI-create-state-mask (root-window modifier)
  (declare (ignore root-window))
  (case modifier
    (:shift                   *shift-bit*)
    (:lock                    *caps-lock-bit*)
    ((:control :command)      *control-bit*)
    ((:mod-1 :meta :option)   *option-bit*)))

(defun gdi-delete-font (root-window font)
  (declare (ignore root-window))
  (gdi::deleteobject (kr:g-cached-value font :xfont)))


(defun gdi-delete-pixmap (window pixmap &optional buffer-too)
  (gdi::deleteobject (bitmap-handle pixmap))
  (if buffer-too
    (gdi::free-gcontext (g-value window :buffer-gcontext))))


;;; Destroys the <gdi-window>, a raw window (NOT an Opal window!)
;;;
(defun gdi-delete-window (root-window gdi-window)
  (declare (ignore root-window))
  (setf (getf (drawable-plist gdi-window) :garnet) NIL)
    (gdi::destroywindow gdi-window))

;;; RETURNS: multiple values:
;;; - x of the last mouse event that was discarded;
;;; - y of the last mouse event;
;;; - Opal window in which the last event happened.
;;;
(defun gdi-discard-mouse-moved-events (root-window) nil)

#-(and cmu mp)
(defun gdi-discard-pending-events (root-window &optional (timeout 1))
  (declare (ignore root-window))
  (gdi::event-case (opal::*default-gdi-display* :discard-p t :timeout timeout)
		   (:destroy-notify () NIL) ; get rid of warnings
		   (otherwise () t)))

;;; This function could be made more efficient by compressing the
;;; arithmetic.  For now, leave it expanded for clarity.
(defun bit-vector-to-int (b)
  (let ((b0 (eql 1 (aref b 0)))
        (b1 (eql 1 (aref b 1)))
        (b2 (eql 1 (aref b 2)))
        (b3 (eql 1 (aref b 3))))
    (+ (if b0 128 0)
       (if b1  64 0)
       (if b2  32 0)
       (if b3  16 0)
       (if b0   8 0)
       (if b1   4 0)
       (if b2   2 0)
       (if b3   1 0))))

#+(and cmu mp)
(defun gdi-discard-pending-events (root-window &optional (timeout 1))
  (declare (ignore root-window timeout))
  (ext:flush-display-events opal::*default-gdi-display*))

(defun fill-to-pattern (fill)
  (if fill
      (let ((stipple (g-value fill :stipple)))
        (if stipple
            (g-value stipple :image)
            gdi:*black*))))

(defun linestyle-to-pattern (linestyle)
  (if linestyle
      (let ((stipple (g-value linestyle :stipple)))
        (if stipple
            (g-value stipple :image)
            ;; Fudge for dashed/dotted lines
            (if (eq :solid (g-value linestyle :line-style))
                gdi:*black*
                gdi::*gray*)))))

(defun style-to-xcolors (style)
  (if style
      (let ((fcolor (g-value style :foreground-color :xcolor))
            (bcolor (g-value style :background-color :xcolor)))
        (values fcolor bcolor))))

#+fixme
(defmacro with-gdi-colors (style &body body)
  `(multiple-value-bind (fcolor bcolor)
       (style-to-xcolors ,style)
     (when fcolor
       (gdi:with-fore-color fcolor
         (gdi:with-back-color bcolor
           ,@body)))))
(print "1002")
(defun gdi-draw-arc (window x y width height angle1 angle2 function
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
    (setf function (get function :gdi-draw-function))
    (if fill-style
      (let ((filling-style-gc (opal::display-info-line-style-gc display-info)))
	(set-filling-style
	 fill-style
	 filling-style-gc
	 (opal::opal-gc-gcontext filling-style-gc) root-window function)
	(gdi::draw-arc drawable (opal::opal-gc-gcontext filling-style-gc)
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
	(gdi::draw-arc
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



(defun gdi-draw-image (window left top width height image function fill-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window))
	 (bitmap-p (= (gdi::image-depth image) 1)))

    (setf function (get function :gdi-draw-function))
    (if fill-style
      (let* ((fill-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-fill (opal::opal-gc-gcontext fill-style-gc)))
	(set-filling-style fill-style fill-style-gc xlib-gc-fill
			   root-window function)
	(if (and (eq (gdi::gcontext-fill-style xlib-gc-fill) :stippled)
		 bitmap-p)
	  (let ((save-stipple (gdi::gcontext-stipple xlib-gc-fill)))
	    (setf (gdi::gcontext-stipple xlib-gc-fill)
		  (opal::build-pixmap window image width height bitmap-p))
	    (setf (gdi::gcontext-ts-x xlib-gc-fill) left)
	    (setf (gdi::gcontext-ts-y xlib-gc-fill) top)
	    (gdi::rectangle drawable
				 left top width height )
	    (if save-stipple
	      (setf (gdi::gcontext-stipple xlib-gc-fill) save-stipple)))
	  (gdi::put-image drawable xlib-gc-fill image
			  :x left
			  :y top
			  :width width
			  :height height
			  :bitmap-p bitmap-p))))))


(defun gdi-draw-line (window x1 y1 x2 y2 function line-style &optional drawable)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info)))
    ;; Provide the actual drawable of the window if you want to bypass drawing
    ;; into the buffer.  This is used by the gesture-interactor to draw lines
    ;; directly into the window, not the buffer.
    (unless drawable
      (setf drawable (the-drawable window)))
    (setf function (get function :gdi-draw-function))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(gdi::draw-line drawable xlib-gc-line x1 y1 x2 y2)))))



(defun gdi-draw-lines (window point-list function line-style fill-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (setf function (get function :gdi-draw-function))
    (if fill-style
      (let* ((filling-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-filling (opal::opal-gc-gcontext filling-style-gc)))
	(set-filling-style
	 fill-style filling-style-gc xlib-gc-filling root-window function)
	(gdi::draw-lines drawable xlib-gc-filling point-list :fill-p T)))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(gdi::draw-lines drawable xlib-gc-line point-list)))))



(defun gdi-draw-points (window point-list function line-style)
  (let* ((display-info (g-value window :display-info))
	 (root-window (opal::display-info-root-window display-info))
	 (drawable (the-drawable window)))
    (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	   (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
      (set-line-style line-style
		      line-style-gc xlib-gc-line
		      root-window (get function :gdi-draw-function))
      (gdi::draw-points drawable xlib-gc-line point-list))))



(defun gdi-draw-rectangle (window left top width height function
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
    (setf function (get function :gdi-draw-function))
    (if fill-style
      (let* ((filling-style-gc (opal::display-info-line-style-gc display-info))
	     (gc (opal::opal-gc-gcontext filling-style-gc))
	     (th2 (* 2 thickness)))
	(set-filling-style fill-style filling-style-gc gc
			   root-window function)
	(gdi::rectangle drawable
			     (+ left thickness) (+ top thickness)
			     (- width th2) (- height th2))))
    (if line-style
      (let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	     (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	     (half-thickness (truncate thickness 2)))
	(set-line-style line-style line-style-gc xlib-gc-line
			root-window function)
	(gdi::rectangle drawable xlib-gc-line
			     (+ left half-thickness)
			     (+ top half-thickness)
			     (- width thickness)
			     (- height thickness))))))

(defun gdi-draw-roundtangle (window left top width height
				    x-radius y-radius function
				    line-style fill-style)
  (declare (ignore window))
  (let* ((thickness (if line-style
                      (max 1 (g-value line-style :line-thickness))
                      0))
         (th2 (+ thickness thickness))
         (fill-pattern (fill-to-pattern fill-style))
         (line-pattern (linestyle-to-pattern line-style))
         (corner-width (+ x-radius x-radius))
         (corner-height (+ y-radius y-radius))
         (right (+ left width))
         (bottom (+ top height)))
    (setf function (get function :x-draw-function))
    (PenMode (position function gdi::*pen-modes*))
    (when fill-pattern
      (PenPat fill-pattern)
      (with-gdi-colors fill-style
        (gdi::with-rectangle-arg (r (+ left thickness) (+ top thickness)
                                    (- right thickness) (- bottom thickness))
          (PaintRoundRect r (- corner-width th2)
                              (- corner-height th2)))))
    (when line-pattern
      (with-gdi-colors line-style
        (PenPat line-pattern)
        (PenSize thickness thickness)
        (gdi::with-rectangle-arg (r left top right bottom)
          (gdi::roundrect r corner-width corner-height))
        ))))


(defun gdi-draw-text (window x y string font function
			   line-style &optional fill-background invert-p)
  (setf font (g-value font :xfont))
  (setf function (get function :gdi-draw-function))
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
	    (gdi::draw-image-glyphs drawable xlib-gc-line x y string)
	    (when invert-p
	      ;; restore gc
	      (set-gc line-style-gc xlib-gc-line
		      :foreground foreground)
	      (set-gc line-style-gc xlib-gc-line
		      :background background)))
	  (gdi::draw-glyphs drawable xlib-gc-line x y string))))))


;;; Given a drawable or pixmap, returns the associated Opal window.
;;;
(defun gdi-drawable-to-window (root-window drawable)
  (declare (ignore root-window))
  (getf (drawable-plist drawable) :garnet))



;;; From windows.lisp (eliminate the obsolete deleting-window-drop-events)
;;;
(defun destroy-notify-window (event-window)
  (gdi::remove-callback event-window WM_CLOSE)
    t)


(defun connected-window-p (event-window)
  (let ((a-window (getf (drawable-plist event-window) :garnet)))
    (if a-window
      (let ((drawable (g-value a-window :drawable)))
	(and drawable (equalp (window-id drawable)
			 (window-id event-window)))))))

(print 1252)
;;; Taken from windows.lisp
;;;
(defun Delete-Notify (event-debug event-window)
  (if event-debug (format t " delete-notify ~s~%"
			  event-window;; DZG  (gdi:window-id event-window)
			  ))
  ;; Will be changed to take a-window as a parameter, rather than event-window.
  ;; Hence, the following will be unnecessary.
  (let ((a-window (getf (drawable-plist event-window) :garnet)))
    (if a-window
      (if (schema-p a-window)
	(let ((drawable (g-value a-window :drawable)))
	  (if (and drawable (equalp (window-id drawable)
			       (window-id event-window)))
	    (opal:destroy a-window)
	    ;; Then event-window is an orphaned window
	    (gdi-delete-window a-window event-window)))
	;; Then event-window is an orphaned window
	(gdi-delete-window NIL event-window)))))

(print 1272)
;; Gdi is like this, I think
;; Event-handling on the Mac is governed by particular methods for each type
;; of event (see "defmethod gdi:view-click-event-handler" above).  There is no
;; top-level loop like in X, so this function is a no-op on the Mac.
;; Maybe it should not even be a GEM method?
;;
;; This function has been particularly customized to only be called from
;; inside inter:wait-interaction-complete.  Without this throw, you would
;; never break out of the loop, and the event (a button click in the modal
;; dialog box) could never be processed.
;;
(defun gdi-event-handler (root-window ignore-keys)
  (declare (ignore root-window ignore-keys))
  (when (boundp 'gdi:*current-event*)
    (unless (zerop (event-record-what gdi:*current-event* ))
      (throw 'inter::exit-main-loop-exception NIL))))
    

;;; Returns list of drawable, parent, grandparent, ... , root.
;;;
(defun lineage-of-drawable (drawable)
  (list drawable opal::*default-gdi-root*))

#-debug-event-handler
(defmacro event-handler-debug (message &rest args)
  (declare (ignore message args)))

#+debug-event-handler
(defmacro event-handler-debug (message &rest args)
  `(format t "event-handler ~S   ~S~%" ,message ',args))

(defun GDI-font-name-p (root-window arg)
  (declare (ignore root-window))
  (gdi::logfont-p arg))

;; NIY
(defun GDI-font-exists-p (root-window name)
  (declare (ignore root-window name))
  T)

(defun gdi-flush-output (window)
  (declare (ignore window))
  (gdi::gdiflush))

;;; RETURNS: the maximum character width for the font; if <min-too> is non-nil,
;;; returns both maximum and minimum width, as multiple values.  This function
;;; used to be called by opal::get-index, but was replaced by a simple g-value
;;; of the font's :char-width.
;;;
(defun gdi-font-max-min-width (root-window opal-font min-too)
  (let ((font (g-value opal-font :xfont)))
    (if min-too
	(values (gdi::max-char-width font)
		(gdi::min-char-width font))
	(gdi::max-char-width font))))

;; Returns either a string which describes the font using X conventions,
;; or a cons of the bad value and slot.
(defun gdi-make-font-name (root-window key)
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
           (if (subsetp face-spec *gdi-font-faces*)
               face-spec)))
        (size-part
         (case (third key)
           (:small      opal::*Small-Font-Size*)
           (:medium     opal::*Medium-Font-Size*)
           (:large      opal::*Large-Font-Size*)
           (:very-large opal::*Very-Large-Font-Size*)
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
             (gdi:make-logfont size-part ;height 1
                0 ;width 2
                0 ;escapement 3
                0 ;orientation 4
                0 ; fixme weight 5
                0 ; fixme italic 6
                0 ; fixme underline
                0 ;strikeout 7
                0 ; fixme character set will be a problem for unicode 8
                0 ;outprecision 9
                0 ;clip precision
                0 ;quality 10
                0 ; fixme pitch and family 11
                family-part)))))) ; 12

(defun gdi-font-exists-p (root-window name)
  (declare (ignore root-window))
  (gdi::list-font-names opal::*default-gdi-display* name))

(defun gdi-font-to-internal (root-window font-from-file)
  (let* ((hdc (multiple-value-bind (retval h) (gdi::getdc nil)
                 (if retval h (error "getdc ~S" h))))
         (hfont (multiple-value-bind (retval h)
                  (gdi::getcurrentobject hdc GDI:OBJ_FONT)
                  (if retval h (error "getcurrentobject ~S" h)))))
    hfont))

;;; Sets the Cut buffer for X.  Note that this does NOT do a select, and
;;; therefore the cut buffer will not be affected if there already is a
;;; selection in some xterm window.
;;;
(defun gdi-get-cut-buffer (window) nil)

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


#+fixme
(defun make-event-mask (&rest all &key
   (button-press t) (button-release t) (enter-window t) (exposure t)
   (key-press t) (leave-window t)
   (owner-grab-button t) (pointer-motion t) (structure-notify t)) NIL)

(defun make-event-mask (&rest all) nil)

(defparameter *report-motion-pem*
  (make-event-mask :button-press :button-release
			:pointer-motion))

(defparameter *enter-leave-report-motion-pem*
  (make-event-mask :button-press :button-release
			:pointer-motion
			:enter-window
			:leave-window))


(defparameter *ignore-motion-grab-em*
  (make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify
			:owner-grab-button))

(defparameter *enter-leave-ignore-motion-grab-em*
  (make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify
			:enter-window
			:leave-window
			:owner-grab-button))

(defparameter *ignore-motion-em*
  (make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify))

(defparameter *enter-leave-ignore-motion-em*
  (make-event-mask :button-press :button-release
			:key-press
			:exposure
			:structure-notify
			:enter-window
			:leave-window))


;;; em = eventmask , used to change an event mask
(defparameter *report-motion-em*
  (make-event-mask :button-press :button-release
			:key-press
			:exposure
			:pointer-motion
			:structure-notify))


(defparameter *enter-leave-report-motion-em*
  (make-event-mask :button-press :button-release
			:key-press
			:exposure
			:pointer-motion
			:structure-notify
			:enter-window
			:leave-window))



;;; RETURNS: a single pixel of an image.
;;;
(defun gdi-image-bit (root-window image x y)
  (declare (ignore root-window))
  (let* ((bytes-per-line (gdi::image-gdi-bytes-per-line image))
	 (byte-pos (+ (floor x 8) (* bytes-per-line y)))
	 (byte (aref (gdi::image-gdi-data image) byte-pos))
	 (bit-pos (mod x 8)))
    (logbitp bit-pos byte)))


;;; Create an X bitmap from a series of patterns (specified as integers)
;;;
(defun gdi-image-from-bits (root-window patterns)
  (declare (ignore root-window))
  (apply #'gdi::bitmap-image patterns))

;;; Create an X bitmap from a series of patterns (specified as bit-vectors)
;;;
(defun gdi-device-image (root-window index)
  (declare (ignore root-window))
  (let ((descriptor (opal::get-descriptor index)))
    (apply #'gdi::bitmap-image descriptor)))


;;; RETURNS: the <image>'s hot spot, as multiple values.
;;;
(defun gdi-image-hot-spot (root-window image)
  (declare (ignore root-window))
  (multiple-value-bind (w h d)
     (gdi-image-size root-window image)
     (values (/ w 2) (/ h 2))))

;;; Given an X image, returns its size as multiple values (width, height,
;;; depth).
;;;
(defun gdi-image-size (root-window image)
  (declare (ignore root-window))
  (multiple-value-bind (retval w h)
     (gdi::getbitmapdimensionex (bitmap-handle image))
     (unless retval (error "getbitmapdimensionex ~S" w))
     (values w h 8)))

;;; Given an image, returns its internal array.
;;;
(defun gdi-image-to-array (root-window image)
  (declare (ignore root-window))
  (gdi::image-z-pixarray image))



;;; Helper function for gdi-initialize-window-borders
;;;
(defun set-four-borders (window left &optional top right bottom)
  (unless top
    ;; Only left specified - use for all three.
    (setf top (setf right (setf bottom left))))
  (s-value window :window :left-border-width left)
  (s-value window :top-border-width top)
  (s-value window :right-border-width right)
  (s-value window :bottom-border-width bottom))

(defun show-drawable (drawable)
  (cond
   ((eq *GDI-DRAWABLE* (class-of drawable))
    (if (not (gdi::window-shown-p drawable))
        (gdi::window-show drawable)))
   ((eq *GDI-SUBDRAWABLE* (class-of drawable))
    (if (not (gdi::view-container drawable))
        (let ((opal-window (GDI-window-from-drawable NIL drawable)))
          (gdi::set-view-container drawable
                                  (g-value opal-window :parent :drawable)))))))

(defun hide-drawable (drawable)
  (cond
   ((eq *GDI-DRAWABLE* (class-of drawable))
    (if (gdi::window-shown-p drawable)
        (gdi::window-hide drawable)))
   ((eq *GDI-SUBDRAWABLE* (class-of drawable))
    (when (gdi::view-container drawable)
      (gdi::set-view-container drawable NIL)))))
      
; can call gdi::ShowWindow here, need to determine flag
(defun GDI-map-and-wait (a-window drawable)
  (declare (ignore a-window))
  (show-drawable drawable))

(defun gdi-initialize-device (root-window)
  (declare (ignore root-window))
  (let* ((cn "HelloWorld"))
     (unless *garnet-window-class*
         (setf *garnet-window-class*
             (or (gdi::findatoma cn) (gdi::globalfindatoma cn)
                 (multiple-value-bind (retval h)
                    (gdi::registerclassa
                      (gdi::make-wndclass 0 0 0 gdi::*hinstance* NIL
                           (multiple-value-bind (retval h)
                               (gdi::loadcursora NIL (gdi::getidc gdi:IDC_ARROW))
                             (if retval h (error "loadcursor ~S" h)))
                           (multiple-value-bind (retval h)
                               (gdi::getstockobject gdi:WHITE_BRUSH)
                             (if retval h (error "getstockobject ~S" h)))
                           NIL cn))
                          (if retval h (error "registerclassa ~S" h)))))))
  (let* ((gdi-line-style-gc
	  (gdi::create-gcontext :drawable opal::*default-gdi-root*
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
	 (gdi-filling-style-gc
	  (gdi::create-gcontext :drawable opal::*default-gdi-root*
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
	  (opal::make-opal-gc	:gcontext gdi-line-style-gc
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
	  (opal::make-opal-gc	:gcontext gdi-filling-style-gc
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

    (opal::make-display-info :display opal::*default-gdi-display*
		       :screen  opal::*default-gdi-screen*
		       :root-window opal::*default-gdi-root*
		       :line-style-gc opal-line-style-gc
		       :filling-style-gc opal-filling-style-gc)))



;;; Set the border widths of the <window>.  This is quite complex, because of
;;; the differences among various window systems.
;;;
(defun gdi-initialize-window-borders (window drawable)
  ;; find out what borders really are
  (if (g-value window :parent);; window is really subwindow
    (set-four-borders window (gdi::drawable-border-width drawable))
    (let ((lineage (g-value window :lineage)))
      (case (length lineage)
	(2		;;; UWM or window without title
	 (set-four-borders window (gdi::drawable-border-width drawable)))
	(3		;;; TWM
	 (let ((border-width (gdi::drawable-border-width (second lineage))))
	   (set-four-borders
	    window
	    (+ border-width (gdi::drawable-x drawable))
	    (+ border-width (gdi::drawable-y drawable))
	    (- (gdi::drawable-width (second lineage))
	       (gdi::drawable-width (first lineage))
	       (gdi::drawable-x (first lineage))
	       (- border-width))
	    (- (gdi::drawable-height (second lineage))
	       (gdi::drawable-height (first lineage))
	       (gdi::drawable-y (first lineage))
	       (- border-width)))))
	((4 6)	;;; MWM and DECWindows, or possibly TVTWM
	 ;; if it is TVTWM, i.e. 3rd window is virtual root
	 (if (gdi::get-property (third lineage) :__SWM_VROOT)
	   (let* ((parent (second lineage))
		  (border-width (gdi::drawable-border-width parent)))
	     (set-four-borders
	      window
	      (+ border-width (gdi::drawable-x (first lineage)))
	      (+ border-width (gdi::drawable-y (first lineage)))
	      (- (gdi::drawable-width (second lineage))
		 (gdi::drawable-width (first lineage))
		 (gdi::drawable-x (first lineage))
		 (- border-width))
	      (- (gdi::drawable-height (second lineage))
		 (gdi::drawable-height (first lineage))
		 (gdi::drawable-y (first lineage))
		 (- border-width))))
	   (let* ((parent (second lineage))
		  (grandparent (third lineage))
		  (left-border-width
		   (MAX (gdi::drawable-x parent) ; MWM
			(gdi::drawable-border-width parent))) ; DECwindows
		  (top-border-width (gdi::drawable-y parent)))
	     (set-four-borders window
			       left-border-width
			       top-border-width
			       (- (gdi::drawable-width grandparent)
				  (gdi::drawable-width parent)
				  left-border-width)
			       (- (gdi::drawable-height grandparent)
				  (gdi::drawable-height parent)
				  top-border-width)))))))))


(defun gdi-inject-event (window index)
  (let ((drawable (g-value window :drawable)))
    (gdi::send-event drawable 
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
(defun gdi-map-and-wait (a-window drawable)
  (let ((display (the-display a-window)))
    #+(or allegro lispworks cmu)
    (when (eq (gdi::window-map-state drawable) :unmapped)
      (let ((suspend-process (opal::main-event-loop-process-running-p)))
	(when suspend-process
	  (opal::kill-main-event-loop-process))
	(gdi::map-window drawable)
	(gdi::display-force-output display)
	(gdi::event-case (display :discard-p nil :peek-p t :timeout 5)
			 (:map-notify (event-window)
				      (eq event-window drawable)))
	(when suspend-process
	  (opal::launch-main-event-loop-process))))
    #-(or lucid allegro lispworks cmu)
    (progn
      (gdi::map-window drawable)
      (gdi::display-force-output display))))


(defun gdi-max-character-ascent (root-window opal-font)
  (declare (ignore root-window))
  (gdi::max-char-ascent (g-value opal-font :xfont)))


(defun gdi-max-character-descent (root-window opal-font)
  (gdi::max-char-descent (g-value opal-font :xfont)))

(defun GDI-mouse-grab (window grab-p want-enter-leave &optional owner-p)
  (declare (ignore window grab-p want-enter-leave &optional owner-p)))

(defun GDI-raise-or-lower (window raise-p)
  (let ((drawable (g-value window :drawable))
        (new-layer (if raise-p 0 most-positive-fixnum)))
    ;; You can't raise or lower a GDI-SUBDRAWABLE
    (if (eq *GDI-DRAWABLE* (class-of drawable))
	(gdi::set-window-layer drawable new-layer T))))

(defun GDI-reparent (window new-parent drawable left top)
  (declare (ignore left top))
  (unless (or (null new-parent)
              (is-a-p new-parent opal::window))
    (error "Parent ~S of window ~S is not of type window~%"
           new-parent window))
  (let ((old-buffer (g-value window :buffer))
        (children (g-value window :child))
        new-window)
    (hide-drawable drawable)
    (when old-buffer
      (s-value window :buffer NIL)
      (gdi::dispose old-buffer))
    (setf new-window (opal::create-x-drawable window))
    (dolist (child children)
      (gdi::set-view-container (g-value child :drawable) new-window))))


;;; A function to determine the next multiple of 8 past the current #
;;; used for x-read-an-image function to determine width of array
(defun get-width (n)
  (let ((remainder (mod n 8)))
    (if (zerop remainder) n (+ n (- 8 remainder)))))


;;; Read and return the numbers at the end of a line on stream bitstream
;;;
(defun get-nums (bitstream)
     (do ((ch (peek-char t bitstream) (peek-char t bitstream)))
	 ((digit-char-p ch))
          (read-char bitstream))
     (parse-integer (read-line bitstream)))


;;; A function that punts each line of text that begins with #
;;; helper function for x-read-an-image
(defun punt-till (stream)
  (let ((ch (read-char stream)))
    (if (char= ch #\#)
	(progn (read-line stream)
	       (punt-till stream)))))


;;; Reads a bitmap file into an array and displays on screen in window
;;; root-window

;(defun setpixelv (pic row-index h val) t)
;(trace setpixelv)

(defun gdi-read-an-image (root-window pathname)
  (with-open-file (bitstream pathname :direction :input)
    (let* ((hdc (multiple-value-bind (retval h) (gdi::getdc nil)
                 (if retval h (error "getdc ~S" h))))
           (width (get-nums bitstream))
	   (height (get-nums bitstream))
           (data-width (get-width width))
           (pic (multiple-value-bind (retval h)
                    (gdi::createcompatiblebitmap hdc width height)
                 (if retval h (error "createcompatiblebitmap ~S" h))))
	   (char-map '((#\0  0) (#\1  1) (#\2  2) (#\3  3)
		       (#\4  4) (#\5  5) (#\6  6) (#\7  7)
		       (#\8  8) (#\9  9) (#\a 10) (#\b 11)
		       (#\c 12) (#\d 13) (#\e 14) (#\f 15)))
           (row-index 0)
           ch ch2 char-num)
      (multiple-value-bind (retval h)
         (gdi::setbitmapdimensionex pic width height)
           (unless retval (error "setbitmapdimensionex ~S" h)))
      (punt-till bitstream)                ; punt chars till data

      (dotimes (h height)
        (dotimes (w (/ data-width 8))
	  (read-delimited-list #\0 bitstream); throw away all chars till 0
	  (read-char bitstream)              ; throw away x character
	  (setf ch2 (read-char bitstream))   ; read 1st hex #
	  (setf ch (read-char bitstream))    ; read 2nd hex #
          (read-char bitstream)              ; throw away comma
          (dotimes (n 2)
            (setf char-num (second (assoc ch char-map :test #'char-equal)))
            (dotimes (i 4)
              (when (< row-index width)
                (gdi::setpixelv pic row-index h
                      (if (logbitp i char-num)
                          gdi:*black*
                          gdi:*white*))
                (incf row-index)))
            (setf ch ch2)))
        (setf row-index 0))
      (make-bitmap :handle pic :depth 8))))

(defun GDI-set-clip-mask (a-window clip-mask &optional lstyle-ogc fstyle-ogc)
  (declare (ignore lstyle-ogc fstyle-ogc))
  (let* ((drawable (g-value a-window :drawable)))
    (cond ((eq :none clip-mask)
           ;; Even though we might be setting the clip-mask of the buffer,
           ;; we have to use the drawable to get the size
           (let ((view-size-point (gdi::view-size drawable))
                 (old-clip-rgn (gdi::view-clip-region drawable))
                 (wptr (gdi::wptr drawable)))
             ;; Will clip into *spare-region-1*
             (SetRectRgn *spare-region-1* 0 0 (gdi::point-h view-size-point)
                                                 (gdi::point-v view-size-point))
             ;; Might not have been set yet, like when an error-gadget
             ;; dialog box is becoming visible
             (if old-clip-rgn
                 (SectRgn *spare-region-1* old-clip-rgn *spare-region-1*))
             ;; Don't draw over the grow box.  Have to send TOP-LEVEL window!
             (unless (or (g-value a-window :omit-title-bar-p)
                         (null wptr))
               (diff-with-grow-rgn *spare-region-1* drawable
                                   (gdi::window-object wptr)))
            (diff-with-subview-rgns *spare-region-1* drawable)
             (SetClip *spare-region-1*)))
          (t
           (multiple-value-bind (l1 t1 w1 h1 l2 t2 w2 h2)
               (values-list clip-mask)
             ;; Will clip into *spare-region-1*
             (SetRectRgn *spare-region-1* l1 t1 (+ l1 w1) (+ t1 h1))
             ;; When there is a second clip-mask, then create another region
             ;; and union it with region-1, storing in region-1, then throw
             ;; away region-2 and continue using region-1
             (when l2
               (SetRectRgn *spare-region-2* l2 t2 (+ l2 w2) (+ t2 h2))
               (UnionRgn *spare-region-1* *spare-region-2*
                           *spare-region-1*))
             ;; Now intersect Opal's computed clip-region with the bounding
             ;; region of the view
             (SectRgn *spare-region-1* (gdi::view-clip-region drawable)
                        *spare-region-1*)
             ;; Don't draw over the grow box.  Have to send TOP-LEVEL window!
             (unless (g-value a-window :omit-title-bar-p)
               (diff-with-grow-rgn *spare-region-1* drawable
                                   (gdi::window-object (gdi::wptr drawable))))
             (diff-with-subview-rgns *spare-region-1* drawable)
             (SetClip *spare-region-1*))))))
                                                   
;;; If <grab-p>, this is a mouse grab or a change-active-pointer grab;
;;; otherwise, it is a mouse ungrab.
;;; If <owner-p> is a keyword, then do a change-active-pointer-grab; otherwise,
;;; do a regular grab-pointer.
;;;
(defun gdi-mouse-grab (window grab-p want-enter-leave &optional owner-p)
  (declare (ignore window))
  (if grab-p
    ;; Mouse grab.
    (if (keywordp owner-p)
      (gdi::change-active-pointer-grab opal::*default-gdi-display*
				       (if want-enter-leave
					 *enter-leave-report-motion-pem*
					 *report-motion-pem*))
      (gdi::grab-pointer opal::*default-gdi-display*
			 (if want-enter-leave
			   *enter-leave-report-motion-pem*
			   *report-motion-pem*)
			 :owner-p owner-p))
    ;; Mouse ungrab.
    (gdi::ungrab-pointer opal::*default-gdi-display*)))
    
;;; Move the <window> to the top (if <raise-p>) or to the bottom.
;;;
(defun gdi-raise-or-lower (window raise-p)
  (setf (gdi::window-priority (g-value window :drawable))
	(if raise-p :above :below)))

;;; Reparent a window.
;;;
(defun gdi-reparent (window new-parent drawable left top)
  (if new-parent
    (if (is-a-p new-parent opal::window)
      (gdi::reparent-window drawable
			    (g-value new-parent :drawable)
			    left top)
      (error "Parent ~S of window ~S is not of type window~%"
	     new-parent window))
    (gdi::reparent-window drawable
			  (opal::display-info-root-window
			   (g-value window :display-info))
			  left top)))



(defun gdi-set-clip-mask (a-window clip-mask &optional lstyle-ogc fstyle-ogc)
  (declare (ignore a-window))
  (let ((lstyle-xgc (opal::opal-gc-gcontext lstyle-ogc))
	(fstyle-xgc (opal::opal-gc-gcontext fstyle-ogc)))
    (set-gc lstyle-ogc lstyle-xgc :clip-mask clip-mask)
    (set-gc fstyle-ogc fstyle-xgc :clip-mask clip-mask)))



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



(defun gdi-set-device-variables (root-window)
  (declare (ignore root-window))
  (setq *default-gdi-display-number*
	(get-display-number opal::*default-gdi-display-name*))

  (setq opal::*default-gdi-display*
	(gdi::open-display opal::*default-gdi-display-name*
			   :display *default-gdi-display-number*))
  (setq opal::*default-gdi-screen*
        (nth opal::*default-gdi-screen-number*
             (gdi::display-roots opal::*default-gdi-display*)))
  (setq opal::*screen-width* (gdi::screen-width opal::*default-gdi-screen*))
  (setq opal::*screen-height* (gdi::screen-height opal::*default-gdi-screen*))
  (setq opal::*default-gdi-root* (gdi::screen-root opal::*default-gdi-screen*))

  ;;; We must call xlib:open-display a second time to get to the colormap,
  ;;; because it turns out that if we simply used the *default-gdi-display*
  ;;; to get at the colormap, then every time xlib:alloc-color was called
  ;;; it would cause an implicit xlib:display-force-output.
  ;;; (Except that in CMUCL you cannot use two displays at one time.)
  (setq opal::*default-gdi-colormap*
	(gdi::screen-default-colormap
	 #+cmu
	 opal::*default-gdi-screen*
	 #-cmu
	 (nth opal::*default-gdi-screen-number*
	      (gdi::display-roots
	       (gdi::open-display
		opal::*default-gdi-display-name*
		:display *default-gdi-display-number*)))))
  (setq opal::*white* (gdi::screen-white-pixel opal::*default-gdi-screen*))
  (setq opal::*black* (gdi::screen-black-pixel opal::*default-gdi-screen*))
  (setf opal::*exposure-event-mask*
	(make-event-mask :exposure :structure-notify
			      :button-press :key-press)))

;; There are three types of scrap -- :text, :lisp, and :fred.  The last
;; argument to gdi:put-scrap clears out what was previously in the scrap.
(defun GDI-set-cut-buffer (window string)
  (declare (ignore window))
  (gdi::put-scrap :TEXT string T))

;;; Sets the pointer from a raw X <drawable> (a drawable or pixmap) to the
;;; Opal <window>.
;;;
(defun gdi-set-drawable-to-window (window drawable)
   (setf (drawable-plist drawable) (list :garnet window)))

(defun gdi-set-draw-function-alist (root-window)
  (declare (ignore root-window))
  (setq opal::*function-alist*
	(cond ((eq opal::*white* gdi::*white*)
	       `((:clear . ,boole-clr);; (color, opal::*white* = 0)
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
	      (opal::*is-this-a-color-screen?*;; HP
	       `((:clear . ,boole-set);; (color, opal::*white* = 1)
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
	      (t;; IBM-RT (black-and-white)
	       `((:clear . ,boole-set);; (black-and-white, opal::*white* = 1)
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


;;; RETURNS:
;;; normally NIL; returns T if:
;;; - the property is :WIDTH or :HEIGHT and a new buffer is required because
;;;   the old one was too small; or
;;; - the property is :VISIBLE and the window needs to be mapped.
;;;
(defun gdi-set-window-property (window property value)
  (case property
    (:BACKGROUND-COLOR
     (let* ((gc (g-value window :buffer-gcontext))
	    (drawable (g-value window :drawable))
	    (index (gdi-color-to-index window value)))
       (setf (gdi::window-background drawable) index)
       (when gc (setf (gdi::gcontext-background gc) index))
       (when (g-value window :visible)
	 (gdi::map-window drawable)))
     nil)
    (:BUFFER-GCONTEXT
     ;; The <value> is a list of three elements: (buffer foregr. backgr.)
     (s-value window :buffer-gcontext
	      (gdi::create-gcontext :drawable (first value)
				    :foreground (second value)
				    :background (third value))))
    (:CURSOR
;     (setf (gdi::window-cursor (g-value window :drawable)) value))
      nil)
    (:EVENT-MASK-FIXME
     ;; The <value> should be a keyword, encoded as explained above the ...em
     ;; defparameters.
     (let ((skip-force-output NIL))
       (setf (gdi::window-event-mask (g-value window :drawable))
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
	 #+CMU (gdi::display-force-output opal::*default-gdi-display*)
	 #-CMU NIL

	 ;; Need to force-output when using the background m-e-l
	 ;; process,  otherwise this doesn't get noticed.
	 (gdi::display-force-output opal::*default-gdi-display*))))
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
	 #-clgdi-cl-error
	 (2				; UWM or window without label.
	  (s-value window :left x)
	  (s-value window :top y))
	 #+clgdi-cl-error
	 (2				; xlib:query-tree does not work, so
	  ;; true values of left and top may be unobtainable.
	  ;; In fact, x and y will be 0 if you have just
	  ;; resized the window.
	  (unless (and (zerop x) (zerop y))
	    (s-value window :left x)
	    (s-value window :top y)))
	 (3				; TWM
	  (s-value window :left (gdi::drawable-x (second lineage)))
	  (s-value window :top (gdi::drawable-y (second lineage))))
	 ((4 6)				; MWM and DECWindows, or possibly TVTWM
	  (let ((3rd (third lineage)))
	    (if (gdi::get-property 3rd :__SWM_VROOT)
	      (let ((2nd (second lineage)))
		(s-value window :left (gdi::drawable-x 2nd))
		(s-value window :top (gdi::drawable-y 2nd)))
	      (progn
		(s-value window :left (gdi::drawable-x 3rd))
		(s-value window :top (gdi::drawable-y 3rd)))))))))
    (:HEIGHT
     (let ((old-buffer (g-value window :buffer)))
       (setf (gdi::drawable-height (g-value window :drawable))
	     (max 0 value))
       (setf (opal::win-update-info-height (g-value window :win-update-info))
	     value)
       ;; Does the buffer need to be recreated?
       (and old-buffer (> value (gdi::drawable-height old-buffer)))))
    (:ICON-TITLE
     (gdi::set-standard-properties (g-value window :drawable) :icon-name value)
     nil)
    (:LEFT
     (let* ((drawable (g-value window :drawable))
	    (hints (gdi::wm-normal-hints drawable)))
       (setf (gdi::drawable-x drawable) value
	     (gdi::wm-size-hints-x hints) value
	     (gdi::wm-normal-hints drawable) hints))
     nil)
    (:PARENT
     (let ((left (g-value window :left))
	   (top (g-value window :top))
	   (drawable (g-value window :drawable)))
       (if value
	 (gdi::reparent-window drawable (g-value window :drawable) left top)
	 (gdi::reparent-window drawable (opal::display-info-root-window
					 (g-value window :display-info))
			       left top)))
     nil)
    (:POINTER-POSITION
     ;; Warps the pointer to the position expressed by the <value>
     (gdi::warp-pointer (g-value window :drawable) (car value) (cdr value))
     (gdi::display-force-output opal::*default-gdi-display*))
    (:REPORT-ASYNCHRONOUS-ERRORS
     (setf (gdi::display-report-asynchronous-errors
	    opal::*default-gdi-display*)
	   value))
    (:SAVE-UNDER
     (setf (gdi::window-save-under (g-value window :drawable)) value)
     nil)
    (:SUBWINDOW-MODE
     (let ((display-info (g-value window :display-info)))
       (setf (gdi::gcontext-subwindow-mode
	      (opal::opal-gc-gcontext
	       (opal::display-info-line-style-gc display-info)))
	     value)
       (setf (gdi::gcontext-subwindow-mode
	      (opal::opal-gc-gcontext
	       (opal::display-info-filling-style-gc display-info)))
	     value)))
    (:TITLE
     (let ((drawable (g-value window :drawable)))
       (setf (gdi::wm-name drawable) value)
       (gdi::set-standard-properties drawable :name value))
     nil)
    (:TOP
     (let* ((drawable (g-value window :drawable))
	    (hints (gdi::wm-normal-hints drawable)))
       (setf (gdi::drawable-y drawable) value
	     (gdi::wm-size-hints-y hints) value
	     (gdi::wm-normal-hints drawable) hints))
     nil)
    (:VISIBLE
     (let* ((drawable (g-value window :drawable))
	    (vis (g-value window :visible))
	    (map-window NIL))
       (cond ((eq vis t)
	      (setf map-window t))
	     ((eq vis :iconified)
	      #+clgdi-mit-r4
	      (gdi::iconify-window drawable opal::*default-gdi-screen*))
	     ((eq vis nil)
	      #+clgdi-mit-r4
	      (gdi::withdraw-window drawable opal::*default-gdi-screen*)
	      #-clgdi-mit-r4
	      (gdi::unmap-window drawable)))
       ;; Does the window need to be mapped?
       map-window))
    (:WIDTH
     (let ((old-buffer (g-value window :buffer)))
       (setf (gdi::drawable-width (g-value window :drawable))
	     (max 0 value))
       (setf (opal::win-update-info-width (g-value window :win-update-info))
	     value)
       ;; Does the buffer need to be recreated?
       (and old-buffer (> value (gdi::drawable-width old-buffer)))))
    (T
     (format t "Unknown property ~S in gem:gdi-set-window-property.~%"
	     property))))



;;; RETURNS: T if the filling style of the given display is stippled
;;;
(defun gdi-stippled-p (root-window)
  (eq (gdi::gcontext-fill-style
       (opal::opal-gc-gcontext
	(opal::display-info-filling-style-gc
	 (g-value root-window :display-info))))
      :stippled))



;;; RETURNS: multiple values:
;;; width
;;; ascent
;;; descent
;;; left-bearing
;;; right-bearing
;;; plus other information we do not use.
;;; fixme release the device context
(defun gdi-text-extents (root-window font string)
  ;; This g-value works for instances of both opal:font and opal:font-from-file
  (let* ((font-spec (g-value font :font-from-file :xfont))
         (hdc (multiple-value-bind (retval h) (gdi::getdc nil)
                 (if retval h (error "getdc ~S" h))))
         (tm (gdi:make-textmetric))
         (hOldFont (multiple-value-bind (retval h)
                  (gdi::selectobject hdc font-spec)
                  (if retval h (error "selectobject ~S" h)))))
     (multiple-value-bind (retval h)
        (gdi::gettextmetricsa hdc tm)
        (unless retval (error "gettextmetricsa ~S" h)))
     (let* ((ascent (gdi::textmetric-tmascent tm))
            (descent (gdi::textmetric-tmdescent tm))
            (left-bearing 1)) ;fixme
        (multiple-value-bind (retval h)
           (gdi::selectobject hdc hOldFont)
           (unless retval (error "selectobject ~S" h)))
        (multiple-value-bind (retval width height)
;;; fixme use string length in next expression
           (gdi::gettextextentpoint32a hdc string (length string))
           (unless retval (error "gettextextentpoint32 ~S" width))
         (values width ascent descent left-bearing)))))

;;; Returns the width of the <string> in the given font.
;;;
(defun gdi-text-width (root-window opal-font string)
   (gdi-text-extents root-window opal-font string))

;;; Translates a keyboard scan
(defun gdi-translate-code (window scan-code shiftp)
  (gdi::keycode->keysym
   (opal::display-info-display
    (the opal::DISPLAY-INFO (g-value window :display-info)))
   scan-code (if shiftp 1 0)))

;;; RETURNS: the coordinates of point <x,y> in the <window> relative to
;;; the <window2>, or to the screen's origin if <window2> is not specified.
;;; Returns multiple values.
;;;
(defun gdi-translate-coordinates (root-window window1 x y &optional window2)
  (declare (ignore root-window))
  (let ((draw1 (if window1 (g-value window1 :drawable)))
	(draw2 (if window2 (g-value window2 :drawable))))
    #-clgdi-cl-error
    (if (and draw1 (null window2))
      (setf draw2 (gdi::drawable-root draw1)))
    #-clgdi-cl-error
    (when (and draw2 (null window1))
      (setq draw1 (gdi::drawable-root draw2)))
    (if (and draw1 draw2)
      (gdi::translate-coordinates draw1 x y draw2))))

(defun multiple-value-point (point)
  (values (gdi::point-h point)
          (gdi::point-v point)))

(defun GDI-translate-coordinates (root-window win1 x y &optional win2)
  (declare (ignore root-window))
  (let ((draw1 (if win1 (g-value win1 :drawable)))
        (draw2 (if win2 (g-value win2 :drawable))))
    (cond
     ((and draw1 draw2
           (eq (g-value win1 :parent)(g-value win2 :parent)))
      (let ((left1 (g-value win1 :left))
            (left2 (g-value win2 :left))
            (top1 (g-value win1 :top))
            (top2 (g-value win2 :top)))
        (values (+ (- left1 left2) x)
                (+ (- top1 top2) y))))
     ((and draw1 draw2) ;; different parent windows
      (multiple-value-bind (x1 y1)
            (multiple-value-point (gdi::local-to-global draw1 x y))
        (multiple-value-point (gdi::global-to-local draw2 x1 y1))))
     (draw2 (multiple-value-point (gdi::global-to-local draw2 x y)))
     (draw1 (multiple-value-point (gdi::local-to-global draw1 x y)))
     (T (error "translate coord both wins NIL")))))

;;; RETURNS: a human-readable representation for the drawable associated
;;; with the <window>.
;;;
(defun gdi-window-debug-id (window)
  (gdi::window-id (g-value window :drawable)))

(defun GDI-window-has-grown (window width height)
  (let ((old-buffer (g-value window :buffer)))
    (if old-buffer
        (let* ((old-size (slot-value old-buffer 'gdi::size))
               (old-width (gdi::point-h old-size))
               (old-height (gdi::point-v old-size)))
          (or (> width old-width)
              (> height old-height))))))


;;; RETURNS: the depth in bits of the drawable associated with the window.
;;;
(defun junk-drawable-depth (u) 24)
(defun gdi-window-depth (window)
  (junk-drawable-depth (g-value window :drawable)))

;;; Given an GDI drawable, returns the associated Opal window.
;;; The documented way to get this function is through opal:drawable-to-window,
;;; which only takes one parameter: the GDI drawable.
;;;
(defun gdi-window-from-drawable (root-window gdi-window)
  (declare (ignore root-window))
  (getf (gdi::drawable-plist gdi-window) :garnet))


;;; RETURNS: true if the <window>'s old buffer was smaller (in at least one
;;; dimension) than the new <width> and <height>
;;;
(defun gdi-window-has-grown (window width height)
  (let ((old-buffer (g-value window :buffer)))
    (and old-buffer
	 (or (> height (gdi::drawable-height old-buffer))
	     (> width  (gdi::drawable-width old-buffer))))))


;;; Create an image from (a piece of) a window.
;;;
(defun gdi-window-to-image (window left top width height)
  (let ((drawable (g-value window :drawable)))
    (if drawable
      (gdi::get-image drawable :format :z-pixmap :x left :y top
		      :width width :height height))))



(defun gdi-write-an-image (root-window pathname image)
  (declare (ignore root-window))
  (gdi::write-bitmap-file pathname image))

(defun gdi-compare-and-get-possible-stop-event () nil)

;;; --------------------------------------------------

(defun attach-GDI-methods (gdi-device)
  (attach-method gdi-device :all-garnet-windows #'gdi-all-garnet-windows)
  (attach-method gdi-device :beep #'gdi-beep)
  (attach-method gdi-device :bit-blit #'gdi-bit-blit)
  (attach-method gdi-device :black-white-pixel #'gdi-black-white-pixel)
  (attach-method gdi-device :character-width #'gdi-character-width)
  (attach-method gdi-device :clear-area #'gdi-clear-area)
  (attach-method gdi-device :color-to-index #'gdi-color-to-index)
  (attach-method gdi-device :colormap-property #'gdi-colormap-property)
  (attach-method gdi-device :copy-to-pixmap #'gdi-copy-to-pixmap)
  (attach-method gdi-device :create-cursor #'gdi-create-cursor)
  (attach-method gdi-device :create-image #'gdi-create-image)
  (attach-method gdi-device :create-image-array #'gdi-create-image-array)
  (attach-method gdi-device :create-pixmap #'gdi-create-pixmap)
  (attach-method gdi-device :create-state-mask #'gdi-create-state-mask)
  (attach-method gdi-device :create-window #'gdi-create-window)
  (attach-method gdi-device :delete-font #'gdi-delete-font)
  (attach-method gdi-device :delete-pixmap #'gdi-delete-pixmap)
  (attach-method gdi-device :delete-window #'gdi-delete-window)
  (attach-method gdi-device :device-image #'gdi-device-image)
  (attach-method gdi-device :discard-mouse-moved-events
		 #'gdi-discard-mouse-moved-events)
  (attach-method gdi-device :discard-pending-events #'gdi-discard-pending-events)
  (attach-method gdi-device :draw-arc #'gdi-draw-arc)
  (attach-method gdi-device :draw-image #'gdi-draw-image)
  (attach-method gdi-device :draw-line 'gdi-draw-line)
  (attach-method gdi-device :draw-lines #'gdi-draw-lines)
  (attach-method gdi-device :draw-points #'gdi-draw-points)
  (attach-method gdi-device :draw-rectangle #'gdi-draw-rectangle)
  (attach-method gdi-device :draw-roundtangle #'gdi-draw-roundtangle)
  (attach-method gdi-device :draw-text #'gdi-draw-text)
  (attach-method gdi-device :drawable-to-window #'gdi-drawable-to-window)
  (attach-method gdi-device :event-handler #'gdi-event-handler)
  (attach-method gdi-device :flush-output #'gdi-flush-output)
  (attach-method gdi-device :font-max-min-width #'gdi-font-max-min-width)
  (attach-method gdi-device :font-name-p #'gdi-font-name-p)
  (attach-method gdi-device :font-exists-p #'gdi-font-exists-p)
  (attach-method gdi-device :font-to-internal #'gdi-font-to-internal)
  (attach-method gdi-device :get-cut-buffer #'gdi-get-cut-buffer)
  (attach-method gdi-device :device-image #'gdi-device-image)
  (attach-method gdi-device :image-bit #'gdi-image-bit)
  (attach-method gdi-device :image-from-bits #'gdi-image-from-bits)
  (attach-method gdi-device :image-hot-spot #'gdi-image-hot-spot)
  (attach-method gdi-device :image-size #'gdi-image-size)
  (attach-method gdi-device :image-to-array #'gdi-image-to-array)
  (attach-method gdi-device :initialize-device #'gdi-initialize-device)
  (attach-method gdi-device :initialize-window-borders
		 #'gdi-initialize-window-borders)
  (attach-method gdi-device :inject-event #'gdi-inject-event)
  (attach-method gdi-device :make-font-name #'gdi-make-font-name)
  (attach-method gdi-device :map-and-wait #'gdi-map-and-wait)
  (attach-method gdi-device :max-character-ascent #'gdi-max-character-ascent)
  (attach-method gdi-device :max-character-descent #'gdi-max-character-descent)
  (attach-method gdi-device :mouse-grab #'gdi-mouse-grab)
  (attach-method gdi-device :raise-or-lower #'gdi-raise-or-lower)
  (attach-method gdi-device :read-an-image #'gdi-read-an-image)
  (attach-method gdi-device :reparent #'gdi-reparent)
  (attach-method gdi-device :set-clip-mask #'gdi-set-clip-mask)
  (attach-method gdi-device :set-cut-buffer #'gdi-set-cut-buffer)
  (attach-method gdi-device :set-device-variables #'gdi-set-device-variables)
  (attach-method gdi-device :set-draw-function-alist #'gdi-set-draw-function-alist)
  (attach-method gdi-device :set-drawable-to-window #'gdi-set-drawable-to-window)
  (attach-method gdi-device :set-window-property #'gdi-set-window-property)
  (attach-method gdi-device :stippled-p #'gdi-stippled-p)
  (attach-method gdi-device :text-extents #'gdi-text-extents)
  (attach-method gdi-device :text-width #'gdi-text-width)
  (attach-method gdi-device :translate-code #'gdi-translate-code)
  (attach-method gdi-device :translate-coordinates #'gdi-translate-coordinates)
  (attach-method gdi-device :window-debug-id #'gdi-window-debug-id)
  (attach-method gdi-device :window-depth #'gdi-window-depth)
  (attach-method gdi-device :window-from-drawable #'gdi-window-from-drawable)
  (attach-method gdi-device :window-has-grown #'gdi-window-has-grown)
  (attach-method gdi-device :window-to-image #'gdi-window-to-image)
  (attach-method gdi-device :write-an-image #'gdi-write-an-image)
  ;; Defined in inter/gdi-inter.lisp
  (attach-method gdi-device :check-double-press #'gdi-check-double-press)
  (attach-method gdi-device :compare-and-get-possible-stop-event
                          #'gdi-compare-and-get-possible-stop-event)
  (attach-method gdi-device :set-interest-in-moved #'gdi-set-interest-in-moved)
  (attach-method gdi-device :translate-mouse-character
		          #'gdi-translate-mouse-character)
  (attach-method gdi-device :translate-character #'gdi-translate-character)
  ;; now make all windows inherit Gem methods from the GDI device.
  ;;

  (set-window-methods opal::window gdi-device)
  )

(defun GDI-TOP-LEVEL-INITIALIZE (display-name)
  (declare (ignore display-name))

  ;; This schema stands for the top-level root window for the X device.
  ;; We use create-schema to prevent any :initialize method from firing.
  ;;
  (create-schema '*root-window*
    (:is-a opal::window))

  ;; This schema points to the root window, and contains the slot :methods
  ;; which names all existing Gem method.  The slot is copied into the root
  ;; nodes of the windows and fonts hierarchies.
  ;;
  (create-schema 'GDI-DEVICE
    (:root-window *root-window*)
    (:device-type :GDI))

  (attach-GDI-methods GDI-DEVICE)

  (opal::initialize-gdi-values (or display-name (opal::get-full-display-name))
                               *root-window*)

  (s-value opal::DEVICE-INFO :current-root *root-window*)
  (s-value opal::DEVICE-INFO :current-device GDI-DEVICE)
  (pushnew GDI-DEVICE (g-value opal::DEVICE-INFO :active-devices))

  ;; *black* and *white* are used in gdi-initialize-device
  (setf opal::*black* gdi:*black*)
  (setf opal::*white* gdi:*white*)

  (let ((display-info (gdi-initialize-device NIL)))
    (s-value *root-window* :drawable
	     (opal::display-info-root-window display-info))
    (s-value *root-window* :display-info display-info))

(print 2518)

  ;; This is supposed to determine if you have a color screen.
  (with-constants-disabled
    (s-value opal::COLOR :color-p
             (setf opal::*is-this-a-color-screen?*
                   (color-device-attached?))))

  ;; Enable processing of key-up events.  By default, Garnet defines function
  ;; keys to simulate leftdown, middledown, and rightdown.  If we didn't get
  ;; key-up events, we couldn't generate up-events for these keys.
  ; (SetEventMask T)

  (setf opal:*screen-width* 
         (gdi::getsystemmetrics gdi:SM_CXSCREEN))
  (setf opal:*screen-height*
         (gdi::getsystemmetrics gdi:SM_CYSCREEN))


  (opal::set-draw-functions)
  (opal::initialize-halftones)

  *root-window*)

;;; Make the initializer function available to the outside world.
;;;
(push (cons :GDI #'GDI-TOP-LEVEL-INITIALIZE) *device-initializers*)

(trace window-id)
(trace connected-gdi-drawable-p)
(trace color-device-attached?)
(trace get-gdi-stipple)
(trace set-line-style)
(trace set-filling-style)
;(trace do-all-garnet-windows)
;(trace get-event-bits)
;(trace find-overlapping-view-containing-point)
(trace gdi-all-garnet-windows)
(trace gdi-beep)
(trace gdi-bit-blit)
(trace gdi-black-white-pixel)
(trace gdi-character-width)
(trace gdi-clear-area)
(trace gdi-color-to-index)
(trace gdi-colormap-property)
(trace gdi-colormap-property)
(trace gdi-copy-to-pixmap)
(trace gdi-create-cursor)
(trace get-pixmap-formats)
(trace depth-pixmap-format)
(trace depth-to-bits-per-pixel)
(trace gdi-create-image)
(trace gdi-create-image-array)
(trace gdi-create-state-mask)
(trace gdi-create-pixmap)
(trace gdi-create-window)
(trace GDI-create-state-mask)
(trace gdi-delete-font)
(trace gdi-delete-pixmap)
(trace gdi-delete-window)
(trace gdi-discard-mouse-moved-events)
(trace gdi-discard-pending-events)
(trace bit-vector-to-int)
(trace gdi-discard-pending-events)
(trace fill-to-pattern)
(trace linestyle-to-pattern)
(trace style-to-xcolors)
(trace gdi-draw-arc)
(trace gdi-draw-image)
(trace gdi-draw-line)
(trace gdi-draw-lines)
(trace gdi-draw-points)
(trace gdi-draw-rectangle)
(trace gdi-draw-roundtangle)
(trace gdi-draw-text)
(trace gdi-drawable-to-window)
(trace destroy-notify-window)
(trace connected-window-p)
(trace Delete-Notify)
(trace gdi-event-handler)
(trace lineage-of-drawable)
(trace GDI-font-name-p)
(trace GDI-font-exists-p)
(trace gdi-flush-output)
(trace gdi-font-max-min-width)
(trace gdi-initialize-device)
(trace gdi-make-font-name)
(trace gdi-font-exists-p)
(trace gdi-font-to-internal)
(trace gdi-get-cut-buffer)
;(trace make-event-mask)
(trace gdi-image-bit)
(trace gdi-image-from-bits)
(trace gdi-device-image)
(trace gdi-image-hot-spot)
(trace gdi-image-size)
(trace gdi-image-to-array)
(trace set-four-borders)
(trace show-drawable)
(trace hide-drawable)
(trace GDI-map-and-wait)
(trace gdi-initialize-window-borders)
(trace gdi-inject-event)
(trace gdi-map-and-wait)
(trace gdi-max-character-ascent)
(trace gdi-max-character-descent)
(trace GDI-mouse-grab)
(trace GDI-raise-or-lower)
(trace GDI-reparent)
(trace GDI-set-clip-mask)
(trace gdi-mouse-grab)
(trace gdi-raise-or-lower)
(trace gdi-read-an-image)
(trace gdi-reparent)
(trace gdi-set-clip-mask)
(trace get-display-number)
(trace gdi-set-device-variables)
(trace GDI-set-drawable-to-window)
(trace GDI-set-cut-buffer)
(trace gdi-set-drawable-to-window)
(trace gdi-set-draw-function-alist)
(trace gdi-set-window-property)
(trace gdi-stippled-p)
(trace gdi-text-extents)
(trace gdi-text-width)
(trace gdi-translate-code)
(trace gdi-translate-coordinates)
(trace multiple-value-point)
(trace GDI-translate-coordinates)
(trace gdi-window-debug-id)
(trace GDI-window-has-grown)
(trace gdi-window-depth)
(trace gdi-window-from-drawable)
(trace gdi-window-has-grown)
(trace gdi-window-to-image)
(trace gdi-write-an-image)
(trace gdi-compare-and-get-possible-stop-event)
(trace attach-GDI-methods)
(trace GDI-TOP-LEVEL-INITIALIZE)
