;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

(in-package :gem)

;;  Functions that will determine whether the display can be opened
(defun get-full-display-name ()
  ;; If you have CLX, you most likely have this. (Except for allegro.)
  (xlib::getenv "DISPLAY"))

(defun get-display-name (display)
  ;; The display name is everything up to the first colon.
  (unless (find #\: display)
    (error "The display specification  \"~A\" is ill-formed: missing colon" display))
  (subseq display 0 (position #\: display)))

(defun get-display-number (display)
  ;; The display number is everything from the colon to the period (if
  ;; it is present).
  (let* ((colon-pos (position #\: display :from-end t))
         (period-pos (and colon-pos (position #\. display :start colon-pos))))
    (unless colon-pos
      (error "The display specification  \"~A\" is ill-formed: missing colon" display))
    (let ((display-number
           (ignore-errors
             (parse-integer
              (subseq display (1+ colon-pos) period-pos)))))
      (unless (numberp display-number)
        (error "The display specification  \"~A\" is invalid: bad display number" display))
      display-number)))

;; (defun verify-display-can-be-opened ()
;;   (let* (val errorp)
;;     (unwind-protect
;;          (progn
;;            (multiple-value-setq (val errorp)
;;              (ignore-errors
;;                (xlib:open-default-display)))
;;            (if errorp
;;                (error "Could not open a display for ~S.
;;      You must already be running X to load or compile Garnet. Your
;; DISPLAY environment variable must be set with the name of the machine
;; on which the Garnet windows will be displayed. Ordinarily this should
;; be done automagically for you. You can see the value of this variable
;; by typing \"printenv DISPLAY\" at the shell prompt.

;; The DISPLAY environment variable should look something like one of the
;; following:

;;   \"localhost:12.0\"
;;   \"desktop.cs.cmu.edu:0.0\"
;;   \"unix:0.0\"
;;   \":0.0\"
;;   \":0\"

;; The first case is usually the result of an SSH tunnel setting the
;; environment variable. This is either enabled by ssh -X or ssh -Y or by
;; your ssh configuration. Further explanation is beyond the scope of
;; this error message.

;; The second case is common in secure local networks with shared NFS
;; file systems, of which there are no longer any instances in the known
;; universe.

;; The last three values will usually be more efficient when you want the
;; Garnet windows to appear on the display of the machine that Garnet is
;; running on.

;;      If you find that this error occurs inexplicably, you can try
;; executing the command \"xhost +\" on the machine where the windows
;; will be displayed, if it is different from the machine running Garnet.
;; This disables security and is not recommended for ordinary use, but it
;; may help in troubleshooting."
;;                       (get-full-display-name))))
;;       (when val
;;         (xlib:close-display val)))
;;     t))

;;(verify-display-can-be-opened)

(defvar *debug-gem-mode*)
(defvar *default-x-display-name*)

;; Moved here from opal:defs.lisp for the sake of modularity. This is
;; sort of the X footprint in opal. So we import the symbols into
;; opal.  This defstruct generates the functions Make-Display-Info,
;; Copy-Display-Info, Display-Info-Display, Display-Info-Screen,
;; Display-Info-Root-Window, Display-Info-Line-Style-GC, and
;; Display-Info-Filling-Style-GC.
(defstruct (display-info (:print-function display-info-printer))
  display
  screen
  root-window
  line-style-gc
  filling-style-gc)

(defun display-info-printer (s stream ignore)
  (format stream "#<GEM-DISPLAY-INFO ~A>" (display-info-display s)))

;;; A graphic context structure
(defstruct (gem-gc (:print-function gem-gc-print-function))
  gcontext
  opal-style                            ; This is either a line or filling style
  function
  foreground
  background
  line-width
  line-style
  cap-style
  join-style
  dashes                                ; do not set to NIL
  font                                  ; do not set to NIL
  fill-style
  fill-rule
  stipple
  clip-mask
  ;; The clip-mask actually stored in the xlib:gcontext -- except if
  ;; the clip-mask is :none, in which case this contains a list like
  ;; '(nil 0 0 0) (to avoid unnecessary consing)
  stored-clip-mask)

(defun gem-gc-print-function (gc stream depth)
  (declare (ignore depth))
  (format stream "#<GEM-GC function ~A clip-mask ~A>"
          (gem-gc-function gc)
          (gem-gc-clip-mask gc)))

(setf (documentation '*default-x-display-name* 'variable)
      "Essentially the X HOSTNAME as string.")

(defvar *default-x-display*)
(defvar *default-x-display-number*)
(defvar *default-x-screen-number*)
(defparameter *default-x-screen* nil)
(defvar *default-x-root*)
(defvar *default-x-colormap*)
(defvar *screen-width*)
(defvar *screen-height*)
(defvar *white*)
(defvar *black*)
(defvar *function-alist*)
(defvar *color-screen-p* nil)

(defvar *read-write-colormap-cells-p*
  "This variable will be t if the screen type is :direct-color
   or :pseudo-color."
  nil)

(defparameter *exposure-event-mask* nil)

(defun x-set-screen-color-attribute-variables (root-window)
  (declare (ignore root-window))
  (let ((color-screen-types '(:pseudo-color
                              :direct-color
                              :static-color
                              :true-color
                              :quickdraw))
        (screen-type (xlib::visual-info-class
                      (xlib::screen-root-visual-info
                       *default-x-screen*))))
    (unless (eq screen-type :true-color)
      (error  (concatenate 'string
	      "Garnett only supports true-color display.  All other "
	      "displays, i.e. :direct-color or :quickdraw, have been "
	      "depreciated")))
    (setq *color-screen-p* :true-color)))

(defun x-color-to-index (root-window a-color)
  (declare (ignore root-window))
  (if *color-screen-p*
      (if a-color (g-value a-color :colormap-index) *white*)
      (if (eq a-color opal::black)      ; XXX this breaks modularity.
          *black*
          *white*)))

;; The following two variables used to be in Inter/i-windows.lisp; they
;; have been moved here because nobody seems to be using them.
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
		    (setq roots-entry (x-build-pixmap *root-window* the-image
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

;;; With-styles works like xlib:with-gcontext except it takes a gob
;;  and extracts all the relevant things for you. This is a win for
;;  the simple draw methods; it will be a lose for performance. See
;;  below.
;;
;;  This is a quick hack to get around the caching of various gcontext
;;  values; it will work until we understand how CLX and the RT X11
;;  server cache gcontexts better.

(defmacro set-gc (gem-gcontext xlib-gcontext slot value)
  (case slot
    (:foreground
     `(let ((v ,value))
	(unless (eq v (gem-gc-foreground ,gem-gcontext))
	  (setf (gem-gc-foreground ,gem-gcontext)
		(setf (xlib:gcontext-foreground ,xlib-gcontext) v)))))
    (:background
     `(let ((v ,value))
	(unless (eq v (gem-gc-background ,gem-gcontext))
	  (setf (gem-gc-background ,gem-gcontext)
		(setf (xlib:gcontext-background ,xlib-gcontext) v)))))
    (:function
     `(let ((v ,value))
	(unless (eq v (gem-gc-function ,gem-gcontext))
	  (setf (gem-gc-function ,gem-gcontext)
		(setf (xlib:gcontext-function ,xlib-gcontext) v)))))
    (:line-width
     `(let ((v ,value))
	(unless (eq v (gem-gc-line-width ,gem-gcontext))
	  (setf (gem-gc-line-width ,gem-gcontext)
		(setf (xlib:gcontext-line-width ,xlib-gcontext) v)))))
    (:line-style
     `(let ((v ,value))
	(unless (eq v (gem-gc-line-style ,gem-gcontext))
	  (setf (gem-gc-line-style ,gem-gcontext)
		(setf (xlib:gcontext-line-style ,xlib-gcontext) v)))))
    (:cap-style
     `(let ((v ,value))
	(unless (eq v (gem-gc-cap-style ,gem-gcontext))
	  (setf (gem-gc-cap-style ,gem-gcontext)
		(setf (xlib:gcontext-cap-style ,xlib-gcontext) v)))))
    (:join-style
     `(let ((v ,value))
	(unless (eq v (gem-gc-join-style ,gem-gcontext))
	  (setf (gem-gc-join-style ,gem-gcontext)
		(setf (xlib:gcontext-join-style ,xlib-gcontext) v)))))
    (:dashes
     `(let ((v ,value))
	(unless (eq v (gem-gc-dashes ,gem-gcontext))
	  (setf (gem-gc-dashes ,gem-gcontext)
		(if v                    ; do not set to NIL
		    (setf (xlib:gcontext-dashes ,xlib-gcontext) v))))))
    (:font
     `(let ((v ,value))
	(unless (eq v (gem-gc-font ,gem-gcontext))
	  (setf (gem-gc-font ,gem-gcontext)
		(if v                    ; do not set to NIL
		    (setf (xlib:gcontext-font ,xlib-gcontext) v))))))
    (:fill-style
     `(let ((v ,value))
	(unless (eq v (gem-gc-fill-style ,gem-gcontext))
	  (setf (gem-gc-fill-style ,gem-gcontext)
		(setf (xlib:gcontext-fill-style ,xlib-gcontext) v)))))
    (:fill-rule
     `(let ((v ,value))
	(unless (eq v (gem-gc-fill-rule ,gem-gcontext))
	  (setf (gem-gc-fill-rule ,gem-gcontext)
		(setf (xlib:gcontext-fill-rule ,xlib-gcontext) v)))))
    (:stipple
     `(let ((v ,value))
	(unless (eq v (gem-gc-stipple ,gem-gcontext))
	  (setf (gem-gc-stipple ,gem-gcontext)
		(if v                    ; do not set to NIL
		    (setf (xlib:gcontext-stipple ,xlib-gcontext) v))))))
    (:clip-mask
     `(let* ((v ,value)
             (s (gem-gc-stored-clip-mask ,gem-gcontext))
             do-copy?)
	(setf (gem-gc-clip-mask ,gem-gcontext) v)
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

(defun set-line-style (line-style gem-gc xlib-gc root-window x-draw-fn)
  (declare (optimize (speed 3) (safety 1)))
  (when line-style
    (let ((draw-fn-changed? (set-gc gem-gc xlib-gc :function x-draw-fn)))
      (unless (eq x-draw-fn boole-2)
        (let ((x-stipple (get-x-stipple line-style root-window))
              x-dash-pattern)

          ;; If the draw-function is :xor and *black* = 0 (for instance
          ;; on HP machines), then we must draw black as white and white
          ;; as black.  But we must check the draw-function first.
          ;; Set-gc returns non-NIL if draw-function changed.
          (when (or draw-fn-changed?
                    (not (eq line-style (gem-gc-opal-style gem-gc))))
            (set-gc gem-gc xlib-gc :foreground
                     (g-value line-style :foreground-color :colormap-index))
            (set-gc gem-gc xlib-gc :background
                     (g-value line-style :background-color :colormap-index)))

          (unless (eq line-style (gem-gc-opal-style gem-gc))
            (setf (gem-gc-opal-style gem-gc) line-style)
            (set-gc gem-gc xlib-gc :line-width
                    (g-value line-style :line-thickness))
            (set-gc gem-gc xlib-gc :line-style
                    (g-value line-style :line-style))
            (set-gc gem-gc xlib-gc :cap-style
                    (g-value line-style :cap-style))
            (set-gc gem-gc xlib-gc :join-style
                    (g-value line-style :join-style))
            (if (setq x-dash-pattern (g-value line-style :dash-pattern))
                (set-gc gem-gc xlib-gc :dashes x-dash-pattern)))

          ;; This can't be in the "unless" since the same
          ;; line-style can have different x-stipples
          (if x-stipple
              (progn
                (set-gc gem-gc xlib-gc :fill-style :opaque-stippled)
                (set-gc gem-gc xlib-gc :stipple x-stipple))
              (set-gc gem-gc xlib-gc :fill-style :solid)))))))

;;; Set Styles Functions (from opal: new-defs.lisp)

;; This is called by with-*-styles, and it replaces the old :x-tiles
;; slot.  It gets the *-style's :stipple, and checks its
;; :root-pixmap-plist slot for an entry for this Root.  If so, it
;; returns it.  Else, it creates the entry and places it at the head
;; of the plist.  These were split into two macros because the draw
;; method for opal:bitmap also needs to use the first macro now...
(defun set-filling-style (filling-style gem-gc xlib-gc root-window x-draw-fn)
  (declare (optimize (speed 3) (safety 1) (space 0) (debug 2)))
  (when filling-style
    (unless (eq x-draw-fn boole-2)
      (let ((x-stipple (get-x-stipple filling-style root-window)))
        ;; Set-gc returns non-NIL if draw-function changed.
        (when (or (set-gc gem-gc xlib-gc :function x-draw-fn)
                  (not (eq filling-style (gem-gc-opal-style gem-gc))))
          (set-gc gem-gc xlib-gc :foreground
                   (g-value filling-style :foreground-color :colormap-index))
          (set-gc gem-gc xlib-gc :background
                   (g-value filling-style :background-color :colormap-index)))

        (unless (eq filling-style (gem-gc-opal-style gem-gc))
          (setf (gem-gc-opal-style gem-gc) filling-style)
          (set-gc gem-gc xlib-gc :fill-style
                  (g-value filling-style :fill-style))
          (set-gc gem-gc xlib-gc :fill-rule
                  (g-value filling-style :fill-rule)))
        (if x-stipple (set-gc gem-gc xlib-gc :stipple x-stipple))))
    (set-gc gem-gc xlib-gc :function x-draw-fn)))

(defun do-all-garnet-windows (clx-window)
  "Iterates over all CLX windows.  Clean-Up calls this function with
the root CLX window."
  (let ((windows (if (member :garnet (xlib:drawable-plist clx-window))
		     (list clx-window))))
    (dolist (w (xlib:query-tree clx-window))
      (setf windows (append windows (do-all-garnet-windows w))))
    windows))

(defun x-all-garnet-windows ()
  "Returns a list of all the X windows that were created by Garnet.
These are raw windows, NOT Opal windows!"
  (do-all-garnet-windows *default-x-root*))

(defun x-beep (root-window)
  (declare (ignore root-window))
  (xlib:bell *default-x-display*)
  (xlib:display-force-output *default-x-display*))

;;; This assumes that the <window> has a gcontext.
(defun x-bit-blit (window source s-x s-y width height destination d-x d-y)
  (xlib:copy-area source
                  (g-value window :buffer-gcontext)
                  s-x s-y width height destination d-x d-y))

(defun x-black-white-pixel (window)
  "Returns: the black and white pixel for the screen of the <window>, as
multiple values."
  (let ((screen (display-info-screen (g-value window :display-info))))
    (values (xlib:screen-black-pixel screen)
            (xlib:screen-white-pixel screen))))

(defun x-character-width (root-window opal-font the-char-code)
  (declare (ignore root-window))
  (xlib:char-width (g-value opal-font :xfont) the-char-code))

;; This variable (formerly in opal/defs.lisp) apparently is to
;; speed up getting the :copy gc op. It's only used in this file.
(defvar *copy*)

(defun x-clear-area (window &optional (x 0) (y 0) width height clear-buffer-p)
  "Clears the visible area associated with a window.  If <clear-buffer-p>,
operate on the window's buffer instead."
  (if clear-buffer-p
      ;; Clear the window's buffer
      (let* ((gc (g-value window :buffer-gcontext))
             (buffer (g-value window :buffer))
             (background (xlib:gcontext-background gc)))
        (xlib:with-gcontext (gc :function *copy* :foreground background)
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

;;; we assume the use of true color.  So colormap-property is a noop.
(defun x-colormap-property (root-window property &optional a b c)
  "Returns various things, depending on which <property> is requested:
:COLOR-LOOKUP -- looks up <a> (a color name) and returns three values,
                 the R-G-B values in the color lookup table.
:MAKE-COLOR   -- creates and returns a color whose three RGB components
                 are given by <a, b, c>"
  (declare (ignore root-window))
  (case property
    (:ALLOC-COLOR
     (xlib:alloc-color *default-x-colormap* a))
    (:MAKE-COLOR
     (xlib:make-color :red a :green b :blue c))
    (:QUERY-COLORS
     ;; Returns three values: red, green, blue components
     (let ((color (car (xlib:query-colors *default-x-colormap* (list a)))))
       (values (floor (* 65535 (xlib:color-red color)))
               (floor (* 65535 (xlib:color-green color)))
               (floor (* 65535 (xlib:color-blue color))))))
    (t
     (error "Unknown property ~S in gem::x-colormap-property~%"
            property))))

(defun x-copy-to-pixmap (root-window to from width height)
  "Copy the cursor or bitmap in <from> to the pixmap <to>.  The operation
affects an area of <width> by <height>."
  (let* ((screen (display-info-screen
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
  (declare (ignore root-window))
  "If <from-font-p> is true, the <source> is a font; otherwise, it is
a pixmap.  Same for the <mask>.  <x> and <y> are a position when the
source is a pixmap; otherwise, they are the cursor-char and the mask-char
for the two fonts."
  (if from-font-p
      (xlib:create-glyph-cursor :source-font source :mask-font mask
                                :source-char x
                                :mask-char y
                                :foreground foreground
                                :background background)
      (xlib:create-cursor :source source :mask mask
                          :x x :y y
                          :foreground (g-value opal::black :xcolor)
                          :background (g-value opal::white :xcolor))))


;; The following deals with cases where the display provides pixmaps
;; with depths different from bits-per-pixel.
(defun get-pixmap-formats ()
  "Return valid pixmap formats for this display."
  (xlib:display-pixmap-formats *default-x-display*))

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


(defun pixarray-element-type (depth)
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
      "gem::x-create-image-array: bits-per-pixel ~S is not valid (1, 4, 8, 16, 24 or 32)"
      depth)
     'xlib::pixarray-8-element-type)))


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
      (let* ((element-type (pixarray-element-type bits-per-pixel))
             (data-array (make-array (list height width)
                                     :element-type element-type
                                     :initial-element
                                     (if color-or-data
                                         (g-value color-or-data :colormap-index)
                                         *white*))))
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
              :element-type (pixarray-element-type depth)))

(defun x-create-pixmap (window width height depth
                        &optional image bitmap-p data-array)
  (declare (ignore data-array))
  (let* ((drawable (g-value window :drawable))
         (pixmap (xlib:create-pixmap :width width
                                     :height height
                                     :depth depth
                                     :drawable drawable)))
    (if image
	(let ((gc (xlib:create-gcontext
		   :drawable pixmap :function boole-1
		   ;; Since this pixmap is going to be used as a stipple mask,
		   ;; you must have 1's in foreground, regardless of whether
		   ;; *black* is 1 on this machine or not (on HP's, it's 0).
		   :foreground 1  ; NOT opal::*black*
		   :background 0  ; NOT opal::*white*
		   )))
	  (xlib:put-image pixmap gc image
			  :x 0 :y 0 :width width :height height
			  :bitmap-p bitmap-p)
	  (xlib:free-gcontext gc)
	  (xlib:set-wm-properties drawable :icon-pixmap pixmap)))
    pixmap))


;; I guess this is some kind of convenience function....
(defun x-build-pixmap (a-window image width height bitmap-p)
  (x-create-pixmap a-window width height 1 image bitmap-p))


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
                    :border (xlib:screen-black-pixel (display-info-screen
                                                      display-info))
                    :override-redirect override-redirect
                    :event-mask *exposure-event-mask*
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
			    ;;                          :client-machine
			    ;;                          (machine-instance)
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
                                 (display-info-display display-info)
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
	   (xlib:event-case (*default-x-display*
			     :discard-p nil :timeout 0)
	     (:motion-notify ((:x x-prime) (:y y-prime)
			      (:event-window win-prime))
			     (setf current-x x-prime)
			     (setf current-y y-prime)
			     (setf current-win win-prime)
			     (if *mouse-debug*
				 (incf *mouse-throw-aways*))
			     t)
	     (t () nil))   ; any other event, return nil (causes
                                        ; event-case to terminate), which causes
                                        ; loop to terminate
	 (return)))
    #+ALLEGRO
    (block throw-away
      (xlib:event-case (*default-x-display*
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
  (xlib:event-case (*default-x-display* :discard-p t :timeout timeout)
    (:destroy-notify () NIL) ; get rid of warnings
    (otherwise () t)))

#+(and cmu mp)
(defun x-discard-pending-events (root-window &optional (timeout 1))
  (declare (ignore root-window timeout))
  (ext:flush-display-events *default-x-display*))



;; These two 2x2x2 arrays are used as a correction to a flaw in xlib:draw-arc
(defparameter *left-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (0 1)) ((0 1) (0 1)))))
(defparameter *top-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (0 0)) ((0 0) (0 1)))))
(defparameter *width-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (0 1)) ((0 1) (0 1)))))
(defparameter *height-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (1 1)) ((1 1) (0 1)))))

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
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let ((filling-style-gc (display-info-line-style-gc display-info)))
	  (set-filling-style
	   fill-style
	   filling-style-gc
	   (gem-gc-gcontext filling-style-gc) root-window function)
	  (xlib:draw-arc drawable (gem-gc-gcontext filling-style-gc)
			 (+ x thickness) (+ y thickness)
			 fill-width fill-height angle1 angle2 T)))
    (if line-style
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc))
	       (half-thickness (truncate thickness 2))
	       (diameter (min width height))
	       (d-mod-2 (mod diameter 2))
	       (t-mod-2 (mod thickness 2)))
	  (set-line-style line-style line-style-gc xlib-gc-line
			  root-window function)
	  (xlib:draw-arc
	   drawable xlib-gc-line
	   (+ x half-thickness
	      (aref *left-adjustment* d-mod-2 d-mod-2 t-mod-2))
	   (+ y half-thickness
	      (aref *top-adjustment* d-mod-2 d-mod-2 t-mod-2))
	   (max 0 (- width thickness
		     (aref *width-adjustment* d-mod-2 d-mod-2 t-mod-2)))
	   (max 0 (- height thickness
		     (aref *height-adjustment* d-mod-2 d-mod-2 t-mod-2)))
	   angle1 angle2 NIL)))))



(defun x-draw-image (window left top width height image function fill-style)
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window))
         (bitmap-p (= (xlib:image-depth image) 1)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let* ((fill-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-fill (gem-gc-gcontext fill-style-gc)))
	  (set-filling-style fill-style fill-style-gc xlib-gc-fill
			     root-window function)
	  (if (and (eq (xlib:gcontext-fill-style xlib-gc-fill) :stippled)
		   bitmap-p)
	      (let ((save-stipple (xlib:gcontext-stipple xlib-gc-fill)))
		(setf (xlib:gcontext-stipple xlib-gc-fill)
		      (x-build-pixmap window image width height bitmap-p))
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
         (root-window (display-info-root-window display-info)))
    ;; Provide the actual drawable of the window if you want to bypass drawing
    ;; into the buffer.  This is used by the gesture-interactor to draw lines
    ;; directly into the window, not the buffer.
    (unless drawable
      (setf drawable (the-drawable window)))
    (setf function (get function :x-draw-function))
    (if line-style
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc)))
	  (set-line-style line-style line-style-gc xlib-gc-line
			  root-window function)
	  (xlib:draw-line drawable xlib-gc-line x1 y1 x2 y2)))))

(defun x-draw-lines (window point-list function line-style fill-style)
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let* ((filling-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-filling (gem-gc-gcontext filling-style-gc)))
	  (set-filling-style
	   fill-style filling-style-gc xlib-gc-filling root-window function)
	  (xlib:draw-lines drawable xlib-gc-filling point-list :fill-p T)))
    (if line-style
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc)))
	  (set-line-style line-style line-style-gc xlib-gc-line
			  root-window function)
	  (xlib:draw-lines drawable xlib-gc-line point-list)))))

(defun x-draw-points (window point-list function line-style)
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window)))
    (let* ((line-style-gc (display-info-line-style-gc display-info))
           (xlib-gc-line (gem-gc-gcontext line-style-gc)))
      (set-line-style line-style
                      line-style-gc xlib-gc-line
                      root-window (get function :x-draw-function))
      (xlib:draw-points drawable xlib-gc-line point-list))))


(defun x-draw-roundtangle (window left top width height
			   x-radius y-radius function
			   line-style fill-style)
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window))
         (th (if line-style (max 1 (g-value line-style :line-thickness)) 0))
         (th\2 (ceiling th 2))
         (th/2 (floor th 2))
         ;; The mnemonic for c-w and c-h is "corner-width" and "corner-height"
         (c-w (+ x-radius x-radius))
         (c-h (+ y-radius y-radius)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let* ((filling-style-gc (display-info-line-style-gc
				  display-info))
	       (gc (gem-gc-gcontext filling-style-gc))
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
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc))
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
  (declare (fixnum x y))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 1)))
  (setf font (g-value font :xfont))
  (setf function (get function :x-draw-function))
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window)))
    (if (and line-style font)
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc)))
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


(defun x-event-handler (root-window ignore-keys)
  (let ((display (the-display root-window)))
    (xlib:event-case
	(display :discard-p t :timeout (if ignore-keys 0 NIL))
      ;; this first one is for when a window is deleted by the wm
      (:CLIENT-MESSAGE
       (event-window type data format)
       (event-handler-debug :CLIENT-MESSAGE event-window type data format)
       (interactors::do-client-message event-window type data format display))
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
       (when (connected-window-p event-window)
	 (interactors::do-exposure (x-window-from-drawable root-window event-window)
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
	       (x-window-from-drawable root-window event-window)
	     x y state code time)))
      (:BUTTON-PRESS
       (event-window x y state code time event-key)
       (setf *last-button-press* time)
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
	   x y state code time event-key))
       (setf *last-button-press* nil))
      (:MOTION-NOTIFY
       (event-window x y)
       (event-handler-debug :MOTION-NOTIFY event-window x y)
       (unless ignore-keys
	 (interactors::do-motion-notify (x-window-from-drawable root-window event-window)
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
      (OTHERWISE
       ()
       (event-handler-debug :UNKNOWN)
       t))))


(defvar *my-root-window*)
(defvar *my-ignore-keys*)

(defvar *composite-events* '((:BUTTON-PRESS :BUTTON-RELEASE)))

(defun proper-subsequence (set-a set-b)
  (and (< (length set-a) (length set-b))
       (reduce (lambda (result item) (and result (eq (car item) (cdr item))))
	       (mapcar #'cons
		       (reverse set-a)
		       (last (reverse set-b) (length set-a)))
	       :initial-value t)))

(defun lookup-composite-event (event-type-list)
  (and (> (length event) 1)
       (dolist (composite-event *composite-events* )
	 (if (and (= (length event-type-list) (length composite-event))
		  (reduce (lambda (result item) (and result (eq (car item) (cdr item))))
			  (mapcar #'cons
				  event-type-list
				  composite-event)
			  :initial-value t))
	     (return t)))))

(defun get-event-type (event)
  (car (reverse event)))


(defun timeout-p (event)
  (eq (get-event-type event) :timeout))

(defun any-more-p (composite-event)
  (reduce (lambda (result event-item)
	    (or result (proper-subsequence composite-event event-item)))
	  *composite-events* :initial-value nil))

(defun valid-event-p (composite-event)
  (or (= 1 (length composite-event))
      (lookup-composite-event composite-event)))


(defun x-event-handler-new (root-window ignore-keys)
  (setf *my-root-window* root-window)
  (setf  *my-ignore-keys* ignore-keys)
  (apply 'process-x-event
	 (let* ((building-composit-event nil)
		(valid-event nil)
		(composite-event nil))
	   (do* ((event (fetch-next-event root-window ignore-keys)
			(fetch-next-event root-window ignore-keys))
		 (composite-event (if event
				      (push (get-event-type event) composite-event)
				      composite-event)
				  (if event
				      (push (get-event-type event) composite-event)
				      composite-event)))
		((or (timeout-p composit-event)
		     (not (any-more-p composite-event)))
		 (if (valid-event-p composite-event)
		     (create-garnet-event composite-event)
		     nil))
	     (format t "event: ~s!%" event)
	     (push event composite-event)
	     )
	   )))

(defun fetch-next-event (root-window ignore-keys)
  (let* ((display (the-display root-window))
	 new-above-sibling
	 new-code
	 new-count
	 new-data
	 new-event-key
	 new-event-window
	 new-format
	 new-height
	 new-state
	 new-time
	 new-type
	 new-width
	 new-x
	 new-y
	 new-event-type)
    (xlib:event-case
	(display :discard-p t :timeout (if ignore-keys 0 NIL))
      ;; this first  one is for when a window is deleted by the wm
      (:CLIENT-MESSAGE
       (event-window type data format)
       (setf new-event-type :CLIENT-MESSAGE
	     new-event-window event-window
	     new-type type
	     new-data data
	     new-format format))
      (:MAP-NOTIFY
       (event-window)
       (setf new-event-type :CLIENT-MESSAGE
	     new-event-window event-window))
      (:UNMAP-NOTIFY
       (event-window)
       (setf new-event-type :UNMAP-NOTIFY
	     new-event-window event-window))
      (:REPARENT-NOTIFY
       (event-window)
       (setf new-event-type :REPARENT-NOTIFY
	     new-event-window event-window))
      (:CIRCULATE-NOTIFY
       ()
       (setf new-event-type :CIRCULATE-NOTIFY))
      (:GRAVITY-NOTIFY
       ()
       (setf new-event-type :GRAVITY-NOTIFY))
      (:DESTROY-NOTIFY
       (event-window)
       (setf new-event-type :DESTROY-NOTIFY
	     new-event-window event-window))
      (:CONFIGURE-NOTIFY
       (event-window x y width height above-sibling)
       (setf new-event-type :CONFIGURE-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-width width
	     new-height height
	     new-above-sibling above-sibling))
      (:EXPOSURE
       (event-window x y width height count)
       (setf new-event-type :EXPOSURE
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-width width
	     new-height height
	     new-count count))
      (:KEY-PRESS
       (event-window x y state code time)
       (setf new-event-type :KEY-PRESS
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-state state
	     new-code code
	     new-time time))
      (:BUTTON-PRESS
       (event-window x y state code time event-key)
       (setf new-event-type :BUTTON-PRESS
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-state state
	     new-code code
	     new-time time
	     new-event-key event-key))
      (:BUTTON-RELEASE
       (event-window x y state code time event-key)
       (setf new-event-type :BUTTON-RELEASE
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-state state
	     new-code code
	     new-time time
	     new-event-key event-key))
      (:MOTION-NOTIFY
       (event-window x y)
       (setf new-event-type :MOTION-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y))
      (:ENTER-NOTIFY
       (event-window x y time)
       (setf new-event-type :ENTER-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-time time))
      (:LEAVE-NOTIFY
       (event-window x y time)
       (setf new-event-type :LEAVE-NOTIFY
	     new-event-window event-window
	     new-x x
	     new-y y
	     new-time time))
      (:NO-EXPOSURE
       ()
       (setf new-event-type :NO-EXPOSURE))
      (OTHERWISE
       ()
       (setf new-event-type :OTHERWISE)))
    (list root-window
	  ignore-keys
	  new-above-sibling
	  new-code
	  new-count
	  new-data
	  new-event-key
	  new-event-window
	  new-format
	  new-height
	  new-state
	  new-time
	  new-type
	  new-width
	  new-x
	  new-y
	  new-event-type)))



(defun process-x-event (root-window
			ignore-keys
			above-sibling
			code
			count
			data
			event-key
			event-window
			format
			height
			state
			time
			my-type
			width
			x
			y
			event-type)
  (let ((display (the-display root-window)))
    ;; if the event is a release, wait 100 milliseconds to see if the
    ;; next character
    (case event-type
      (:CLIENT-MESSAGE
       (event-handler-debug :CLIENT-MESSAGE event-window my-type data format)
       (interactors::do-client-message event-window my-type data format display))
      (:MAP-NOTIFY
       (event-handler-debug :MAP-NOTIFY)
       (interactors::do-map-notify (x-window-from-drawable root-window
							   event-window)))
      (:UNMAP-NOTIFY
       (event-handler-debug :UNMAP-NOTIFY)
       (interactors::do-unmap-notify (x-window-from-drawable root-window
							     event-window)))
      (:REPARENT-NOTIFY
       (event-handler-debug :REPARENT-NOTIFY)
       (if (connected-window-p event-window)
	   (let ((window (x-window-from-drawable root-window event-window)))
	     (s-value window :already-initialized-border-widths nil)
	     (s-value window :lineage (lineage-of-drawable event-window)))))
      (:CIRCULATE-NOTIFY
       (event-handler-debug :CIRCULATE-NOTIFY)
       (interactors::do-circulate-notify))
      (:GRAVITY-NOTIFY
       (event-handler-debug :GRAVITY-NOTIFY)
       (interactors::do-gravity-notify))
      (:DESTROY-NOTIFY
       (event-handler-debug :DESTROY-NOTIFY)
       (destroy-notify-window event-window))
      (:CONFIGURE-NOTIFY
       (event-handler-debug :CONFIGURE-NOTIFY)
       (if (connected-window-p event-window)
	   (interactors::do-configure-notify (x-window-from-drawable root-window
								     event-window)
	     x y width height above-sibling)))
      (:EXPOSURE
       (event-handler-debug :EXPOSURE x y width height count)
       (when (connected-window-p event-window)
	 (interactors::do-exposure (x-window-from-drawable root-window event-window)
	   x y width height count display)))
      (:KEY-PRESS
       (event-handler-debug :KEY-PRESS event-window x y state code time)
       (if ignore-keys
	   ;; We don't want keys, but check if this is the abort key
	   (let ((c (x-translate-character *root-window* 0 0 state code 0)))
	     (when (eq c interactors::*garnet-break-key*)
	       (format T "~%**Aborting transcript due to user command**~%")
	       (return-from process-x-event :abort)))
	   ;; Normal case: we do want keys
	   (interactors::do-key-press
	       (x-window-from-drawable root-window event-window)
	     x y state code time)))
      (:BUTTON-PRESS
       (setf *last-button-press* time)
       (event-handler-debug :BUTTON-PRESS event-window x y state code time
			    event-key)
       (unless ignore-keys
	 (interactors::do-button-press (x-window-from-drawable root-window
							       event-window)
	   x y state code time event-key)))
      (:BUTTON-RELEASE
       (event-handler-debug :BUTTON-RELEASE event-window x y state code time
			    event-key)
       (unless ignore-keys
	 (interactors::do-button-release (x-window-from-drawable root-window
								 event-window)
	   x y state code time event-key))
       (setf *last-button-press* nil))
      (:MOTION-NOTIFY
       (event-handler-debug :MOTION-NOTIFY event-window x y)
       (unless ignore-keys
	 (interactors::do-motion-notify (x-window-from-drawable root-window event-window)
	   x y display)))
      (:ENTER-NOTIFY
       (event-handler-debug :ENTER-NOTIFY event-window x y time)
       (unless ignore-keys
	 (interactors::do-enter-notify (x-window-from-drawable root-window
							       event-window)
	   x y time)))
      (:LEAVE-NOTIFY
       (event-handler-debug :LEAVE-NOTIFY event-window x y time)
       (unless ignore-keys
	 (interactors::do-leave-notify (x-window-from-drawable root-window
							       event-window)
	   x y time)))
      (:NO-EXPOSURE
       (event-handler-debug :NO-EXPOSURE)
       (unless ignore-keys
	 t))
      (:OTHERWISE
       (event-handler-debug :UNKNOWN)
       t))))

(defun x-flush-output (window)
  ;;(xlib:display-force-output (the-display window)))
  (xlib:display-finish-output (the-display window)))


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

(defparameter dpi 151)
;;; (defparameter scale-factor (/ dpi 96))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter scale-factor 1))

(defvar *Fixed-Font-Family*      "courier")
(defvar *Serif-Font-Family*      "times")
(defvar *Sans-Serif-Font-Family* "helvetica")

(defvar *Small-Font-Size*      (ceiling (* scale-factor 10)))
(defvar *Medium-Font-Size*     (ceiling (* scale-factor 12)))
(defvar *Large-Font-Size*      (ceiling (* scale-factor 18)))
(defvar *Very-Large-Font-Size* (ceiling (* scale-factor 24)))

(defvar *Small-Font-Point-Size*      (ceiling (* scale-factor 100)))
(defvar *Medium-Font-Point-Size*     (ceiling (* scale-factor 120)))
(defvar *Large-Font-Point-Size*      (ceiling (* scale-factor 180)))
(defvar *Very-Large-Font-Point-Size* (ceiling (* scale-factor 240)))


;; Returns either a string which describes the font using X conventions,
;; or a cons of the bad value and slot.
(defun x-make-font-name (root-window key)
  (declare (ignore root-window))
  (let ((family-part
	 (case (first key)
	   (:fixed      *Fixed-Font-Family*)
	   (:serif      *Serif-Font-Family*)
	   (:sans-serif *Sans-Serif-Font-Family*)
	   (otherwise   nil)))
        (face-part
         (let ((face-spec (if (consp (second key))
                              (second key)
                              (list (second key)))))
           (if (subsetp face-spec *x-font-faces*)
               face-spec)))
        (size-part
	 (case (third key)
	   (:small      (princ-to-string *Small-Font-Point-Size*))
	   (:medium     (princ-to-string *Medium-Font-Point-Size*))
	   (:large      (princ-to-string *Large-Font-Point-Size*))
	   (:very-large (princ-to-string *Very-Large-Font-Point-Size*))
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



;; This exists expressly to convert paths using CMU's
;; ext:search-list keys into normal paths.  Not robust, but better than
;; what used to be done...
(defun fix-font-path (path-argument)
  (when path-argument
    (let* ((path path-argument)
           (colon-posn (position #\: path))
           (search-path (when colon-posn
                          #+cmu (ext:search-list
                                 (subseq path 0 (1+ colon-posn)))
                          #-cmu nil)))
      (if search-path
          (concatenate 'string (car search-path) (subseq path (1+ colon-posn)))
          (if (eq (position #\/ path :from-end t) (1- (length path)))
              path
              (concatenate 'string path "/"))))))

;; Hack used in font-to-xfont to counteract ridiculous tendency
;; of CLX to tack on #\null characters at the end of font paths.
(declaim (inline remove-null-char))
(defun remove-null-char (s)
  (remove #\null s :start (1- (length s))))


(defun x-font-exists-p (root-window name)
  (declare (ignore root-window))
  (xlib:list-font-names *default-x-display* name))

(defun x-font-to-internal (root-window font-from-file)
  (let ((dx-plist (g-value font-from-file :display-xfont-plist))
        (display (the-display root-window)))
    (or (getf dx-plist display)
        (let ((font-path (fix-font-path
                          (g-value font-from-file :font-path)))
              (font-name (g-value font-from-file :font-name)))
          (when font-path
            (let ((xfont-path (mapcar #'remove-null-char
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
                (x-font-to-internal root-window default-font-from-file)))))))



;;; Sets the Cut buffer for X.  Note that this does NOT do a select, and
;;; therefore the cut buffer will not be affected if there already is a
;;; selection in some xterm window.
;;;
(defun x-get-cut-buffer (window)
  (xlib:cut-buffer
   (display-info-display (g-value window :display-info))))



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

(defun get-descriptor (index)
  (case index
    (0 '(#*0000 #*0000 #*0000 #*0000))
    (1 '(#*1000 #*0000 #*0000 #*0000))
    (2 '(#*1000 #*0000 #*0010 #*0000))
    (3 '(#*1010 #*0000 #*0010 #*0000))
    (4 '(#*1010 #*0000 #*1010 #*0000))
    (5 '(#*1010 #*0100 #*1010 #*0000))
    (6 '(#*1010 #*0100 #*1010 #*0001))
    (7 '(#*1010 #*0101 #*1010 #*0001))
    (8 '(#*1010 #*0101 #*1010 #*0101))
    (9 '(#*1010 #*0101 #*1010 #*0111))
    (10 '(#*1010 #*1101 #*1010 #*0111))
    (11 '(#*1010 #*1101 #*1010 #*1111))
    (12 '(#*1010 #*1111 #*1010 #*1111))
    (13 '(#*1010 #*1111 #*1011 #*1111))
    (14 '(#*1110 #*1111 #*1011 #*1111))
    (15 '(#*1110 #*1111 #*1111 #*1111))
    (16 '(#*1111 #*1111 #*1111 #*1111))))


(defun x-device-image (root-window index)
  (declare (ignore root-window))
  (let ((descriptor (get-descriptor index)))
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
          (xlib:create-gcontext :drawable *default-x-root*
                                :cache-p t
                                :function 2
                                :foreground *black*
                                :background *white*
                                :line-width 0
                                :line-style :solid
                                :cap-style :butt
                                :join-style :miter
                                :fill-style :solid
                                :fill-rule :even-odd))
         (x-filling-style-gc
          (xlib:create-gcontext :drawable *default-x-root*
                                :cache-p t
                                :function 2
                                :foreground *black*
                                :background *white*
                                :line-width 0
                                :line-style :solid
                                :cap-style :butt
                                :join-style :miter
                                :fill-style :solid
                                :fill-rule :even-odd))
         (gem-line-style-gc
          (make-gem-gc  :gcontext x-line-style-gc
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
         (gem-filling-style-gc
          (make-gem-gc  :gcontext x-filling-style-gc
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

    (make-display-info :display *default-x-display*
                       :screen  *default-x-screen*
                       :root-window *default-x-root*
                       :line-style-gc gem-line-style-gc
                       :filling-style-gc gem-filling-style-gc)))



;;; Set the border widths of the <window>.  This is quite complex, because of
;;; the differences among various window systems.
;;;
(defun x-initialize-window-borders (window drawable)
  ;; find out what borders really are
  (if (g-value window :parent);; window is really subwindow
      (set-four-borders window (xlib:drawable-border-width drawable))
      (let ((lineage (g-value window :lineage)))
	(case (length lineage)
	  (2                              ; UWM or window without title
	   (set-four-borders window (xlib:drawable-border-width drawable)))
	  (3                              ; TWM
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
	  ((4 6)                          ; MWM and DECWindows, or possibly TVTWM
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
		       (MAX (xlib:drawable-x parent)              ; MWM
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




(defparameter *update-lock*
  (bordeaux-threads:make-recursive-lock "UPDATE-LOCK"))

;;; Does a map-window, and then waits for it to actually appear on the
;;; screen.  The waiting is necessary, because otherwise objects in
;;; the window won't appear in Lucid and Allegro (due to some race
;;; condition).
(defun x-map-and-wait (a-window drawable)
  (let ((display (the-display a-window)))
    (when (eq (xlib:window-map-state drawable) :unmapped)
      (bordeaux-threads:with-recursive-lock-held (*update-lock*)
        (xlib:map-window drawable)
        (xlib:display-force-output display))
      (loop
         (if (eq (xlib:window-map-state drawable) :unmapped)
             (sleep .1)
             (return t))))))

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
	  (xlib:change-active-pointer-grab *default-x-display*
					   (if want-enter-leave
					       *enter-leave-report-motion-pem*
					       *report-motion-pem*))
	  (xlib:grab-pointer *default-x-display*
			     (if want-enter-leave
				 *enter-leave-report-motion-pem*
				 *report-motion-pem*)
			     :owner-p owner-p))
      ;; Mouse ungrab.
      (xlib:ungrab-pointer *default-x-display*)))




;;; Move the <window> to the top (if <raise-p>) or to the bottom.
;;;
(defun x-raise-or-lower (window raise-p)
  #+(and)
  (setf (xlib:window-priority (g-value window :drawable))
        (if raise-p :above :below))
  #-(and)
  (if raise-p
      (xlib:circulate-window-up (g-value window :drawable))
      (xlib:circulate-window-down (g-value window :drawable)))
  )


(defun x-read-an-image (root-window pathname)
  (declare (ignore root-window))
  (xlib:read-bitmap-file pathname))



;;; Reparent a window.
(defun x-reparent (window new-parent drawable left top)
  (if new-parent
      (if (is-a-p new-parent opal::window)
	  (xlib:reparent-window drawable
				(g-value new-parent :drawable)
				left top)
	  (error "Parent ~S of window ~S is not of type window~%"
		 new-parent window))
      (xlib:reparent-window drawable
			    (display-info-root-window
			     (g-value window :display-info))
			    left top)))

(defun x-set-clip-mask (a-window clip-mask &optional lstyle-ogc fstyle-ogc)
  (declare (ignore a-window))
  (let ((lstyle-xgc (gem-gc-gcontext lstyle-ogc))
        (fstyle-xgc (gem-gc-gcontext fstyle-ogc)))
    (set-gc lstyle-ogc lstyle-xgc :clip-mask clip-mask)
    (set-gc fstyle-ogc fstyle-xgc :clip-mask clip-mask)))

(defun x-set-cut-buffer (window string)
  (setf (xlib:cut-buffer
         (display-info-display (g-value window :display-info)))
        string))

;; ;;; Routines used to get name of display, and extract
;; ;;  display number and screen number.
;; ;;  Normally, the name of a display is of the form
;; ;;  "displayname:displaynumber.screennumber"
;; (defun get-full-display-name ()
;;   (sb-posix:getenv "DISPLAY"))

;; FMG Rewrote the following three functions just to make
;; them lispier.
(defun get-display-name (display)
  "This function takes a full display name and, somewhat misleadingly,
   returns the HOST name, stripping off the display number."
  (subseq display 0 (position #\: display :test #'char=)))

(defun get-display-number (display)
  ;; The display number is everything from the colon to the period (if
  ;; it is present).
  (let* ((colon-pos (position #\: display :from-end t))
         (period-pos (and colon-pos (position #\. display :start colon-pos))))
    (unless colon-pos
      (error "The display specification  \"~A\" is ill-formed: missing colon" display))
    (let ((display-number
           (ignore-errors
             (parse-integer
              (subseq display (1+ colon-pos) period-pos)))))
      (unless (numberp display-number)
        (error "The display specification  \"~A\" is invalid: bad display number" display))
      display-number)))

(defun get-screen-number (display-name)
  (let*  ((colon-pos (position #\: display-name :from-end t))
          (dot (and colon-pos (position #\. display-name :start colon-pos))))
    (if dot
        (or (parse-integer (subseq display-name (1+ dot)) :junk-allowed t) 0)
        0)))


(defun x-set-device-variables (full-display-name)
  (setf *default-x-display-number*
        (if full-display-name
            (get-display-number full-display-name)
            0))
  (setq *default-x-display-name*
        (if full-display-name (get-display-name full-display-name) ""))
  (setq *default-x-screen-number* (get-screen-number full-display-name))
  (setq *default-x-display*
        (xlib:open-default-display))
  (setq *default-x-screen*
        (nth *default-x-screen-number*
             (xlib:display-roots *default-x-display*)))
  (setq *screen-width* (xlib:screen-width *default-x-screen*))
  (setq *screen-height* (xlib:screen-height *default-x-screen*))
  (setq *default-x-root* (xlib:screen-root *default-x-screen*))
  (setq *default-x-colormap*
        (xlib:screen-default-colormap
         (nth *default-x-screen-number*
              (xlib:display-roots
               (xlib:open-default-display)))))
  (setq *white* (xlib:screen-white-pixel *default-x-screen*))
  (setq *black* (xlib:screen-black-pixel *default-x-screen*))
  ;; Added :button-press and :key-press so garnet-debug:ident will work.
  (setf *exposure-event-mask*
        (xlib:make-event-mask :exposure :structure-notify
                              :button-press :key-press)))

;;; Sets the pointer from a raw X <drawable> (a drawable or pixmap) to
;;; the Opal <window>.
(defun x-set-drawable-to-window (window drawable)
  (if (xlib:pixmap-p drawable)
      (setf (xlib:pixmap-plist drawable) (list :garnet window))
      (setf (xlib:window-plist drawable) (list :garnet window))))



(defun x-set-draw-functions (root-window)
  "Create Alist since CLX likes to get the draw function in the form of an
integer.  We want to specify nice keywords instead of those silly
 numbers."
  (gem:set-draw-function-alist root-window)
  (dolist (fn-pair *function-alist*)
    (setf (get (car fn-pair) :x-draw-function) (cdr fn-pair))))

(defun x-set-draw-function-alist (root-window)
  (declare (ignore root-window))
  (setq *function-alist*
               `((:clear . ,boole-clr)      ; (color, *white* = 0)
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
  ;; For erasing buffers
  (setq *copy* (cdr (assoc :copy *function-alist*))))


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
;;;      #+CMU (xlib:display-force-output *default-x-display*)
	   #-CMU NIL

	   ;; Need to force-output when using the background m-e-l
	   ;; process,  otherwise this doesn't get noticed.
	   (xlib:display-force-output *default-x-display*))))
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
         (2                             ; UWM or window without label.
          (s-value window :left x)
          (s-value window :top y))
         (3                             ; TWM
          (s-value window :left (xlib:drawable-x (second lineage)))
          (s-value window :top (xlib:drawable-y (second lineage))))
         ((4 6)                         ; MWM and DECWindows, or possibly TVTWM
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
	   (xlib:reparent-window drawable (display-info-root-window
					   (g-value window :display-info))
				 left top)))
     nil)
    (:POINTER-POSITION
     ;; Warps the pointer to the position expressed by the <value>
     (xlib:warp-pointer (g-value window :drawable) (car value) (cdr value))
     (xlib:display-force-output *default-x-display*))
    (:REPORT-ASYNCHRONOUS-ERRORS
     (setf (xlib:display-report-asynchronous-errors
            *default-x-display*)
           value))
    (:SAVE-UNDER
     (setf (xlib:window-save-under (g-value window :drawable)) value)
     nil)
    (:SUBWINDOW-MODE
     (let ((display-info (g-value window :display-info)))
       (setf (xlib:gcontext-subwindow-mode
              (gem-gc-gcontext
               (display-info-line-style-gc display-info)))
             value)
       (setf (xlib:gcontext-subwindow-mode
              (gem-gc-gcontext
               (display-info-filling-style-gc display-info)))
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
              (xlib:iconify-window drawable *default-x-screen*))
             ((eq vis nil)
              (xlib:withdraw-window drawable *default-x-screen*)))
       ;; Does the window need to be mapped?
       map-window))
    (:WIDTH
     (let ((old-buffer (g-value window :buffer)))
       (setf (xlib:drawable-width (g-value window :drawable))
             (max 0 value))

       ;; Does the buffer need to be recreated?
       (and old-buffer (> value (xlib:drawable-width old-buffer)))))
    (T
     (format t "Unknown property ~S in gem:set-window-property.~%"
             property))))



;;; RETURNS: T if the filling style of the given display is stippled
;;;
(defun x-stippled-p (root-window)
  (eq (xlib:gcontext-fill-style
       (gem-gc-gcontext
        (display-info-filling-style-gc
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


;;; Creates a state mask for keyboard events.
;;;
(defun x-create-state-mask (root-window modifier)
  (declare (ignore root-window))
  (xlib:make-state-mask modifier))


(defun x-translate-code (window scan-code shiftp)
  "Translates a keyboard scan"
  (xlib:keycode->keysym
   (display-info-display
    (the DISPLAY-INFO (g-value window :display-info)))
   scan-code (if shiftp 1 0)))


(defun x-translate-coordinates (root-window window1 x y &optional window2)
  "RETURNS: the coordinates of point <x,y> in the <window> relative to
the <window2>, or to the screen's origin if <window2> is not specified.
Returns multiple values."
  (declare (ignore root-window))
  (declare (fixnum x y))
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
  (declare (fixnum width height))
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
  ;;  (attach-method x-device :event-handler #'x-event-handler-new)
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
  (attach-method x-device :set-screen-color-attribute-variables #'x-set-screen-color-attribute-variables)
  (attach-method x-device :set-draw-function-alist #'x-set-draw-function-alist)
  (attach-method x-device :set-draw-functions #'x-set-draw-functions)
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


;;; This is also called in reconnect-garnet.
(defun initialize-device-values (full-display-name root-window)
  ;; Set up all the GEM variables used to identify display, screen,
  ;; etc. These are needed by discard-all-pending-events (in
  ;; process.lisp), which is called by launch-main-event-loop-process.
  (x-set-device-variables full-display-name)
  (gem:set-screen-color-attribute-variables root-window))

;;; Make the initializer function available to the outside world.
;; (setf (get :garnet-modules :gem) t)

(defmacro event-handler-debug (message &rest args)
  `(format t "event-handler ~S   ~S~%" ,message ',args))

(defun x-draw-rectangle (window left top width height function
                         line-style fill-style)
  (declare (fixnum left top width height))
  (if (< width 1)
      (setf width 1))
  (if (< height 1)
      (setf height 1))
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window))
         (thickness (if line-style
                        (max (g-value line-style :line-thickness) 1) 0)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let* ((filling-style-gc (display-info-line-style-gc display-info))
	       (gc (gem-gc-gcontext filling-style-gc))
	       (th2 (* 2 thickness)))
	  (set-filling-style fill-style filling-style-gc gc
			     root-window function)
	  (xlib:draw-rectangle drawable gc
			       (+ left thickness) (+ top thickness)
			       (- width th2) (- height th2)
			       t)))
    (if line-style
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc))
	       (half-thickness (truncate thickness 2)))
	  (set-line-style line-style line-style-gc xlib-gc-line
			  root-window function)
	  (xlib:draw-rectangle drawable xlib-gc-line
			       (+ left half-thickness)
			       (+ top half-thickness)
			       (- width thickness)
			       (- height thickness) NIL)))))
