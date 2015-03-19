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

(in-package "OPAL")

;;; Windows

;;; Class Window 
;;; To create a window for displaying gobs, create a schema which is an
;;; instance of the window class described below specifying slots as
;;; needed. For example:
;;; 
;;; (create-instance my-window opal:window
;;;   (:width 100)
;;;   (:height 100))
;;; 

(define-method :point-in-gob window (gob x y)
  (declare (fixnum x y))
  (and (<= 0 x (g-value gob :width))
       (<= 0 y (g-value gob :height))))

;;; A couple routines useful for windows with backing store

;;; Create a buffer the same size as drawable.
(defun create-buffer (a-window)
  (let ((pixmap (gem:create-pixmap a-window
				   (g-value a-window :width)
				   (g-value a-window :height)
				   (gem:window-depth a-window))))
    ;; We need to associate a window pointer with the pixmap, in order to
    ;; be able to find the method.
    (gem:set-drawable-to-window a-window pixmap)
    pixmap))


;;; Initalize the buffer to be background color.
(defun clear-buffer (a-window)
  (gem:clear-area a-window nil nil nil nil T))


;;; Create new larger buffer.
(defun expand-buffer (a-window)
  (gem:delete-pixmap a-window (g-value a-window :buffer))
  (s-value a-window :buffer (create-buffer a-window)))


;;; Map-notify is called when a :map-notify event occurs.
;;; It always sets the :visible slot of the window to T.
(defun Map-Notify (event-debug a-window)
  (when event-debug (format t "map-notify ~S~%"
			    a-window
			    ;; DZG (xlib:window-id event-window)
			    ))
  (if a-window
    (kr:with-demon-disabled  (g-value window :invalidate-demon)
      (s-value a-window :visible t)))
  t)

(defvar *exit-main-event-loop-function* NIL)

;; returns t if any top level window is visible
(defun any-top-level-window-visible ()
  (some #'(lambda (win)
	    (and (schema-p win)
		 (g-value win :visible)
		 (not (g-value win :parent))))
	*garnet-windows*))
  #|
  (maphash #'(lambda (xwin win)
		(declare (ignore xwin))
		(when (and (schema-p win)
			   (g-value win :visible)
			   (not (g-value win :parent)))
		  (return-from any-top-level-window-visible t)))
	    *drawable-to-window-mapping*)
  |#


;;; Unmap-notify is called when an :unmap-notify event occurs.
;;; It sets the :visible slot of the unmapped window as follows,
;;;           T  -->  :iconified      (invoked after the window is iconified
;;;                                    by hand)
;;;  :iconified  -->  :iconified      (invoked after opal:iconify-window)
;;;         NIL  -->  NIL             (invoked after the window is made
;;;                                    invisible)
;;; If no more window is visible and we are in a main-event-loop,
;;; then leave that event-loop.
(defun Unmap-Notify (event-debug a-window)
  (when event-debug (format t "unmap-notify ~S~%"
			    a-window
			    ;; DZG (xlib:window-id event-window)
			    ))
  (when (schema-p a-window)
    (if (g-value a-window :visible)
      (with-demon-disabled (g-value WINDOW :invalidate-demon)
	(s-value a-window :visible :iconified)))
    (if (and *exit-main-event-loop-function*
	     (not (any-top-level-window-visible)))
      (funcall *exit-main-event-loop-function*)))
  t)


(defun Circulate-Notify (event-debug)
  (when event-debug (format t "circulate-notify~%"))
  t)


(defun Gravity-Notify (event-debug)
  (when event-debug (format t "gravity-notify~%"))
  t)

(defun iconify-window (a-window)
  (let ((drawable (g-value a-window :drawable)))
    (when drawable
      (s-value a-window :visible :iconified)
      (gem:set-window-property a-window :visible :iconified)
      (gem:flush-output a-window))))


(defun deiconify-window (a-window)
  (s-value a-window :visible t)
  (update a-window))


(defun raise-window (a-window)
  (cond ((is-a-p a-window window)
	 (let ((drawable (g-value a-window :drawable)))
	   (when drawable
	     (gem:raise-or-lower a-window T))
	   ;; if drawable was NIL, window will appear on top anyway.
	   (update a-window)
	   ;; if there were no invalid objects in the window, update wouldn't
	   ;; have called display-force-output
	   (gem:flush-output a-window)))
	(t (error "~S is not a real Opal window~%" a-window))))

(defun lower-window (a-window)
  (cond
    ((is-a-p a-window window)
     (let ((drawable (g-value a-window :drawable)))
       (unless drawable
	 (setq drawable (create-drawable a-window)))
       (gem:raise-or-lower a-window NIL)
       (update a-window)
       ;; if there were no invalid objects in the window, update wouldn't
       ;; have called display-force-output
       (gem:flush-output a-window)))
    (t (error "~S is not a real Opal window~%" a-window))))


#|
  Zoom operation:
	If :zoomdims are NIL, then
		store the window's dims in :zoomdims, and
		zoom the window
	else
		restore the window's dims to :zoomdims, and
		clear :zoomdims
|#

(defun zoom-window (a-window &optional fullzoom?)
  (when (is-a-p a-window window)
    (let ((zoomdims (g-local-value a-window :zoomdims)))
      (if zoomdims
	(progn
	  (s-value a-window :zoomdims NIL)
	  (s-value a-window :top    (aref zoomdims 0))
	  (s-value a-window :left   (aref zoomdims 1))
	  (s-value a-window :width  (aref zoomdims 2))
	  (s-value a-window :height (aref zoomdims 3)))

	;;else no zoomdims, so store zoomdims and zoom!
	(let ((top    (g-value a-window :top   ))
	      (left   (g-value a-window :left  ))
	      (width  (g-value a-window :width ))
	      (height (g-value a-window :height)))
	  (s-value a-window :zoomdims
		   (make-array 4
			       :initial-contents (list top left width height)))
	  (s-value a-window :top 0)
	  (s-value a-window :height gem:*screen-height*)
	  (when fullzoom?
	    (s-value a-window :left 0)
	    (s-value a-window :width gem:*screen-width*))))
      (update a-window))))


(defun fullzoom-window (a-window) (zoom-window a-window T))

(defun convert-coordinates (win1 x y &optional win2)
  (declare (fixnum x y))
  (multiple-value-bind (xc yc)
      (gem:translate-coordinates (if win1 win1 (g-value gem:DEVICE-INFO :current-root))
                                 win1 x y win2)
    (if xc
      ;; Low-level conversion is better.
      (values xc yc)
      ;; If not, do it purely in Opal terms.
      (let ((left1 (the fixnum (if win1 (g-value-fixnum win1 :left) 0)))
	    (top1  (the fixnum (if win1 (g-value-fixnum win1 :top) 0)))
	    (left2 (the fixnum (if win2 (g-value-fixnum win2 :left) 0)))
	    (top2  (the fixnum (if win2 (g-value-fixnum win2 :top) 0))))
	(declare (fixnum left1 top1 left2 top2))
	(values (- (+ x left1) left2)
		(- (+ y top1)  top2))))))


(defun simple-initialize-window-border-widths (a-window border-width)
  (s-value a-window :left-border-width border-width)
  (s-value a-window :top-border-width border-width)
  (s-value a-window :right-border-width border-width)
  (s-value a-window :bottom-border-width border-width))


(defun initialize-window-border-widths (a-window drawable)
  (gem:initialize-window-borders a-window drawable))


(defun Configure-Notify (event-debug x y width height a-window
				     above-sibling)
  (if event-debug
    (format t "Configure-notify win=~s ~s ~s ~s ~s ~s~%"
	    a-window
	    ;; DZG (xlib:window-id event-window)
	    x y
	    width height above-sibling))
  (if a-window
    (let* ((drawable (g-value a-window :drawable))
	   (event-window drawable))
      (when (g-value a-window :visible)
	;; szekely: added test for window being visible.  This
	;; eliminates the problem of the configure notify that olwm
	;; sends before mapping a window.
	(if (g-value a-window :parent)
	  (progn			; If it's a subwindow, we don't
	    (s-value a-window :left x)  ; have to check lineage.
	    (s-value a-window :top y))

	  (unless (eq (g-value a-window :visible) :iconified)
	    (gem:set-window-property a-window :EVENT-POSITION
				     (list x y event-window))))
	(unless (or (g-value a-window :already-initialized-border-widths)
		    (eq (g-value a-window :visible) :iconified))
	  (initialize-window-border-widths a-window event-window)
	  (s-value a-window :already-initialized-border-widths t))
	(s-value a-window :width width)
	(s-value a-window :height height)
	;; Don't want top, left, width, height to be invalid,
	;; or else we might get a drifting window.
	(let ((win-info (g-value a-window :win-update-info)))
	  (setf (win-update-info-invalid-slots win-info)
		(set-difference (win-update-info-invalid-slots win-info)
				'(:left :top :width :height))))
	(when (gem:window-has-grown a-window width height)
	  (expand-buffer a-window)
	  ;; This update will redraw contents of window into new buffer.
	  (update a-window t))
	(update-all))))
  t)


(defun Exposure (event-debug a-window count x y width height display)
  (declare (ignore display))
  (declare (fixnum count x y width height))
  (when event-debug
    (format t "exposure, count = ~S window-id=~s~%"
	    count (gem:window-debug-id a-window)))
  (if (schema-p a-window)
    (let ((drawable (g-value a-window :drawable)))
      (if drawable
	;; Do not update the window in the case where the window
	;; was just created and mapped for the first time
	(if (g-local-value a-window :very-first-exposure)
	  (when (zerop count)
	    (kr:destroy-slot a-window :very-first-exposure))
	  (let* ((win-ui (get-local-value a-window :win-update-info))
		 (exposed-bbox (win-update-info-exposed-bbox win-ui)))
	    (unless exposed-bbox
	      (setq exposed-bbox
		    (setf (win-update-info-exposed-bbox win-ui)
			  (make-bbox :valid-p NIL))))
	    (if (bbox-valid-p exposed-bbox)
		;; already valid, so merge into existing bbox
		(setf (bbox-x2 exposed-bbox)
		      (max (+ x width) (bbox-x2 exposed-bbox))
		      (bbox-y2 exposed-bbox)
		      (max (+ y height) (bbox-y2 exposed-bbox))
		      (bbox-x1 exposed-bbox)
		      (min x (bbox-x1 exposed-bbox))
		      (bbox-y1 exposed-bbox)
		      (min y (bbox-y1 exposed-bbox)))
		;; invalid, so copy in and make valid
		(setf (bbox-valid-p exposed-bbox) T
		      (bbox-x2 exposed-bbox) (+ x width)
		      (bbox-y2 exposed-bbox) (+ y height)
		      (bbox-x1 exposed-bbox) x
		      (bbox-y1 exposed-bbox) y))
	    (when (zerop count)
	      (s-value a-window :exposed-bbox exposed-bbox)
	      (kr-send a-window :update a-window t)
	      (s-value a-window :exposed-bbox nil)
	      (setf (bbox-valid-p exposed-bbox) NIL)))))))
  t)

(defun initialize-display (root-window)
  (unless diamond-fill
    ;; Moved here from halftones.lisp.  Used to be done at load time.
    (setf diamond-fill
	  (make-filling-style '((1 1 1 1 1 1 1 1 1)
				(1 1 1 1 0 1 1 1 1)
				(1 1 1 0 0 0 1 1 1)
				(1 1 0 0 0 0 0 1 1)
				(1 0 0 0 0 0 0 0 1)
				(1 1 0 0 0 0 0 1 1)
				(1 1 1 0 0 0 1 1 1)
				(1 1 1 1 0 1 1 1 1)
				(1 1 1 1 1 1 1 1 1))
			      :root-window root-window)))
  (unless *halftone-table*
    ;;; This used to be done by a DefVar, but now the DefVars all occur at
    ;; the start of loading Opal, before the function is defined, so we must
    ;; Setf it here...
    (setf *halftone-table* (build-halftone-table root-window))
    (install-bitmap-images))
  (gem:initialize-device root-window))


;; The PAIR argument can either be
;; 1) A dotted pair of two bitmaps: an image and a mask
;; 2) A three-element list consisting of a font and two indices into the font
;;    indicating a cursor character and its mask
;; 3) Same as #2 only with :CURSOR as the first element instead of a font.
;;    In this case, CURSOR-FONT is used as the font.
;;
(defun set-window-cursor (a-window drawable pair)
  (declare (ignore drawable))

  ;; First, look up the bitmap pair in the association list of known cursors
  (let ((old-cursor (cdr (assoc pair (g-value a-window :cursor-pairs)
				:test #'equal))))
    (if old-cursor
      (gem:set-window-property a-window :CURSOR old-cursor)

      ;; Haven't seen this bitmap pair before in this window.  Create
      ;; a new cursor pixmap, which will be used to create a new cursor.
      (let ((pair-car (car pair))
	    (new-cursor NIL))
	(cond
	  ((is-a-p pair-car bitmap)
	   (let ((cursor-bm (g-value pair-car :image)))
	     (multiple-value-bind (cursor-width cursor-height)
		 (gem:image-size a-window cursor-bm)
	       (let ((cursor-pm (gem:create-pixmap a-window
						   cursor-width
						   cursor-height
						   1))
		     (mask-bm (g-value (cdr pair) :image))
		     (mask-pm nil) (mask-width nil) (mask-height nil))
		 ;; Write the mask into the cursor pixmap
		 (when mask-bm
		   (multiple-value-bind (width height)
		       (gem:image-size a-window mask-bm)
		     (setf mask-pm
			   (gem:create-pixmap a-window
					      (setf mask-width width)
					      (setf mask-height height)
					      1)))
		   (gem:copy-to-pixmap a-window mask-pm mask-bm
				       mask-width mask-height))

		 ;; Write the bitmap into the cursor pixmap
		 (gem:copy-to-pixmap a-window cursor-pm cursor-bm
				     cursor-width cursor-height)

		 ;; Create a new cursor with the cursor pixmap
		 (multiple-value-bind (x y)
		     (gem:image-hot-spot a-window cursor-bm)
		   (setf new-cursor
			 (gem:create-cursor a-window cursor-pm
					    (if mask-bm mask-pm)
					    (g-value black :xcolor)
					    (g-value white :xcolor)
					    NIL ;; not from-font
					    (or x 0)
					    (or y 0))))
		 (gem:set-window-property a-window :CURSOR new-cursor)
		 (gem:delete-pixmap a-window cursor-pm)
		 (if mask-bm (gem:delete-pixmap a-window mask-pm))))))

	  ((or (eq pair-car :CURSOR)
	       (is-a-p pair-car font-from-file))
	   (let* ((font (if (eq pair-car :CURSOR) CURSOR-FONT pair-car))
		  (cursor-char (second pair))
		  (mask-char (third pair)))
	     (setf new-cursor
		   (gem:create-cursor a-window font font
				      (g-value black :xcolor)
				      (g-value white :xcolor)
				      T	;; from font
				      cursor-char mask-char))
	     (gem:set-window-property a-window :CURSOR new-cursor)))
	  (t (error "Can't parse cursor spec ~S.~%" pair)))

	;; Store the cursor in case we see this pair again
	(push (cons pair new-cursor) (g-value a-window :cursor-pairs))
	)))
  t)


(defun Change-Cursors (pair &optional a-window-list)
  (let ((window-list (or a-window-list *garnet-windows*))
	(root NIL))
    (dolist (a-window window-list)
      ;; Don't change the cursor of windows that don't have drawables yet.
      ;; Identify these beasties via their :display-info slot.
      (let ((display-info (g-value a-window :display-info)))
	(when display-info
	  (Set-Window-Cursor a-window (g-value a-window :drawable) pair)
	  (or root (setf root a-window)))))
    (if root
      (gem:flush-output root))))


(defun Restore-Cursors (&optional a-window-list)
  (let ((window-list (or a-window-list *garnet-windows*))
	(root NIL))
    (dolist (a-window window-list)
      (let ((display-info (g-value a-window :display-info)))
	(when display-info
	  (Set-Window-Cursor a-window
			     (g-value a-window :drawable)
			     (g-value a-window :cursor))
	  (or root (setf root a-window)))))
    (if root
      (gem:flush-output root))))

;;;
;; Cosmetic application of cursors.
(defun set-gc-cursor ()
  (opal:change-cursors garbage-pair))

(defun unset-gc-cursor ()
  (opal:restore-cursors))

;; Actually this seems to freeze up the lisp for some reason.
#-(and)  (pushnew #'set-gc-cursor ext:*before-gc-hooks*)
#-(and) (pushnew #'unset-gc-cursor ext:*after-gc-hooks*)
#+cmu (setf ext:*gc-verbose* nil)

;;; Set the :window slot of the window to be the window itself!
;;;
(define-method :initialize window (a-window)
  (call-prototype-method a-window)
  (let ((win-info (make-win-update-info))
	(a-window-update-info (g-local-value a-window :update-info)))
    (unless (or (g-local-value a-window :title)
		(let ((proto-title (get-value a-window :title)))
		  (and proto-title
		       (or (formula-p proto-title)
			   ;; If proto does not have a default "Opal N" title
			   ;; (i.e., trimming "Opal " from it does not make a
			   ;; difference), then do not set the :title slot.
			   (string= proto-title
				    (string-left-trim "Opal " proto-title))))))
      (s-value a-window :title (new-garnet-window-name)))
    (unless (g-local-value a-window :icon-title)
      (s-value a-window :icon-title (g-value a-window :title)))
    (s-value a-window :win-update-info win-info)
    (if a-window-update-info
	(s-value a-window :window
		 (setf (update-info-window a-window-update-info) a-window)))
    (pushnew a-window *garnet-windows*)
    (let ((parent (g-value a-window :parent)))
      (if parent
	;; dzg - 11-27-1991
 	(s-value parent :child (cons a-window (g-local-value parent :child)))))
    (setf (win-update-info-new-bbox win-info) (make-bbox))
    ;;; Clip-mask-1 is the last four elements of clip-mask-2.
    (setf (win-update-info-clip-mask-1 win-info)
      (cddddr
        (setf (win-update-info-clip-mask-2 win-info) (make-list 8))))))


;;; Sets the icon of a window to be a particular pixmap.
(defun set-wm-icon (a-window bitmap-file)
  (if bitmap-file
    (if (stringp bitmap-file)
      (if (probe-file bitmap-file)
	(let ((image (read-image bitmap-file a-window)))
	  (multiple-value-bind (width height)
	      (gem:image-size a-window image)
	    (gem:build-pixmap a-window image width height t)))
	(format t "Warning: Icon bitmap file ~A does not exist." bitmap-file))
      (warn
       "Warning: the :icon-bitmap slot of a window should be NIL or a string."
       ))))


;;; Sets the gcontext-subwindow-mode of the gcontexts of a window's display.
(defun set-subwindow-mode (a-window mode)
  (gem:set-window-property a-window :SUBWINDOW-MODE mode))



;;; This now returns the drawable it creates.
;;;
(defun create-drawable (a-window)
  (let ((display-info (initialize-display a-window)))
    ;; Make sure the window has an attached display-info.
    (setf (g-value a-window :display-info) display-info)
    (multiple-value-bind (black-pixel #+comment white-pixel)
	(gem:black-white-pixel a-window)
      (let* ((title-name (g-value a-window :title))
	     (left (g-value a-window :left))
	     (top  (g-value a-window :top))
	     (border-width (g-value a-window :border-width))
	     (width  (g-value a-window :width))
	     (height (g-value a-window :height))
	     (parent (or (g-value a-window :parent)
			 ;; dzg & amickish -- use the root window of the
			 ;; current device.
			 (g-value gem:device-info :current-root)))
             ;; NIL background-color means white
	     (background (gem:color-to-index a-window
                          (g-value a-window :background-color)))
	     (drawable (gem:create-window
			parent
			left top width height
			title-name
			(or (g-value a-window :icon-title) title-name)
			background border-width
			(if (g-value a-window :save-under) :on :off)
			(if (eq (g-value a-window :visible) :iconified)
			    :iconic
			    :normal)
			(g-value a-window :min-width)
			(g-value a-window :min-height)
			(g-value a-window :max-width)
			(g-value a-window :max-height)
			(not (g-value a-window :position-by-hand))
			(not (g-value a-window :position-by-hand))
			(if (g-value a-window :omit-title-bar-p) :on :off))))

	(gem:set-drawable-to-window a-window drawable)
	(set-wm-icon a-window (g-value a-window :icon-bitmap))

	(if (g-value a-window :draw-on-children)
	  (set-subwindow-mode a-window :include-inferiors))

	(setf (g-value a-window :drawable) drawable)
	(if (g-value a-window :double-buffered-p)
	  (let ((buffer (create-buffer a-window)))
	    (s-value a-window :buffer buffer)
	    (gem:set-window-property a-window :BUFFER-GCONTEXT
				     (list buffer black-pixel background))
	    (clear-buffer a-window)))

	(s-value a-window :top-border-width border-width)
	(s-value a-window :left-border-width border-width)
	(s-value a-window :bottom-border-width border-width)
	(s-value a-window :right-border-width border-width)

        ;; Andyism = minor Edism + verbose comment
        ;;
        ;; The :very-first-exposure slot was invented for X windows to prevent
        ;; redundant redrawing of the window as it was mapped.  If this slot is
        ;; set on the Mac, then scrolling-windows will not be drawn properly --
        ;; the scroll-bars will be erased as the contents are drawn.  Note that
        ;; by allowing exposure events to be handled, we are observing the Mac
        ;; convention of allowing drawing to be done by the view-draw-contents
        ;; method.  Unfortunately, we are ALSO clinging to the traditional
        ;; Garnet way of explicitly updating windows on command.  Since both
        ;; strategies are being allowed, top-level windows will be redundantly
        ;; redrawn as they are mapped.  If you are interested in this line of
        ;; code, you will probably also be interested in the ccl:validate-view
        ;; instruction at the end of the update method.
        #-cmu
	(s-value a-window :very-first-exposure t)

	;; set the cursor to hemlock's cursor or specified cursor/mask combo
	;; (cursor-file . mask-file)
	(set-window-cursor a-window
			   drawable
			   (g-value a-window :cursor))

	;; bring up the window, and display it
	(if (g-value a-window :visible)
	  (gem:map-and-wait a-window drawable))
	(s-value a-window :old-parent (g-value a-window :parent))
	drawable))))



(defun fix-window-properties (a-window changed-slots drawable)
  (let ((make-new-buffer nil)
	(map-window-at-end-of-fix-properties nil)
	(win-info (g-value a-window :win-update-info)))
    (gem::batch-changes (drawable)
      (dolist (slot changed-slots)
	(case slot
	  ((:aggregate :drawable)
	   ;; not done
	   (let* (
		  (old-agg  (win-update-info-old-aggregate win-info))
		  (agg      (g-value a-window :aggregate)))
	     (set-window-cursor a-window drawable (g-value a-window :cursor))
	     (unless (eq old-agg agg)
	       (if (and (kr:schema-p old-agg)
		        (eq (g-value old-agg :window) a-window))
		 (set-display-slots old-agg NIL NIL))
	       (if agg
		 (set-display-slots agg a-window T))
	       (setf (win-update-info-old-aggregate win-info) agg)
	       (if (and old-agg (null agg))
		 (gem:clear-area a-window drawable)))))
	  (:parent
	   ;; not checked
	   (let ((old-parent (g-value a-window :old-parent))
	         (new-parent (g-value a-window :parent))
		 (left       (g-value a-window :left))
		 (top        (g-value a-window :top)))
	     (unless (eq old-parent new-parent)
	       ;; first, fix old-parent's :child slot
	       (if (schema-p old-parent)
		 (s-value old-parent :child
			  (delete a-window (g-value old-parent :child))))
	       ;; then update new-parent's :child slot
	       (if (schema-p new-parent)
		 (s-value new-parent :child
			  (pushnew a-window (g-value new-parent :child))))
	       ;; then update slots of the window itself
	       (s-value a-window :lineage nil)
	       (s-value a-window :old-parent new-parent)
	       ;; and, finally, tell X to reparent the drawable
	       (gem:reparent a-window new-parent drawable left top))))
	  (:cursor
	   (set-window-cursor a-window drawable (g-value a-window :cursor)))
	  (:title
	   (gem:set-window-property a-window :TITLE (g-value a-window :title)))
	  (:icon-title
	   (gem:set-window-property a-window :ICON-TITLE
				    (g-value a-window :icon-title)))
	  (:left
	   (gem:set-window-property a-window :LEFT
				    (+ (g-value a-window :left)
				       (g-value a-window :left-border-width))))
	  (:top
	   (gem:set-window-property a-window :TOP
				    (+ (g-value a-window :top)
				       (g-value a-window :top-border-width))))
	  (:width
	   (let ((width (g-value a-window :width)))
	     (setf (opal::win-update-info-width win-info) width)
	     (setf make-new-buffer
		   (or (gem:set-window-property a-window :WIDTH width)
		       make-new-buffer))))
	  (:height
	   (let ((height (g-value a-window :height)))
	     (setf (opal::win-update-info-height win-info) height)
	     (setf make-new-buffer
		 (or (gem:set-window-property a-window :HEIGHT height)
		     make-new-buffer))))
	  (:background-color
	   (gem:set-window-property a-window :BACKGROUND-COLOR
				    (g-value a-window :background-color)))
	  (:icon-bitmap
	   (set-wm-icon a-window (g-value a-window :icon-bitmap)))
	  (:draw-on-children
	   (if (g-value a-window :draw-on-children)
	     (set-subwindow-mode a-window :include-inferiors)
	     (set-subwindow-mode a-window :clip-by-children)))
	  (:save-under
	   (gem:set-window-property a-window :background-color
				    (if (g-value window :save-under)
				      :on :off)))
	  (:visible
	   (setf map-window-at-end-of-fix-properties
		 (or (gem:set-window-property a-window :VISIBLE
					      (g-value window :visible))
		     map-window-at-end-of-fix-properties))))))
    ;; Do this last so that window does not momentarily flicker
    ;; in its old position
    (if map-window-at-end-of-fix-properties
      (gem:map-and-wait a-window drawable))
    ;; Expand buffer after the with-state, but within let.
    (if make-new-buffer
      (expand-buffer a-window))))

(defun Delete-Notify (event-debug event-window)
  (if event-debug (format t " delete-notify ~s~%" event-window))
  ;; Will be changed to take a-window as a parameter, rather than
  ;; event-window. Hence, the following will be unnecessary.
  ;;
  ;; XXX FMG I don't think so. Not sure how you deal with orphaned
  ;; windows if you don't have the event window (X window) available.
  (let ((a-window (getf (xlib:drawable-plist event-window) :garnet)))
    (if (schema-p a-window)
	(let ((drawable (g-value a-window :drawable)))
	  (if (and drawable (= (xlib:window-id drawable)
			       (xlib:window-id event-window)))
	      (progn
		;; Because the window is being destroyed because of an
		;; event from the window manager, we want to allow the
		;; application to do something about it.
		(dolist (hook (g-value a-window :destroy-hooks))
		  (funcall hook a-window))
		(opal:destroy a-window))
	      ;; Then event-window is an orphaned window
	      (gem:delete-window a-window event-window)))
	;; Then event-window is an orphaned window
	(gem:delete-window NIL event-window))))

(define-method :destroy-me window (a-window)
  ;; first recursively destroy all subwindows
  (dolist (child (g-value a-window :child))
    (when (eq a-window (g-value child :parent))
      (destroy child)))
  ;; remove window from parent (if not top-level)
  (let ((parent (g-value a-window :parent)))
    (when parent
      (s-value parent :child
	       (delete a-window (g-local-value parent :child)))))
  ;; then destroy main window
  (let ((drawable (g-value a-window :drawable)))
    (when drawable
      (setf *garnet-windows* (delete a-window *garnet-windows*))
      (gem:delete-window a-window drawable)
      (gem:flush-output a-window)))
  (let ((agg (g-value a-window :aggregate)))
    (when agg (destroy agg nil)))
  (s-value a-window :window nil)
  ;; destroy the backing store
  (let ((buffer (g-value a-window :buffer)))
    (when buffer
      (gem:delete-pixmap a-window buffer T)))
  (call-prototype-method a-window))


(define-method :destroy window (a-window)
  (dolist (instance (copy-list (g-local-value a-window :is-a-inv)))
    (destroy instance))
  (destroy-me a-window)
  (when (and *exit-main-event-loop-function*
	     (not (any-top-level-window-visible)))
    (funcall *exit-main-event-loop-function*)))


(define-method :flush window (a-window)
  (gem:flush-output a-window))


;;; The following two functions have been added to be used by
;;; interactors. They are exported from Opal.
;;;
;;; XXX The name implies that this is X stuff. But there's also a mac
;;; version. So this name should be changed and the code updated here
;;; and elsewhere (interactor code). The only use of "X" in the Opal
;;; code should be as a counterpart to "Y" as in the X-Y axis. But ...
;;; it opens up a big can of worms and is not too important now.
(defun Get-X-Cut-Buffer (window)
  (if window
    (gem:get-cut-buffer window)
    ;; else return the empty string
    ""))


(defun Set-X-Cut-Buffer (window newstring)
  (if window
    (gem:set-cut-buffer window newstring)))
