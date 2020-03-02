(defpackage :gem-lab
  (:use common-lisp
	;; gem
	xlib)
  (:shadowing-import-from :xlib
			  draw-lines
			  text-extents
			  draw-points
			  create-image
			  draw-line
			  draw-arc
			  create-window
			  create-cursor
			  event-handler
			  clear-area
			  text-width
			  translate-coordinates
			  create-pixmap
			  draw-rectangle))

(in-package :gem-lab)

(defun run-all-garnet-windows ()
  (x-all-garnet-windows)
  ;;gem::*default-x-root*
  )


;;; create window
;;; draw on window
;;; apply bitmap

(defun the-colormap ()
  (first (xlib::installed-colormaps *default-x-root*)))

(defun run-make-color ()
  (xlib:make-color :red 1 :green 1 :blue 1))


(defun run-query-color ()
  (xlib:query-colors (the-colormap) (list (run-make-color))))


(defun alocate-color ()
  ;; get the pixel (device dependant 32 bit value) of a color (device
  ;; independant triplet of reals).
  ;;(xlib:alloc-color the-colormap (run-make-color))
  )



(defstruct (menu)
  "A simple menu of text strings."
  (title "Choose an item:")
  ;; E.g., '((item-window item-string))
  item-alist
  window
  gcontext
  width
  title-width
  item-width
  item-height
  (geometry-changed-p t))


(defun create-menu (parent-window text-color background-color text-font)
  (make-menu
   ;; Create menu graphics context
   :gcontext (create-gcontext :drawable
			      parent-window
			      :foreground text-color
			      :background background-color
			      :font
			      text-font)
   ;; Create menu window
   :window (xlib:create-window
	    :parent parent-window
	    :class :input-output
	    ;; temporary value
	    :x 0
	    ;; temporary value
	    :y 0
	    ;; temporary value
	    :width 16
	    ;; temporary value
	    :height 16
	    :border-width 2
	    :border text-color
	    :background background-color
	    :save-under :on
	    ;; override window mgr when positioning
	    :override-redirect :on
	    :event-mask
	    (MAKE-EVENT-MASK :leave-window :exposure))))


(defun menu-set-item-list (menu &rest item-strings)
  ;; Assume the new items will change the menu’s width and height
  (setf (menu-geometry-changed-p menu) t)
  ;; Destroy any existing item windows
  (dolist (item (menu-item-alist menu))
    (destroy-window (first item)))
  ;; Add (item-window item-string) elements to item-alist
  (setf (menu-item-alist menu)
	(let (alist)
	  (dolist (item item-strings (nreverse alist))
	    (push (list (xlib:create-window
			 :parent (menu-window menu)
			 :x 0
			 :y 0
			 :width 16
			 :height 16
			 :background (gcontext-background (menu-gcontext menu))
			 :event-mask (make-event-mask :enter-window
						      :leave-window
						      :button-press
						      :button-release))
			item)
		  alist)))))

(defparameter *menu-item-margin* 4)

(defun menu-highlight-item (menu position)
  (let* ((box-margin  (round *menu-item-margin* 2))
	 (left        (- (round (- (menu-width menu) (menu-item-width menu)) 2)
			 box-margin))
	 (top         (- (* (+ *menu-item-margin* (menu-item-height menu))
			    (1+ position))
			 box-margin))
	 (width       (+ (menu-item-width menu) box-margin box-margin))
	 (height      (+ (menu-item-height menu) box-margin box-margin)))
    ;; Draw a box in menu window around the given item.
    (xlib:draw-rectangle (menu-window menu)
		    (menu-gcontext menu)
		    left top
		    width height)))

(defun menu-recompute-geometry (menu)
  (when (menu-geometry-changed-p menu)
    (let* ((menu-font
	    (gcontext-font (menu-gcontext menu)))
	   (title-width (xlib:text-extents menu-font (menu-title menu)))
	   (item-height (+ (font-ascent menu-font)
			   (font-descent menu-font)
			   *menu-item-margin*))
	   (item-width 0)
	   (items
	    (menu-item-alist menu))
	   menu-width)
      ;; Find max item string width
      (setf item-width
	    (+ *menu-item-margin*
	       (dolist (next-item items item-width)
		 (setf item-width (max item-width
				       (xlib:text-extents menu-font (second next-item)))))))
      ;; Compute final menu width, taking margins into account
      (setf menu-width (max title-width (+ item-width *menu-item-margin*)))
      (let ((window
	     (menu-window menu)))
	;; Update width and height of menu window
	(with-state (window)
	  (setf (drawable-width
		 window) menu-width
		 (drawable-height window) (* (1+ (length items)) item-height)))
	;; Update width, height, position of item windows
	(let ((item-left
	       (round (- menu-width item-width) 2))
	      (next-item-top (- item-height (round *menu-item-margin* 2))))
	  (dolist (next-item items)
	    (let ((window (first next-item)))
	      (with-state (window)
		(setf (drawable-height window) item-height
		      (drawable-width
		       window) item-width
		      (drawable-x
		       window) item-left
		      (drawable-y
		       window) next-item-top)))
	    (incf next-item-top item-height))))
      ;; Map all item windows
      (map-subwindows (menu-window menu))
      ;; Save item geometry
      (setf (menu-item-width menu)
	    item-width
	    (menu-item-height menu)
	    item-height
	    (menu-width menu)
	    menu-width
	    (menu-title-width menu)
	    title-width
	    (menu-geometry-changed-p menu) nil))))




(defun menu-refresh (menu)
  (let* ((gcontext
	  (menu-gcontext menu))
	 (baseline-y (FONT-ASCENT (GCONTEXT-FONT gcontext))))
    ;; Show title centered in "reverse-video"
    (let ((fg (gcontext-background gcontext))
	  (bg (gcontext-foreground gcontext)))
      (with-gcontext (gcontext :foreground fg :background bg)
	(draw-image-glyphs
	 (menu-window menu)
	 gcontext
	 (round (- (menu-width menu)
		   (menu-title-width menu)) 2)
	 baseline-y
	 (menu-title menu))))
    ;; Show each menu item (position is relative to item window)
    (let ((box-margin (round *menu-item-margin* 2)))
      (dolist (item (menu-item-alist menu))
	(draw-image-glyphs
	 (first item) gcontext
	 box-margin
	 (+ baseline-y box-margin)
	 (second item))))))




;; (let ((#:g595 (drawable-display mw))
;;       (#:g596 nil))
;;   (declare (type display #:g595))
;;   (xlib::event-loop (#:g595 #:g594 nil t nil)
;;     (xlib::event-dispatch (#:g595 #:g594 #:g596)
;;       (:exposure (count) (progn (when (zerop count) (menu-refresh menu)) t))
;;       (:button-release (event-window)
;;        (progn (setf selected-item (second (assoc event-window items))) t))
;;       (:enter-notify (window)
;;        (progn (menu-highlight-item menu (find window items :key #'first)) t))
;;       (:leave-notify (window kind)
;;        (progn
;;         (if (eql mw window)
;;             (setf selected-item (when (eq kind :ancestor) :none))
;;             (menu-unhighlight-item menu (find window items :key #'first)))
;;         t))
;;       (otherwise nil (progn t)))))

(defun menu-present (menu x y)
  ;; Make sure menu geometry is up-to-date
  (menu-recompute-geometry menu)
  ;; Try to center first item at the given location, but
  ;; make sure menu is completely visible in its parent
  (let ((menu-window (menu-window menu)))
    (multiple-value-bind (tree parent) (query-tree menu-window)
      (declare (ignore tree))
      (with-state (parent)
	(let* ((parent-width  (drawable-width parent))
	       (parent-height (drawable-height parent))
	       (menu-height   (+ *menu-item-margin*
				 (* (1+ (length (menu-item-alist menu)))
				    (+ (menu-item-height menu)  *menu-item-margin*))))
	       (menu-x        (max 0 (min (- parent-width (menu-width menu))
					  (- x (round (menu-width menu) 2)))))
	       (menu-y        (max 0 (min (- parent-height menu-height)
					  (- y (round (menu-item-height menu) 2/3)
					     *menu-item-margin*)))))
	  (with-state (menu-window)
	    (setf (drawable-x menu-window) menu-x
		  (drawable-y menu-window) menu-y)))))
    ;; Make menu visible
    (map-window menu-window)))

(defun menu-choose (menu x y)
  ;; Display the menu so that first item is at x,y.
  (menu-present menu x y)
  (let ((items (menu-item-alist menu))
	(mw (menu-window menu))
	selected-item)
    ;; Event processing loop
    (do ()
	(selected-item)
      (event-case ((drawable-display mw) :force-output-p t)
	(:exposure
	 (count)
	 ;; Discard all but final :exposure then display the menu
	 (when (zerop count) (menu-refresh menu))
	 t)
	(:button-release
	 (event-window)
	 ;;Select an item
	 (setf selected-item (second (assoc event-window items)))
	 t)
	(:enter-notify
	 (window)
	 ;;Highlight an item
	 (menu-highlight-item menu (find window items :key #'first))
	 t)
	(:leave-notify
	 (window kind)
	 (if (eql mw window)
	     ;; Quit if pointer moved out of main menu window
	     (setf selected-item
		   (when (eq kind :ancestor)
		     :none))
	     ;; Otherwise, unhighlight the item window left
	     (menu-unhighlight-item menu (find window items :key #'first)))
	 t)
	(otherwise
	 ()
	 ;;Ignore and discard any other event
	 t)))
    ;; Erase the menu
    (unmap-window mw)
    ;; Return selected item string, if any
    (unless (eq selected-item :none)
      selected-item)))

(defun just-say-lisp (&optional host font-name)
  (let* ((display
	  (if host
	      (open-display host)
	      (xlib::open-default-display)))
	 (screen
	  (first (display-roots display)))
	 (fg-color (screen-black-pixel screen))
	 (bg-color (screen-white-pixel screen))
	 ;; font-name used to have a default of "fg-16" But,
	 ;; apperently, this is no longer commonly included in a basic
	 ;; X11 installation. At least it was missing from Ubuntu 19.10.
	 (nice-font (if font-name
			(open-font display font-name)
			(car (xlib:list-fonts
			 (xlib::open-default-display)
			 "*-adobe-times-medium-*34-240-*-170*-iso8859-1"))))
	 ;; Create a menu as a child of the root window.
	 (a-menu
	  (create-menu (screen-root screen)
		       fg-color bg-color nice-font)))
    (setf (menu-title a-menu) "please pick your favorite language:")
    (menu-set-item-list a-menu "fortran" "apl" "forth" "lisp")
    ;; Bedevil the user until he picks a nice programming language
    ;; (unwind-protect
    ;; 	 (loop
    ;; 	    ;; Determine the current root window position of the pointer
    ;; 	    (multiple-value-bind (x y)
    ;; 		(query-pointer (screen-root screen))
    	      (let ((choice (menu-choose a-menu 50 50)))
    		;; (when (string-equal "lisp" choice)
    		;;   (return))
		)
    ;;   (close-display display))
    ))

(defun menu-unhighlight-item (menu position)
  ;; Draw a box in the menu background color
  (let ((gcontext (menu-gcontext menu)))
    (with-gcontext (gcontext :foreground (gcontext-background gcontext))
      (menu-highlight-item menu position))))
