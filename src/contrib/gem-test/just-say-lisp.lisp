;;; -*- Mode:Lisp; Syntax: Common-lisp; Package:XLIB; Base:10; Lowercase: Yes -*-


;;; Texas Instruments Incorporated
;;; P.O. BOX 2909
;;; Austin, TX 78769
;;;
;;; Copyright (C) 1988 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is"
;;; without express or implied warranty.


;;; These functions demonstrate a simple menu implementation described
;;; in Kimbrough, Kerry, "Windows to the Future", Lisp Pointers,
;;; Oct-Nov, 1987.  See functions just-say-lisp and pop-up for
;;; demonstrations.

;;; Some changes are backported from CMUCL CLX source (our implementation had
;;; errors when we tried to use menu). This one is a little shorter.

(defpackage :gem-test
  (:use :common-lisp))

(in-package :gem-test)

(defun get-font-name ()
  (gem:make-font-name (kr:gv gem:DEVICE-INFO :current-device)
				'(:fixed :roman :medium)))

(defun get-font ()
  (xlib:open-font gem::*default-x-display* (get-font-name)))

(defstruct (menu)
  "A simple menu of text strings."
  (title "choose an item:")
  ;; ((item-window item-string))
  item-alist
  window
  width
  title-width
  item-width
  item-height
  ;; nil iff unchanged since displayed
  (geometry-changed-p t))


(defvar *display*)
(defvar *foreground-color*)
(defvar *background-color*)
(defvar *screen*)
(defvar *font*)
(defvar *gcontext*)

(defun setup-gem ()
  (gem::x-set-device-variables nil)
  (setf *display* gem::*default-x-display*)
  (setf *screen* gem::*default-x-screen*)
  (setf *font* (get-font))
  (setf *foreground-color* gem::*black*)
  (setf *background-color* gem::*white*)
  (setf *gcontext*
	(gem::gem-gc-gcontext
	 (gem::display-info-line-style-gc gem::*display-info*)))
  (setf (xlib:gcontext-foreground *gcontext*) *foreground-color*)
  (setf (xlib:gcontext-background *gcontext*) *background-color*))

(defun create-menu (parent-window)
  (make-menu
   ;; Create menu window
   :window   (xlib:create-window
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
	      :border *foreground-color*
	      :background *background-color*
	      :save-under :on
	      ;; override window mgr when positioning
	      ;; :override-redirect :on
	      :event-mask (xlib:make-event-mask :leave-window :exposure))))

(defun pop-up (strings &key (title "Pick one:"))
  (setup-gem)
  (let* ((parent-width 400)
         (parent-height 400)
         (parent (gem::x-create-window (kr:gv gem:device-info :current-root)
				       200
				       200
				       parent-width
				       parent-height
				       "jsl"
				       "jsl"
				       *background-color*
				       1
				       :off nil
				       10
				       10
				       parent-width
				       parent-height
				       nil
				       nil
				       :on))
         (a-menu    (create-menu parent))
         (prompt    "press a button...")
         (prompt-y  (xlib:font-ascent *font*))
         (ack-y     (- parent-height  (xlib:font-descent *font*))))
    (setf (menu-title a-menu) title)
    (menu-set-item-list a-menu strings)
    ;; present main window
    (gem::x-map-and-wait parent parent)
    (flet ((display-centered-text
	       (window string height width)
             (multiple-value-bind (w a d l r fa fd) (xlib:text-extents *gcontext* string)
               (declare (ignore a d l r))
               (let ((box-height (+ fa fd)))
                 ;; clear previous text
                 (xlib:clear-area window
				  :x 0 :y (- height fa)
				  :width width :height box-height)
                 ;; draw new text
                 (xlib:draw-image-glyphs window *gcontext* (round (- width w) 2) height string)))))
      (unwind-protect
	   (loop
	      (xlib:event-case (*display* :force-output-p t)
		(:exposure (count)
			   ;; display prompt
			   (when (zerop count)
			     (display-centered-text
			      parent
			      prompt
			      prompt-y
			      parent-width))
			   t)
		(:button-press (x y)
			       ;; pop up the menu
			       (let ((choice (menu-choose a-menu x y)))
				 (if choice
				     (display-centered-text
				      parent
				      (format nil "you have selected ~a." choice)
				      ack-y
				      parent-width)
				     (display-centered-text
				      parent
				      "No selection...try again."
				      ack-y
				      parent-width)))
			       t)
		(otherwise ()
			   ;; Ignore and discard any other event
			   t)))
        (xlib:close-display *display*)))))

(defun menu-set-item-list (menu item-strings)
  ;; Assume the new items will change the menu's width and height
  (setf (menu-geometry-changed-p menu) t)
  ;; Destroy any existing item windows
  (dolist (item (menu-item-alist menu))
    (xlib:destroy-window (first item)))
  ;; Add (item-window item-string) elements to item-alist
  (setf (menu-item-alist menu)
	(let (alist)
	  (dolist (item item-strings (nreverse alist))
	    (push (list (xlib:create-window
			  :parent     (menu-window menu)
			  :x          0         ;temporary value
			  :y          0         ;temporary value
			  :width      16        ;temporary value
			  :height     16        ;temporary value
			  :background *background-color*
			  :event-mask (xlib:make-event-mask :enter-window
						       :leave-window
						       :button-press
						       :button-release))
			item)
		  alist)))))

(defparameter *menu-item-margin* 4
  "Minimum number of pixels surrounding menu items.")

(defun menu-recompute-geometry (menu)
  (when (menu-geometry-changed-p menu)

    (let* (
	   (title-width (xlib:text-extents *font* (menu-title menu)))
	   (item-height (+ (xlib:font-ascent *font*) (xlib:font-descent *font*)))
	   (item-width  0)
	   (items       (menu-item-alist menu))
	   menu-width)
      ;; Find max item string width
      (dolist (next-item items)
	(setf item-width (max item-width
			      (xlib:text-extents *font* (second next-item)))))
      ;; Compute final menu width, taking margins into account
      (setf menu-width (max title-width
			    (+ item-width *menu-item-margin* *menu-item-margin*)))
      (let ((window  (menu-window menu))
	    (delta-y (+ item-height *menu-item-margin*)))
	;; Update width and height of menu window
	(xlib:with-state (window)
	  (setf (xlib:drawable-width  window) menu-width
		(xlib:drawable-height window) (+ *menu-item-margin*
					    (* (1+ (length items))
					       delta-y))))
	;; update width, height, position of item windows
	(let ((item-left     (round (- menu-width item-width) 2))
	      (next-item-top delta-y))
	  (dolist (next-item items)
	    (let ((window (first next-item)))
	      (xlib:with-state (window)
		(setf (xlib:drawable-height window) item-height
		      (xlib:drawable-width  window) item-width
		      (xlib:drawable-x      window) item-left
		      (xlib:drawable-y      window) next-item-top)))
	    (incf next-item-top delta-y))))
      ;; Map all item windows
      (xlib:map-subwindows (menu-window menu))
      ;; Save item geometry
      (setf (menu-item-width menu)         item-width
	    (menu-item-height menu)        item-height
	    (menu-width menu)              menu-width
	    (menu-title-width menu)        title-width
	    (menu-geometry-changed-p menu) nil))))

(defun menu-refresh (menu)
  (xlib:set-wm-properties (menu-window menu)
			  :name (menu-title menu)
			  :icon-name (menu-title menu)
			  :resource-name (menu-title menu))
  (let* ((baseline-y (xlib:font-ascent (get-font))))
   ;; Show title centered in "reverse-video"
       (xlib:draw-image-glyphs
	 (menu-window menu)
	 *gcontext*
	 ;; start x
	 (round (- (menu-width menu)
		   (menu-title-width menu)) 2)
	 ;; start y
	 baseline-y
	 (menu-title menu))
   ;; Show each menu item (position is relative to item window)
   (dolist (item (menu-item-alist menu))
     (xlib:draw-image-glyphs
      (first item)
      *gcontext*
       0					;start x
       baseline-y				;start y
       (second item)))))

(defun menu-choose (menu x y)
  ;; Display the menu so that first item is at x,y.
  (menu-present menu x y)
  (let ((items (menu-item-alist menu))
	(mw    (menu-window menu))
	selected-item)
    ;; Event processing loop
    (do () (selected-item)
      (xlib:event-case (*display* :force-output-p t)
	(:exposure     (count)
	 ;; Discard all but final :exposure then display the menu
	 (when (zerop count) (menu-refresh menu))
	 t)
	(:button-release (event-window)
	 ;;Select an item
	 (setf selected-item (second (assoc event-window items)))
	 t)
	(:enter-notify (window)
	 ;;Highlight an item
	 (let ((position (position window items :key #'first)))
	   (when position
	     (menu-highlight-item menu position)))
	 t)
	(:leave-notify (window kind)
	 (if (eql mw window)
	     ;; Quit if pointer moved out of main menu window
	     (setf selected-item (when (eq kind :ancestor) :none))
	   ;; Otherwise, unhighlight the item window left
	   (let ((position (position window items :key #'first)))
	     (when position
	       (menu-unhighlight-item menu position))))
	 t)
	(otherwise ()
		   ;; Ignore and discard any other event
		   t)))
    ;; Erase the menu
;;;    (UNMAP-WINDOW mw)
    ;; Return selected item string, if any
    (unless (eq selected-item :none) selected-item)))

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
		    *gcontext*
		    left top
		    width height)))

(defun menu-unhighlight-item (menu position)
  ;; draw a box in the menu background color
  (menu-highlight-item menu position))

(defun menu-present (menu x y)
  ;; Make sure menu geometry is up-to-date
  (menu-recompute-geometry menu)
  ;; Try to center first item at the given location, but
  ;; make sure menu is completely visible in its parent
  (let ((menu-window (menu-window menu)))
    (multiple-value-bind (tree parent) (xlib:query-tree menu-window)
      (declare (ignore tree))
      (xlib:with-state (parent)
	(let* ((parent-width  (xlib:drawable-width parent))
	       (parent-height (xlib:drawable-height parent))
	       (menu-height   (+ *menu-item-margin*
				 (* (1+ (length (menu-item-alist menu)))
				    (+ (menu-item-height menu)  *menu-item-margin*))))
	       (menu-x        (max 0 (min (- parent-width (menu-width menu))
					  (- x (round (menu-width menu) 2)))))
	       (menu-y        (max 0 (min (- parent-height menu-height)
					  (- y (round (menu-item-height menu) 2/3)
					     *menu-item-margin*)))))
	  (xlib:with-state (menu-window)
	    (setf (xlib:drawable-x menu-window) menu-x
		  (xlib:drawable-y menu-window) menu-y)))))
    ;; Make menu visible
    (xlib:map-window menu-window)))

;;; Demo functions

(defun just-say-lisp ()
  (setup-gem)
  (gem::x-set-device-variables nil)
  (setf *font* (get-font))
  (setf *display* gem::*default-x-display*)
  (let* ((a-menu (create-menu (xlib:screen-root *screen*))))
    (setf (menu-title a-menu) "Please pick your favorite language:")
    (menu-set-item-list a-menu '("Fortran" "APL" "Forth" "Lisp"))
    ;; Bedevil the user until he picks a nice programming language
    (unwind-protect
	 (do (choice)
	     ((and (setf choice (menu-choose a-menu 100 100))
		   (string-equal "Lisp" choice))))
      (xlib:close-display *display*))))
