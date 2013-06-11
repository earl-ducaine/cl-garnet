;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Scrolling Window Parts
;;;      see the file Scrolling-window for an explanation
#|
============================================================
Change log:
 1/12/94 Andrew Mickish - xlib:drawable-plist ---> opal:drawable-to-window
 9/08/93 Andrew Mickish - opal:cursor-multi-text ---> opal:text
 9/01/93 Andrew Mickish - Updated Get-Cursor-Box according to new opal:text
 7/26/93 Andrew Mickish - Put #+garnet-debug around demo functions
 6/28/93 Rajan Parthasarathy - Fixed show-box to consider the case when
           scroll bars are not on default side.
14/17/93 Andrew Mickish - Removed clip-mask parameters from update calls
12/15/92 Andrew Mickish - Added type and parameter declarations
12/10/92 Andrew Mickish - *drawable-to-window-mapping* ---> *garnet-windows*
9/07/92  Andrew Mickish - Set :clip-window in Scrolling-Window-Creator
6/26/92  Rajan Parthasarathy - Fixed show-box to scroll horizontally;
           Changed :auto-scroll method to scroll one line at a time.
6/19/92  Rajan Parthasarathy - Added auto-scroll
2/19/92  Ed Pervin - Implemented double-clip-masks.
2/18/92  Andrew Mickish - Added :maybe-constant list
7/26/91  Brad Myers - removed extra Scrolling-Window-With-Bars-Destroy
                      and Scrolling-Window-With-Bars-Creator.
                    - added Pedro's change so you can specify the
                      :inner-aggregate-prototype for scrolling-window
5/14/91  Andrew Mickish - Redefined Set-Scroll-Bar-Values to set
           the indicators' :box slot instead of the bars' :value slot
5/13/91  Edward Pervin - Scrolling-Window-With-Bars-Destroy
           was accidentally declared twice.
3/14/91  Brad Myers - Separated from Scrolling-window to allow 
           Motif-scrolling-window-with-bars
============================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (eval load compile)
  (export '(Scrolling-Window Auto-Scroll
	    Scroll-Win-Inc Scroll-Win-To
	    Scrolling-Window-Go Scrolling-Window-Stop
	    Show-Box Show-Cursor))

  (proclaim '(special SCROLLING-WINDOW SCROLLING-WINDOW-WITH-BARS
		      MOTIF-SCROLLING-WINDOW-WITH-BARS)))

(defmacro auto-scroll (obj)
  `(kr-send ,obj :auto-scroll ,obj))

(defconstant min-win-size 20) ; windows smaller than this size sometimes
			   ; cause an X error


;; This might be called directly by the user, in which case we need to
;; destroy the window, or it might be called when the window is destroyed,
;; in which case we don't want to destroy the window.  The gethash call
;; determines which case this is.
(defun Scrolling-Window-Destroy (window-gadget &optional erase)
  ;; have to destroy the windows
  (let ((window (g-value window-gadget :outer-window)))
    ;; make sure the window is not being destroyed already
    (if (and window
	     (schema-p window)
	     (opal:drawable-to-window (get-local-value window :drawable)))
	(opal:destroy window)))
  (call-prototype-method window-gadget erase))

;;; Must return outer window
(defun Scrolling-Window-Creator (window-gadget)
  (let* ((outer-window (create-instance NIL inter:interactor-window
		 (:left (o-formula (gvl :scroll-win-gadget :left)))
		 (:top (o-formula (gvl :scroll-win-gadget :top)))
		 (:position-by-hand (o-formula (gvl :scroll-win-gadget
						    :position-by-hand)))
		 (:width (o-formula (gvl :scroll-win-gadget :width)))
		 (:height (o-formula (gvl :scroll-win-gadget :height)))
		 (:border-width (o-formula (gvl :scroll-win-gadget
						:border-width)))
		 (:title (o-formula (gvl :scroll-win-gadget :title)))
		 (:icon-title (o-formula (gvl :scroll-win-gadget :icon-title)))
		 (:visible (o-formula (gvl :scroll-win-gadget :visible)))
		 (:scroll-win-gadget window-gadget)
		  ; use g-value for the next one because parent can't change
		 (:parent (g-value window-gadget :parent-window))))
	 (outer-agg (create-instance NIL opal:aggregate))
	 (inner-window (create-instance NIL inter:interactor-window
                 (:scroll-win-gadget window-gadget)
		 (:left (o-formula (gvl :scroll-win-gadget :X-Offset)))
		 (:top (o-formula (gvl :scroll-win-gadget :Y-Offset)))
		 (:width (o-formula
			  (let ((w (gvl :scroll-win-gadget :total-width)))
			    (if w (max min-win-size w) min-win-size))))
		 (:height (o-formula
			   (let ((h (gvl :scroll-win-gadget :total-height)))
			     (if h (max h min-win-size) min-win-size))))
		 (:border-width 0) ; no border
		 (:double-buffered-p
		  (o-formula (gvl :scroll-win-gadget :double-buffered-p)))
		 (:parent outer-window)))
	 (inner-agg (create-instance NIL
			 (or (g-value window-gadget :inner-aggregate-prototype)
			     opal:aggregadget))))
    (s-value outer-window :aggregate outer-agg) ; is an aggregate needed?
    (s-value inner-window :aggregate inner-agg)
    (s-value window-gadget :inner-window inner-window)
    (s-value window-gadget :outer-window outer-window)
    (s-value window-gadget :clip-window outer-window)
    (s-value window-gadget :inner-aggregate inner-agg)
    outer-window))

;;; This can be called from the top level, as if the gadget was a window, in
;;; which case it updates the window, creating it first if it is not there.  It
;;; can tell whether it is being called from the top level or from the user by
;;; the presence of the second parameter with a value.
;;; Actually, the window update call does NOT use a message send to call update
;;; on the children, but rather calls update-method-aggregate directly, so this
;;; method will NOT be called recursively when it is used to update the window,
;;; but leave the checks in just to be safe!
;;; 
;;; This method is used by both scrolling windows and scrolling-windows-with-bars.
(defun Scrolling-Window-Update
       (agg &optional (update-info :top-level)
	    line-style-gc filling-style-gc bbox-1 bbox-2 (total-p nil))
  (if (or (eq update-info :top-level)
	  (eq update-info T))
      ; then is a top level call from the user, so create windows if necessary
      ; and update the window
      (let ((win (g-value agg :outer-window)))
	(unless win
	  (setq win (kr-send agg :Creator-Func agg)))
	(opal:update win (if (eq update-info T) T NIL)))
      ;; else update the object normally
      (call-prototype-method agg update-info line-style-gc filling-style-gc
			     bbox-1 bbox-2 total-p)))

(create-instance 'Scrolling-Window opal:aggregadget
   :declare ((:parameters :left :top :width :height :position-by-hand
			  :border-width :parent-window :double-buffered-p
			  :title :icon-title :total-width :total-height
			  :x-offset :y-offset :visible)
	     (:type (kr-boolean :position-by-hand :double-buffered-p)
		    ((integer 0) :border-width :total-width :total-height)
		    (integer :x-offset :y-offset)
		    ((or (is-a-p inter:interactor-window) null) :parent-window)
		    ((or null string) :title :icon-title))
	     (:maybe-constant :title :parent-window))
   ; Customizable slots
   (:left 0) (:top 0)
   (:position-by-hand NIL)
   (:width 150)(:height 150)  ; note: is INNER width and height of outer window
   (:border-width 2)
   (:parent-window NIL)
   (:double-buffered-p NIL)
   (:title "Scrolling-Window")
   (:icon-title (o-formula (gvl :title)))
   (:total-width 200)
   (:total-height 200)
   (:X-Offset 0)
   (:Y-Offset 0)
   (:visible T)
      ; read-only slots
   (:Inner-Window NIL)  ; these are created by the update method
   (:inner-aggregate NIL) ; add your objects to this aggregate (but have to
			  ; update first)
   (:outer-window NIL) ; call Opal:Update on this window (or on gadget itself)

      ; internal slots
   (:destroy-me 'Scrolling-Window-Destroy)
   (:Update 'Scrolling-Window-Update)
   (:Creator-Func 'Scrolling-Window-Creator)
   )

;; This might be called directly by the user, in which case we need to
;; destroy the window, or it might be called when the window is destroyed,
;; in which case we don't want to destroy the window.  The gethash call
;; determines which case this is.
;; This is in this file, since it used by both the Garnet and Motif
;; scrolling window with bars.
(defun Scrolling-Window-With-Bars-Destroy (window-gadget &optional erase)
  ;; First, remove the gadget from its window so when the window is
  ;; destroyed, the gadget will not be.  Then destroy the gadget itself
  ;; using call-prototype-method
  (let ((agg (g-value window-gadget :parent))
	(window (g-value window-gadget :outer-window)))
    (when agg
      (opal:remove-component agg window-gadget))
    ;; make sure window isn't already being destroyed
    (if (and window
	     (schema-p window)
	     (opal:drawable-to-window (get-local-value window :drawable)))
	(opal:destroy window))
    (call-prototype-method window-gadget erase))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exported functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When a program explicitly sets the x-offset and y-offset, set the
;;   :value slots of the h and v scroll bars, so the indicator will move.
;; -- Andrew Mickish
;;    Redefined function to set the :box of the two scroll bar indicators
;;    instead of the :value slot of the scroll bars.  There are dependencies
;;    set up in the gadget that require setting these lower-level slots
;;    instead of the :value slots.
(defun Set-Scroll-Bar-Values (scroll-win-gadget)
  (let* ((h-scroll-bar (g-value scroll-win-gadget :h-scroll))
	 (v-scroll-bar (g-value scroll-win-gadget :v-scroll))
	 (h-indicator (g-value h-scroll-bar :indicator))
	 (v-indicator (g-value v-scroll-bar :indicator))
	 (x-offset (g-value scroll-win-gadget :x-offset))
	 (y-offset (g-value scroll-win-gadget :y-offset)))
    ; Set :left in h-scroll-bar
    (let ((bound-left (g-value h-scroll-bar :bound-left))
	  (bound-right (g-value h-scroll-bar :bound-right))
	  (val-1 (g-value h-scroll-bar :val-1))
	  (val-2 (g-value h-scroll-bar :val-2))
	  (ind-width (g-value h-indicator :width)))
      (s-value h-indicator
	       :box
	       (list (inter:clip-and-map (- x-offset) val-1 val-2
					 bound-left
					 (- bound-right ind-width))
		     0 0 0)))
    ; Set :top in v-scroll-bar
    (let ((bound-top (g-value v-scroll-bar :bound-top))
	  (bound-bottom (g-value v-scroll-bar :bound-bottom))
	  (val-1 (g-value v-scroll-bar :val-1))
	  (val-2 (g-value v-scroll-bar :val-2))
	  (ind-width (g-value v-indicator :width)))
      (s-value v-indicator
	       :box
	       (list 0
		     (inter:clip-and-map (- y-offset) val-1 val-2
					 bound-top
					 (- bound-bottom ind-width))
		     0 0)))))

;; Scroll-Outside-Win-P checks to see if we are being asked to scroll
;; vertically so that the background of the clip window is exposed.
;; If we are, it returns T, so that Scroll-Win-To and Scroll-Win-Inc
;; can reset the y-offsets so that we never scroll (vertically) out of the
;; inner-window

(defun Scroll-Outside-Win-P (scroll-win-gadget y)
  (AND
   (> (g-value scroll-win-gadget :total-height)
      (g-value scroll-win-gadget :clip-window :height))
   (> (- (g-value scroll-win-gadget :clip-window :height) y)
      (g-value scroll-win-gadget :total-height))))

(defun Scroll-Win-To (scroll-win-gadget x y)
"Scroll the specified window to the specified x and y offset (absolute)"
  (s-value scroll-win-gadget :x-offset x)
  (let ((new-y-offset (if (Scroll-Outside-Win-P scroll-win-gadget y)
			  (- (g-value scroll-win-gadget :clip-window :height)
			     (g-value scroll-win-gadget :total-height))
			  y)))
    (s-value scroll-win-gadget :y-offset new-y-offset))
  (if (g-value scroll-win-gadget :h-scroll)
      (Set-Scroll-Bar-Values scroll-win-gadget))
  (opal:update (g-value scroll-win-gadget :outer-window)))

(defun Scroll-Win-Inc (scroll-win-gadget x y)
"Scroll the specified window by the specified amount in x and y (relative)"
  (incf (g-value scroll-win-gadget :x-offset) x)
  (let* ((temp-y-offset (+ y (g-value scroll-win-gadget :y-offset)))
	 (new-y-offset (if (Scroll-Outside-Win-P scroll-win-gadget
						 temp-y-offset)
			   (- (g-value scroll-win-gadget :clip-window :height)
			      (g-value scroll-win-gadget :total-height))
			   temp-y-offset)))
    (s-value scroll-win-gadget :y-offset new-y-offset))
  (if (g-value scroll-win-gadget :h-scroll)
      (Set-Scroll-Bar-Values scroll-win-gadget))
  (opal:update (g-value scroll-win-gadget :outer-window)))


;;;
;;;  Auto Scroll
;;;

(defun Is-A-Scrolling-Window (obj)
  (or (and (boundp 'scrolling-window) (is-a-p obj scrolling-window))
      (and (boundp 'scrolling-window-with-bars)
	   (is-a-p obj scrolling-window-with-bars))
      (and (boundp 'motif-scrolling-window-with-bars)
	   (is-a-p obj motif-scrolling-window-with-bars))))


;; Showbox takes in a scrolling-window, and a left, top, right and bottom
;; of an area you want to scroll to.  If the area is too big, it will
;; scroll to the top of that area.
;;
;; 1) If the top is above the scrolling-window, but left is inside the
;;    scrolling-window, it'll move up only.
;;
;; 2) If left is too far to the left, but top is inside the scr-win, it
;;    will move left only.
;;
;; 3) Same way for bottom and right.
;;
;; 4) If the area is already in the window, it won't do anything.
;;
;; 5) For all other cases, it moves the top and left of the scrolling-window
;;    to the top and left of the area desired.
;;

(defun show-box (scr-win left top right bottom)
  (let* ((win-top (g-value scr-win :clip-window :top))
	 (win-left (g-value scr-win :clip-window :left))
	 (win-bottom (+ win-top (g-value scr-win :clip-window :height)))
	 (win-right (+ win-left (g-value scr-win :clip-window :width))))

    ;; have to check for :v-scroll-on-left
    ;; and :h-scroll-on-bottom before decrementing
    ;; win-left/win-bottom
    (when (AND (g-value scr-win :v-scroll-bar-p)
	       (g-value scr-win :v-scroll-on-left-p))
      (decf win-left (g-value scr-win :v-scroll :width)))
    (when (AND (g-value scr-win :h-scroll-bar-p)
	       (NOT (g-value scr-win :h-scroll-on-top)))
      (decf win-bottom (g-value scr-win :h-scroll :height)))


    (cond
      ((AND (> top win-top)
	    (>= left 0)
	    (<= right win-right)
	    (<= bottom win-bottom)))  ;The box is already in window

      ((AND (= top 0) (>= left 0))
       (gg:scroll-win-to scr-win win-left 0))   ;Move to start, keep left same
      
      ((AND (> bottom win-bottom) (>= left 0) (<= right win-right))
       (gg:scroll-win-to scr-win win-left (- top)))  ;Move down, keep left same
      
      ((AND (< top win-top) (>= left 0) (<= right win-right))
       (gg:scroll-win-to scr-win  win-left top))     ;Move up, but keep left same
      
      ((AND (<= bottom win-bottom) (> right win-right) (<= top win-top))
       (gg:scroll-win-to scr-win (- left) win-top))  ;Move right, keep top same
      
      ((AND (>= top win-top) (< left 0) (<= top win-top))
       (gg:scroll-win-to scr-win  left win-top))     ;Move left, but keep top same
      
      (T (gg:scroll-win-to scr-win
			   (if (> left 0)
			       (- left)
			     left)
			   (if (> top win-top)
			       (- top)
			     top))))))

;;
;; Get-Cursor-Box takes a cursor-multi-text object and returns four values:
;; the left, top, right, and bottom of the cursor.
;;
(defun Get-Cursor-Box (obj)
  (let* ((line-number (g-value obj :line-number))
	 (line-height (g-value obj :line-height))
	 (left (g-value obj :left))
	 (top (g-value obj :top))
	 (cursor-offset (g-value obj :cursor-offset)))
    (values (+ left cursor-offset)
	    (+ top (* line-number line-height))
	    (+ left cursor-offset opal::*cursor-width*)
	    (+ top (* (1+ line-number) line-height)))))
		

(define-method :auto-scroll opal:text (obj)
  (when (g-value obj :auto-scroll-p)	       
    (let ((scr-win (g-value obj :scrolling-window)))
      (if (Is-A-Scrolling-Window scr-win)
	  (let* ((win-height (g-value scr-win :clip-window :height))
		 (win-top (- (g-value scr-win :inner-window :top)))
		 (win-bottom (- win-height (g-value scr-win :y-offset)))
		 (cur-height NIL)
		 cur-left cur-top cur-right cur-bottom)
	    (multiple-value-setq (cur-left cur-top cur-right cur-bottom)
	      (Get-Cursor-Box obj))
	    (setf cur-height (- cur-bottom cur-top))
	    (cond
	      
	      ((OR (> cur-bottom (- (+ win-height cur-height)
				    (g-value scr-win :y-offset)))
		   (<= cur-top (- (+ cur-bottom
				     (g-value scr-win :y-offset)))))
	       (show-box scr-win cur-left
			 (if (> 0 (decf cur-top (ceiling win-height 2)))
			     0                     ;; Top of scr-win
			     cur-top)
			 cur-right (incf cur-bottom (ceiling win-height 2))))
	      
	      ((> cur-bottom (- win-height (g-value scr-win :y-offset)))
	       (show-box scr-win cur-left (incf win-top cur-height)
			 cur-right cur-bottom))
	      
	      ((<= cur-top (- (g-value scr-win :y-offset)))
	       (show-box scr-win cur-left cur-top cur-right
			 (decf win-bottom cur-height)))))
	  
	  (warn "Tried to scroll ~S for ~S,
but it is not a scrolling window." scr-win obj)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Demo programs 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+garnet-test
(defun Internal-Scrolling-Window-Go (which-obj dont-enter-main-event-loop
					       double-buffered-p)
  (declare (ignore dont-enter-main-event-loop))
  (let* ((My-Scrolling-Window (create-instance NIL which-obj
		   (:left 350)(:top 50)(:width 300)(:height 400)
		   (:title "Scrolling Window")
		   (:int-feedback-p NIL) ; used only for ..-with-bars
		   (:double-buffered-p double-buffered-p)
		   (:total-width 500)
		   (:total-height 500)))
	 agg)
    (opal:update My-Scrolling-Window)
    ;; agg not available until after the update
    (setq agg (g-value My-Scrolling-Window :inner-aggregate))
    (opal:add-components agg
	(create-instance NIL opal:rectangle
			 (:Left 0)(:top 0)(:width 30)(:height 30))
	(create-instance NIL opal:rectangle
			 (:Left 470)(:top 0)(:width 30)(:height 30)
			 (:line-style NIL)
			 (:filling-style opal:black-fill))
	(create-instance NIL opal:rectangle
			 (:Left 0)(:top 470)(:width 30)(:height 30)
			 (:filling-style opal:gray-fill))
	(create-instance NIL opal:rectangle
			 (:Left 470)(:top 470)(:width 30)(:height 30)
			 (:filling-style opal:light-gray-fill))
	(create-instance NIL opal:circle
			 (:Left 235)(:top 235)(:width 30)(:height 30)
			 (:filling-style #+apple opal:dark-gray-fill
                                         #-apple opal:diamond-fill)))
    (opal:update My-Scrolling-Window)
    My-Scrolling-Window))

#+garnet-test
(defparameter Scrolling-Window-Obj NIL)

#+garnet-test
(defun Scrolling-Window-Go (&key dont-enter-main-event-loop double-buffered-p)
  (setq Scrolling-Window-Obj
	(Internal-Scrolling-Window-Go Scrolling-Window 
				      dont-enter-main-event-loop
				      double-buffered-p)))

#+garnet-test
(defun Scrolling-Window-Stop ()
  (opal:destroy Scrolling-Window-Obj))
