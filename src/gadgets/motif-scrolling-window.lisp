;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;          The Garnet User Interface Development Environment.       ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;;; $Id$


;;;  Motif-Scrolling-Window-With-Bars
;;    contains two optional scroll bars
;; 
;;     Customizable slots
;;   	  :left, :top, :width, :height, Default=0,0,150,150 - left, top,
;; 				width and height of outer window (size of visible
;; 				portion smaller by :min-scroll-bar-width)
;;	  :position-by-hand, default=NIL - if T, the user is asked for the outer
;; 				window's position.
;;       :border-width, default=2 - of outer window
;;       :parent-window, default=NIL - window this scrolling-window is
;;				inside of, or NIL
;;       :double-buffered-p, default=NIL
;;       :title, default="Motif Scrolling Window"
;;       :icon-title, default=(same as title)
;;       :total-width, default=200 - total size of the scrollable area inside
;;       :total-height, default=200)  
;;       :X-Offset, default=0 - offset in the scrollable area; DO NOT SET
;; 			THESE OR PUT FORMULAS IN THEM, use the exported functions
;;       :Y-Offset, default=0   
;;       :visible, default=T - whether the entire window is visible (mapped)
;;
;;       :h-scroll-bar-p, default=T - Is there a horizontal scroll bar?
;;       :v-scroll-bar-p, default=T - Is there a vertical scroll bar?
;;       :Foreground-Color (default=Motif-gray) - the color of the
;;                         scrollbars and the background of the window
;;
;;     Scroll Bar slots
;;       :h-scroll-on-top-p, default=NIL - whether horiz bar is on top or bottom
;;       :v-scroll-on-left-p, default=T - whether vert bar is on left or right
;;       :h-scr-incr, default=10 - in pixels
;;       :h-page-incr - default jumps one page
;;       :v-scr-incr, default=10 - in pixels
;;       :v-page-incr - default jumps one page
;;
;;    Read-Only slots
;;       :Inner-Window - these are created by the update method
;;       :inner-aggregate - add your objects to this aggregate (but have to
;;				   update first)
;;       :outer-window - call Opal:Update on this window (or on gadget itself)
;;       :clip-window
;;
;; NOTE: Create the window, then call Update on it.  Do not add it
;; to an aggregate or a window.  If you want the scrolling window in
;; another window, specify the :parent-window slot instead:
;;     (create-instance NIL garnet-gadgets:scrolling-window(-with-bars)
;; 			(...)(:parent-window other-window) )
;;
;;  Designed and written by Brad Myers
;;  Based on an idea from Roderick J. Williams at the University of Leeds
;;
;;  *** KNOWN BUG *** When the user changes the size or position of the outer
;;  window with the window manager, the fields of the scrolling
;;  window gadget are not updated.  Circular constraints won't work
;;  because the user will usually override the values for the slots
;;  when the window is created.  I think the fix will have to wait
;;  for eager evaluation --BAM


;;; Change log:
;;        12/13/93  Andrew Mickish - Added :omit-title-bar-p and made :parent
;;                    reference a formula
;;         7/26/93  Andrew Mickish - Put #+garnet-debug around demo functions
;;        12/15/92  Andrew Mickish - Added type and parameter declarations
;;	10/02/91  Meltsner - Checked if there was zero width or height
;;			     inner window in formula for :percent-visible
;;	 3/14/91  Brad Myers - created


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Scrolling-Window-With-Bars
	    Motif-Scrolling-Window-With-Bars-Go 
	    Motif-Scrolling-Window-With-Bars-Stop)))

;; ** Scrolling-window-parts must be loaded first **

;; Must return the outer-window
(defun Motif-Scrolling-Window-With-Bars-Creator (window-gadget)
  (let* ((outer-window (create-instance NIL inter:interactor-window
			 (:scroll-win-gadget window-gadget)
			 (:left (o-formula (gvl :scroll-win-gadget :left)))
			 (:top (o-formula (gvl :scroll-win-gadget :top)))
			 (:position-by-hand (o-formula (gvl :scroll-win-gadget
							    :position-by-hand)))
			 (:width (o-formula (gvl :scroll-win-gadget :width)))
			 (:height (o-formula (gvl :scroll-win-gadget :height)))
			 (:title (o-formula (gvl :scroll-win-gadget :title)))
			 (:icon-title (o-formula (gvl :scroll-win-gadget :icon-title)))
			 (:visible (o-formula (gvl :scroll-win-gadget :visible)))
			 (:border-width (o-formula (gvl :scroll-win-gadget
							:border-width)))
			 (:parent (o-formula (gvl :scroll-win-gadget :parent-window)))
			 (:omit-title-bar-p (g-value window-gadget :omit-title-bar-p))
			 (:background-color (o-formula (gvl :scroll-win-gadget
							    :foreground-color)))))
	 (outer-agg (create-instance NIL opal:aggregate))
	 (clip-window (create-instance NIL inter:interactor-window
			(:scroll-win-gadget window-gadget)
			(:left (o-formula (gvl :scroll-win-gadget :clip-win-left)))
			(:top (o-formula (gvl :scroll-win-gadget :clip-win-top)))
			(:width (o-formula (gvl :scroll-win-gadget :inner-width)))
			(:height (o-formula (gvl :scroll-win-gadget :inner-height)))
			(:border-width 0)
			(:parent outer-window)
			(:background-color (o-formula (gvl :scroll-win-gadget
							   :foreground-color)))))
	 (clip-agg (create-instance NIL opal:aggregate))
	 (inner-window (create-instance NIL inter:interactor-window
			 (:scroll-win-gadget window-gadget)
			 (:left (o-formula (gvl :scroll-win-gadget :X-Offset)))
			 (:top (o-formula (gvl :scroll-win-gadget :Y-Offset)))
			 (:width (o-formula
				  (let ((w (or (gvl :scroll-win-gadget :total-width)
					       0))
					(innerw (gvl :scroll-win-gadget
						     :clip-window :width)))
				    (max min-win-size w innerw))))
			 (:height (o-formula
				   (let ((h (or (gvl :scroll-win-gadget :total-height)
						0))
					 (innerh (gvl :scroll-win-gadget
						      :clip-window :height)))
				     (max h innerh min-win-size))))
			 (:border-width 0) ; no border
			 (:double-buffered-p
			  (o-formula (gvl :scroll-win-gadget :double-buffered-p)))
			 (:parent clip-window)
			 (:background-color (o-formula (gvl :scroll-win-gadget
							    :foreground-color)))))
	 (inner-agg2 (create-instance NIL opal:aggregate))
	 (inner-agg (create-instance NIL opal:aggregate)))
    (s-value outer-window :aggregate outer-agg)
    (s-value clip-window :aggregate clip-agg) ; is an aggregate needed?
    (s-value inner-window :aggregate inner-agg2)
    (opal:add-component inner-agg2 inner-agg)
    (s-value window-gadget :inner-window inner-window)
    (s-value window-gadget :clip-window clip-window)
    (s-value window-gadget :outer-window outer-window)
    (s-value window-gadget :inner-aggregate inner-agg)
    (opal:add-component outer-agg window-gadget)
    outer-window
    ))

(create-instance 'Motif-Scrolling-Window-With-Bars opal:aggregadget
  :declare ((:parameters :left :top :width :height :position-by-hand
			 :border-width :parent-window :double-buffered-p
			 :title :icon-title :total-width :total-height
			 :x-offset :y-offset :h-scroll-bar-p :v-scroll-bar-p
			 :h-scroll-on-left-p :v-scroll-on-top-p
			 :min-scroll-bar-width :scr-trill-p :page-trill-p
			 :h-scr-incr :h-page-incr :v-scr-incr :v-page-incr
			 :int-feedback-p :indicator-text-p :indicator-font
			 :foreground-color :visible)
	    (:type (kr-boolean :position-by-hand :double-buffered-p
			       :h-scroll-bar-p :v-scroll-bar-p :h-scroll-on-left-p
			       :v-scroll-on-top-p :scr-trill-p :page-trill-p
			       :int-feedback-p :indicator-text-p)
		   ((integer 0) :border-width :total-width :total-height
		    :min-scroll-bar-width)
		   (integer :x-offset :y-offset :h-scr-incr :h-page-incr
			    :v-scr-incr :v-page-incr)
		   ((or (is-a-p inter:interactor-window) null) :parent-window)
		   ((or null string) :title :icon-title)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :indicator-font)
		   ((is-a-p opal:color) :foreground-color))
	    (:maybe-constant :left :top :width :height :border-width
			     :title :total-width :total-height :foreground-color
			     :h-scroll-bar-p :v-scroll-bar-p :visible
			     :h-scroll-on-top-p :v-scroll-on-left-p
			     :h-scr-incr :h-page-incr :v-scr-incr :v-page-incr
			     :icon-title :parent-window))
					;  Customizable slots
  (:left 0) (:top 0)
  (:position-by-hand NIL)
  (:width 150)(:height 150)		; INNER width and height of outermost window
  (:border-width 2)
  (:parent-window NIL)
  (:double-buffered-p NIL)
  (:title "Motif Scrolling Window")
  (:icon-title (o-formula (gvl :title)))
  (:total-width 200)			; of the full area that graphics will be in
  (:total-height 200)			; of the full area that graphics will be in
  (:X-Offset 0)				; can be set explicitly, and is set by scroll bars
  (:Y-Offset 0)				; can be set explicitly, and is set by scroll bars
  (:h-scroll-bar-p T)			; Is there a horizontal scroll bar?
  (:v-scroll-bar-p T)			; Is there a vertical scroll bar?
  (:visible T)
  (:foreground-color Opal:Motif-gray)

					;scroll bar slots
  (:h-scroll-on-top-p NIL)		; whether scroll bar is on left or right
  (:v-scroll-on-left-p T)		; whether scroll bar is on top or bottom
  (:h-scr-incr 10)			; in pixels
  (:h-page-incr (o-formula (- (gvl :outer-window :width) 10)))
  (:v-scr-incr 10)			; in pixels
  (:v-page-incr (o-formula (- (gvl :outer-window :height) 10)))

					; read-only slots
  (:Inner-Window NIL)			; these are created by the update method
  (:inner-aggregate NIL)		; add your objects to this aggregate
  (:outer-window NIL)			; call Opal:Update on this window (or on gadget itself)
  (:clip-window NIL)

					; internal slots
					; make the next two depend on the outer window in
					; case it is changed by the user using the window manager.
  (:outer-win-inner-height (o-formula (gvl :outer-window :height) 50))
  (:outer-win-inner-width (o-formula (gvl :outer-window :width) 50))
  (:inner-width (o-formula (- (gvl :outer-win-inner-width)
			      (if (gvl :v-scroll-bar-p)
				  (gvl :v-scroll :width)
				  0))))
  (:inner-height (o-formula (- (gvl :outer-win-inner-height)
			       (if (gvl :h-scroll-bar-p)
				   (gvl :h-scroll :height)
				   0))))
  (:clip-win-left (o-formula (if (and (gvl :v-scroll-bar-p)
				      (gvl :v-scroll-on-left-p))
				 (gvl :v-scroll :width)
				 0)))
  (:clip-win-top (o-formula (if (and (gvl :h-scroll-bar-p)
				     (gvl :h-scroll-on-top-p))
				(gvl :h-scroll :height)
				0)))
  (:destroy-me 'Scrolling-Window-With-Bars-Destroy)
  (:update 'Scrolling-Window-Update)
  (:creator-func 'Motif-Scrolling-Window-With-Bars-Creator)

  (:parts 
   `((:v-scroll ,garnet-gadgets:motif-v-scroll-bar
		(:left ,(o-formula (if (gvl :parent :v-scroll-on-left-p)
				       0
					; else at right
				       (- (gvl :parent :outer-win-inner-width)
					  (gvl :width)
					  -1))))
		(:top ,(o-formula (if (and (gvl :parent :h-scroll-bar-p)
					   (gvl :parent :h-scroll-on-top-p))
				      (gvl :parent :h-scroll :height)
				      0)))
		(:val-1 0)
		(:height ,(o-formula (gvl :parent :inner-height)))
		(:val-2 ,(o-formula (Max 1 (- (gvl :parent :total-height)
					      (gvl :parent :inner-height))) 0))
		(:scr-incr ,(o-formula (gvl :parent :v-scr-incr)))
		(:page-incr ,(o-formula (gvl :parent :v-page-incr)))
		(:active-p ,(o-formula
			     (and (gvl :window)
				  (or (/= 0 (gvl :parent :y-offset))
				      (>= (gvl :parent :total-height)
					  (gvl :parent :inner-height))))))
		(:visible ,(o-formula (gvl :parent :v-scroll-bar-p)))
		(:foreground-color ,(o-formula (gvl :parent :foreground-color)))
		(:percent-visible ,(o-formula
				    (if (zerop (gvl :parent :total-height))
					1
					(min (/ (gvl :parent :inner-height)
						(gvl :parent :total-height))
					     1))))
		(:selection-function
		 ,#'(lambda (gadget new-val)
		      (s-value (g-value gadget :parent) :y-offset
			       (- new-val)))))
     (:h-scroll ,garnet-gadgets:motif-h-scroll-bar
		(:left ,(o-formula (if (and (gvl :parent :v-scroll-bar-p)
					    (gvl :parent :v-scroll-on-left-p))
				       (gvl :parent :v-scroll :width)
				       0)))
		(:val-1 0)
		(:top ,(o-formula (if (gvl :parent :h-scroll-on-top-p)
				      0
				      (- (gvl :parent :outer-win-inner-height)
					 (gvl :height)
					 -1))))
		(:width ,(o-formula (gvl :parent :inner-width)))
		(:val-2 ,(o-formula (Max 1 (- (gvl :parent :total-width)
					      (gvl :parent :inner-width))) 0))
		(:scr-incr ,(o-formula (gvl :parent :h-scr-incr)))
		(:page-incr ,(o-formula (gvl :parent :h-page-incr)))
		(:active-p ,(o-formula
			     (and (gvl :window)
				  (or (/= 0 (gvl :parent :x-offset))
				      (>= (gvl :parent :total-width)
					  (gvl :parent :inner-width))))))
		(:visible ,(o-formula (gvl :parent :h-scroll-bar-p)))
		(:foreground-color ,(o-formula (gvl :parent :foreground-color)))
		(:percent-visible ,(o-formula
				    (if (zerop (gvl :parent :inner-width))
					1
					(min (/ (gvl :parent :inner-width)
						(gvl :parent :total-width))
					     1))))
		(:selection-function
		 ,#'(lambda (gadget new-val)
		      (s-value (g-value gadget :parent) :x-offset
			       (- new-val))))))))


;;; Demo programs 
;;

#+garnet-test
(defparameter Motif-Scrolling-Window-With-Bars-Obj NIL)

#+garnet-test
(defun Motif-Scrolling-Window-With-Bars-Go (&key dont-enter-main-event-loop
					   double-buffered-p)
  (setq Motif-Scrolling-Window-With-Bars-Obj
	(Internal-Scrolling-Window-Go Motif-Scrolling-Window-With-Bars
				      dont-enter-main-event-loop
				      double-buffered-p)))

#+garnet-test
(defun Motif-Scrolling-Window-With-Bars-Stop ()
  (opal:destroy Motif-Scrolling-Window-With-Bars-Obj))

