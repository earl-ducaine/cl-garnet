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
;;; This file creates the box constraint menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHANGE LOG
;;;
;;; 08/24/92 amickish - Removed declare ignore of inter in :height-button,
;;;                     added proclaim
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")

;; *box-constraint-menu* and box-constraint-win are referenced in other
;; files.
(declaim (special *box-constraint-menu* box-constraint-win))

(defvar constraint-selection-formula 
  (o-formula (progn 
	       (or (gv *constraint-gadget* :obj-to-constrain)
		   (gv *constraint-gadget* :obj-to-reference))
	       nil)))

(defun box-constraint-do-go ()

(create-instance 'BOX-CONSTRAINT-WIN inter:interactor-window
   (:title "box constraints")
   (:left #-apple 772 #+apple 0)
   (:top #-apple 0 #+apple 200)
   (:width 375)
   (:height 510))


(create-instance '*BOX-CONSTRAINT-MENU* opal:aggregadget
   (:constant '(:left :top :width :height :visible))
   (:left 10)
   (:top 10)
   (:obj-to-constraint nil)
   (:obj-to-reference nil)
   (:top-offset 0)
   (:left-offset 0)
   (:width-difference 0)
   (:height-difference 0)
   (:width-scale 1)
   (:height-scale 1)
   (:parts
    `((:title ,opal:text
	      (:constant (t))
	      (:left ,(o-formula (gvl :parent :left)))
	      (:top ,(o-formula (gvl :parent :top)))
	      (:string "Box Constraint Menu")
	      (:font ,*large-bold-italic-serif-font*))

      (:ok ,garnet-gadgets:text-button-panel
        (:constant (t))
	(:button-width 44)
	(:left ,(o-formula (- (gvl :window :width) (gvl :width) 5)))
        (:top 5)
	(:direction :horizontal)
	(:fixed-width-p nil)
        (:gray-width 3) (:shadow-offset 5) 
	(:text-offset 2)
        (:selection-function
           ,#'(lambda (button string)
	  (cond ((string= string "OK")		
		 (s-value (g-value button :window) :visible nil))
		(t (set-box-constraint-feedback)))))
        (:items ("Show Constraints" "OK"))
        (:final-feedback-p nil))

      ;;;  *******************************************************************
      ;;;  The LEFT-SLOT-AGG component is the set of gadgets associated with
      ;;;  the :left slot of the primary selection, and appears at the top
      ;;;  of the menu.
      ;;;  *******************************************************************

      (:left-slot-agg ,opal:aggregadget
          (:constant (t :left :top :height))
	  (:left ,(o-formula (opal:gv-center-x-is-center-of
			      (gvl :parent :box-agg))))
	  (:top ,(o-formula (+ 30 (gvl :parent :top))))
	  (:width ,(o-formula (gvl :con-panel :width)))
	  (:height ,(o-formula (+ (gvl :slot-box :height)
				  (gvl :offset-box :height)
				  (gvl :con-panel :height) 10)))
	  (:parts
	   ((:slot-box ,SLOT-BOX
		(:constant (t))
		(:slot :left)
		(:selection-function set-left)
		(:label-string ":left")
		 (:parts
		  ((:label-text :modify
		       ; label doesn't look y-centered without this formula
		       (:top ,(o-formula (- (gvl :parent :center-y)
					    (floor (gvl :height) 2) 2))))
		   :field-text :frame)))
	     (:offset-box ,OFFSET-BOX
		(:constant (t :except :label-string))
		(:selection-function l-constraint-offset-action))
	     (:con-panel ,CON-PANEL
		 (:constant (t :left :top))
		 (:slot :left)
		 (:link-slot :left-over)
		 (:offset-slot :left-offset)
		 (:deselect ,(o-formula (gvl :parent :parent :box-agg
					     :left-button-panel :buttons)))
		 (:left ,(o-formula (gvl :parent :left)))
		 (:top ,(o-formula (+ 5 (opal:gv-bottom
					 (gvl :parent :offset-box)))))
		 (:width ,(o-formula (+ (* 2 (+ (gvl :fixed-width-size)
						(gvl :shadow-offset)))
					(gvl :h-spacing))))
		 (:height ,(o-formula (+ (gvl :shadow-offset)
					 (gvl :fixed-height-size))))
		 (:direction :horizontal)))))


      ;;;  *******************************************************************
      ;;;  The TOP-SLOT-AGG component is the set of gadgets associated with
      ;;;  the :top slot of the primary selection, and appears at the left
      ;;;  of the menu.
      ;;;  *******************************************************************

      (:top-slot-agg ,opal:aggregadget
          (:constant (t :left :top :height))
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (opal:gv-center-y-is-center-of
			     (gvl :parent :box-agg))))
	  (:width ,(o-formula (gvl :con-panel :width)))
	  (:height ,(o-formula (+ (gvl :slot-box :height)
				  (gvl :offset-box :height)
				  (gvl :con-panel :height) 10)))
	  (:parts
	   ((:slot-box ,SLOT-BOX
		 (:constant (t))
		 (:slot :top)
		 (:selection-function set-top)
		 (:label-string ":top")
		 (:parts ((:label-text :modify
			      (:top ,(o-formula (- (gvl :parent :center-y)
						   (floor (gvl :height) 2) 2))))
			  :field-text :frame)))
	     (:offset-box ,OFFSET-BOX
		(:constant (t :except :label-string))
		(:selection-function t-constraint-offset-action))
	     (:con-panel ,CON-PANEL
		 (:slot :top)
		 (:constant (t :left :top))
		 (:link-slot :top-over)
		 (:offset-slot :top-offset)
		 (:deselect ,(o-formula (gvl :parent :parent :box-agg
					     :top-button-panel :buttons)))
		 (:left ,(o-formula (gvl :parent :left)))
		 (:top ,(o-formula (+ 5 (opal:gv-bottom
					 (gvl :parent :offset-box)))))))))


      ;;;  *******************************************************************
      ;;;  The WIDTH-BUTTON component is an aggregadget which allows the user
      ;;;  to constrain the :width of the primary selection to the :width of
      ;;;  the secondary selection.
      ;;;  *******************************************************************

      (:width-button ,opal:aggregadget
	  (:constant (:height :left :top :width))
	  (:left ,(o-formula (gvl :parent :box-agg :rect-left)))
	  (:top ,(o-formula (opal:gv-bottom (gvl :parent :box-agg :rect-agg))))
	  (:width ,(o-formula (gvl :parent :box-agg :rect-width)))
	  (:height ,(+ 3    ; distance from top of h-arrow to v-arrow
		       20   ; height of v-arrow
		       23)) ; height of con-button
	  (:parts
	   ((:h-arrow ,DIMENSION-ARROW
		  (:constant (t))
		  (:x1 ,(o-formula (gvl :parent :parent :box-agg :rect-left)))
		  (:y1 ,(o-formula (+ 3 (gvl :parent :top))))
		  (:x2 ,(o-formula (+ (gvl :x1) (gvl :parent :width))))
		  (:y2 ,(o-formula (gvl :y1))))
	     (:v-arrow ,CONSTRAINT-ARROW
		  (:constant (t))
		  (:x1 ,(o-formula (opal:gv-center-x (gvl :parent))))
		  (:y1 ,(o-formula (+ 20 (gvl :y2))))
		  (:x2 ,(o-formula (gvl :x1)))
		  (:y2 ,(o-formula (gvl :parent :h-arrow :y1))))
	     (:con-button ,BOX-CONSTRAINT-BUTTON
		  (:constant (t))
		  ;; want no constraint to be selected once the user
		  ;; switches selections
		  (:value 0)
		  (:left ,(o-formula (opal:gv-center-x-is-center-of
				      (gvl :parent))))
		  (:top ,(o-formula (gvl :parent :v-arrow :y1))))))
	  (:interactors
	   ((:con-inter ,BOX-CONSTRAINT-INTER
		  (:constant (t))
		  (:final-function 
		   ,#'(lambda (inter obj)
		      (progn
		       (if (apply-box-constraint-p)
			   (progn
			     ;; deselect any buttons that were selected
			     ;; in the unconstrain/customize panel
			     (s-value (g-value *box-constraint-menu*
					       :width-slot-agg :con-panel)
				      :value nil)
			     (attach-constraint *box-constraint-menu*
					  :width :width-over
					  :width-difference
					  (aref *width-constraint-vector* (g-value obj :value))
					  :width-scale))
			   (deselect-constraint-button 
			    (g-value inter :operates-on :con-button))))))
		  (:start-where ,(o-formula (list :in-box (gvl :operates-on
							   :con-button))))))))


      ;;;  *******************************************************************
      ;;;  The WIDTH-SLOT-AGG component is the set of gadgets associated with
      ;;;  the :width slot of the primary selection, and appears at the
      ;;;  bottom-left of the menu.
      ;;;  *******************************************************************

      (:width-slot-agg ,opal:aggregadget
          (:constant (t :left :top :height))
	  (:left ,(o-formula (opal:gv-right-is-left-of
			      (gvl :parent :width-button :con-button))))
          (:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :width-button)))))
	  (:width ,(o-formula (gvl :difference-box :width)))
	  (:parts
	   ((:slot-box ,SLOT-BOX
		(:constant (t))
		(:slot :width)
		(:selection-function set-width)
		(:label-string ":width"))
	    (:scale-box ,SCALE-BOX
		(:constant (t))
		(:selection-function w-constraint-scale-action)
	        (:left ,(o-formula (opal:gv-center-x-is-center-of
				    (gvl :parent))))
		(:top ,(o-formula (+ 5 (opal:gv-bottom
					(gvl :parent :slot-box))))))
	    (:difference-box ,DIFFERENCE-BOX
		(:constant (t))
	        (:selection-function w-constraint-difference-action))
	    (:con-panel ,CON-PANEL
		(:constant (t :left :top))
		(:slot :width)
		(:link-slot :width-over)
		(:offset-slot :width-difference)
		(:scale-slot :width-scale)
		(:deselect ,(o-formula (gvl :parent :parent 
					    :width-button :con-button)))
		(:left ,(o-formula (opal:gv-center-x-is-center-of
				    (gvl :parent))))
		(:top ,(o-formula (+ 5 (opal:gv-bottom
					(gvl :parent :difference-box)))))))))


      ;;;  *******************************************************************
      ;;;  The HEIGHT-BUTTON component is an aggregadget which allows the user
      ;;;  to constrain the :height of the primary selection to the :height of
      ;;;  the secondary selection.
      ;;;  *******************************************************************

      (:height-button ,opal:aggregadget
	  (:constant (:width :left :top))
	  (:left ,(o-formula (opal:gv-right (gvl :parent :box-agg :rect-agg))))
	  (:top ,(o-formula (opal:gv-center-y-is-center-of
			     (gvl :parent :box-agg :rect-agg))))
	  (:width ,(+ 3    ; distance from left of v-arrow to h-arrow
		      20   ; width of h-arrow
		      23)) ; width of con-button
	  (:height ,(o-formula (gvl :v-arrow :height)))
	  (:parts
	   ((:v-arrow ,DIMENSION-ARROW
		 (:constant (t))
		 (:x1 ,(o-formula (+ 3 (gvl :parent :left))))
		 (:y1 ,(o-formula (gvl :parent :parent :box-agg :rect-top)))
		 (:x2 ,(o-formula (gvl :x1)))
		 (:y2 ,(o-formula (+ (gvl :y1)
				     (gvl :parent :parent :box-agg
					  :rect-width)))))
	     (:h-arrow ,CONSTRAINT-ARROW
		 (:constant (t))
		 (:x1 ,(o-formula (+ 20 (gvl :x2))))
		 (:y1 ,(o-formula (opal:gv-center-y (gvl :parent))))
		 (:x2 ,(o-formula (gvl :parent :v-arrow :x1)))
		 (:y2 ,(o-formula (gvl :y1))))
	     (:con-button ,BOX-CONSTRAINT-BUTTON
		 (:constant (t))
	         ;; want no constraint to be selected once the user
	         ;; switches selections
		 (:value 0)
		 (:left ,(o-formula (gvl :parent :h-arrow :x1)))
		 (:top ,(o-formula (opal:gv-center-y-is-center-of
				    (gvl :parent)))))))
	  (:interactors
	   ((:con-inter ,BOX-CONSTRAINT-INTER
		  (:constant (t))
		  (:final-function 
		   ,#'(lambda (inter obj)
		     (progn
		       (if (apply-box-constraint-p)
			   (progn
			     ;; deselect any buttons that were selected
			     ;; in the unconstrain/customize panel
			     (s-value (g-value *box-constraint-menu*
					       :width-slot-agg :con-panel)
				      :value nil)
			     (attach-constraint *box-constraint-menu*
					    :height :height-over
					  :height-difference
					  (aref *height-constraint-vector* (g-value obj :value))
					  :height-scale))
		           (deselect-constraint-button 
			    (g-value inter :operates-on :con-button))))))
		 (:start-where ,(o-formula (list :in-box (gvl :operates-on
							  :con-button))))))))


      ;;;  *******************************************************************
      ;;;  The HEIGHT-SLOT-AGG component is the set of gadgets associated with
      ;;;  the :height slot of the primary selection, and appears at the
      ;;;  bottom-right of the menu.
      ;;;  *******************************************************************

      (:height-slot-agg ,opal:aggregadget
          (:constant (t :left :top :height))
          (:left ,(o-formula (opal:gv-right
			      (gvl :parent :width-button :con-button))))
	  (:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :width-button)))))
	  (:width ,(o-formula (gvl :difference-box :width)))
	  (:height ,(o-formula (+ (gvl :slot-box :height)
				  (gvl :scale-box :height)
				  (gvl :difference-box :height)
				  (gvl :con-panel :height))))
	  (:parts
	   ((:slot-box ,SLOT-BOX
		 (:constant (t))
		 (:slot :height)
		 (:selection-function set-height)
		 (:left ,(o-formula (opal:gv-center-x-is-center-of
				     (gvl :parent))))
		 (:label-string ":height")
		 (:parts ((:label-text :modify
			     (:top ,(o-formula (- (gvl :parent :center-y)
						  (floor (gvl :height) 2) 2))))
			  :field-text :frame)))
	     (:scale-box ,SCALE-BOX
		 (:constant (t))
		 (:selection-function h-constraint-scale-action)
		 (:left ,(o-formula (opal:gv-center-x-is-center-of
				     (gvl :parent))))
		 (:top ,(o-formula (+ 5 (opal:gv-bottom
					 (gvl :parent :slot-box))))))
	     (:difference-box ,DIFFERENCE-BOX	 
		 (:constant (t))
		 (:selection-function h-constraint-difference-action))
	     (:con-panel ,CON-PANEL
                 (:constant (t :left :top))
		 (:slot :height)
		 (:link-slot :height-over)
		 (:offset-slot :height-difference)
		 (:scale-slot :height-scale)
		 (:deselect ,(o-formula (gvl :parent :parent 
					     :height-button :con-button)))
		 (:left ,(o-formula (opal:gv-center-x-is-center-of
				     (gvl :parent))))
		 (:top ,(o-formula (+ 5 (opal:gv-bottom
					 (gvl :parent :difference-box)))))))))


      ;;;  *******************************************************************
      ;;;  The BOX-AGG component contains
      ;;;    1  RECT-AGG:  the rectangle which represents the secondary
      ;;;        selection, with a tiny rectangle at each of its four corners.
      ;;;    2  LEFT-BUTTON-PANEL:  the five buttons-with-arrows associated
      ;;;        with the :left slot of the primary selection, which appear
      ;;;        at the top of the menu
      ;;;    3  TOP-BUTTON-PANEL:  the five buttons-with-arrows associated
      ;;;        with the :top slot of the primary selection, which appear at
      ;;;        the left of the menu
      ;;;  *******************************************************************

      (:box-agg ,opal:aggregadget
	  (:constant (:width :height :left :top))
          (:left ,(o-formula (+ 10 (opal:gv-right
				    (gvl :parent :top-slot-agg)))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :left-slot-agg)))))
	  (:width 200)
	  (:height 200)
	  (:sel-rect-left ,(o-formula (+ (gvl :left) 23 20)))
	  (:sel-rect-top ,(o-formula (+ (gvl :top) 23 20)))
	  (:rect-left ,(o-formula (+ (gvl :sel-rect-left) 4)))
	  (:rect-top ,(o-formula (+ 4 (gvl :sel-rect-top))))
	  (:rect-width ,(o-formula (- (gvl :width) 23 23 20 1)))
	  (:rect-height ,(o-formula (- (gvl :height) 23 23 20 1)))
	  (:parts
	   ((:rect-agg ,opal:aggregadget
		(:constant (:left :top :width :height))
		(:left ,(o-formula (gvl :parent :sel-rect-left)))
		(:top ,(o-formula (gvl :parent :sel-rect-top)))
		(:width ,(o-formula (+ 10 (gvl :parent :rect-width))))
		(:height ,(o-formula (+ 10 (gvl :parent :rect-width))))
		(:rect-left ,(o-formula (gvl :parent :rect-left)))
		(:rect-top ,(o-formula (gvl :parent :rect-top)))
		(:parts
		 ((:rect ,opal:rectangle
			 (:constant (t))
			 (:left ,(o-formula (gvl :parent :rect-left)))
			 (:top ,(o-formula (gvl :parent :rect-top)))
			 (:width ,(o-formula (gvl :parent :parent :rect-width)))
			 (:height ,(o-formula (gvl :parent :parent :rect-height))))
		  (:nw ,SEL-BOX
		       (:constant (t))
		       (:left ,(o-formula (gvl :parent :left)))
		       (:top ,(o-formula (gvl :parent :top))))
		  (:ne ,SEL-BOX
		       (:constant (t))
		       (:center-x ,(o-formula (+ (gvl :parent :left)
						 (gvl :parent :parent
						      :rect-width))))
		       (:top ,(o-formula (gvl :parent :top))))
		  (:sw ,SEL-BOX
		       (:constant (t))
		       (:left ,(o-formula (gvl :parent :left)))
		       (:center-y ,(o-formula (+ (gvl :parent :top)
						 (gvl :parent :parent 
						      :rect-height)))))
		  (:se ,SEL-BOX
		       (:constant (t))
		       (:center-x ,(o-formula (+ (gvl :parent :left)
						 (gvl :parent :parent
						      :rect-width))))
		       (:center-y ,(o-formula (+ (gvl :parent :top)
						 (gvl :parent :parent
						      :rect-height))))))))

	    (:left-button-panel ,opal:aggregadget
                (:constant (:height :width :left :top))
		(:left ,(o-formula (- (gvl :parent :sel-rect-left) 20)))
		(:top ,(o-formula (gvl :parent :top)))
		(:width ,(o-formula (+ 35 (gvl :parent :rect-agg :width))))
		(:height ,(+ 23 20))  ; height of a button + height of an arrow
		(:button-bottom ,(o-formula (+ 23 (gvl :top))))
		(:parts
		 ((:buttons ,opal:aggregadget
		      (:constant (:height))
		      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (gvl :parent :top)))
		      (:width ,(o-formula (gvl :parent :width)))
		      (:height 23)
		      (:filling-style ,opal:black-fill)
		      ;; want no constraint to be selected once the user
		      ;; switches selections or if the selection type is
		      ;; not of type :button
		      (:selected ,(o-formula 
				   (progn 
				     (or (gv *constraint-gadget* :obj-to-constrain)
					 (gv *constraint-gadget* :obj-to-reference))
				     nil)))
		      (:parts
		       ((:out-left ,BOX-CONSTRAINT-BUTTON
			    (:constant (t))
			    (:value 0)
			    (:left ,(o-formula (gvl :parent :parent :left)))
			    (:top ,(o-formula (gvl :parent :parent :top))))
			(:in-left ,BOX-CONSTRAINT-BUTTON
			    (:constant (t))
			     (:value 1)
			     (:left ,(o-formula (opal:gv-right
						 (gvl :parent :out-left))))
			     (:top ,(o-formula (gvl :parent :parent :top))))
			(:center ,BOX-CONSTRAINT-BUTTON
			    (:constant (t))
			     (:value 2)
			     (:left ,(o-formula
				      (- (opal:gv-center-x
					  (gvl :parent :parent :parent
					       :rect-agg))
					 (floor (gvl :width) 2))))
			     (:top ,(o-formula (gvl :parent :parent :top)))
			     (:center-p T))
			(:in-right ,BOX-CONSTRAINT-BUTTON
			    (:constant (t))
			     (:value 3)
			     (:left ,(o-formula (- (gvl :parent :out-right
							:left) 23)))
			     (:top ,(o-formula (gvl :parent :parent :top))))
			(:out-right ,BOX-CONSTRAINT-BUTTON
			    (:constant (t))
			     (:value 4)
			     (:left ,(o-formula (- (opal:gv-right
						    (gvl :parent :parent)) 23)))
			     (:top ,(o-formula (gvl :parent :parent :top))))))
		      (:interactors
		       ((:left-press ,BOX-CONSTRAINT-INTER
		          (:constant (t))
			  (:final-function
			   ,#'(lambda (inter obj)
			 (let ((box (g-value inter :operates-on :parent :parent
				      :parent :left-slot-agg :offset-box)))
			   (if (g-value obj :center-p)
			       (when (not (string= (g-value box :label-string)
						   "percent"))
				 (s-value box :label-string "percent")
				 (s-value box :value "50")
				 (s-value *box-constraint-menu* :left-offset 0.5))
			       (when (not (string= (g-value box :label-string)
						   "offset"))
				 (s-value box :label-string "offset")
				 (s-value box :value "0")
				 (s-value *box-constraint-menu* :left-offset 0)))
			   (if (apply-box-constraint-p)
			       (progn
				 ;; deselect any buttons that were selected
				 ;; in the unconstrain/customize panel
				 (s-value (g-value *box-constraint-menu*
						   :left-slot-agg :con-panel)
					  :value nil)
				 (attach-constraint *box-constraint-menu*
					      :left :left-over :left-offset
					      (aref *left-constraint-vector*
						    (g-value obj :value))))
			       (deselect-constraint-button 
				(g-value inter :operates-on))))))))))
			   
		  (:arrows ,opal:aggregadget
			   (:constant (:left :top :width :height))
			   (:parts
			    ((:l ,CONSTRAINT-ARROW
			         (:constant (t))
				 (:x1 ,(o-formula
					(+ 2 (gvl :parent :parent
						  :buttons :in-left :left))))
				 (:y1 ,(o-formula (gvl :parent :parent
						       :button-bottom)))
				 (:x2 ,(o-formula (gvl :x1)))
				 (:y2 ,(o-formula (+ (gvl :y1) 20))))
			     (:c ,CONSTRAINT-ARROW
			         (:constant (t))
				 (:x1 ,(o-formula (opal:gv-center-x
						   (gvl :parent :parent
							:buttons :center))))
				 (:y1 ,(o-formula (gvl :parent :parent
						       :button-bottom)))
				 (:x2 ,(o-formula (gvl :x1)))
				 (:y2 ,(o-formula (+ (gvl :y1) 20))))
			     (:r ,CONSTRAINT-ARROW
			         (:constant (t))
				 (:x1 ,(o-formula
					(+ 2 (gvl :parent :parent :buttons
						  :out-right :left))))
				 (:y1 ,(o-formula (gvl :parent :parent
						       :button-bottom)))
				 (:x2 ,(o-formula (gvl :x1)))
				 (:y2 ,(o-formula (+ (gvl :y1) 20))))))))))

	    (:top-button-panel ,opal:aggregadget
		 (:constant (:width :left :top :height))
		 (:left ,(o-formula (gvl :parent :left)))
		 (:top ,(o-formula (- (gvl :parent :sel-rect-top) 20)))
		 (:width ,(+ 20 23))
		 (:height ,(o-formula (+ 35 (gvl :parent :rect-agg :width))))
		 (:parts
		  ((:buttons ,opal:aggregadget
		       (:constant (:width))
		       (:left ,(o-formula (gvl :parent :left)))
		       (:top ,(o-formula (gvl :parent :top)))
		       (:width 23)
		       (:height ,(o-formula (gvl :parent :height)))
		       (:filling-style ,opal:black-fill)
		       ;; want no constraint to be selected once the user
		       ;; switches selections
		       (:selected ,(o-formula 
				    (progn 
				      (or (gv *constraint-gadget* :obj-to-constrain)
					  (gv *constraint-gadget* :obj-to-reference))
				      nil)))
		       (:parts
			((:out-top ,BOX-CONSTRAINT-BUTTON
			     (:constant (t))
			     (:value 0)
			     (:left ,(o-formula (gvl :parent :parent :left)))
			     (:top ,(o-formula (gvl :parent :parent :top))))
			 (:in-top ,BOX-CONSTRAINT-BUTTON
			     (:constant (t))
			     (:value 1)
			     (:left ,(o-formula (gvl :parent :parent :left)))
			     (:top ,(o-formula (opal:gv-bottom
						(gvl :parent :out-top)))))
			 (:center ,BOX-CONSTRAINT-BUTTON
			     (:constant (t))
			     (:value 2)
			     (:left ,(o-formula (gvl :parent :parent :left)))
			     (:top ,(o-formula (- (opal:gv-center-y
						   (gvl :parent :parent :parent
							:rect-agg))
						  (floor (gvl :height) 2))))
			     (:center-p T))
			 (:in-bot ,BOX-CONSTRAINT-BUTTON
			     (:constant (t))
			     (:value 3)
			     (:left ,(o-formula (gvl :parent :parent :left)))
			     (:top ,(o-formula (- (gvl :parent :out-bot :top)
						  (gvl :height)))))
			 (:out-bot ,BOX-CONSTRAINT-BUTTON
			     (:constant (t))
			     (:value 4)
			     (:left ,(o-formula (gvl :parent :parent :left)))
			     (:top ,(o-formula (- (opal:gv-bottom 
						   (gvl :parent :parent)) 23))))))
		       (:interactors
			((:top-press ,BOX-CONSTRAINT-INTER
		          (:constant (t))
			  (:final-function
		       ,#'(lambda (inter obj)
			 (let ((box (g-value inter :operates-on :parent :parent
				      :parent :top-slot-agg :offset-box)))
			   (if (g-value obj :center-p)	
			       (when (not (string= (g-value box :label-string)
						   "percent"))
				 (s-value box :label-string "percent")
				 (s-value box :value "50")
				 (s-value *box-constraint-menu* :top-offset 0.5))
			       (when (not (string= (g-value box :label-string)
						   "offset"))
				 (s-value box :label-string "offset")
				 (s-value box :value "0")
				 (s-value *box-constraint-menu* :top-offset 0)))
			   (if (apply-box-constraint-p) 
			       (progn
				 ;; deselect any buttons that were selected
				 ;; in the unconstrain/customize panel
				 (s-value (g-value *box-constraint-menu*
						   :top-slot-agg :con-panel)
					  :value nil)
				 (attach-constraint *box-constraint-menu*
						  :top :top-over :top-offset
						  (aref *top-constraint-vector*
							(g-value obj :value))))
			       (deselect-constraint-button 
				(g-value inter :operates-on))))))))))
		   (:arrows ,opal:aggregadget
		      (:constant (:left :top :width :height))
		      (:parts
		       ((:t ,CONSTRAINT-ARROW
			    (:constant (t))
			    (:x1 ,(o-formula (opal:gv-right
					      (gvl :parent :parent
						   :buttons :in-top))))
			    (:y1 ,(o-formula (+ 2 (gvl :parent :parent
						       :buttons :in-top :top))))
			    (:x2 ,(o-formula (+ 20 (gvl :x1))))
			    (:y2 ,(o-formula (gvl :y1))))
			(:c ,CONSTRAINT-ARROW
			    (:constant (t))
			    (:x1 ,(o-formula (opal:gv-right
					      (gvl :parent :parent
						   :buttons :center))))
			    (:y1 ,(o-formula (opal:gv-center-y
					      (gvl :parent :parent
						   :buttons :center))))
			    (:x2 ,(o-formula (+ 20 (gvl :x1))))
			    (:y2 ,(o-formula (gvl :y1))))
			(:b ,CONSTRAINT-ARROW
			    (:constant (t))
			    (:x1 ,(o-formula (opal:gv-right
					      (gvl :parent :parent 
						   :buttons :out-bot))))
			    (:y1 ,(o-formula (+ 2 (gvl :parent :parent
						       :buttons :out-bot :top))))
			    (:x2 ,(o-formula (+ 20 (gvl :x1))))
			    (:y2 ,(o-formula (gvl :y1))))))))))))))))


(create-instance 'BOX-WIN-agg opal:aggregate
   (:constant '(:left :top :width :height :visible)))
(opal:add-component BOX-WIN-AGG *BOX-CONSTRAINT-MENU*)
(s-value BOX-CONSTRAINT-WIN :aggregate BOX-WIN-AGG)
(opal:update BOX-CONSTRAINT-WIN))

(defun box-constraint-do-stop ()
  (when (boundp 'BOX-CONSTRAINT-WIN) (opal:destroy BOX-CONSTRAINT-WIN)))
