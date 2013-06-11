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
;;; This file contains the parts used to create the line constrant menu
;;; and sets up the line constraint menu
;;;
;;; Changes:
;;; 14-Jul-93 AMICKISH  *white-line-style* ---> opal:white-line;  Declared
;;;                     LINE-CON-PANEL special in Line-Constraint-Do-Go.
;;; 12-May-92 ECP  Added ok and cancel buttons to *line-constraint-menu*
;;;

(in-package :GARNET-GADGETS)

(defvar *line-constraint-menu* nil)
(defvar line-con-prim-sel-agg nil)
(defvar line-con-sec-sel-agg nil)

(defun line-constraint-do-go ()
  (declare (special LINE-CON-PANEL))
(line-constraint-do-stop)

;; model diagonal line--used to initialize the lines in the
;; line constraint menu

(create-instance 'model-diag-line opal:line
   (:x1 0) (:y1 0)
   (:x2 100) (:y2 100))

(create-instance '*LINE-CONSTRAINT-MENU* opal:aggregate
   (:constant '(:left :top :width :height :visible))
   (:x1-offset 0)
   (:x2-offset 0)
   (:y1-offset 0)
   (:y2-offset 0)
   (:left-offset 0)
   (:top-offset 0))

(create-instance 'LINE-CONSTRAINT-INTER inter:button-interactor
   (:maybe-constant :window :start-where)
   (:window (o-formula (gvl :operates-on :window)))
   (:active (o-formula (gvl :operates-on :parent :active)))
   (:start-where (o-formula (list :element-of (gvl :operates-on))))
   (:how-set :set))


(create-instance 'LINE-CONSTRAINT-BUTTON opal:aggregadget
   (:constant '(:width :height :visible :where-attach))
   (:floating-left (o-formula (+ (gvl :left)
				 (if (gvl :interim-selected) 4 0))))
   (:floating-top (o-formula (+ (gvl :top)
				(if (gvl :interim-selected) 4 0))))
   (:width 23) (:height 23)
   ;; become deselected whenever the user changes a selection
   (:selected (o-formula (progn (or (gv *constraint-gadget* :obj-to-constrain)
				    (gv *constraint-gadget* :obj-to-reference))
				nil)))
   (:where-attach (o-formula (nth (gvl :rank)
				  (gvl :parent :parent :where-attach))))
   (:filling-style (o-formula (gvl :parent :filling-style)))
   (:parts
    `((:shadow ,opal:rectangle
	   (:left ,(o-formula (+ 4 (gvl :parent :left))))
	   (:top ,(o-formula (+ 4 (gvl :parent :top))))
	   (:width 19) (:height 19)
	   (:filling-style ,opal:black-fill)
	   (:line-style NIL))
      (:gray-outline ,opal:rectangle
	   (:left ,(o-formula (gvl :parent :floating-left)))
	   (:top ,(o-formula (gvl :parent :floating-top)))
	   (:width 19) (:height 19)
	   (:filling-style ,opal:gray-fill))
      (:white-field ,opal:rectangle
	   (:left ,(o-formula (+ 4 (gvl :parent :floating-left))))
	   (:top ,(o-formula (+ 4 (gvl :parent :floating-top))))
	   (:width 11) (:height 11)
	   (:filling-style ,(o-formula (if (gvl :parent :selected)
					   opal:black-fill
					   opal:white-fill))))
      (:label ,opal:rectangle
	   (:visible ,(o-formula (not (gvl :parent :selected))))
	   (:left ,(o-formula (+ 7 (gvl :parent :floating-left))))
	   (:top ,(o-formula (+ 7 (gvl :parent :floating-top))))
	   (:width 5) (:height 5)
	   (:filling-style ,(o-formula (gvl :parent :filling-style)))
	   (:line-style ,(o-formula (if (equal (gvl :filling-style)
					       opal:black-fill)
					opal:white-line
					opal:default-line-style)))))))

(create-instance 'BOX-CONSTRAINT-OBJ opal:aggregadget
   (:maybe-constant :left :top :visible)
   (:constant '(:width :height :where-attach))
   (:left 0)
   (:top 0)
   (:width 123)
   (:height 123)
   (:active (o-formula (eq (gvl :parent :active) :box)))
;   (:where-attach '(:nw :w :sw :n :c :s :ne :e :se))
   (:where-attach '(0 1 2 3 4 5 6 7 8))
   (:parts
    `((:box ,opal:rectangle
	    (:left ,(o-formula (opal:gv-center-x
				(first (gvl :parent :buttons :components)))))
	    (:top ,(o-formula (opal:gv-center-y
			       (first (gvl :parent :buttons :components)))))
	    (:width 100) (:height 100)) 
      (:buttons ,opal:aggrelist
		(:constant (t :except :left :top))
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (gvl :parent :top)))
		(:v-spacing 27) (:h-spacing 27)
		(:rank-margin 3)
		(:items 9)
		(:filling-style ,(o-formula
				  (case (gvl :parent :parent :selection)
				    (:primary opal:black-fill)
				    (t opal:white-fill))))
		(:item-prototype ,LINE-CONSTRAINT-BUTTON)
		(:interactors
		 ((:box-press ,LINE-CONSTRAINT-INTER
		      (:final-function
		       ,#'(lambda (interactor obj)
			    (s-value LINE-CON-PANEL :value nil)
			 (s-value (g-value interactor :operates-on :parent
					   :parent) 
				  :where-attach
				  (g-value obj :where-attach))
			   (ATTACH-line-CONSTRAINT interactor obj)))))))     
      (:gray-shade ,opal:rectangle
	     (:left ,(o-formula (- (gvl :parent :left) 1)))
	     (:top ,(o-formula (- (gvl :parent :top) 1)))
	     (:width ,(o-formula (+ 2 (gvl :parent :width))))
	     (:height ,(o-formula (+ 2 (gvl :parent :height))))
	     (:draw-function :and)
	     (:filling-style ,opal:gray-fill)
	     (:visible ,(o-formula (not (gvl :parent :active))))))))

(create-instance 'LINE-CONSTRAINT-OBJ opal:aggregadget
   (:maybe-constant :left :top)
   (:constant '(:visible :width :height :max-size :where-attach))
   (:left 19)
   (:top 185)
   (:width 123)
   (:height 123)
   (:max-size (o-formula (- (gvl :width) 22)))
   (:line-over nil)
   (:active (o-formula (eq (gvl :parent :active) :line)))
;   (:where-attach `(:x1 :x2))
   (:where-attach `(0 1))
   (:parts
    `((:line ,opal:line
	     (:line-over ,(o-formula (gvl :parent :line-over)))
	     (:max-size ,(o-formula (gvl :parent :max-size)))
	     (:left ,(o-formula (if (>= (gvl :line-over :width)
					(gvl :line-over :height))
				    (+ (gvl :parent :left) 11)
				    (opal:gv-center-x-is-center-of
				     (gvl :parent))) 30))
	     (:top ,(o-formula (if (>= (gvl :line-over :height)
				       (gvl :line-over :width))
				   (+ (gvl :parent :top) 11)
				   (opal:gv-center-y-is-center-of
				    (gvl :parent))) 196))
	     (:width ,(o-formula (let ((line-wd (gvl :line-over :width))
				       (line-ht (gvl :line-over :height)))
				   (if (>= line-wd line-ht)
				       (gvl :max-size)
				       (round (* (gvl :max-size)
						 (/ line-wd line-ht)))))
				 101))
	     (:height ,(o-formula (let ((line-wd (gvl :line-over :width))
					(line-ht (gvl :line-over :height)))
				   (if (>= line-ht line-wd)
				       (gvl :max-size)
				       (round (* (gvl :max-size)
						 (/ line-ht line-wd)))))
				  101))
	     (:x1 ,(o-formula (if (<= (gvl :line-over :x1)
				      (gvl :line-over :x2))
				  (gvl :left)
				  (opal:gv-right (gv :self)))
			      30))
			      
	     (:x2 ,(o-formula (if (<= (gvl :line-over :x2)
				      (gvl :line-over :x1))
				  (gvl :left)
				  (opal:gv-right (gv :self)))
			      130))
	     (:y1 ,(o-formula (if (<= (gvl :line-over :y1)
				      (gvl :line-over :y2))
				  (gvl :top)
				  (opal:gv-bottom (gv :self)))
			      196))
	     (:y2 ,(o-formula (if (<= (gvl :line-over :y2)
				      (gvl :line-over :y1))
				  (gvl :top)
				  (opal:gv-bottom (gv :self)))
			      296)))
      (:buttons ,opal:aggregadget
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (gvl :parent :top)))
		(:width ,(o-formula (gvl :parent :width)))
		(:height ,(o-formula (gvl :parent :height)))
		(:filling-style ,(o-formula (case (gvl :parent :parent
						       :selection)
					      (:primary opal:black-fill)
					      (t opal:white-fill))))
		(:parts
		 ((:endpt1 ,LINE-CONSTRAINT-BUTTON
		       (:left ,(o-formula (- (gvl :parent :parent :line :x1)
					     11)))
		       (:top ,(o-formula (- (gvl :parent :parent :line :y1)
					    11)))
		       (:where-attach 0)
		       (:filling-style ,(o-formula (gvl :parent :filling-style))))
		  (:center ,LINE-CONSTRAINT-BUTTON
		       (:left ,(o-formula (opal:gv-center-x-is-center-of
					      (gvl :parent :parent :line))))
		       (:top ,(o-formula (opal:gv-center-y-is-center-of
					     (gvl :parent :parent :line))))
		       (:where-attach 1)
		       (:visible ,(o-formula (gvl :parent :parent 
						  :middle-button-p)))
		       (:filling-style ,(o-formula (gvl :parent :filling-style))))
		  (:endpt2 ,LINE-CONSTRAINT-BUTTON
		       (:left ,(o-formula (- (gvl :parent :parent :line :x2)
					     11)))
		       (:top ,(o-formula (- (gvl :parent :parent :line :y2)
					    11)))
		       (:where-attach ,(o-formula (if (gvl :parent :center :visible)
						      2
						      1)))
		       (:filling-style ,(o-formula (gvl :parent :filling-style))))))
		(:interactors
		 ((:line-press ,LINE-CONSTRAINT-INTER
			       (:final-function
				,#'(lambda (interactor obj)
				  (s-value LINE-CON-PANEL :value nil)
				  (s-value (g-value interactor :operates-on 
						    :parent :parent) 
					   :where-attach
					   (g-value obj :where-attach))
				  (ATTACH-line-CONSTRAINT interactor obj)))
			       ))))
      (:gray-shade ,opal:rectangle
	     (:left ,(o-formula (- (gvl :parent :left) 1)))
	     (:top ,(o-formula (- (gvl :parent :top) 1)))
	     (:width ,(o-formula (+ 2 (gvl :parent :width))))
	     (:height ,(o-formula (+ 2 (gvl :parent :height))))
	     (:draw-function :and)
	     (:filling-style ,opal:gray-fill)
	     (:visible ,(o-formula (not (gvl :parent :active))))))))

(create-instance 'LINE-CON-TITLE opal:text
   (:constant '(t))
   (:left 10)
   (:top 10)
   (:font *large-bold-italic-serif-font*)
   (:string "Line Constraint Menu"))

(opal:add-component *LINE-CONSTRAINT-MENU* LINE-CON-TITLE)

(create-instance 'LINE-CON-PRIM-SEL-AGG opal:aggregadget
;  (:maybe-constant :left :top :width :height :visible)
  (:constant '(:left :top :width :height :visible))
  (:where-attach (o-formula (progn (or (gv *constraint-gadget* :obj-to-constrain)
				       (gv *constraint-gadget* :obj-to-reference))
				   nil)))
  (:active (o-formula (case (gv *constraint-gadget* :selection-type)
			    ((one-zero one-one)
			     (if (is-a-line-p (gv *constraint-gadget*
						  :obj-to-constrain))
				 :line
			         :box))
			    (t nil))))
  (:selection :primary)
  (:left 10) 
  (:top (+ (opal:bottom line-con-title) 20))
  (:parts `((:title-frame ,titled-frame
		   (:string "obj-to-constrain")
		   (:left ,(o-formula (gvl :parent :left)))
		   (:top ,(o-formula (gvl :parent :top)))
		   (:width ,(o-formula (+ (- (opal:gv-right 
					      (gvl :parent :box))
					     (gvl :parent :line :left))
					  30)))
		   (:parts (
		     (:frame :modify
			     (:height ,(o-formula
					(+ (- (opal:bottom
					       (gvl :parent :parent :box))
					      (gvl :parent :parent :box :top))
					   20))))
		     :text-frame
		     :text)))	     
	    (:line ,line-constraint-obj
		   (:middle-button-p nil)
		   (:line-over ,(o-formula (when (gvl :active)
						 (gv *constraint-gadget*
						     :obj-to-constrain))))
		   (:left ,(o-formula (+ (gvl :parent :left) 15)))
		   (:top ,(o-formula (+ (gvl :parent :top) 17))))
	    (:box ,box-constraint-obj
		  (:left ,(o-formula (+ (opal:gv-right (gvl :parent :line))
					20)))
		  (:top ,(o-formula (+ (gvl :parent :top) 17)))))))

;; demand the value of the :line-over slot in :line so that the links
;; are set up. then store model-diag-line in :line-over so :line can
;; display itself properly
(let* ((line-agg (g-value line-con-prim-sel-agg :line))
       (line (g-value line-agg :line)))
  (g-value line-agg :line-over)
  (s-value line-agg :line-over model-diag-line)
  (g-value line :x1)
  (g-value line :x2)
  (g-value line :y1)
  (g-value line :y2)
  (g-value line-con-prim-sel-agg :where-attach))

(create-instance 'LINE-CON-SEC-SEL-AGG opal:aggregadget
  (:constant '(:left :top :width :height :visible))
  (:top (+ (opal:bottom line-con-prim-sel-agg) 20))
  (:left 10)
  (:where-attach (o-formula (progn (or (gv *constraint-gadget* :obj-to-constrain)
				       (gv *constraint-gadget* :obj-to-reference))
				   nil)))
  (:active (o-formula (case (gv *constraint-gadget* :selection-type)
			    (one-one
			     (if (is-a-line-p (gv *constraint-gadget*
						  :obj-to-reference))
				 :line
			         :box))
			    (t nil))))
  (:selection :secondary)
  (:parts `((:title-frame ,titled-frame
		   (:string "obj-to-reference")
		   (:left ,(o-formula (gvl :parent :left)))
		   (:top ,(o-formula (gvl :parent :top)))
		   (:width ,(o-formula (+ (- (opal:gv-right 
					      (gvl :parent :box))
					     (gvl :parent :line :left))
					  30)))
		   (:parts (
		     (:frame :modify
			     (:height ,(o-formula
					(+ (- (opal:bottom
					       (gvl :parent :parent :box))
					      (gvl :parent :parent :box :top))
					   20))))
		     :text-frame
		     :text)))	     
	    (:line ,line-constraint-obj
		   (:middle-button-p t)
		   (:line-over ,(o-formula (when (gvl :active)
						 (gv *constraint-gadget*
						  :obj-to-reference))))
		   (:left ,(o-formula (+ (gvl :parent :left) 15)))
		   (:top ,(o-formula (+ (gvl :parent :top) 17))))
	    (:box ,box-constraint-obj
		  (:left ,(o-formula (+ (opal:gv-right (gvl :parent :line))
					20)))
		  (:top ,(o-formula (+ (gvl :parent :top) 17)))))))

;; demand the value of the :line-over slot in :line so that the links
;; are set up. then store model-diag-line in :line-over so :line can
;; display itself properly
(let* ((line-agg (g-value line-con-sec-sel-agg :line))
       (line (g-value line-agg :line)))
  (g-value line-agg :line-over)
  (s-value line-agg :line-over model-diag-line)
  (g-value line :x1)
  (g-value line :x2)
  (g-value line :y1)
  (g-value line :y2)
  (g-value line-con-sec-sel-agg :where-attach))


(opal:add-components *LINE-CONSTRAINT-MENU* LINE-CON-PRIM-SEL-AGG
		                         LINE-CON-SEC-SEL-AGG)

;; set the interim selected slots of the buttons in the secondary line menu
;; to nil, so that they will not inherit from the prototype

(dolist (obj (g-value line-CON-SEC-SEL-AGG :line :buttons :components))
	(s-value obj :interim-selected nil))
(dolist (obj (g-value line-CON-SEC-SEL-AGG :box :buttons :components)) 
	(s-value obj :interim-selected nil))

(create-instance 'LINE-CON-PANEL garnet-gadgets:text-button-panel 
   (:constant '(t))
   (:left 10)
   (:top (+ (opal:bottom line-con-sec-sel-agg) 20))
   (:value (o-formula (progn 
			(or (gv *constraint-gadget* :obj-to-constrain)
			    (gv *constraint-gadget* :obj-to-reference))
			nil)))
   (:shadow-offset 5) (:gray-width 3)
   (:text-offset 3)
   (:final-feedback-p t)
   (:items `(("unconstrain" LINE-UNCONSTRAIN-FN) 
	     ("customize" LINE-CUSTOM-FN)))
   (:interactors `(
     (:text-button-press :modify
   	(:final-function
	 ,#'(lambda (interactor final-obj-over)
	      (let* ((action (g-value final-obj-over :action))
		     (gadget (g-value interactor :operates-on))
		     (selected (g-value final-obj-over :selected))
		     (string (when selected
			       (g-value final-obj-over :string))))
	   
		;; Propagate new selection toward :value slot
		(s-value gadget :value-obj (when selected final-obj-over))
		
		;; Actually store the new value in the value slot
		(s-value gadget :value string)

		;; Global function for all items
		(kr-send gadget :selection-function gadget string)

		;; Local function assigned to item
		(when action
		  (funcall action gadget string)))))))))

(create-instance 'LINE-CON-OK-PANEL garnet-gadgets:text-button-panel
  (:constant '(t))
  (:left (o-formula (+ (opal:gv-right line-con-title) 10)))
  (:top 10)
  (:direction :horizontal)
  (:fixed-width-p nil)
  (:button-width 44)
  (:gray-width 3) (:shadow-offset 5) (:text-offset 2)
  (:selection-function
      #'(lambda (button string)
	  (cond ((string= string "OK")
		 (s-value (g-value button :window) :visible nil))
		(t
		 (set-line-constraint-feedback)))))
  (:items '("Show Constraints" "OK"))
  (:final-feedback-p nil))

(create-instance 'X-OFFSET-BOX garnet-gadgets:labeled-box
   (:constant '(t))
   (:left (+ (opal:right LINE-CON-PANEL) 20))
   (:top (g-value LINE-CON-PANEL :top))
   (:value "0")
   (:old-value "0")
   (:numerical-value (o-formula (read-from-string (gvl :value))))
   (:label-string "x-offset")
   (:selection-function #'set-x-offset))

(create-instance 'Y-OFFSET-BOX garnet-gadgets:labeled-box
   (:constant '(t))
   (:left (g-value x-offset-box :left))
   (:top (+ (opal:bottom x-offset-box) 10))
   (:value "0")
   (:old-value "0")
   (:numerical-value (o-formula (read-from-string (gvl :value))))
   (:label-string "y-offset")
   (:selection-function #'set-y-offset))

(opal:add-components *LINE-CONSTRAINT-MENU* LINE-CON-PANEL
			LINE-CON-OK-PANEL X-OFFSET-BOX Y-OFFSET-BOX)

;; create the labeled boxes that display the coordinates of a line
(create-instance 'line-position-proto garnet-gadgets:labeled-box
   (:left (o-formula (let ((offset-right (opal:gv-right x-offset-box)))
		       (if (< offset-right 223)
			   233
			   (+ offset-right 10)))))
   (:top 0)
   (:value (o-formula (let ((selected (gv *constraint-gadget* :obj-to-constrain))
			    (slot (gvl :slot)))
			(if (and selected (is-a-line-p selected))
			    (princ-to-string (gv selected slot))
			    "0"))))
   (:selection-function #'set-position-slot)
   ;; can only change a line's position when a line is the primary or
   ;; secondary selection
   (:interactors `((:text-inter :modify
		     (:active ,(o-formula (eq (gv *constraint-gadget*
						  :selection-type)
					      'one-zero))))))
   (:slot nil))

(create-instance 'x1-box line-position-proto
   (:constant '(t))
   (:top (g-value x-offset-box :top))
   (:slot :x1)
   (:label-string "x1"))

(create-instance 'y1-box line-position-proto   
   (:constant '(t))
   (:top (+ (opal:bottom x1-box) 10))
   (:slot :y1)
   (:label-string "y1"))

(create-instance 'x2-box line-position-proto
   (:constant '(t))
   (:top (+ (opal:bottom y1-box) 20))
   (:slot :x2)
   (:label-string "x2"))

(create-instance 'y2-box line-position-proto
   (:constant '(t))
   (:top (+ (opal:bottom x2-box) 10))
   (:slot :y2)
   (:label-string "y2"))

;; gray out the position boxes when they are inactive
(create-instance 'line-pos-gray-box opal:rectangle
   (:constant '(t :except :visible))
   (:draw-function :and)
   (:left (o-formula (gv x1-box :left)))
   (:top (- (g-value x1-box :top) 2))
   (:width (o-formula (- (max (opal:gv-right x1-box)
			      (opal:gv-right y1-box)
			      (opal:gv-right x2-box)
			      (opal:gv-right y2-box))
			 (gvl :left))))
   (:height (+ (- (opal:bottom y2-box) (g-value x1-box :top)) 5))
   (:filling-style opal:gray-fill)
   (:visible nil))

(opal:add-components *line-constraint-menu* x1-box y1-box x2-box y2-box
		                         line-pos-gray-box)

(create-instance 'LINE-CON-WIN inter:interactor-window
   (:title "line constraints")
   (:aggregate *line-constraint-menu*)
   (:left (g-value box-constraint-win :left))
   (:top #-apple (opal:bottom box-constraint-win)
         #+apple (g-value box-constraint-win :top))
   (:width 356)
   (:height (o-formula (+ 20 (gvl :aggregate :height))))
   (:visible nil)
;   (:double-buffered-p t)
)


(opal:update LINE-CON-WIN))

(defun line-constraint-do-stop ()
  (when (boundp 'LINE-CON-WIN) (opal:destroy LINE-CON-WIN)))
