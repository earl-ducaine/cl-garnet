;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "LAPIDARY")

(create-instance 'rectangle-feedback opal:rectangle
		     (:draw-function :xor)
		     (:fast-redraw-p t)
		     (:name "Interim Rect feedback")
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:width (o-formula (third (gvl :box))))
		     (:height (o-formula (fourth (gvl :box))))
		     (:visible nil)
		     (:box '(0 0 0 0))
		     (:line-style opal:dashed-line))
(create-instance 'line-feedback opal:line
		     (:draw-function :xor)
		     (:fast-redraw-p t)
		     (:name "Interim Line feedback")
		     (:x1 (o-formula (first (gvl :points))))
		     (:y1 (o-formula (second (gvl :points))))
		     (:x2 (o-formula (third (gvl :points))))
		     (:y2 (o-formula (fourth (gvl :points))))
		     (:visible nil)
		     (:points '(0 0 0 0))
		     (:line-style opal:dashed-line))

(create-instance 'roundtangle-feedback opal:roundtangle
		     (:draw-function :xor)
		     (:fast-redraw-p t)
		     (:name "Interim Roundtangle feedback")
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:width (o-formula (third (gvl :box))))
		     (:height (o-formula (fourth (gvl :box))))
		     (:visible nil)
		     (:box '(0 0 0 0))
		     (:line-style opal:dashed-line))

(create-instance 'circle-feedback opal:circle
		     (:draw-function :xor)
		     (:fast-redraw-p t)
		     (:name "Interim Circle feedback")
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:width (o-formula (third (gvl :box))))
		     (:height (o-formula (fourth (gvl :box))))
		     (:visible nil)
		     (:box '(0 0 0 0))
		     (:line-style opal:dashed-line))

(create-instance 'text-feedback opal:cursor-multi-text 
		     (:string "")
		     (:cursor-index nil)
;		     (:draw-function :xor)
		     (:draw-function :copy)
		     (:fast-redraw-p nil)
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:name "Interim text feedback")
		     (:visible (o-formula (gvl :obj-over)))
		     (:font (o-formula (gv text-properties-win :value))))

(create-instance 'button-feedback opal:rectangle
		 (:draw-function :xor)
		 (:fast-feedback-p t)
		 (:visible (o-formula (gvl :obj-over)))
		 (:line-style nil)
		 (:filling-style opal:black-fill)
		 (:white-field (o-formula (gvl :obj-over :white-field)))
		 (:left (o-formula (1+ (gvl :white-field :left))))
		 (:top (o-formula (1+ (gvl :white-field :top))))
		 (:width (o-formula (- (gvl :white-field :width) 2)))
		 (:height (o-formula (- (gvl :white-field :height) 2))))


#|
(create-instance 'agg-sel-feedback-circle opal:aggregadget
  (:left 0) (:top 0)
  (:width *agg-sel-circle-size*) (:height *agg-sel-circle-size*)
  (:filling-style opal:black-fill)
  (:parts `((:circle ,opal:circle
	       (:left ,(o-formula (gvl :parent :left)))
	       (:top ,(o-formula (gvl :parent :top)))
	       (:width ,*agg-sel-circle-size*)
	       (:height ,*agg-sel-circle-size*)
;	       (:fast-feedback-p t)
;	       (:draw-function :xor)
	       (:filling-style ,(o-formula (gvl :parent :filling-style)))
	       (:line-style ,(o-formula (if (eq (gvl :filling-style) 
						opal:black-fill)
					    nil
					    opal:thin-line))))
	    (:label ,opal:text
	       (:left ,(o-formula (opal:gv-center-x-is-center-of 
				   (gvl :parent :circle))))
	       (:top ,(o-formula (opal:gv-center-y-is-center-of 
				  (gvl :parent :circle))))
	       (:string ,(o-formula (gvl :parent :parent :string)))
;	       (:fast-feedback-p t)
;	       (:draw-function :xor)))))
	       ))))
|#

(create-instance 'undersized-feedback-obj garnet-gadgets:arrow-line
  (:x2 (o-formula (gvl :obj-over :left)))
  (:y2 (o-formula (opal:gv-center-y (gvl :obj-over))))
  (:x1 (o-formula (- (gvl :x2) 20)))
  (:y1 (o-formula (gvl :y2)))
  (:line-style (o-formula (if (is-a-p (gvl :obj-over) opal:aggregate)
			      opal:line-2
			      opal:thin-line)))
  (:filling-style opal:black-fill)
  (:open-p (o-formula (if (eq (gvl :filling-style) opal:black-fill)
			  t nil)))
  (:visible (o-formula (gvl :obj-over))))

;; feedback objects for moving non-line objects

;; the formulas are set up so that if the user selects one of the
;; object's corners, both the left and top change. If the user
;; selects one of the sides, then either the left or top changes, 
;; depending on which is appropriate

(create-instance 'move-box-feedback opal:rectangle
  (:draw-function :xor)
  (:fast-redraw-p t)
  (:left (o-formula (case (gvl :where-attach) 
		      ((list :n :s) 
		       (gvl :obj-over :left))
		      (t 
		       (first (gvl :box))))))
  (:top (o-formula (case (gvl :where-attach)
		     ((list :e :w)
		      (gvl :obj-over :top))
		     (t
		      (second (gvl :box))))))
  (:width (o-formula 
	   (if (is-a-p (gvl :obj-over) opal:circle)
	       (min (third (gvl :box))
		    (fourth (gvl :box)))
	     (third (gvl :box)))))
  (:height (o-formula 
	    (if (is-a-p (gvl :obj-over) opal:circle)
		(min (third (gvl :box))
		     (fourth (gvl :box)))
	      (fourth (gvl :box)))))
;  (:width (o-formula (gvl :obj-over :width)))
;  (:height (o-formula (gvl :obj-over :height)))
  (:visible (o-formula (gvl :obj-over)))
  (:line-style opal:dashed-line))

;; the formulas are set up so that if the user selects one of the
;; object's corners, both the height and width change. If the user
;; selects one of the sides, then either the height or width changes, 
;; depending on which is appropriate

(create-instance 'grow-box-feedback opal:rectangle
  (:draw-function :xor)
  (:fast-redraw-p t)
  (:left (o-formula (case (gvl :where-attach) 
		      ((list :n :s) 
		       (gvl :obj-over :left))
		      (t 
		       (first (gvl :box))))))
  (:top (o-formula (case (gvl :where-attach)
		     ((list :e :w)
		      (gvl :obj-over :top))
		     (t
		      (second (gvl :box))))))
  (:width (o-formula (case (gvl :where-attach) 
		      ((list :n :s) 
		       (if (is-a-p (gvl :obj-over) opal:circle)
			   (min (gvl :obj-over :width)
				(fourth (gvl :box)))
			   (gvl :obj-over :width)))
		      (t 
		       (if (is-a-p (gvl :obj-over) opal:circle)
			   (min (third (gvl :box))
				(fourth (gvl :box)))
			   (third (gvl :box)))))))
  (:height (o-formula (case (gvl :where-attach)
		     ((list :e :w)
		      (if (is-a-p (gvl :obj-over) opal:circle)
			  (min (gvl :obj-over :height)
			       (fourth (gvl :box)))
			  (gvl :obj-over :height)))
		     (t
		      (if (is-a-p (gvl :obj-over) opal:circle)
			  (min (third (gvl :box))
			       (fourth (gvl :box)))
			  (fourth (gvl :box)))))))
  (:visible (o-formula (gvl :obj-over)))
  (:line-style opal:dashed-line))

;; feedback for moving line objects

(create-instance 'move-grow-line-feedback opal:line
  (:draw-function :xor)
  (:fast-redraw-p t)
  (:x1 (o-formula (first (gvl :points))))
  (:y1 (o-formula (second (gvl :points))))
  (:x2 (o-formula (third (gvl :points))))
  (:y2 (o-formula (fourth (gvl :points))))
  (:visible (o-formula (gvl :obj-over)))
  (:line-style opal:dashed-line))

(create-instance 'selection-box opal:rectangle
	(:width sel-box-size)
	(:height sel-box-size)
	(:draw-function :xor)
;	(:fast-redraw-p t)
	(:filling-style (o-formula (gvl :parent :filling-style)))
	(:line-style (o-formula (gvl :parent :line-style))))

(create-instance 'line-selection-boxes opal:aggregadget
    (:obj-over NIL)
    (:x1 (o-formula (- (gvl :obj-over :x1) sel-box-sizeD2) 0))
    (:y1 (o-formula (- (gvl :obj-over :y1) sel-box-sizeD2) 0))
    (:x2 (o-formula (- (gvl :obj-over :x2) sel-box-sizeD2) 0))
    (:y2 (o-formula (- (gvl :obj-over :y2) sel-box-sizeD2) 0))
    (:line-style (o-formula (if (eql (gvl :filling-style) opal:black-fill)
				nil
				opal:thin-line)))
    (:visible (o-formula (gvl :obj-over)))
    (:parts `((:box1 ,selection-box
		   (:left ,(o-formula (gvl :parent :x1)))
		   (:top ,(o-formula (gvl :parent :y1)))
		   (:where-attach 1))
	      (:box2 ,selection-box
		   (:left ,(o-formula (gvl :parent :x2)))
		   (:top ,(o-formula (gvl :parent :y2)))
		   (:where-attach 2))
	      (:boxcenter ,selection-box
		   (:left ,(o-formula (+ (gvl :parent :x1)
					(floor (- (gvl :parent :x2)
						  (gvl :parent :x1))
					       2))))
		   (:top ,(o-formula (+ (gvl :parent :y1)
				       (floor (- (gvl :parent :y2)
						 (gvl :parent :y1))
					      2))))
		   (:where-attach :center)))))


(create-instance 'dashed-rectangle opal:rectangle
  (:line-style opal:dashed-line))

(create-instance 'aggrelist-feedback opal:aggrelist
  (:left (o-formula (first (gvl :box))))
  (:top (o-formula (second (gvl :box))))
  (:width (o-formula (third (gvl :box))))
  (:height (o-formula (fourth (gvl :box))))
  (:direction (o-formula (if (eq (gvl :shape-menu :name) :horizontal-aggrelist)
			    :horizontal
			    :vertical) :horizontal))
  (:item-prototype dashed-rectangle)
  (:shape-menu nil)
  (:box '(0 0 20 20))
  (:visible nil)
  (:items 1))
