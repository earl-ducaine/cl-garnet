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
;;; this file contains the parts used to construct the gadgets in the
;;; box constraint menu
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Change Log
;;;
;;; 5/10/93 bvz Created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")


(create-instance 'SLOT-BOX garnet-gadgets:labeled-box
   (:left (o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
   (:top (o-formula (gvl :parent :top)))
   (:label-font *slot-font*)
   (:slot nil)
   (:value (o-formula (let ((selected (gv *constraint-gadget* :obj-to-constrain))
			    (slot (gvl :slot)))
			(if (and selected (not (is-a-line-p selected)))
			    (princ-to-string (gv selected slot))
			    "0"))))
   (:interactors `((:text-inter :modify
		     (:active ,(o-formula (eq (gv *constraint-gadget*
						  :selection-type)
					      'one-zero))))))
   (:min-frame-width 30))


(create-instance 'OFFSET-BOX garnet-gadgets:labeled-box
   (:maybe-constant (append (g-value garnet-gadgets:labeled-box :maybe-constant)
			    '(:label-string :label-font)))
   (:left (o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
   (:top (o-formula (+ 5 (opal:gv-bottom (gvl :parent :slot-box)))))
   (:label-string "offset")
   (:label-font opal:default-font)
   (:old-value "0")
   (:value "0")
   (:min-frame-width 30))


(create-instance 'CON-PANEL garnet-gadgets:text-button-panel
   (:width (o-formula (+ (gvl :shadow-offset) (gvl :fixed-width-size))))
   (:height (o-formula (+ (* 2 (+ (gvl :fixed-height-size)
				  (gvl :shadow-offset)))
			  (gvl :v-spacing))))
   (:shadow-offset 5) (:gray-width 3) (:text-offset 3)
   (:value (o-formula (progn 
			(or (gv *constraint-gadget* :obj-to-constrain)
			    (gv *constraint-gadget* :obj-to-reference))
			nil)))
   (:final-feedback-p t)
   (:items `(("unconstrain" remove-constraint)
	     ("customize" create-custom-constraint)))
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



(create-instance 'SCALE-BOX garnet-gadgets:labeled-box
   (:label-string "Scale")
   (:label-font opal:default-font)
   (:value "1")
   (:old-value "1")
   (:min-frame-width 30))


(create-instance 'DIFFERENCE-BOX garnet-gadgets:labeled-box
   (:left (o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
   (:top (o-formula (+ 5 (opal:gv-bottom (gvl :parent :scale-box)))))
   (:label-string "Difference
in pixels")
   (:label-font opal:default-font)
   (:old-value "0")
   (:value "0")
   (:min-frame-width 30)
   (:parts `((:label-text ,opal:multi-text :inherit (:left :top :string :font))
	     :field-text :frame)))


(create-instance 'BOX-CONSTRAINT-BUTTON opal:aggregadget
   (:maybe-constant :left :top :width :height :where-attach :visible)
   (:floating-left (o-formula (+ (gvl :left)
				 (if (gvl :interim-selected) 4 0))))
   (:floating-top (o-formula (+ (gvl :top)
				(if (gvl :interim-selected) 4 0))))
   (:width 23) (:height 23)
   (:selected (o-formula (progn (or (gv *constraint-gadget* :obj-to-constrain)
				    (gv *constraint-gadget* :obj-to-reference))
				nil)))
   (:where-attach (o-formula (nth (gvl :rank)
				  (gvl :parent :parent :where-attach))))
   (:parts
    `((:shadow ,opal:rectangle
	   (:constant (:visible))
	   (:left ,(o-formula (+ 4 (gvl :parent :left))))
	   (:top ,(o-formula (+ 4 (gvl :parent :top))))
	   (:width 19) (:height 19)
	   (:filling-style ,opal:black-fill)
	   (:line-style NIL))
      (:gray-outline ,opal:rectangle
	   (:constant (:visible))
	   (:left ,(o-formula (gvl :parent :floating-left)))
	   (:top ,(o-formula (gvl :parent :floating-top)))
	   (:width 19) (:height 19)
	   (:filling-style ,opal:gray-fill))
      (:white-field ,opal:rectangle
	   (:constant (:visible))
	   (:left ,(o-formula (+ 4 (gvl :parent :floating-left))))
	   (:top ,(o-formula (+ 4 (gvl :parent :floating-top))))
	   (:width 11) (:height 11)
	   (:filling-style ,(o-formula (if (gvl :parent :selected)
					   opal:black-fill
					   opal:white-fill))))
      (:label ,opal:rectangle
	   (:left ,(o-formula (+ 7 (gvl :parent :floating-left))))
	   (:top ,(o-formula (+ 7 (gvl :parent :floating-top))))
	   (:width 5) (:height 5)
	   (:line-style NIL)
	   (:filling-style ,opal:black-fill)
	   (:visible ,(o-formula (not (gvl :parent :selected))))))))


(create-instance 'BOX-CONSTRAINT-INTER inter:button-interactor
   (:window (o-formula (gvl :operates-on :window)))
   (:start-where (o-formula (list :element-of (gvl :operates-on)
				  :type BOX-CONSTRAINT-BUTTON)))
   (:how-set :set))


(create-instance 'CONSTRAINT-ARROW garnet-gadgets:arrow-line
   (:filling-style opal:black-fill)
   (:parts
    `(:line
      (:arrowhead :modify (:diameter 7)))))


(create-instance 'DIMENSION-ARROW garnet-gadgets:double-arrow-line
   (:parts
    `(:line
      (:arrowhead1 :modify (:diameter 7) (:length 7))
      (:arrowhead2 :modify (:diameter 7) (:length 7)))))


(create-instance 'SEL-BOX opal:rectangle
   (:left (o-formula (- (gvl :center-x) 2)))
   (:top (o-formula (- (gvl :center-y) 2)))
   (:width 10) (:height 10))
