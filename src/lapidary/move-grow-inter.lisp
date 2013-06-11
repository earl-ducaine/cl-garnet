;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;;  Move/Grow Interactor dialog box
;;;

;;;  Note: This file needs parts from the file  dialog-parts.lisp

(in-package "LAPIDARY")

(defun move-grow-inter-do-go ()
  (let ((kr::*constants-disabled* nil))
  (move-grow-inter-do-stop)

  (create-instance 'MOVE-GROW-INTER-AGG 
		   garnet-gadgets:scrolling-window-with-bars
      (:left 500)(:top 0)(:width 560)(:height 710)
      (:v-scroll-on-left-p nil)
      (:h-scroll-bar-p nil)
      (:title "Move/Grow Interactor")
      (:total-width (o-formula (+ 20 (gvl :inner-aggregate :width))))
      (:total-height (o-formula (+ 20 (gvl :inner-aggregate :height)))))

  (opal:update MOVE-GROW-INTER-AGG)
  (setf move-grow-inter-win (g-value move-grow-inter-agg :window))
  (s-value (g-value move-grow-inter-agg :inner-aggregate :window)
	   :queue '*move-grow-inter-queue*)

  (create-instance 'MOVE-GROW-INTER-MENU opal:aggregadget
   (:constant '(:left :top :width :height))
   (:left 10)
   (:top 10)
   (:parts
    `((:title ,opal:text
	  (:constant t)
          (:left ,(o-formula (gvl :parent :left)))
	  (:top ,(o-formula (gvl :parent :top)))
	  (:string "Move/Grow Interactor")
	  (:font ,*very-large-bold-italic-serif-font*))

      (:known-as ,NAME-BOX
	  (:constant t)
	  (:selection-function MOVE-GROW-INTERACTOR-NAME-FN))

      (:start-where ,START-WHERE
	  (:constant t)
          (:items (("Object to Press Over" MOVE-GROW-OBJ-PRESS-OVER-FN)
		   ("One of This Aggregate" MOVE-GROW-ONE-THIS-AGG-FN))))

      (:act-buttons ,ACT-BUTTONS
	  (:constant t)
	  (:queue *MOVE-GROW-INTER-QUEUE*)
          (:left ,(o-formula (+ 20 (opal:gv-right
				    (gvl :parent :start-where))))))

      (:line-p ,garnet-gadgets:radio-button-panel
	  (:constant (t))
	  (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :start-where)))))
	  (:direction :horizontal)
	  (:fixed-width-p NIL)
	  (:items ("Line" "Box" "<Formula>"))
	  (:selection-function MOVE-GROW-LINE-P-FN))

      (:grow-p ,garnet-gadgets:radio-button-panel
	  (:constant (t))
	  (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :line-p)))))
	  (:direction :horizontal)
	  (:fixed-width-p NIL)
	  (:items ("Grow" "Move" "<Formula>"))
	  (:selection-function MOVE-GROW-GROW-P-FN))
      (:grow-parm ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :grow-p)))))
	  (:on ,(o-formula (string/= "Move" (gvl :parent :grow-p :value))))
	  (:parts
	   ((:contents ,opal:aggregadget
         	(:constant (:left :top :width :height))
		(:top ,(o-formula (+ (gvl :parent :top) 10)))
		(:left ,(o-formula (+ (gvl :parent :left) 10)))
                (:parts
		 ((:title ,opal:text
		      (:constant (t))
                      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (gvl :parent :top)))
		      (:string "Grow Parameters")
		      (:font ,*bold-font*))
		  (:change-size ,garnet-gadgets:radio-button-panel
		      (:constant (t))
                      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (+ 20 (opal:gv-bottom
					       (gvl :parent :title)))))
		      (:items ("Change Width" "Change Height" 
			       "Change Width and Height" "<Formula>"))
		      (:selection-function grow-parms-final-fn)
		      (:interactors
		       ((:radio-button-press :modify
			   (:active ,(o-formula (gvl :operates-on :parent
						     :parent :on)))))))
		  (:line-text ,opal:text
		      (:constant (t))
		      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (+ 20 (opal:gv-bottom
					       (gvl :parent :change-size)))))
		      (:string "Growing Line")
		      (:font ,opal:default-font))
		  (:min-length ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 20 (gvl :parent :left))))
		      (:top ,(o-formula (+ 10 (opal:gv-bottom
					       (gvl :parent :line-text)))))
		      (:min-frame-width 50)
		      (:old-value "")
		      (:label-string "Min-Length")
		      (:label-font ,opal:default-font)
		      (:selection-function move-grow-min-length-fn)
		      (:value ,(o-formula
				(let ((val (gvl :parent :parent :min-length)))
				  (if (numberp val) (princ-to-string val)))))
		      (:interactors
		       ((:text-inter :modify
			   (:active ,(o-formula (gvl :operates-on :parent
						     :parent :on)))))))
		  (:non-line-text ,opal:text
		      (:constant (t))
                      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (+ 20 (opal:gv-bottom
					       (gvl :parent :min-length)))))
		      (:string "Growing Non-Line")
		      (:font ,opal:default-font))
		  (:min-width ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 20 (gvl :parent :left))))
		      (:top ,(o-formula (+ 10 (opal:gv-bottom
					       (gvl :parent :non-line-text)))))
		      (:min-frame-width 50)
		      (:old-value "")
		      (:selection-function move-grow-min-width-fn)
		      (:label-string "Min-Width  ")
		      (:label-font ,opal:default-font)
		      (:value ,(o-formula
				(let ((val (gvl :parent :parent :min-width)))
				  (if (numberp val) (princ-to-string val)))))
		      (:interactors
		       ((:text-inter :modify
			   (:active ,(o-formula (gvl :operates-on :parent
						     :parent :on)))))))
		  (:min-height ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 20 (gvl :parent :left))))
		      (:top ,(o-formula (+ 10 (opal:gv-bottom
					       (gvl :parent :min-width)))))
		      (:min-frame-width 50)
		      (:old-value "")
		      (:selection-function move-grow-min-height-fn)
		      (:label-string "Min-Height")
		      (:label-font ,opal:default-font)
		      (:interactors
		       ((:text-inter :modify
			   (:active ,(o-formula (gvl :operates-on :parent
						     :parent :on))))))
		      (:value ,(o-formula
				(let ((val (gvl :parent :parent :min-height)))
				  (if (numberp val)
				      (princ-to-string val)))))))))
	    (:frame ,opal:rectangle
		(:constant (t))
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (gvl :parent :top)))
		(:width ,(o-formula (+ 20 (gvl :parent :contents :width))))
		(:height ,(o-formula (+ (gvl :parent :contents :height)
					20))))
	    (:gray-out-frame ,opal:rectangle
		(:constant (t :except :visible))
		(:visible ,(o-formula (not (gvl :parent :on))))
		(:filling-style ,opal:gray-fill)
		(:draw-function :and)
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (gvl :parent :top)))
		(:width ,(o-formula (+ 20 (gvl :parent :contents :width))))
		(:height ,(o-formula (+ (gvl :parent :contents :height)
					20)))))))

      (:move-parm ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 20 (opal:gv-right (gvl :parent :grow-parm)))))
	  (:top ,(o-formula (gvl :parent :grow-parm :top)))
	  (:on ,(o-formula (string/= "Grow" (gvl :parent :grow-p :value))))
	  (:parts
	   ((:contents ,opal:aggregadget
	        (:constant (:left :top :width :height))
		(:top ,(o-formula (+ (gvl :parent :top) 10)))
		(:left ,(o-formula (+ (gvl :parent :left) 10)))
                (:parts
		 ((:title ,opal:text
		      (:constant (t))
                      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (gvl :parent :top)))
		      (:string "Move Parameters")
		      (:font ,*bold-font*))
		  (:change-size ,garnet-gadgets:radio-button-panel
		      (:constant (t))
                      (:left ,(o-formula (gvl :parent :left)))
		      (:top ,(o-formula (+ 20 (opal:gv-bottom
					       (gvl :parent :title)))))
		      (:items ("Change Left" "Change Top" 
			       "Change Left and Top" "<Formula>"))
		      (:selection-function move-parms-final-fn)
		      (:interactors
		       ((:radio-button-press :modify
			   (:active ,(o-formula (gvl :operates-on :parent
						     :parent :on))))))))))
	    (:frame ,opal:rectangle
		(:constant (t))
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (gvl :parent :top)))
		(:width ,(o-formula (+ 20 (gvl :parent :contents :width))))
		(:height ,(o-formula (+ (gvl :parent :contents :height)
					20))))
	    (:gray-out-frame ,opal:rectangle
		(:constant (t :except :visible))
		(:visible ,(o-formula (not (gvl :parent :on))))
		(:filling-style ,opal:gray-fill)
		(:draw-function :and)
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (gvl :parent :top)))
		(:width ,(o-formula (+ 20 (gvl :parent :contents :width))))
		(:height ,(o-formula (+ (gvl :parent :contents :height)
					20)))))))
      (:obj-to-change ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 20 (opal:gv-right (gvl :parent :grow-parm)))))
	  (:top ,(o-formula (+ (opal:gv-bottom
				(gvl :parent :move-parm)) 10)))
	  (:parts
	   ((:titled-frame ,titled-frame
			   (:constant t)
			   (:string ":obj-to-change"))
	    (:contents ,garnet-gadgets:radio-button-panel
		(:constant t)
		(:value ,(o-formula (gvl :parent :value)))
		(:top ,(o-formula (+ 20 (gvl :parent :top))))
		(:left ,(o-formula (+ 15 (gvl :parent :left))))
		(:selection-function MOVE-GROW-OBJ-TO-CHANGE-FN)
		(:items (("Result of :start-where")
			 ("Change this object")
			 ("<Formula>")))))))
	    
      
      (:final-function ,garnet-gadgets:labeled-box
	  (:constant t)
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :grow-parm)))))
	  (:label-string "Final Function:")
	  (:value "")
	  (:min-frame-width 150)
	  (:selection-function MOVE-GROW-FINAL-FUNCTION-FN))

      (:feedback-obj ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :final-function)))))
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":feedback-obj"))
	    (:contents ,opal:aggregadget
	        (:constant (:left :top :width :height))
                (:width ,(o-formula (+ 10 (gvl :feedback-obj-box :width)
				       (gvl :other-button :width))))
		(:height ,(o-formula (gvl :other-button :height)))
		(:value ,(o-formula (gvl :parent :value)))
		(:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
		 ((:feedback-obj-box ,SELECT-BOX
		      (:constant (t))
		      (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		      (:string "Interim Feedback")
		      (:min-frame-width 125)
		      (:selection-function
		       MOVE-GROW-FEEDBACK-OBJ-FN))
		  (:other-button ,garnet-gadgets:radio-button-panel
		      (:constant (t))
		      (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
                      (:left ,(o-formula (+ (opal:gv-right
					     (gvl :parent :feedback-obj-box))
					     10)))
		      (:direction :vertical)
		      (:items ("Change Original" move-grow-nil-feedback-obj-fn)
			      ("<Formula>" move-grow-formula-feedback-obj-fn))
		      (:value ,(o-formula (let ((value (gvl :parent :value)))
					    (if (string= value
							 "Interim Feedback")
						nil
					        value)))))))))))

      (:attach-point ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :feedback-obj)))))
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
		(:constant (t))
	        (:string ":attach-point"))
	    (:contents ,opal:aggregadget
	        (:parts
		 ((:box ,opal:rectangle
		      (:constant (t))
		      (:shadow ,(o-formula 
				 (gv (first (gvl :parent :box-buttons :components))
					  :shadow)))
		      (:left ,(o-formula (opal:gv-center-x (gvl :shadow))))
		      (:top ,(o-formula (opal:gv-center-y (gvl :shadow))))
		      (:width 112) (:height 112))
		  (:box-buttons ,opal:aggrelist
		      (:constant (t))
		      (:left ,(o-formula (+ (gvl :parent :parent :left) 15)))
		      (:top ,(o-formula (+ (gvl :parent :parent :top) 20)))
		      (:items (:nw :n :ne :w :c :e :sw :s :se))
		      (:item-prototype
		       (,lap-radio-button
			 (:attach-point ,(o-formula (nth (gvl :rank) 
							 (gvl :parent :items))))))
		      (:direction :horizontal)
		      (:v-spacing 27) 
		      (:h-spacing 27)
		      (:rank-margin 3))
		  (:line ,opal:line
		      (:constant (t))
		      (:x1 ,(o-formula (opal:gv-center-x
					(gvl :parent :line-buttons :nw))))
		      (:y1 ,(o-formula (opal:gv-center-y
					(gvl :parent :line-buttons :nw))))
		      (:x2 ,(o-formula (+ 100 (gvl :x1))))
		      (:y2 ,(o-formula (+ 100 (gvl :y1)))))
		  (:line-buttons ,opal:aggregadget
		      (:constant (:left :top :width :height))
		      (:left ,(o-formula (+ 50 (opal:gv-right
						(gvl :parent :box-buttons)))))
		      (:top ,(o-formula (gvl :parent :box-buttons :top)))
		      (:parts
		       ((:nw ,LAP-RADIO-BUTTON
			     (:constant (t))
			     (:left ,(o-formula (gvl :parent :left)))
			     (:top ,(o-formula (gvl :parent :top)))
			     (:attach-point 1))
			(:c ,LAP-RADIO-BUTTON
			     (:constant (t))
			    (:left ,(o-formula (+ (gvl :parent :left) 50)))
			    (:top ,(o-formula (+ (gvl :parent :top) 50)))
			    (:visible
			     ,(o-formula
			       (string= (gvl :parent :parent :parent :parent
					     :grow-p :value) "Move")))
			    (:attach-point :center))
			(:se ,LAP-RADIO-BUTTON
			     (:constant (t))
			     (:left ,(o-formula (+ (gvl :parent :left) 100)))
			     (:top ,(o-formula (+ (gvl :parent :top) 100)))
			     (:attach-point 2)))))
		  (:where-hit ,LABELED-LAP-RADIO-BUTTON
		      (:constant (t))
		      (:left ,(o-formula (+ (gvl :parent :parent :left) 30)))
		      (:top ,(o-formula 
			      (+ (opal:gv-bottom (gvl :parent :box-buttons)) 27)))
		      (:attach-point :where-hit)
		      (:string "Nearest Point"))
		  (:formula ,LABELED-LAP-RADIO-BUTTON
		      (:constant (t))
		      (:left ,(o-formula (- (opal:gv-right (gvl :parent :line-buttons)) 
					    (gvl :width)
					    30)))
		      (:top ,(o-formula 
			      (+ (opal:gv-bottom (gvl :parent :box-buttons)) 27)))
		      (:attach-point :formula)
		      (:string "<Formula>"))
		  (:feedback ,lap-radio-button-feedback)))
		 (:interactors
		  ((:attach-pt-press ,inter:button-interactor
			    (:window ,(o-formula (gv-local :self :operates-on
							   :window)))
			    (:final-function move-grow-attach-point-fn)
			    (:final-feedback-obj 
			     ,(o-formula (gvl :operates-on :feedback)))
			    (:start-where
			     ,(o-formula (list :leaf-element-of (gvl :operates-on)
					       :type lap-radio-button)))
			    (:how-set :toggle))))))))
  
      (:event-panel ,event-panel
		    (:constant (t))
		    (:left ,(o-formula (+ 20 (gvl :parent :left))))
		    (:top ,(o-formula (+ (opal:gv-bottom
					  (gvl :parent :attach-point))
					 10)))
		    (:queue *MOVE-GROW-INTER-QUEUE*)))))

(opal::fix-update-slots (g-value move-grow-inter-menu :start-where :contents
				:select-box-panel))
(opal::fix-update-slots (g-value move-grow-inter-menu :attach-point :contents
				:box-buttons))
(opal:add-component (g-value MOVE-GROW-INTER-AGG :inner-aggregate)
		    MOVE-GROW-INTER-MENU)
(opal:update MOVE-GROW-INTER-AGG)))


