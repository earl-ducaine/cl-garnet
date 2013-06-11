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

;;; CHANGE LOG
;;;
;;; 07/14/93 amickish - Declared ANGLE-INTER-MENU special in
;;;                     Angle-Feedback-Obj-Fn
;;; 08/25/92 amickish - Added proclaim

(in-package "LAPIDARY")

(declaim (special angle-inter-win))
(defvar *ANGLE-INTER-QUEUE* NIL)

(defun angle-inter-do-stop ()
  (when (boundp 'ANGLE-INTER-WIN) (opal:destroy ANGLE-INTER-WIN)))

(defmacro ANGLE-START-WHERE ()
  `(g-value ANGLE-INTER-MENU :start-where))
(defmacro ANGLE-FEEDBACK-OBJ ()
  `(g-value ANGLE-INTER-MENU :feedback-obj))
(defmacro ANGLE-OTHER-BOX ()
  `(g-value ANGLE-INTER-MENU :start-where :contents :other-box))
(defmacro ANGLE-OTHER-BUTTON ()
  `(g-value ANGLE-INTER-MENU :start-where :contents :other-button))
(defmacro ANGLE-CENTER-OF-ROTATION ()
  `(g-value ANGLE-INTER-MENU :center-of-rotation))

(defun ANGLE-INTERACTOR-NAME-FN (gadget interactor-name)
  (declare (special angle-inter-menu))
  (declare (ignore gadget))
  (dialog-enqueue :known-as
		  (if (string/= "" interactor-name)
		      (read-from-string
		       (concatenate 'string ":" interactor-name)))
		  *ANGLE-INTER-QUEUE*))

;;;    :start-where is in a single object
(defun ANGLE-OBJ-PRESS-OVER-FN (obj-box button-label)
  (declare (special angle-inter-menu))
  (declare (ignore obj-box))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(start-where (ANGLE-START-WHERE)))
    (if selection
	(progn
	  (s-value start-where :field-string (name-for-schema selection))
	  (s-value start-where :value button-label)
	  (s-value start-where :type nil)
	  (dialog-enqueue :start-where
			  `(:in-box ,selection)
			  *ANGLE-INTER-QUEUE*))
	(progn
	  (s-value start-where :field-string nil)
	  (s-value start-where :value nil)
	  (s-value start-where :type nil)))))

(defun ANGLE-START-ANYWHERE (button button-label)
  (declare (ignore button))
  (declare (special angle-inter-menu))
  (let ((start-where (ANGLE-START-WHERE)))
    (s-value start-where :field-string nil)
    (s-value start-where :type nil)
    (s-value start-where :value button-label)
    (dialog-enqueue :start-where t *angle-inter-queue*)))

(defun ANGLE-OBJ-TO-CHANGE-FN (panel value)
  (declare (special angle-inter-menu))
  (s-value (g-value panel :parent) :value value) 
  (if (string= value "<Formula>")
      (progn
	(create-custom-inter-constraint (g-value panel :window :inter) 
					:obj-to-change 
					'*angle-inter-queue*))
      (dialog-enqueue :obj-to-change
		      nil
		      *ANGLE-INTER-QUEUE*)))

(defun ANGLE-FEEDBACK-OBJ-FN (feedback-obj-box button-label)
  (declare (special *selection-info* ANGLE-INTER-MENU))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (cond ((null selection)
	   (s-value (ANGLE-FEEDBACK-OBJ) :field-string nil)
	   (s-value (ANGLE-FEEDBACK-OBJ) :value nil)
	   (lapidary-error "please make a selection, then press the
interim feedback button again"))
	((null (cdr selection)) ; only one selection
	 (s-value (ANGLE-FEEDBACK-OBJ) :field-string
		  (name-for-schema (car selection)))
	 (s-value (ANGLE-FEEDBACK-OBJ) :value button-label)
	 (dialog-enqueue :feedback-obj selection *ANGLE-INTER-QUEUE*))
	(t ; multiple selections
	 (s-value (ANGLE-FEEDBACK-OBJ) :field-string
		  (princ-to-string selection))
	 (s-value (ANGLE-FEEDBACK-OBJ) :value button-label)
	 ;; pop up C32 and ask the user to enter a formula that
	 ;; selects which feedback object to use
	 (get-inter-feedback-formula (g-value feedback-obj-box :window :inter)
				     :feedback-obj
				     selection
				     '*angle-inter-queue*)))))

(defun ANGLE-NIL-FEEDBACK-OBJ-FN (button button-label)
  (declare (special angle-inter-menu))
  (declare (ignore button))
  (dialog-enqueue :feedback-obj NIL *ANGLE-INTER-QUEUE*)
  (s-value (ANGLE-FEEDBACK-OBJ) :field-string nil)
  (s-value (ANGLE-FEEDBACK-OBJ) :value button-label))

(defun ANGLE-FINAL-FUNCTION-FN (labeled-box string)
  (declare (ignore labeled-box))
  (declare (special angle-inter-menu))
  (dialog-enqueue :final-function
		  (if (string= "" string)
		      nil
		      (read-from-string string))
		  *ANGLE-INTER-QUEUE*))

(defun angle-attach-point-fn (inter button)
  (declare (special angle-inter-menu))
  (s-value (ANGLE-CENTER-OF-ROTATION) :value button)
  (s-value (ANGLE-CENTER-OF-ROTATION) :x "")
  (s-value (ANGLE-CENTER-OF-ROTATION) :y "")
  (let ((attach-point (g-value button :attach-point)))
    (if (eq attach-point :formula)
      (progn
	(create-custom-inter-constraint (g-value inter :window :inter) 
					:center-of-rotation
					'*angle-inter-queue*))
      (dialog-enqueue :center-of-rotation
		      attach-point
		      *ANGLE-INTER-QUEUE*))))

(defun angle-x-fn (gadget value)  
  (declare (special angle-inter-menu))

  ;; first determine if the value is valid
  (let* ((x (read-from-string value))
	(y-value (g-value (ANGLE-CENTER-OF-ROTATION) :y))
	(y (if (string= y-value "") 0 (read-from-string y-value)))
	(selection (g-value (ANGLE-CENTER-OF-ROTATION) :value)))

    (when (not (gg:valid-integer-p gadget value))
	  (return-from angle-x-fn))

    (s-value (ANGLE-CENTER-OF-ROTATION) :x value)
    (dialog-enqueue :center-of-rotation (list x y) *angle-inter-queue*)
    
    ;; deselect previous selection
    (when selection
	  (s-value (ANGLE-CENTER-OF-ROTATION) :value nil)
	  (s-value (g-value (ANGLE-CENTER-OF-ROTATION) :contents :feedback)
		   :obj-over nil))))

(defun angle-y-fn (gadget value)  
  (declare (special angle-inter-menu))

  ;; first determine if the value is valid
  (let* ((y (read-from-string value))
	(x-value (g-value (ANGLE-CENTER-OF-ROTATION) :x))
	(x (if (string= x-value "") 0 (read-from-string x-value)))
	(selection (g-value (ANGLE-CENTER-OF-ROTATION) :value)))

    (when (not (gg:valid-integer-p gadget value))
	  (return-from angle-y-fn))

    (s-value (ANGLE-CENTER-OF-ROTATION) :y value)
    (dialog-enqueue :center-of-rotation (list x y) *angle-inter-queue*)
    
    ;; deselect previous selection
    (when selection
	  (s-value (ANGLE-CENTER-OF-ROTATION) :value nil)
	  (s-value (g-value (ANGLE-CENTER-OF-ROTATION) :contents :feedback)
		   :obj-over nil))))

(defun angle-inter-do-go ()
  (let ((kr::*constants-disabled* nil))
  (angle-inter-do-stop)

  (create-instance 'ANGLE-INTER-WIN inter:interactor-window
		   (:title "angle interactor")
		   (:left #-apple 500 #+apple 200)
		   (:top  #-apple 0   #+apple 50)
		   (:width 545)(:height 692)
		   (:queue '*angle-inter-queue*))
  (opal:update ANGLE-INTER-WIN)

  (create-instance 'ANGLE-INTER-MENU opal:aggregadget
   (:constant '(:left :top :width :height))
   (:left 10)
   (:top 10)
   (:parts
    `((:title ,opal:text
	  (:constant (t))
          (:left ,(o-formula (gvl :parent :left)))
	  (:top ,(o-formula (gvl :parent :top)))
	  (:string "Angle Interactor")
	  (:font ,*very-large-bold-italic-serif-font*))

      (:known-as ,NAME-BOX
	  (:constant (t))
	  (:selection-function ANGLE-INTERACTOR-NAME-FN))

      (:start-where ,START-WHERE
	  (:constant (t))
	  (:items (("Object to Press Over" ANGLE-OBJ-PRESS-OVER-FN)
		   ("Start Anywhere in Window" ANGLE-START-ANYWHERE))))

      (:act-buttons ,ACT-BUTTONS
	  (:constant (t))
	  (:queue *ANGLE-INTER-QUEUE*)
          (:left ,(o-formula (+ 20 (opal:gv-right
				    (gvl :parent :start-where))))))

      (:obj-to-change ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ (opal:gv-bottom
				(gvl :parent :start-where)) 10)))
	  (:parts
	   ((:titled-frame ,titled-frame
			   (:constant (t))
			   (:string ":obj-to-change"))
	    (:contents ,garnet-gadgets:radio-button-panel
		(:constant (t))
		(:top ,(o-formula (+ 20 (gvl :parent :top))))
		(:left ,(o-formula (+ 15 (gvl :parent :left))))
		(:selection-function ANGLE-OBJ-TO-CHANGE-FN)
		(:items (("Result of :start-where")
			 ("<Formula>")))))))

      (:feedback-obj ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (gvl :parent :obj-to-change :left)))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :obj-to-change)))))
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":feedback-obj"))
	    (:contents ,opal:aggregadget
		(:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :feedback-obj-box :width)
				       10 (gvl :none-button :width))))
		(:height ,(o-formula (gvl :none-button :height)))
		(:value ,(o-formula (gvl :parent :value)))
		(:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
		 ((:feedback-obj-box ,SELECT-BOX
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		      (:top ,(o-formula (opal:gv-center-y-is-center-of
					 (gvl :parent :none-button))))
		      (:string "Interim Feedback")
		      (:min-frame-width 125)
		      (:selection-function ANGLE-FEEDBACK-OBJ-FN))
		  (:none-button ,garnet-gadgets:radio-button
		      (:constant (t))
                      (:left ,(o-formula (+ 10 (opal:gv-right
						(gvl :parent :feedback-obj-box)))))
		      (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
		      (:string "None")
		      (:value ,(o-formula (if (string= (gvl :parent :value)
						       "None")
					      "None")))
		      (:selection-function ANGLE-NIL-FEEDBACK-OBJ-FN))))))))

      (:final-function ,garnet-gadgets:labeled-box
          (:constant (t))
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :feedback-obj)))))
	  (:label-string "Final Function:")
	  (:value "")
	  (:min-frame-width 150)
	  (:selection-function ANGLE-FINAL-FUNCTION-FN))

      (:center-of-rotation ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :final-function)))))
	  (:x "")
	  (:y "")
	  (:value nil)
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
	        (:string ":center-of-rotation"))
	    (:contents ,opal:aggregadget
	        (:parts
		 ((:box ,opal:rectangle
		      (:constant (t))
		      (:shadow ,(o-formula 
				 (g-value (first (g-value kr::*schema-self* 
							  :parent :box-buttons
							  :components))
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
			 (:constant (t :except :left :top))
			 (:attach-point ,(o-formula (nth (gvl :rank) 
							 (gvl :parent :items))))))
		      (:direction :horizontal)
		      (:v-spacing 27) 
		      (:h-spacing 27)
		      (:rank-margin 3))
		  (:line ,opal:line
		      (:constant (t))
		      (:x1 ,(o-formula (opal:gv-center-x
					(gvl :parent :line-buttons :x1))))
		      (:y1 ,(o-formula (opal:gv-center-y
					(gvl :parent :line-buttons :x1))))
		      (:x2 ,(o-formula (+ 100 (gvl :x1))))
		      (:y2 ,(o-formula (+ 100 (gvl :y1)))))
		  (:line-buttons ,opal:aggregadget
		      (:constant (:left :top :width :height))
		      (:left ,(o-formula (+ 50 (opal:gv-right
						(gvl :parent :box-buttons)))))
		      (:top ,(o-formula (gvl :parent :box-buttons :top)))
		      (:parts
		       ((:x1 ,LAP-RADIO-BUTTON
			     (:constant (t))
			     (:left ,(o-formula (gvl :parent :left)))
			     (:top ,(o-formula (gvl :parent :top)))
			     (:attach-point 1))
			(:line-c ,LAP-RADIO-BUTTON
			     (:constant (t))
			    (:left ,(o-formula (+ (gvl :parent :left) 50)))
			    (:top ,(o-formula (+ (gvl :parent :top) 50)))
			    (:attach-point :center))
			(:x2 ,LAP-RADIO-BUTTON
			     (:constant (t))
			     (:left ,(o-formula (+ (gvl :parent :left) 100)))
			     (:top ,(o-formula (+ (gvl :parent :top) 100)))
			     (:attach-point 2)))))
		  (:x ,garnet-gadgets:labeled-box
		      (:constant (t))
		      (:left ,(o-formula (gvl :parent :box-buttons :left)))
		      (:top ,(o-formula 
			      (+ (opal:gv-bottom (gvl :parent :box-buttons)) 
				 27)))
		      (:min-frame-width 30)
		      (:old-value "")
		      (:value ,(o-formula (gvl :parent :parent :x)))
		      (:selection-function angle-x-fn)
		      (:label-string "X")
		      (:label-font ,opal:default-font))

		  (:y ,garnet-gadgets:labeled-box
		      (:constant (t))
		      (:left ,(o-formula (+ (opal:gv-right (gvl :parent :x))
					    10)))
		      (:top ,(o-formula (gvl :parent :x :top)))
		      (:min-frame-width 30)
		      (:old-value "")
		      (:value ,(o-formula (gvl :parent :parent :y)))
		      (:selection-function angle-y-fn)
		      (:label-string "Y")
		      (:label-font ,opal:default-font))

		  (:formula ,LABELED-LAP-RADIO-BUTTON
		      (:constant (t))
		      (:left ,(o-formula (- (opal:gv-right (gvl :parent :line-buttons)) 
					    (gvl :width))))
		      (:top ,(o-formula 
			      (+ (opal:gv-bottom (gvl :parent :box-buttons)) 27)))
		      (:attach-point :formula)
		      (:string "<Formula>"))
		  (:feedback ,lap-radio-button-feedback)))
		 (:interactors
		  ((:attach-pt-press ,inter:button-interactor
			    (:window ,(o-formula (gv-local :self :operates-on
							   :window)))
			    (:final-function angle-attach-point-fn)
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
					  (gvl :parent :center-of-rotation))
					 10)))
		    (:queue *ANGLE-INTER-QUEUE*)))))

  ;; make the text box for the start anywhere item be invisible
  (s-value (g-value (second (g-value angle-inter-menu :start-where 
			     :contents :select-box-panel :components))
		    :text-box) 
	   :visible nil)


(opal::fix-update-slots (g-value angle-inter-menu :start-where :contents
				:select-box-panel))
(opal::fix-update-slots (g-value angle-inter-menu :center-of-rotation :contents
				:box-buttons))

(s-value ANGLE-INTER-WIN :aggregate ANGLE-INTER-MENU)
(opal:update ANGLE-INTER-WIN)))
