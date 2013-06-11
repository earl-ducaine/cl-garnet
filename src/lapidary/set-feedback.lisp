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
;;; This file contains the code that determines which constraint buttons
;;; should be highlighted when the user hits the ``show feedback'' button
;;; in the constraint menus
;;;
;;; CHANGE LOG
;;;
;;; 08/24/92 amickish - Added proclaim

(in-package "GARNET-GADGETS")

(defun set-box-constraint-feedback ()
  (declare (special *box-constraint-menu* *constraint-gadget*))
  (let* ((selection-type (g-value *constraint-gadget* :selection-type))
	 (p-selection (g-value *constraint-gadget* :obj-to-constrain))
	 (s-selection (g-value *constraint-gadget* :obj-to-reference)))
       
    ;; determine which constraint icons should be highlighted 
    ;; constraint icons should be highlighted only if there is one
    ;; primary selection and zero or one secondary selections
    (if (or (eq selection-type 'one-zero) 
	    (eq selection-type 'one-one))
       (cond ((or (is-a-line-p p-selection)
		  (is-a-line-p s-selection))
	      (constraint-gadget-error "One of the selected objects is a line, not a box object.
Use the line constraint menu instead"))
	     (t
	      (dolist (slot '(:left :top :width :height))
		(multiple-value-bind (constraint-buttons offset-box 
							 scale-box con-panel)
			(case slot
			 (:left (values 
				 (g-value *box-constraint-menu* :box-agg 
					  :left-button-panel :buttons)
				 (g-value *box-constraint-menu* :left-slot-agg
					  :offset-box)
				 nil
				 (g-value *box-constraint-menu* :left-slot-agg
					  :con-panel)))
			 (:top  (values
				 (g-value *box-constraint-menu* :box-agg 
					 :top-button-panel :buttons)
				 (g-value *box-constraint-menu* :top-slot-agg
					 :offset-box)
				 nil
				 (g-value *box-constraint-menu* :top-slot-agg
					  :con-panel)))
			 (:width (values
				  (g-value *box-constraint-menu* :width-button)
				  (g-value *box-constraint-menu* 
					   :width-slot-agg :difference-box)
				  (g-value *box-constraint-menu* 
					   :width-slot-agg :scale-box)
				  (g-value *box-constraint-menu* 
					   :width-slot-agg :con-panel)))
			 (:height (values
				   (g-value *box-constraint-menu* :height-button)
				   (g-value *box-constraint-menu* 
					    :height-slot-agg :difference-box)
				   (g-value *box-constraint-menu* 
					   :height-slot-agg :scale-box)
				   (g-value *box-constraint-menu* 
					    :height-slot-agg :con-panel))))
		   (let* ((value (get-value p-selection slot))
			  (highlight-p 
			   (and (formula-p value)
				(or (eq selection-type 'one-zero)
				    (and (eq selection-type 'one-one)
					 (depends-on-p p-selection
						       s-selection slot)))))
			  (menu-item 
			   (when highlight-p
				 (g-formula-value value :menu-item))))
		     (cond ((or (eq menu-item :customize)
				;; account for the case where the menu item
				;; is not in the formula for some reason
				(and (null menu-item) highlight-p))
			    (s-value con-panel :value "customize"))
			   (menu-item
			    (let ((button (g-value constraint-buttons menu-item)))
			      (s-value button :selected t)
			      (when (or (eq slot :left) (eq slot :top))
				  (s-value (g-value button :parent)
					   :selected button)))
			 
			    ;; set the offset and scale boxes
			    (when (or (eq slot :left) (eq slot :top))
			       (if (eq menu-item :center)
				  (s-value offset-box :label-string "percent")
				  (s-value offset-box :label-string "offset")))
			    (if (g-formula-value value :offset)
				(s-value offset-box :value 
					 (princ-to-string
					  (if (eq menu-item :center)
					      (* 100 (g-value p-selection
						        (g-formula-value value :offset)))
					      (g-value p-selection
						       (g-formula-value value :offset)))))
			        (s-value offset-box :value "0"))
			    (if (g-formula-value value :scale)
				(s-value scale-box :value
					 (princ-to-string
					  (g-value p-selection
						   (g-formula-value value :scale))))
			        (when scale-box 
				      (s-value scale-box :value "0"))))
						   
			   (t 
			    (s-value con-panel :value "unconstrain")))))))))))

(defun set-line-constraint-feedback ()
  (declare (special *constraint-gadget* line-con-prim-sel-agg
		    line-con-sec-sel-agg line-con-panel x-offset-box
		    y-offset-box *constraint-gadget-query-window*))
  (let* ((selection-type (g-value *constraint-gadget* :selection-type))
	 (p-selection (g-value *constraint-gadget* :obj-to-constrain))
	 (s-selection (g-value *constraint-gadget* :obj-to-reference)))

    ;; determine which constraint icons should be highlighted 
    ;; constraint icons should be highlighted only if there is one
    ;; primary selection and zero or one secondary selections
    (if (or (eq selection-type 'one-zero) 
	    (eq selection-type 'one-one))
       (cond ((and (not (is-a-line-p p-selection))
		   s-selection
		   (not (is-a-line-p s-selection)))
	      (constraint-gadget-error "Both selected objects are box objects.
Use the box constraint menu instead"))

	     ;; if the primary selection is a line object, and there is
	     ;; only one endpoint is constrained--show the constrained 
	     ;; endpoint and the location on the other object that it is 
	     ;; constrained to. if both 
	     ;; endpoints are constrained, but only one is constrained to
	     ;; the secondary selection, show the one constrained endpoint and
	     ;; the location on the other object that is is constrained to.
	     ;; if both endpoints are constrained, and either there is no 
	     ;; secondary selection, or both endpoints are constrained to the 
	     ;; secondary selection, display the first endpoint, then pop up 
	     ;; a query gadget telling them they can choose to see the second 
	     ;; endpoint as well.
	     ((is-a-line-p p-selection)
	      (let* ((formula-x1 (get-value p-selection :x1))
		     (formula-x2 (get-value p-selection :x2))
		     (highlight-x1 (when (formula-p formula-x1)
					  (or (eq selection-type 'one-zero)
					      (depends-on-p p-selection 
							    s-selection :x1))))
		     (highlight-x2 (when (formula-p formula-x2)
					  (or (eq selection-type 'one-zero)
					      (depends-on-p p-selection 
							    s-selection :x2))))
		     menu-item-x1 menu-item-x2)
		;; retrieve menu items for the endpoints of the line
		(when highlight-x1
		      (setf menu-item-x1
			    (g-formula-value formula-x1 :menu-item)))
		(when highlight-x2
		      (setf menu-item-x2
			    (g-formula-value formula-x2 :menu-item)))
		;; now display the selections
		;; if both endpoints should be shown, show them one at a time
		(cond ((and highlight-x1 highlight-x2)
		       (select-line-constraint-buttons menu-item-x1
						       p-selection
						       s-selection :x1)
		       ;; ask whether the user wants to see the second 
		       ;; endpoint
		       (setf (opal:center-x *constraint-gadget-query-window*)
			     opal:*screen-width*)
		       (setf (opal:center-y *constraint-gadget-query-window*)
			     opal:*screen-height*)
		       (when (string= (constraint-gadget-query "Both endpoints of the line are constrained. To 
see the constraint on the second endpoint,
press 'NEXT'. If you do not want to see the
second constraint, press 'DONE'." '("NEXT" "DONE") t)
				      "NEXT")
			     ;; deselect the previous line constraint 
			     ;; selections
			     (select-line-constraint-buttons menu-item-x1
						       p-selection
						       s-selection :x1
						       nil)
			     ;; select the appropriate constraint buttons
			     ;; for the constraint on the second endpoint
			     (select-line-constraint-buttons menu-item-x2
						       p-selection
						       s-selection :x2)))

		      ;; only the first endpoint needs to be highlighted
		      (highlight-x1
		       (select-line-constraint-buttons menu-item-x1
						       p-selection
						       s-selection :x1))

		      ;; only the second endpoint needs to be highlighted
		      (highlight-x2
		       (select-line-constraint-buttons menu-item-x2
						       p-selection
						       s-selection :x2))
		      ;; neither endpoint is constrained
		      (t 
		       (deselect-line-buttons)
		       (s-value line-con-panel :value "unconstrain")))))
	     ;; if the primary selection is a box object, the secondary 
	     ;; selection is a line, and the left and top slots of the box
	     ;; object depend on the line, highlight the appropriate
	     ;; constraint icons in the line constraint menu
	     (t
	      ;; only have to check :left, since both the :left and :top
	      ;; of a box are constrained by a line constraint
	      (let* ((formula (get-value p-selection :left))
		    (p-selection-buttons (g-value line-con-prim-sel-agg
						  :box :buttons))
		    (s-selection-buttons (g-value line-con-sec-sel-agg
						  :line :buttons))
		    (menu-item-left
		     (if (and (formula-p formula)
			      (or (eq selection-type 'one-zero)
				  (depends-on-p p-selection 
						s-selection :left)))
			 (g-formula-value formula :menu-item))))
		(cond ((eq menu-item-left :customize)
		       (deselect-line-buttons)
		       (s-value line-con-panel :value "customize"))
		      (menu-item-left
		       ;; make sure it is safe to proceed--the primary 
		       ;; selection is a box so the item it is constrained 
		       ;; to must be a line. The test at the start of this 
		       ;; function made sure that that if there is a secondary
		       ;; selection, that the secondary selection is a line. 
		       ;; The following test ensures that if there is no 
		       ;; secondary selection, then whatever the box is
		       ;; constrained to is a line
		       (when (not (is-a-line-p 
				   (g-value p-selection 
					    (car (g-formula-value formula :links)))))
			     (constraint-gadget-error "The object you have selected is a box object
and so is the object it is constrained to.
Please use the box constraint menu instead")
			     (return-from set-line-constraint-feedback))
		       (let* ((menu-item-top
			       (g-formula-value (get-value p-selection :top)
					       :menu-item))
			      (index (if menu-item-top
					 (+ (* (car menu-item-left) 3)
					    (car menu-item-top))
				         (* menu-item-left 3)))
			      (p-button 
				(nth index
				     (g-value p-selection-buttons :components)))
			      (s-button 
			       (nth (cdr menu-item-left)
				    (g-value s-selection-buttons :components))))
			 (s-value line-con-panel :value nil)
			 (s-value p-button :selected t)
			 (s-value s-button :selected t)
			 (s-value (g-value p-button :parent) 
				  :selected p-button)
			 (s-value (g-value s-button :parent)
				  :selected s-button))
		       
		       ;; set the x and y offsets
		       ;; first set the x offset
		       (if (g-formula-value formula :offset)
			   (s-value x-offset-box :value
				    (princ-to-string
				     (g-value p-selection
					(g-formula-value formula :offset))))
			   (s-value x-offset-box :value "0"))
		       ;; now set the y offset
		       (let ((top-formula (get-value p-selection :top)))
			 (if (g-formula-value top-formula :offset)
			     (s-value y-offset-box :value
			      (princ-to-string
			       (g-value p-selection
					(g-formula-value top-formula :offset))))
		             (s-value y-offset-box :value "0"))))
		      ;; if there is a formula, but no menu item, assume it
		      ;; is a custom constraint
		      ((formula-p formula)
		       (deselect-line-buttons)
		       (s-value line-con-panel :value "customize"))
		      (t
		       (deselect-line-buttons)
		       (s-value line-con-panel :value "unconstrain")))))))))

(defun select-line-constraint-buttons (menu-item p-selection s-selection slot
						 &optional (select-p t))
  (declare (special line-con-prim-sel-agg line-con-sec-sel-agg
		    line-con-panel x-offset-box y-offset-box))
  (let* ((p-selection-buttons (g-value line-con-prim-sel-agg
				      :line :buttons))
	 (formula (get-value p-selection slot))
	 (link (car (g-formula-value formula :links)))
	 (line-p (or (is-a-line-p s-selection)
		     (is-a-line-p (g-value p-selection link))))
	 (s-selection-buttons 
	  (if line-p
	      (g-value line-con-sec-sel-agg
		       :line :buttons)
	      (g-value line-con-sec-sel-agg
		       :box :buttons)))
	 (p-button (nth (if (eq slot :x1) 0 1)
			(g-value p-selection-buttons :components)))
	 s-button)

    ;; set the appropriate endpoint button in the primary selection box
    (s-value p-button :selected select-p)
    (s-value (g-value p-button :parent) :selected p-button)

    ;; set the appropriate button in the secondary selection box
    (cond ((eq menu-item :customize)
	   (deselect-line-buttons t)
	   (s-value line-con-panel :value
		    (if select-p "customize" nil)))
	  (menu-item
	   (s-value line-con-panel :value nil)
	   (if line-p
	       (progn
		 (setf s-button (nth (cdr menu-item)
				     (g-value s-selection-buttons :components)))
		 (s-value s-button :selected select-p)
		 (if select-p
		     (s-value (g-value s-button :parent) :selected s-button)
		     (s-value (g-value s-button :parent) :selected nil)))

	     ;; to get the correct constraint button for a box object, you
	     ;; must compute the equation 3*left + top, where top and left
	     ;; are the numbers contained in their respective menu items
	     (let* ((top-menu-item (g-formula-value (get-value p-selection 
							  (if (eq slot :x1)
							      :y1
							      :y2))
					      :menu-item))
		    (index (if top-menu-item
			       (+ (* (cdr menu-item) 3) (cdr top-menu-item))
			     (* (cdr menu-item) 3))))
	       (setf s-button (nth index
				   (g-value s-selection-buttons :components)))
	       (s-value s-button :selected select-p)
	       (if select-p
		   (s-value (g-value s-button :parent) :selected s-button)
		   (s-value (g-value s-button :parent) :selected nil))))
	   ;; set the x and y offsets
	   (when select-p
		 ;; first set the x offset
		 (if (g-formula-value formula :offset)
		     (s-value x-offset-box :value
			      (princ-to-string
			       (g-value p-selection
					(g-formula-value formula :offset))))
		     (s-value x-offset-box :value "0"))
		 ;; now set the y offset
		 (let ((y-formula (if (eq slot :x1) 
				      (get-value p-selection :y1)
				      (get-value p-selection :y2))))
		   (if (g-formula-value y-formula :offset)
		     (s-value y-offset-box :value
			      (princ-to-string
			       (g-value p-selection
					(g-formula-value y-formula :offset))))
		     (s-value y-offset-box :value "0")))))
	  ;; if there is no menu item, choose the customize button--this
	  ;; function is called only if there is a formula in the p-selection's
	  ;; slot
	  (t
	   (deselect-line-buttons t)
	   (s-value line-con-panel :value 
		    (if select-p "customize" nil))))))

  

