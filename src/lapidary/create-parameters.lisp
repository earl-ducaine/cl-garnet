;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CHANGE LOG
;;;
;;; 08/24/92 amickish - Removed declaration of slots from let* of
;;;                     Create-Parameters

(in-package "LAPIDARY")

(create-instance 'parameter-menu OPAL:AGGREGADGET
  (:WINDOW-TITLE "Parameters")
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH (o-formula (+ 20 (gvl :width))))
  (:WINDOW-HEIGHT (o-formula (+ 20 (gvl :height))))
  (:PACKAGE-NAME "LAPIDARY")
  (:items nil)
  (:FUNCTION-FOR-OK `create-parameters)
  (:LEFT 0)
  (:TOP 0)
  (:parts `(
    (:OKCANCEL-BUTTON ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION OKCANCEL-FUNCTION)
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:V-SPACING 5)
      (:H-ALIGN :CENTER)
      (:FIXED-HEIGHT-P T)
      (:H-SPACING 5)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:FIXED-WIDTH-P T)
      (:SELECT-FUNCTION OKCANCEL-FUNCTION)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:LEFT 80)
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :link-slots)) 20))))
    (:label ,opal:multi-text
      (:left 10)
      (:top 10)
      (:string "If a slot should be a parameter, enter the name of
the slot in the secondary selection that it should 
retrieve its value from. If you do not want a slot 
to be a parameter, just leave the text box next to 
it blank (or make it blank it it currently contains 
the name of a slot."))
#|
      (:string "To make a slot a parameter, select its text box and
enter the name of the slot in the secondary selection that
the slot should retrieve its value from. To deselect a slot, 
make its text box be blank"))
|#
    (:slots ,opal:aggrelist
      (:items ,(o-formula (gvl :parent :items)))
      (:fixed-height-p t)
      (:fixed-height-size 18)
      (:v-align :center)
      (:v-spacing 10)
      (:item-prototype ,opal:text
	 (:string ,(o-formula (prin1-to-string
			       (nth (gvl :rank) (gvl :parent :items))))))
      (:left 10)
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :label)) 20))))
    (:parameters-list ,opal:aggrelist
      (:left ,(o-formula (+ (opal:gv-right (gvl :parent :slots)) 10)))
      (:top ,(o-formula (gvl :parent :slots :top)))
      (:v-spacing 10)
      (:item-prototype 
       (,garnet-gadgets:labeled-box
	(:label-string "")
	(:value ,(o-formula (nth (gvl :rank) (gvl :parent :items)))))))
    (:instructions ,opal:multi-text
      (:left 10)
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :parameters-list)) 20)))
      (:string "Select the following button to get a list of objects
that are referenced by the primary selection, and
which lapidary thinks should be parameters. If there
are no such objects, nothing will appear."))
    (:link-slots ,garnet-gadgets:text-button
      (:left 10)
      (:GRAY-WIDTH 3)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :instructions)) 20)))
      (:string "link parameters")
      (:final-feedback-p nil)
      (:selection-function set-up-and-display-link-parameters)))))


(defun set-up-and-display-link-parameters (gadget value)
  (declare (special save-link-parameters))
  (when (g-value save-link-parameters :link-values)
	;; show-link-parameters returns a non-nil value if the ok button
	;; is hit
	(when (show-link-parameters gadget value)
	      (s-value save-link-parameters :ok-p t))))

(define-method :notice-items-changed parameter-menu
               (gadget &optional no-propagation)
  (declare (ignore no-propagation))
  (opal::fix-update-slots (g-value gadget :slots))
  (opal::fix-update-slots (g-value gadget :parameters-list)))

;;; make sure the :value slots of parameter menu are demanded so that
;;; their formulas are not wiped out
(g-value parameter-menu :slots :value)

(s-value opal:view-object :lapidary-parameters 
	 '(:filling-style :line-style :draw-function))

(s-value opal:text :lapidary-parameters	
	 '(:font :string :line-style :draw-function))

(s-value opal:line :lapidary-parameters
	 '(:line-style :draw-function))

(s-value garnet-gadgets:arrow-line :lapidary-parameters
	 '(:line-style :draw-function :filling-style))

(s-value garnet-gadgets:double-arrow-line :lapidary-parameters
	 '(:line-style :draw-function :filling-style))

(s-value opal:aggrelist :lapidary-parameters
	 '(:filling-style :line-style :draw-function :select-function 
				:direction
				:v-spacing :h-spacing
				:fixed-width-size
				:fixed-height-size
				:fixed-width-p :fixed-height-p
				:h-align :v-align 
				:rank-margin :pixel-margin 
				:indent))

(defun create-parameters (gadget values)
  (declare (ignore values))
  (declare (special *selection-info* save-link-parameters))
  (let* ((p-selected (car (g-value *selection-info* :p-selected)))
	 (s-selected (car (g-value *selection-info* :s-selected)))
	 (path (gilt:make-path p-selected s-selected))
	 (parameter-list (g-value p-selected :slot-parameters))
	 parameter-slot slot parameter old-parameter-slot parameters-entry
	 items)

    ;; when the secondary selection is an aggrelist, formulas should be
    ;; installed in the item prototype object rather than the primary 
    ;; selection. The item prototype object (or the appropriate child) is
    ;; the :is-a of the primary selection
    (when (is-a-p s-selected opal:aggrelist)
	  (setf p-selected (car (g-value p-selected :is-a))))

    (do ((slots (g-value gadget :items) (cdr slots))
	 (parameters (g-value gadget :parameters-list :components) (cdr parameters)))
	((null slots))
      (setf parameter (car parameters))
      (setf slot (car slots))
      (setf parameter-slot (if (string= (g-value parameter :value) "") 
			       nil
			       (read-from-string (g-value parameter :value))))

      ;; determine if the slot used to be a parameter
      (setf parameters-entry (assoc slot parameter-list))
      (setf old-parameter-slot (cdr parameters-entry))

      ;; if there was an entry on the parameters list, destroy the
      ;; constraint in the slot if there is a new parameter slot (or
      ;; none at all)
      (when (and old-parameter-slot
		 (not (eq old-parameter-slot parameter-slot)))
	    (destroy-constraint p-selected slot)
	    ;; see if the old parameter slot should be destroyed
	    (when (null (kr::get-dependents s-selected old-parameter-slot))
		  (destroy-slot s-selected old-parameter-slot)))

      (if (null parameter-slot)
	  ;; slot was not selected to be a parameter. Delete it from the
	  ;; parameters list if it is there
	  (setf parameter-list (delete slot parameter-list :key #'car))
	  
	  (progn
	    ;; if the parameter slot is :items and the secondary selection
	    ;; is an aggrelist, push the slot onto the items list and handle
	    ;; it at the end
	    (cond ((and (eq parameter-slot :items) 
			(is-a-p s-selected opal:aggrelist))
		   (push slot items))
		  (t
		   ;; if the parameter slot is new or has changed, copy the 
		   ;; value of the primary selection into the secondary 
		   ;; selection 
		   (when (or (null parameters-entry)
			     (not (eq old-parameter-slot parameter-slot)))
		      (s-value s-selected parameter-slot 
			       (g-value p-selected slot))

		      (when (or (not (eq p-selected s-selected))
				(not (eq parameter-slot slot)))
			(s-value p-selected slot 
				 (formula `(gvl ,@path ,parameter-slot)))))))

	    ;; update the old entry on the parameters list or insert 
	    ;; a new entry
	    (if parameters-entry
		(setf (cdr parameters-entry) parameter-slot)
	      ;; this is a new parameter so insert an entry onto
	      ;; the parameters list
	      (push (cons slot parameter-slot) parameter-list)))))

    ;; store the new parameter-list
    (s-value p-selected :slot-parameters parameter-list)
    
    ;; if the secondary selection is an aggrelist, install formulas that
    ;; reference the items slot in the slots on the items list
    (when (is-a-p s-selected opal:aggrelist)
	  (AggListParam (reverse items)))

    (when (g-value save-link-parameters :ok-p)
	  (convert-links-to-parameters))))

;;; =================================================================
;;; return a list of all components in an aggregate, including the
;;; aggregate itself and interactors
;;; =================================================================
(defun transitive-closure (agg)
  (let (components)
    (dovalues (obj agg :components)
	      (setf components (append components (transitive-closure obj))))
    ;; get the interactors
    (setf components (append (g-value agg :behaviors) components))
    (push agg components)
    components))

(defun show-parameter-window (gadget)
  (declare (ignore gadget))
  (declare (special parameter-menu save-link-parameters))
  (let ((p-selected (car (g-value *selection-info* :p-selected)))
	 (s-selected (car (g-value *selection-info* :s-selected)))
	 parameters 
	 symbol-table link-values parameter-name items)
    (when (cdr (g-value *selection-info* :p-selected))
	  (lapidary-error "There should only be one primary selection. Please
deselect objects so you have only one
selection, then try again")
	  (return-from show-parameter-window))
    (when (cdr (g-value *selection-info* :s-selected))
	  (lapidary-error "There should only be one secondary selection. Please
deselect objects so you have only one
selection, then try again")
	  (return-from show-parameter-window))
    (when (null s-selected)
	  (lapidary-error "Must use a secondary selection to select the 
object which provides the parameter values")
	  (return-from show-parameter-window))
    (when (null p-selected)
	  (lapidary-error "Must use a primary selection to select the 
object which should be parameterized")
	  (return-from show-parameter-window))

    (setf parameters (g-value p-selected :slot-parameters))
    ;; set the normal parameters such as filling-style and line-style
    (s-value parameter-menu :items (g-value p-selected :lapidary-parameters))

    ;; determine if any of these slots are already parameters
    (dolist (item (g-value p-selected :lapidary-parameters))
	    (if (setf parameter-name (assoc item parameters))
		(push (prin1-to-string (cdr parameter-name)) items)
	        (push "" items)))
    (s-value (g-value parameter-menu :parameters-list) :items (reverse items))

    ;; determine which link slots might be parameters
    (setf symbol-table (transitive-closure s-selected))
    (setf link-values (find-link-slots p-selected symbol-table))
    (s-value save-link-parameters :link-values link-values)
    (when link-values
	  (s-value save-link-parameters :obj p-selected)
	  (s-value save-link-parameters :top-level-obj s-selected)
	  (s-value save-link-parameters :ok-p nil))
   
    (opal:notice-items-changed parameter-menu)
    (gilt:show-in-window parameter-menu 300 100 t)))
