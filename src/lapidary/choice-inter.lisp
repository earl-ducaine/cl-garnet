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
;;;
;;;  Choice of Items Interactor dialog box
;;;

;;;  Note: This file needs parts from the file  dialog-parts.lisp

;;; CHANGE LOG
;;;
;;  07/14/93 amickish - Removed declare ignore of button-panel in HOW-SET-FN
;;; 08/25/92 amickish - Removed declare ignore of label in AGG-OF-ITEMS-FN and
;;;                     SINGLE-ITEM-FN, added proclaim

(in-package "LAPIDARY")

(declaim (special choice-menu *by-demo-copy*))
(defvar *CHOICE-INTER-QUEUE* NIL)

;; accessor functions for various aggregates in the choice interactor
;; menu

(defmacro CHOICE-START-WHERE ()
  `(g-value CHOICE-MENU :start-where))
(defmacro CHOICE-FEEDBACK-OBJ ()
  `(g-value CHOICE-MENU :feedback-obj))
(defmacro CHOICE-FINAL-FEEDBACK ()
  `(g-value CHOICE-MENU :final-feedback-obj))
(defmacro CHOICE-HOW-SET ()
  `(g-value CHOICE-MENU :how-set))
(defmacro CHOICE-OTHER-BOX ()
  `(g-value CHOICE-MENU :start-where :contents :other-box))
(defmacro CHOICE-OTHER-BUTTON ()
  `(g-value CHOICE-MENU :start-where :contents :other-button))

;;; *********************************
;;; Change the name of the interactor
;;; *********************************
;;;    -- interactor-name is a string
(defun INTERACTOR-NAME-FN (gadget interactor-name)
  (declare (ignore gadget))
  (dialog-enqueue :known-as
		  (if (string/= "" interactor-name)
		      (read-from-string
		       (concatenate 'string ":" interactor-name)))
		  *CHOICE-INTER-QUEUE*))


;;; ************************************
;;; Functions for the :start-where panel
;;; ************************************

;;;    :start-where is in an aggregate of items
(defun AGG-OF-ITEMS-FN (agg-box label)
  (declare (ignore agg-box))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(panel (CHOICE-START-WHERE)))
    (if selection
	(progn
	  (s-value panel :field-string (name-for-schema selection))
	  (s-value panel :value label)
	  (dialog-enqueue :start-where
			  `(:element-of ,selection 
					,@(g-value panel :type-restriction))
			  *CHOICE-INTER-QUEUE*))
	(progn
	  (s-value panel :value NIL)
	  (s-value panel :field-string "")))))

;;;    :start-where is in a single item
(defun SINGLE-ITEM-FN (item-box label)
  (declare (ignore item-box))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(panel (CHOICE-START-WHERE)))
    (if selection
	(progn
	  (s-value panel :field-string (name-for-schema selection))
	  (s-value panel :value label)
	  (dialog-enqueue :start-where
			  `(:in-box ,selection)
			  *CHOICE-INTER-QUEUE*))
	(progn
	  (s-value panel :value NIL)
	  (s-value panel :field-string "")))))


;;; *************************************
;;; Functions for the :feedback-obj panel
;;; *************************************

(defun FEEDBACK-OBJ-FN (feedback-obj-box button-label)
  (declare (special *selection-info*))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (cond ((null selection)
	   (s-value (CHOICE-FEEDBACK-OBJ) :field-string nil)
	   (s-value (CHOICE-FEEDBACK-OBJ) :value nil)
	   (lapidary-error "please make a selection, then press the
interim feedback button again"))
	((null (cdr selection)) ; only one selection
	 (s-value (CHOICE-FEEDBACK-OBJ) :field-string
		  (name-for-schema (car selection)))
	 (s-value (CHOICE-FEEDBACK-OBJ) :value button-label)
	 (dialog-enqueue :feedback-obj selection *CHOICE-INTER-QUEUE*))
	(t
	 (s-value (CHOICE-FEEDBACK-OBJ) :field-string
		  (princ-to-string selection))
	 (s-value (CHOICE-FEEDBACK-OBJ) :value button-label)
	 ;; pop up C32 and ask the user to enter a formula that
	 ;; selects which feedback object to use
	 (get-inter-feedback-formula (g-value feedback-obj-box :window :inter)
				     :feedback-obj
				     selection
				     '*choice-inter-queue*)))))

(defun by-demo-interim-feedback (gadget value)
  (declare (special lapidary-query-gadget))
  (if (or (null *by-demo-copy*) (null (schema-p *by-demo-copy*)))
      (let ((interim-feedback-gadget (CHOICE-FEEDBACK-OBJ)))
	;; save the previous value for the interim feedback in case the
	;; by-demo operation is canceled, then highlight the by-demo button
	(s-value interim-feedback-gadget :save-value
		 (g-value interim-feedback-gadget :value))

	;; don't highlight by-demo button until we're sure it's ok
	;; to do a demonstration
	(s-value gadget :value nil)

	;; set up the demonstration and save the ``before'' state
	(when (by-demo gadget value)
	      (s-value interim-feedback-gadget :value :by-demo)
	      (s-value lapidary-query-gadget :feedback-gadget interim-feedback-gadget)
	      (s-value lapidary-query-gadget :demo-gadget gadget)
	      (s-value lapidary-query-gadget :selection-function 
		       'by-demo-OKCANCEL-FUNCTION)
	      (s-value lapidary-query-gadget :function-for-ok 'by-demo-ok-function)
	      (multiple-value-bind (left top)
		 (opal:convert-coordinates (g-value gadget :window)
					   (g-value gadget :left)
					   (opal:bottom gadget) NIL)
		 (s-value lapidary-query-gadget :window-left left)
		 (s-value lapidary-query-gadget :window-top top)
		 (garnet-gadgets:display-query lapidary-query-gadget
					       "Press when finished editing"
					       '("OK" "CANCEL")))
	      (s-value gadget :value :by-demo)))
      ;; otherwise tell the user that a demonstration is already under
      ;; way
      (progn
	;; return the by-demo button to its previous selection state
	(s-value gadget :value (not (g-value gadget :value)))
	(lapidary-error "Demonstration is already in progress. Press the 
cancel button in the by-demo window, then the by-demo button 
if you want to start over"))))

(defun NIL-FEEDBACK-OBJ-FN (button button-label)
  (declare (ignore button))
  (dialog-enqueue :feedback-obj NIL *CHOICE-INTER-QUEUE*)
  (s-value (CHOICE-FEEDBACK-OBJ) :field-string nil)
  (s-value (CHOICE-FEEDBACK-OBJ) :value button-label))

;;; ******************************************
;;; Functions for changing the :final-feedback-obj
;;; ******************************************

(defun FINAL-FEEDBACK-FN (final-feedback-box button-label)
  (declare (special *selection-info*))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (cond ((null selection)
	   (s-value (CHOICE-FINAL-FEEDBACK) :field-string nil)
	   (s-value (CHOICE-FINAL-FEEDBACK) :value nil)
	   (lapidary-error "please make a selection, then press the
interim feedback button again"))
	((null (cdr selection)) ; only one selection
	 (s-value (CHOICE-FINAL-FEEDBACK) :field-string
		  (name-for-schema (car selection)))
	 (s-value (CHOICE-FINAL-FEEDBACK) :value button-label)
	 (dialog-enqueue :final-feedback-obj selection *CHOICE-INTER-QUEUE*))
	(t ; multiple selections
	 (s-value (CHOICE-FINAL-FEEDBACK) :field-string
		  (princ-to-string selection))
	 (s-value (CHOICE-FINAL-FEEDBACK) :value button-label)
	 ;; pop up C32 and ask the user to enter a formula that
	 ;; selects which feedback object to use
	 (get-inter-feedback-formula (g-value final-feedback-box :window 
					      :inter)
				     :final-feedback-obj
				     selection
				     '*choice-inter-queue*)))))

(defun by-demo-final-feedback (gadget value)
  (declare (special lapidary-query-gadget))
  (if (or (null *by-demo-copy*) (null (schema-p *by-demo-copy*)))
      (let ((final-feedback-gadget (CHOICE-FINAL-FEEDBACK)))
	;; save the previous value for the final feedback in case the
	;; by-demo operation is canceled, then highlight the by-demo button
	(s-value final-feedback-gadget :save-value
		 (g-value final-feedback-gadget :value))

	;; don't highlight by-demo button until we're sure it's ok
	;; to do a demonstration
	(s-value gadget :value nil)

	;; set up the demonstration and save the ``before'' state
	(when (by-demo gadget value)
	      (s-value final-feedback-gadget :value :by-demo)
	      (s-value lapidary-query-gadget :feedback-gadget final-feedback-gadget)
	      (s-value lapidary-query-gadget :demo-gadget gadget)
	      (s-value lapidary-query-gadget :selection-function 
		       'by-demo-OKCANCEL-FUNCTION)
	      (s-value lapidary-query-gadget :function-for-ok 'by-demo-ok-function)
	      (multiple-value-bind (left top)
	        (opal:convert-coordinates (g-value gadget :window)
					  (g-value gadget :left)
					  (opal:bottom gadget) NIL)
		 (s-value lapidary-query-gadget :window-left left)
		 (s-value lapidary-query-gadget :window-top top)
		 (garnet-gadgets:display-query lapidary-query-gadget
					       "Press when finished editing"
					       '("OK" "CANCEL"))
		(s-value gadget :value :by-demo))))
      ;; otherwise tell the user that a demonstration is already under
      ;; way
      (progn
	;; return the by-demo button to its previous selection state
	(s-value gadget :value (not (g-value gadget :value)))
	(lapidary-error "Demonstration is already in progress. Press the 
cancel button in the by-demo window, then the by-demo button 
if you want to start over"))))
  
(defun NIL-FINAL-FEEDBACK-FN (button button-label)
  (declare (ignore button))
  (dialog-enqueue :final-feedback-obj NIL *CHOICE-INTER-QUEUE*)
  (s-value (CHOICE-FINAL-FEEDBACK) :field-string nil)
  (s-value (CHOICE-FINAL-FEEDBACK) :value button-label))


;;; *****************************
;;; Change the type of interactor
;;; *****************************

(defun INTER-TYPE-FN (button-panel value)
  (declare (ignore button-panel))
  (dialog-enqueue :is-a
		  (if (string= value "Menu")
		      lapidary-menu-interactor
		      lapidary-button-interactor)
		  *CHOICE-INTER-QUEUE*))

;;; ********************************************
;;; Change the :final-function of the interactor
;;; ********************************************

;;;    Note:  (funcall (read-from-string "equal") 3 3) ==> T
(defun FINAL-FUNCTION-FN (labeled-box string)
  (declare (ignore labeled-box))
  (dialog-enqueue :final-function
		  (if (string= "" string)
		      nil
		      (read-from-string string))
		  *CHOICE-INTER-QUEUE*))

;;; *************************************
;;; Change the :how-set of the interactor
;;; *************************************

;;;    Note:  assume items in the button-panel are atoms (i.e., :toggle)
(defun HOW-SET-FN (button-panel value)
  (s-value (CHOICE-HOW-SET) :value value)
  (if (and (stringp value) (string-equal value "<formula>"))
      (create-custom-inter-constraint (g-value button-panel :window :inter) 
				      :how-set '*choice-inter-queue*)
      (dialog-enqueue :how-set value *CHOICE-INTER-QUEUE*)))

(defun increment-by-fn (gadget value)
  (declare (ignore gadget))
  (let ((old-value (g-value (CHOICE-HOW-SET) :value)))
    (if (listp old-value)
	(progn
	  (setf (car old-value) (read-from-string value))
	  (dialog-enqueue :how-set old-value *CHOICE-INTER-QUEUE*))
	(progn
	  (s-value (CHOICE-HOW-SET) :value (read-from-string value))
	  (dialog-enqueue :how-set (read-from-string value) *CHOICE-INTER-QUEUE*)))))

(defun max-value-fn (gadget value)
  (declare (ignore gadget))
  (let ((old-value (g-value (CHOICE-HOW-SET) :value)))
    (if (listp old-value)
	(progn
	  (setf (second old-value) (read-from-string value))
	  (dialog-enqueue :how-set old-value *CHOICE-INTER-QUEUE*))
	(let ((incr-max-pair (list 1 (read-from-string value))))
	  (s-value (CHOICE-HOW-SET) :value incr-max-pair)
	  (dialog-enqueue :how-set incr-max-pair *CHOICE-INTER-QUEUE*)))))




(defun choice-inter-do-go ()
  (let ((kr::*constants-disabled* nil))
  (choice-inter-do-stop)


(create-instance 'CHOICE-INTERACTOR-WIN inter:interactor-window
   (:title "choice interactor")
   (:left #-apple 500 #+apple 200)
   (:top  #-apple 10  #+apple 50)
   (:width 540)(:height 600)
   (:queue '*choice-inter-queue*))
(opal:update CHOICE-INTERACTOR-WIN)


(create-instance 'CHOICE-MENU opal:aggregadget
   (:constant '(:left :top :width :height :visible))
   (:left 10)
   (:top 10)
   (:parts
    `((:title ,opal:text
	  (:constant (t))
          (:left ,(o-formula (gvl :parent :left)))
	  (:top ,(o-formula (gvl :parent :top)))
	  (:string "Choice of Items Interactor")
	  (:font ,*very-large-bold-italic-serif-font*))

      (:known-as ,NAME-BOX
          (:constant (t))
	  (:selection-function INTERACTOR-NAME-FN))

      (:start-where ,START-WHERE
          (:constant (t))
	  (:items (("Aggregate of items" AGG-OF-ITEMS-FN)
		   ("Single item" SINGLE-ITEM-FN))))

      (:act-buttons ,ACT-BUTTONS
          (:constant (t))
	  (:queue *CHOICE-INTER-QUEUE*)
          (:left ,(o-formula (+ 20 (opal:gv-right
				    (gvl :parent :start-where))))))


      (:feedback-obj ,opal:aggregadget
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :start-where)))))
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":feedback-obj"))
	    (:contents ,opal:aggregadget
		(:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :feedback-obj-box :width)
				       10 (gvl :demo-button :width)
				       10 (gvl :none-button :width))))
		(:height ,(o-formula (gvl :demo-button :height)))
		(:value ,(o-formula (gvl :parent :value)))
		(:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
		 ((:feedback-obj-box ,SELECT-BOX
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		      (:top ,(o-formula (opal:gv-center-y-is-center-of
					 (gvl :parent :demo-button))))
		      (:string "Interim Feedback")
		      (:min-frame-width 125)
		      (:selection-function FEEDBACK-OBJ-FN))
		  (:demo-button ,garnet-gadgets:text-button
		      (:constant (t))
		      (:demo-slot :interim-selected)
		      (:demo-p t)
		      (:slot :feedback)
		      (:value ,(o-formula (eq (gvl :parent :value) :by-demo)))
		      (:queue ,(o-formula (gvl :window :queue)))
		      (:inter ,(o-formula (gvl :window :inter)))
                      (:left ,(o-formula
			       (+ 20 (opal:gv-right
				      (gvl :parent :feedback-obj-box)))))
		      (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
;		      (:width ,(o-formula (+ (gvl :shadow-offset)
;					     (gvl :fixed-width-size))))
;		      (:height ,(o-formula (+ (gvl :shadow-offset)
;					      (gvl :fixed-height-size))))
		      (:string "By Demo")
		      (:selection-function by-demo-interim-feedback)
		      (:gray-width 3) (:shadow-offset 5) (:text-offset 3)
		      (:final-feedback-p t))
		  (:none-button ,garnet-gadgets:radio-button
		      (:constant (t))
                      (:left ,(o-formula (+ 10 (opal:gv-right
						(gvl :parent :demo-button)))))
		      (:top ,(o-formula (opal:gv-center-y-is-center-of
					 (gvl :parent :demo-button))))
		      (:string "None")
		      (:value ,(o-formula (if (string= (gvl :parent :value)
						       "None")
					      "None")))
		      (:selection-function NIL-FEEDBACK-OBJ-FN))))))))


      (:final-feedback-obj ,opal:aggregadget
	  (:constant (:width :height))
	  (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :feedback-obj)))))
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":final-feedback-obj"))
	    (:contents ,opal:aggregadget
		(:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :final-feedback-box :width)
				       10 (gvl :demo-button :width)
				       10 (gvl :none-button :width))))
		(:height ,(o-formula (gvl :demo-button :height)))
		(:value ,(o-formula (gvl :parent :value)))
		(:field-string ,(o-formula (gvl :parent :field-string)))
                (:parts
		 ((:final-feedback-box ,SELECT-BOX
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		      (:top ,(o-formula (opal:gv-center-y-is-center-of
					 (gvl :parent :demo-button))))
		      (:string "Final Feedback")
		      (:min-frame-width 125)
		      (:selection-function FINAL-FEEDBACK-FN))
		  (:demo-button ,garnet-gadgets:text-button
		      (:constant (t))
		      (:demo-slot :selected)
		      (:demo-p t)
		      (:slot :feedback)
		      (:value ,(o-formula (eq (gvl :parent :value) :by-demo)))
		      (:queue ,(o-formula (gvl :window :queue)))
		      (:inter ,(o-formula (gvl :window :inter)))
                      (:left ,(o-formula
			       (+ 20 (opal:gv-right
				      (gvl :parent :final-feedback-box)))))
		      (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
;		      (:width ,(o-formula (+ (gvl :shadow-offset)
;					     (gvl :fixed-width-size))))
;		      (:height ,(o-formula (+ (gvl :shadow-offset)
;					      (gvl :fixed-height-size))))
		      (:string "By Demo")
		      (:selection-function by-demo-final-feedback)
		      (:gray-width 3) (:shadow-offset 5) (:text-offset 3)
		      (:final-feedback-p t))
		  (:none-button ,garnet-gadgets:radio-button
		      (:constant (t))
                      (:left ,(o-formula (+ 10 (opal:gv-right
						(gvl :parent :demo-button)))))
		      (:top ,(o-formula (opal:gv-center-y-is-center-of
					 (gvl :parent :demo-button))))
		      (:string "None")
		      (:value ,(o-formula (if (string= (gvl :parent :value)
						       "None")
					      "None")))
		      (:selection-function NIL-FINAL-FEEDBACK-FN))))))))


      (:inter-type ,garnet-gadgets:radio-button-panel
	  (:constant (t))
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :final-feedback-obj)))))
	  (:width ,(o-formula (let ((width 0))
				(gvl :radio-button-list :components)
				(opal:do-components (gvl :radio-button-list)
				 #'(lambda (button)
				     (setf width
					   (+ width (g-value button :width)))))
				(+ (gvl :v-spacing) width))))
	  (:height ,(o-formula (gvl :fixed-height-size)))
	  (:direction :horizontal)
	  (:fixed-width-p NIL)
	  (:value "Menu")
	  (:items ("Menu" "Button"))
	  (:font ,opal:default-font)
	  ;; need an extra bit of function here since :value formula was
	  ;; overridden with initial value
	  (:selection-function ,#'(lambda (panel value)
				    (s-value panel :value value)
				    (INTER-TYPE-FN panel value))))

      (:final-function ,garnet-gadgets:labeled-box
          (:constant (t))
          (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :inter-type)))))
	  (:label-string "Final Function:")
	  (:value "")
	  (:min-frame-width 150)
	  (:selection-function FINAL-FUNCTION-FN))


      (:how-set ,opal:aggregadget
	  (:constant (:left :top :width :height))
          (:left ,(o-formula (+ 10 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :final-function)))))
	  (:parts
	   ((:titled-frame ,TITLED-FRAME
		(:constant (t))
                (:string ":how-set"))
	    (:contents ,opal:aggregadget
		(:constant (:left :top :width :height))
                (:width ,(o-formula (+ (gvl :increment-box :width)
				       10 (gvl :max-value-box :width))))
		(:height ,(o-formula (+ (gvl :how-set-panel :height)
					10 (gvl :increment-box :height))))
		(:value ,(o-formula (gvl :parent :value)))
		(:parts
		 ((:how-set-panel ,garnet-gadgets:radio-button-panel
		       (:constant (t))
		       (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		       (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
		       (:height ,(o-formula (+ (gvl :h-spacing)
					       (* 2 (gvl :fixed-height-size)))))
		       (:direction :horizontal)
		       (:fixed-width-p NIL)
		       (:rank-margin 4)
		       (:font ,opal:default-font)
		       (:value ,(o-formula (let ((value (gvl :parent :value)))
					     (when (or (stringp value)
						       (keywordp value))
					       value))))
		       (:items (:set :clear :toggle "<formula>"
				:list-add :list-toggle :list-remove))
		       ;; need an extra bit of function here since :value
		       ;; formula was overridden with initial value
		       (:selection-function HOW-SET-FN))
		  (:increment-box ,garnet-gadgets:labeled-box
		      (:constant (t))
                      (:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		      (:top ,(o-formula (+ 5 (opal:gv-bottom
					      (gvl :parent :how-set-panel)))))
		      (:label-string "increment by:")
		      (:label-font ,opal:default-font)
		      (:selection-function increment-by-fn)
		      (:value ,(o-formula (let ((value (gvl :parent :value)))
					    (cond ((numberp value) 
						   (princ-to-string value))
						  ((listp value)
						   (princ-to-string (car value)))
						  (t nil)))))
		      (:min-frame-width 100))
		  (:max-value-box ,garnet-gadgets:labeled-box
		      (:constant (t))
		      (:left ,(o-formula (+ 15 (opal:gv-right
						(gvl :parent :increment-box)))))
		      (:top ,(o-formula (+ 5 (opal:gv-bottom
					      (gvl :parent :how-set-panel)))))
		      (:label-string "max value:")
		      (:label-font ,opal:default-font)
		      (:selection-function max-value-fn)
		      (:value ,(o-formula (let ((value (gvl :parent :value)))
					    (if (listp value)
						(princ-to-string (second value))))))
		      (:min-frame-width 100))))))))


      (:event-panel ,event-panel
          (:constant (t))
	  (:left ,(o-formula (+ 20 (gvl :parent :left))))
	  (:top ,(o-formula (+ 10 (opal:gv-bottom
				   (gvl :parent :how-set)))))
	  (:queue *CHOICE-INTER-QUEUE*)))))

(opal::fix-update-slots (g-value choice-menu :start-where :contents
				:select-box-panel))

(opal:update CHOICE-INTERACTOR-WIN)
(s-value choice-interactor-win :aggregate CHOICE-MENU)
(opal:update CHOICE-INTERACTOR-WIN)))

(defun choice-inter-do-stop ()
  (when (boundp 'CHOICE-INTERACTOR-WIN) (opal:destroy CHOICE-INTERACTOR-WIN)))
