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
;;;  Text Interactor dialog box
;;;
;;;
;;;  Change Log:
;;;
;;;  07/14/93 amickish - Added declarations to TEXT-INTER-AGG-OF-ITEMS-FN,
;;;             TEXT-INTER-ANYWHERE-FN, TEXT-FEEDBACK-OBJ-FN

;;;  Note: This file needs parts from the file dialog-parts.lisp


(in-package "LAPIDARY")

(defvar *TEXT-INTER-QUEUE* NIL)

(defmacro TEXT-START-WHERE ()
  `(g-value TEXT-INTERACTOR-MENU :start-where))
(defmacro TEXT-FEEDBACK-OBJ ()
  `(g-value TEXT-interactor-MENU :feedback-obj))

;;; *********************************
;;; Change the name of the interactor
;;; *********************************
;;;    -- interactor-name is a string
(defun TEXT-INTERACTOR-NAME-FN (gadget interactor-name)
  (declare (ignore gadget))
  (dialog-enqueue :known-as
		  (if (string/= "" interactor-name)
		      (read-from-string
		       (concatenate 'string ":" interactor-name)))
		  *TEXT-INTER-QUEUE*))


;;; ************************************
;;; Functions for the :start-where panel
;;; ************************************

;;;    :start-where is in an aggregate of items
(defun TEXT-INTER-AGG-OF-ITEMS-FN (agg-box button-label)
  (declare (special TEXT-INTERACTOR-MENU)
	   (ignore agg-box))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(start-where (TEXT-START-WHERE)))
    (if selection
	(progn
	  (s-value start-where :field-string (name-for-schema selection))
	  (s-value start-where :value button-label)
	  (dialog-enqueue :start-where
			  (if (g-value start-where :type)
			      `(:element-of ,selection :type
					,@(g-value start-where :type-restriction))
			      `(:element-of ,selection))
			  *TEXT-INTER-QUEUE*))
	(progn
	  (s-value start-where :field-string nil)
	  (s-value start-where :value nil)
	  (s-value start-where :type nil)))))

;;;    :start-where is t
(defun TEXT-INTER-ANYWHERE-FN (obj-box button-label)
  (declare (special TEXT-INTERACTOR-MENU)
	   (ignore obj-box))
  (let ((start-where (TEXT-START-WHERE)))
    (s-value start-where :value button-label)
    (s-value start-where :type nil)
    (dialog-enqueue :start-where t *TEXT-INTER-QUEUE*)))

;;;*************************************************
;;; Change the :obj-to-change slot of the interactor
;;;*************************************************
(defun TEXT-INTER-OBJ-TO-CHANGE-FN (panel value)
  (declare (special *selection-info*))
  (s-value (g-value panel :parent :parent) :value value)
  (cond ((string= value "<formula>")
	 (create-custom-inter-constraint (g-value panel :window :inter) 
					 :obj-to-change
					 '*text-inter-queue*))
	((string= value "change this object")
	 (let ((obj-to-change (car (g-value *selection-info* :selected))))
	   (when (null obj-to-change)
		 (lapidary-error "please make a selection, then press 
'change this object' again")
		 (return-from text-inter-obj-to-change-fn))
	   (dialog-enqueue :obj-to-change obj-to-change *TEXT-INTER-QUEUE*)
	   (s-value panel :obj-to-change obj-to-change)))
	(t (dialog-enqueue :obj-to-change nil *TEXT-INTER-QUEUE*))))

;;; *************************************
;;; Functions for the :feedback-obj panel
;;; *************************************

(defun TEXT-FEEDBACK-OBJ-FN (feedback-obj-box button-label)
  (declare (special *selection-info* TEXT-INTERACTOR-MENU))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (cond ((null selection)
	   (s-value (TEXT-FEEDBACK-OBJ) :field-string nil)
	   (s-value (TEXT-FEEDBACK-OBJ) :value nil)
	   (lapidary-error "please make a selection, then press the
interim feedback button again"))
	((null (cdr selection)) ; only one selection
	 (s-value (TEXT-FEEDBACK-OBJ) :field-string
		  (name-for-schema (car selection)))
	 (s-value (TEXT-FEEDBACK-OBJ) :value button-label)
	 (dialog-enqueue :feedback-obj selection *TEXT-INTER-QUEUE*))
	(t ; multiple selections
	 (s-value (TEXT-FEEDBACK-OBJ) :field-string
		  (princ-to-string selection))
	 (s-value (TEXT-FEEDBACK-OBJ) :value button-label)
	 ;; pop up C32 and ask the user to enter a formula that
	 ;; selects which feedback object to use
	 (get-inter-feedback-formula (g-value feedback-obj-box :window :inter)
				     :feedback-obj
				     selection
				     '*text-inter-queue*)))))

(defun text-NIL-FEEDBACK-OBJ-FN (button button-label)
  (declare (special text-interactor-menu))
  (declare (ignore button))
  (dialog-enqueue :feedback-obj NIL *TEXT-INTER-QUEUE*)
  (s-value (TEXT-FEEDBACK-OBJ) :field-string nil)
  (s-value (TEXT-FEEDBACK-OBJ) :value button-label))

;;;********************************
;;; Change where the cursor appears
;;;********************************
(defun TEXT-INTER-CURSOR-FN (panel value)
  (s-value (g-value panel :parent) :value value)
  (dialog-enqueue :cursor-where-press
		  (string= value "where pressed")
		  *TEXT-INTER-QUEUE*))

;;;********************************
;;; Change whether the text scrolls
;;;********************************
(defun TEXT-INTER-SCROLLING-TEXT-FN (button value)
  (declare (ignore button))
  (dialog-enqueue :scrolling-text-p (if value T) *TEXT-INTER-QUEUE*))

;;;********************************
;;; Change the :final-function slot
;;;********************************
(defun TEXT-INTER-FINAL-FUNCTION-FN (gadget value)
  (declare (ignore gadget))
  (dialog-enqueue :final-function 
		  (if (string= "" value)
		      nil
		      (read-from-string value))
		  *TEXT-INTER-QUEUE*))

(defun text-inter-do-go ()
  (let ((kr::*constants-disabled* nil))
  (text-inter-do-stop)

  (create-instance 'TEXT-INTERACTOR-WIN inter:interactor-window
		   (:title "text interactor")
		   #+apple (:top 50)
		   (:width 534)(:height 543)
		   (:queue '*TEXT-INTER-QUEUE*))

  (create-instance 'TEXT-INTERACTOR-MENU opal:aggregadget
   (:left 5)
   (:top 5)
   (:parts
    `((:title ,opal:text
	  (:constant (t))
	  (:left ,(o-formula (+ (gvl :parent :left) 10)))
	  (:top ,(o-formula (gvl :parent :top)))
	  (:string "Text Interactor")
	  (:font ,*very-large-bold-italic-serif-font*))

      (:known-as ,NAME-BOX
	  (:constant (t))
          (:selection-function TEXT-INTERACTOR-NAME-FN))

      (:act-buttons ,ACT-BUTTONS
	  (:constant (t))
	  (:left ,(o-formula (+ 20 (opal:gv-right
				    (gvl :parent :start-where)))))
	  (:queue *TEXT-INTER-QUEUE*))

      (:start-where ,START-WHERE
	  (:constant (t))
          (:items (("Start Anywhere in Window" TEXT-INTER-ANYWHERE-FN)
		   ("One of this aggregate" TEXT-INTER-AGG-OF-ITEMS-FN))))

      (:obj-to-change ,opal:aggregadget
		(:constant (:left :top :height :width))
		(:left ,(o-formula (gvl :parent :start-where :left)))
		(:top ,(o-formula (+ (opal:gv-bottom
				      (gvl :parent :start-where)) 10)))
		(:parts
		 ((:titled-frame ,titled-frame
				 (:constant (t))
				 (:string ":object-to-change"))
		  (:contents ,opal:aggregadget
		      (:constant (:left :top :width :height))
		      (:left ,(o-formula (+ (gvl :parent :left) 10)))
		      (:top ,(o-formula (+ (gvl :parent :titled-frame
						:frame :top) 10)))
		      (:value ,(o-formula (gvl :parent :value)))
		      (:parts
		       ((:choice ,garnet-gadgets:radio-button-panel
			    (:constant (t))
			    (:left ,(o-formula (gvl :parent :left)))
			    (:top ,(o-formula (gvl :parent :top)))
			    (:items ("result of start-where" 
				     "change this object"
				     "<formula>"))
			    (:font ,opal:default-font)
			    (:v-spacing 10)
			    (:h-align :center)
			    (:value ,(o-formula (gvl :parent :value)))
			    (:selection-function TEXT-INTER-OBJ-TO-CHANGE-FN))
			(:change-this-obj-feedback ,TEXT-BOX
			    (:constant (t))
			    (:obj-next-to ,(o-formula
					    (second (gvl :parent :choice 
							 :radio-button-list 
							 :components))))
			    (:left ,(o-formula 
				     (+ 10 (opal:gv-right (gvl :obj-next-to)))))
					    
			    (:top ,(o-formula (opal:gv-center-y-is-center-of
					       (gvl :obj-next-to))))
			    (:string ,(o-formula (if (gvl :obj-next-to :selected)
						     (princ-to-string 
						      (gvl :parent :choice
							   :obj-to-change))
						     ""))))))))))
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
		      (:selection-function text-FEEDBACK-OBJ-FN))
		  (:none-button ,garnet-gadgets:radio-button
             	      (:constant (t))
                      (:left ,(o-formula (+ 10 (opal:gv-right
						(gvl :parent :feedback-obj-box)))))
		      (:top ,(o-formula (+ 20 (gvl :parent :parent :top))))
		      (:string "None")
		      (:value ,(o-formula (if (string= (gvl :parent :value)
						       "None")
					      "None")))
		      (:selection-function text-NIL-FEEDBACK-OBJ-FN))))))))

      (:cursor-where-press ,opal:aggregadget
	  (:constant (:left :top :width :height))
	  (:left ,(o-formula (gvl :parent :feedback-obj :left)))
	  (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :feedback-obj)) 10)))
	  (:parts
	   ((:cursor-label ,opal:text
		(:constant (t))
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (opal:gv-center-y-is-center-of
				   (gvl :parent :cursor))))
		(:font ,*bold-font*)
		(:string "Cursor appears:"))
	    (:cursor ,garnet-gadgets:radio-button-panel
		(:constant (t))
	        (:left ,(o-formula (+ (opal:gv-right
				       (gvl :parent :cursor-label)) 15)))
		(:top ,(o-formula (gvl :parent :top)))
		(:direction :horizontal)
		(:fixed-width-p NIL)
		(:font ,opal:default-font)
		(:value ,(o-formula (gvl :parent :value)))
		(:items ("where pressed" "at end of string"))
		(:selection-function TEXT-INTER-CURSOR-FN)))))
      (:final-function ,garnet-gadgets:labeled-box
		(:constant (t))
		(:left ,(o-formula (gvl :parent :cursor-where-press :left)))
		(:top ,(o-formula (+ 10 (opal:gv-bottom
					 (gvl :parent :cursor-where-press)))))
		(:min-frame-width 150)
		(:label-string "Final Function:")
		(:value "")
		(:selection-function TEXT-INTER-FINAL-FUNCTION-FN))
      (:event-panel ,event-panel
		(:constant (t))
	        (:left ,(o-formula (gvl :parent :final-function :left)))
		(:top ,(o-formula (+ 10 (opal:gv-bottom
					 (gvl :parent :final-function)))))
		(:queue *TEXT-INTER-QUEUE*)
		(:inter ,(o-formula (gvl :window :inter)))))))

(opal::fix-update-slots (g-value text-interactor-menu :start-where :contents
				:select-box-panel))

(s-value TEXT-INTERACTOR-WIN :aggregate TEXT-INTERACTOR-MENU)
(opal:update TEXT-INTERACTOR-WIN)))


(defun text-inter-do-stop ()
  (when (boundp 'TEXT-INTERACTOR-WIN) (opal:destroy TEXT-INTERACTOR-WIN)))
