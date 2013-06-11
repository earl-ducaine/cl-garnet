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
;;; This file and card1.lisp provides the functions and aggregadgets used 
;;; to create event cards in the interactor menus
;;;

;;; CHANGE LOG
;;;
;;; 08/24/92 amickish - Removed declaration to ignore value in Mouse-Handler

(in-package "LAPIDARY")

#|
(when (boundp 'event) (opal:destroy event))
(when (boundp 'card) (opal:destroy card))
(when (boundp 'deck) (opal:destroy deck))
|#

;;;----------------------------
;;; a list of free event-cards
;;;----------------------------
(defvar *free-cards* NIL)
;;;----------------------------

;;; =========================================================================
;;;
;;;    Functions used for the event-cards
;;;
;;; =========================================================================

(defun add-del-event (gadget value)
  (if (string-equal value "Add an event")
      (add-event)
      (del-event (g-value gadget :parent))))

;;;********************************************************
;;; Delete a card from the deck and put it with free-cards
;;;********************************************************

(defun del-event (card)
  (declare (special *event-info*))
  (if (cdr (g-value *event-info* :event-cards))
      (remove-card card)
      (lapidary-error "this is the only event so you cannot delete it")))

(defun remove-card (card)
  (declare (special *event-info*))
  (s-value (g-value card :window) :visible nil)
  (s-value *event-info* :event-cards
	   (delete card (g-value *event-info* :event-cards)))
  (push card *free-cards*))
  
;;;****************************************************
;;; Add a event card to the deck with default settings
;;;****************************************************

(defun add-event ()
  (declare (special *event-info*))
  (let ((new-event (make-card))
	(last-card (car (g-value *event-info* :event-cards)))
	(kr::*constants-disabled* nil))
    (push new-event (g-value *event-info* :event-cards))
    (s-value (g-value new-event :mouse) :value
	     (if (eq :start-event (g-value *event-info* :event-type))
		 "leftdown" "leftup"))
    (gilt:show-in-window new-event 
			 (if last-card 
			     (+ (g-value last-card :window :left) 40)
			     100)
			 (if last-card
			     (+ (g-value last-card :window :top) 40)
			     100))))

(defun make-card ()
  (let ((new-card (or (pop *free-cards*)
		      (create-instance NIL event-card))))
    ;;; g-value the :value fields to make sure they are not wiped out
    ;;; by the s-value in clear-event-slots
    (g-value new-card :modifiers :value)
    (g-value new-card :mouse :value)
    (g-value new-card :any-key :value)
    (g-value new-card :key :value)
    (clear-event-slots new-card '(:modifiers :mouse :any-key :key))
    new-card))
  
;;;*******************************************
;;; Clear certain value slots within an event
;;;*******************************************

(defun clear-event-slots (event slot-list)
  (dolist (slot slot-list)
    (s-value (g-value event slot) :value NIL)
    (s-value (g-value event slot) :old-value nil)))

;;;*******************************************************
;;; Make an event specification from provided information 
;;;*******************************************************

(defun make-event (mod-bits string)
  (let* ((mod-string (bits-to-mod-string mod-bits))
	 (mod-string-p (not (string= mod-string ""))))
    (cond (mod-string-p
	   (keyword-from-string (concatenate 'string
				 mod-string 
				 "-"
				 (if (and (= (length string) 1)
					  (lower-case-p (character string)))
				       "\\" "")
				 string)))
        ;; determine if the string is a single letter character, a
        ;; graphic character, such as return, or a user-defined 
        ;; character (the default case)
	  ((= (length string) 1) (character string))
	  ((name-char string))
	  (t (keyword-from-string string)))))


;;; =========================================================================
;;;
;;;    handlers for the various button groups in event cards
;;;
;;; =========================================================================

(defun modifiers-handler (gadget value)
  ;; find which button was selected--if a button was deselected, nothing
  ;; needs to be done
  (let ((button (car (set-difference value (g-value gadget :old-value) 
				     :test #'string-equal)))
	(mouse (g-value gadget :parent :mouse)))

    ;; if any-mousedown or any-mouseup is already selected, then none
    ;; of the modifier keys are valid
    (cond ((or (string-equal (g-value mouse :value) "any-mousedown")
	       (string-equal (g-value mouse :value) "any-mouseup"))
	   (lapidary-error (format nil "~S is built-in to ~S and
thus is not a valid selection" button (g-value mouse :value)))
	   (s-value gadget :value (g-value gadget :old-value))
	   (return-from modifiers-handler))

	  ;; if the button is "any modifier", make sure that a mouse button
	  ;; is already selected
	  ((string-equal button "any modifier")
	   (when (not (g-value mouse :value))
		 (lapidary-error "You cannot use the any modifier prefix with keyboard characters.
You need to create a separate event for each set of
modifiers and keyboard combinations that you want")
		 (s-value gadget :value (g-value gadget :old-value))
		 (return-from modifiers-handler))
	   (s-value gadget :value '("any modifier")))

	  ;; if the button is "shift", clear the "any modifier" button.
	  ;; since shift is not a valid modifier with a key, clear the
	  ;; key and any-key entries and set the mouse button to leftdown
	  ;; if it is not already set
	  ((string-equal button "shift")
	   (when (not (g-value mouse :value))	       
		 (lapidary-error "shift is built-in to the keyboard characters
and thus is not a valid selection")
		 (s-value gadget :value (g-value gadget :old-value))
		 (return-from modifiers-handler))
	   (s-value gadget :value 
		    (delete "any modifier" value :test #'string-equal)))

	  ;; if the buttons are "control" or "meta", clear the "any modifier"
	  ;; and any-key buttons. If the any-key button is set, set the
	  ;; mouse button to leftdown
	  (t 
	   (when (g-value gadget :parent :any-key :value)
		 (lapidary-error (format nil "~S is not a valid modifier for any keypress" (pop value)))
		 (s-value gadget :value value)
		 (return-from modifiers-handler))
	   (s-value gadget :value 
		    (delete "any modifier" value :test #'string-equal))))
    ;; save the new value
    (s-value gadget :old-value (g-value gadget :value))))

(defun mouse-handler (gadget value)
  (clear-event-slots (g-value gadget :parent) '(:key :any-key))
  ;; when any-mousedown or any-mouseup is selected, any selected
  ;; modifiers must be unselected
  (when (or (string-equal value "any-mousedown")
	    (string-equal value "any-mouseup"))
	(clear-event-slots (g-value gadget :parent) '(:modifiers))))

(defun any-key-handler (gadget value)
  (declare (ignore value))
  (clear-event-slots (g-value gadget :parent) '(:modifiers :mouse :key)))

(defun key-handler (gadget value)
  (declare (ignore value))
  (let ((modifiers (g-value gadget :parent :modifiers :value)))
    (clear-event-slots (g-value gadget :parent) '(:mouse :any-key))
    (setf modifiers (delete "any modifier" modifiers :test #'equal))
    (setf modifiers (delete "shift" modifiers :test #'equal))
    (s-value (g-value gadget :parent :modifiers) :value modifiers)))

;;; =========================================================================
;;;
;;;    Event card definitions: event, card, and deck
;;;
;;; =========================================================================

#|
;;;***************************************************
;;; Definition of an event (an event card's contents)
;;;***************************************************

(create-instance 'event opal:aggregadget
   (:on-top (o-formula (gvl :parent :on-top)))
   (:event-symbol (o-formula
		   (let ((modifier (concatenate 'string
						(gvl :mod-any :value)
						(gvl :mod-shift :value)
						(gvl :mod-control :value)
						(gvl :mod-meta :value)))
			 (string (or (gvl :mouse :value)
				     (gvl :key-any :value)
				     (gvl :key-spec :value))))
		     (make-event (if (string= "" modifier)
				     NIL
				     modifier)
				 string))))
   (:parts
    `((:mod-text ,opal:text
		 (:left ,(o-formula (gvl :parent :left)))
		 (:top ,(o-formula (gv-center-my-top
				    (gvl :parent :mod-shift))))
		 (:string "Modifiers:")
		 (:font ,*bold-font*))
      (:mod-shift ,garnet-gadgets:x-button
		  (:top ,(o-formula (gvl :parent :top)))
		  (:left ,(o-formula (gvl :parent :mouse :left)))
		  (:string "shift")
		  (:value ,(o-formula (if (gvl :selected) "SHIFT-")))
		  (:selection-function
		   ,#'(lambda (gadget val)
			(let ((mouse (g-value gadget :parent :mouse)))
			  (if (not (g-value mouse :value))
			      (s-value mouse :value "leftdown")))
			(clear-event-slots (g-value gadget :parent)
					   '(:key-any :key-spec :mod-any))
			(let ((deck (g-value gadget :parent :parent :parent)))
			  (dialog-enqueue (g-value deck :event-type)
					  (g-value deck :event-list)
					  (symbol-value
					   (g-value deck :queue))))))
		  (:interactors
		   ((:x-button-press :modify
		       (:active ,(o-formula
				  (gvl :operates-on :parent :on-top)))))))
      (:mod-control ,garnet-gadgets:x-button
		    (:top ,(o-formula (gvl :parent :top)))
		    (:left ,(o-formula (+ 10 (opal:gv-right
					      (gvl :parent :mod-shift)))))
		    (:string "control")
		    (:value ,(o-formula (if (gvl :selected) "CONTROL-")))
		    (:selection-function
		     ,#'(lambda (gadget val)
			  (clear-event-slots (g-value gadget :parent)
					     '(:mod-any :key-any))
			  (let ((deck (g-value gadget :parent :parent :parent)))
			    (dialog-enqueue (g-value deck :event-type)
					    (g-value deck :event-list)
					    (symbol-value
					     (g-value deck :queue))))))
		    (:interactors
		     ((:x-button-press :modify
			 (:active ,(o-formula
				    (gvl :operates-on :parent :on-top)))))))
      (:mod-meta ,garnet-gadgets:x-button
		 (:top ,(o-formula (gvl :parent :top)))
		 (:left ,(o-formula (+ 10 (opal:gv-right
					   (gvl :parent :mod-control)))))
		 (:string "meta")
		 (:value ,(o-formula (if (gvl :selected) "META-")))
		 (:selection-function
		  ,#'(lambda (gadget val)
		       (clear-event-slots (g-value gadget :parent)
					  '(:mod-any :key-any))
		       (let ((deck (g-value gadget :parent :parent :parent)))
			 (dialog-enqueue (g-value deck :event-type)
					 (g-value deck :event-list)
					 (symbol-value
					  (g-value deck :queue))))))
		 (:interactors
		  ((:x-button-press :modify
		      (:active ,(o-formula
				 (gvl :operates-on :parent :on-top)))))))
      (:mod-any ,garnet-gadgets:x-button
		(:top ,(o-formula (gvl :parent :top)))
		(:left ,(o-formula (+ 10 (opal:gv-right
					  (gvl :parent :mod-meta)))))
		(:string "any modifier")
		(:value ,(o-formula (if (gvl :selected) "ANY-")))
		(:selection-function
		 ,#'(lambda (gadget val)
		      (let ((mouse (g-value gadget :parent :mouse)))
			(if (not (g-value mouse :value))
			    (s-value mouse :value "leftdown")))
		      (clear-event-slots (g-value gadget :parent)
					 '(:mod-shift :mod-control
					   :mod-meta :key-any :key-spec))
		      (let ((deck (g-value gadget :parent :parent :parent)))
			(dialog-enqueue (g-value deck :event-type)
					(g-value deck :event-list)
					(symbol-value
					 (g-value deck :queue))))))
		(:interactors
		 ((:x-button-press :modify
		     (:active ,(o-formula
				(gvl :operates-on :parent :on-top)))))))
      (:mouse-text ,opal:text
		   (:left ,(o-formula (gvl :parent :left)))
		   (:top ,(o-formula (gv-center-my-top (gvl :parent :mouse))))
		   (:string "Mouse Action:")
		   (:font ,*bold-font*))
      (:mouse ,garnet-gadgets:radio-button-panel
	      (:left ,(o-formula (+ 10 (opal:gv-right
					(gvl :parent :mouse-text)))))
	      (:top ,(o-formula (+ 10 (opal:gv-bottom
				       (gvl :parent :mod-shift)))))
	      (:rank-margin 4)
	      (:font ,opal:default-font)
	      (:value ,(o-formula (let ((obj (gvl :value-obj)))
				    (if obj (gv obj :string) "leftdown"))))
	      (:selection-function
	       ,#'(lambda (gadget val)
		    (clear-event-slots (g-value gadget :parent)
				       '(:key-spec :key-any))
		    (let ((deck (g-value gadget :parent :parent :parent)))
		      (dialog-enqueue (g-value deck :event-type)
				      (g-value deck :event-list)
				      (symbol-value
				       (g-value deck :queue))))))
	      (:h-align :right)
	      (:direction :horizontal)
	      (:items ("leftdown" "middledown" "rightdown" "mousedown"
		       "leftup" "middleup" "rightup" "mouseup"))
	      (:interactors
	       ((:radio-button-press :modify
		   (:active ,(o-formula
			      (gvl :operates-on :parent :on-top)))))))
      (:key-text ,opal:text
		 (:left ,(o-formula (gvl :parent :left)))
		 (:top ,(o-formula (gv-center-my-top (gvl :parent :key-any))))
		 (:string "Keyboard:")
		 (:font ,*bold-font*))
      (:key-any ,garnet-gadgets:radio-button
		(:left ,(o-formula (opal:gv-right (gvl :parent :key-text))))
		(:top ,(o-formula (+ 10 (opal:gv-bottom
					 (gvl :parent :mouse)))))
		(:string "   Any keypress:")
		(:value ,(o-formula (if (gvl :selected) "ANY-KEYBOARD")))
		(:selection-function
		 ,#'(lambda (gadget val)
		      (clear-event-slots (g-value gadget :parent)
					 '(:mouse :key-spec :mod-shift
					   :mod-control :mod-meta :mod-any))
		      (let ((deck (g-value gadget :parent :parent :parent)))
			(dialog-enqueue (g-value deck :event-type)
					(g-value deck :event-list)
					(symbol-value
					 (g-value deck :queue))))))
		(:interactors
		 ((:radio-button-press :modify
		     (:active ,(o-formula
				(gvl :operates-on :parent :on-top)))))))
      (:key-spec ,garnet-gadgets:labeled-box
		 (:left ,(o-formula (+ 10 (opal:gv-right
					   (gvl :parent :key-any)))))
		 (:top ,(o-formula (gv-center-my-top (gvl :parent :key-any))))
		 (:label-string "Specific keypress:")
		 (:label-font ,opal:default-font)
		 (:value NIL)
		 (:selection-function
		  ,#'(lambda (gadget val)
		       (clear-event-slots (g-value gadget :parent)
					  '(:mouse :key-any
					    :mod-shift :mod-any))
		       (let ((deck (g-value gadget :parent :parent :parent)))
			 (dialog-enqueue (g-value deck :event-type)
					 (g-value deck :event-list)
					 (symbol-value 
					  (g-value deck :queue))))))
		 (:interactors
		  ((:text-inter :modify
		      (:active ,(o-formula
				 (gvl :operates-on :parent :on-top)))))))
      (:add-del ,garnet-gadgets:text-button-panel
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (+ 10 (opal:gv-bottom
					 (gvl :parent :key-any)))))
		(:final-feedback-p NIL)
		(:direction :horizontal)
		(:items (("Add an event"
			  ,#'(lambda (gadget val)
			       (add-event
				(g-value gadget :parent :parent :parent))))
			 ("Delete this event"
			  ,#'(lambda (gadget val)
			       (del-event
				(g-value gadget :parent :parent))))))
		(:interactors
		 ((:text-button-press :modify
		     (:active ,(o-formula
				(gvl :operates-on :parent :on-top))))))))))

|#
