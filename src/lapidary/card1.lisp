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
;;; 08/24/92 Martin Sjolin - Added zerop check in PARSE

(in-package "LAPIDARY")

#|
;;;*****************************
;;; Definition of a single card
;;;*****************************

(create-instance 'card opal:aggregadget
   (:left (o-formula (+ (gvl :parent :left)
			(* (- (gvl :parent :max-item) (gvl :rank) 1) 25))))
   (:top (o-formula (+ (gvl :parent :top)
		       (* (- (gvl :parent :max-item) (gvl :rank) 1) 25))))
   (:rank 0)
   (:event-symbol (o-formula (gvl :contents :event-symbol)))
   (:on-top (o-formula (eq (gv :self) (gvl :parent :top-card))))
   (:parts
    `((:frame ,opal:rectangle
	      (:left ,(o-formula (gvl :parent :left)))
	      (:top ,(o-formula (gvl :parent :top)))
	      (:width ,(o-formula (+ (gvl :parent :contents :width) 20)))
	      (:height ,(o-formula (+ (gvl :parent :contents :height) 20)))
	      (:filling-style ,opal:white-fill))
      (:contents ,event
		 (:left ,(o-formula (+ (gvl :parent :left) 10)))
		 (:top ,(o-formula (+ (gvl :parent :top) 10)))))))

;;;**********************************************************
;;; Definition a card deck (Be sure to set the :queue slot!)
;;;**********************************************************

(create-instance 'deck opal:aggregadget
   (:left 5)
   (:top 5)
   (:queue '*QUEUE*)
   (:event-type :start-event)
   (:event-list (o-formula (let (result)
			     (gvl :components)
			     (dovalues (card (gv :self) :components)
			       (push (gv card :event-symbol) result))
			     (if (cdr result)
				 result
				 (car result)))))
   (:max-item (o-formula (length (gvl :components))))
   (:top-card (o-formula (car (last (gvl :components)))))
   (:interactors
    `((NIL ,inter:button-interactor
	   (:window ,(o-formula (gvl :operates-on :window)))
	   (:start-event :rightdown)
	   (:start-where ,(o-formula (list :element-of (gvl :operates-on))))
	   (:final-function ,#'(lambda (inter card)
			      (opal:move-component
			       (g-value inter :operates-on) card)))))))
|#

;;; translate a modifier string to individual modifier bits; the lists
;;; are kept in alphabetical order because bits-to-mod-string uses rassoc
;;; to search the list and it sorts its search list into alphabetical order

(defvar *modifier-list* '((any "any modifier")
			  (shift "shift")
			  (control "control")
			  (meta "meta")
			  (shift-control "control" "shift")
			  (shift-meta "meta" "shift")
			  (control-meta "control" "meta")
			  (shift-control-meta "control" "meta" "shift")))

(defun mod-string-to-bits (mod-string)
  (declare (special *modifier-list*))
  (cdr (assoc (read-from-string mod-string) *modifier-list*)))

(defun bits-to-mod-string (mod-bits)
  (let ((mod-entry (rassoc (sort mod-bits #'string-lessp)
			   *modifier-list* :test #'equal)))
    (if mod-entry
	(princ-to-string (car mod-entry))
        "")))

;;;
;;; this function
;;; parses an event into its modifiers (shift, control, meta, or any)
;;; and its mouse button (e.g., leftdown) or key (e.g., a)
;;;

(defun parse (event-symbol)
  (declare (special *event-info*))
  (let ((card (make-card))
	(event-string 
	 (cond ((keywordp event-symbol) (symbol-name event-symbol))
	       ((graphic-char-p event-symbol) (string event-symbol))
	       (t (char-name event-symbol))))			    
	(last-card (car (g-value *event-info* :event-cards)))
	(mod-end 0) 
	(kr::*constants-disabled* nil)
	mod-bits char)

    (cond ((string= "ANY-KEYBOARD" event-string)
	   (s-value (g-value card :any-key) :value "Any keypress:"))
	  ((string-equal "any-mousedown" event-string)
	   (s-value (g-value card :mouse) :value "any-mousedown"))
	  ((string-equal "any-mouseup" event-string)
	   (s-value (g-value card :mouse) :value "any-mouseup"))
	  (t
           ;;; separate the modifiers from the mouse button or key. Do this by
           ;;; individually searching for "any", "shift", "meta", and
	   ;;; "control". It would be nice to just look for the last '-',
	   ;;; but this does not work because some character names can have
	   ;;; dashes. Add the number of characters in these strings to a
	   ;;; running count, so that the modifiers can be stripped off
	   ;;; to yield the mouse button or character string.
	   (dolist (modifier-length '(("ANY" 4) ("SHIFT" 6)
				      ("CONTROL" 8) ("META" 5)))
		   (when (search (car modifier-length) event-string 
				 :test #'char-equal)
			 (push (car modifier-length) mod-bits)
			 (incf mod-end (second modifier-length))))

	   (s-value (g-value card :modifiers) :value mod-bits)

	   ;;; determine if this is a mouse button or a key
	   (setf char (if (zerop mod-end) (subseq event-string mod-end)
			  event-string))
	   (if (member char
		       '("leftdown" "middledown" "rightdown" "mousedown"
			 "leftup" "middleup" "rightup" "mouseup")
		       :test #'string-equal)
	       (s-value (g-value card :mouse) :value
			(string-downcase char))
	       (s-value (g-value card :key) :value char))))
    (push card (g-value *event-info* :event-cards))
    (gilt:show-in-window card 
	     (if last-card (+ (g-value last-card :window :left) 40) 100)
	     (if last-card (+ (g-value last-card :window :top) 40) 100))))
			     
#|
(defun fill-deck (deck event-list)
  (cond ((null event-list) (add-event deck))
	((atom event-list) (parse (make-card deck) event-list))
	((listp event-list) (dolist (event-symbol event-list)
			      (parse (make-card deck) event-symbol)))))

;;;======================================================================
;;;
;;; create a list of events from the cards in the event window and make
;;; the event window disappear
;;;
;;;======================================================================

(defun create-events (gadget value)
  (let ((deck (g-value gadget :parent :deck)))
    (dialog-enqueue (g-value deck :event-type)
		    (g-value deck :event-list)
		    (symbol-value (g-value deck :queue)))
    
    (mapc #'remove-card
	  (g-value gadget :parent :deck :components))
    (s-value event-win :visible nil)))
|#

(defun event-ok (gadget value)
  (declare (ignore gadget value))
  (declare (special *event-info*))
  (let (events)
    (dolist (card (g-value *event-info* :event-cards))
	    (push (make-event 
		   (g-value card :modifiers :value)
		   (or (g-value card :mouse :value)
		       (and (g-value card :any-key :value) "any-keyboard")
		       (g-value card :key :value))) 
		  events))
    (dialog-enqueue (g-value *event-info* :event-type)
		    events
		    (symbol-value (g-value *event-info* :queue)))
    ;; easy way of returning cards to free list and making them invisible
    (event-cancel nil nil)))
    

(defun event-cancel (gadget value)
  (declare (ignore gadget value))
  (declare (special *event-info* *free-cards*))
  (dolist (card (g-value *event-info* :event-cards))
	  (s-value (g-value card :window) :visible nil)
	  (push card *free-cards*))
  (s-value *event-info* :event-cards nil))

;;;======================================================================
;;;
;;; fill the cards in the event window with the appropriate information
;;; about the selected event and then display the event window
;;;
;;;======================================================================

(defun event-final-function (gadget value)
  (declare (special *event-info*))
  (let ((event-list 
	 (or (cdr (assoc value (symbol-value (g-value gadget :queue))))
		 (g-value gadget :inter value))))
    ;; place the appropriate slot-queue and event-type into *event-info*
    (s-value *event-info* :event-type value)
    (s-value *event-info* :queue (g-value gadget :queue))
    (s-value *event-info* :event-cards nil)

    ;; put the event information in the event cards
    (cond ((null event-list) (add-event))
	  ((atom event-list) (parse event-list))
	  ((listp event-list) (dolist (event-symbol event-list)
				      (parse event-symbol))))))


(defvar event-win nil)
#|
(defun make-event-window ()
  (let (event-agg)
    (create-instance 'event-win inter:interactor-window
      #+apple (:top 50)
      (:width 570)(:height 275)(:title "event window"))

    (setf event-agg (create-instance nil opal:aggregadget
      (:left 5)(:top 5)
      (:parts
       `((:ok ,garnet-gadgets:text-button
	      (:left 480)(:top 5)
	      (:string "OK")
	      (:final-feedback-p nil)
	      (:selection-function ,#'create-events))
	 (:deck ,deck
		(:left 5)(:top 50)
		(:queue ,(o-formula (gvl :window :queue)))
		(:event-type ,(o-formula (gvl :window :event-type))))))))

    ;; add event-agg to the window
    (s-value event-win :aggregate event-agg)
    (opal:update event-win)))

|#

(create-instance 'event-panel garnet-gadgets:text-button-panel
   (:left 5)
   (:top 5)
   (:inter (o-formula (gvl :window :inter)))
   (:final-feedback-p NIL)
   (:direction :horizontal)
   (:items '(:start-event :stop-event :abort-event))
   (:selection-function #'event-final-function))



