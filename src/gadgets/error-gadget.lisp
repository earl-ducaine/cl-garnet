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
;;;  Error Gadget and Query Gadget
;;;
;;;  Features:
;;;   The error-gadget and query-gadget can be used in applications
;;;   where the designer wants to tell the user that some error
;;;   (possibly caused by the user) has been encountered, or to ask a
;;;   question.  When the display-** functions (described below) are
;;;   called, the error or query window becomes visible and displays
;;;   a message.
;;;
;;;   The error-gadget supplies a single button "OK", and the query
;;;   gadget supplies an arbitrary list of buttons.
;;;
;;;  Error Gadget
;;;  Customizable slots:
;;;   1) :Parent-window - The window that the error window should be centered
;;;                      inside of
;;;   2) :Font - The font for the message
;;;   3) :Justification - How to justify the multi-line message
;;;   4) :Modal-p - Whether to shut down all other interactors until the
;;;                "OK" button has been pressed
;;;   5) :Window-left, window-top, window-width, window-height - dimensions of
;;;        the error window (do not set these slots)
;;;   6) :Window - Window created by the error-gadget (do not set this slot)
;;;
;;;  Query Gadget adds:
;;;   7) :button-names - the labels of the buttons.  Default: '("OK" "CANCEL")
;;;
;;;  Error Gadget Programmer's interface:
;;;   In order to associate an error window with an application, an instance
;;;   of the error-gadget should be created with the :parent-window slot
;;;   set to the window of the application.  To activate the error
;;;   window, call the function DISPLAY-ERROR or
;;;   DISPLAY-ERROR-AND-WAIT, which takes the instance of 
;;;   the error-gadget and the desired message as parameters.
;;;
;;;  Query Gadget Programmer's interface:
;;;   Same as error gadget, but use DISPLAY-QUERY or
;;;   DISPLAY-QUERY-AND-WAIT
;;;
;;;  Caveats:
;;;   1) Update the parent window before instantiating the
;;;      error-gadget or query-gadget.
;;;   2) The instance of the error-gadget or query-gadget should not
;;;      be added to an aggregate. Bookkeeping for the parent window
;;;      is automatically taken care of during the create-instance call.

#|
============================================================
Change log:
 05/16/94  Andrew Mickish - Added :window-title to demo
 04/5/92   Andrew Mickish - Removed with-demon-enabled from initialize method
 12/14/92  Andrew Mickish - Added type and parameter declarations
 11/2/92   Masahiro Hori - Excepted :fixed-width-size in :constant list of
                           query-gadget's text-button.
 08/6/92   Andrew Mickish - Added with-demon-enabled to initialize method
 06/5/92   Brad Myers - used :modal-p windows instead of error-priority-level
                      - allow change of string for error-gadget
 04/8/92   Brad Myers - added motif-error-gadget so moved some functions
                        to error-gadget-utils
 03/26/92  Brad Myers - added query so have multiple buttons (OK, Cancel)
                      - display-error-and-wait
 09/23/91  Andrew Mickish - Added opal:update call to DISPLAY-ERROR;
              added defvar.
 03/07/91  Andrew Mickish - Changed s-value's in DISPLAY-ERROR to take the
              place of formulas in the :left, :top, :width, and :height
              slots of the error window.
 08/14/90  Pavan Reddy - removed one error-priority-level since only a
              single level is needed.  Also, set :active slot of
              :text-button-press interactor to T to fix a bug that leaves
              the interactor inactive.
 07/16/90  Andrew Mickish - Rewrote button part using new aggregadgets
============================================================
|#


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(ERROR-GADGET QUERY-GADGET)))

;; NOTE:  If :parent-window is specified, then the parent window must already
;; have been opal:update'd when the instance of ERROR-GADGET is created.
;;
(create-instance 'ERROR-GADGET opal:aggregadget
   :declare ((:parameters :parent-window :window-left :window-top :window-width
			  :window-height :justification :font :modal-p :beep-p
			  :button-name :selection-function)
	     (:type ((or (is-a-p inter:interactor-window) null) :parent-window)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		    ((member :left :center :right) :justification)
		    (string :string :button-name)
		    (kr-boolean :modal-p :beep-p)
		    (integer :window-left :window-top)
		    ((integer 0) :window-width :window-height)
		    ((or null function symbol) :selection-function)))
   (:width (o-formula (MAX (gvl :text :width) (gvl :button :width))))
   (:height (o-formula (+ 20 (gvl :text :height) (gvl :button :height))))

   ; If there is no parent window, then the error window is created at
   ; position (200, 200).
   (:window-left (o-formula (if (gvl :parent-window)
				(- (floor (gvl :parent-window :width) 2)
				   (floor (gvl :window-width) 2))
				200)))
   (:window-top (o-formula (if (gvl :parent-window)
			       (- (floor (gvl :parent-window :height) 2)
				  (floor (gvl :window-height) 2))
			       200)))
   (:window-width (o-formula (+ 20 (gvl :width))))    ; 10 on each side
   (:window-height (o-formula (+ 40 (gvl :height))))  ; 20 on top, bottom
   (:window-title "Error")

   (:parent-window NIL)    ;; The parent of the error-window
   (:string "Error")
   (:font opal:default-font)
   (:justification :center)
   (:modal-p T)
   (:selection-function NIL)
   (:button-name "OK")

   (:destroy #'Error-Gadget-Destroy)

   (:waiting NIL) ; if T, then OK should call interaction-complete.
		  ; set by display-error
   (:beep-p T)
   (:parts
    `((:text ,opal:multi-text
	     (:left ,(o-formula
		      (+ 10 (- (floor (MAX (gvl :width)
					   (gvl :parent :button :width)) 2)
			       (floor (gvl :width) 2)))))
	     (:top 20)
	     (:justification ,(o-formula (gvl :parent :justification)))
	     (:string ,(o-formula (gvl :parent :string)))
	     (:font ,(o-formula (gvl :parent :font))))

      (:button ,TEXT-BUTTON
	       (:left ,(o-formula
			(+ 10 (- (floor (MAX (gvl :width)
					     (gvl :parent :text :width)) 2)
				 (floor (gvl :width) 2)))))
	       (:top ,(o-formula (+ 20 (opal:gv-bottom
					(gvl :parent :text)))))
               (:constant (T :Except :left :top))
	       (:string ,(o-formula (gvl :parent :button-name)))
	       (:shadow-offset 5) (:text-offset 5) (:gray-width 3)
	       (:final-feedback-p NIL)
               (:selection-function
		,#'(lambda (button value)
		     (declare (ignore value))
		     (E-Q-Gadget-Sel-Func button T))) ; always use the
						      ; value T
	       (:parts
		(:shadow :gray-outline :white-field :text
		 (:feedback-obj :omit)))
       ))))

(create-instance 'QUERY-GADGET ERROR-GADGET
   :declare ((:parameters :parent-window :window-left :window-top :window-width
			  :window-height :justification :font :modal-p :beep-p
			  :button-names :selection-function)
	     (:type (cons :button-names)))
   (:window-title "Query")
   (:string "Is that OK?")
   (:button-names '("OK" "CANCEL"))
   (:parts
    `(:text 
      (:button ,TEXT-BUTTON-PANEL
	       (:left ,(o-formula
			(+ 10 (- (floor (MAX (gvl :width)
					     (gvl :parent :text :width)) 2)
				 (floor (gvl :width) 2)))))
	       (:top ,(o-formula (+ 20 (opal:gv-bottom
					(gvl :parent :text)))))
               (:constant (T :except :left :top :width :height :items
			     :fixed-width-size))
	       (:shadow-offset 5) (:text-offset 5)(:direction :horizontal)
               (:gray-width 3)
	       (:final-feedback-p NIL)
	       (:selection-function
		,#'(lambda (button value) (E-Q-Gadget-Sel-Func button value)))
               (:items ,(o-formula (gvl :parent :button-names)))))))


(define-method :initialize ERROR-GADGET (error-gadget)
  (call-prototype-method error-gadget) 
  (let ((window
	 (create-instance NIL inter:interactor-window
	    (:parent (g-value error-gadget :parent-window))
	    (:visible NIL)))
	(aggregate (create-instance NIL opal:aggregate)))
    (s-value window :aggregate aggregate)
    (opal:update window)
    ;; The :window slot of error-gadget is automatically set by add-component
;    (with-demon-enabled #'inter::inter-update-slot-invalidated
      (opal:add-component aggregate error-gadget)
;      )
    (s-value window :error-gadget error-gadget)
    (s-value window :modal-p (o-formula (gvl :error-gadget :modal-p)))))


;;; testing/demo function for error-gadgets and query gadgets
#+garnet-test
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Error-Gadget-Go Error-Gadget-Stop EGAGDET QGADGET))
  (proclaim '(special EG-WIN EG-AGG EG-FEED EGADGET QGADGET)))

#+garnet-test
(defun Error-Gadget-Go ()
  (create-instance 'EG-WIN inter:interactor-window
    (:title "test error and query gadget"))
  (s-value EG-WIN :aggregate
	   (create-instance 'EG-AGG opal:aggregate))
  (create-instance 'EGADGET error-gadget
    (:modal-p NIL)
    (:window-title "Error"))
  (create-instance 'QGADGET query-gadget
    (:window-title "Query"))
  (opal:update-all)
  (opal:add-component EG-AGG (create-instance 'EG-FEED opal:text
			       (:left 10)
			       (:top 125)
			       (:string "")))
  (opal:add-component EG-AGG (create-instance NIL text-button-panel
			       (:final-feedback-p NIL)
			       (:left 10)(:top 10)
			       (:items `( ("Error"
		  ,#'(lambda(inter val)
		       (declare (ignore inter val))
		       (display-error EGADGET "There was an error")
		       (s-value EG-FEED :string
				"You can still operate the buttons")))
					 ("Query and wait"
		  ,#'(lambda(inter val)
		       (declare (ignore inter val))
		       (s-value EG-FEED :string (format NIL "Value is ~a~%"
			       (display-query-and-wait QGADGET "Select a value"
					       '("One" "Two" "Three"))))))))))
  (opal:update EG-WIN)
  (inter:main-event-loop))

#+garnet-test
(defun error-gadget-stop ()
  (opal:destroy EG-WIN)
  (opal:destroy EGADGET)
  (opal:destroy QGADGET))
