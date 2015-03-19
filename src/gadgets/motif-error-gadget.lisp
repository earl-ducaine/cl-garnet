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
;;;  Motif Error Gadget and Motif Query Gadget
;;;
;;;  Features:
;;;   The motif-error-gadget and motif-query-gadget can be used in applications
;;;   where the designer wants to tell the user that some error
;;;   (possibly caused by the user) has been encountered, or to ask a
;;;   question.  When the display-** functions (described below) are
;;;   called, the error or query window becomes visible and displays
;;;   a message.
;;;
;;;   The motif-error-gadget supplies a single button "OK", and the motif query
;;;   gadget supplies an arbitrary list of buttons.
;;;
;;;  Motif Error Gadget
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
;;;   7) :foreground-color - color of the window.  Default opal:motif-orange
;;;
;;;  Query Gadget adds:
;;;   8) :button-names - the labels of the buttons.  Default: '("OK" "CANCEL")
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
;;;      motif-error-gadget or motif-query-gadget.
;;;   2) The instance of the motif-error-gadget or motif-query-gadget
;;;      should not be added to an aggregate. Bookkeeping for the parent window
;;;      is automatically taken care of during the create-instance call.

#|
============================================================
Change log:
 05/16/94  Andrew Mickish - Added :window-title to demo
 10/06/93  Andrew Mickish - :background-color ---> :foreground-color
  4/05/93  Andrew Mickish - Removed with-demon-enabled call from :initialize
             method.
 12/15/92  Andrew Mickish - Added type and parameter declarations
 06/05/92  Brad Myers - used :modal-p windows instead of error-priority-level
                      - allow change of string for error-gadget
 04/08/92  Brad Myers - created based on error-gadget
============================================================
|#


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(MOTIF-ERROR-GADGET MOTIF-QUERY-GADGET)))

;; NOTE:  If :parent-window is specified, then the parent window must already
;; have been opal:update'd when the instance of ERROR-GADGET is created.
;;
(create-instance 'MOTIF-ERROR-GADGET opal:aggregadget
  :declare ((:parameters :string :parent-window :font :justification
			 :modal-p :beep-p :window-left :window-top
			 :window-width :window-height :foreground-color
			 :selection-function)
	    (:type (string :string)
		   ((or (is-a-p inter:interactor-window) null) :parent-window)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((member :left :center :right) :justification)
		   (kr-boolean :modal-p :beep-p)
		   (integer :window-left :window-top)
		   ((integer 0) :window-width :window-height)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function)))
  (:width (o-formula (opal::q-max (gvl :text :width) (gvl :button :width))))
  (:height (o-formula (+ 20 (gvl :text :height) (gvl :button :height))))

  ;; If there is no parent window, then the error window is created at
  ;; position (200, 200).
  (:window-left (o-formula (if (gvl :parent-window)
			       (- (floor (gvl :parent-window :width) 2)
				  (floor (gvl :window-width) 2))
			       200)))
  (:window-top (o-formula (if (gvl :parent-window)
			      (- (floor (gvl :parent-window :height) 2)
				 (floor (gvl :window-height) 2))
			      200)))
  (:window-width (o-formula (+ 20 (gvl :width))))    ; 10 on each side
  (:window-height (o-formula (+ 40 (gvl :height)))) ; 20 on top, bottom
  (:window-title "Error")

  (:parent-window NIL) ;; The parent of the error-window
  (:string "Error")
  (:font (opal:get-standard-font :sans-serif :bold :medium))
  (:justification :center)
  (:modal-p T)
  (:selection-function NIL)
  (:foreground-color opal:motif-orange)
  (:button-name "OK")

  (:destroy #'Error-Gadget-Destroy)

  (:waiting NIL)     ; if T, then OK should call interaction-complete.
					; set by display-error
  (:beep-p T)
  (:parts
   `((:text ,opal:multi-text
	    (:left ,(o-formula
		     (+ 10 (- (floor (opal::q-max (gvl :width)
					    (gvl :parent :button :width)) 2)
			      (floor (gvl :width) 2)))))
	    (:top 20)
	    (:justification ,(o-formula (gvl :parent :justification)))
	    (:string ,(o-formula (gvl :parent :string)))
	    (:font ,(o-formula (gvl :parent :font))))

     (:button ,MOTIF-TEXT-BUTTON
	      (:left ,(o-formula
		       (+ 10 (- (floor (opal::q-max (gvl :width)
					      (gvl :parent :text :width)) 2)
				(floor (gvl :width) 2)))))
	      (:top ,(o-formula (+ 20 (opal:gv-bottom
				       (gvl :parent :text)))))
	      (:foreground-color ,(o-formula (gvl :parent :foreground-color)))
	      (:Constant (T :Except :left :top :foreground-color))
	      (:font ,(opal:get-standard-font :sans-serif :bold :medium))
	      (:string ,(o-formula (gvl :parent :button-name)))
	      (:final-feedback-p NIL)
	      (:selection-function
	       ,#'(lambda (button value)
		    (declare (ignore value))
		    (E-Q-Gadget-Sel-Func button T))))))) ; always use the
						          ; value T

(create-instance 'MOTIF-QUERY-GADGET MOTIF-ERROR-GADGET
  :declare ((:parameters :string :button-names :parent-window :font
			 :justification :modal-p :beep-p :window-left
			 :window-top :window-width :window-height
			 :foreground-color :selection-function)
	    (:type (string :string)
		   ((or (is-a-p inter:interactor-window) null) :parent-window)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((member :left :center :right) :justification)
		   (kr-boolean :modal-p :beep-p)
		   (integer :window-left :window-top)
		   (list :button-names)
		   ((integer 0) :window-width :window-height)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function)))
   (:window-title "Query")
   (:string "Is that OK?")
   (:button-names '("OK" "CANCEL"))
   (:parts
    `(:text ;; same as error gadget
      (:button ,MOTIF-TEXT-BUTTON-PANEL
	       (:left ,(o-formula
			(+ 10 (- (floor (opal::q-max (gvl :width)
					       (gvl :parent :text :width)) 2)
				 (floor (gvl :width) 2)))))
	       (:top ,(o-formula (+ 20 (opal:gv-bottom
					(gvl :parent :text)))))
               (:Constant (T :Except :left :top :width :height :items
			     :foreground-color))
               (:foreground-color ,(o-formula (gvl :parent :foreground-color)))
               (:font ,(opal:get-standard-font :sans-serif :bold :medium))
               (:direction :horizontal)
	       (:final-feedback-p NIL)
	       (:selection-function
		,#'(lambda (button value) (E-Q-Gadget-Sel-Func button value)))
               (:items ,(o-formula (gvl :parent :button-names)))))))


(define-method :initialize MOTIF-ERROR-GADGET (e-gadget)
  (call-prototype-method e-gadget) 
  (let ((window
	 (create-instance NIL inter:interactor-window
	    (:parent (g-value e-gadget :parent-window))
	    (:visible NIL)))
	(aggregate (create-instance NIL opal:aggregate)))
    (s-value window :aggregate aggregate)
    (s-value window :error-gadget e-gadget)
    (s-value window :background-color
	     (o-formula (gvl :error-gadget :foreground-color)))
    (opal:update window)
    ;; The :window slot of e-gadget is automatically set by add-component
;    (with-demon-enabled #'inter::inter-update-slot-invalidated
      (opal:add-component aggregate e-gadget)
;      )
    (s-value window :modal-p
	     (o-formula (gvl :error-gadget :modal-p)))))


;;; testing/demo function for error-gadgets and query gadgets
#+garnet-test
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Error-Gadget-Go Motif-Error-Gadget-Stop MOTIF-EGADGET
	    MOTIF-QGADGET))
  (proclaim '(special MOTIF-EG-WIN MOTIF-EG-AGG MOTIF-EG-FEED
		      MOTIF-EGADGET MOTIF-QGADGET)))

#+garnet-test
(defun Motif-Error-Gadget-Go ()
  (create-instance 'MOTIF-EG-WIN inter:interactor-window
    (:foreground-color opal:motif-gray)
    (:title "test error and query gadget"))
  (s-value MOTIF-EG-WIN :aggregate
	   (create-instance 'MOTIF-EG-AGG opal:aggregate))
  (create-instance 'MOTIF-EGADGET motif-error-gadget
    (:modal-p NIL)
    (:window-title "Error"))
  (create-instance 'MOTIF-QGADGET motif-query-gadget
    (:window-title "Query"))
  (opal:update-all)
  (opal:add-component MOTIF-EG-AGG (create-instance 'MOTIF-EG-FEED opal:text
				     (:left 10)
				     (:top 125)
				     (:string "")))
  (opal:add-component MOTIF-EG-AGG (create-instance NIL motif-text-button-panel
				     (:final-feedback-p NIL)
				     (:left 10)(:top 10)
				     (:items `( ("Error"
		  ,#'(lambda(inter val)
		       (declare (ignore inter val))
		       (display-error MOTIF-EGADGET "There was an error")
		       (s-value MOTIF-EG-FEED :string
				"You can still operate the buttons")))
					       ("Query and wait"
		  ,#'(lambda(inter val)
		       (declare (ignore inter val))
		       (s-value MOTIF-EG-FEED :string
				(format NIL "Value is ~a~%"
			         (display-query-and-wait MOTIF-QGADGET
				   "Select a value"
				   '("One" "Two" "Three"))))))))))
    (opal:update MOTIF-EG-WIN)
    (inter:main-event-loop))

#+garnet-test
(defun Motif-Error-Gadget-Stop ()
  (opal:destroy MOTIF-EG-WIN)
  (opal:destroy MOTIF-EGADGET)
  (opal:destroy MOTIF-QGADGET))
