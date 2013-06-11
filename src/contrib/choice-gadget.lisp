;;; -*- mode: Lisp; syntax: common-lisp; package: GARNET-GADGETS; base: 10 -*-
;;****************************************************************************
;;
;;   Copyright 1991, frank ritter.
;;   The material in this file is made available according to the
;;   terms of the GNU LGPL, modified by the Lisp LGPL preamble.  Both
;;   of those documents are available in the doc subdirectory of the
;;   Garnet distribution.
;;
;;****************************************************************************


;;; HISTORY
;;  21Jun04 - per agreement with Frank Ritter, the license terms of
;;  this file are clarified to be LLGPL. [2004/06/21:rpg]
;;  24Jan95 - added modifications to run in Garnet 2.2
;;  (RLO)
;;  02/26/91  created fer
;;
;;; $Id$
;;****************************************************************************



;;; file            : choice-gadget.lisp
;;  author          : frank ritter
;;  created on      : fri feb 22 13:41:50 1991
;;  last modified by: Roberto L. Ong
;;  last modified on: Tue Jan 24 11:55:04 1995
;;  update count    : 78
;;   
;;; purpose
;;     provides a gadget for presenting a multiple valued choice to the user.
;;
;;; table of contents
;;
;;     i.	disclaimer and programmer notes
;;     ii.	package initialization and such
;;
;;     I.	display-choice function
;;     IB.	graphic-yes-or-no-p
;;     ii.	new change-priority level
;;



;;; i.		disclaimer and programmer notes
;;____________________________________________________________________
;;  the garnet user interface development environment
;;  copyright (c) 1990, carnegie mellon university
;;  all rights reserved.  the cmu software license agreement specifies
;;  the terms and conditions for use and redistribution.
;; 
;;____________________________________________________________________
;; 
;;   choice gadget, copyright 1991, frank ritter
;; 
;;; features: 
;;    the choice-gadget can be used in applications where
;;    the designer wants to allow (or force) the user to make a
;;    choice.  when the display-choice function (described below) is
;;    called, the choice window becomes visible (at the appropriate
;;    size) and displays the choices.  if modal-p the user must then
;;    click on a choice button before proceeding graphically.  if
;;    really-modal-p, then lisp hangs also.  the slot names and
;;    behavior are similar to the error-gadget and the labeled box gadget.
;; 
;;; customizable slots: 
;;   1) parent-window - the window that the choice window should be centered
;;       inside of
;;   2) font - the font for the prompt, and help string
;;       items-font - the font for the items
;;   3) justification - how to justify the multi-line message
;;   4) modal-p - whether to shut down all other interactors until a
;;       button has been pressed
;;  4b) really-modal-p - whether to hang all lisp processing until a
;;       button has been pressed.
;;   5) window-left, window-top -- where the window will appear.  the default
;;       is centered in the parent window, or at 100, 200.  you can set these.
;;       window-width, window-height - dimensions of the choice window
;;       (automagically set, you should  not set these slots)
;;   6) window - the window created by the error-gadget (do not set this slot)
;;   7) sleep-time - how often to check (really-modal-p)
;;       to see if the user has done anything.
;;   8) choices -- the default choices are "yes" and "no".  these can be set
;;       by hand, or with display-choice
;; 
;;; programmer's interface:
;;    in order to associate an choice window with an application, an instance
;;    of the choice-gadget should be created with the :parent-window slot
;;    set to the window of the application.  to activate the choice
;;    window, call the function display-choice, which takes the instance of
;;    the choice-gadget, the choices and the desired message as parameters.
;;    this is very similar to the error gadget.
;; 
;;; caveats:
;;   1) update the parent window before instantiating the choice-gadget.
;;   2) the instance of the choice-gadget should not be added to an aggregate.
;;      bookkeeping for the parent window is automatically taken care of
;;      during the create-instance call.
;; 
;;


;;; ii.		package initialization and such
;; 

(in-package "GARNET-GADGETS")


;; :use '("COMMON-LISP" "KR"))

(export '(choice-gadget display-choice graphic-yes-or-no-p)
   	(find-package "GARNET-GADGETS"))


;;; i.		display-choice function
;;

(defun display-choice (&optional choice-gadget prompt choices)
  "Display the choice-gadget using choices and prompt."

  ;; activate modal feature if appropriate
  (if (g-value choice-gadget :modal-p)
      (s-value choice-priority-level :stop-when :always)
      (s-value choice-priority-level :stop-when :if-any))

  ;; set the prompt and choices to be displayed
  (if prompt (s-value choice-gadget :string prompt))
  (if choices (s-value choice-gadget :choices choices))

  ;; this may work, or else remove-component all elem, & add-component them
  (opal:notice-items-changed (g-value choice-gadget
				      :buttons :text-button-list))

  ;; turn visibility on
  (let ((window (g-value choice-gadget :window)))
    (s-value window :left (g-value window :left))
    (s-value window :top (g-value window :top))     ; won't size correctly
    (s-value window :width (g-value window :width))   ;  without these lines
    (s-value window :height (g-value window :height))
    (opal:update window)  ; this resizes before drawing
    (s-value window :visible t)
    (opal:update window)
    )
  (inter:beep)

  ;; make sure it gets clicked if really-modal-p
  (if (g-value choice-gadget :really-modal-p)
      (prog ((sleep-time (g-value choice-gadget :sleep-time))
	     (display (let ((win1 (car opal::*garnet-windows*)))
			;; original code does not seem to work anymore
			;; on version 1.4
			;; -RLO (17Jan95)
			;; (caar (opal::get-table-contents))))
	                (if win1
			    opal::*default-x-display*
			    (xlib:window-display win1)))) )
	 ;; original code that does not seem to work
	 ;; on version 1.4
	 ;; -RLO (17Jan95)
	 ;; (xlib:window-display win1)
	 ;; opal::*default-x-display*))) )

       start
       ;; call the event handler to get anything
       ;; this only works on Garnet 1.4 (not on 2.2)
       ;; - RLO (18Jan95)
       ;; (opal::default-event-handler display :timeout 0)
       (sleep sleep-time)
       (if (g-value choice-gadget :window :visible)
	   (go start))
       (return (g-value choice-gadget :value))))
  (g-value choice-gadget :buttons :value)  )


;;; IB.		graphic-yes-or-no-p
;;

(defun graphic-yes-or-no-p (gadget prompt &optional (choices '("Yes" "No")))
  (if (string= (first choices)
	       (display-choice gadget prompt choices))
      t
      nil))



;;; II.		New change-priority level
;;

;;    This function creates a new priority level and adds it to the front
;; of the interactors priority level list.  Thus, this level has higher
;; priority than the default inter-levels.
;;    This priority level are needed in case the choice-gadget is modal.
;; The idea is to set the CHOICE-PRIORITY-LEVEL's :stop-when to :always
;; when the choice-gadget is modal, so that the choice buttons are the
;; only gadgets that will work in the entire interface while the
;; choice-window is visible.  So, the effect is to shut down the rest of
;; the interface until the user clicks on a button (if the choice-gadget
;; is modal).  Other things can happen in lisp unless the really-modal-p
;; is chosen.

(defun ADD-CHOICE-PRIORITY-LEVEL ()
  (unless (and (boundp 'CHOICE-PRIORITY-LEVEL)
	       (member CHOICE-PRIORITY-LEVEL inter:priority-level-list))
    (push (create-instance 'CHOICE-PRIORITY-LEVEL inter:priority-level)
	  inter:priority-level-list)))

(add-choice-priority-level)


;;; III.	Choice-gadget
;;

;; NOTE:  If :parent-window is specified, then the parent window must already
;; have been opal:update'd when the instance of CHOICE-GADGET is created.
;;

(create-instance 'CHOICE-GADGET opal:aggregadget
   (:width (o-formula (MAX (gvl :text :width)
			   (gvl :help-text :width) (gvl :buttons :width))))
   (:height (o-formula (+ 30 (gvl :text :height) (gvl :help-text :height)
			  (gvl :buttons :height))))

   ;; If there is no parent window, then the choice window is created at
   ;; position (200, 200).
   (:window-left (o-formula (if (gvl :parent-window)
				(- (floor (gvl :parent-window :width) 2)
				   (floor (gvl :window-width) 2))
				100)))
   (:window-top (o-formula (if (gvl :parent-window)
			       (- (floor (gvl :parent-window :height) 2)
				  (floor (gvl :window-height) 2))
			       100)))
   (:window-width (o-formula (+ 20 (gvl :width))))    ; 10 on each side
   (:window-height (o-formula (+ 40 (gvl :height))))  ; 20 on top, bottom
   (:window-title "Choice message")
   (:modal-p T)
   (:string "Choice?")
   (:help-string "(click on choice to proceed)")
   (:really-modal-p nil) ; do you force user to click before doing *anything*
   (:sleep-time 0.1) ;for really-modal-p, time in s between checking if done
   (:parent-window NIL)    ;; The parent of the choice-window
   (:choices '("Yes" "No"))
   (:font opal:default-font)
   (:items-font opal:default-font)   
   (:justification :center)
   (:destroy 'Choice-Gadget-Destroy)
   (:parts `(
      (:text ,opal:multi-text
         (:left ,(o-formula 10))
         (:top 20)
         (:justification ,(o-formula (gvl :parent :justification)))
	 (:string ,(o-formula (gvl :parent :string)))
	 (:font ,(o-formula (gvl :parent :font))))
      (:help-text ,opal:multi-text
         (:left ,(o-formula 10))
         (:top ,(o-formula (+ 20 (opal:gv-bottom (gvl :parent :text)))))       
         (:justification ,(o-formula (gvl :parent :justification)))
	 (:string ,(o-formula (gvl :parent :help-string)))
	 (:font ,(o-formula (gvl :parent :font)))) 
      (:buttons ,text-button-panel
        (:left ,(o-formula 30))
        (:top ,(o-formula (+ 20 (opal:gv-bottom (gvl :parent :help-text)))))
        (:items ,(o-formula (gvl :parent :choices)))
        (:font ,(o-formula (gvl :parent :items-font)))
        (:h-align :center)
        (:text-on-left-p nil) 
        (:shadow-offset 5) (:text-offset 5) (:gray-width 3)
        (:final-feedback-p NIL)
	(:selection-function
		,#'(lambda (gadget value)
		     (declare (ignore value))
		     (let ((window (g-value gadget :window)))
		       (s-value window :visible NIL)
        	       ;(opal:update window)
		       (s-value CHOICE-PRIORITY-LEVEL :stop-when NIL))))
        (:interactors (
           (:text-BUTTON-PRESS :modify
	   (:waiting-priority ,CHOICE-PRIORITY-LEVEL)
           (:active T)
	   (:running-priority ,CHOICE-PRIORITY-LEVEL))))   )))
)


;;; IV.		initialize & destroy methods for Choice-Gadget
;;

(define-method :initialize CHOICE-GADGET (choice-gadget)
  (call-prototype-method choice-gadget)
  (let ((window
	 (create-instance NIL inter:interactor-window
	    (:left (o-formula (gvl :aggregate :components :window-left)))
	    (:top (o-formula (gvl :aggregate :components :window-top)))
	    (:title (o-formula (gvl :aggregate :components :window-title)))
	    (:width (o-formula (gvl :aggregate :components :window-width)))
	    (:height (o-formula (gvl :aggregate :components :window-height)))
	    (:parent (g-value choice-gadget :parent-window))
	    (:visible NIL)))
	(aggregate (create-instance NIL opal:aggregate)))
    (s-value window :aggregate aggregate)
    ;; The :window slot of choice-gadget is automatically set by add-component
    (opal:add-component aggregate choice-gadget)
    (opal:update window)))

(defun Choice-Gadget-Destroy (choice-gadget &optional erase)
  ;; first, remove the gadget from its window so when the window is
  ;; destroyed, the gadget will not be.  
  ;; Then destroy the gadget itself
  ;; using call-prototype-method
  (let ((agg (g-value choice-gadget :parent))
	(window (g-value choice-gadget :window)))
    (when agg
      (opal:remove-component agg choice-gadget))
    ;; make sure window isn't already being destroyed
    (when (and window
	       (schema-p window)
	       (gethash (get-local-value window :drawable)
			opal::*drawable-to-window-mapping*))
      (opal:destroy window))
    (call-prototype-method choice-gadget erase)))


;;; V.		Demo code
;;

#+garnet-test
(defparameter test-choice NIL)

#+garnet-test
(defun choice-gadget-Go ()
  (create-instance 'test-choice gg:choice-gadget
     (:window-title "Test question")
     (:really-modal-p t)
     (:top 5) (:left 650))

 (format t "Asked the user and got ~a back.~%" (display-choice test-choice))
  
 ;; Test choice has two choices, then 3 choices, and Hobson's choice is
 ;; no choice at all, which should be one alternative.

 (gg:display-choice test-choice "Future Big 10 Teams:"
		    '("Chatham" "MIT" "Pitt"))

 (gg:display-choice test-choice "Hobsen's choices:"
		    '("Pick me!"))
 (gg:graphic-yes-or-no-p "Do we really have a choice in the end?")
 )

#+garnet-test
(defun choice-gadget-Stop ()
  (opal:destroy text-Buttons-win))

#+garnet-test
(export '(choice-gadget-go choice-gadget-Stop)
	(find-package "GARNET-GADGETS"))
