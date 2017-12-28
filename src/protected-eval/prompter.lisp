;;; -*- Mode: COMMON-LISP; Package: GARNET-GADGETS; Base: 10 -*-     ;;
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written by Russell Almond at Statistical Sciences  ;;
;;  as an independent contribution to the Garnet project at          ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;                                                                   ;;
;;  The authors of the code make no warentee expressed or implied    ;;
;;  about its utility, rather we hope that someone may find a use    ;;
;;  for it.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id$
;;


;;;  Prompter Gadget
;;
;;   Features:
;;    This gadget is like the error or query gadget except that it
;;    prompts for an arbitrary lisp expression to read.  An optional
;;    "eval" allows the user to "eval" the expression before reading
;;    it.  The "*" and "+" symbols have their usual meaning.
;;
;;   The number of buttons supplied by default is dependent on the
;;   value of the :modal-p slot.  If the gadget is modal, an "OK" and
;;   a "Cancel" button are supplied.  If the gadget is not modal, an
;;   "OK", a "Apply" and a "Cancel" button are supplied.  The "OK" and
;;   "Apply" methods apply the selection function, with two args, the
;;   error gadget and the value of the prompt (after reading).
;;
;;   Prompter Gadget
;;   Customizable slots:
;;    1) :Parent-window - The window that the error window should be centered
;;                       inside of
;;    2) :Font - The font for the message
;;    3) :Input-Font - The font for the input (fixed width)
;;    4) :Justification - How to justify the multi-line message
;;    5) :Modal-p - Whether to shut down all other interactors until the
;;                 "OK" button has been pressed
;;    6) :button-names - the labels of the buttons.
;;                       Default: '("OK" "CANCEL") if modal
;;                       Default: '("OK" "APPLY" "CANCEL") if not modal
;;    7) :eval? -- should eval button be included
;;    8) :read-bindings --  List of variables with bindings (like a
;;                      let statement).  These bindings are put in
;;                      force when the input string is read.
;;    9) :read-package --- The package in which to inter newly read
;;    symbols (defaults to (find-package :user)
;;    10) :field-width --- width of the prompt field
;;
;;    The following slots should be considered :read-only, but may be
;;    of some interest.
;;    1) :Window-left, window-top, window-width, window-height - dimensions of
;;         the prompter window (do not set these slots)
;;    2) :Window - Window created by the prompter-gadget (do not set this slot)
;;    3) :* --- The last expression returned from eval
;;    4) :+ --- The last expression fed to eval
;;    5) :- --- The expression currently read.
;;    6) :modified? --- Was the value modified since the last time it
;;    was set.
;;
;;   Prompter Gadget Programmer's interface:
;;    In order to associate an error window with an application, an instance
;;    of the prompter-gadget should be created with the :parent-window slot
;;    set to the window of the application.  To activate the error
;;    window, call the function DISPLAY-PROMPT or
;;    DISPLAY-PROMPT-AND-WAIT, which takes the instance of
;;    the prompter-gadget and the desired message as parameters.
;;    This is similar to the error and query gadgets.
;;
;;   DISPLAY-PROMPT-AND-WAIT returns two values.  The first is the
;;   value which was entered by the user in the prompter field and the
;;   second is the button pressed to return the value, which by
;;   default will be one of :ok or :cancel, under certain error
;;   condtions it could also return :abort.  In both cases the
;;   selection function lambda list is (prompter user-value
;;   button-value).
;;
;;   Caveats:
;;    1) Update the parent window before instantiating the
;;       prompter-gadget
;;    2) The instance of the prompter-gadget should not
;;       be added to an aggregate. Bookkeeping for the parent window
;;       is automatically taken care of during the create-instance call.

;;  Known bugs:  Sometimes a recursive error will cause "WARNING:
;;  Interaction-Complete called but not inside
;;  Wait-Interaction-Complete" And fail to pass back the proper abort
;;  message.  I don't know what causes this problem.

(in-package :garnet-gadgets)

(export '(PROMPTER-GADGET DISPLAY-PROMPT DISPLAY-PROMPT-AND-WAIT))

;;;      (proclaim '(special ERROR-INPUT-PRIORITY-LEVEL))
;;;      (user::garnet-load "gadgets:error-gadget-loader")
;;;      (user::garnet-load "contrib:scrolling-unlabeled-box-loader")
;;;      (user::garnet-load "contrib:protected-eval-loader")

(defun prompter-accept-input (string-gadget value)
  (let ((prompter (g-value string-gadget :parent)))
    (s-value prompter :value value)
    (s-value prompter :modified? t)))


(defun prompter-gadget-eval-func (button but-value)
  (declare (ignore but-value))
  (let* ((prompter (g-value button :parent))
	 (val (g-value prompter :value))
	 (+ (g-value prompter :+))
	 (* (g-value prompter :*))
	 - new-val)
    (setq -
	  (restart-case
	      (garnet-protected-read-from-string
	       val :read-package (g-value prompter :read-package)
	       :read-bindings (g-value prompter :default-bindings)
	       :default-value (g-value prompter :default-value))
	    (abort () :report "Ignore Read/Eval request."
		   (return-from prompter-gadget-eval-func))))
    (setq new-val
	  (restart-case
	      (garnet-protected-eval -)
	    (abort () :report "Ignore Eval request."
		   (return-from prompter-gadget-eval-func))))
    (s-value prompter :+ -)
    (s-value prompter :* new-val)
    (s-value prompter :value (format nil "~S" new-val))
    (s-value prompter :modified? nil)
    (opal:update (g-value prompter :window))))


(defun Prompter-Gadget-Sel-Func (button but-value)
  (let* ((window (g-value button :window))
	 (prompter (g-value button :parent))
	 (waiting (g-value prompter :waiting))
	 (value (if (g-value prompter :modified?)
		    (restart-case
			(garnet-protected-read-from-string
			 (g-value prompter :value)
			 :read-package (g-value prompter :read-package)
			 :read-bindings (g-value prompter :read-bindings)
			 :default-value (g-value prompter :default-value))
		      (abort () :report "Abort Input"
			(setq but-value :abort)))
		  (g-value prompter :*))))
    (kr-send prompter :selection-function prompter value but-value)
    (unless (eql but-value :apply)
      (when (schema-p window)		; May have been destroyed by
					; :selection-function!
	(s-value window :visible NIL)
	(opal:update window)))
    (when waiting (inter:interaction-complete (list value but-value)))))

(or (kr:def-kr-type Package () 'Package)
    t)

;; note: if :parent-window is specified, then the parent window must
;; already have been opal:update'd when the instance of ERROR-GADGET
;; is created.
(when gem::*x11-server-available*
  (kr:create-instance 'prompter-gadget GG:Motif-Query-Gadget
    :declare
    ((:parameters :string :field-width :input-font :value
		  :default-value :eval? :read-bindings
		  :read-package)
     (:output :value :+ :*)
     (:type (String :string :value)
	    ((Integer 1) :field-width)
	    (Font :input-font)
	    (kr-boolean :waiting :eval?)
	    (List :read-bindings)
	    (Package :read-package)
	    )
     (:maybe-constant T :string :field-width :input-font :value
		      :read-bindings :read-package))
    (:documentation
     "This gagdet solicits a lisp expression to be read from the user.
    Setting the value of the :eval? to T offers the user the chance to
    evaluate the expression and return the value.")
    (:slot-doc :string "Prompting expression."
	       :field-width "Width of input field."
	       :input-font "Font for input field, should be fixed width."
	       :value "The string value of the prompted field, also the
                      initial value of that field."
	       :default-value "The value to be returned in the case of
                  an error or abort."
	       :waiting "If T, then :OK should call
                      inter:wait-interaction-complete."
	       :eval?  "If T, user is presented with an eval button
                     allowing the input field to be evaluated."
	       :read-bindings "List of bindings in effect when read
                         takes place."
	       :read-package "Package in which read should take place."
	       :* "Value of last evaluated expression.  (Usable inside
                  prompted evaluations as *)"
	       :+ "Value of last read expression.  (Usable inside
                  prompted evaluations as +; - is bound to currently
		  read expression but not exported."
	       )
    (:width (o-formula (MAX (gvl :text :width)
			    (gvl :prompt :width)
			    (gvl :button :width))))
    (:height (o-formula (+ 40 (gvl :text :height)
			   (gvl :prompt :height)
			   (gvl :eval-but :height)
			   (gvl :button :height))))
    ;; If there is no parent window, then the error window is created at
    ;; position (200, 200).
    (:parent-window NIL)			; The parent of the error-window
    (:string "Enter Expression:")
    (:font opal:default-font)
    (:field-width 130)
    (:input-font opal:default-font)
    (:justification :center)
    (:modal-p T)
    (:selection-function NIL)
    (:value "Nil")
    (:default-value nil)
    (:destroy #'Error-Gadget-Destroy)
    (:waiting NIL)			; if T, then OK should call interaction-complete.
					; set by display-error
    (:button-names (o-formula (if (gvl :modal-p)
				  '(:Ok :Cancel)
				  '(:OK :APPLY :CANCEL))))
    (:eval? t)					    ; if T, then "EVAL" button is included.
    (:read-bindings nil)				    ; List of variables with
    (:read-package (find-package :common-lisp-user)) ; bindings (like a let
					; statement).  These bindings
					; are put in force when the
					; input string is read.
    (:+ nil) (:* nil)
    (:parts
     `(:text
       (:prompt ,motif-scrolling-unlabeled-box
		(:left ,(o-formula (gvl :parent :text :left)))
		(:top ,(o-formula (+ 10 (opal:gv-bottom
					 (gvl :parent :text)))))
		(:width ,(o-formula (max 130
					 (gvl :parent :text :width)
					 (gvl :parent :field-width))))
		(:value ,(o-formula (gvl :parent :value)))
		(:selection-function ,#'prompter-accept-input)
		)
       (:eval-but ,MOTIF-TEXT-BUTTON
		  (:left ,(o-formula
			   (+ 10 (- (floor (MAX (gvl :width)
						(gvl :parent :text :width)) 2)
				    (floor (gvl :width) 2)))))
		  (:top ,(o-formula (+ 10 (opal:gv-bottom
					   (gvl :parent :prompt)))))
		  (:Constant (T :Except :left :top :visible))
		  (:visible ,(o-formula (gvl :parent :eval?)))
		  (:string "Eval")
		  (:shadow-offset 5) (:text-offset 5) (:gray-width 3)
		  (:final-feedback-p NIL)
		  (:selection-function
		   (lambda (button value)
		     (declare (ignore value))
		     (Prompter-Gadget-Eval-Func button T))) ; always use the value T
		  (:parts
		   (;; :shadow :gray-outline :white-field
		    :text
		    (:feedback-obj :omit)))
;;;	       (:interactors
;;;		((:TEXT-BUTTON-PRESS :modify
;;;		     (:waiting-priority ,ERROR-PRIORITY-LEVEL)
;;;		     (:active ,(o-formula (gvl :operates-on :visible)))
;;;		     (:running-priority ,ERROR-PRIORITY-LEVEL))))
		  )
       (:button ,MOTIF-TEXT-BUTTON-PANEL
		(:left ,(o-formula
			 (+ 10 (- (floor (MAX (gvl :width)
					      (gvl :parent :text :width)) 2)
				  (floor (gvl :width) 2)))))
		(:top ,(o-formula (+ 10 (opal:gv-bottom
					 (gvl :parent :eval-but)))))
		(:Constant (T :Except :left :top :width :height :items))
		(:shadow-offset 5) (:text-offset 5)(:direction :horizontal)
		(:gray-width 3)
		(:final-feedback-p NIL)
		(:selection-function ,#'prompter-gadget-sel-func)
		(:items ,(o-formula (gvl :parent :button-names)))))))
  (kr:s-value (kr:g-value Prompter-Gadget :eval-but) :selection-function
	      (lambda (button value)
		(Prompter-Gadget-Eval-Func button value)))

  (kr:s-value (kr:g-value Prompter-Gadget :button) :selection-function
	      (lambda (button value)
		(Prompter-Gadget-Sel-Func button value))))


(defun display-prompt (prompter string
		      &key (label-list '(:ok :cancel))
			   (eval? t)
			   (default-value nil)
			   (read-package (find-package :common-lisp-user))
			   (read-bindings nil))
  "Displays a prompter gadget in the modless form. Assumes that there
is a selection-function which takes three values: prompter-gadget,
value, and button-val. Probably should not use selection functions in
the items with this one, as it won't apply read to the value."
  (s-value prompter :eval? eval?)
  (s-value prompter :default-value default-value)
  (s-value prompter :* default-value)
  (s-value prompter :value (format nil "~S" default-value))
  (s-value prompter :read-package read-package)
  (s-value prompter :read-bindings read-bindings)
  (s-value prompter :modal-p nil)
  (internal-display-q-or-e prompter string NIL label-list))

(defun display-prompt-and-wait (prompter string
				&key (label-list '(:ok :cancel))
				     (eval? t)
				     (default-value nil)
				     (read-package *package*)
				     (read-bindings nil))
  "Displays a prompter and then waits for the user response. The
prompter will catch user errors. It returns two values, the first is
the actual value, the second is the button pressed to terminate the
request. It could also be :abort if the gadget was terminated
abnormally."
  (s-value prompter :eval? eval?)
  (s-value prompter :default-value default-value)
  (s-value prompter :* default-value)
  (s-value prompter :value (format nil "~S" default-value))
  (s-value prompter :read-package read-package)
  (s-value prompter :read-bindings read-bindings)
  (s-value prompter :modal-p t)
  (values-list (internal-display-q-or-e prompter string T label-list)))



;;; testing/demo function for error-gadgets and query gadgets
(export '(prompter-gadget-go prompter-gadget-stop))


(defparameter prompt-text
    "Enter a lisp expression in the box below.
Use emacs-like commands to edit text.
Press 'eval' to evaluate your expression.
* is last returned value, + is last read value.
Value returned is either last return value or
latest value read from input.")

(defun prompter-gadget-go ()
  (let (agg egadget pgadget feed)
    (create-instance 'prompter-gadget-test-win inter:interactor-window
		     (:title "test prompter gadget"))
    (s-value prompter-gadget-test-win :aggregate
	     (setq agg (create-instance NIL opal:aggregate)))
    (setf pgadget (create-instance nil prompter-gadget))
    (opal:update-all)
    (opal:add-component agg
			(setq feed (create-instance nil opal:text
				     (:string "")
				     (:left 10)
				     (:top 125))))
    (opal:add-component agg
	(create-instance nil motif-text-button-panel
	  (:final-feedback-p NIL)
	  (:left 10)(:top 10)
	  (:items
	   `(("Prompt and wait"
	      ,(lambda (inter val)
		(declare (ignore inter val))
		(multiple-value-bind (value sig)
		    (display-prompt-and-wait
		     pgadget prompt-text)
		  (s-value feed :string
			   (case sig
			     (:ok
			      (format NIL
				      "Value is ~S~%"
				      value))
			     (:cancel "Cancel!")
			     (:abort "Abort!"))))))
	     ("Clear Display"
	      ,(lambda (inter val)
		(declare (ignore inter val))
		(s-value feed :string ""))))
	   )))
    (opal:update prompter-gadget-test-win)
    (inter:main-event-loop)))

(defun prompter-gadget-stop ()
  (opal:destroy prompter-gadget-test-win))
