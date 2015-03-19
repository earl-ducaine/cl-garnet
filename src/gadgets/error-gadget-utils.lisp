;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Error Gadget and Query Gadget Utility file
;;;   This file contains the functions used by both motif- and regular
;;;   error gadgets
#|
============================================================
Change log:
 12/06/94  Bruno Haible - Referenced package in system::*error-handler*
 05/16/94  Andrew Mickish - Added :window-title
 01/12/94  Andrew Mickish - xlib:drawable-plist ---> opal:drawable-to-window
 09/22/93  Bruno Haible - Added CLISP version of Careful-Eval
 01/25/93  Andrew Mickish - Call to notice-items-changed --> opal:update in
             internal-display-q-or-e
 12/12/92  Andrew Mickish - Added Careful-Eval-Formula-Lambda
 12/10/92  Andrew Mickish - *drawable-to-window-mapping* ---> *garnet-windows*
 10/20/92  Andrew Mickish - Added Careful-Eval, Careful-String-Eval,
             Careful-Read-From-String
 08/21/91  Brad Myers - fixed race condition in wait-interaction-complete
 08/21/91  Andrew Mickish - Made string parameter to display-* optional
 08/4/92   Brad Myers - inter:beep is optional, and make window visible before
                        calling sub-menu
 06/5/92   Brad Myers - used :modal-p windows instead of error-priority-level
 05/19/92  Brad Myers - Raise window in internal-display-q-or-e
 04/23/92  Ed Pervin - Added schema-p checks to e-q-gadget-sel-func
 04/8/92   Brad Myers - created from error-gadget
============================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Display-Error Display-Error-And-Wait
	    Display-Query Display-Query-And-Wait
	    Careful-Eval Careful-String-Eval Careful-Read-From-String
	    Careful-Eval-Formula-Lambda)))

(defun internal-display-q-or-e (gadget string wait labels)
  ;; Set the message to be displayed
  (s-value gadget :string string)
  ;; Set the label list
  (when (and labels
	     (not (equal labels (g-value gadget :button-names))))
    (s-value gadget :button-names labels)
    ;; Invoke the :fix-update-slots method of the aggrelist to adjust
    ;; the button-panel's components according to the new items
    (opal:update (g-value gadget :window))
    )
  ;; Set up window
  (let ((window (g-value gadget :window)))
    ;; Set the position and dimensions of the window
    (s-value window :left (g-value gadget :window-left))
    (s-value window :top (g-value gadget :window-top))
    (s-value window :width (g-value gadget :window-width))
    (s-value window :height (g-value gadget :window-height))
    (s-value window :title (g-value gadget :window-title))
    (s-value gadget :waiting wait)
    (s-value window :visible T)
    ;; beep if desired
    (when (g-value gadget :beep-p) (inter:beep))
    ;; wait if desired, updating window
    (if wait
	(inter:wait-interaction-complete window) ;; will raise the window
	;; else raise window here
	(opal:raise-window window))))   ;; calls update


(defun DISPLAY-ERROR (error-gadget &optional
		      (string (g-value error-gadget :string)))
  (internal-display-q-or-e error-gadget string NIL NIL))

(defun DISPLAY-ERROR-AND-WAIT (error-gadget &optional
			       (string (g-value error-gadget :string)))
  (internal-display-q-or-e error-gadget string T NIL))

(defun DISPLAY-QUERY (query-gadget &optional
		      (string (g-value query-gadget :string))
		      label-list)
  (internal-display-q-or-e query-gadget string NIL label-list))

(defun DISPLAY-QUERY-AND-WAIT (query-gadget &optional
			       (string (g-value query-gadget :string))
			       label-list)
  (internal-display-q-or-e query-gadget string T label-list))

(defun E-Q-Gadget-Sel-Func (button value)
  (let* ((window (g-value button :window))
	 (error-gadget (g-value button :parent))
	 (waiting (g-value error-gadget :waiting)))
    ;; make this query window go away first, in case selection
    ;; function brings up another window.
    (when (schema-p window)
      (s-value window :visible NIL)
      (opal:update window))
    (kr-send error-gadget :selection-function
	     error-gadget value)
    ;; use stored value in case selection-function destroys the
    ;; error-gadget, still need to get out of the waiting loop.
    (when waiting
      (inter:interaction-complete value))))

(defun Error-Gadget-Destroy (error-gadget &optional erase)
  ;; first, remove the gadget from its window so when the window is
  ;; destroyed, the gadget will not be.  Then destroy the gadget itself
  ;; using call-prototype-method
  (let ((agg (g-value error-gadget :parent))
	(window (g-value error-gadget :window)))
    (if agg
	(opal:remove-component agg error-gadget))
    ;; make sure window isn't already being destroyed
    (when (and window
	       (schema-p window)
	       (opal:drawable-to-window (get-local-value window :drawable)))
      (opal:destroy window)))
  (call-prototype-method error-gadget erase))


;; This function takes an expression to evaluate.  If there is an error
;; detetected during the eval, then the error gadget is raised with the
;; error message.  The function returns two values: the result of the eval 
;; and a flag indicating whether there was an error.
;;
;;  Example:  (gg:Careful-Eval `(read-from-string ,string))

(defmacro Careful-Eval (form &optional an-error-gadget error-message)
  #-clisp
  `(#+allegro-V3.1 excl::handler-case
    #+lucid lcl::handler-case
    #-(or allegro-v3.1 lucid) handler-case
    (eval ,form)
    (error (condition)
     (when ,an-error-gadget
       (let ((str (if ,error-message
		      (format NIL "~A~%~%~A" condition ,error-message)
		      (format NIL "~A" condition))))
	 (display-error ,an-error-gadget str)))
     (values NIL condition)))
  #+clisp
  `(let ((system::*error-handler*
	  #'(lambda (continue errorstring &rest args)
	      (if continue
                  (let ((system::*error-handler* nil))
                    (apply #'cerror continue errorstring args))
                  (let ((condition errorstring))
                    (when ,an-error-gadget
                      (let ((str (if ,error-message
				     (format NIL "~A~%~%~A" condition ,error-message)
				     (format NIL "~A" condition))))
			(display-error ,an-error-gadget str)))
                    (values NIL condition))))
	  ))
    (eval ,form))
  )

;; This function will take a string like "opal:black-fill", read the symbol
;; (or expression) from the string, and then evaluate the symbol (or expr) to
;; compute its value.  If the string cannot be read, it is returned.
;;
(defun Careful-String-Eval (string
			    &optional an-error-gadget error-message)
  ;; It is an error to pass anything but a string
  (if (and (stringp string) (not (string= "" string)))
      ;; Set val to be what the user typed in
      (multiple-value-bind (val errorp)
	  (gg:Careful-Eval `(read-from-string ,string)
			   an-error-gadget error-message)
	;; Errorp could be an error condition or a number.  If it is an error
	;; condition, then there was an error while reading from the string.
	;; If it is a number, then it is the second value returned by
	;; read-from-string, which is the number of characters in the symbol
	;; (or expression) that was read from the string.  In the latter case,
	;; we are in the unfortunate situation of errorp being non-NIL even
	;; though there was no error.  Compensate with the following operation:
	(if (numberp errorp) (setq errorp NIL))
	
	(if errorp
	    ;; If couldn't even read the string, just return it with error.
	    (values string errorp)
	    ;; Now eval what the user typed in
	    (gg:Careful-Eval val an-error-gadget error-message)))
      ;; Return the bogus argument and signal an error
      (values string T)))

(defun Careful-Read-From-String (string
				 &optional an-error-gadget error-message)
  (if (and (stringp string) (not (string= "" string)))
      (multiple-value-bind (val errorp)
	  (gg:Careful-Eval `(read-from-string ,string)
			   an-error-gadget error-message)
	(if (numberp errorp) (setq errorp NIL))
	(values val errorp))
      ;; Argument was not a string, so return it with an error
      (values string T)))


(defun Careful-Eval-Formula-Lambda (expr an-error-gadget error-message
					 the-obj the-slot the-formula
					 warn-p)
  (let ((kr::*schema-self* the-obj)
	(kr::*schema-slot* the-slot)
	(kr::*current-formula* the-formula)
	(kr::*warning-on-null-link* warn-p))
    (catch 'kr::no-link
      (gg:Careful-Eval expr an-error-gadget error-message)
      )))



;;  Tell the world that error-gadget-utils has been loaded
;;
(setf (get :garnet-modules :error-gadget-utils) T)

