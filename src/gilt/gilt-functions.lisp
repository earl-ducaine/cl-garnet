;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;; Gilt is a simple interface builder for Garnet.  This file is the functions
;;; needed by applications to pop-up dialog boxes
;;; 
;;; Designed and implemented by Osamu Hashimoto


(in-package "GILT")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(SHOW-IN-WINDOW SHOW-IN-WINDOW-AND-WAIT
	    DESTROY-GADGET-WINDOW GADGET-VALUES SET-INITIAL-VALUE
	    OKCANCEL-FUNCTION VALUE-OF
	    VALUE-FROM-ONE-OBJ SET-VALUE-OF-APPROPRIATE-OBJS)))

(defun internal-show-in-window (top-gadget x y wait-p modal-p)
  (let ((win (g-local-value top-gadget :window))
	(window-width (g-value top-gadget :window-width))
	(window-height (g-value top-gadget :window-height))
	agg)
    (if win
	(progn
	  (when x (s-value win :left x))
	  (when y (s-value win :top y))
	  (if window-width (s-value win :width window-width))
	  (if window-height (s-value win :height window-height))
	  (s-value win :modal-p modal-p)
	  (s-value win :visible T))
        (progn 
          (setq win
            (create-instance NIL inter:interactor-window
              (:left   (or x (g-value top-gadget :window-left)))
              (:top    (or y (g-value top-gadget :window-top)))
              (:width  window-width)
              (:height window-height)
              (:title  (g-value top-gadget :window-title))
	      (:background-color (g-value top-gadget
					  :window-background-color))
	      (:modal-p modal-p)))
	  (setq agg (create-instance NIL opal:aggregate))
	  (s-value win :aggregate agg)
          (opal:add-component agg top-gadget)))
    (if wait-p
	(Inter:Wait-Interaction-Complete win) ; return the value of wait-i-c
	(progn (opal:update win)
	       win)))) ; return win

(defun show-in-window (top-gadget &optional x y modal-p)
"This pops up a window to show the specified gadget.  If x and y
supplied, they are used, otherwise the gadget's :window-left and :window-top
are used.  Modal-p determines whether the window is modal or not."
  (internal-show-in-window top-gadget x y NIL modal-p))

(defun show-in-window-and-wait (top-gadget &optional x y modal-p)
"This pops up a window to show the specified gadget and waits for the
OK to be hit.  If x and y supplied, they are used, otherwise the
gadget's :window-left and :window-top are used.  Modal-p determines
whether the window is modal or not."
  (internal-show-in-window top-gadget x y T modal-p))

(defun destroy-gadget-window (top-gadget)
"This destroys the window that goes with the gadget, without destroying the
gadget itself"
  (let ((win (g-local-value top-gadget :window)))
    (when win
      (opal:remove-component (g-local-value win :aggregate) top-gadget)
      (opal:destroy win))))


(defun gadget-values (top-gadget)
"This returns the values of all the named components of the gadget.
The return is of the form:
((:RADIO-B1 (\"Label1\"))(:TEXT-B1 \"Label3\")).  You might use
the function Value-Of to get the appropriate value."
  (let ((values-list NIL))
    (dovalues (part top-gadget :components)
       (if (g-local-value part :known-as)
         (push (list (g-local-value part :known-as)
           (g-value part :value)) values-list)))
      values-list))

(defun value-of (gadget value-list)
"Value-list is the return from gadget-values, which is also the second
parameter to a user-specified OK function.  gadget is the name of a
gadget (e.g. :add-replace) Returns the associated value"
  (cadr (assoc gadget value-list)))

(defun set-initial-value (top-gadget gadget-name value)
"This is used to set the initial value of a component in the dialog box
before it is displayed.  For example:
(set-initial-value TOP-GADGET :rb1 \"Label1\")
This can be done before or after the window is shown."
  (g-value top-gadget gadget-name :value)
  (s-value (g-value top-gadget gadget-name) :value value))


;;
;; REMEMBER! The :function-for-ok value must be set in the top-level of
;; the dialog box, not in the okcancel gadget.  That is, set the
;; :function-for-ok in the parent of the okcancel panel.
;;
(defun standard-OK-function (top-gadget)
"This is embedded in Gilt-created gadgets to call the user-specified OK
 function"
  (let ((win (g-local-value top-gadget :window)))
    (when win
      (s-value win :visible NIL)
      (kr-send top-gadget :function-for-ok top-gadget
	       (gadget-values top-gadget))
      (opal:update win)
      ;;; now check if inside a wait-interaction-complete
      (when (plusp inter::*waiting-for-exit-wait-interaction-complete*)
	(inter:interaction-complete (gadget-values top-gadget)))
      )))

(defun standard-Apply-function (top-gadget)
"This is embedded in Gilt-created gadgets to call the user-specified
OK function, but does not take down the window" 
  (kr-send top-gadget :function-for-ok top-gadget (gadget-values top-gadget)))

(defun standard-Cancel-function (top-gadget)
"This is embedded in Gilt-created gadgets to handle the cancel function"
  (let ((win (g-local-value top-gadget :window)))
    (when win
      (s-value win :visible NIL)
      (kr-send top-gadget :function-for-cancel
	       top-gadget (gadget-values top-gadget))
      (opal:update win)
      ;;; now check if inside a wait-interaction-complete
      (when (plusp inter::*waiting-for-exit-wait-interaction-complete*)
	(inter:interaction-complete NIL))
      )))

(defun OKCancel-Function (gadget value)
  "This is embedded in Gilt-created gadgets to handle the OK, Apply
and cancel functions"
  (cond ((string= value "OK")
	 (standard-OK-function (g-value gadget :parent)))
	((string= value "Apply")
	 (standard-Apply-function (g-value gadget :parent)))
	((string= value "Cancel")
	 (standard-Cancel-function (g-value gadget :parent)))))

;; one of the objects in the list must have slot as a parameter,
;; return its value.  If can't find any, returns NIL
(defun Value-From-One-Obj (obj-or-objs slot)
  (if (listp obj-or-objs)
      (dolist (obj obj-or-objs)
	(when (member slot (g-value obj :parameters))
	  (return (g-value obj slot))))
      ;; else just one obj
      (g-value obj-or-objs slot)))

;; When obj-or-objs is a list, checks each one and sets those that
;; have slot as a parameter.  When not a list, just sets it without checking
(defun Set-Value-Of-Appropriate-Objs (obj-or-objs slot val)
  (if (listp obj-or-objs)
      (dolist (obj obj-or-objs)
	(when (member slot (g-value obj :parameters))
	  (s-value obj slot val)))
      ;; else just one obj
      (s-value obj-or-objs slot val)))
