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
;;; 
;;; This file contains functions used to create custom formulas with
;;; the constraint gadget. The functions in this file interface with
;;; c32.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Change Log
;;;
;;; 7/14/93 amickish - Get-Gilt-Bitmap ---> opal:Get-Garnet-Bitmap;
;;;           Added variables to (declare (special ...)) in C32 definition
;;; 5/10/93 bvz Created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")

;;; This function loads the bitmap specified from the Gilt directory
(defun Get-Gilt-Bitmap (bitmapname)
  (opal:read-image (merge-pathnames bitmapname
			 common-lisp-user::Garnet-Gilt-Bitmap-PathName)))

(defparameter HourGlassCursor
  (cons (create-instance NIL opal:bitmap
			 (:image (opal:Get-Garnet-Bitmap "hourglass.cursor")))
	(create-instance NIL opal:bitmap
			 (:image (opal:Get-Garnet-Bitmap "hourglass.mask")))))
(defparameter RegularCursor (g-value opal::window :cursor))

(defun SetHourGlassCursor (&optional extrawindows)
  (declare (special *constraint-gadget*))
  (when (g-value *constraint-gadget* :obj-to-constrain)
	(s-value (g-value *constraint-gadget* :obj-to-constrain :window)
		 :cursor HourGlassCursor)
	(opal:update (g-value *constraint-gadget* :obj-to-constrain :window)))
  (dolist (win extrawindows)
    (s-value win :cursor HourGlassCursor)
    (opal:update win)))


(defun RestoreRegularCursor (&optional extrawindows)
  (declare (special *constraint-gadget*))
  (when (g-value *constraint-gadget* :obj-to-constrain)
	(s-value (g-value *constraint-gadget* :obj-to-constrain :window)
		 :cursor RegularCursor)
	(opal:update (g-value *constraint-gadget* :obj-to-constrain :window)))
  (dolist (win extrawindows)
    (s-value win :cursor RegularCursor)
    (opal:update win)))

;;; make c32 windows disappear
(defun c32-ok (gadget value)
  (declare (ignore value))
  (s-value gadget :selection-function nil)
  (s-value gadget :modal-p t)
  (c32::c32-ok-function))

;;; pop up the c32 windows
(defun c32 (&optional (obj nil) (slot nil) 
	    &key (left nil) (top nil) (c32-custom-function nil)
	         (prompt "Press OK when you're finished with C32"))
  (declare (special *constraint-gadget* c32::lapidary-p
		    c32::*all-windows* c32::ask-object
		    c32::*current-panel-set*
		    *constraint-gadget-query-window*
		    c32::*Custom-Function* c32::*C32-Custom-Function*
		    c32::*Top-Level-Agg*))


  (let ((p-selected (g-value *constraint-gadget* :obj-to-constrain))
	(s-selected (g-value *constraint-gadget* :obj-to-reference))
	(win-list c32::*all-windows*)
	(reversed-list (reverse c32::*all-windows*))
	win panel w)
    (SetHourGlassCursor win-list)

    (if c32::*all-windows*
	(progn
	  ;; make the windows visible so that the user can see something
	  ;; happening--only do first two windows
	  (setf win (car reversed-list))
	  (s-value win :visible t)
	  (opal:raise-window win)

	  (setf win (second reversed-list))
	  (s-value win :visible t)
	  (opal:raise-window win)

	  ;; create columns for the primary and secondary selections
	  (when p-selected
		(c32::NewColumnForObj p-selected))
	  (when s-selected
		(c32::NewColumnForObj s-selected)))

        ;; else...c32 does not exist so load it and create it
        (c32:do-go :startup-objects (list p-selected s-selected)))

    (unless (and (boundp 'c32::ask-object)
		 (schema-p c32::ask-object))
	    (c32::create-ask-object))

    (setf c32::lapidary-p t)
    
    ;; set the *top-level-agg*, *custom-function*, and *c32-custom-function*
    ;; global variables in c32
    (setf c32::*top-level-agg* (g-value *constraint-gadget* :top-level-agg))
    (setf c32::*c32-custom-function* c32-custom-function)
    ;; custom function should be called only if a constraint is installed--if
    ;; a c32-custom-function is provided, the constraint will not be installed,
    ;; so custom-function should not be called
    (setf c32::*custom-function* 
	  (if c32-custom-function
	      nil
	      (g-value *constraint-gadget* :custom-function)))
    ;; if there was an object passed in, display the object and pop up
    ;; the formula box for the slot that is passed in

    (when obj
	  ;; when the object was not selected (it is probably an interactor),
	  ;; display the object
	  (when (and (not (eq obj p-selected))
		     (not (eq obj s-selected)))
		(c32::NewColumnForObj obj))

	  ;; find the panel that contains the object
	  (dolist (comp (g-value c32::*current-panel-set* :aggrel
				 :components))
		  (when (eq (g-value comp :obj) obj)
			(setf panel comp)
			(return)))
	  (when slot
		;; find the c32 item that corresponds to this slot, extract its
		;; formula icon, and call the appropriate c32 function to bring
		;; up a formula window for the slot
		(dolist (item (g-value panel :c32-items))
		  (when (eq (g-value item :slot) slot)
			(setf w (c32::get-form-win-for-icon nil 
				     (g-value item :form-icon)))
			(return)))

		;; if a constraint already exists on the object, set the
		;; :existing-links slot in the formula window (w) to be
		;; the current links in the formula. The reason for doing
		;; this is that the user may edit the existing formula,
		;; possibly keeping old links. These old links need to be
		;; preserved. It is possible that old links will be deleted
		;; as well, in which case we would like to delete them from
		;; the :links list. Unfortunately, this is not possible.
		;; The only feasible way to determine if a link is still
		;; used by a formula is to evaluate the formula. However,
		;; if the formula uses a conditional, the link might be
		;; in a branch of the conditional that is not currently
		;; used, in which case the link might be erroneously deleted.
		(when (formula-p (get-value obj slot))
		      (s-value w :existing-links
			       (g-formula-value (get-value obj slot) :links)))
		(s-value w :disappear-p t)))

    ;; if left and top are passed in, give the user an ok button to
    ;; press when they're done with C32
    (when left
	  ;; make sure query gadget stays on screen
	  (when (> (+ left (g-value *constraint-gadget-query-window* :width))
		   opal:*screen-width*)
	    (setf left (- opal:*screen-width* 
			  (g-value *constraint-gadget-query-window* :width))))
	  (when (> (+ top (g-value *constraint-gadget-query-window* :height))
		   opal:*screen-height*)
	    (setf top (- opal:*screen-height*
			 (g-value *constraint-gadget-query-window* :height))))
	  (s-value *constraint-gadget-query-window* :selection-function
		   'c32-ok)
	  (s-value *constraint-gadget-query-window* :modal-p nil)
	  (garnet-gadgets:display-query *constraint-gadget-query-window*
			prompt
			'("OK")))

    (RestoreRegularCursor win-list)))
	 
