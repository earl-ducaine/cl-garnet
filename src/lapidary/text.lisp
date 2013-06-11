;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
============================================================
Change log:
         8/25/92  Andrew Mickish - Added proclaim
         6/25/89  Brad Vander Zanden - created
============================================================
|#
; (load "text.lisp")

(in-package "LAPIDARY")

(declaim (special text-inter create-text-inter shape-menu))

(defun text-do-go ()
  (text-do-stop)
  (make-text-properties-db)
  (make-text-inter)
  (make-text-creation-inter))

(defun text-do-stop ()
  (when (boundp 'text-properties-win) (opal:destroy text-properties-win))
  (when (boundp 'text-inter) (opal:destroy text-inter))
  (when (boundp 'create-text-inter) (opal:destroy create-text-inter)))

;;;===================================================================
;;;
;;; set up the text interactor for editing a piece of selected text
;;;
;;;===================================================================

(defun make-text-inter ()
  (declare (special *selection-info* move-inter))

  (create-instance 'text-inter inter:text-interactor
	   (:window (o-formula (gv *selection-info* :window)))
	   (:active (o-formula (and (gv editor-menu :build-p)
				    (gv *selection-info* :selected))))
	   (:waiting-priority inter:high-priority-level)
	   (:start-action #'(lambda (inter obj evt)
	      (call-prototype-method inter obj evt)
	      ;; turn selection boxes off
	      (let ((feedback (or (g-value obj :p-feedback-obj)
				  (g-value obj :s-feedback-obj))))
		(s-value feedback :visible nil))))
	   (:stop-action #'(lambda (inter obj evt)
	      (call-prototype-method inter obj evt)
	      ;; if there was a constraint in the slot, destroy it
	      (destroy-constraint obj :string)
	      ;; turn selection boxes back on
	      (let ((feedback (or (g-value obj :p-feedback-obj)
				 (g-value obj :s-feedback-obj))))
		(s-value feedback :visible t))))
	   (:start-where (formula '(list :list-element-of *selection-info*
					 :selected :type opal:text)))
	   (:start-event *obj-creation-button*)
	   (:stop-event (o-formula (if (is-a-p (gvl :remembered-object)
					       opal:cursor-multi-text)
				       '(:any-mousedown)
				       '(:any-mousedown #\Return))))))

;;;===================================================================
;;;
;;; set up the text interactor for creating a piece of text
;;;
;;;===================================================================

(defun make-text-creation-inter ()
  (create-instance 'create-text-inter inter:text-interactor
	 (:start-where t)
	 (:active (o-formula (and (or (eq (gv shape-menu :value) opal:text)
				      (is-a-p (gv shape-menu :value) opal:text))
				  (gv editor-menu :build-p))))
	 (:start-event *obj-creation-button*)
	 (:stop-event (o-formula (if (is-a-p (gv shape-menu :value)
					     opal:cursor-multi-text)
				     '(:any-mousedown)
				     '(:any-mousedown #\Return))))
	 (:feedback-obj (o-formula (gvl :current-window :create-text-feedback)))
	 (:window (o-formula (gv *selection-info* :window)))
	 (:stop-action
	  #'(lambda (inter obj evt)
	      ;; call parent to turn off feedback obj visibility
	      (call-prototype-method inter obj evt)
	      (let* ((feedback-obj (g-value inter :feedback-obj))
		     (editor-agg (g-value inter :current-window :editor-agg))
                     new-obj)    
               (when (not (string= (g-value feedback-obj :string) "")) 
		 (setf new-obj (create-instance nil (g-value shape-menu :value)
		        (:left (g-value feedback-obj :left))
			(:top (g-value feedback-obj :top))
			(:string (g-value feedback-obj :string))))
		 ;; generate a name for the new object
		 (name-lapidary-obj new-obj)

		 (s-value editor-agg (g-value new-obj :known-as) new-obj)
		 (s-value new-obj :font (g-value text-properties-win :value))
		 (opal:add-component editor-agg new-obj)
		 ;; cause the new object to be selected
		 (deselect-objects)
		 (primary-select new-obj)
		 (s-value feedback-obj :string "")))))))
