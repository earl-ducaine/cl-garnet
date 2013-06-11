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
;;; This file contains code that supports the multiple
;;; selection of objects via selection boxes and the
;;; selection of covered objects.
;;;
;;; select-multiple-objects: determines if an object intersects
;;;     a selection box
;;;
;;; select-one-object: determines if the user has selected a single 
;;;     object or swept out a selection rectangle, and selects
;;;     objects based on the outcome.
;;;
;;; push-selection-under
;;; push-under-recur:    select a covered object
;;;

;;; CHANGE LOG
;;;
;;; 08/10/93 bvz - modified the multiple object selection routines so that
;;;                  they could support either the selection of leaves or
;;;                  top-level objects.
;;; 08/24/92 amickish - Removed declare ignore of point-list in push-selection-
;;;                     under.

(in-package "LAPIDARY")

;;;===================================================
;;;
;;; traverse the aggregate hierarchy and select those
;;; objects which intersect the selection box
;;;
;;; Note: sel-fct is the function that makes the 
;;;    selection
;;;
;;;===================================================

(opal::define-method :select-multiple-objects opal:aggregate (agg sel-fct)
  (declare (special *selection-bbox* *selection-info*))
  (let (agg-bbox)
    (when (g-value agg :visible)
      (setf agg-bbox (opal::update-info-old-bbox
		      (get-local-value agg :update-info)))
      (when (opal::bbox-intersect-p *selection-bbox* agg-bbox)
	(if (g-value *selection-info* :leaf)
	    (dovalues (child agg :components)
		  (kr-send child :select-multiple-objects child sel-fct))
	    (funcall sel-fct agg))))))

(opal::define-method :select-multiple-objects opal:graphical-object (obj sel-fct)
  (declare (special *selection-bbox* *selection-info*))
  (let (obj-bbox)
    (when (g-value obj :visible)
      (setf obj-bbox (opal::update-info-old-bbox
		      (get-local-value obj :update-info)))
      (when (opal::bbox-intersect-p *selection-bbox* obj-bbox)
	(funcall sel-fct obj)))))

(opal::define-method :select-multiple-objects 
		     garnet-gadgets:double-arrow-line (agg sel-fct)
  (declare (special *selection-bbox* *selection-info*))
  (let (agg-bbox)
    (when (g-value agg :visible)
      (setf agg-bbox (opal::update-info-old-bbox
		      (get-local-value agg :update-info)))
      (when (opal::bbox-intersect-p *selection-bbox* agg-bbox)
	(funcall sel-fct agg)))))

(opal::define-method :select-multiple-objects 
		     garnet-gadgets:arrow-line (agg sel-fct)
  (declare (special *selection-bbox* *selection-info*))
  (let (agg-bbox)
    (when (g-value agg :visible)
      (setf agg-bbox (opal::update-info-old-bbox
		      (get-local-value agg :update-info)))
      (when (opal::bbox-intersect-p *selection-bbox* agg-bbox)
	(funcall sel-fct agg)))))

;;;===================================================
;;;
;;; Determine if the user has selected a single object
;;; or swept out a selection rectangle, and select
;;; objects based on the outcome.
;;;
;;; Note: sel-fct determines which function makes the
;;;   selection and sel-inter determines which 
;;;   interactor to activate if only one selection
;;;   is being made.
;;;
;;;===================================================

(defun select-one-object (inter point-list)
  (declare (special *current-window* *selection-bbox*))

  ; single selection or selection box?
  (if (null point-list)
      
      ; single selection--find the object that the mouse is
      ; over and select it. if the mouse is over the background,
      ; deselect all objects
      (let ((selection-inter (g-value inter :selection-inter)))
	;; before activating the appropriate selection interactor, set its
	;; start-where to the aggregate of the current window
	(setf (second (g-value selection-inter :start-where))
	      (g-value inter :current-window :editor-agg))
	(inter:start-interactor selection-inter))
      
      ; else
         
      (let ((x (first point-list))
	    (y (second point-list))
	    (wd (third point-list))
	    (ht (fourth point-list)))
	
	; should currently selected objects be deselected?
	(let ((deselect-fct (get-local-value inter :deselect-fct)))
	  (when deselect-fct
	    (funcall deselect-fct :none)))
	
	; turn the selection box into a bounding box that
	; bbox-intersect-p can use
	
	(setf (opal::bbox-x1 *selection-bbox*) x)
	(setf (opal::bbox-y1 *selection-bbox*) y)
	(setf (opal::bbox-x2 *selection-bbox*) (1- (+ x wd)))
	(setf (opal::bbox-y2 *selection-bbox*) (1- (+ y ht)))
	
	; traverse the aggregate hierarchy to determine which 
	; objects should be selected
	
	(dovalues (child (g-value inter :current-window :editor-agg) :components)
		  (kr-send child :select-multiple-objects child 
					   (g-value inter :selection-fct)))))
  )

;;; Find the topmost selected item in a stack of items and deselect it.
;;; Then descend through the stack until an unselected item is found
;;; and select it. If the function reaches the bottom of the stack without
;;; finding an unselected item it does nothing. If none of the items
;;; in the stack are selected, the function selects the topmost item.

(defun push-selection-under (inter point-list)
  (declare (special *selectp*)) ; indicates whether to select or deselect
                                ; an obj
  (declare (special *top-obj*)) ; top obj in the stack
  (declare (special *select-fct* *deselect-fct*)) ; functions used to
                                                  ; select and deselect
                                                  ; objs 
  (declare (special *sel-slot*)) ; slot used to determine if obj currently
                                 ; selected
  
  (setf *selectp* nil)
  (setf *top-obj* nil)
  (setf *select-fct* (g-value inter :select-fct))
  (setf *deselect-fct* (g-value inter :deselect-fct))
  (setf *sel-slot* (g-value inter :sel-slot))
  
  (let ((x (first point-list))
	(y (second point-list)))
    (when (and (not (push-under-recur 
		     (g-value inter :current-window :editor-agg :components) 
		     x y))
	       (not *selectp*)
	       *top-obj*)
      
      ; if the bottom of the stack was reached without finding a selection,
      ; select the top element of the stack
      (funcall *select-fct* *top-obj*)))
  )
	    
(defun push-under-recur (component-list x y)
  (declare (special *selectp* *top-obj*
		    *select-fct* *deselect-fct* *sel-slot*))
  (and component-list
       (or (push-under-recur (cdr component-list) x y)
	   (let ((child (car component-list)))
	     (cond ((is-a-p child opal:aggregate)
		    (when (opal::point-in-gob-method-view-object child x y)
		      (push-under-recur
		       (g-value child :components) x y)))
		   (t
		     (when (opal::point-in-gob child x y) 
		       (let ((selected (g-value child *sel-slot*)))
			 
			 ; if this obj is the topmost selected obj, 
			 ; deselect it
			 (cond ((and (not *selectp*) selected)
				(funcall *deselect-fct* child)
				(setf *selectp* t)
				nil)

			 ; if a topmost selected obj has been found and
			 ; this obj is the first unselected obj found
			 ; since then, select it
			       ((and *selectp* (not selected))
				(funcall *select-fct* child)
				t)

			 ; if this obj is the topmost obj in the stack
			 ; and it is unselected, remember it
			       ((and (not selected) (not *top-obj*))
				(setf *top-obj* child)
				nil)))))))))
  )
