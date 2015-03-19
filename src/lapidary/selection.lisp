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
;;; This file contains code that handles selections in the
;;; graphical editor. It defines
;;;     1) primary and secondary selection feedback objects
;;;     2) primary and secondary selections
;;;     3) moving objects
;;;     4) growing objects

;;; CHANGE LOG
;;;
;;; 08/10/93 bvz - Added two interactors, prim-sel-add-using-sel-handles,
;;;                 and sec-sel-add-using-sel-handles, that allow covered
;;;                 objects which have been selected to cycle up the 
;;;                 aggregate hierarchy
;;; 08/24/92 amickish - Proclaimed editor-menu special

(in-package "LAPIDARY")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Selection-Do-Go Selection-Do-Stop))
  (proclaim '(special editor-menu)))

(defvar *selection-bbox* (opal::make-bbox))

(defun Selection-do-go ()

;;; the editor keeps a list of feedback objects that it can use to cover
;;; selected objects. initially this list is set to nil. whenever, the
;;; editor needs a feedback object it tries to grab one from the list.
;;; if the list is empty, the editor creates a new feedback object. when
;;; an object is deselected, its feedback object is returned to the list.
;;; we need to keep two lists--one for primary selections and one for
;;; secondary selections

  (setf *undersized-feedback-list* nil)
  (setf *leaf-feedback-list* nil)
  (setf *agg-feedback-list* nil)
  (setf *line-feedback-list* nil)

;;; create an interactor that selects objects and adds them to a list of
;;; selected objects.

  (create-instance
   'prim-add-to-select-inter inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:continuous nil)
   (:active (o-formula (gv editor-menu :build-p)))
   (:how-set :set-toggle)
   (:start-event nil)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (when final-obj-over
		       (if (and (or (is-a-p final-obj-over opal:line)
				    (is-a-p final-obj-over opal:arrowhead))
				(or (is-a-p (g-local-value final-obj-over :parent)
					    garnet-gadgets:arrow-line)
				    (is-a-p (g-local-value final-obj-over :parent)
					    garnet-gadgets:double-arrow-line)))
			   (make-p-selection (g-value final-obj-over :parent))
			   (make-p-selection final-obj-over))
		       )))
   (:start-where (list :leaf-element-of nil))
   (:outside nil))
  
;;; create an interactor that secondarily selects objects and adds them
;;; to a list of secondary selections.

  (create-instance 
   'sec-add-to-select-inter inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:continuous nil)
   (:active (o-formula (gv editor-menu :build-p)))
   (:how-set :set-toggle)
   (:start-event nil)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (when final-obj-over
		       (if (and (or (is-a-p final-obj-over opal:line)
				    (is-a-p final-obj-over opal:arrowhead))
				(or (is-a-p (g-local-value final-obj-over :parent)
					    garnet-gadgets:arrow-line)
				    (is-a-p (g-local-value final-obj-over :parent)
					    garnet-gadgets:double-arrow-line)))
			   (make-s-selection (g-value final-obj-over :parent))
			   (make-s-selection final-obj-over))
		       )))
   (:start-where (list :leaf-element-of nil))
   (:outside nil))

;; create an interactor that makes primary selections and adds them to a
;; list of primary selections--this interactor only activates over selection
;; handles--by doing do, the interactor can move up the aggregate hierarchy
;; on objects that are obscured by other objects.
  (create-instance 'prim-sel-add-using-sel-handles inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:start-where 
    (o-formula (list :list-leaf-element-of
		     *selection-info* :feedback)))
   (:waiting-priority inter:high-priority-level)
   (:continuous t)
   (:active (o-formula (gv editor-menu :build-p)))
   (:how-set :set-toggle)
   (:start-event *prim-add-to-select*)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (let ((current-selection 
			    (g-value final-obj-over :parent :obj-over)))
		       ;; if current selection is a top-level aggregate, we
		       ;; want to cycle the selection down to one of the
		       ;; leaves
		       (if (eq (g-value current-selection :parent)
			       (g-value current-selection :window :editor-agg))
			   (make-p-selection
			    (car (get-agg-leaves current-selection)))
			   (make-p-selection current-selection))))))

;; create an interactor that makes secondary selections and adds them to a
;; list of secondary selections--this interactor only activates over selection
;; handles--by doing do, the interactor can move up the aggregate hierarchy
;; on objects that are obscured by other objects.
  (create-instance 'sec-sel-add-using-sel-handles inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:start-where 
    (o-formula (list :list-leaf-element-of
		     *selection-info* :feedback)))
   (:waiting-priority inter:high-priority-level)
   (:continuous t)
   (:active (o-formula (gv editor-menu :build-p)))
   (:how-set :set-toggle)
   (:start-event *sec-add-to-select*)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (let ((current-selection 
			    (g-value final-obj-over :parent :obj-over)))
		       ;; if current selection is a top-level aggregate, we
		       ;; want to cycle the selection down to one of the
		       ;; leaves
		       (if (eq (g-value current-selection :parent)
			       (g-value current-selection :window :editor-agg))
			   (make-s-selection
			    (car (get-agg-leaves current-selection)))
			   (make-s-selection current-selection))))))

;;; create an interactor that selects an object.
;;; the interactor should toggle selections, so that
;;; selecting a previously selected object causes it to cycle through
;;; the aggregate hierarchy. if the object is not previously 
;;; selected, deselect the other primary selections

  (create-instance
   'prim-select-one-obj-inter inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:continuous nil)
   (:active (o-formula (gv editor-menu :build-p)))
   (:how-set :set-toggle)
   (:start-event nil)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (if (eq final-obj-over :none)
			 (primary-deselect-objects :none)
		       (if (and (or (is-a-p final-obj-over opal:line)
				      (is-a-p final-obj-over opal:arrowhead))
				  (or (is-a-p (g-local-value final-obj-over :parent)
					      garnet-gadgets:arrow-line)
				      (is-a-p (g-local-value final-obj-over :parent)
					      garnet-gadgets:double-arrow-line)))
			     (make-p-selection (g-value final-obj-over
							:parent) t)
			     (make-p-selection final-obj-over t)))
		     ))
   (:start-where (list :leaf-element-of-or-none nil))
   (:outside nil))
  
;;; create an interactor that secondarily selects an object.
;;; the interactor should toggle selections, so that
;;; selecting a previously selected object causes it to cycle through
;;; the aggregate hierarchy. if the object is not previously 
;;; selected, deselect the other secondary selections

  (create-instance
   'sec-select-one-obj-inter inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:continuous nil)
   (:active (o-formula (gv editor-menu :build-p)))
   (:how-set :set-toggle)
   (:start-event nil)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (if (eq final-obj-over :none)
			 (secondary-deselect-objects :none)
			 (if (and (or (is-a-p final-obj-over opal:line)
				      (is-a-p final-obj-over opal:arrowhead))
				  (or (is-a-p (g-local-value final-obj-over :parent)
					      garnet-gadgets:arrow-line)
				      (is-a-p (g-local-value final-obj-over :parent)
					      garnet-gadgets:double-arrow-line)))
			     (make-s-selection (g-value final-obj-over
							:parent) t)
			     (make-s-selection final-obj-over t)))
		     ))
   (:start-where (list :leaf-element-of-or-none nil))
   (:outside nil))

  (create-instance 'prim-sel-one-obj inter:two-point-interactor
		 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:abort-if-too-small t)
		 (:min-width *selection-threshold*)
		 (:min-height *selection-threshold*)
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'select-one-object)
		 (:start-event *prim-select-one-obj*)
		 (:deselect-fct 'primary-deselect-objects)
		 (:selection-inter prim-select-one-obj-inter)
		 (:selection-fct 'make-p-selection))

  (create-instance 'sec-sel-one-obj inter:two-point-interactor
		 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:abort-if-too-small t)
		 (:min-width *selection-threshold*)
		 (:min-height *selection-threshold*)
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'select-one-object)
		 (:start-event *sec-select-one-obj*)
		 (:deselect-fct 'secondary-deselect-objects)
		 (:selection-inter sec-select-one-obj-inter)
		 (:selection-fct 'make-s-selection))

  (create-instance 'prim-add-to-sel inter:two-point-interactor
		 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:abort-if-too-small t)
		 (:min-width *selection-threshold*)
		 (:min-height *selection-threshold*)
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'select-one-object)
		 (:start-event *prim-add-to-select*)
		 (:selection-inter prim-add-to-select-inter)
		 (:selection-fct 'make-p-selection))

  (create-instance 'sec-add-to-sel inter:two-point-interactor
		 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:abort-if-too-small t)
		 (:min-width *selection-threshold*)
		 (:min-height *selection-threshold*)
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'select-one-object)
		 (:start-event *sec-add-to-select*)
		 (:selection-inter sec-add-to-select-inter)
		 (:selection-fct 'make-s-selection))
  
;;; create an interactor that deselects one primary selection. the 
;;; interactor should only deselect the object it is over

  (create-instance 
   'primary-deselection-inter inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:active (o-formula (gv editor-menu :build-p)))
   (:continuous nil)
   (:how-set :set-toggle)
   (:start-event nil)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (primary-deselect-objects final-obj-over)
		     ))
   (:start-where (list :leaf-element-of-or-none nil))
   (:outside nil))

;;; create an interactor that deselects secondary selections. the interactor should
;;; deselect all secondary selections if it is not over any object; otherwise
;;; it should only deselect the object it is over

  (create-instance 
   'secondary-deselection-inter inter:button-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:active (o-formula (gv editor-menu :build-p)))
   (:continuous nil)
   (:how-set :set-toggle)
   (:start-event nil)
   (:stop-action #'(lambda (interactor final-obj-over)
		     (declare (ignore interactor))
		     (secondary-deselect-objects final-obj-over)
		     ))
   (:start-where (list :leaf-element-of-or-none nil))
   (:outside nil))

;;; create an interactor that deselects either one primary selection or
;;; all primary selections within a selection box

  (create-instance 
   'prim-deselect-obj inter:two-point-interactor
                 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:abort-if-too-small t)
		 (:min-width *selection-threshold*)
		 (:min-height *selection-threshold*)
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'select-one-object)
		 (:start-event *primary-deselection-button*)
		 (:selection-inter primary-deselection-inter)
		 (:selection-fct 'primary-deselect-objects))

;;; create an interactor that deselects either one secondary selection or
;;; all secondary selections within a selection box

  (create-instance 
   'sec-deselect-obj inter:two-point-interactor
                 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:abort-if-too-small t)
		 (:min-width *selection-threshold*)
		 (:min-height *selection-threshold*)
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'select-one-object)
		 (:start-event *secondary-deselection-button*)
		 (:selection-inter secondary-deselection-inter)
		 (:selection-fct 'secondary-deselect-objects))

;;; The push selection under operation acts like a push stack operation.
;;;	It deselects the topmost selected object and selects the topmost
;;;	unselected object
;;;
;;;	e.g., 	->A	  A
;;;		->B  => ->B
;;;		  C	->C
;;;		->D	->D
;;;
;;;	(The selections got pushed from A and B to B and C)
;;;
;;;	2 exceptions are if there are no selected objects, in which
;;;	case the topmost object is selected, and when the bottommost
;;;	object is selected, in which case the topmost selection is
;;;	deselected but no selectio is made (in effect, one of the
;;;	selections falls off the edge).

  (create-instance 
   'prim-push-sel-under inter:two-point-interactor
                 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'push-selection-under)
		 (:start-event *prim-push-sel-under-button*)
		 (:select-fct 'make-p-selection)
		 (:deselect-fct 'primary-deselect)
		 (:sel-slot :p-feedback-obj))

  (create-instance 
   'sec-push-sel-under inter:two-point-interactor
                 (:window (o-formula (gv *selection-info* :window)))
		 (:active (o-formula (gv editor-menu :build-p)))
		 (:feedback-obj 
		  (formula `(gvl :current-window :selection-feedback)))
		 (:start-where t)
		 (:final-function 'push-selection-under)
		 (:start-event *sec-push-sel-under-button*)
		 (:select-fct 'make-s-selection)
		 (:deselect-fct 'secondary-deselect)
		 (:sel-slot :s-feedback-obj)))


(defun Selection-Do-Stop ()
  (when (boundp 'prim-add-to-sel) (opal:destroy prim-add-to-sel))
  (when (boundp 'prim-sel-one-obj) (opal:destroy prim-sel-one-obj))
  (when (boundp 'prim-select-one-obj-inter) 
    (opal:destroy prim-select-one-obj-inter))
  (when (boundp 'prim-add-to-select-inter) 
    (opal:destroy prim-add-to-select-inter))
  (when (boundp 'sec-add-to-sel) (opal:destroy sec-add-to-sel))
  (when (boundp 'sec-sel-one-obj) (opal:destroy sec-sel-one-obj))
  (when (boundp 'sec-select-one-obj-inter) 
    (opal:destroy sec-select-one-obj-inter))
  (when (boundp 'sec-add-to-select-inter) 
    (opal:destroy sec-add-to-select-inter))
  (when (boundp 'primary-deselection-inter) 
    (opal:destroy primary-deselection-inter))
  (when (boundp 'secondary-deselection-inter) 
    (opal:destroy secondary-deselection-inter)) 
  (when (boundp 'prim-deselect-obj) (opal:destroy prim-deselect-obj)) 
  (when (boundp 'sec-deselect-obj) (opal:destroy sec-deselect-obj))
  )


