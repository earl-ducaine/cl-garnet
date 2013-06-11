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
;;; This file contains code that deletes objects from the
;;; Lapidary editor window

#|
========================================================================
Change log:
         5/22/92 Brad Vander Zanden - fixed destroy-lapidary-object so
                   that it recursively destroys instances
========================================================================
|#
(in-package "LAPIDARY")

(defparameter slots-to-check-at-destroy-time
  '((:left :left-offset :left-over)
    (:top :top-offset :top-over)
    (:width :width-offset :width-over)
    (:height :height-offset :height-over)
    (:x1 :x1-over :x1-offset)
    (:x2 :x2-over :x2-offset)
    (:y1 :y1-over :y1-offset)
    (:y2 :y2-over :y2-offset)))

;;;===================================================
;;; Top-level delete object routine. It deselects
;;; all selected objects and then deletes them
;;; one at a time.
;;;===================================================

(defun delete-objects ()
  (let ((selected-objects (g-value *selection-info* :selected)))

    (deselect-objects)
    ;; first make sure that all objects to be deleted are at the top level
    (dolist (object selected-objects)
	    (when (not (eq (g-value object :parent)
			   (g-value object :window :editor-agg)))
		  (lapidary-error (format nil
"Only top-level objects can be deleted. Object ~S is part
of an aggregate and thus cannot be deleted. You should first
ungroup its aggregate." object))
		  (return-from delete-objects)))

    ;; now destroy the objects
    (dolist (object selected-objects)
	    (destroy-lapidary-object object))
    (opal:update-all)))

(defun destroy-lapidary-object (object)

  ;; first destroy all instances of this aggregate
  (dovalues (inst object :is-a-inv :local t)
	    (destroy-lapidary-object inst))

  ;; deactivate any interactors that reference this object. Do not
  ;; destroy them because the user may want to replace the deleted object
  ;; with another object
    (dolist (inter (search-for-inter object))
	    (s-value inter :active nil))

  ;; destroy all formulas that reference this object. the reason for
  ;; destroying them rather than keeping than around using KR's broken
  ;; link mechanism is that they may reference this object indirectly
  ;; through link slots, and the link slots may be left with dangling
  ;; pointers. When an instance of the object containing the "broken-link"
  ;; formulas is created, the instance will inherit the dangling pointer,
  ;; and the formula will fail to evaluate properly. Thus we are better
  ;; off destroying the formulas. 
  (let (schema slot-list slot)
    (doslots (destroy-slot object)
	    (dolist (formula (kr::get-dependents object destroy-slot))
		    (when (formula-p formula)
			  (setf slot (kr::on-slot formula))
			  (setf schema (kr::on-schema formula))
			  (setf slot-list 
				(assoc slot slots-to-check-at-destroy-time))
			  (when (and slot-list 
				     (has-slot-p schema (second slot-list)))
				(destroy-constraint schema slot)
				(destroy-slot schema (second slot-list))
				(destroy-slot schema (third slot-list)))))))

  (let ((parent (g-local-value object :parent)))
    (when parent
	  ;; destroy the link to this object in the object's parent, then 
	  ;; destroy the object
	  (destroy-slot parent (g-local-value object :known-as))
	  (opal:remove-component parent object)

	  ;; destroy the object's parent if its parent no longer has any
	  ;; components and its parent is not the top-level aggregate
	  (when (and (null (g-value parent :components))
		     (not (eq (g-value parent :window :editor-agg) parent)))
		(destroy-lapidary-object parent))))

  (opal:destroy object))
