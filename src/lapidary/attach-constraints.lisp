;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: GARNET-GADGETS -*-
;;; This file contains code for attaching constraints to objects
;;; in the graphical editor. The code handles the actions associated
;;; with the constraint menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Change Log
;;;
;;; 7/14/93 amickish - Ignored obj-over in Remove-Constraint and Create-Custom-
;;;                    constraint.
;;; 5/10/93 bvz Created
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "GARNET-GADGETS")

;;; ============================================================
;;; determine if the two objects have a common ancestor in the
;;; aggregate hierarchy. A simple way of doing this is to go
;;; to the tops of each of their aggregate hierarchies and 
;;; comparing the respective roots. Although a marking strategy
;;; could save us from going all the way to the top of the
;;; hierarchies, we would have to retract all the marks at
;;; the end, which would complicate the function. Also the
;;; hierarchies are likely to be shallow so marking probably
;;; would not save any time
;;; ============================================================


(defun get-root (obj)
  (declare (special *constraint-gadget*))
  (let ((parent (or (g-value obj :parent)
		    (g-value obj :operates-on)))
	(top-agg (or (g-value *constraint-gadget* :top-level-agg)
		     (g-value obj :window :aggregate))))
    (if (is-a-p obj inter:interactor)
	(if parent
	    (get-root parent)
	    obj)
        (if (eq (g-value obj :parent)
		top-agg)
	    (return-from get-root obj)
	    (get-root parent)))))

(defun common-ancestor-p (obj1 obj2)
  (let ((parent1 (get-root obj1))
	(parent2 (get-root obj2)))
    (eq parent1 parent2)))

;;; ============================================================
;;; destroy any support slots for a constraint,
;;; such as link and offset slots. Only destroy
;;; these slots when no other formulas depend on
;;; them
;;; ============================================================

(defun destroy-constraint-support-slots (schema formula 
					  &optional (destroy-meta-info-p nil))
  (let ((meta (kr::a-formula-meta formula)))
    (when meta
	(dolist (link (g-formula-value formula :links))
	  (when (null (cdr (kr::get-dependents schema link)))
		(destroy-slot schema link)
		(s-value schema :links 
			 (delete link (g-value schema :links)))))
	(when (null (cdr (kr::get-dependents schema 
				   (g-formula-value formula :offset))))
	      (destroy-slot schema (g-formula-value formula :offset)))
	(when (null (cdr (kr::get-dependents schema 
				       (g-formula-value formula :scale))))
	      (destroy-slot schema (g-formula-value formula :scale)))
	(when destroy-meta-info-p
	      (destroy-slot meta :links)
	      (destroy-slot meta :offset)
	      (destroy-slot meta :scale)))))

;;; ============================================================
;;; destroy a constraint and any associated support
;;; slots, such as link and offset slots
;;; ============================================================

(defun cg-destroy-constraint (schema slot)
  (when (null schema)
	(constraint-gadget-error "You need to select an object before you can
destroy a constraint. If you have already
selected an object, try making a different
selection. For example, if you have made a
secondary selection, try making a primary
selection instead.")
	(return-from cg-destroy-constraint))
  (let ((formula (get-value schema slot)))
    (when (formula-p formula)
	  (destroy-constraint-support-slots schema formula)
	  (destroy-constraint schema slot))))

;;; ======================================
;;; attach a constraint to a slot
;;; of a primarily selected object
;;; ======================================

(defun attach-constraint (menu slot link-slot offset-slot constraint
			       &optional (scale-slot))
  (declare (special *constraint-gadget*))
  (let ((p-selected-item (g-value *constraint-gadget* :obj-to-constrain))
	(s-selected-item (g-value *constraint-gadget* :obj-to-reference)))

    ;; certain slots should not be altered. if this slot is
    ;; one of them, tell the user and do not proceed
    (when (member slot (g-value p-selected-item :do-not-alter-slots))
	  (constraint-gadget-error
	   (format nil "cannot change ~S's ~S slot" 
		   p-selected-item slot))
	  (return-from attach-constraint))

    ;; store the value of the offset field in the offset slot,
    ;; the value of the secondary selection in the link slot,
    ;; and the value of the scale field (if there is one) in the
    ;; scale slot. these slots must be set
    ;; before the formula is installed in slot, since KR will 
    ;; immediately evaluate the formula and thus these fields must
    ;; already be set
    (s-value p-selected-item offset-slot
	     (g-value menu offset-slot))
    (when scale-slot
	  (s-value p-selected-item scale-slot
		   (g-value menu scale-slot)))

    (when (not (eq (g-value p-selected-item link-slot) s-selected-item))
	  (cond ((eq p-selected-item s-selected-item)
		 ;; this case must be handled differently since make-path
		 ;; would generate nil and (gvl ) would be returned.
		 ;; The (gv :self) will be substituted into the slot's 
		 ;; formula when the object is saved, so the superfluous
		 ;; link access will not slow down the object when it
		 ;; runs in an application
		 (s-value p-selected-item link-slot (o-formula (gv :self))))

		;; if there is a common ancestor, construct a formula that
		;; walks through the aggregate hierarchy to find the secondary
		;; selection
		((common-ancestor-p p-selected-item s-selected-item)
		 (s-value p-selected-item link-slot
		       (eval `(o-formula (gvl ,@(gilt::make-path p-selected-item
						   s-selected-item))))))

		;; if no common ancestor, simply store the secondary
		;; selection in the link slot
		(t (s-value p-selected-item link-slot s-selected-item))))

    ;; install the formula in the slot 

    (when (not (is-a-p (get-value p-selected-item slot) constraint))
	  (let ((formula (kr::make-into-o-formula (formula constraint))))
	    ;; store information about links and offsets in the formula
	    (s-formula-value formula :links (list link-slot))
	    (s-formula-value formula :offset offset-slot)
	    (when scale-slot
		  (s-formula-value formula :scale scale-slot))
	    (kr::copy-to-all-instances p-selected-item slot formula)))
    
    ;; call the custom function, even if the formula already existed, because
    ;; the offset or link might have changed
    (when (g-value *constraint-gadget* :custom-function)
	  (funcall (g-value *constraint-gadget* :custom-function) 
		   p-selected-item slot
		   (get-value p-selected-item slot)))))

;;; ======================================
;;; remove a constraint from a slot and
;;; destroy all slots used to support that
;;; constraint
;;; ======================================

(defun remove-constraint (inter obj-over)
  (declare (special *constraint-gadget*)
	   (ignore obj-over))

;; if there was a constraint button selected, deselect it
  (when (g-value inter :deselect)
	(deselect-constraint-button (g-value inter :deselect)))
  (let ((slot (g-value inter :slot))
        (obj (g-value *constraint-gadget* :obj-to-constrain)))
    (cg-destroy-constraint obj slot)))

;;; ======================================
;;; create a custom constraint
;;; ======================================

(defun create-custom-constraint (inter obj-over)
  (declare (special *constraint-gadget*)
	   (ignore obj-over))

;; if there was a constraint button selected, deselect it
  (when (g-value inter :deselect)
	(deselect-constraint-button (g-value inter :deselect)))
  (if (g-value *constraint-gadget* :obj-to-constrain)
      (c32 (g-value *constraint-gadget* :obj-to-constrain)
	   (g-value inter :slot))
      (c32)))

