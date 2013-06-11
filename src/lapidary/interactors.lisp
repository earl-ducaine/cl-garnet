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
;;; This file provides the functions that handle the action buttons
;;; in the interactor menus
;;;

#|
==============================================================================
Change log:
    08/25/92: Andrew Mickish - Removed declare ignore of value in
               Write-Interactor  
    07/06/92: Brad Vander Zanden -- changed set-interactor-slots so that
               it sees the name field of an interactor when the user changes
               the name
==============================================================================
|#

 (in-package "LAPIDARY")

(defun get-start-where (queue inter)
  (let ((start-where (assoc :start-where queue)))
    (if start-where
	(cdr start-where)
        (g-value inter :start-where))))

;;;=================================================================
;;;
;;; is this interactor a garnet defined interactor?
;;;
;;;=================================================================

(defun garnet-inter-p (inter)
  (or (eq inter lapidary-button-interactor)
      (eq inter lapidary-menu-interactor)
      (eq inter lapidary-text-interactor)
      (eq inter directional-move-grow-interactor)
      (eq inter lapidary-angle-interactor)
      (eq inter lapidary-two-point-interactor)))

;;;=================================================================
;;;
;;; is this interactor a choice interactor?
;;;
;;;=================================================================

(defun choice-inter-p (inter)
  (or (eq inter lapidary-button-interactor)
      (eq inter lapidary-menu-interactor)
      (is-a-p inter lapidary-button-interactor)
      (is-a-p inter lapidary-menu-interactor)))

;;;=================================================================
;;;
;;; determine if an object pointed to by an interactor slot is in
;;; the same aggregate as the interactor. If it is, place a formula
;;; in the interactor slot that traverses the aggregate hierarchy
;;; and retrieves the object. Otherwise simply store the object in
;;; the interactor slot
;;;
;;;=================================================================

(defun store-obj-in-inter-slot (inter slot value)
  (if (common-ancestor-p inter value)
      (progn
	(s-value inter slot
		 (kr::make-into-o-formula
		  (formula `(gvl ,@(gilt:make-path inter value)))))
	;; immediately demand the value of the formula so
	;; that dependencies are properly set up
	(g-value inter slot))
      (s-value inter slot value)))

;;;=================================================================
;;;
;;; iterate through the list of slot-value pairs and store the 
;;; values in the correct slots of the interactor
;;;
;;;=================================================================

(defun set-interactor-slots (inter slot-list)

  (dolist (slot-value (reverse slot-list))
    (let ((slot (car slot-value))
	  (value (cdr slot-value)))

      ;; note--slot may be saved more than once but this restoring
      ;; it twice during undo shouldn't be a problem
;        (undo-save inter slot)

	;; perform slot specific processing
	(case slot
	  ;; destroy the old link to this interactor and create a 
	  ;; new one based on its new name (its new name will be
	  ;; a string so it must be converted to a keyword)
	  (:known-as (let ((operates-on (g-value inter :operates-on)))
		       (when operates-on
			 (destroy-slot operates-on (g-value inter :known-as))
			 (s-value operates-on value inter))
		       (s-value inter :known-as value)
		       
		       ;; change the kr name of this object to the new name
		       (setf (kr::schema-name inter) 
			     (read-from-string (symbol-name value)))
		       (set (kr::schema-name inter) inter)
		       ))
	  (:feedback
	   ;; insert list of formulas created in by-demo operation
	   ;; into the objects that this interactor can operate on;
	   ;; if the installation succeeds, set the feedback-obj slot
	   ;; to nil
	   (if (install-by-demo-formulas
		(get-start-where slot-list inter) value)
	       (progn
;		 (undo-save inter :feedback-obj)
		 (s-value inter :feedback-obj nil))))
	  (:feedback-obj 
	   (gg:cg-destroy-constraint inter :feedback-obj)
	   (cond ((null value)
		   (when (is-a-p inter lapidary-two-point-interactor)
			 (s-value inter :std-feedback-p nil))
		   (s-value inter :feedback-obj nil))
		 ((equal value "standard")
		  (s-value inter :std-feedback-p t)
		  (s-value inter :feedback-obj (o-formula (if (gvl :line-p)
						    (gvl :std-line-feedback)
						    (gvl :std-box-feedback)))))
		 ((formula-p value)
		  (install-inter-formula inter slot-value)
		  (when (is-a-p inter lapidary-two-point-interactor)
			(s-value inter :std-feedback-p nil)))
		 (value
		  ;; if there are multiple feedback objects, value has
		  ;; the form (formula feedback-obj*). Otherwise it has
		  ;; the form (feedback-obj). 
		  (let ((feedback-formula (if (formula-p (car value))
					      (car value)
					      nil)))
		    ;; if there is a feedback formula, set the value to
		    ;; the list of feedback objects
		    (when feedback-formula
			  (setf value (cdr value)))
		    (dolist (obj value)
		      (cond ((or (is-a-p inter lapidary-menu-interactor)
				 (is-a-p inter lapidary-button-interactor))
			     (create-feedback-obj obj slot-list inter))
			    ;; feedback for two-point and text
			    ;; interactors must have constraints to the :box 
			    ;; or :points slots--move-grow interactors have
			    ;; slots directly set by the interactor
			    ((or (is-a-p inter lapidary-two-point-interactor)
				 (is-a-p inter lapidary-text-interactor))

			     (if (is-a-line-p obj)
			       (progn
			         (s-value obj :points
					  (list (g-value obj :x1)
						(g-value obj :y1)
						(g-value obj :x2)
						(g-value obj :y2)))
				 (s-value obj :x1 
					  (o-formula (first (gvl :points))))
				 (s-value obj :y1 
					  (o-formula (second (gvl :points))))
				 (s-value obj :x2 
					  (o-formula (third (gvl :points))))
				 (s-value obj :y2 
					  (o-formula (fourth (gvl :points)))))
			       (progn
				 (s-value obj :box
					(list (g-value obj :left)
					      (g-value obj :top)
					      (g-value obj :width)
					      (g-value obj :height)))
			       (s-value obj :left
					(o-formula (first (gvl :box))))
			       (s-value obj :top
					(o-formula (second (gvl :box))))
			       (when (is-a-p inter lapidary-two-point-interactor)
				     (s-value obj :width
					      (o-formula (third (gvl :box))))
				     (s-value obj :height
					      (o-formula (fourth (gvl :box)))))))))

		      ;; mark the object as a feedback-object
		      (s-value obj :feedback-p t)
		      
		      ;; get rid of the inherited visibility formula, set
		      ;; the visible slot to nil, then install a formula 
		      ;; that sets the visible slot depending on whether 
		      ;; lapidary is in build or test mode (done by 
		      ;; save-value)--only do these operations if the formula
		      ;; is not already dependent on lapidary's modes
		      (when (not (eq (g-formula-value (get-value obj :visible)
						      :menu-item)
				     :lapidary-mode))
			    (gg:cg-destroy-constraint obj :visible)
			    (s-value obj :visible (o-formula (gvl :obj-over)))
			    (save-value obj :visible t)
			    ;; indicate that this formula depends on the mode
			    ;; lapidary is in
			    (s-formula-value (get-value obj :visible) 
					     :menu-item
					     :lapidary-mode))
		      
		      ;; set the :left and :top slots so that in build mode
		      ;; the feedback object appears in its current position
		      ;; and in test mode it appears as it will at
		      ;; runtime--only do this if the formulas do not already
		      ;; depend on lapidary's modes
		      (when (not (eq (g-formula-value (get-value obj :left)
						      :menu-item)
				     :lapidary-mode))
			    (save-value obj :left (g-value obj :left))
			    (s-formula-value (get-value obj :left) :menu-item
					     :lapidary-mode))
		      (when (not (eq (g-formula-value (get-value obj :top)
						      :menu-item)
				     :lapidary-mode))
			    (save-value obj :top (g-value obj :top))
			    (s-formula-value (get-value obj :top) :menu-item
					     :lapidary-mode)))
		 
		    (s-value inter :std-feedback-p nil)
		    
		    ;; if there are multiple feedback objects, then there
		    ;; is a formula that determines which object to use. This
		    ;; formula must be installed on the feedback slot. 
		    ;; Otherwise, if there is only one feedback object, it 
		    ;; should be stored in the feedback slot
		    (if feedback-formula
			(install-inter-formula inter (cons :feedback-obj
							   feedback-formula))
		      ;; check to see if the interim feedback is in the same
		      ;; aggregate as the interactor, and if it is create a
		      ;; formula to it. Otherwise, just store the interim 
		      ;; feedback object in the feedback-obj slot
		        (store-obj-in-inter-slot inter :feedback-obj 
						 (car value)))))
	  (t (s-value inter :feedback-obj nil))))
	  (:final-feedback-obj
	   (cond ((null value)
		  (s-value inter :final-feedback-obj nil))
		 (value
		  ;; if there are multiple feedback objects, value has
		  ;; the form (formula feedback-obj*). Otherwise it has
		  ;; the form (feedback-obj). 
		  (let ((feedback-formula (if (formula-p (car value))
					      (car value)
					      nil)))
		    ;; if there is a feedback formula, set the value to
		    ;; the list of feedback objects
		    (when feedback-formula
			  (setf value (cdr value)))
		    (dolist (obj value)
		      (create-feedback-obj obj slot-list inter)

		      ;; get rid of the inherited visibility formula, set
		      ;; the visible slot to nil, then install a formula 
		      ;; that sets the visible slot depending on whether 
		      ;; lapidary is in build or test mode (done by 
		      ;; save-value)--only do these operations if the
		      ;; formula does not already depend on lapidary's modes
		      (when (not (eq (g-formula-value (get-value obj :visible)
						      :menu-item)
				     :lapidary-mode))
			    (gg:cg-destroy-constraint obj :visible)
			    (s-value obj :visible (o-formula (gvl :obj-over)))
			    (save-value obj :visible t)
			    ;; indicate that this formula depends on the mode
			    ;; lapidary is in
			    (s-formula-value (get-value obj :visible) 
					     :menu-item
					     :lapidary-mode))

		      ;; set the :left and :top slots so that in build mode
		      ;; the feedback object appears in its current position
		      ;; and in test mode it appears as it will at 
		      ;; runtime--only do this if the formulas do not already
		      ;; depend on lapidary's modes
		      (when (not (eq (g-formula-value (get-value obj :left)
						      :menu-item)
				     :lapidary-mode))
			    (save-value obj :left (g-value obj :left))
			    (s-formula-value (get-value obj :left) :menu-item
					     :lapidary-mode))
		      (when (not (eq (g-formula-value (get-value obj :left)
						      :menu-item)
				     :lapidary-mode))
			    (save-value obj :top (g-value obj :top))
			    (s-formula-value (get-value obj :top) :menu-item
					     :lapidary-mode)))

		    (s-value inter :std-feedback-p nil)

		    ;; if there are multiple feedback objects, then there
		    ;; is a formula that determines which object to use. This
		    ;; formula must be installed on the feedback slot. 
		    ;; Otherwise, if there is only one feedback object, it 
		    ;; should be stored in the feedback slot
		    (if feedback-formula
		      (install-inter-formula inter (cons :final-feedback-obj
							 feedback-formula))
		      ;; check to see if the interim feedback is in the same
		      ;; aggregate as the interactor, and if it is create a
		      ;; formula to it. Otherwise, just store the interim 
		      ;; feedback object in the feedback-obj slot
		      (store-obj-in-inter-slot inter :final-feedback-obj 
					       (car value)))))
		 (t (s-value inter :final-feedback-obj nil)))
	   (s-value inter :final-feed-inuse nil)
	   (s-value inter :final-feed-avail nil))
	  (:obj-to-change
	   (cond ((null value)
		  (destroy-constraint inter :obj-to-change)
		  (s-value inter :obj-to-change nil))
		 ((formula-p value)
		  (install-inter-formula inter slot-value))
		 ;; check to see if the obj-to-change is in the same
		 ;; aggregate as the interactor, and if it is create a
		 ;; formula to it. Otherwise, just store the obj-to-change
		 ;; in the obj-to-change slot
		 (t 
		  (destroy-constraint inter :obj-to-change)
		  (store-obj-in-inter-slot inter :obj-to-change value))))
	  ((:how-set :grow-p :line-p :move-box-parms :grow-box-parms)
	   (cond ((formula-p value)
		  (install-inter-formula inter slot-value))
		 (t (s-value inter slot value))))
	  (:center-of-rotation
;	   (undo-save inter :attach-point)
	   (cond ((and (listp value) (numberp (car value)))
		  (s-value inter :center-of-rotation value)
		  (s-value inter :attach-point :pair))
		 ((formula-p value)
		  (install-inter-formula inter slot-value)
		  (s-value inter :attach-point :formula))
		 (t
		  (s-value inter :attach-point value)
		  (case value
			(:nw (o-formula (list (gvl :obj :left) 
					      (gvl :obj :top))))
			(:n (o-formula (list (opal:gv-center-x (gvl :obj)) 
					     (gvl :obj :top))))
			(:ne (o-formula (list (opal:gv-right (gvl :obj)) 
					      (gvl :obj :top))))
			(:w (o-formula (list (gvl :obj :left) 
					     (opal:gv-center-y (gvl :obj)))))
			(:c (o-formula (list (opal:gv-center-x (gvl :obj)) 
					     (opal:gv-center-y (gvl :obj)))))
			(:e (o-formula (list (opal:gv-right (gvl :obj))
					     (opal:gv-center-y (gvl :obj)))))
			(:sw (o-formula (list (gvl :obj :left) 
					      (opal:gv-bottom (gvl :obj)))))
			(:s (o-formula (list (opal:gv-center-x (gvl :obj)) 
					     (opal:gv-bottom (gvl :obj)))))
			(:se (o-formula (list (opal:gv-right (gvl :obj))
					      (opal:gv-bottom (gvl :obj)))))
			(:x1 (o-formula (list (gvl :obj :x1) (gvl :obj :y1))))
			(:line-c (o-formula (list (opal:gv-center-x (gvl :obj))
						  (opal:gv-center-y (gvl :obj)))))
			(:x2 (o-formula (list (gvl :obj :x2) 
					      (gvl :obj :y2))))))))
	  (:attach-point
	   (cond ((formula-p value)
		  (install-inter-formula inter slot-value))
		 (t
		  (s-value inter :attach-point value))))
	;; default-case: set the slot with its new value
	  (t (s-value inter slot value))))))

;;;=================================================================
;;;
;;; changes a menu-interactor to a button-interactor or vice
;;; versa
;;;
;;;=================================================================

(defun change-interactor (inter is-a)
  (let ((new-inter (create-instance nil is-a
		     (:start-where (g-value inter :start-where))
		     (:lapidary-p t))))
    (doslots (slot inter)
      (unless (or (eq slot :is-a) (eq slot :generated-running-where))
	(let ((value (get-local-value inter slot)))
	  (s-value new-inter slot (if (formula-p value)
				      (kr::copy-formula value)
				      value)))))
    new-inter))

;;;=================================================================
;;;
;;; changes the start-where of an interactor and determines if the
;;; interactor should be added to an aggregate. An interactor is
;;; added to an aggregate if:
;;;
;;; 1) the start-where references an aggregate
;;; 2) the start-where references an object that belongs to an
;;;      aggregate (an object is not considered to belong to an
;;;      aggregate if it belongs to lapidary's editor-agg
;;;
;;;=================================================================

(defun change-start-where (inter start-where)
  (declare (special inter-agg-query))
  (when (listp start-where)
    (let* ((ref-obj (second start-where))
	   (agg (cond ((is-a-p ref-obj opal:aggregate) ref-obj)
		      ((and (g-value ref-obj :parent)
			    (not (eq (g-value ref-obj :parent)
				     (g-value ref-obj :window :editor-agg))))
		       (g-value ref-obj :parent))
		      (t nil)))
	   type-restriction)
      ;; first set the start-where--if this function is called from
      ;; create-interactor, setting the start-where is redundant but
      ;; s-value is smart enough to avoid setting the slot twice
      (s-value inter :start-where start-where)

      ;; determine if this interactor should belong to an aggregate
      (when (and (not (formula-p start-where))
		 (listp start-where)
		 agg)
	    ;; ask the user if the interactor should be inserted into the
	    ;; aggregate
	    (s-value inter-agg-query :agg-to-insert-into agg)
	    (gilt:show-in-window inter-agg-query 
				 (round (- opal:*screen-width* 
					   (g-value inter-agg-query :window-width))
					2)
				 (round (- opal:*screen-height*
					   (g-value inter-agg-query :window-height))
					2)
				 t)

	    (when (inter:wait-interaction-complete)
		  (cond 
		   ((g-value inter :operates-on)
		    ;; only add the interactor to an aggregate if it does not
		    ;; already belong to the aggregate
		    (when (not (eq (g-value inter :operates-on) agg))
			  ;; the interactor already belongs to an aggregate so
			  ;; remove it from its old aggregate
			  ;; first delete any formulas that depend on the
			  ;; :operates-on slot
			  (dolist (formula (kr::get-dependents inter :operates-on))
				  (gg:cg-destroy-constraint (kr::on-schema formula)
							    (kr::on-slot formula)))
			  (opal:remove-interactor (g-value inter :operates-on)
						  inter)
			  (opal:add-interactor agg inter)))
		   (t
		    ;; interactor does not belong to an aggregate so add it
		    (opal:add-interactor agg inter)))

		  ;; store a formula in the interactor's :start-where slot that makes
		  ;; the aggregate part of the start where depend on the :operates-on slot
		  (setf start-where (copy-list start-where))
		  (setf (second start-where) 
			(if (is-a-p ref-obj opal:aggregate)
			    `(gvl :operates-on)
			  `(gvl :operates-on ,(g-value ref-obj :known-as))))
		  ;; if the start-where has a type restriction, shove a list
		  ;; modifier in front of the type restriction if it is a list
		  (when (and (setf type-restriction (member :type start-where))
			     (listp (second type-restriction)))
			(push 'list (second type-restriction)))
		  (s-value inter :start-where (formula `(list ,@start-where)))
		  ;; demand the value of the :start-where slot so that the
		  ;; proper dependencies are set up. This is necessary to ensure
		  ;; that when an object is renamed, the link in the start-where
		  ;; slot will be renamed
		  (g-value inter :start-where)

		  ;; check the :feedback-obj, final-feedback-obj, and obj-to-change
		  ;; slots to see if the objects they point to are in the same 
		  ;; aggregate as the interactor. if they are, create formulas
		  ;; to these objects
		  (dolist (slot '(:feedback-obj :final-feedback-obj :obj-to-change))
			  (store-obj-in-inter-slot inter slot (g-value inter slot))))))))

;;;=================================================================
;;;
;;; This function creates a new interactor and attaches it to
;;; the aggregadget in its start-where slot (if there is an aggregadget in
;;; its start-where slot)
;;;
;;;=================================================================

(defun create-interactor (gadget value)
  (declare (ignore value))
  (let* ((slot-value-list (symbol-value (g-value gadget :queue)))
	 (is-a (or (cdr (assoc :is-a slot-value-list)) 
		   (g-value gadget :inter)))
	 (start-where-pair (assoc :start-where slot-value-list))
	 (start-where (cdr start-where-pair))
	 new-inter)
    (when (and (not start-where-pair)
	       (eq (g-value is-a :start-where) :not-supplied))
      (lapidary-error "start-where must be supplied")
      (return-from create-interactor nil))
;    (reset-undo)
    (setf new-inter (create-instance nil is-a
		      (:lapidary-p t)
		      (:start-where start-where)))
    
    ;; name the interactor
    (name-lapidary-obj new-inter)

    ;; get the is-a and start-where slots off the slot-value-list 
    (setf slot-value-list (delete :is-a slot-value-list :key #'car))
    (setf slot-value-list (delete :start-where slot-value-list :key #'car))
    
    ;; save this interactor in case it must be deleted by an undo operation
    (push new-inter *undo-added-obj-list*)

    (s-value new-inter :window (o-formula (gv lapidary::*selection-info*
					      :window)))


    ;; determine if this interactor should belong to an aggregate
    (change-start-where new-inter start-where)

    ;; set the :active slot of the interactor so that the interactor
    ;; only operates when lapidary is in test mode and when the
    ;; :start-where, :feedback-obj, :final-feedback-obj, and :obj-to-change
    ;; have valid objects. If any of these slots do not have valid objects,
    ;; set the slots to nil *in the formula*. This should not have a
    ;; negative effect on the interactor, since the interactors only demand
    ;; these slots after it has been determined that the interactor is active.
    ;; It would be better to handle this destruction in the pre-set demon,
    ;; but the pre-set demon is not really viable yet.
    ;; It is more efficient to test for destroyed objects when an interactor
    ;; is about to go active, since there are several places in the lapidary
    ;; code where objects can be destroyed.
    (s-value new-inter :active 
	     (o-formula 
	      (when (not (gv lapidary::editor-menu :build-p))
		(let* ((active-p t)
		       (inter kr::*schema-self*)
		      ;; do not want to create dependencies, since :active
		      ;; slot should not be reevaluated just because one
		      ;; of these slots changes
		      (start-where (g-value inter :start-where)))
		  ;; test for destroyed objects in crucial slots
		  (when (and (listp start-where)
			     (not (schema-p (second start-where))))
			(s-value inter :start-where nil)
			(setf active-p nil))
		  (dolist (slot '(:feedback-obj :final-feedback-obj
						:obj-to-change))
			  (when (and (g-value inter slot)
				     (not (schema-p (g-value inter slot))))
				(s-value inter slot nil)
				(setf active-p nil)))
		  active-p))))
		  

    ;; save a 't' value for :active in the :save-values portion of the 
    ;; interactor
    (push (cons :active t) (g-value new-inter :save-values))

    ;; set the interactor's slots
    (set-interactor-slots new-inter slot-value-list)
    (setf (symbol-value (g-value gadget :queue)) nil)

    ;; make the interactor menu point to this interactor
    (s-value (g-value gadget :window) :inter new-inter)

    ;; make the window disappear
    (if (is-a-p new-inter inter:move-grow-interactor)
	(s-value (g-value gadget :window :parent :parent) :visible nil)
        (s-value (g-value gadget :window) :visible nil))

    new-inter))

(defun modify-interactor (gadget value)
  (declare (ignore value))
  (let* ((inter (g-value gadget :inter))
	 (slot-value-list (symbol-value (g-value gadget :queue)))
         (is-a (assoc :is-a slot-value-list))
	 (start-where-pair (assoc :start-where slot-value-list))
	 (start-where (cdr start-where-pair))
	 new-inter)

    ;; the basic interactors cannot be modified so make sure
    ;; that the user is not trying to modify one of them
    (when (garnet-inter-p inter)
      (lapidary-error "** cannot modify a garnet-defined interactor")
      (return-from modify-interactor))

    (when (and (choice-inter-p inter) is-a)
      (let ((operates-on (g-value inter :operates-on)))
	(setf new-inter (change-interactor inter (cdr is-a)))

	;; get the is-a slot off the slot-value-list if it is there
	(setf slot-value-list (delete :is-a slot-value-list :key #'car))

	;; save the new interactor in case it must be deleted by an undo
	;; operation
	(push new-inter *undo-added-obj-list*)

	;; save the changed interactor in case the user wants to undo
	;; this operation. 	
	(push inter *undo-deleted-obj-list*)
  
	;; remove the interactor from an aggregate if it belonged to one
	(when operates-on
	      (opal:remove-interactor operates-on inter))

	;; save the :active slot of the interactor and the aggregate
	;; it used to belong to
	(s-value inter :undo-slots (cons operates-on 
					 (get-value inter :active)))
	
	;; deactivate the interactor--not destroyed because of possibility
	;; of undo
	(gg:cg-destroy-constraint inter :active)
	(inter:change-active inter nil)

	(s-value gadget :inter new-inter)
	(setf inter new-inter)))

    ;; handle start-where here because it may involve changing the
    ;; aggregate the interactor belongs to
    (when start-where
	  (change-start-where inter start-where))

    (set-interactor-slots inter slot-value-list)
    (setf (symbol-value (g-value gadget :queue)) nil)

    ;; make the window disappear
    (if (is-a-p inter inter:move-grow-interactor)
	(s-value (g-value gadget :window :parent :parent) :visible nil)
        (s-value (g-value gadget :window) :visible nil))

))

(defun cancel-interactor-changes (gadget value)
  (declare (ignore value))
  (let ((inter (g-value gadget :inter)))
    ;; discard the set of changes
    (setf (symbol-value (g-value gadget :queue)) nil)
    ;; reset the interactor menu to the old values of the interactor
;    (reset-inter-menu inter)
    ;; make the window disappear
    (if (is-a-p inter inter:move-grow-interactor)
	(s-value (g-value gadget :window :parent :parent) :visible nil)
        (s-value (g-value gadget :window) :visible nil))
))

(defun destroy-interactor (gadget value)
  (declare (ignore value))
  (let* ((inter (g-value gadget :inter))
	 (parent-inter (car (g-value inter :is-a))))
    ;; the basic interactors cannot be destroyed so make sure
    ;; that the user is not trying to destroy one of them
    (when (garnet-inter-p inter)
      (lapidary-error "** cannot destroy a garnet-defined interactor")
      (return-from destroy-interactor))

    ;; store the interactor's :is-a parent in the interactor menu
    (s-value (g-value gadget :window) :inter parent-inter)

    ;; reset the interactor menu so that it displays the information
    ;; associated with the interactor's :is-a parent
    (reset-inter-menu parent-inter)

    ;; reset the slot-value list to nil
    (setf (symbol-value (g-value gadget :queue)) nil)

    ;; destroy the interactor
    (opal:destroy inter)
    ;; make the window disappear
    (if (is-a-p parent-inter inter:move-grow-interactor)
	(s-value (g-value gadget :window :parent :parent) :visible nil)
        (s-value (g-value gadget :window) :visible nil))
))

(defun write-interactor (gadget value)
  (let ((inter (g-value gadget :inter)))
    ;; first create or modify the interactor, then write it out
    (if (garnet-inter-p inter)
	(setf inter (create-interactor gadget value))
	(modify-interactor gadget value))
    (s-value save-file :inter inter)
    ;; make the window disappear
    (if (is-a-p inter inter:move-grow-interactor)
	(s-value (g-value gadget :window :parent :parent) :visible nil)
        (s-value (g-value gadget :window) :visible nil))

    (show-save-dialog nil nil)))

(defun display-interactor-properties (gadget value)
  (declare (ignore value))
  (multiple-value-bind (left top)
       (get-coordinates gadget)
       (gg:c32 (g-value gadget :inter) nil :left left :top top)))

(defun read-interactor (gadget value)
  (declare (ignore gadget value))
  (let (file)
    (setf file (lapidary-prompt-for-input 
		"What is the name of the file that contains this interactor? "))
    (load file)
    ;; go through the list of created objects until we find an interactor and
    ;; then instantiate the proper interactor menu with information from this
    ;; interactor
    (dolist (obj *created-instances*)
      (when (is-a-p obj inter:interactor)
	(init-inter-menu obj)
	(return-from read-interactor)))
    (lapidary-error "This file did not contain any interactors")))
