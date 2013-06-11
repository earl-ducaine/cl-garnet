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
#|
========================================================================
Change log:
         8/24/92 Andrew Mickish - Removed declaration of agg-name in
                   Make-Aggregate.
         5/22/92 Brad Vander Zanden - fixed ungroup so that it handles
                   single objects in a-formula-depends-on
========================================================================
|#
;;; This file provides the functions that handle aggregadgets in Lapidary
;;; Lapidary supports the operations "make aggregate", "ungroup", 
;;; "send-to-back", and "bring-to-front". Ungroup can only be applied
;;; to top-level aggregates. This simplifies the handling of link slots,
;;; as explained below.
;;;
;;; Handling of constraints: In lapidary, constraints alway reference
;;;   other objects indirectly through links. If a constraint references
;;;   an object in the same aggregadget, the link contains a formula that
;;;   traverses the aggregate hierarchy to find the object. If a constraint
;;;   references an object outside the aggregadget, the link directly points
;;;   at the object (no formula is involved). 
;;;
;;; Rationale for ungroup: By only permitting ungroup on top-level
;;;   aggregadgets, we can avoid having to translate formulas, which
;;;   is messy and error-prone. Ungroup changes the aggregate hierarchy,
;;;   and therefore, formulas which traverse the aggregate hierarchy must
;;;   be translated. The one exception is if the top-level aggregadget
;;;   is destroyed, since formulas that went through the componets of this
;;;   aggregadget should be destroyed rather than translated (the components 
;;;   are no longer part of the same aggregadget, and therefore, links should 
;;;   point directly at these objects). Destroying formulas is manageable,
;;;   so ungroup is permitted at the top level (indeed, it is necessary to
;;;   provide some sort of ungroup!).
;;;

(in-package "LAPIDARY")
      
;;; =====================================================
;;; add a list of selected objects to an aggregate. if
;;; the aggregate does not already exist, create it. the
;;; objects must be at the top-level.
;;; =====================================================

(defun make-aggregate (&optional selected-objects)
  
  (when (not selected-objects)
	(setf selected-objects (remove-duplicates
				(g-value *selection-info* :selected))))

  (when (not selected-objects)
	(lapidary-error "You must have one or more objects selected
before creating an aggregate")
	(return-from make-aggregate))

  (let* (new-agg
	 (kr::*constants-disabled* t) 
	 (current-agg (g-value (car selected-objects) :parent))
	 (current-agg-components 
	  (copy-list (g-value current-agg :components))))
    
    ; determine if the selected-objects all belong to the top-level aggregate.
    ; if they don't, print an error message and return
  
    (if (not (top-level-aggregate-p selected-objects))
	(return-from make-aggregate))
    
    ;; compute the bounding box of the new aggregate--while computing the
    ;; bounding box, determine if all of its components have formulas in
    ;; the :left and :top slots. If they do, then leave the default formula
    ;; for computing the :left and :top slots in the aggregate
    (let ((left most-positive-fixnum)
	  (right 0)
	  (top most-positive-fixnum)
	  (bottom 0)
	  (formula-left-p t)
	  (formula-top-p t))
      (dolist (obj selected-objects)
	      (when (not (formula-p (get-value obj :left)))
		    (setf formula-left-p nil))
	      (when (not (formula-p (get-value obj :top)))
		    (setf formula-top-p nil))
	      (when (< (g-value obj :left) left)
		    (setf left (g-value obj :left)))
	      (when (> (opal:right obj) right)
		    (setf right (opal:right obj)))
	      (when (< (g-value obj :top) top)
		    (setf top (g-value obj :top)))
	      (when (> (opal:bottom obj) bottom)
		    (setf bottom (opal:bottom obj))))
	      
      ;; create the aggregate
      (with-constants-disabled
       (setf new-agg
	     (cond ((and formula-left-p formula-top-p)
		    (create-instance nil opal:aggregadget))
		   (formula-left-p
		    (create-instance nil opal:aggregadget
				     (:top top)))
		   (formula-top-p
		    (create-instance nil opal:aggregadget
				     (:left left)))
		   (t
		    (create-instance nil opal:aggregadget
		      (:left left)
		      (:top top)))))))
;		      (:width (- right left))
;		      (:height (- bottom top)))))

    ;; generate a name for the new object
    (name-lapidary-obj new-agg)

    ; add the aggregate to the children's previous parent
    (opal:add-component current-agg new-agg)

    ;; add the selected objects to the new aggregate in the order in
    ;; which they appear in the old aggregate.
    (dolist (item current-agg-components)
      (when (member item selected-objects)
	(lapidary-add-component new-agg item current-agg)))

    ;; if link slots in the selected objects point to each other, insert
    ;; formulas in the link slots that will reach these objects by 
    ;; traversing the aggregate hierarchy
    (install-formulas-in-links (get-all-objects selected-objects))

    ; deselect the selected items and select the new aggregate
    (deselect-objects)
    (primary-select new-agg)
    
    new-agg))

;;; ===================================================== 
;;; if an object's left, top, width, or height slots
;;; are unconstrained, constrain them to the aggregate's
;;; slots so that they maintain their current offsets
;;; with respect to the aggregate's left, top, width
;;; and height slots. also recompute the bounding boxes 
;;; of the components' old and new aggregates 
;;; ===================================================== 

(defun attach-agg-constraints (aggregate item)
  ;; only constrain the components if their new aggregate is
  ;; not the editor aggregate
  (when (not (eq aggregate (g-value aggregate :window :editor-agg)))
    (if (is-a-line-p item)
	(progn
	  (when (not (formula-p (get-value item :x1)))
		(s-value item :x1-over (o-formula (gvl :parent)))
		(s-value item :y1-over (o-formula (gvl :parent)))
		(s-value item :x1-offset (- (g-value item :x1)
					    (g-value aggregate :left)))
		(s-value item :y1-offset (- (g-value item :y1)
					    (g-value aggregate :top)))
		(s-value item :x1 (kr::make-into-o-formula
				   (formula gg::*x1-to-box-left* 0
					   (:links (list :x1-over))
					   (:offset :x1-offset))))
		(s-value item :y1 (kr::make-into-o-formula
				   (formula gg::*y1-to-box-top* 0
					   (:links (list :y1-over))
					   (:offset :y1-offset)))))

	  (when (not (formula-p (get-value item :x2)))
		(s-value item :x2-over (o-formula (gvl :parent)))
		(s-value item :y2-over (o-formula (gvl :parent)))
		(s-value item :x2-offset (- (g-value item :x2)
					    (g-value aggregate :left)))
		(s-value item :y2-offset (- (g-value item :y2)
					    (g-value aggregate :top)))
		(s-value item :x2 (kr::make-into-o-formula
				   (formula gg::*x2-to-box-left* 0
					   (:links (list :x2-over))
					   (:offset :x2-offset))))
		(s-value item :y2 (kr::make-into-o-formula
				   (formula gg::*y2-to-box-top* 0
					   (:links (list :y2-over))
					   (:offset :y2-offset))))))

      (progn
	(when (not (formula-p (get-value item :left)))
	      (s-value item :left-over (o-formula (gvl :parent)))
	      (s-value item :left-offset
		       (- (g-value item :left) (g-value aggregate :left)))
	      (s-value item :left (kr::make-into-o-formula
				   (formula gg::*left-inside-formula* 0
					   (:links (list :left-over))
					   (:offset :left-offset)))))
	(when (not (formula-p (get-value item :top)))
	      (s-value item :top-over (o-formula (gvl :parent)))
	      (s-value item :top-offset
		       (- (g-value item :top) (g-value aggregate :top)))
	      (s-value item :top (kr::make-into-o-formula
				  (formula gg::*top-inside-formula* 0
					  (:links (list :top-over))
					  (:offset :top-offset)))))))))
#|
	(when (not (formula-p (get-value item :width)))
	      (s-value item :width-over (formula `(gvl :parent)))
	      (s-value item :width-scale 1.0)
	      (s-value item :width-difference
		       (- (g-value item :width) (g-value aggregate :width)))
	      (s-value item :width (formula gg::*width-formula*)))
	(when (not (formula-p (get-value item :height)))
	      (s-value item :height-over (formula `(gvl :parent)))
	      (s-value item :height-scale 1)
	      (s-value item :height-difference
		       (- (g-value item :height) (g-value aggregate :height)))
	      (s-value item :height (formula gg::*height-formula*)))))))
|#
;;; ===================================================== 
;;; remove the components from an aggregate and destroy
;;; the aggregate
;;; =====================================================

(defun ungroup (&optional (aggregate nil))

  ; determine the aggregates that must be destroyed
  (let ((selected-objects (if aggregate
			      (cons aggregate nil)
			      (g-value *selection-info* :selected)))
	depends-on)
    
    (when (not aggregate) (deselect-objects))
    (dolist (aggregate selected-objects)
      (cond ((is-a-p aggregate opal:aggregate)
	     (if (eq (g-value aggregate :window :editor-agg)
		     (g-value aggregate :parent))
		 
		 (let ((new-agg (g-value aggregate :parent))
		       (components (copy-list 
				    (g-value aggregate :components))))
		 
		   ;; first destroy all instances of this aggregate
		   (dovalues (inst aggregate :is-a-inv :local t)
			     (destroy-lapidary-object inst))

		   ;; smash any formulas that are in link slots that 
		   ;; point to objects which will no longer be in the
		   ;; same aggregadget. these formulas must go through
		   ;; the :parent slot of one of the top-level
		   ;; components--note this would be automatically
		   ;; accomplished by destroy-lapidary-object, but
		   ;; lapidary-add-component is executed first and it
		   ;; needs to have these link slots cleaned out
		   (dolist (item components)
		     (dolist (formula (kr::get-dependents item :parent))
			   (gg:cg-destroy-constraint (kr::on-schema formula)
					       (kr::on-slot formula)))
		   
		   ;; smash any formulas in the position or size slots that
		   ;; depend on the parent
		     (dolist (slot (if (is-a-line-p item)
				     '(:x1 :x2 :y1 :y2)
				     '(:left :top :width :height)))
			     (let ((value (get-value item slot)))
			       (when (and (formula-p value)
					  (or (eq aggregate
						  (setf depends-on 
							(kr::a-formula-depends-on value)))
					      (and (listp depends-on)
						   (member aggregate depends-on))))
				   (gg:cg-destroy-constraint item slot)))))
		   
		   ;; remove all formulas in the aggregate's interactors
		   ;; that depend on the aggregate
		   (dolist (item (g-value aggregate :behaviors))
		     (dolist (formula (kr::get-dependents item :operates-on))
			   (gg:cg-destroy-constraint (kr::on-schema formula)
					       (kr::on-slot formula))))


		   ;; now move the components to the top-level editor aggregate
		   (dolist (item components)
		     (lapidary-add-component new-agg item aggregate
					     :behind aggregate))

		   ;; destroy the aggregate
		   (destroy-lapidary-object aggregate))
	       ; else 
	       ;; signal an error--object is not a top-level aggregate and thus
	       ;; cannot be ungrouped
	       (format t "~% object ~S is not a top-level aggregate and thus cannot be ungrouped" aggregate)))
	    (t
	     (lapidary-beeps 1)
	     (format t "~% object ~S is not an aggregate--it cannot be ungrouped ~%"
		     aggregate))))))
					 

;;; ===================================================== 
;;; bring a component to the front or back of an aggregate
;;; =====================================================

(defun change-covering-order (where)

  (let ((selected-objects (g-value *selection-info* :selected)))
    (when (null selected-objects)
	  (lapidary-error "You must first select one or more objects")
	  (return-from change-covering-order))
    
    (let* ((aggregate (g-value (car selected-objects) :parent))
	   (aggregate-components (copy-list (g-value aggregate :components))))
    
    ; if there is only one selected object, we can simply move the
    ; object to the front of the aggregate; otherwise we have to
    ; move objects in the order in which they appear in the aggregate
    
    (cond ((null (cdr selected-objects))
	     (opal:move-component aggregate (car selected-objects) 
				  :where where))
	  
	  (t
	   ; determine if the selected-objects all belong to the same aggregate.
	   ; if they don't, print an error message and return
	   
	   (when (not (same-aggregate-p aggregate selected-objects))
	       (return-from change-covering-order))
	   
	   ; send the selected objects to the front of the aggregate in the
	   ; order that they appear in the aggregate
	   
	   (dolist (item (if (eq where :front)
			     aggregate-components
			     (reverse aggregate-components)))
	     (when (member item selected-objects)
	       (opal:move-component aggregate item :where where))))))))

#|
;;; =====================================================
;;; add a list of selected objects to an aggregate.
;;; =====================================================

(defun lapidary-add-components ()
  
    (let* ((s-selected (g-value *selection-info* :s-selected))
	   (new-agg (car s-selected))
           (selected-objects (g-value *selection-info* :p-selected))
	   (current-agg (g-value (car selected-objects) :parent))
	   (current-agg-components 
	    (copy-list (g-value current-agg :components))))
      
      ; if the secondary selection is not an aggregate, raise an error
      ; condition

      (when (not (is-a-p new-agg opal:aggregate))
	(lapidary-beeps 1)
	(format t "~% the secondary selection must be an aggregate but is not ~%")
	(return-from lapidary-add-components))

      ; determine if the selected-objects all belong to the top-level 
      ; aggregate. if they don't, print an error message and return
      
      (if (not (top-level-aggregate-p selected-objects))
	  (return-from lapidary-add-components))
    
      ; if there
      ; are no secondary selections or more than one, raise an error 
      ; condition, because in this case, we don't know which aggregate
      ; to use
      
      (when (not (eql (list-length s-selected) 1))
	(lapidary-beeps 1)
	(format t "~% please increase or decrease the number of
	secondary selections so that there is only one secondary
	selection ~%")
	(return-from lapidary-add-components))

      ; add the selected objects to the new aggregate in the order in
      ; which they appear in the old aggregate. objects that appear
      ; before the new aggregate should be placed at the beginning of
      ; the new aggregate; objects that appear after the new aggregate
      ; should be placed at the end of the new aggregate. the same
      ; strategy is used in changing the objects' positions in the
      ; selection order

      (let ((where :back))
	(dolist (item current-agg-components)
	  (cond ((member item selected-objects)
		 (lapidary-add-component new-agg item :where where))
		((eql item new-agg)
		 (setf where :front)))))

      ;; if link slots in either the original or new components of the
      ;; aggregate point to each other, insert
      ;; formulas in the link slots that will reach these objects by 
      ;; traversing the aggregate hierarchy
      (install-formulas-in-links (get-all-objects 
				  (g-value new-agg :components)))

      ; deselect the components but leave the aggregate selected
      (primary-deselect-objects :none)
      new-agg))

;;; =====================================================
;;; delete a group of components from an aggregate and
;;; remove any constraints that these components may
;;; have had that depended on the aggregate.
;;; ===================================================== 

(defun lapidary-remove-components (components)
  (let ((aggregate (g-value (car components) :parent)))
    
    ; it is an error to try to remove an object from the top level
    ; aggregate
    
    (when (eql aggregate (g-value aggregate :window :editor-agg))
      (lapidary-beeps 1)
      (format t "~% cannot remove objects from the top-level aggregate ~%")
      (return-from lapidary-remove-components))
    
    ; determine if the components all belong to the same aggregate.
    ; if they don't, print an error message and return
    
    (if (and (cdr components)
	     (not (same-aggregate-p aggregate components)))
	(return-from lapidary-remove-components))

    ; find the top-level aggregadget that these items belong to. the
    ; removed components will be placed behind this aggregadget
    (let ((top-level-agg aggregate)
	  (editor-agg (g-value aggregate :window :editor-agg)))
      (loop
	(when (eq editor-agg (g-value top-level-agg :parent))
	  (return))
	(setf top-level-agg (g-value top-level-agg :parent)))

      ; if there is only one component, we can simply remove the
      ; object from the aggregate; otherwise we have to
      ; remove objects in the order in which they appear in the aggregate.
  
      (cond ((null (cdr components))
	     (lapidary-add-component editor-agg (car components)
				      :behind top-level-agg))
	    (t
	 
	     ; remove the objects from the aggregate in the order in which
	     ; they appear in the aggregate 
	 
	     (let ((aggregate-components 
		    (copy-list (g-value aggregate :components))))
	       (dolist (item (reverse aggregate-components))
		 (when (member item components)
		   (lapidary-add-component editor-agg item 
					   :behind top-level-agg))))))

      ; if the aggregate from which the components were removed is now 
      ; empty, destroy it
      (when (not (g-value aggregate :components))
	    ;; destroy the link to this aggregate in the aggregate's parent, 
	    ;; then destroy the aggregate
	    (destroy-slot (g-local-value aggregate :parent)
			  (g-local-value aggregate :known-as))
	    (opal:destroy aggregate)))
))
|#

;;; ===================================================== 
;;; determine if a group of items belong to the top-level
;;; aggregate. If they don't, beep three times and print
;;; an error message.
;;; =====================================================

(defun top-level-aggregate-p (objects)
  (let ((agg (g-value (car objects) :window :editor-agg)))
    (cond ((not (subsetp objects (g-value agg :components)))
	 (lapidary-error "The selected objects must all
belong to the top-level aggregate but they do not")
	 nil)
	(t t))))

;;; ===================================================== 
;;; determine if a group of items belong to the same
;;; aggregate. If they don't, beep three times and print
;;; an error message.
;;; =====================================================

(defun same-aggregate-p (aggregate objects)
  (cond ((not (subsetp objects (g-value aggregate :components)))
	 (lapidary-error "The selected objects must all
belong to the same aggregate but they do not")
	 nil)
	(t t)))

;;; =========================================================
;;; move a component from the aggregate it currently
;;; belongs to to the specified aggregate. The object's old
;;; aggregate must be passed in because if the object
;;; is an instance of a prototype, and the prototype and
;;; the object both are being added to the new aggregate, and
;;; the prototype is removed from the old aggregate before
;;; the object, than the object will be removed when the
;;; prototype is removed and thus will no longer belong
;;; to the old aggregate. Thus to ensure that we know which
;;; aggregate an object belonged to, we must pass in the
;;; old aggregate.
;;; =========================================================

(defun lapidary-add-component (agg item old-agg &key (where nil) (behind nil))

  ;; remove all slots and formulas in the item that reference
  ;; the item's aggregate
  (remove-agg-formulas old-agg item)

  (when (g-local-value item :parent)
	(opal:remove-component old-agg item))

  ;; add the item to its new aggregate. this must be done before
  ;; constraints that tie it to the aggregate are installed, since
  ;; the constraints may fire immediately, and the link through
  ;; :parent must be set

  (cond (where (opal:add-component agg item :where where))
	(behind (opal:add-component agg item :behind behind))
	(t (opal:add-component agg item)))

  ;; attach constraints to the items that cause them to resize or
  ;; reposition themselves when their new aggregate is resized or 
  ;; repositioned
  (attach-agg-constraints agg item))


;;; =========================================================
;;; remove any links in item that point to agg (the links 
;;; old parent)
;;; =========================================================

(defun remove-agg-formulas (agg item)
  (doslots (slot item)
	   (when (and (eq (g-cached-value item slot) agg)
		      (not (eq slot :parent)))
		 (destroy-slot item slot))))


;;; =================================================================
;;; return a list of all components in the passed objects, including 
;;; the objects themselves
;;; =================================================================
(defun get-all-objects (objs)
  (let (components)
    (dolist (obj objs)
	    (when (is-a-p obj opal:aggregate)
	        (setf components (append components (transitive-closure obj)))
	        ;; get interactors
	        (setf components (append (g-value obj :behaviors) components)))
	    (push obj components))
    components))

;;; =================================================================
;;; Examine the link slots in each of the passed objects. If a link
;;; slot references another of the passed objects, place a formula
;;; in the slot that traverses the aggregate hierarchy in order to 
;;; reach the object it references
;;; =================================================================

(defun install-formulas-in-links (objs)
  (declare (special save-time-do-not-dump-slots))
  (dolist (obj objs)
    ;; find all link slots and establish a formula so that they still
    ;; refer to the object they currently point at. A link slot is a
    ;; slot that contains a view object, is not on the 
    ;; save-time-do-not-dump-slots list, and is not a :parent slot
    ;; or a link slot for an aggregadget
    
    (let ((links (g-value obj :links)))
      (doslots (slot obj)
       (let ((value (get-value obj slot)))
	 ;; determine if value is atomic or a formula. if it's a
	 ;; formula, it could be a by-demo formula. if the formula
	 ;; contains an object that has to be converted, the object
	 ;; will be found in either the true or false branches of
	 ;; the if statement. 
	 (when (or (member slot '(:left-over :top-over :width-over :height-over
				  :x1-over :x2-over :y1-over :y2-over))
		   (member slot links))
	       (if (formula-p value)
		   (progn
		     (let ((form (extract-formula value)))
		       (when (eq (first form) 'if)
			     (let ((true-expr (third form))
				   (false-expr (fourth form))
				   changed-p)
			       (when (member true-expr objs)
				     (setf (third form)
					   `(gvl ,@(gilt:make-path obj true-expr)))
				     (setf changed-p t))
			       (when (member false-expr objs)
				     (setf (fourth form)
					   `(gvl ,@(gilt:make-path obj false-expr)))
				     (setf changed-p t))
			       (when changed-p
				     (change-formula obj slot form))))))
		   (when (member value objs)
		       (s-value obj slot 
				(formula `(gvl ,@(gilt:make-path obj value))))
		       ;; immediately evaluate the formula to set up the
		       ;; dependencies
		       (g-value obj slot)))))))))

