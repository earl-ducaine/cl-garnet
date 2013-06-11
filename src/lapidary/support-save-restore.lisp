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
;;; lapidary's functions for save and restore
;;;

(in-package "LAPIDARY")

;;***********************************************************************
;;
;;  This program takes a list of garnet objects and performs slot 
;;  name substitutions and simplifies formulas to improve efficiency.
;;
;;  For example:
;;
;;   (:TOP-OVER ,(formula `(GVL :PARENT)))
;;   (:TOP-OFFSET 0)
;;
;;   (:TOP ,(o-formula (+ (GVL :TOP-OVER :TOP ) (GVL :TOP-OFFSET)) 33))
;;  
;; would be replaced by
;;
;;   (:TOP ,(formula `(+ (GVL :PARENT TOP ) 33))
;;
;;***********************************************************************
 

(defvar delete-slot-list '(:p-feedback-obj        ;; A list of slots we
                           :s-feedback-obj        ;; wish to delete
                           :height-scale
                           :width-scale
                           :height-over
                           :left-over
                           :top-over
                           :width-over
			   :x1-over :x2-over :y1-over :y2-over
			   :x1-offset :x2-offset :y1-offset :y2-offset
                           :left-offset
                           :top-offset
                           :box
			   :mark
                           :height-difference
                           :width-difference))



;; The following function extracts the value from a formula.

(defun extract-formula (value)
  (kr::a-formula-lambda value))

;;; 
;;; takes an expression and replaces all occurrences of (gvl :link :slot)
;;; with (fct pattern :slot). For example, if the pattern is (:parent :label),
;;; and the fct is gvl, the pattern (gvl :link :slot) will be replaced with
;;; (gvl :parent :label :slot)
;;;

(defun parse-expression (form pattern link &optional (fct 'gvl))
  (cond ((atom form) nil)
	(t (cond ((and (eq 'gvl (car form))
		       (= (list-length form) 3)
		       (eq (second form) link))
		  (setf (car form) fct)
		  (setf (cdr form) (append pattern (cddr form))))
		 (t
		  (parse-expression (car form) pattern link fct)
		  (parse-expression (cdr form) pattern link fct))))))

;; The following function substitutes expressions contained in the
;; link-, offset-, and scale-slots into the expression in slot. For
;; example, given:
;;
;;  :left (cond ((gvl :interim-selected) (+ (gvl :left-over :left)
;;                                          (gvl :left-offset)))
;;              (t (+ (opal:gv-right (gvl :left-over)) (gvl :left-offset))))
;;
;;  :left-over (gvl :parent :frame)
;;
;;  :left-offset (cond ((gvl :interim-selected) 10)
;;                     ((gvl :selected) 20)
;;                     (t 30))
;;
;; substitute expression would return:
;;
;;  :left (cond ((gvl :interim-selected) (+ (gvl :parent :frame :left) 10))
;;              ((gvl :selected) (+ (opal:gv-right (gvl :parent :frame)) 20))
;;              (t (+ (opal:gv-right (gvl :parent :frame)) 30)))

(defun substitute-expr (obj slot)

  ;; only process formulas that are not inherited
  (when (null (has-slot-p obj slot))
	(return-from substitute-expr))

  ;; pattern substitution required only on formulas
  (when (formula-p (get-value obj slot))
    (let* ((value (get-value obj slot))
	  (links (g-formula-value value :links))
	  (offset-slot (g-formula-value value :offset))
	  (scale-slot (g-formula-value value :scale))
	expr-list cond-expr link-value slot-value-list)

      (setf value (extract-formula value))

      ;; if formula contained an atomic expression, get out of here
      (when (not (listp value))
	    (return-from substitute-expr))
      ;; extract expressions from pattern slots
      (dolist (link links)
	(setf link-value (get-value obj link))
	(when (formula-p link-value) 
	      (setf link-value (extract-formula link-value))
	      ;; cannot substitute (gv :self) into an expression but
	      ;; (gvl) is ok
	      (when (equal link-value '(gv :self))
		    (setf link-value '(gvl))))
	(push (cons link link-value) slot-value-list))

      (when offset-slot 
	  (push (cons offset-slot
		      (if (formula-p (get-value obj offset-slot))
			  (extract-formula (get-value obj offset-slot))
			  (get-value obj offset-slot)))
		slot-value-list))

      (when scale-slot 
	  (push (cons scale-slot
		      (if (formula-p (get-value obj scale-slot))
			  (extract-formula (get-value obj scale-slot))
			  (get-value obj scale-slot)))
		slot-value-list))

	  ;; if the slot's formula contains a conditional, store each of
	  ;; of the branches in the expression list; otherwise store the
	  ;; expression with a t affixed to it, since the support slots may
	  ;; have conditional expressions
	  (setf expr-list (if (eql 'cond (car value))
			      (copy-tree (cdr value))
			      `((t ,(copy-tree value)))))

	  ;; perform the pattern replacement. for each support slot, extract
	  ;; its expression and process each of its branches. if it is not
	  ;; a conditional, it will have only one branch, which is assumed
	  ;; to have a condition of t. For each branch, attempt to match
	  ;; the branch's condition against the condition of one of the
	  ;; branches in the expression list. If this succeeds, substitute
	  ;; the expression part of the branch into the expression part of
	  ;; the matched branch. For example, the interim-
	  ;; selected branch of the :left-offset slot shown in the comment's
	  ;; example at the beginning of this function will match the 
	  ;; interim-selected branch in :left's expression list:
	  ;;    ((gvl :interim-selected) 10)
	  ;; matches
	  ;;    ((gvl :interim-selected) (+ (gvl :parent :frame :left) 
	  ;;                                (gvl :left-offset)))
	  ;; Thus 10 will be substituted for (gvl :left-offset) to produce:
	  ;;    ((gvl :interim-selected) (+ (gvl :parent :frame :left) 10))
	  ;;
	  ;; If the branch cannot be matched against any of the branches in
	  ;; the expression list, find the t branch in the expression list and
	  ;; create a new branch by copying the t branch, replacing the t
	  ;; condition with the condition of the pattern branch, and 
	  ;; substituting the expression part of the pattern branch into the
	  ;; copied branch. For example, the selected branch of the 
	  ;; :left-offset slot does not match any of :left's branches, so
	  ;; we create a new branch from :left's t branch:
	  ;;   ((gvl :selected) 20)
	  ;; modifies a copy of
	  ;;   (t (+ (opal:gv-right (gvl :parent :frame)) (gvl :left-offset)))
	  ;; to become
	  ;;   ((gvl :selected) (+ (opal:gv-right (gvl :parent :frame)) 20)).
	  ;;
	  ;; Finally, if the pattern slot is a link slot and the value is
	  ;; an opal object, ask the user if this slot should be a link slot.
	  ;; if the answer is yes, keep the slot and do not do any pattern
	  ;; replacement.
	  (dolist (slot-value-pair slot-value-list)
	     (let ((slot-value (cdr slot-value-pair))
		   (support-slot (car slot-value-pair)))

	       ;; if the slot is a link slot and it points to an object
	       ;; outside the current aggregadget, do nothing (in the 
	       ;; future we may want to do something). if the slot-value 
	       ;; is a parameter or points to an object in the aggregadget,
	       ;; it will be a formula and therefore a list. thus the
	       ;; test is for a non-list slot value
	       (cond ((and (member support-slot links)
			   (not (listp slot-value)))
		      (dolist (cond-expr expr-list)
			      (nsubst slot-value `(gvl ,support-slot) 
				      (second cond-expr) :test #'equal)
			      (parse-expression (second cond-expr)
						slot-value
						support-slot
						'kr:gv)))
		     ;; if this slot has multiple branches because it is a 
		     ;; conditional, process each of its branches
		     ((and (listp slot-value) (eql (car slot-value) 'cond))
		      (dolist (slot-expr (cdr slot-value))
			      ; if this branch is the default branch, 
			      ; substitute its pattern into every branch.
			      ; since all other branches have already been
			      ; processed and removed references to the slot, 
			      ; this pattern will only be 
			      ; substituted into the branches that have not
			      ; been processed
			      (cond ((eql (car slot-expr) 't) 
				     (dolist (cond-expr expr-list)
				       (nsubst (second slot-expr) 
					       `(gvl ,support-slot) 
					       (second cond-expr) :test #'equal)
				       ; replace all patterns of the form
				       ; (gvl support-slot slot)
				       (parse-expression (second cond-expr)
							 (if (listp (second slot-expr))
							     (cdr (second slot-expr))
							     (second slot-expr))
							 support-slot)))
				    ; try to match the branch against an
				    ; existing branch
				    ((setf cond-expr 
					   (assoc (car slot-expr) 
						  expr-list :test #'equal))
				     (nsubst (second slot-expr) 
					     `(gvl ,support-slot) 
					     (second cond-expr) :test #'equal)
				     ; replace all patterns of the form
				     ; (gvl support-slot slot)
				     (parse-expression (second cond-expr)
						       (if (listp (second slot-expr))
							   (cdr (second slot-expr))
							   (second slot-expr))
						       support-slot))
				    ; create a new branch
				    (t
				     (setf cond-expr 
					   (copy-tree (assoc 't expr-list)))
				     (setf (car cond-expr) (car slot-expr))
				     (nsubst (second slot-expr) 
					     `(gvl ,support-slot) 
					     (second cond-expr) :test #'equal)
				     ; replace all patterns of the form
				     ; (gvl support-slot slot)
				     (parse-expression (second cond-expr)
						       (if (listp (second slot-expr))
							   (cdr (second slot-expr))
							   (second slot-expr))
						       support-slot)
				     (push cond-expr expr-list)))))
		     ;; there was no conditional so this slot has only one
		     ;; branch
		     (t
		      (dolist (cond-expr expr-list)
			      (nsubst slot-value `(gvl ,support-slot) 
				      (second cond-expr) :test #'equal)
			      (parse-expression (second cond-expr)
						(if (listp slot-value)
						    (cdr slot-value)
						    slot-value)
						support-slot))))))

	  ;; destroy the support slots associated with this formula
	  ;; and destroy the meta slots that store the support slot
	  ;; information
	  (gg:destroy-constraint-support-slots obj (get-value obj slot) t)

	  ;; put the formula back together
	  (let ((new-form (if (null (cdr expr-list))
			      (second (car expr-list))
			      (push 'cond expr-list))))
	    (when (not (equal new-form value))
		  (s-value obj slot 
			   (kr::make-into-o-formula
			    (formula (simplify-expression new-form)))))))))




;; Note never use global variables in a recursive function!!!
;; That is, always use let......
;;
;; The following function is used to simplify two types of identities.
;; Case I: if the operation is addition, then remove all operands 
;;   that are zero.  
;; Case II: if the operation is multiplication remove all operands
;;   that are 1 or 1.0

(defun simplify-expression (expr) 
  (if (atom expr) (return-from simplify-expression expr))
  (let (new-expr) 
    (cond ((equal (car expr) '+) 
             (setf new-expr (mapcar #'simplify-expression expr))
             (setf new-expr (remove 0 new-expr)) 
             (if (equal (third new-expr) nil)
               (return-from simplify-expression (second new-expr))))

          ((equal (car expr) '*) 
             (setf new-expr (mapcar #'simplify-expression expr))
             (setf new-expr (remove 1.0 new-expr)) 
             (setf new-expr (remove 1 new-expr)) 
             (if (equal (third new-expr) nil)
               (return-from simplify-expression (second new-expr)))) 

          (t 
             (setf new-expr (mapcar #'simplify-expression expr))))

    (return-from simplify-expression new-expr)
  ) 
)



;; Examines each slot of every object.  When finished all slots
;; in the delete-slot-list are removed. 

(defun process-an-obj (obj gadget)
  (declare (special *top-level-obj*))
  (when (and (is-a-p obj opal:aggregate)
	     (not (is-a-line-p obj)))
	(dovalues (child obj :components)
		  (process-an-obj child gadget)))
  
  ;; replace references to links and offsets in formulas with their 
  ;; actual values--then delete the link and offset slots
  (if (is-a-line-p obj)
      (dolist (slot '(:x1 :y1 :x2 :y2))
	(substitute-expr obj slot))
      (dolist (slot '(:left :top :width :height))
	(substitute-expr obj slot)))

  (dolist (slot '(:visible :draw-function :font :string :line-style 
		  :filling-style))
	  (substitute-expr obj slot))

  ;; set all parameter slots to nil
  (dolist (parameter-slot (g-value obj :parameters-defined-in-lapidary))
    (s-value obj parameter-slot nil))

  ;; delete extra slots in all objects
  (dolist (delete-slot delete-slot-list)
	  (destroy-slot obj  delete-slot)))


;;; Find the maximum right side of a set of gadgets that are being read
;;; into memory. If the gadget represents the contents of a window, return
;;; the value of the gadget's :window-width slot
(defun max-right-side ()
  (declare (special common-lisp-user::*Garnet-Objects-Just-Created*))
  (let ((first-obj (car common-lisp-user::*Garnet-Objects-Just-Created*))
	(max-right 0))
    (if (g-value first-obj :window-p)
	(setf max-right (g-value first-obj :window-width))
      (dolist (obj common-lisp-user::*Garnet-Objects-Just-Created*)
	      (when (> (opal:right obj) max-right)
		    (setf max-right (opal:right obj)))))
    max-right))

;;; Find the maximum bottom side of a set of gadgets that are being read
;;; into memory. If the gadget represents the contents of a window, return
;;; the value of the gadget's :window-height slot
(defun max-bottom-side ()
  (declare (special common-lisp-user::*Garnet-Objects-Just-Created*))
  (let ((first-obj (car common-lisp-user::*Garnet-Objects-Just-Created*))
	(max-bottom 0))
    (if (g-value first-obj :window-p)
	(setf max-bottom (g-value first-obj :window-height))
      (dolist (obj common-lisp-user::*Garnet-Objects-Just-Created*)
	      (when (> (opal:bottom obj) max-bottom)
		    (setf max-bottom (opal:bottom obj)))))
    max-bottom))

(declaim (special common-lisp-user::*Garnet-Object-Just-Created*))

;; check-for-links only examines non-interactor objects. if it finds
;; a slot that points to an object which is not in the aggregate
;; hierarchy, it asks the user whether the slot should become a
;; parameter slot

(defun check-for-links (object top-level-obj symbol-table gadget)

  ;; determine which link slots might be parameters
  (let ((link-values (find-link-slots object symbol-table)))
    (when (and (is-a-p object opal:aggregate)
	       (not (is-a-line-p object)))
	  (dovalues (child object :components)
		    (check-for-links child top-level-obj symbol-table gadget))
	  (dovalues (inter object :behaviors)
		    (check-for-links inter top-level-obj symbol-table gadget)))

    (s-value save-link-parameters :link-values link-values)
    (when link-values
	  (s-value save-link-parameters :obj object)
	  (s-value save-link-parameters :top-level-obj top-level-obj)
	  (s-value save-link-parameters :ok-p nil)
	  ;; show-link-parameters returns a non-nil value if the user
	  ;; selects the ok button. if the user hits the ok button, 
	  ;; convert the links the user specifies to parameters
	  (when (show-link-parameters gadget nil)
		(convert-links-to-parameters)))))
   
;;;Sets the do-not-dump slot of all objects correctly before saving
(defun Set-Do-Not-Dump-Slot-For-Save (objects)
  (declare (special save-time-do-not-dump-slots))
  (dolist (obj objects)
	  (when (is-a-p obj opal:aggregate)
		(set-do-not-dump-slot-for-save (g-value obj :components))
		(set-do-not-dump-slot-for-save (g-value obj :behaviors)))
	  (s-value obj :do-not-dump-slots 
			(append save-time-do-not-dump-slots
				(g-value obj :do-not-dump-slots)))))

;;;Removes the extra values from do-not-dump-slots
(defun Set-Do-Not-Dump-Slot-After-Save (objects)
  (declare (special save-time-do-not-dump-slots))
  (dolist (obj objects)
	  (when (is-a-p obj opal:aggregate)
		(set-do-not-dump-slot-after-save (g-value obj :components))
		(set-do-not-dump-slot-after-save (g-value obj :behaviors)))
	  (s-value obj :do-not-dump-slots 
		      (set-difference
		       (g-value obj :do-not-dump-slots)
		       save-time-do-not-dump-slots))))

;;; Put value into slot being sure that an old constraint, if any, is removed.
(defun Smash-Value (obj slot new-val)
  (destroy-constraint obj slot)
  (s-value obj slot new-val))

;; Goes through all the interactors anywhere in agg and any sub-aggregadgets 
;; and fixes the :active slot and the :visible slots of the interim and
;; final feedback objects. If save-p is true, then the :active and :visible
;; values are restored from the :save-values slot of the interactors and
;; feedback objects; otherwise the :active and :visible values are stored
;; in the :save-values slot and formulas that depend on whether Lapidary
;; is in test or build mode are inserted into these slots. Also checks the
;; graphical parameters of the interactor and determines if any of these
;; graphical objects need to be written out
(defun Fix-All-Interactors (agg save-p)
  (let ((inters (if (is-a-p agg inter:interactor) 
		    (list agg)
		    (g-value agg :behaviors))))

    ;; fix the :visible slot of feedback objects
    (when (g-value agg :feedback-p)
	  (if save-p
	      (progn
		;; make the feedback be invisible so that the value written
		;; out as the default for a formula is nil--first destroy
		;; the constraint in the :visible slot so we do not get
		;; a warning about its not being evaluated
		(destroy-constraint agg :visible)
		(s-value agg :visible nil)
		(restore-value agg :visible)
		;; restore the :left and :top values, first destroying the
		;; current constraints so we do not get warnings about
		;; the formulas not being evaluated
		(destroy-constraint agg :left)
		(destroy-constraint agg :top)
		(restore-value agg :left)
		(restore-value agg :top))
	      (save-value agg :visible t)))
    (dolist (i inters)
      (if save-p
	  (progn
	    ; make start-where be nil if start-where is determined by a
	    ; formula. this way write-gadget will not write out a list
	    ; improperly
	    (when (formula-p (get-value i :start-where))
		  ;; first evaluate the formula so that we won't get a
		  ;; message from kr about setting a slot before its
		  ;; formula is evaluated
		  (g-value i :start-where)
		  (s-value i :start-where nil))
	    ; restore the old values of the :window and :active 
	    ; slots
	    (restore-value i :active)
	    (restore-value i :window)
	    ;; if the restored value of the window slot is nil, create
	    ;; a formula that will compute the proper value for the window
	    ;; slot
	    (when (null (g-value i :window))
	      (s-value i :window
		  (let ((start-where (g-value i :start-where)))
		    (cond ((g-value i :operates-on) 
			   (o-formula (gvl :operates-on :window)))
			  ((eq start-where t) t)
			  ((eq start-where nil) nil)
			  (t (case (first start-where)
				   ;; if the start-where is an element of an
				   ;; aggregate,
				   ;; return the aggregate's window
				   ((:in :in-box :in-but-not-on
					 :element-of :leaf-element-of 
					 :element-of-or-none
					 :leaf-element-of-or-none 
					 :check-leaf-but-return-element
					 :check-leaf-but-return-element-of-or-none)
				    (o-formula (gv (second (gvl :start-where)) :window)))
				   
				   ;; if the start-where is a list of 
				   ;; objects, return the list of windows 
				   ;; that hold these objects
				   ;; (ignore the type argument)
				   ((:list-element-of 
				     :list-leaf-element-of
				     :list-element-of-or-none
				     :list-leaf-element-of-or-none
				     :list-check-leaf-but-return-element
				     :list-check-leaf-but-return-element-or-none)
				    (o-formula (let ((start-where (gvl :start-where))
						     (window-list nil))
						 (dolist (obj (gv (second start-where)
								  (third start-where)))
							 (let ((window (g-local-value obj :window)))
							   (when window
								 (pushnew window window-list))))
			       window-list)))
				   (t nil)))))))
	    ;; if the interactor is a move-grow interactor and the 
	    ;; :obj-to-change is a formula inferred by Lapidary, check
	    ;; to see whether the link slot in the start-where object
	    ;; that points to the :obj-to-change has a formula in it.
	    ;; if it does, substitute the expression in this formula into
	    ;; the :obj-to-change formula
	    (when (is-a-p i directional-move-grow-interactor)
		  (let* ((obj-to-change-value (get-value i :obj-to-change))
			(start-where-obj (and (formula-p obj-to-change-value)
					      (g-formula-value obj-to-change-value
							       :start-where-obj)))
			link obj-to-change-expr link-expr)
		    ;; if start-where-obj exists, then check for possible
		    ;; link replacement
		    (when (and start-where-obj
			       (schema-p start-where-obj))
			  (setf link (car (g-formula-value obj-to-change-value
							   :links)))
			  (when (formula-p (get-value start-where-obj link))
				(setf link-expr (kr::a-formula-lambda
						 (get-value start-where-obj link)))
				(setf obj-to-change-expr 
				      (kr::a-formula-lambda obj-to-change-value))
				;; get rid of link slot
				(setf obj-to-change-expr 
				      (reverse obj-to-change-expr))
				(setf obj-to-change-expr
				      (cdr obj-to-change-expr))

				(setf obj-to-change-expr
				      (reverse obj-to-change-expr))
				;; add the link expr to the end of the
				;; obj-to-change-expr
				(s-value i :obj-to-change
					 (kr::make-into-o-formula
					  (formula (append obj-to-change-expr
							   (cdr link-expr))))))))))
	  (progn 
	    ;; save the old values of the :window and :active slots, then
	    ;; insert the new formulas that depend on whether Lapidary is
	    ;; in build or test mode
	    (save-value i :window nil)
	    (save-value i :active nil)))))

  ;; now recursively do any children
  (dolist (child (g-value agg :components))
    (when (or (is-a-p child opal:aggregate) ; includes aggregadgets and aggrelists
	      (g-value child :feedback-p))
      (Fix-All-Interactors child save-p))))
#|
(defun Fix-All-Interactors (agg save-p)
  (let ((inters (if (is-a-p agg inter:interactor) 
		    (list agg)
		    (g-value agg :behaviors))))
    (dolist (i inters)
     (let ((interim-feedback (g-value i :feedback-obj))
	   (final-feedback (g-value i :final-feedback-obj)))
      (if save-p
	  (progn
	    ; restore the old values of the :visible, :window, and :active 
	    ; slots
	    (restore-value i :active)
	    (restore-value i :window)
	    (when interim-feedback 
		  (restore-value interim-feedback :visible)
		  (restore-value interim-feedback :left)
		  (restore-value interim-feedback :top))
	    (when final-feedback 
		  (restore-value final-feedback :visible)
		  (restore-value final-feedback :left)
		  (restore-value final-feedback :top))
	    ;; now determine if any graphical objects must be written out
	    (fix-interactor-object i :feedback-obj)
	    (fix-interactor-object i :final-feedback-obj)
	    (fix-interactor-object i :obj-to-change)

	    ;; if the interactor does not belong to an aggregate, the object
	    ;; it operates on may need to be written out
	    (when (not (g-value i :operates-on))
		  (let ((start-where (get-value i :start-where)))
		    (when (and (not (formula-p start-where))
			       (listp start-where))
			  (save-inter-object (second start-where) i)))))

	  (progn 
	    ;; save the old values of the :visible and :active slots, then
	    ;; insert the new formulas that depend on whether Lapidary is
	    ;; in build or test mode
	    (save-value i :window nil)
	    (save-value i :active nil)
	    (when interim-feedback
		  (save-value interim-feedback :visible t))
	    (when final-feedback
		  (save-value final-feedback :visible t)))))))

  ;; now recursively do any children
  (dolist (child (g-value agg :components))
    (when (is-a-p child opal:aggregate) ; includes aggregadgets and aggrelists
      (Fix-All-Interactors child save-p))))
|#

(defun destroyed-schema-p (value)
  (and (kr::is-schema value)
       (not (formula-p value))
       (kr::deleted-p value)))

;;; do processing to make an object ready for saving, copying, or
;;; instancing:
;;; 1) remove all <*kr:destroyed*> slots from an object and destroy any
;;;     formulas that depend on these slots
;;; 2) put formulas in the :feedback-obj, :final-feedback-obj, and 
;;;     :obj-to-change slots of an interactor if the interactor belongs
;;;     to the same aggregadget as the objects in these slots
(defun fix-up-obj (obj)
  (declare (special save-time-do-not-dump-slots))
 (let (deleted-links)
       
  ;; recursively remove corrupted slots
  (dolist (child (g-local-value obj :components))
	  (fix-up-obj child))
  (dolist (child (g-local-value obj :behaviors))
	  (fix-up-obj child))
  ;; get rid of :selected slot if it exists--we do not want a warning
  ;; message
  (when (has-slot-p obj :selected)
	(destroy-slot obj :selected))

  (doslots (slot obj)
    (let ((value (g-value obj slot))
	  formula-slot formula-schema)
      (when (destroyed-schema-p value)
	    ;; if the slot is corrupted (i.e., points to a destroyed object),
	    ;; find all formulas that depends on this slot and destroy them.
	    ;; Then get rid of the slot.
	    (dolist (formula (kr::get-dependents obj slot))
		    (when (formula-p formula)
			  (setf formula-slot (kr::on-slot formula))
			  (setf formula-schema (kr::on-schema formula))
			  (destroy-constraint formula-schema formula-slot)))
	    (destroy-slot obj slot))))

#|
;;; links can be created in conditional branches, and if the conditional
;;; branches are not executed, dependencies will never get created. This
;;; can happen when formulas are created in interactors, and users to do
;;; not test all possible branches. Thus we cannot safely eliminate links
;;; that do not have dependents
  ;; get rid of link slots that have no dependents. Lapidary cannot
  ;; guarantee that it eliminates link slots when it eliminates constraints
  ;; so we must clean up here. 
  (dolist (slot (g-value obj :links))
	  (when (null (kr::get-dependents obj slot))
		(destroy-slot obj slot)
		(push slot deleted-links)))
|#
  (dolist (slot '(:left-over :top-over :width-over :height-over
			     :x1-over :y1-over :x2-over :y2-over))
	  (when (null (kr::get-dependents obj slot))
		(destroy-slot obj slot)))
  (s-value obj :links (set-difference (g-value obj :links) deleted-links))

  (when (is-a-p obj inter:interactor)
	(dolist (slot '(:feedback-obj :final-feedback-obj :obj-to-change))
		(let ((value (get-local-value obj slot)))
		  (when (and value (not (formula-p value)) (schema-p value)
			     (common-ancestor-p obj value))
			(s-value obj slot (formula `(gvl ,@(gilt:make-path obj value))))))))))

;;; determines if an object is being written out
(defun save-p (obj saved-objects)
  (let ((parent obj))
    (loop
     (when (null parent)
	   (return-from save-p nil))
     (when (member parent saved-objects)
	   (return-from save-p t))
     (setf parent (g-value parent :parent)))))

;;; move a value from the :save-values list to the indicated slot

(defun restore-value (obj slot)
  (let ((value (cdr (assoc slot (get-local-value obj :save-values)))))
    ;; do not want this formula returned to the formula free-list if
    ;; the object is destroyed
    (when (formula-p value) 
	  (setf value (copy-formula value)))
    (if (eq value :none)
	;remove the constraint and go back to inheriting
	(progn
	  (destroy-constraint obj slot)
	  (destroy-slot obj slot))
	;else use old value
        (smash-value obj slot value))))

;;; save the value of the indicated slot in the :save-values list and
;;; store a formula in the slot that makes it depend on whether Lapidary
;;; is in test or build mode

(defun save-value (obj slot build-value)
  (push (cons slot (if (has-slot-p obj slot)
		       (if (formula-p (get-local-value obj slot))
			   (copy-formula (get-local-value obj slot))
			   (get-local-value obj slot))
		       :NONE))
	(g-value obj :save-values))
  (s-value obj slot
	   (formula `(if (gv lapidary::editor-menu :build-p)
			 ,build-value
			 ,(let ((value (get-value obj slot)))
			    (if (formula-p value)
				(extract-formula value)
			      value))))))

;;; determine if a graphical object reference by an interactor should be saved
(defun save-inter-object (obj inter)
  (declare (special *save-objects*))
  (when (not (save-p obj *save-objects*))
	(inter:beep)
	(format t "~% ~S is referenced by interactor ~S but is not being saved" 
		obj inter)
	(when (yes-or-no-p "Should it be saved?")
	      (let ((copy-obj (opal:copy-gadget obj nil)))
		(push copy-obj *save-objects*)
		(setf (kr::schema-name copy-obj) (kr::schema-name obj))))))
		
;;; determine if a graphical object referenced by an interactor should
;;; be moved to the interactor's aggregate (if it belongs to one) or
;;; saved (if the interactor does not belong to an aggregate)

(defun fix-interactor-object (inter slot)
  (let ((operates-on (g-value inter :operates-on))
	(obj (g-value inter slot)))
    (when obj
	  (if operates-on
	      (progn
		(when (or (null (g-value obj :parent))
			  (eq (g-value obj :parent) 
			      (g-value obj :window :editor-agg)))
		      ;; if the object that the interactor operates on
		      ;; is an independent object (i.e., is not a
		      ;; child of another aggregate), try to merge the
		      ;; obj and operates-on objects into a common aggregate;
		      ;; if the user does not want this, try to save the
		      ;; obj object
		      (if (null (g-value operates-on :parent))
			  (progn
			    ;; ask user if the obj and operates-on objects
			    ;; should be merged into a common aggregate
			    (inter:beep)
			    (format t "~% Objects ~S and ~S are referenced by interactor ~S but are not in a common aggregate"
				    obj operates-on inter)
			    (if (yes-or-no-p "Should they be placed in a common aggregate?")
				(make-aggregate (list obj operates-on))
			        (save-inter-object obj inter)))

			;; if the object that the interactor operates on
			;; is belongs to another aggregate, try to merge
			;; the obj object into this aggregate; if the user
			;; does not want this, try to save the obj object
			  (progn
			    (inter:beep)
			    (format t "~% Objects ~S and ~S are referenced by interactor ~S but are not in a common aggregate"
				    obj operates-on inter)
			    (if (yes-or-no-p "Should they be placed in a common aggregate?")
				(progn
				  (opal:remove-component (g-value obj :parent)
							 obj)
				  (opal:add-component (g-value operates-on :parent) obj))
			        (save-inter-object obj inter)))))

		(when (eq (g-value obj :parent) 
			  (g-value operates-on :parent))
		      (s-value inter slot
			       (formula `(gvl :operates-on :parent 
					      ,(g-value obj :known-as))))))
	    ; else interactor is not part of an aggregate. determine
	    ; if this object should be saved
	      (save-inter-object obj inter)))))
