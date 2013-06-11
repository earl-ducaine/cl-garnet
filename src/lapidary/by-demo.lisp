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
;;; This file provides the functions that handle inferencing and
;;; generalization of constraints for the interactors
;;;

#|
==============================================================================
Change log:
     9/22/93: Fernando -- Renamed "position" variables to "component-position"
      7/6/92: Brad Vander Zanden -- changed by-demo dialog box to a query
                 gadget
==============================================================================
|#

 (in-package "LAPIDARY")

(defvar *by-demo-obj* nil
  "object the user modifies when demonstrating how feedback should behave")

(defvar *by-demo-copy* nil
  "object the user actually modifies when demonstrating how feedback should behave")

(defvar *by-demo-comparison* nil
  "object used to contain the ``before'' state of the demonstration")

;;; This is embedded in the by-demo gadget to handle the OK
;;; and cancel functions"
(defun by-demo-OKCancel-Function (gadget value)
  (cond ((string-equal value "OK")
	 (gilt::standard-OK-function gadget))
	((string-equal value "Cancel")
	 ;; restore the original object to the display and reset the
	 ;; by-demo indicator
;	 (reset-undo)
	 (restore-original-obj)
	 (s-value (g-value gadget :demo-gadget) :demo-p t)
	 ;; restore the original value to the appropriate feedback gadget
	 (s-value (g-value gadget :feedback-gadget) :value 
		  (g-value gadget :feedback-gadget :save-value))
	 (standard-Cancel-function gadget))))
		  	
(defun by-demo-ok-function (gadget value)
  (let ((formula-list (by-demo (g-value gadget :demo-gadget) value))
	(feedback-gadget (g-value gadget :feedback-gadget)))
    (s-value feedback-gadget :field-string nil)
    (if (eq (g-value feedback-gadget :known-as) :feedback-obj)
	(dialog-enqueue :feedback-obj nil *choice-inter-queue*)
        (dialog-enqueue :final-feedback-obj nil *choice-inter-queue*))
    (dialog-enqueue :feedback formula-list *CHOICE-INTER-QUEUE*)))
    
;;; save a formula in an object and all its instances
(defun save-formula (obj slot formula &optional (first-p t))
  (let ((slot-value (assoc slot (g-value obj :save-values))))
    (dovalues (instance obj :is-a-inv :local t)
	      (save-formula instance slot formula nil))
    ;; if not the first call to save-formula, create an instance of formula
    (when (not first-p)
	  (setf formula (kr::make-into-o-formula (formula formula))))
    (if slot-value
	(setf (cdr slot-value) formula)
        (push (cons slot formula) (g-value obj :save-values)))))
 
;;; install a formula in an object and all its instances
(defun install-formula (obj slot formula &optional (first-p t))
  (let ((check-value :lapidary-error)
	(old-value (get-value obj slot)))

    ;; if the old-value is a formula, save a copy of the formula.
    ;; if this is not done, the formula will be destroyed when 
    ;; the new formula is stored in the slot
    (when (formula-p old-value)
	  (setf old-value (copy-formula old-value)))

    (if first-p
      (kr::copy-to-all-instances obj slot (kr::make-into-o-formula formula))
      (kr::copy-to-all-instances obj slot 
				 (kr::make-into-o-formula (formula formula))))

    ;; demand the value of this formula and be prepared to undo this formula 
    ;; if something goes wrong	  
    (unwind-protect
	(setf check-value (g-value obj slot))
      (when (eq check-value :lapidary-error)
	    (destroy-constraint obj slot)
	    (s-value obj slot old-value)
	    (format t "~% The bad constraint was destroyed and the old value restored.")
	    (format t "~% To continue, type (inter:main-event-loop)~%")
	    (opal:update-all)))))
 
;;; restore the original object to the display and select it when a
;;; by-demo operation is completed or canceled

(defun restore-original-obj ()
  (declare (special *by-demo-copy* *by-demo-obj* *by-demo-comparison*))
  ;; deselect any selected objects
  (primary-deselect-objects :none)
  (secondary-deselect-objects :none)
	  
  ;; destroy the copied objects and place the original by-demo 
  ;; object back into the editor window. This must be done with
  ;; care by determining the position of the component to be
  ;; removed, removing the component, and then inserting the
  ;; component at the appropriate position. If this is not done,
  ;; an aggrelist can get messed up

  (let* ((agg (g-value *by-demo-copy* :parent))
	 (components (g-value agg :components))
	 (component-position (- (list-length components)
		      (list-length (member *by-demo-copy* components)))))
    (opal:remove-component agg *by-demo-copy*)
    (opal:add-component agg *by-demo-obj* :at component-position))

  (opal:destroy *by-demo-copy*)
  (opal:destroy *by-demo-comparison*)

  ;; select the original by-demo object
  (primary-select *by-demo-obj*))

;;; create a copy of the object selected by the user, allow the user to
;;; modify the copy, then find the differences between the original and
;;; the copy and create formulas that reflect the differences

(defun by-demo (gadget value)
  (declare (ignore value))
  (let* ((demo-slot (g-value gadget :demo-slot))
	(demo-p (g-value gadget :demo-p))
	(queue (g-value gadget :queue))
	(inter (g-value gadget :inter))
	(slot (g-value gadget :slot))
	(by-demo-formula-list (assoc slot (symbol-value queue)))
	(selection (car (g-value *selection-info* :selected)))
	formula-list start-where start-where-objs control)
    ;; determine if the demonstration is to begin or is just ending
    (if demo-p
	;; demonstration has just begun. Create two copies
	;; of the selected object and replace the selected object with
	;; one of the copies in the editor window.
	(progn
	  ;; first make sure that there is a selection and that it is
	  ;; a member of the objects acted on by this interactor
	  (when (null selection)
	    (lapidary-error
	     "Must select an object to demonstrate with")
	    (return-from by-demo nil))

	  (setf start-where (get-start-where (symbol-value queue) inter))
	  (when (eq start-where :not-supplied)
		(lapidary-error "Must supply a start-where before doing demonstrations")
		(return-from by-demo nil))
	  (setf control (if (atom start-where) 
			    start-where
			    (car start-where)))
	  (when (member control '(t nil :in-but-not-on))
		(lapidary-error
		 (format nil "cannot demonstrate behavior for a start-where that begins with ~S" control))
		(return-from by-demo nil))
	  (setf start-where-objs (find-start-where-objs start-where))
	  (when (not (member selection start-where-objs))
		(lapidary-error 
		 (format nil "The selection ~S is not an object that satisfies the interactor's start-where predicate" selection))
		(return-from by-demo nil))

	  ;; save the selected object
	  (setf *by-demo-obj* selection)

	  ;; deselect any selected objects
	  (primary-deselect-objects :none)
	  (secondary-deselect-objects :none)

	  ;; create a copy of the by-demo object and install the current
	  ;; by-demo formulas in it
	  (setf *by-demo-comparison* (opal:copy-gadget selection nil))

	  ;; create a copy of the by-demo object and place the copy
	  ;; in the editor window
	  (setf *by-demo-copy* (create-instance nil *by-demo-comparison*))
	  
	  (when by-demo-formula-list
		(install-by-demo-formulas nil (cdr by-demo-formula-list)
					  *by-demo-comparison*))

	  ;; add the copied object to the original's aggregate in the
	  ;; same position as the original; then remove the original from
	  ;; the aggregate. Since the remove components method for
	  ;; aggregadgets removes all instances of a prototype from an
	  ;; aggregadget when the prototype is removed from an aggregadget,
	  ;; we must determine the position of the original object, then
	  ;; remove it, then add the copied object
	  (let* ((agg (g-value selection :parent))
		 (components (g-value agg :components))
		 (component-position (- (list-length components)
			      (list-length (member selection components)))))
	    (opal:remove-component agg selection)
	    (opal:add-component agg *by-demo-copy* :at component-position))


	   ;; select the copy
	   (primary-select *by-demo-copy*))

	;; demonstration has just ended--create formulas 
	(progn
	  (setf formula-list 
		(compare-slots *by-demo-comparison* *by-demo-copy* demo-slot))

	  (when by-demo-formula-list
		(setf formula-list
		      (merge-by-demo-lists formula-list 
					   (cdr by-demo-formula-list))))
	  (restore-original-obj)))
    ;; invert the demo-p flag
    (s-value gadget :demo-p (not demo-p))
    ;; return t if the demonstration is being set up, or the formula
    ;; list if the demonstration is being completed--notice that demo-p
    ;; will be nil if the demonstration is being completed, so the formula
    ;; list will be returned
    (or demo-p formula-list)))

;;;======================================================================
;;; merge the current list of by-demo formulas with the previous list
;;; of by-demo formulas. Entries on the current list override duplicate
;;; entries on the previous list. Entries are of the form:
;;; (path (slot formula) (slot formula) ...) where path is a sequence
;;; of link names that allows an object to be found in an aggregate 
;;; hierarchy
;;;
;;;======================================================================
(defun merge-by-demo-lists (current-list prev-list)
  (dolist (entry current-list)
    (let* ((path (car entry))
	   (slot-formulas (cdr entry))
	   (prev-entry (assoc path prev-list :test #'equal)))
	  (if prev-entry
	      ;; only retain elements from prev-entry that are not in entry
	      (setf (cdr prev-entry) 
		    (append slot-formulas 
			    (set-difference (cdr prev-entry) slot-formulas
					    :key #'car)))
	      (push entry prev-list))))
  prev-list)

;;;======================================================================
;;;
;;; compare selected slots from the original and copied objects. When
;;; a difference is found, construct a formula that chooses between
;;; the "before" and "after" states based on the value of the demo slot.
;;; For example, given the following values:
;;;  "before" :left (+ (gvl :left-over :left) (gvl :left-offset))
;;;  "after"  :left (+ (opal:gv-right (gvl :left-over)) (gvl :left-offset))
;;;           :demo-slot :interim-selected
;;; we might construct the formula:
;;;    :left (cond ((gvl :interim-selected)
;;;                   (+ (opal:gv-right (gvl :left-over)) (gvl :left-offset)))
;;;                (t (+ (gvl :left-over :left) (gvl :left-offset))))
;;;
;;; if the "after" branch already exists, then we simply substitute the
;;; new "after" expression into the expression portion of the branch. For
;;; example, if the user demoed the above object a second time, we might
;;; get the following values:
;;;
;;;  "before" :left (cond ((gvl :interim-selected)
;;;                        (+ (opal:gv-right (gvl :left-over)) 
;;;                                          (gvl :left-offset)))
;;;                       (t (+ (gvl :left-over :left) (gvl :left-offset))))
;;;
;;;  "after" :left (- (opal:gv-right (gvl :left-over))
;;;                   (gvl :width)
;;;                   (gvl :left-offset))
;;;          :demo-slot :interim-selected
;;;  we would substitute the "after" expression into the 
;;;  (gvl :interim-selected) branch of the conditional statement to obtain:
;;;          :left (cond ((gvl :interim-selected)
;;;                        (- (opal:gv-right (gvl :left-over))
;;;                                          (gvl :width)
;;;                                          (gvl :left-offset)))
;;;                       (t (+ (gvl :left-over :left) (gvl :left-offset))))
;;;
;;; The resulting formulas should not be inserted into the objects affected by
;;; this interactor until the OK or NEW buttons are selected. Thus the
;;; formulas will be saved in the following data structure:
;;;    ((path (slot formula) (slot formula) ...)
;;;     (path (slot formula) (slot formula) ...)
;;;        ...
;;;     (path (slot formula) (slot formula) ...))
;;;
;;; where each path is a path that traverses the aggregate hierarchy
;;; to reach the appropriate component of the demoed object
;;;
;;;======================================================================

(defun compare-slots (original copy demo-slot)
  (let (overall-formula-list formula-list)
  (when (is-a-p original opal:aggregate)
    (do ((original-components (g-value original :components)
			      (cdr original-components))
	 (copy-components (g-value copy :components)
			  (cdr copy-components)))
	((null original-components))
      (setf overall-formula-list
	    (append overall-formula-list
		    (compare-slots (car original-components) 
				   (car copy-components)
				   demo-slot)))))
  (dolist (primary-slot '(:left :top :width :height :visible :draw-function
		  :font :string :line-style :filling-style :x1 :x2
		  :y1 :y2))
    ;; accumulate all the link and offset slots that should be looked at
    ;; (e.g., :left-over, :left-offset, :width-scale, etc)
    (let* ((primary-copy-value (get-value copy primary-slot))
	   (original-copy-value (get-value original primary-slot))
	   (slot-list (list primary-slot)))
      (when (formula-p primary-copy-value)
	    (setf slot-list (append (g-formula-value primary-copy-value :links)
				    slot-list))
	    (when (g-formula-value primary-copy-value :offset)
		  (push (g-formula-value primary-copy-value :offset) 
			slot-list))
	    (when (g-formula-value primary-copy-value :scale)
		  (push (g-formula-value primary-copy-value :scale)
			slot-list)))
      (when (formula-p original-copy-value)
	    (setf slot-list (union slot-list 
				   (g-formula-value original-copy-value :links)))
	    (when (g-formula-value original-copy-value :offset)
		  (pushnew (g-formula-value original-copy-value :offset) 
			   slot-list))
	    (when (g-formula-value original-copy-value :scale)
		  (pushnew (g-formula-value original-copy-value :scale)
			   slot-list)))
     
      (dolist (slot slot-list)
	(let* ((copy-value (get-value copy slot))
	       (original-value (get-value original slot))
	       ;; at the end of this loop a new formula is created. The
	       ;; information in the :links, :scale, and :offset slots of 
	       ;; the original formulas must be preserved
	       (link-slot (union (if (formula-p copy-value)
				      (g-formula-value copy-value :links)
				      nil)
				  (if (formula-p original-value)
				      (g-formula-value original-value :links)
				      nil)))
	       (offset-slot (cond ((formula-p copy-value)
				   (g-formula-value copy-value :offset))
				  ((formula-p original-value)
				   (g-formula-value original-value :offset))
				  (t nil)))
	       (scale-slot (cond ((formula-p copy-value)
				   (g-formula-value copy-value :scale))
				  ((formula-p original-value)
				   (g-formula-value original-value :scale))
				  (t nil))))
	   
       ;; both of the following tests are required since the copy-value may
       ;; be a formula and an instance of original-value, in which
       ;; case the slots are considered similar
       (when (if (and (formula-p copy-value) 
		      (formula-p original-value))
		 (not (is-a-p copy-value original-value))
	         (not (equal copy-value original-value)))

	  ;; if either the original or copy slots contains a formula, extract
	  ;; the formula's expression from the formula object
	  (when (formula-p copy-value)
		(setf copy-value (extract-formula copy-value)))

	  (when (formula-p original-value)
		(setf original-value (extract-formula original-value)))

	  ;; if the slot's formula contains a conditional, store each
	  ;; of the branches in the expression list; otherwise store the
	  ;; expression with a t affixed to it, since the support slots may
	  ;; have conditional expressions
	  (let* ((expr-list (if (and (listp original-value)
				     (eql 'cond (car original-value)))
				(copy-tree (cdr original-value))
			        `((t ,(copy-tree original-value)))))
		 (path `(gvl ,@(gilt:make-path original *by-demo-comparison*) ,demo-slot))
		 ;; try to match the path against one of the conditions in
		 ;; the branches of the conditional expression (even if there 
		 ;; was no previous conditional, there is a branch labeled with
		 ;; t, so the search will be unsuccessful but won't crash)
		 (branch (assoc path expr-list :test #'equal)))

	    ;; if a branch whose condition matches the path is found, 
	    ;; substitute the demoed expression into the branch's expression;
	    ;; otherwise create a new branch with the path as a condition
	    ;; and the demoed expression as the branch's expression
	    (if branch
		(setf (second branch) copy-value)
	        (let ((cond-expr (copy-tree (assoc 't expr-list 
						   :test #'equal))))
		  (setf (car cond-expr) path)
		  (setf (second cond-expr) copy-value)
		  (push cond-expr expr-list)))

	    ;; a formula list has the form: (object (slot formula) ...), so
	    ;; push a (slot formula) cons pair onto the list. 
	    (push (cons slot (formula `(cond ,@expr-list)
				      (g-cached-value original slot)
				      (:links link-slot)
				      (:offset offset-slot)
				      (:scale scale-slot)))
		  formula-list)))))))
  ;; if there is one or more entries on the formula-list, prepend a path
  ;; to the object onto the front of the list so the list has the form
  ;; (path (slot formula) (slot formula) ...). Then push this list onto
  ;; the overall formula list
  (when formula-list
	(push (gilt:make-path *by-demo-comparison* original) formula-list)
	(push formula-list overall-formula-list))
  overall-formula-list))

;;;======================================================================
;;;
;;; Install the formulas on the formula list in all objects that
;;; satisfy the start-where condition. The formulas are stored in
;;; the following data structure:
;;;
;;;  (by-demo-obj (component (slot formula) (slot formula) ...)
;;;               (component (slot formula) (slot formula) ...))
;;;
;;; where by-demo-obj is the object that the demonstration was performed
;;; on and each component is either by-demo-obj or one of the components
;;; of by-demo-obj. To install the formulas in the objects satisfying
;;; the start-where condition, we construct a path for each component
;;; in the formula that will allow us to find the corresponding component 
;;; in each of the objects. This path is constructed by working up the
;;; aggregate hierarchy from the component and pushing the values of the
;;; :known-as slots onto the path.
;;;
;;;======================================================================

(defun install-by-demo-formulas (start-where by-demo-list &optional obj)
  ;; find the list of objects that satisfy the start-where condition
  (let ((objs (if obj (list obj) (find-start-where-objs start-where))))
    ;; if the objects are components of an aggrelist, change the 
    ;; item-prototype rather than the objects themselves--this is something 
    ;; of a hack but it should generally accomplish what the user intends. If
    ;; only the item-prototype-object is changed, then things do not work
    ;; properly. For example, if the :left-offset slot is changed, the change
    ;; is faithfully reflected in the item-prototype-object when the aggrelist
    ;; is changed. However, the :left-offset slot is filtered out of the
    ;; item-prototype, which means that the :left formula in the item-prototype
    ;; does not use :left-offset. Therefore, the inherited :left formula
    ;; in item-prototype-object does not use :left-offset. Thus the change
    ;; to :left-offset gets ignored. The alternative approach would be to
    ;; create a new :left formula for item-prototype-object (copying may not
    ;; work--we want the actual expression copied, and copy does not guarantee
    ;; that the expression will be copied--it will create an instance of
    ;; the grandparent if it exists). However, often the user really wants
    ;; the item prototype modified as well (e.g., floating buttons), so I
    ;; chose to modify the item prototype.
    (when (is-a-p (g-value (car objs) :parent) opal:aggrelist)
	(setf objs (list (g-value (car objs) :parent :item-prototype))))

    (dolist (component-entry by-demo-list)
      (let* ((path (car component-entry))
	     (formula-list (cdr component-entry)))
	(dolist (obj objs)
	  (let ((target-component (eval `(g-value ,obj ,@path))))
	    (dolist (slot-formula formula-list)
;	      (undo-save target-component (car slot-formula))
	      (s-value target-component (car slot-formula) 
		       (kr::make-into-o-formula 
			(formula (cdr slot-formula)))))))))

    ;; return t to indicate a successful installation of the formulas
    t))


;;;======================================================================
;;;
;;; find the list of objects that an interactor can start over
;;;
;;;======================================================================

(defun find-start-where-objs (start-where)
  (let ((control (if (listp start-where) (car start-where) start-where)))
    (case control
	  ((:in :in-box)
	   (list (second start-where)))
	  ((:element-of :leaf-element-of :element-of-or-none 
			:leaf-element-of-or-none 
			:check-leaf-but-return-element
			:check-leaf-but-return-element-or-none)
	   (g-value (second start-where) :components))
	  ((:list-element-of :list-leaf-element-of 
			     :list-element-of-or-none
			     :list-leaf-element-of-or-none 
			     :list-check-leaf-but-return-element 
			     :list-check-leaf-but-return-element-or-none)
	   (g-value (second start-where) (third start-where))))))

;;;======================================================================
;;;
;;; generalize the constraints in the feedback object so that it
;;; can appear over any of the objects in the start-where of the
;;; interactor. also insert a formula that controls the feedback's visibility
;;; into the visibility slot
;;;======================================================================

(defun create-feedback-obj (feedback-obj queue inter)
  (let* ((start-where (or (cdr (assoc :start-where queue))
			  (g-value inter :start-where)))
	 (control (if (listp start-where) (car start-where) start-where))
	 selectable-objs)

    (when (or (not (listp start-where)) (eq control :in-but-not-on))
      (lapidary-error "Cannot generalize constraints if start-where is either t, nil, or :in-but-not-on")
      (return-from create-feedback-obj))

    ;; find the list of objects this feedback object can appear with
    (setf selectable-objs (find-start-where-objs start-where))

    ;; generalize the constraints in the feedback object
    (generalize-constraints feedback-obj selectable-objs)

    ;; mark the object as a feedback-object
    (s-value feedback-obj :feedback-p t)))

#|
    ;; set the visibility slot of the object and any of its instances
    (save-formula feedback-obj :visible 
		  (kr::make-into-o-formula (formula `(gvl :obj-over))))
    (install-formula feedback-obj :visible
		  (formula `(if (gv lapidary::editor-menu :build-p)
				t
				(gvl :obj-over))))))
|#
;;;======================================================================
;;;
;;; this function generalizes the constraints in a feedback object so
;;; that it can appear over any element in the start-where of an
;;; interactor. the function is passed the feedback object and the list
;;; of objects that the interactor can select (call these objects the
;;; selectable objects). the function examines
;;; each link in the feedback object that is associated with a position
;;; slot and determines which object this link points to (call this
;;; object the link object). It then determines which of the selectable
;;; objects is an ancestor of the link object, establishes a path
;;; from the selectable object to the link object and stores the path
;;; in the link slot. the path uses :obj-over to refer to the selectable
;;; object, so the link slot will be able to point to any of the
;;; selectable objects.
;;;
;;;======================================================================

(defun generalize-constraints (obj selectable-objs &optional (parent nil))
  (let (parent-obj obj-over)
    ;; first do children
    (dolist (child (g-value obj :components))
	    (generalize-constraints child selectable-objs 
				    (if parent parent obj)))
    (dolist (link (if (is-a-line-p obj)
		      (append '(:x1-over :x2-over :y1-over :y2-over)
			      (g-value obj :links))
		      (append '(:left-over :top-over :width-over :height-over)
			      (g-value obj :links))))
      (when (setf obj-over (g-local-value obj link))
	(setf parent-obj obj-over)
	(loop
	  (when (or (member parent-obj selectable-objs) (null parent-obj))
	    (return))
	  (setf parent-obj (g-value parent-obj :parent)))
	;; the link should be generalized only if the link points to an
	;; an object which is a descendent of a selectable object
	(when parent-obj
	  ;; must set this link to the selectable object
	  ;; because KR will eagerly evaluate the formula
	  ;; and expect to find :obj-over set
;	  (undo-save obj :obj-over)
;	  (undo-save obj link)
	  (if parent
	      (s-value parent :obj-over parent-obj)
	      (s-value obj :obj-over parent-obj))
	  (s-value obj link 
		   (kr::make-into-o-formula 
		    (formula `(gvl ,@(gilt:make-path obj parent)
				   :obj-over 
				   ,@(gilt:make-path parent-obj obj-over))))))))))
  
