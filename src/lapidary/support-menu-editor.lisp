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
;;; This file provides the support functions for the main editor menu.
;;; For example, one of the menu commands is "make aggregate", which
;;; takes a list of selected objects and makes them into an aggregate

(in-package "LAPIDARY")

#|
========================================================================
Change log:
         5/22/92 Brad Vander Zanden - fixed translate formula so that
                   it no longer uses the :kr-function slot
========================================================================
|#

; (load "support-menu-editor.lisp")

(defun get-coordinates (gadget)
  (opal:convert-coordinates (g-value gadget :window)
			      (g-value gadget :left)
			      (opal:bottom gadget)
			      nil))

;;; ==============================================
;;; this function calls the appropriate procedure
;;; to implement the selected editor menu command,
;;; and then deselects all the selected objects
;;; ==============================================

(defun lap-menu-handler (gadget menu selected-string)
  (declare (ignore menu))
  (declare (special editor-menu read-file save-file add-gadget 
		    *selection-info*))
    (cond ((equal selected-string "make aggregadget")
	   (make-aggregate))
	  ((equal selected-string "quit")
	   (do-stop))
	  ((equal selected-string "delete object")
	   (delete-objects))
	  ((equal selected-string "make instance")
	   (make-instance-or-copy t))
	  ((equal selected-string "make copy")
	   (make-instance-or-copy nil))
	  ((equal selected-string "list properties")
	   (PopUpPropsWin *aggrelist-slots*))
	  ((equal selected-string "text properties")
	   (s-value text-properties-win :visible t))
	  ((equal selected-string "line constraints")
	   (gg:show-line-constraint-menu))
	  ((equal selected-string "box constraints")
	   (gg:show-box-constraint-menu))
	  ((equal selected-string "c32")
	   (multiple-value-bind (left top)
	      (get-coordinates gadget)
	      (gg:c32 nil nil :left left :top top)))
	  ((equal selected-string "clear workspace")
	   (clear-workspace))
	  ((equal selected-string "ungroup")
	   (ungroup))
          ((equal selected-string "bring to front")
	   (change-covering-order :front))
          ((equal selected-string "send to back")
	   (change-covering-order :back))          
	  ((equal selected-string "delete window")
	   (lapidary-delete-window))
	  ((equal selected-string "name object")
	   (multiple-value-bind (left top)
	      (get-coordinates gadget)
	      (name-obj left top)))
	  ((equal selected-string "save gadget")
	   (s-value save-file :inter nil)
	   (show-save-dialog nil nil))
	  ((equal selected-string "load gadget")
	   (s-value read-file :function-for-ok 'do-read-file)
	   (show-read-dialog nil nil))
	  ((equal selected-string "add gadget")
	   (show-add-gadget-dialog))
	  ((equal selected-string "interactors")
	   (multiple-value-bind (left top)
	      (get-coordinates gadget)
	      (show-interactor-menu left top)))
	  ((equal selected-string "filling style")
	   (multiple-value-bind (left top)
	      (get-coordinates gadget)
	      (Show-Fill-Props-Dialog left top :filling-style)))
	  ((equal selected-string "line style")
	   (multiple-value-bind (left top)
	      (get-coordinates gadget)
	      (Show-Line-Props-Dialog left top :line-style)))
	  ((equal selected-string "draw function")
	   (multiple-value-bind (left top)
	      (get-coordinates gadget)
	      (when (g-value *selection-info* :selected)
		    (s-value draw-fct-gadget :value 
		       (g-value (car (g-value *selection-info* :selected))
				:draw-function)))
	      (let ((kr::*constants-disabled* nil))
		(gilt:show-in-window draw-fct-gadget left top))))
	  ((equal selected-string "parameters")
	   (show-parameter-window gadget)))
  (opal:update-all))

(defun test-build-fct (gadget button)
  (declare (special editor-menu *selection-info*))
  (declare (ignore gadget))
  (cond
   ((equal button "build")
    ;; make any windows that were made invisible during run mode
    ;; visible again
    (dolist (win (g-value *selection-info* :visible-windows))
      (when (not (g-value win :visible))
        (s-value win :visible t)))
    (s-value *selection-info* :visible-windows nil)
    (s-value editor-menu :build-p t))
   ((equal button "test")
    ;; make a list of visible windows
    (dolist (win (g-value *selection-info* :window))
     (when (g-value win :visible)
      (push win (g-value *selection-info* :visible-windows))))
    (deselect-objects)
    (s-value editor-menu :build-p nil))))

;;; =====================================================
;;; delete all objects in Lapidary windows
;;; or in the window passed to clear-workspace
;;; =====================================================

(defun clear-workspace (&optional (window-to-clear nil))

  ; deselect everything
  (deselect-objects)

  ; clear the objects in the editor window
  (dolist (window (if window-to-clear 
		      (list window-to-clear)
		      (g-value *selection-info* :window)))
    (dolist (item (copy-list (g-value window :editor-agg :components)))
      ;; destroy the link to this object in the object's parent, then destroy
      ;; the object
      (when (schema-p item)
	(when (not (null (g-local-value item :parent)))
	  (destroy-slot (g-local-value item :parent)
			(g-local-value item :known-as)))
	(opal:destroy item)))))


;;; =================================================================
;;; determine if a formula defines a link and if it does, modify
;;; the pathname in the formula so that the formula still computes
;;; the same value
;;; =================================================================

(defun translate-formula (schema slot formula old-link new-link)
  ;; replace all references to old-link with new-link in formula
  ;; first get the formula
  (let ((expression (extract-formula formula)))
    (when (listp expression)
	  (setf expression (subst new-link old-link expression))
	  (change-formula schema slot expression))))

;;; =================================================================
;;; rename an object and translate any formula references to this
;;; object so they reference the new name
;;; =================================================================

(defun name-obj (left top)
  (declare (special prompt-gadget))
  (when (g-value *selection-info* :selected)
    (let* ((obj (car (g-value *selection-info* :selected)))
	   (current-name (g-value obj :known-as))
	   (parent (g-value obj :parent))
	   (kr::*print-as-structure* nil)
	   new-name duplicate-p values)

      (s-value prompt-gadget :string 
	       "Please enter a name for this object (e.g., label)")
      
      (set-initial-value prompt-gadget :result 
			 (princ-to-string (g-value obj :known-as)))

      ;; handle ok function here, so nil out function-for-ok slot
      (s-value prompt-gadget :function-for-ok nil)

      ;; loop until a valid name is entered
      (loop
       (setf values (gilt:show-in-window-and-wait prompt-gadget left top t))
       (when (null values)
	     ;; cancel was hit--leave name-obj
	     (return-from name-obj))
       (setf new-name (gilt:value-of :result values))
       (cond ((string= new-name "")
	      (lapidary-error "Please enter a non-null string"))
	     (t
	      (setf new-name (keyword-from-string new-name))

	      ;; if the new-name is the same as the old name, simply return
	      (when (eq new-name (g-value obj :known-as))
		    (return-from name-obj))

	      ;; make sure that no other object in this aggregate already
	      ;; has the same name
	      (setf duplicate-p nil)
	      (dolist (child (g-value obj :parent :components))
		      (when (eq (g-value child :known-as) new-name)
			    (setf duplicate-p t)
			    (return)))
	      (if duplicate-p
		  (lapidary-error 
		   "The name you just entered is already used")
		;; if the name is not a duplicate, it is a good name
		;; so exit the loop
		(return)))))
       
		    
      ;; change the object's name in the parent and in the object
      (s-value obj :known-as new-name)
      (s-value parent new-name obj)
      
      ;; change the kr name of this object to the new name
      (setf (kr::schema-name obj) (read-from-string (symbol-name new-name)))
      (set (kr::schema-name obj) obj)

      ;; find any links in other objects that point to this object and translate
      ;; their paths so they still point at this object. these links will have
      ;; formulas that depend on this object, so we simply go through the
      ;; list of formulas that reference the link slot for this object. This
      ;; list of formulas can be found in the parent's dependency list
      ;; under the current name of this object.
      (dolist (formula (kr::get-dependents parent current-name))
	(Let ((dependent-schema (kr::on-schema formula))
	      (dependent-slot (kr::on-slot formula)))
	    (translate-formula dependent-schema dependent-slot formula 
			       current-name new-name)))
      (destroy-slot parent current-name)
)))

;;; an undo record contains the value for a schema and a slot

(defvar *undo-free-list* nil)
(defvar *undo-list* nil)
(defvar *undo-added-obj-list* nil)
(defvar *undo-deleted-obj-list* nil)

(defstruct undo-rec 
  schema           
  slot
  value)

;;; add the undo records to the free list and set the undo list to nil
(defun reset-undo ()
  (when *undo-deleted-obj-list*
	(dolist (obj *undo-deleted-obj-list*)
		(when (is-a-p obj inter:interactor)
		      (opal:destroy obj))))
  (setf *undo-deleted-obj-list* nil)
  (setf *undo-added-obj-list* nil)
  (when *undo-list*
	(setf (cdr (last *undo-list*)) *undo-free-list*)
	(setf *undo-free-list* *undo-list*)
	(setf *undo-list* nil)))

;;; save the value of a slot
(defun undo-save (schema slot)
  (let ((entry (or (pop *undo-free-list*) 
		   (make-undo-rec)))
	(value (if (has-slot-p schema slot)
		   (get-local-value schema slot)
		   :NONE)))
    (setf (undo-rec-schema entry) schema)
    (setf (undo-rec-slot entry) slot)
    (setf (undo-rec-value entry) (if (formula-p value) 
				     (copy-formula value)
				     value))
    (push entry *undo-list*)))

;;; reset the stored slots to their previous values
(defun undo ()
  (let (schema slot)
    ;; first restore any deleted objects 
    (dolist (entry *undo-deleted-obj-list*)
	    (when (is-a-p entry inter:interactor)
		  (let* ((undo-slots (g-value entry :undo-slots))
			 (agg (car undo-slots))
			 (active (cdr undo-slots)))
		    ;; if the interactor belonged to an aggregate, place
		    ;; it back in the aggregate
		    (when (and agg (schema-p agg))
			  (opal:add-interactor agg entry))
		    ;; restore the :active slot of the interactor
		    (s-value entry :active active)
		    ;; reset the appropriate interactor menu
		    (reset-inter-menu entry))))
    (setf *undo-deleted-obj-list* nil)

    ;; delete any added objects
    (dolist (entry *undo-added-obj-list*)
	    (when (is-a-p entry inter:interactor)
		  (reset-inter-menu (g-value entry :is-a))
		  (opal:destroy entry)))
    (setf *undo-added-obj-list* nil)

    (dolist (entry *undo-list*)
      ;; destroy a constraint if necessary
      (setf schema (undo-rec-schema entry))  
      (setf slot (undo-rec-slot entry))
      ;; only undo if schema still exists
      (when (schema-p schema)
	    (destroy-constraint schema slot)
	    ;; certain slots need special processing
	    (case slot
	      (:known-as
	       (let ((value (undo-rec-value entry))
		     (agg (or (g-value schema :parent)
			      (g-value schema :operates-on))))
		 (when agg
		       (destroy-slot agg (g-value schema :known-as))
		       (s-value agg value schema))
		 (s-value schema :known-as value)))
	      (t 
	       ;; if previous value was :NONE, the slot was inherited
	       (if (eq (undo-rec-value entry) :NONE)
		   (destroy-slot schema slot)
		   (s-value schema slot (undo-rec-value entry)))))))
  (reset-undo)))

;;; currently the designer can only change the properties of an aggrelist
(defun PopUpPropsWin (prop-slots)
  (let ((obj (car (g-value *selection-info* :selected)))
	left top)
    (when (or (null obj) (not (is-a-p obj opal:aggrelist)))
	  (lapidary-error "A list object is not selected. You must 
select a list object before changing its properties")
	  (return-from PopUpPropsWin))
    (multiple-value-setq (left top)
      (opal:convert-coordinates (g-value obj :window)
			 (g-value obj :left)
			 (opal:bottom obj) NIL))
    (setq top (+ 40 top))
    (let ((kr::*print-as-structure* nil))
      (Garnet-gadgets:pop-up-win-change-obj *prop-sheet* obj prop-slots 
					  left top
					  "Aggrelist Properties"))))

;; change the values of slots in an aggrelist
(defun Aggrelist-Prop-Sheet-Finish (prop-sheet)
  (declare (special *aggrelist-slots*))
  (let* ((obj (g-value prop-sheet :obj))
	 (changed-values (g-value prop-sheet :changed-values))
	 item-list slot)
    ;; if a slot's value is the same as in the aggrelist prototype, destroy
    ;; the slot. this is easier than trying to modify the property sheet
    ;; function that indiscriminately changes values
    (dolist (slot-fct *aggrelist-slots*)
	    (setf slot (if (listp slot-fct) (car slot-fct) slot-fct))
	    (when (eq (g-value obj slot) (g-value opal:aggrelist slot))
		  (destroy-slot obj slot)))
    (when (setf item-list (second (assoc :items changed-values)))
	  (when (functionp (car item-list))
		(s-value obj :items (eval item-list)))
	  (opal:notice-items-changed obj))))
    
