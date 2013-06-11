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
;;; This file contains code that creates objects in the
;;; graphical editor. It uses the two point interactor
;;; and provides a dashed rectangle to indicate where
;;; the object is position and how large it is

(in-package "LAPIDARY")

(eval-when (eval load compile)
  (export '(create-object-do-go create-object-do-stop)))

(defun create-name (obj)
  (declare (special fill-prop line-prop shape-menu))
  (let ((filling-style (g-value obj :filling-style))
	(line-style (g-value obj :line-style))
	(fill-selected (g-value fill-prop :fill-boxes :selected))
	(line-selected (g-value line-prop :line-style-boxes :selected)))
    (keyword-from-string 
     (concatenate 'string 
		  (if filling-style
		      (if fill-selected
			  (g-value fill-selected :name)
			  "solid")
		      "transparent")
		  "-"
		  (if line-style
		      (if line-selected
			  (g-value line-selected :name)
			  "thin-line")
		      "borderless")
		  "-"
		  (symbol-name (g-value shape-menu :name))
		  "-"
		  (princ-to-string (kr::schema-name obj))))))
    
;;; ================================================================
;;; initialize the list feedback for either a move-grow or two-point
;;; interactor. since the move-grow interactor takes three arguments,
;;; and the two-point interactor takes only two, there is an optional
;;; third parameter
;;; ================================================================

(defun initialize-list-feedback (inter arg1 &optional arg2)
  (declare (special shape-menu))
  (let* ((two-point-p (is-a-p inter inter:two-point-interactor))
	 (item-prototype (if two-point-p
			     (car (g-value *selection-info* :selected))
			     (g-value inter :obj-to-change :item-prototype)))
	 (feedback-obj (g-value inter :feedback-obj))
	 (feedback-prototype (g-value feedback-obj :item-prototype)))

    ;; make sure an item prototype exists--it can only be nil if a two point
    ;; interactor is being used and there was no selection
    (when (null item-prototype)
	  (inter:abort-interactor inter)
	  (lapidary-error "You must select a prototype object before creating a list")
	  (return-from initialize-list-feedback))
    (when (and two-point-p (cdr (g-value *selection-info* :selected)))
	  (lapidary-error "You cannot select more than one prototype object.
Please deselect objects so you have only
one selection, then try again.")
	  (inter:abort-interactor inter)
	  (return-from initialize-list-feedback))

    ;; if this is a resize, make sure none of the aggrelist's components
    ;; are selected--the current version of aggrelists destroys components
    ;; when the :items slot changes, which causes selection handles to
    ;; surround a destroyed object. It is easiest to allow the user to have
    ;; only one object (the aggrelist) selected when a resize operation is
    ;; attempted. Otherwise, not only would the components of the aggrelist
    ;; have to be checked for selection, but so would any of the component's
    ;; children.
    (when (and (not two-point-p) (cdr (g-value *selection-info* :selected)))
	  (lapidary-error "Only the list may be selected when resizing a list.
Please deselect all other objects and then
try again")
	  (inter:abort-interactor inter)
	  (return-from initialize-list-feedback))

    ;; set size of the item prototype
    (s-value feedback-prototype :width (g-value item-prototype :width))
    (s-value feedback-prototype :height (g-value item-prototype :height))
    
    ;; make feedback visible
    (s-value feedback-obj :visible t)
#|
    ;; interactor should create a minimum of one item
    (s-value inter :min-width (g-value item-prototype :width))
    (s-value inter :min-height (g-value item-prototype :height))
|#
    ;; now display the feedback for the first time--if this is a two-point
    ;; interactor display one item, otherwise display the current number
    ;; of items--the current number of items can be obtained from the
    ;; :obj-to-change slot. 
    (let ((obj-to-change (if two-point-p
			     aggrelist-feedback
			     (g-value inter :obj-to-change)))
	      value)
      (dolist (slot *aggrelist-feedback-slots*)
	      (setf value (get-value obj-to-change slot))
	      (s-value feedback-obj slot
		       (if (formula-p value)
			   (kr:copy-formula value)
			 value)))
      (when two-point-p
	    (s-value feedback-obj :items 1)
	    (s-value feedback-obj :rank-margin nil))

      (opal:notice-items-changed feedback-obj)	
      (if two-point-p
	  (progn
	    ;; store the points list in the :box slot of the feedback
	    ;; object--otherwise it won't appear in the proper position
	    ;; when it is initially displayed
	    (s-value feedback-obj :box arg1)
	    (kr:call-prototype-method inter arg1))
	  (kr:call-prototype-method inter arg1 arg2)))))

;;; ================================================================
;;; create an aggrelist using an item prototype. if there are 
;;; selected objects, use one of them as the prototype, otherwise
;;; use the previously selected item in the shape menu as the
;;; prototype
;;; ================================================================

(defun create (inter points-list)
  (declare (special shape-menu))
  (let* ((feedback-obj (g-value inter :feedback-obj))
	 (obj (create-instance NIL opal:aggrelist
		(:visible t)
		(:left (first points-list))
		(:top (second points-list))
		(:direction (g-value feedback-obj :direction))
		(:item-prototype (car (g-value *selection-info* :selected)))
		(:items (g-value feedback-obj :items)))))
    
    (when (g-value feedback-obj :rank-margin)
	  (s-value obj :rank-margin (g-value feedback-obj :rank-margin)))

    (kr:call-prototype-method inter points-list)

    ;; generate a name for the new object
    (name-lapidary-obj obj)

    (opal:add-component (g-value inter :current-window :editor-agg) obj)
    ;; cause the new object to be selected
    (deselect-objects)
    (primary-select obj)

    ;; change selection in shapes menu back to a regular shape so that
    ;; lists are not accidentally created
    (s-value (g-value shape-menu :old-value) :selected t)
    (s-value (g-value shape-menu :selected) :selected nil)
    (s-value shape-menu :selected (g-value shape-menu :old-value))

    ;; allow the user to change the properties of the newly created aggrelist
;    (PopUpPropsWin *aggrelist-slots*)
))

;;; ================================================================
;;; calculate-howmany should work with both move-grow and two-point
;;; interactors so it has an optional third parameter to accomodate
;;; move-grow interactors.
;;; ================================================================

(defun calculate-howmany (inter arg1 &optional arg2)
  (let* ((newpoints (if (is-a-p inter inter:two-point-interactor) arg1 arg2))
	 (feedback-list (g-value inter :feedback-obj))
	 (feedback (g-value feedback-list :item-prototype))
	 (proto-width (g-value feedback :width))
	 (proto-height (g-value feedback :height))
	 (h-spacing (g-value feedback-list :h-spacing))
	 (v-spacing (g-value feedback-list :v-spacing))
	 (width (third newpoints))
	 (height (fourth newpoints))
	 num-items num-cols num-rows)
    (if (is-a-p inter inter:two-point-interactor)
	(kr:call-prototype-method inter newpoints)
      (kr:call-prototype-method inter arg1 arg2))

    (setf num-cols (floor (/ (+ width h-spacing) 
			     (+ proto-width h-spacing))))
    (setf num-rows (floor (/ (+ height v-spacing)
			     (+ proto-height v-spacing))))
    (setf num-items (* num-cols num-rows))
    ;; don't set the :rank-margin slot unless their are multiple columns
    ;; in a vertical aggrelist or multiple rows in a horizontal aggrelist
    (s-value feedback-list :rank-margin 
	     (if (eq (g-value feedback-list :direction) :vertical)
		 (if (= num-cols 1) nil num-rows)
	         (if (= num-rows 1) nil num-cols)))
    (when (not (= num-items 
		  (if (numberp (g-value feedback-list :items))
		      (g-value feedback-list :items)
		      (length (g-value feedback-list :items)))))
	  (s-value feedback-list :items num-items)
	  (opal:notice-items-changed feedback-list))))

;;; =========================================
;;; this function creates a new object in
;;; the editor
;;; =========================================

(defun Create-New-Obj (inter point-list)
  (declare (special shape-menu 
		    line-prop fill-prop))
  (let* ((editor-agg (g-value inter :current-window :editor-agg))
	(object-type (g-value shape-menu :value))
	(fill-selected (g-value fill-prop :fill-boxes :selected))
	(line-selected (g-value line-prop :line-style-boxes :selected))
	(filling-style (Check-Colors-For-Fill (g-value fill-selected :val)
					      (g-value fill-prop :new-color) 
					      fill-selected))
	(line-style (Check-Colors-For-Line (g-value line-selected :val)
					   (g-value line-prop :new-color) 
					   line-selected))
	obj)

    (when (eq filling-style :none) (setf filling-style nil))
    (when (eq line-style :none) (setf line-style nil))
    (when *test-debug* (format T "creating ~S; ~s~%" object-type point-list))
    (when (and (line-p object-type) (eq line-style nil))
	  (lapidary-error "a line must be created with a line-style other than \"none\"")
	  (return-from create-new-obj))

    (setf obj
      (if (line-p object-type)
	  (kr:create-instance nil object-type
		(:x1 (first point-list))
		(:y1 (second point-list))
		(:x2 (third point-list))
		(:y2 (fourth point-list))
		(:filling-style filling-style)
		(:line-style line-style))

	  (kr:create-instance nil object-type
			   (:filling-style filling-style)
			   (:line-style line-style)
			   (:left (first point-list))
			   (:top (second point-list))
			   (:width (third point-list))
			   (:height (fourth point-list)))))
    
    (opal:add-component editor-agg obj)

#|
    ; if the object is a circle add a formula that computes its radius, width
    ; and height from its diameter
    (when (eq object-type opal:circle)
      (s-value obj :suggested-width (third point-list))
      (s-value obj :suggested-height (fourth point-list))
      (s-value obj :diameter 
	       (o-formula (min (gvl :suggested-width) (gvl :suggested-height))))
      (s-value obj :radius (o-formula (round (gvl :diameter) 2))))
|#

    ; create the object's name and create a link to it in its parent

    (s-value obj :known-as (create-name obj))
    (s-value editor-agg (g-value obj :known-as) obj)

    ;; change the object's kr name to the new name
    (setf (kr::schema-name obj) 
	  (read-from-string (symbol-name (g-value obj :known-as))))
    (set (kr::schema-name obj) obj)

    ;; cause the new object to be selected
    (deselect-objects)
    (primary-select obj)
    obj))


(defun create-object-do-go ()
  (declare (special create-text-inter editor-menu shape-menu))
    (create-instance 'create-obj-inter inter:two-point-interactor
     (:window (o-formula (gv *selection-info* :window)))
     (:start-event *obj-creation-button*)
     (:start-where t)
     (:active (o-formula (and (not (gv create-text-inter :active))
			      (not (eq (gv shape-menu :name) :window))
			      (not (eq (gv shape-menu :name) 
				       :horizontal-aggrelist))
			      (not (eq (gv shape-menu :name) 
				       :vertical-aggrelist))
			      (gv editor-menu :build-p))))
     (:line-p (o-formula (line-p (gv shape-menu :value))))
     (:feedback-obj 
      (o-formula (let ((selection (gv shape-menu :name)))
		   (case selection
		     ((:line :arrow-line :double-arrow-line)
		      (gvl :current-window :create-line-feedback))
		     (:rectangle (gvl :current-window :create-rect-feedback))
		     (:roundtangle 
		      (gvl :current-window :create-roundtangle-feedback))
		     (:circle (gvl :current-window :create-circle-feedback))))))
     (:Min-width 3)
     (:Min-height 3)
     (:final-function #'create-new-obj))

    (create-instance 'create-list-interactor inter:two-point-interactor
		     (:start-where t)   
		     (:active (o-formula (and (or (eq (gv shape-menu :name) 
						      :horizontal-aggrelist)
						  (eq (gv shape-menu :name)
						      :vertical-aggrelist))
					      (gv editor-menu :build-p))))
		     (:window (o-formula (gv *selection-info* :window)))
		     (:start-event *obj-creation-button*)
		     (:feedback-obj (o-formula (gvl :current-window
						    :aggrelist-feedback)))
		     (:min-width (o-formula (gvl :feedback-obj :item-prototype :width)))
		     (:min-height (o-formula (gvl :feedback-obj :item-prototype :height)))
		     (:start-action 'initialize-list-feedback)
		     (:running-action 'calculate-howmany)
		     (:final-function 'create)))


(defun create-object-do-stop ()
  (when (boundp 'create-obj-inter)
    (opal:destroy create-obj-inter))
  (when (boundp 'create-list-interactor)
	(opal:destroy create-list-interactor))
)

