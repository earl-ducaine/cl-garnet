;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;;  This file supplies a prop-sheet, which takes a list of values to
;;;  display, and prop-sheet-for-obj which takes a KR object to
;;;  display.  The file propsheetwin supplies prop-sheet-with-OK and
;;;  prop-sheet-for-obj-with-OK which combine a property sheet with OK
;;;  and Cancel buttons and functions to display these in windows.
;;;
;;;  USER INTERFACE (same for both types)
;;;    Press on the value of a slot with left button to begin typing.
;;;    Press with left button again or hit RETURN or CONTROL-j to stop
;;;    editing (if multi-line strings are allowed, then RETURN goes to
;;;    the next line, so you need to use CONTROL-j or left button to
;;;    stop editing).  Pressing with any other button inside the
;;;    string moves the cursor.  Regular editing operations are
;;;    supported.  If you hit TAB, the cursor will move to the next
;;;    field.  If label selection is enabled, then labels can be
;;;    selected by pressing with any mouse button.  If value selection
;;;    is enabled, then values must be selected with the right button
;;;    while they are not being edited.  Selected labels or values are
;;;    displayed in bold. 
;;; 
;;;  PROGRAMMING INTERFACE
;;; 
;;;  Prop-Sheet
;;;    -- displays a list of labels and values, and allows the values
;;;    to be edited.  The labels can optionally be selectable.
;;;
;;;     Customizable slots
;;;        :left, :top - position of the gadget.  Default: 0,0
;;;        :items - the control list of the items to be displayed in
;;;        the gadget.  The format for the list is a list of lists, as follows:
;;;		( (label1 stringval1 [filter1 [realval1 [comment]]])
;;;               (label2 ...) )
;;;               * The labels can be atoms or strings, and are shown
;;;                 at the left 
;;;		  * The stringval is the initial (default) value displayed.
;;;		    It can be:
;;; 		      - a string
;;; 		      - a formula object which computes a string.  Note
;;; 			that all references in the formula must be absolute
;;; 			(since otherwise they would be relative to the
;;; 			property sheet).
;;; 		      - a gadget (e.g., a radio-button-panel), in which case
;;; 			that gadget is used instead of an editable text field.
;;; 			Note that the gadget itself is used, not an instance,
;;; 			so it will be destroyed	if the prop-sheet is destroyed.
;;; 			The gadget should supply its value in a slot
;;;                     called :value (as the standard garnet gadgets
;;;                     do).  NOTE: If a gadget, no filter functions
;;;                     are not called (use the :selection-function of
;;;                     the gadget), the realval is ignored, and the
;;;                     :changed-values slot is not valid. 
;;; 		  * If the filter is non-nil, it is a function called
;;;                 after the user types a the value (see below)
;;;		  * The realval, if supplied, is the actual value the stringval
;;;			represents (e.g.if the real values are not
;;;                     strings).  If stringval is a list of strings,
;;;                     then realval should be a list of the same length.
;;;		  * If supplied, the comment is displayed after the label.
;;;			It can be any string, and will be displayed
;;; 			after the slot label.  Typical uses would be to
;;; 			give legal values (e.g.: "(0..20)").  
;;;	   :default-filter - if there is no filter on an individual
;;;             item, then the global default-filter function is
;;;             called when the user finishes editing.  See below.
;;;             The default for default-filter does nothing.
;;;	   :v-spacing - vertical space between the items.  Default = 1
;;;        :multi-line-p - whether the user can enter multi-line strings, 
;;;		which means that RETURN does not exit a field, but
;;;             makes a new line. Default: NIL.
;;;        :select-label-p - whether pressing on the label (with any mouse
;;; 		button) causes the item	to be selected.  Default: NIL.
;;;        :label-select-event - button to press to select a label,
;;;             default= :any-mousedown
;;; 	   :label-selected-func - called with (gadget label-obj label)
;;;             when a label is selected.
;;;        :select-value-p - whether pressing on the value (with the
;;;             right button) causes the value to be selected.  NOTE:
;;;             Values which are specified as gadgets
;;;             cannot be selected.  Default: NIL. 
;;; 	   :value-selected-func - called when a value is selected with
;;; 		(gadget value-obj value label) where label is
;;;             the label of that field.
;;;	   :single-select-p - whether a single label or value can be
;;;             selected (T) or multiple fields can be selected (NIL).
;;;             Default: NIL.
;;; 
;;;     Read-only (output) slots
;;; 	   :label-selected - will be set with a list of the selected label
;;;             objects.  Call Get-Val-For-PropSheet-Obj to get label name.
;;; 	   :value-selected - will be set with a list of the selected
;;;             value objects. 	Call Get-Val-For-PropSheet-Obj on an
;;;             obj to get the value and label.
;;;        :value - list of all the slots and their (filtered) values,
;;;             as follows: 
;;;		( (label1 value1) (label2 value2) ...)
;;;        :changed-values - list of the slots that have changed, as:
;;;		( (label1 value1) (label2 value2) )
;;; 		NOTE: not kept valid if a gadget is used as an item.
;;;
;;;  FILTER FUNCTIONS
;;;    The filter functions allow the program to convert the string values
;;;    to the appropriate form for the object.  The displayed string
;;;    and the "real" value are stored separately, so they can be
;;;    different.  Filter functions are defined as:
;;;       (lambda (prop-sheet-gadget label value-obj new-str old-str)
;;;    The value obj is the actual object used to display the string, and will
;;;    be needed only by hackers.
;;;    The filter function can return the value to use (modified new-str, not
;;;    necessarily a string) or it can return three values:
;;;	       (new-val in-valid-p new-str)
;;;    where new-val is a value (not necessarily a string) to use,
;;;    in-valid-p is T if the new-str value is invalid (bad), in which
;;;    case the new-str is still used, but it is shown in italic.  If
;;;    new-str is returned, then it is displayed instead of what the
;;;    user typed (for example if the filter function expands or
;;;    corrects the typed value).
;;;
;;;
;;;  Prop-Sheet-For-Obj
;;;    -- Given a list of slots for a KR object, displays the values and
;;; 	  allows them to be edited.  The labels can optionally be selectable.
;;;       Can supply multiple objects, and shows the union or
;;;       intersection of the slots.  If objects have different values
;;;       for a slot, just shows the value for the first object in the
;;;       list, in italics.
;;;
;;;     Customizable slots
;;;        :left, :top - position of the gadget.  Default: 0,0
;;;        :obj - the KR object to be displayed.
;;;        :slots - the list of slots of the object to view.  Default value:
;;; 		    '(:left :top :width :height).  Any item (slot name)
;;; 		    in the list can be a sublist: (:slot "comment" display).
;;;		    - If the comment is non-NIL, it is displayed after
;;;                   the label. 
;;; 		    - If the display parameter is supplied, it can be
;;;                     - a list of legal values for the slot,
;;;                     - a gadget, in which case the :value slot of
;;;                       the gadget is set with the old value, and
;;;                       the :value slot is queried when the user
;;;                       hits OK.  If a gadget is supplied, then
;;;                       :set-immediately-p should be NIL.  A useful
;;;                       gadget is garnet-gadgets:Pop-Up-From-Icon.
;;;                     - a function, which is then used as a filter.
;;;        :eval-p - If NIL, then the values set into the slots will
;;;                  be all strings.  If T, then evaluates what the
;;;                  user types (using Read-From-String) and sets the
;;;                  result into the slot.  Usually, you use T when
;;;                  displaying the graphical fields of graphical
;;;                  objects.  Default=T.  NOTE: Evaluating a slot may
;;;                  cause the interface to crash if the values are not valid.
;;;	  :set-immediately-p - if T, then as soon as the user types
;;;                  CR, the object's slot is set.  If NIL, some
;;;                  external action must set the object's slots
;;;                  (e.g., when using prop-sheet-for-obj-with-OK, the
;;;                  object's slots are not set until the OK button is
;;;                  hit).  Default=T.
;;;       :type-gadgets - an association list to map types to special
;;;              gadgets or to omit slots.  It will contain a list of lists,
;;;              where the first element is a type descriptor, and the second
;;;              is either NIL to omit the slot or a widget to use, or
;;;              a type to use instead of the "real" type.  e.g.:
;;;        `((kr-boolean ,my-toggle)
;;;          (:left  NIL) ; omit the :left slot
;;;          ((OR (IS-A-P OPAL:FONT) (IS-A-P OPAL:FONT-FROM-FILE))  ,my-font)
;;;          ((OR (LIST (MEMBER :THERE :HERE)))   (MEMBER :THERE :HERE)))
;;;       :error-gadget - if supplied, is a gadget to use to report errors
;;;       :union? - if :slots not supplied, then if T then shows the
;;;                 union of all the slots for the 
;;;                 objects.  If NIL, then shows the intersection.
;;; 
;;;       (the rest of the slots are the same as for prop-sheet:)
;;;	  :v-spacing
;;;       :multi-line-p
;;;       :select-label-p
;;; 	  :label-selected-func
;;;       :label-select-event
;;;       :select-value-p
;;; 	  :value-selected-func
;;;	  :single-select-p
;;;
;;;     Read-only (output) slots (same as Prop-Sheet)
;;; 	   :label-selected
;;; 	   :value-selected
;;;        :value
;;;        :changed-values
;;; 
;;;  USEFUL FUNCTIONS:
;;;	ReUsePropSheet (prop-sheet-gadget new-items)
;;;        This allows you to re-use an old property sheet with a new set
;;; 	   of values, which is much more efficient than destroying and
;;; 	   creating a new prop-sheet.  NOTE: it is NOT sufficient to simply
;;; 	   s-value the items slot.
;;;
;;;     ReUsePropSheetObj (Prop-Sheet-For-Obj &optional obj slots)
;;;        This allows a Prop-Sheet-For-Obj gadget to be re-used.  If the
;;; 	   new obj and slots are not supplied, then they should be set into the
;;; 	   object before this function is called.  NOTE: it is NOT
;;;        sufficient to simply s-value the :obj and :slots slot.
;;;     Get-Val-For-PropSheet-Value (label-or-value-obj) returns the label when
;;; 	   a label is passed in, or for a value-obj, returns multiple values:
;;;  	   value label, where label is the label (name, not
;;;        object) of that field. 
;;; 
;;; 
;;;  Designed by A. Bryan Loyall
;;;  Extensively modified by Pavan Reddy and Brad A. Myers


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Prop-Sheet ReUsePropSheet prop-sheet-for-obj
	    ReUsePropSheetObj Get-Val-For-PropSheet-Value
	    Set-Val-For-PropSheet-Value)))


(defun Prop-Sheet-OK (prop-gadget item)
  (declare (ignore item))
  (kr-send prop-gadget :OK-Function prop-gadget))

(defun Prop-Sheet-Apply (prop-gadget item)
  (declare (ignore item))
  (kr-send prop-gadget :Apply-Function prop-gadget))

(defun Prop-Sheet-Cancel (prop-gadget item)
  (declare (ignore item))
  (kr-send prop-gadget :Cancel-Function prop-gadget))

(defun Win-Go-Away (gadget item)
  (unless (string= item "Apply")
    (let ((win (g-value gadget :window)))
      (when win
	(s-value win :visible NIL)
	(opal:update win)))))

(defun Prop-OK-Function (ok-gadget item)
  (let ((prop-gadget (g-value ok-gadget :parent)))
    (cond ((string= item "OK") (Prop-Sheet-OK prop-gadget item))
	  ((string= item "Apply") (Prop-Sheet-Apply prop-gadget item))
	  ((string= item "Cancel") (Prop-Sheet-Cancel prop-gadget item))
	  (T (error "Bad prop sheet return value")))
    (when (g-value prop-gadget :win-go-away)
      (win-go-away prop-gadget item))))

(defun Prop-Done-Function (done-gadget item)
  (let ((prop-gadget (g-value done-gadget :parent)))
    (kr-send prop-gadget :done-Function prop-gadget)
    (when (g-value prop-gadget :win-go-away)
      (win-go-away prop-gadget item))))

(defun Obj-Prop-Sheet-OK (prop-gadget item)
  (declare (ignore item))
  (let ((obj-or-objs (g-value prop-gadget :obj))
	(slot-values (g-value prop-gadget :value)))
    (dolist (slot-val slot-values)
      (Prop-Set-Objs-Slots obj-or-objs (car slot-val) (cadr slot-val)
			   NIL T)) ;; don't want any error reports now
    (kr-send prop-gadget :OK-Function prop-gadget)))

(defun Obj-Prop-Sheet-Apply (prop-gadget item)
  (declare (ignore item))
  (let ((obj-or-objs (g-value prop-gadget :obj))
	(slot-values (g-value prop-gadget :value)))
    (dolist (slot-val slot-values)
      (Prop-Set-Objs-Slots obj-or-objs (car slot-val) (cadr slot-val)
			    NIL T)) ;; don't want any error reports now
    (kr-send prop-gadget :Apply-Function prop-gadget)))

(defun Prop-Obj-OK-Function (ok-gadget item)
  (let ((prop-gadget (g-value ok-gadget :parent)))
    (cond ((string= item "OK") (Obj-Prop-Sheet-OK prop-gadget item))
	  ((string= item "Apply") (Obj-Prop-Sheet-Apply prop-gadget item))
	  ((string= item "Cancel") (Prop-Sheet-Cancel prop-gadget item))
	  (T (error "Bad obj prop sheet return value")))
    (when (g-value prop-gadget :win-go-away)
      (win-go-away prop-gadget item))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now have some useful functions for windows.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Pop-Up-Win-For-Prop (prop-gadget-with-ok left top title
						&optional modal-p)
  (let ((win (g-value prop-gadget-with-ok :window)))
    (unless win
      (setq win (create-instance NIL inter:interactor-window
		 (:width 50)
		 (:height 50)
		 (:aggregate (create-instance NIL opal:aggregate))
		 (:background-color (g-value prop-gadget-with-ok
					     :foreground-color))))
      (s-value prop-gadget-with-ok :win-go-away T)
      (opal:add-component (g-value win :aggregate) prop-gadget-with-ok))
    (s-value win :width (+ 50 (g-value prop-gadget-with-ok :width)))
    (s-value win :height (+ 10 (g-value prop-gadget-with-ok :height)))
    (s-value win :left left)
    (s-value win :title title)
    (s-value win :icon-title title)
    (s-value win :top top)
    (s-value win :modal-p modal-p)
    (s-value win :visible T)
    (opal:update win)
    win))

;;;This will set up the property sheet object with the specified object
;;; and slot and display it
(defun Pop-Up-Win-Change-Obj (prop-gadget-with-ok obj slots left top title
						  &optional modal-p)
  (s-value prop-gadget-with-ok :obj obj)
  (s-value prop-gadget-with-ok :slots slots)
  (when slots
    (mark-as-changed prop-gadget-with-ok :slots)) ; needed in case
				  ; setting with same 
				  ; list as before, but values of slots have
				  ; changed, so need to re-evaluate the
				  ; formula in :items that depends on :slots
  (ReUsePropSheetObj (g-value prop-gadget-with-ok :propsheet))
  (Pop-Up-Win-For-Prop prop-gadget-with-ok left top title modal-p))

;;;This will set up the property sheet object with the specified object
;;; and slot and display it
(defun Pop-Up-Win-Change-Items (prop-gadget-with-ok new-items left top title
						&optional modal-p)
  (s-value prop-gadget-with-ok :items new-items)
  (ReUsePropSheet (g-value prop-gadget-with-ok :propsheet) new-items)
  (Pop-Up-Win-For-Prop prop-gadget-with-ok left top title modal-p))


(defparameter bold-propsheet-font (opal:get-standard-font NIL :bold NIL))
(defparameter propsheet-font (opal:get-standard-font NIL NIL NIL))
(defparameter italic-propsheet-font (opal:get-standard-font NIL :italic NIL))
(defparameter bold-italic-propsheet-font
  (opal:get-standard-font NIL :bold-italic NIL))

(defparameter choose-propsheet-font-formula
  (o-formula (cond
	      ((and (gvl :selected) (gvl :invalid))
	       bold-italic-propsheet-font)
	      ((gvl :selected)
	       bold-propsheet-font)
	      ((gvl :invalid)
	       italic-propsheet-font)
	      (t propsheet-font))))

(create-instance 'label-proto opal:text
   (:invalid nil)
   (:selected NIL)
   (:top (o-formula (gvl :parent :top)))
   (:left (o-formula (gvl :parent :left)))
   (:comment NIL) ; if supplied, then is a used as a comment after the label
   (:string (o-formula (let ((comment (gvl :comment))
			     (str (string (gvl :value))))
			 (if comment 
			     (concatenate 'string str " " comment ":")
			     (concatenate 'string str ":")))))
   (:font (formula choose-propsheet-font-formula)))

(create-instance 'VALUE-PROTO opal:cursor-text
   (:invalid nil)
   (:selected NIL)
   (:label-obj NIL) ; set with a reference to the label object
   (:left (o-formula (+ 10 (opal:gv-right (gvl :label-obj)))))
   (:top (o-formula (gvl :parent :top)))
;   (:height (o-formula (opal:string-height (gvl :font) "X")))
   (:font (formula choose-propsheet-font-formula)))

(defun Get-Val-For-PropSheet-Value (label-or-value-obj)
  (if (is-a-p label-or-value-obj value-proto)
      (values (g-value label-or-value-obj :value)
	      (g-value label-or-value-obj :label-obj :value))
      ; else is a label, just return the name
      (g-value label-or-value-obj :value)))

(defun Set-Val-For-PropSheet-Value (label-or-value-obj new-value)
  (let ((value-obj (cond ((is-a-p label-or-value-obj value-proto)
			  label-or-value-obj)
			 ((is-a-p label-or-value-obj label-proto)
			  (g-value label-or-value-obj :label-obj :value))
			 ((is-a-p label-or-value-obj opal:view-object)
			  ;; then let's hope it is a gadget and that
			  ;; it's value can be set!
			  label-or-value-obj)
			 (T (error "param not value, label or gadget")))))
    (s-value value-obj :value new-value)))


;; This aggregate holds a label-value pair
(create-instance 'label-value-proto opal:aggregadget
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (if (gv-local :self :prev)
			(+ (gvl :parent :v-spacing)
			   (opal:gv-bottom (gv-local :self :prev)))
			(gvl :parent :top))))
   (:width (o-formula (+ (or (gvl :label-obj :width) 0)
			 (or (gvl :value-obj :width) 0))))
   (:label-obj NIL) ;label for this entry
   (:value-obj NIL) ;value for this entry.
                    ;Will be value-proto or a gadget
   (:next NIL) ; set with the next label-value object, or NIL if last one
   (:value (o-formula (let ((value-obj (gvl :value-obj)))
			(list (gvl :label-obj :value)
				  (gv value-obj :value))))))

;;;;;;;;;;;  Reusing parts of the property sheet and gadgets

(defparameter UnUsedLabelValueAggs NIL)
(defparameter UnUsedLabelAggs NIL)
(defparameter UnUsedValueAggs NIL)  ;; list of value-proto's to re-use
(defparameter UnUsedListGadgets NIL)

(defun ReUseLabelValueAgg (labelvalue)
  (push labelvalue UnUsedLabelValueAggs))
(defun ReUseLabelAgg (label)
  (push label UnUsedLabelAggs))
(defun ReUseValueAgg (value)
  (push value UnUsedValueAggs))

;; creates or reuses a value-proto object
(defun GetValueAgg ()
  (let ((value-agg (or (pop UnUsedValueAggs)
		       (create-instance nil value-proto))))
    (s-value value-agg :invalid nil) ; make sure isn't on in case re-used one
    (s-value value-agg :selected nil) 
    value-agg))
(defun GetLabelAgg ()
  (or (pop UnUsedLabelAggs)
      (create-instance nil label-proto)))
(defun GetLabelValueAgg ()
  (or (pop UnUsedLabelValueAggs)
      (create-instance nil label-value-proto)))

;;; used as the selection function in a horiz-choice-list gadget
(defun Choice-list-set-obj-val (choice-gadget val)
  (let ((prop-sheet (g-value choice-gadget :prop-sheet)))
    (when (g-value prop-sheet :set-immediately-p)
      (let ((slot (g-value choice-gadget :on-slot))
	    (obj-or-objs (g-value prop-sheet :obj))
	    (error-gadget (g-value prop-sheet :error-gadget)))
	(Prop-Set-Objs-Slots obj-or-objs slot val error-gadget T)))))

;;; reuse a gadget with the same list of items
(defun GetListGadget (list-of-values newval prop-sheet slot)
  (let (gadget)
    (dolist (lg UnUsedListGadgets)
      (when (equal list-of-values (g-value lg :items))
	(setq gadget lg)
	(setq UnUsedListGadgets (delete gadget UnUsedListGadgets))
	(return)))
    (unless gadget
      ;; Horiz-Choice-List defined in prop-value-gadgets
      (setq gadget (create-instance NIL Horiz-Choice-List
		     (:constant T :except :left :top)
		     (:items list-of-values)
		     (:selection-function #'Choice-list-set-obj-val)
		     (:*prop-created-list* T))))
    (g-value gadget :value)
    (s-value gadget :value newval)
    (s-value gadget :prop-sheet prop-sheet)
    (s-value gadget :on-slot slot)
    gadget))

(defun ReUseGadget (gadget)
  (let (proto)
    (cond ((g-value gadget :*prop-created-list*)
	   ;; then is a LIST gadget
	   (push gadget UnUsedListGadgets))
	  ((setq proto (g-value gadget :*prop-created-from*))
	   ;; then is a gadget created from the type list
	   (push gadget (g-value proto :*prop-unused-instances*)))
	  ;; otherwise, a user-supplied gadget, don't destroy it
	  (T NIL))))

;;; Allocate a new gadget from the type list
(defun GetGadget (proto newval)
  (let ((gad (or (pop (g-value proto :*prop-unused-instances*))
		 (create-instance NIL proto
				  (:*prop-created-from* proto)))))
    (g-value gad :value)
    (s-value gad :value newval)
    gad))

(defun default-filter (prop-sheet-gadget label value-obj new-str old-str)
  (declare (ignore prop-sheet-gadget label value-obj old-str))
  new-str)

;; This is called by the text interactor after a string is finished being
;; edited.  It calls the filter function, if any and processes the results.
;; Obj is a value obj.
(defun After-Edit-Prop-Sheet-Func (inter value-obj event string x y)
  (declare (ignore event x y))
  (let* ((old-string (g-value value-obj :old-string))
	 (new-string (copy-seq string))
	 (gadget (g-value inter :operates-on))
	 (changed-values (g-value gadget :changed-values))
	 (label (g-value value-obj :label-obj :value)))
    (s-value inter :obj-to-change NIL) ; just in case started via a tab, so
				       ; next time will start from the mouse
    (multiple-value-bind (result invalid filteredstring)
			 ;; the function to call is stored in the value-obj.
		 (kr-send value-obj :filter gadget label value-obj
			  new-string old-string)
      (s-value value-obj :value result)
      (if filteredstring ; if filter returns a string, then use and remember it
	  (progn 
	    (s-value value-obj :old-string filteredstring)
	    (s-value value-obj :string filteredstring))
	  ; else remember the original string
	  (s-value value-obj :old-string new-string))
      ;; now set the changed-values slot
      (let (triple-found)
	(dolist (v changed-values)
	  (when (eq (car v) label)
	    ; then just modify the result
	    (setf (cadr v) result)
	    (setf triple-found T)
	    (mark-as-changed gadget :changed-values) ; since new list will be
						     ; eq to old one
	    (return)))
	(unless triple-found   ;  add a new triple
	  (push (list label result) changed-values)
	  (s-value gadget :changed-values changed-values)))
      ;; now, if filter returns invalid, set italic
      (s-value value-obj :invalid invalid))))

(defparameter *tab-event* (inter:make-event :char :leftdown :code 1 :mousep T
					    :downp T))

(defun Handle-Tab (inter value-obj event)
  (declare (ignore event))
  (let* ((parent (g-value value-obj :parent))
	 (label-value-obj parent)
	 (gadget (g-value label-value-obj :parent)) 
	 next-label-value-proto next)
    (loop ; loop until find a non-gadget field
     (setq next-label-value-proto (g-value label-value-obj :next))
     (unless next-label-value-proto
       (setq next-label-value-proto
	     (car (last (g-value gadget :components)))))
     (if (g-value next-label-value-proto :is-gadget)
	 (setq label-value-obj next-label-value-proto) ; find next one
	 (return)))
    (setq next (g-value next-label-value-proto :value-obj))
    (inter:stop-interactor inter)
    (setf (inter:event-window *tab-event*) (g-value gadget :window))
    (setf (inter:event-x *tab-event*) (+ (g-value next :left) 1))
    (setf (inter:event-y *tab-event*) (+ (g-value next :top) 2))
    (s-value inter :cursor-where-press NIL) ; so cursor will be at end of field
    (s-value inter :obj-to-change next) ; this is cleared by the final function
    (inter:start-interactor inter *tab-event*)
    (s-value inter :cursor-where-press T))) ; restore so if user presses

(create-instance 'prop-sheet opal:aggregadget
   ; Customizable slots
    (:maybe-constant '(:left :top :items :default-filter :v-spacing
		       :multi-line-p :select-label-p :visible
		       :label-selected-func :label-select-event
		       :select-value-p :value-selected-func :single-select-p))
    (:left 5) (:top 5)
    (:items NIL)
    (:default-filter 'default-filter)
    (:v-spacing 1)
    (:multi-line-p NIL) ; T if multi-line strings are allowed
    (:select-label-p NIL) ; T if want to be able to select the entries
    (:label-select-event :any-mousedown)
    (:label-selected-func NIL)
    (:select-value-p NIL)
    (:value-selected-func NIL)
    (:single-select-p NIL)

      ; read-only slots
    (:label-selected NIL) ; set with the selected label objects (or a list)
    (:value-selected NIL) ; set with the selected value objects (or a list)
    (:value (o-formula  ; list of pairs of all the slots and their (filtered) values
	     (let ((components (gvl :components)))
	       ;; create a list of the values of all my components.  Each
	       ;; component will be a label-value-proto
	       (mapcar #'(lambda (item)
			   (gv item :value)) components))))
    (:changed-values NIL) ; set explicitly by After-Edit-Prop-Sheet-Func

      ; internal slots
    (:interactors
     `((:editor ,inter:text-interactor
        (:start-where ,(o-formula
                        (list :leaf-element-of (gvl :operates-on)
                              :type value-proto)))
        (:start-event :leftdown)
        (:stop-event ,(o-formula (if (gvl :operates-on :multi-line-p)
				     ;; if multi-line, then allow returns
				     (list :leftdown :control-\j)
				     ;; otherwise, stop on return
				     (list :leftdown #\return :control-\j))))
	(:final-function After-Edit-Prop-Sheet-Func)
        (:window ,(o-formula (gv-local :self :operates-on :window))))
       (:label-selector ,inter:button-interactor
	(:active ,(o-formula (gvl :operates-on :select-label-p)))
	(:start-event ,(o-formula (gvl :operates-on :label-select-event)))
        (:start-where ,(o-formula
                        (list :leaf-element-of (gvl :operates-on)
                              :type label-proto)))
	(:how-set ,(o-formula (if (gvl :operates-on :single-select-p)
				 :set :list-toggle)))
	(:final-function
	 ,#'(lambda(inter obj)
	      (let ((gadget (g-value inter :operates-on)))
		(if (g-value gadget :single-select-p)
		    (progn
		      (s-value gadget :label-selected obj)
		      ; make sure no values are selected
		      (s-value gadget :value-selected NIL)) 
		    (toggle-in-obj-list gadget :label-selected obj))
		(kr-send gadget :label-selected-func gadget obj
			 (g-value obj :value)))))
        (:window ,(o-formula (gv-local :self :operates-on :window))))
       (:value-selector ,inter:button-interactor
	(:start-event :rightdown)
	(:active ,(o-formula (gvl :operates-on :select-value-p)))
        (:start-where ,(o-formula
                        (list :leaf-element-of (gvl :operates-on)
                              :type value-proto)))
	(:how-set ,(o-formula (if (gvl :operates-on :single-select-p)
				 :set :list-toggle)))
	(:final-function
	 ,#'(lambda(inter obj)
	      (let ((gadget (g-value inter :operates-on)))
		(if (g-value gadget :single-select-p)
		    (progn
		      (s-value gadget :value-selected obj)
		      (s-value gadget :label-selected NIL)) ; make sure no labels
							    ; are selected
		    (toggle-in-obj-list gadget :value-selected obj))
		(kr-send gadget :value-selected-func gadget obj
		       (g-value obj :value)(g-value obj :label-obj :value)))))
        (:window ,(o-formula (gv-local :self :operates-on :window)))))))

(inter:bind-key #\tab 'Handle-Tab (g-value prop-sheet :editor))

(defun toggle-in-obj-list (obj slot val)
  (let ((oldval (g-value obj slot)))
    (if (member val oldval)
	(progn
	  (s-value obj slot (delete val oldval))
	  (Mark-As-Changed obj slot))
	(push val (g-value obj slot)))))

;; some of the slots should not be inherited.
(s-value prop-sheet :local-only-slots
	 (append '((:label-selected NIL) (:value-selected NIL)
		   (:changed-values NIL))
		 (g-value opal:aggregadget :local-only-slots)))

(define-method :initialize prop-sheet (self)
  (call-prototype-method self)
  (let ((def-filter (g-value self :default-filter))
	prev this)
    (dolist (line (g-value self :items))
      (setq this (add-label-values self line def-filter))
      (when prev (s-value prev :next this))
      (setq prev this))))

;;Helper function for add-label-values.
;; real-val will be :*Not-supplied* if not supplied since NIL is a valid value
(defun Set-Up-Value-Obj (value-obj filter label init-value real-val invalid)
  (s-value value-obj :filter filter)
  (s-value value-obj :label-obj label)
  (s-value value-obj :invalid invalid)
  (s-value value-obj :string init-value)
  (let ((val (g-value value-obj :string))) ; in case init-value is a formula
    (s-value value-obj :old-string val)
    (s-value value-obj :value 
	     (if (eq real-val :*Not-supplied*) val real-val))))


(defparameter topformgadget (o-formula (gvl :parent :top)))
(defparameter leftformgadget 
  (o-formula (+ 10 (opal:gv-right (gvl :parent :label-obj)))))

;;; The main procedure that adds the labels and values to the gadget.  This
;;; is called for each label, value pair.  Returns the label-value-proto
;;; object, so the caller can set the next field of the previous one.
;;;
;;; The format for entries is:
;;;    (label init-val-string-or-gadget [filter [real-val [comment [f [i]]]]])
;;;  where f and i are used internally and f is an extra filter and i is
;;;  where the entry starts off IN-valid (T means invalid, NIL is OK)
(defun add-label-values (a-prop-sheet entry def-filter)
  (let ((label (first entry))
	(init-value (second entry))
	(filter (or (third entry) def-filter))
	(real-val (if (>= (length entry) 4)
		      (fourth entry)
		      :*Not-supplied*))
	(comment (fifth entry))
	(other (sixth entry))
	(initially-invalid (seventh entry))
	(label-agg (GetLabelAgg))
	is-gadget value-agg)
    (s-value label-agg :invalid nil) ; make sure isn't on in case re-used one
    (s-value label-agg :selected nil) 
    (s-value label-agg :value label)
    (s-value label-agg :comment comment)
    (cond ((is-a-p init-value opal:view-object)  ; then is a gadget,
						 ; set its left and top
	   (setq is-gadget T) ; used below
	   (setq value-agg init-value)
	   (let ((top-val (get-value value-agg :top)))
	     (unless (and (formula-p top-val)
			  (eq (kr::a-formula-is-a top-val) topformgadget))
	       (s-value value-agg :top (formula topformgadget))
	       (s-value value-agg :left (formula leftformgadget)))))
	  ((listp init-value)
	   (error "Prop sheet no longer handles multiple values"))
	  (T ; regular value, string or formula
	   (setq value-agg (GetValueAgg))
	   (s-value value-agg :other-prop-filter other) ;used by prop-sheet-obj
	   (Set-Up-Value-Obj value-agg filter label-agg init-value real-val
			     initially-invalid)))

    ;; now put these two into a label-value-proto aggregate
    (let ((agg (GetLabelValueAgg)))
      (s-value agg :label-obj label-agg)
      (s-value agg :is-gadget is-gadget)
      (s-value agg :value-obj value-agg)
      (s-value agg :prev (car (get-local-value a-prop-sheet :components)))
      ;; now add the label and value objects to the aggregate
      (let ((kr::*constants-disabled* T))
	(opal:add-components agg label-agg value-agg)
	;; add the agg to the end of the property-sheet
	(opal:add-component a-prop-sheet agg :head))
      agg)))

(declaim (special PROP-SHEET-WITH-OK MOTIF-PROP-SHEET-WITH-OK
		  prop-sheet-for-obj-with-OK
		  motif-prop-sheet-for-obj-with-OK
		  motif-prop-sheet-for-obj-with-done))

(defun ReUsePropSheet (gadget items)
  "Call this to set a new set of items into a prop-sheet gadget"
  (when (or (and (boundp 'motif-prop-sheet-for-obj-with-done)
		 (is-a-p gadget motif-prop-sheet-for-obj-with-done))
	    (and (boundp 'prop-sheet-with-OK)
		 (is-a-p gadget prop-sheet-with-OK))
	    (and (boundp 'motif-prop-sheet-with-OK)
		 (is-a-p gadget motif-prop-sheet-with-OK))
	    )
    ;; then set the items slot and actually operate on the sub-prop-sheet
    (s-value gadget :items items)
    (setq gadget (g-value gadget :propsheet)))
  (unless (is-a-p gadget prop-sheet)
    (error "Object ~s passed to ReUsePropSheet must be is-a-p prop-sheet"
	   gadget))
  (CleanupPropSheet gadget) ; remove old values, if any
  (let ((def-filter (g-value gadget :default-filter))
	prev this)
    (s-value gadget :items items)
    (s-value gadget :changed-values NIL)
    (s-value gadget :label-selected NIL)
    (s-value gadget :value-selected NIL)
    (s-value gadget :selected NIL)
    (dolist (line items)
      (setq this (add-label-values gadget line def-filter))
      (when prev (s-value prev :next this))
      (setq prev this))))

(defun CleanupPropSheet (gadget)
  (dolist (label-value-agg (copy-list (g-value gadget :components)))
    (dolist (val-or-label (copy-list (g-value label-value-agg :components)))
      (cond ((is-a-p val-or-label label-proto)
	     (ReUseLabelAgg val-or-label))
	    ((is-a-p val-or-label value-proto)
	     (ReUseValueAgg val-or-label))
	    (T (ReUseGadget val-or-label))) ; otherwise, gadget for the value
      (with-constants-disabled
	  (opal:remove-local-component label-value-agg val-or-label)))
    (ReUseLabelValueAgg label-value-agg)
    (with-constants-disabled
	(opal:remove-local-component gadget label-value-agg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now deal with Object property sheets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Mk-String (val eval-p)
  (if eval-p ;"smart" convert to a string.
      (let ((str (format NIL "~s" val)))
	;; for KR objects, remove the #k part
	(if (and (> (length str) 4)
		 (string= (subseq str 0 3) "#k<"))
	    (subseq str 3 (1- (length str)))
	    str))
      ; otherwise, just use as a string
      (if (stringp val) val ; just use the string
	  (format NIL "~a" val))))
      
;;; This function is a formula in the :items slot of a prop-sheet-for-obj
;;; Format for slot list is :slot or (:slot list-or-gadget-or-func "comment")
(defun Prop-Sheet-Obj-Create-Items (gadget)
  (let ((slots (gv gadget :slots))
	(obj-or-objs (gv gadget :obj))
	(eval-p (gv gadget :eval-p))
	invalid item-list val stringval
	comment list-or-gadget slot-filter slotname)

    (when obj-or-objs ; otherwise :items will be NIL
      ;; when a list of one element, use the object itself
      (when (and (listp obj-or-objs)
		 (= 1 (length obj-or-objs)))
	(setq obj-or-objs (car obj-or-objs)))

      (if slots
	  (progn ;; then check each slot to see if should be special, and
		 ;; generate the descriptor for it
	    (dolist (slot slots)
	      (setq invalid NIL)
	      (setq slot-filter NIL)

	      
	      (if (listp slot) ; then slot is actually a descriptor
		  (setq slotname (first slot))
		  (setq slotname slot)) ; else slot is just a name
	      
	      ;; get the value for the slot
	      (if (listp obj-or-objs)
		  (progn 
		    ;; ** Only do the ones for which it is a parameter??
		    (setq val (g-value (car obj-or-objs) slotname))
		    (dolist (obj (cdr obj-or-objs))
		      (unless (equal (g-value obj slotname) val)
			(setq invalid T)
			(return))))
		  ;; else a single object
		  (setq val (g-value obj-or-objs slotname)))

	      (if (listp slot)
		  (progn		; then slot is actually a descriptor
		    (setq list-or-gadget (second slot))
		    (setq comment (third slot))
		    (cond ((or (functionp list-or-gadget)
			       (symbolp list-or-gadget))
			   (setq slot-filter list-or-gadget)
			   (setq list-or-gadget NIL))
			  ((and list-or-gadget (listp list-or-gadget))
					;need AND since NIL is a list
			   (setq list-or-gadget
				 (GetListGadget list-or-gadget val
						gadget slotname)))
			  (T ;; otherwise list-or-gadget is a gadget
			   (s-value list-or-gadget :value val)))
		    )

		  (progn  ;; otherwise, not a list or gadget
		    (setq comment NIL)
		    (setq list-or-gadget NIL)))

		  (if list-or-gadget 
		      ;; use the gadget
		      (push (list slotname list-or-gadget NIL :*Not-supplied*
				  comment) 
			    item-list)
		      (progn ;; else use value
			(setq stringval (Mk-String val eval-p))
			(push (list slotname stringval NIL val
				    comment slot-filter invalid)
			      item-list))))
	    (setq item-list (nreverse item-list))
	    item-list)

	  (progn ;; else no slots supplied, generate from :parameters
	    (setq slots (Generate-Slots-From-Params obj-or-objs gadget eval-p))
	    slots)))))
      

;;; Generates a list of the slots of the objects which are parameters.
;;; For each, returns a list of the appropriate form:
;;; either (list slot list-or-gadget NIL :*Not-supplied* comment)
;;; or (list slot stringval NIL val comment slot-filter invalid)
(defun Generate-Slots-From-Params (obj-or-objs prop-sheet eval-p)
  (let ((type-gadgets (g-value prop-sheet :type-gadgets))
	(union? (g-value prop-sheet :union?))
	slot-descs desc)
    ;; if list of length one, just use object
    (when (and (listp obj-or-objs)
	       (= 1 (length obj-or-objs)))
      (setq obj-or-objs (car obj-or-objs)))
    (if (listp obj-or-objs)
	(let (objs-slots)
	  ;; make objs-slots as a list of ( (obj1 (slots for obj1)) (obj2 ...))
	  (dolist (obj obj-or-objs)
	    (push (list obj (g-value obj :parameters)) objs-slots))
	  ;; can't use built-in union or intersection functions since want
	  ;; to preserve the list ordering
	  (if union?
	      ;; generate a list of ( (slot obj val invalid) ... )
	      ;; then go through that list and generate the descriptors
	      (let (slots-so-far obj a)
		(dolist (obj-slots objs-slots)
		  (setq obj (car obj-slots))
		  (dolist (slot (cadr obj-slots))
		    (if (setq a (assoc slot slots-so-far))
			;; then check if should be invalid because vals diff
			(unless (equal (third a)
				       (g-value obj slot))
			  (setf (fourth a) T))
			;; else create a new entry
			(push (list slot obj (g-value obj slot) NIL)
			      slots-so-far))))
		;; now go through slots-so-far and generate descriptors
		(dolist (slotd slots-so-far)
		  (setq desc (Generate-Slot-Desc (second slotd)
						 (first slotd)
						 (third slotd)
						 (fourth slotd)
						 type-gadgets
						 eval-p
						 prop-sheet))
		  (when desc (push desc slot-descs))))

	      ;;else INTERSECTION
	      (let ((first-obj (caar objs-slots))
		    (slots (second (first objs-slots)))
		    (second-slots (second (second objs-slots)))
		    slotd val invalid)
		;; make second-slots be the intersection of all the
		;; slots from the second through last object, and then
		;; remove from the first slot list any that aren't in
		;; the second slot list.
		(dolist (obj-slots (cddr objs-slots))
		  (setq second-slots (intersection second-slots
						   (second obj-slots))))
		;; now remove from slots any that aren't in second-slots
		(dolist (s slots)
		  (unless (member s second-slots)
		    (setq slots (remove s slots))))
		;; now slots list is OK, check if any values differ
		(dolist (s slots)
		  (setq val (g-value (first obj-or-objs) s))
		  (setq invalid NIL)
		  (dolist (obj (cdr obj-or-objs))
		    (unless (equal val (g-value obj s))
		      (setq invalid T)
		      (return)))
		  (push (list s val invalid) slotd))
		;; slotd is a list of ( (slot val invalid?) ...)
		;; now get the descriptors
		(dolist (sd (nreverse slotd))
		  (setq desc (Generate-Slot-Desc first-obj (first sd)
						 (second sd)
						 (third sd)
						 type-gadgets
						 eval-p 
						 prop-sheet))
		  (when desc (push desc slot-descs)))
		(setq slot-descs (nreverse slot-descs)))))
	
	;; else is a single object
	(progn
	  (dolist (slot (g-value obj-or-objs :parameters))
	    (setq desc (Generate-Slot-Desc obj-or-objs slot
					   (g-value obj-or-objs slot)
					   NIL type-gadgets eval-p prop-sheet))
	    (when desc (push desc slot-descs)))
	  (setq slot-descs (nreverse slot-descs))))
    slot-descs))

#+sbcl
(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))
(#+sbcl define-constant
 #-sbcl defconstant
 tnil (list T NIL))

;;; Returns NIL if slot should be omitted, or else a description
;;; of the appropriate form (depending on whether a gadget or not)
;;; either (list slot gadget NIL :*Not-supplied* comment)
;;; or (list slot stringval NIL val comment slot-filter invalid)
(defun Generate-Slot-Desc (obj slot val invalid? type-gadgets eval-p
			       prop-sheet)
  (let* ((typ (g-type obj slot))
	 (base-type (kr::GET-TYPE-DEFINITION typ))
	 (comment (kr:get-type-documentation typ))
	 gad)

    (when comment
      (setq comment (substitute #\space #\newline comment)) ;; one line only
      (when (> (length comment) 25) ;; forget it
	(setq comment NIL)))
    (cond ((assoc slot type-gadgets)
	   ;; if the SLOT is in type-gadgets, then should be omitted
	   (return-from Generate-Slot-Desc NIL))
	  ((setq gad (assoc typ type-gadgets :test #'equal))
	   (if (schema-p (cadr gad))
	       ;; allocate a gadget like the one in the list
	       (return-from Generate-Slot-Desc
		 (list slot (Getgadget (cadr gad) val) NIL
		       :*Not-supplied* NIL)) ; don't use comment if gadget
	       ;; else use gad as the type instead of the real type
	       (setq typ (cadr gad)))))
    (cond
      ;; kr-boolean is T NIL
      ((eq typ 'KR-BOOLEAN)
       (list slot (GetListGadget tnil (if val T NIL) prop-sheet slot)
	     NIL :*Not-supplied* NIL)) ; don't use comment from T or NIL's
      ;; handle member (enumerated type)
      ((and (listp typ)
	    (eq (car typ) 'member)
	    (<= (length (cdr typ)) 5))
       (setq gad (GetListGadget (cdr typ) val prop-sheet slot))
       (list slot gad NIL :*Not-supplied* NIL)) ; don't use comment
      ((and (listp base-type)
	    (eq (car base-type) 'member)
	    (<= (length (cdr base-type)) 5))
       (setq gad (GetListGadget (cdr base-type) val prop-sheet slot))
       (list slot gad NIL :*Not-supplied* NIL))  ; don't use comment
      ;; no special gadgets for other values
      (T (list slot (Mk-String val eval-p) NIL val comment
	       NIL invalid?)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The main function that gets the slots out of the objects.  gadget
;;; is the prop-sheet.
(defun Obj-Read-Filter (gadget label value-obj new-str old-str)
  (declare (ignore old-str)) 
  (let ((extra-filter (g-value value-obj :other-prop-filter))
	(eval-p (g-value gadget :eval-p))
	(set-immediately-p (g-value gadget :set-immediately-p))
	(error-gadget (g-value gadget :error-gadget)))
    ;; NOTE: It would be nice if we could replace the two occurrences of
    ;; Careful-Eval with one call to Careful-String-Eval.  However, we
    ;; only want to eval the val if it is a symbol -- Careful-String-Eval
    ;; evals it in all cases.  Rationale:  When the prop sheet displays
    ;; a list, it does not display the quote at the beginning of the list.
    ;; We want the user to be able to type in a list without needing to
    ;; quote it.
    (if eval-p ; then try to find the value of the string
	(multiple-value-bind (val error?)
	    (if (string= "" new-str)
		(values NIL NIL)
		(gg:careful-read-from-string new-str error-gadget))
	  ;; special hack that evals the value if there is no error.  Allows
	  ;; computations and also the use of atom names for objects
	  (if error?
	      ;; unable to read string
	      (progn (inter:beep) (values new-str T new-str))
	      ;; else read ok
	      (progn
		;; check if can evaluate again. OK if this fails
		(multiple-value-bind (new-val new-error?)
		    (gg:careful-eval val)
		  (unless new-error?
		    (setq val new-val)
		    (setq new-str (Mk-String val T))))
		(when extra-filter
		  (setq error? (funcall extra-filter val))
		  (when error?
		    (inter:beep)
		    (return-from obj-read-filter (values new-str T new-str))))
		(if (Prop-Set-Objs-Slots (g-value gadget :obj) label val
					 error-gadget set-immediately-p)
		    (values val NIL new-str) ; fine
		    (progn ; type error in setting slot
		      (inter:beep)
		      (values new-str T new-str))))))
	
	(progn ; here is not eval-p so just use the value as specified
	  (if (Prop-Set-Objs-Slots (g-value gadget :obj) label new-str
				   error-gadget set-immediately-p)
	      new-str
	      (progn ; type error in setting slot
		(inter:beep)
		(values new-str T new-str)))))))

;;; Check if can set the slot of object or objects, and set if
;;; set-now.  Returns T if OK, NIL if error.  When multiple objects,
;;; checks the parameters slot of each to see whether should set or not.
(defun Prop-Set-Objs-Slots (obj-or-objs slot val error-gadget set-now)
  (with-constants-disabled
      (if (listp obj-or-objs)
	  (let (error? type-error fail-objs one-success)
	    (dolist (obj obj-or-objs)
	      (when (member slot (g-value obj :parameters))
		(if (stringp (setq error?
				   (kr:check-slot-type obj slot val NIL)))
		    (progn
		      (setq type-error error?)
		      (push obj fail-objs))
		    (progn
		      (setq one-success T)
		      (when set-now (s-value obj slot val))))))
	    (if one-success
		(progn
		  (when (and fail-objs error-gadget)
		    ;; report which slots failed
		    (display-error error-gadget
		      (format NIL "Value ~s is illegal as the ~s of ~s
due to type error so not set,
but is ok for ~s
so set there"      val slot fail-objs
                  (set-difference obj-or-objs fail-objs))))
		  ;; when at least one-success, return T
		  T) 
		(progn ;; no successes, use the last type-error string
		  (when error-gadget
		    (display-error error-gadget type-error))
		  ;; return NIL
		  NIL)))

	  ;; here, not a list of objects, don't check the :parameters slot
	  (let ((error? (kr:check-slot-type obj-or-objs slot val NIL)))
	    (if (stringp error?)
		(progn
		  (when error-gadget
		    (display-error error-gadget error?))
		  NIL) ; return NIL
		(progn ;; else OK
		  (when set-now (s-value obj-or-objs slot val))
		  T))))))


(create-instance 'prop-sheet-for-obj prop-sheet
   ; Customizable slots
    (:maybe-constant '(:left :top :obj :slots :eval-p :set-immediately-p
		       :v-spacing :multi-line-p :select-label-p
		       :label-selected-func :label-select-event :visible
		       :select-value-p :value-selected-func :single-select-p
		       :type-gadgets :union? :error-gadget))
    (:left 5) (:top 5)
    (:obj NIL) ; a single obj or a list of objects
    (:slots NIL) ; list of slots to show
    (:eval-p T) ; if T, then evaluates what the user types.  Use T for
		  ; graphical objects.  If NIL, then all the values will be strings.
    (:set-immediately-p T) ; if T then sets slots when user hits RETURN, else doesn't
			   ; ever set the slot.
    (:union? T) ; union or intersection, if slots not supplied
    (:type-gadgets NIL) ; descriptor of special handling for types
    (:error-gadget NIL) ; an error gadget to use to report errors.
    ;; plus the rest of the slots exported by prop-sheet


    ;; internal slots
    (:items (o-formula (Prop-Sheet-Obj-Create-Items (gv :SELF))))
    (:default-filter 'Obj-Read-Filter))

(defun ReUsePropSheetObj (gadget &optional obj slots)
  "Call this after setting a new object into a obj-prop-sheet gadget"
   (when (or (and (boundp 'motif-prop-sheet-for-obj-with-done)
		  (is-a-p gadget motif-prop-sheet-for-obj-with-done))
	     (and (boundp 'prop-sheet-for-obj-with-OK)
		  (is-a-p gadget prop-sheet-for-obj-with-OK))
	     (and (boundp 'motif-prop-sheet-for-obj-with-OK)
		  (is-a-p gadget motif-prop-sheet-for-obj-with-OK)))
     ;; then set the slots and actually operate on the sub-prop-sheet
     (s-value gadget :obj obj)
     (s-value gadget :slots slots)
     (mark-as-changed gadget :slots)	; needed in case setting with same
					; list as before, but values of slots have
					; changed, so need to re-evaluate the
					; formula in :items that depends on :slots
     (setq gadget (g-value gadget :propsheet)))
  (unless (is-a-p gadget prop-sheet)
    (error "Object ~s in ReUsePropSheetObj must be is-a-p prop-sheet" gadget))

  (CleanupPropSheet gadget) ; in case not done already
  (let ((def-filter (g-value gadget :default-filter))
	this prev)
    (when obj
      (s-value gadget :obj obj)
      (s-value gadget :slots slots)
      (mark-as-changed gadget :slots)) ; needed in case setting with same
				       ; list as before, but values of
				       ; slots have changed, so need to
				       ; re-evaluate the formula in :items
				       ; that depends on :slots
    (s-value gadget :changed-values NIL) 
    (s-value gadget :label-selected NIL)
    (s-value gadget :value-selected NIL)
    (s-value gadget :selected NIL)
    (dolist (line (g-value gadget :items))
      (setq this (add-label-values gadget line def-filter))
      (when prev (s-value prev :next this))
      (setq prev this))))


#+garnet-test
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(prop-sheet-for-obj-Go  prop-sheet-for-obj-stop)))

#+garnet-test
(defun prop-sheet-for-obj-Go ()
  (unless (get :garnet-modules :error-gadget)
    (common-lisp-user::garnet-load "gg:error-gadget-loader"))
  (create-instance 'prop-test-error-gad error-gadget)
  (create-instance 'prop-sheet-for-obj-win inter:interactor-window
		   (:aggregate (create-instance 'prop-sheet-for-obj-agg
						opal:aggregate))
		   (:title "test prop sheet")
		   (:width 350)(:height 600))
  (create-instance 'prop-sheet-for-obj-test-obj opal:rectangle
                   :declare ((:type
			      ((member :vertical :horizontal) :direction)
			      (string :string)
			      (keyword :order)
			      (kr-boolean :bool)
			      (T :pop-up)))
		   (:left 150)(:top 225)(:width 50)(:height 40)
		   (:pop-up 5)
		   (:direction :vertical)(:order :one)(:string "foo")
		   (:bool T))

   (create-instance 'prop-sheet-for-obj-test-obj2 opal:text
                   :declare ((:type
			      ((member :vertical :horizontal) :direction)
			      (integer :order)))
		   (:left 210)(:top 225)
		   (:string "A string")
		   (:direction :vertical))


  (s-value prop-sheet-for-obj-test-obj :parameters
	   (append (g-value opal:rectangle :parameters)
		   (list :direction :order :string :bool :pop-up)))
  (s-value prop-sheet-for-obj-test-obj2 :parameters
	     (append (g-value opal:text :parameters)
		   (list :direction :order)))

  (create-instance 'prop-sheet-for-obj-pop-up Pop-Up-From-Icon
		   (:pop-up-function
		    #'(lambda (gadget)
			(format T "Gadget ~s value ~s. New value:~%" gadget
				(g-value gadget :value))
			(s-value gadget :value (read)))))

  (create-instance 'prop-sheet-for-obj-obj prop-sheet-for-obj
		   (:left 10) (:top 10)
		   (:error-gadget prop-test-error-gad)
		   (:obj prop-sheet-for-obj-test-obj)
		   (:select-label-p T)
		   (:type-gadgets `((:left NIL) ; omit :left
				    (T ,prop-sheet-for-obj-pop-up))))

  (create-instance 'prop-sheet-for-obj-obj2 prop-sheet-for-obj
		   (:left 10) (:top 270)
		   (:error-gadget prop-test-error-gad)
		   ; (:union? NIL) ;; union or intersection
		   (:obj (list prop-sheet-for-obj-test-obj
			       prop-sheet-for-obj-test-obj2))
		   (:select-label-p T))

  (opal:add-components prop-sheet-for-obj-agg prop-sheet-for-obj-test-obj
		       prop-sheet-for-obj-test-obj2
		       prop-sheet-for-obj-obj prop-sheet-for-obj-obj2)
  (opal:update prop-sheet-for-obj-win)
  #-cmu (inter:main-event-loop)
  )

#+garnet-test
(defun prop-sheet-for-obj-Stop ()
  (opal:destroy prop-sheet-for-obj-win)
  (opal:destroy prop-test-error-gad)
  (opal:destroy prop-sheet-for-obj-pop-up))

