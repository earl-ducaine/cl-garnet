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
;;;
;;;  This file supplies prop-sheet-with-OK and prop-sheet-for-obj-with-OK which
;;;  combine a property sheet with OK and Cancel buttons and functions to display
;;;  these in windows.  See the file propsheet for the basic underlying property
;;;  sheet gadgets: prop-sheet, which takes a list of values to display, and 
;;;  prop-sheet-for-obj which takes a KR object to display.
;;;
;;;  There are also some useful functions for popping up a property sheet
;;;  in a window.
;;; 
;;;
;;;  Prop-Sheet-With-OK
;;;    -- displays a list of labels and values along with OK and Cancel
;;; 	  buttons, and allows the values to be edited. The labels can optionally
;;; 	  be selectable.
;;;
;;;     Customizable slots
;;;	  :OK-Function - function called when the OK button is hit.  Defined as:
;;; 				(lambda (Prop-Sheet-With-OK-gadget)
;;;			 Typically, this would do something with the values gotten 
;;;			 from (g-value Prop-Sheet-With-OK-gadget :values) or
;;; 			 (g-value Prop-Sheet-With-OK-gadget :changed-values)
;;;	  :Apply-Function - function called when the Apply button is hit. Defined as:
;;; 				(lambda (Prop-Sheet-With-OK-gadget).
;;;			 Typically, this would do something with the values gotten 
;;;			 from (g-value Prop-Sheet-With-OK-gadget :values) or
;;; 			 (g-value Prop-Sheet-With-OK-gadget :changed-values)
;;;       :Cancel-Function - function called when Cancel button is hit.  Defined as:
;;; 				(lambda (Prop-Sheet-With-OK-gadget)
;;; 			 Programmers typically would not use this.
;;; 
;;;       (the rest of the slots are the same as for prop-sheet:)
;;;        :left, :top - Default: 0,0
;;;        :items - NIL.
;;;	   :default-filter.  The default for default-filter does nothing.
;;;	   :v-spacing - Default: 1
;;;        :multi-line-p - Default: NIL
;;;        :select-label-p - Default: NIL.
;;; 	   :label-selected-func
;;;        :label-select-event
;;;        :select-value-p - Default: NIL.
;;; 	   :value-selected-func
;;;	   :single-select-p - Default: NIL
;;; 
;;;     Read-only (output) slots (same as Prop-Sheet)
;;; 	   :label-selected
;;; 	   :value-selected
;;;        :value
;;;        :changed-values
;;; 
;;;  Prop-Sheet-For-Obj-With-OK
;;;    -- Given a list of slots for a KR object, displays the values and
;;; 	  allows them to be edited.  The labels can optionally be selectable.
;;; 	  Sets the object's slot only when OK is hit.
;;;       (So :set-immediately-p is always NIL).
;;;
;;;     Customizable slots 
;;;	  :OK-Function - function called when the OK button is hit.  Defined as:
;;; 				(lambda (Prop-Sheet-With-OK-gadget).  Since this
;;; 		         gadget will set the slots of the object automatically when
;;; 			 OK is hit (before this function is called), programmers
;;; 			 rarely need to supply a function here.
;;;	  :Apply-Function - function called when the Apply button is hit. Defined as:
;;; 				(lambda (Prop-Sheet-With-OK-gadget).  Since this
;;; 		         gadget will set the slots of the object automatically when
;;; 			 Apply is hit (before this function is called), programmers
;;; 			 rarely need to supply a function here.
;;;       :Cancel-Function - function called when Cancel button is hit.  Defined as:
;;; 				(lambda (Prop-Sheet-With-OK-gadget)
;;;
;;;       (the rest of the slots are the same as for prop-sheet-for-obj:)
;;;        :left, :top - Default: 0,0
;;;        :obj - the KR object to be displayed.
;;;        :slots - the list of slots of the object to view.  Default value:
;;; 			'(:left :top :width :height).  
;;;        :eval-p - Default=T
;;;	   :v-spacing
;;;        :multi-line-p
;;;        :select-label-p
;;;        :label-select-event
;;; 	   :label-selected-func
;;;        :select-value-p
;;; 	   :value-selected-func
;;;	   :single-select-p
;;;
;;;     Read-only (output) slots (same as Prop-Sheet)
;;; 	   :label-selected
;;; 	   :value-selected
;;;        :value - (Note that the values are set into the slots of the object
;;; 		     automatically when OK is hit).
;;;        :changed-values
;;;
;;; 
;;;  USEFUL FUNCTIONS:
;;;
;;;     Pop-Up-Win-For-Prop (prop-gadget-with-ok left top title)
;;;          Given an existing gadget, this pops up a window which will show the
;;; 	     property sheet, and will go away when the user hits either OK or
;;; 	     cancel.  The window is allocated by this function to be the correct
;;; 	     size.  This function can be called many times on the SAME gadget
;;; 	     (with the same or different values for the :items slot).  This
;;;          is much more efficient than allocating a new gadget and window
;;; 	     each time.  If the programmer has changed the :items slot, he or she
;;; 	     is responsible for calling ReUsePropSheet on it.
;;;
;;;     Pop-Up-Win-Change-Obj (prop-obj-gadget-with-ok obj slots left top title)
;;;          Given an existing gadget, it sets the obj and slot fields of the gadget
;;; 	     to the specified values, and then pops up a window displaying that
;;; 	     property sheet.  (This function calls ReUsePropSheetObj
;;;          automatically).  (Note: if you want to pop up a
;;; 	     Prop-Sheet-For-Obj-With-OK gadget without changing the obj and slot
;;; 	     fields, you can simply pass it to Pop-Up-Win-For-Prop).
;;; 
;;;     Pop-Up-Win-Change-Items (prop-gadget-with-ok new-items left top title)
;;;	     Similar to Pop-Up-Win-Change-Obj, but changes the items.
;;;

#|
============================================================
Change log:
  2/16/93  Brad Myers - fixed for changes to prop-sheet for auto-generation
  9/21/92 Andrew Mickish - Excepted :visible slot from list of constants
            in OK-panel part of gadget
  7/29/92 Andrew Mickish - Moved functions shared by this file and
            motif-prop-sheet-win.lisp into prop-sheet.lisp
  6/5/92 Brad Myers - added modal-p parameter to window functions
  2/13/92 Brad Myers - fixed for no multiple values prop-sheet; constants
                    - changed default position to 5,5 so constant works better
  1/23/91 Andrew Mickish - changed action for "Apply" in the object
                           prop-sheet-for-obj-with-OK to Obj-Prop-Sheet-Apply
 10/16/90 Brad Myers - started
============================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(prop-sheet-with-OK prop-sheet-for-obj-with-OK Pop-Up-Win-For-Prop
	    Pop-Up-Win-Change-Obj Pop-Up-Win-Change-Items)))

(create-instance 'prop-sheet-with-OK opal:aggregadget
    (:left 5) (:top 5)
    (:maybe-constant '(:left :top :items :default-filter :ok-function
		       :apply-function :Cancel-Function :v-spacing
		       :multi-line-p :select-label-p  :visible
		       :label-selected-func :label-select-event
		       :select-value-p :value-selected-func :single-select-p))
		       
    (:items NIL)
    (:default-filter #'default-filter)
    (:OK-Function NIL)
    (:Apply-Function NIL)
    (:Cancel-Function NIL)
    (:v-spacing 1)
    (:multi-line-p NIL) ; T if multi-line strings are allowed
    (:select-label-p NIL) ; T if want to be able to select the entries
    (:label-selected-func NIL)
    (:label-select-event :any-mousedown)
    (:select-value-p NIL)
    (:value-selected-func NIL)
    (:single-select-p NIL)

      ; read-only slots
    (:label-selected (o-formula (gvl :propsheet :label-selected)))
    (:value-selected (o-formula (gvl :propsheet :value-selected)))
    (:value (o-formula (gvl :propsheet :value)))
    (:changed-values (o-formula (gvl :propsheet :changed-values)))
    
      ; internal slots
    (:parts
     `((:ok-panel ,text-button-panel
	    (:left ,(o-formula (gvl :parent :left)))
	    (:top ,(o-formula (gvl :parent :top)))
	    (:CONSTANT (T :except :left :top :visible))
	    (:Direction :Horizontal)
	    (:Shadow-Offset 5)
	    (:Selection-Function ,#'Prop-OK-Function)
	    (:Text-Offset 2)
	    (:final-feedback-p NIL)
	    (:Gray-Width 3)
	    (:Items ("OK" "Apply" "Cancel")))
       (:propsheet ,prop-sheet
	(:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (+ 2 (gvl :parent :top)
			     (gvl :parent :ok-panel :height))))
	(:items ,(o-formula (gvl :parent :items)))
	(:default-filter ,(o-formula (gvl :parent :default-filter)))
	(:v-spacing ,(o-formula (gvl :parent :v-spacing)))
	(:multi-line-p ,(o-formula (gvl :parent :multi-line-p)))
	(:select-label-p ,(o-formula (gvl :parent :select-label-p)))
	(:label-select-event ,(o-formula (gvl :parent :label-select-event)))
	(:label-selected-func ,(o-formula
				(gvl :parent :label-selected-func)))
	(:select-value-p ,(o-formula (gvl :parent :select-value-p)))
	(:value-selected-func ,(o-formula
				(gvl :parent :value-selected-func)))
	(:single-select-p ,(o-formula (gvl :parent :single-select-p)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now deal with Object property sheets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'prop-sheet-for-obj-with-OK prop-sheet-with-OK
    (:maybe-constant '(:left :top :obj :slots :eval-p :ok-function
		       :apply-function :Cancel-Function :v-spacing
		       :multi-line-p :select-label-p :visible
		       :label-selected-func :label-select-event
		       :select-value-p :value-selected-func :single-select-p
		       :type-gadgets :union? :error-gadget))
    ;; Customizable slots
    (:left 5) (:top 5)
    (:obj NIL)
    (:slots '(:left :top :width :height)) ; list of slots to show
    (:eval-p T)
    (:error-gadget NIL)
    (:type-gadgets NIL)
    (:union? T)
    ;; plus the rest of the slots exported by prop-sheet-with-ok
    
    (:default-filter #'Obj-Read-Filter)
    (:items (o-formula (Prop-Sheet-Obj-Create-Items (gv :SELF))))
    (:parts
     `((:ok-panel :modify
	(:Selection-Function ,#'Prop-Obj-OK-Function))
       (:propsheet :modify
		   (:set-immediately-p NIL)
		   (:eval-p ,(o-formula (gvl :parent :eval-p)))
		   (:obj ,(o-formula (gvl :parent :obj)))
	           (:slots ,(o-formula (gvl :parent :slots)))
	           (:union? ,(o-formula (gvl :parent :union?)))
	           (:error-gadget ,(o-formula (gvl :parent :error-gadget)))))))

