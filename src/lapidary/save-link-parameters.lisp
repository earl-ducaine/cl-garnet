;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by LAPIDARY 2.0: A Garnet Interface Builder
;;; on Jul 7, 1992, 7:27 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CHANGE LOG:
;;;
;;; 07/14/93 amickish - Commented out load of lapidary-functions-loader
;;; 08/25/92 amickish - Added proclaim

(in-package "LAPIDARY")

(declaim (special save-time-do-not-dump-slots))

(setf common-lisp-user::*Used-Garnet-Version* "2.0")

;;;
;;;     Functions needed from Lapidary
;;;(garnet-load "lapidary:lapidary-functions-loader")

(create-instance 'save-link-parameters OPAL:AGGREGADGET
  (:LEFT 10)
  (:TOP 15)
  (:window-title "link parameters")
  (:window-width (o-formula (+ 20 (gvl :width))))
  (:window-height (o-formula (+ 20 (gvl :height))))
  (:PACKAGE-NAME "COMMON-LISP-USER")
  (:EXPORT-P NIL)
  (:parts `(
    (:OKCANCEL-BUTTON ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION gilt:OKCANCEL-FUNCTION)
      (:function-for-ok save-link-parameters-ok-fct)
      (:INDENT 0)
      (:V-ALIGN :TOP)
      (:V-SPACING 5)
      (:H-ALIGN :CENTER)
      (:FIXED-HEIGHT-P T)
      (:H-SPACING 5)
      (:PIXEL-MARGIN NIL)
      (:RANK-MARGIN NIL)
      (:FIXED-WIDTH-P T)
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
#|
      (:LEFT ,(o-formula (+ 20 (max (opal:gv-right (gvl :parent :object))
				    (opal:gv-right (gvl :parent :aggregadget))))))
|#
      (:left ,(o-formula (+ 20 (opal:gv-right (gvl :parent :object)))))
      (:top 10))
    (:instructions ,OPAL:CURSOR-MULTI-TEXT
          (:LEFT ,(o-formula (GVL :PARENT :CURSOR-TEXT-11159 :LEFT ) 41))
          (:TOP ,(o-formula (+ (LAPIDARY::GV-BOTTOM (GVL :PARENT :CURSOR-TEXT-11159 ) ) 20 ) 78))
          (:EXPORT-P T)
          (:PACKAGE-NAME "COMMON-LISP-USER")
          (:FONT ,(create-instance nil OPAL:FONT))
	  (:STRING "Some of the slots in the object reference objects that might be parameters. These objects
are listed below, along with the slots that reference them. If you make any of these objects
into parameters, Lapidary will create formulas that cause the slots to retrieve the parameters
from the object labeled 'parameters object'.

If you want to see the object labeled 'object:', select the object's name with the left mouse 
button and the object will be flashed on the display.

If you want to make an object a parameter, enter the parameter name in the blank box next to each
object. This name must be a slot name (e.g., :string). If the user has already provided a name, 
Lapidary puts it in the box.

If you do not want an object to be a parameter, just leave its parameter box blank (or make it
blank if Lapidary has put a name in there).

If you want to see one of the referenced objects, select the object's name with the left mouse 
button and the object will be flashed on the display.

For more information on this operation, see the Lapidary reference manual."))
    (:CURSOR-TEXT-11152 ,OPAL:CURSOR-TEXT
	  (:LEFT ,(o-formula (GVL :PARENT :LEFT ) 41))
          (:TOP ,(o-formula (GVL :PARENT :TOP ) 15))
          (:STRING "object:")
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :LARGE)
                (:FACE :BOLD))))
    (:CURSOR-TEXT-11159 ,OPAL:CURSOR-TEXT
          (:LEFT ,(o-formula (GVL :PARENT :LEFT ) 41))
          (:TOP ,(o-formula (+ (LAPIDARY::GV-BOTTOM (GVL :PARENT :CURSOR-TEXT-11152 ) ) 5 ) 39))
          (:STRING "parameters object:")
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :LARGE)
                (:FACE :BOLD))))
    (:object ,OPAL:CURSOR-TEXT
          (:LEFT ,(o-formula (+ (LAPIDARY::GV-RIGHT (GVL :PARENT :CURSOR-TEXT-11152 ) ) 10 ) 122))
          (:TOP ,(o-formula (GVL :PARENT :CURSOR-TEXT-11152 :TOP ) 15))
          (:STRING "foo")
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :LARGE)
                (:FACE :BOLD))))
    (:aggregadget ,OPAL:CURSOR-TEXT
          (:LEFT ,(o-formula (+ (LAPIDARY::GV-RIGHT (GVL :PARENT :CURSOR-TEXT-11159 ) ) 10 ) 287))
          (:TOP ,(o-formula (GVL :PARENT :CURSOR-TEXT-11159 :TOP ) 39))
          (:STRING "goo")
          (:FONT ,(create-instance nil OPAL:FONT
                (:SIZE :LARGE)
                (:FACE :BOLD))))
    (:obj-flash-feedback ,OPAL:RECTANGLE
      (:LEFT ,(o-formula (GVL :OBJ-OVER :LEFT ) 122))
      (:TOP ,(o-formula (GVL :OBJ-OVER :TOP ) 15))
      (:WIDTH ,(o-formula (ROUND (GVL :OBJ-OVER :WIDTH ) ) 31))
      (:HEIGHT ,(o-formula (ROUND (GVL :OBJ-OVER :HEIGHT ) ) 20))
      (:VISIBLE ,(o-formula (GVL :OBJ-OVER ) ))
      (:LINE-STYLE NIL)
      (:FILLING-STYLE ,OPAL:BLACK-FILL)
      (:DRAW-FUNCTION :XOR)
      (:FEEDBACK-P T))

    (:PARAMETER-SLOTS ,OPAL:AGGRELIST
      (:LEFT ,(o-formula (- (GVL :PARENT :PARAMETER-LABEL :LEFT ) 3) 260))
      (:TOP ,(o-formula (GVL :PARENT :SLOTS :TOP ) 425))
      (:VISIBLE T)
      (:ITEMS '("" "" "" "" ""))
      (:DIRECTION :VERTICAL)
      (:V-SPACING 5)
      (:item-prototype ,garnet-gadgets:labeled-box
	   (:label-string "")
	   (:min-frame-width 100)
	   (:value ,(o-formula (nth (gvl :rank) (gvl :parent :items))))))
    (:REF-OBJS ,OPAL:AGGRELIST
      (:LEFT ,(o-formula (GVL :PARENT :OBJ-LABEL :LEFT ) 41))
      (:TOP ,(o-formula (+ (LAPIDARY::GV-BOTTOM (GVL :PARENT :OBJ-LABEL ) ) 10 ) 425))
      (:VISIBLE T)
      (:ITEMS '("object" "object" "really big and long object"))
      (:DIRECTION :VERTICAL)
      (:V-SPACING 10)
      (:item-prototype ,opal:CURSOR-TEXT
        (:string ,(o-formula (let ((kr::*print-as-structure* nil))
			       (princ-to-string
				(nth (gvl :rank) (gvl :parent :items)))))))
      (:interactors (
	(:FLASH ,inter:MENU-INTERACTOR
		(:WINDOW ,(o-formula (GVL :OPERATES-ON :WINDOW )))
		(:START-WHERE ,(o-formula (LIST :ELEMENT-OF (GVL :OPERATES-ON ))))
		(:final-function 
		 (lambda (inter selected)
		   (declare (ignore inter))
		   (garnet-debug:flash (nth (g-value selected :rank)
					    (g-value selected :parent :items)))))
		(:FEEDBACK-OBJ ,(o-formula (gvl :operates-on :parent :feedback)))
		(:ACTIVE T)))))
    (:SLOTS ,OPAL:AGGRELIST
      (:LEFT ,(o-formula (- (GVL :PARENT :SLOTS-LABEL :LEFT ) 3) 200))
      (:TOP ,(o-formula (GVL :PARENT :REF-OBJS :TOP ) 425))
      (:VISIBLE T)
      (:ITEMS '("(:left :top)" "(:left)"))
      (:DIRECTION :VERTICAL)
      (:V-SPACING 10)
      (:item-prototype ,opal:cursor-text
        (:string ,(o-formula (format nil "~{ ~S~}"
			      (nth (gvl :rank) (gvl :parent :items)))))))
    (:OBJ-LABEL ,OPAL:CURSOR-TEXT
	(:left 10)
	(:TOP ,(o-formula (+ (opal:gv-bottom (gvl :parent :instructions)) 20)))
	(:STRING "object referenced by")
	(:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD))))
    (:SLOTS-LABEL ,OPAL:CURSOR-TEXT
      (:LEFT ,(o-formula (+ (MAX (OPAL:GV-RIGHT (GVL :PREV-COL ) ) (OPAL:GV-RIGHT (GVL :PREV-LABEL ) ) ) 20 ) 200))
      (:TOP ,(o-formula (GVL :PARENT :OBJ-LABEL :TOP ) 402))
      (:STRING "slots")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:PREV-LABEL ,(o-formula (gvl :parent :OBJ-LABEL)))
      (:PREV-COL ,(o-formula (gvl :parent :REF-OBJS))))
    (:PARAMETER-LABEL ,OPAL:CURSOR-TEXT
      (:LEFT ,(o-formula (+ (MAX (OPAL:GV-RIGHT (GVL :PREV-COL ) ) (OPAL:GV-RIGHT (GVL :PREV-LABEL ) ) ) 20 ) 260))
      (:TOP ,(o-formula (GVL :PARENT :SLOTS-LABEL :TOP ) 402))
      (:STRING "parameter name")
      (:FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:PREV-LABEL ,(o-formula (gvl :parent :SLOTS)))
      (:PREV-COL ,(o-formula (gvl :parent :SLOTS-LABEL))))
    (:feedback ,OPAL:RECTANGLE
	       (:LEFT ,(o-formula (- (+ (GVL :OBJ-OVER :LEFT) 
				    (TRUNCATE (* (GVL :OBJ-OVER :WIDTH) 0.5))) 
				    (TRUNCATE (GVL :WIDTH) 2)) 41))
	       (:TOP ,(o-formula (- (+ (GVL :OBJ-OVER :TOP) 
			 (TRUNCATE (* (GVL :OBJ-OVER :HEIGHT) 0.5))) 
		      (TRUNCATE (GVL :HEIGHT) 2)) 425))
	       (:WIDTH ,(o-formula (gvl :obj-over :width) 41))
	       (:HEIGHT ,(o-formula (GVL :OBJ-OVER :HEIGHT) 14))
	       (:VISIBLE ,(o-formula (gvl :obj-over)))
	       (:LINE-STYLE NIL)
	       (:FILLING-STYLE ,OPAL:BLACK-FILL)
	       (:DRAW-FUNCTION :XOR)
	       (:OBJ-OVER NIL)
	       (:FEEDBACK-P T))))
  
  (:interactors `(
	(:FLASH ,inter:BUTTON-INTERACTOR
		(:WINDOW ,(o-formula (GVL :OPERATES-ON :WINDOW )))
		(:START-WHERE ,(o-formula (list :IN-BOX (gvl :operates-on :object))))
		(:FEEDBACK-OBJ ,(o-formula (gvl :operates-on :obj-flash-feedback)))
		(:final-function 
		 (lambda (inter selected)
		   (declare (ignore selected))
		   (garnet-debug:flash (g-value inter :operates-on :obj))))
		(:ACTIVE T)))))


;; if find-link-slots finds
;; a slot that points to an object which is not in the aggregate
;; hierarchy, it asks the user whether the slot should become a
;; parameter slot

(defun find-link-slots (object symbol-table)
  (let ((do-not-dump-slots (append (g-value object :do-not-dump-slots)
				   save-time-do-not-dump-slots))
	(aggregadget-links '(:parent))
	value-list)
    ;; :obj-over should not be a parameter
    (push :obj-over do-not-dump-slots)

   ;; get the list of link slots that might be created by an aggregadget.
   ;; they should not be made into parameters
   (dolist (child (g-value object :components))
	   (push (g-value child :known-as) aggregadget-links))

    (doslots (slot object)
      (let ((value (get-value object slot))
	    value-list-entry slots)
	(when (and (is-a-p value opal:view-object)
		   (not (member slot do-not-dump-slots))
		   (not (member slot aggregadget-links)))

	      ;; determine if value has already been seen. if it has,
	      ;; push the link slot onto the list of link slots. 
	      ;; otherwise determine if the value belongs to the
	      ;; current object's aggregadget. if it does not, it is
	      ;; a probable parameter so find the slots that depend on
	      ;; the link slot and create an entry on the value-list of
	      ;; the form: (value link-slots slots)
	      (setf value-list-entry (assoc value value-list))
	      (when value-list-entry
		    (setf slots (third value-list-entry)))
	      (when (or value-list-entry (not (member value symbol-table)))
            	(dolist (formula (kr::get-dependents object slot))
			(when (eq (kr::a-formula-schema formula) object)
			      (pushnew (kr::a-formula-slot formula) slots))))
	      (when slots
		(if value-list-entry
		  (progn
		    (push slot (second value-list-entry))
		    (setf (third value-list-entry) slots))
		  (push (list value (list slot) slots) value-list))))))
	value-list))

;;; initialize the link parameters dialog box and display it

(defun show-link-parameters (gadget value)
  (declare (ignore value))
  (declare (special save-link-parameters))
  (let* ((object (g-value save-link-parameters :obj))
	 (top-level-obj (g-value save-link-parameters :top-level-obj))
	 (link-values (g-value save-link-parameters :link-values))
	 (kr::*print-as-structure* nil)
	 ref-objs slots parameter-names link-slot)

    (s-value (g-value save-link-parameters :object) :string
	     (princ-to-string object))
    (s-value (g-value save-link-parameters :aggregadget) :string
	     (princ-to-string top-level-obj))

    ;; create the lists for the objects referenced by 'object', the
    ;; slots that reference these objects, and the parameter names. 
    ;; if the link name was created by the user, suggest the link name
    ;; as the parameter name
    (dolist (entry link-values)
	    (push (first entry) ref-objs)
	    (setf link-slot (car (second entry)))
	    (if (member link-slot '(:x1-over :x2-over :y1-over :y2-over
					     :left-over :top-over 
					     :width-over :height-over))
		(push "" parameter-names)
	      (push (prin1-to-string link-slot) parameter-names))
	    (push (third entry) slots))
    (s-value (g-value save-link-parameters :ref-objs) :items (reverse ref-objs))
    (s-value (g-value save-link-parameters :slots) :items (reverse slots))
    (s-value (g-value save-link-parameters :parameter-slots) 
	     :items (reverse parameter-names))
    (opal::fix-update-slots (g-value save-link-parameters :ref-objs))
    (opal::fix-update-slots (g-value save-link-parameters :slots))
    (opal::fix-update-slots (g-value save-link-parameters :parameter-slots))
    (gilt:show-in-window-and-wait save-link-parameters 
				  (opal:right (g-value gadget :window))
				  (opal:bottom (g-value gadget :window)) t)))


;;; add the parameter slots to a parameters-list in the top-level object so
;;; that they can be set to nil when the top-level object is written out.
;;; if the object is not equal to the top-level object, store formulas in
;;; the link slots that retrieve their values from the parameter slot in
;;; the top-level object
(defun convert-links-to-parameters ()
  (declare (special save-link-parameters))
  (let* ((object (g-value save-link-parameters :obj))
	 (top-level-obj (g-value save-link-parameters :top-level-obj))
	 (link-values (g-value save-link-parameters :link-values))
	 (path (gilt:make-path object top-level-obj))
	 (parameter-names (g-value save-link-parameters :parameter-slots :components))
	 links parameter-name)
    (do ((link-entries link-values (cdr link-entries))
	 (parameters parameter-names (cdr parameters)))
	((null parameters))
      (setf links (second (car link-entries)))
      (setf parameter-name (g-value (car parameters) :value))
      (cond ((not (string= parameter-name ""))
	     (setf parameter-name (read-from-string parameter-name))
	     (pushnew parameter-name (g-value top-level-obj :parameters-defined-in-lapidary))
	     ;; copy the value of the object into the top-level object
	     (s-value top-level-obj parameter-name
		      (g-value object (car links)))
	     (dolist (link links)
	       (if (eq top-level-obj object)
		   ;; replace references to the old slot with references
		   ;; to the new slot and destroy the old slot
		   (progn
		     (when (not (eq link parameter-name))
			   (dolist (formula (kr::get-dependents object link))
			     (change-formula (kr::a-formula-schema formula)
					     (kr::a-formula-slot formula)
					     (subst parameter-name link
						     (extract-formula formula))))
			   (destroy-slot object link)))
		   (s-value object link 
				(eval `(o-formula (gvl ,@path
						       ,parameter-name)))))))
	    ;; if the parameter-name is blank, delete the first link slot
	    ;; on the links list from the :parameters-defined-in-lapidary slot. This will prevent
	    ;; the link from being set to nil when it is written out 
	    ;; (note: deletion from the parameters list is done as a 
	    ;; preventative measure--the link may not be on the list)
	    (t
	     (s-value top-level-obj :parameters-defined-in-lapidary 
		      (delete (car links) (g-value top-level-obj :parameters-defined-in-lapidary))))))))
		        

;;; indicate that the ok button was hit by setting ok-p to t

(defun save-link-parameters-ok-fct (gadget value)
  (declare (ignore gadget value))
  (declare (special save-link-parameters))
  (s-value save-link-parameters :ok-p t))
