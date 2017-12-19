;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by GILT V0.4: The Garnet Interface Builder
;;; on May 23, 1991, 12:31 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "LAPIDARY")


;;; This file uses the following objects:
;;;     TEXT-BUTTON-PANEL from package GARNET-GADGETS
;;;     MULTI-TEXT from package OPAL
;;;     RECTANGLE from package OPAL
;;;     LABELED-BOX from package GARNET-GADGETS
;;;     SCROLLING-LABELED-BOX from package GARNET-GADGETS
;;;     SCROLLING-MENU from package GARNET-GADGETS
(dolist (gadget '("scrolling-menu-loader"
		  "scrolling-labeled-box-loader"
		  ;; "labeled-box-loader"
		  ;; "text-buttons-loader"
		  ))
  (common-lisp-user::garnet-load (concatenate 'string "gadgets:" gadget)))

(defvar start-where-options
  '(T Nil :In :In-Box :In-But-Not-On
      :Element-of :Leaf-Element-Of :List-Leaf-Element-Of
      :List-Element-Of-Or-None :List-Leaf-Element-Of-Or-None
      :Check-Leaf-But-Return-Element :List-Check-Leaf-But-Return-Element
      :Check-Leaf-But-Return-Element-Or-None
      :List-Check-Leaf-But-Return-Element-Or-None))

(defvar start-where-list-options
  '(:List-Leaf-Element-Of
    :List-Element-Of-Or-None
    :List-Leaf-Element-Of-Or-None
    :List-Check-Leaf-But-Return-Element
    :List-Check-Leaf-But-Return-Element-Or-None))

(defvar start-where-non-type-options
  '(:in-box t nil :in-box :in-but-not-on))

(defvar *start-where-win* nil)

(defun parse-start-where (start-where)
  (declare (special start-where-gadget))
  (let* ((control (if (listp start-where) (car start-where) start-where))
	 (obj (if (listp start-where) (second start-where)))
	 (slot (if (and (listp start-where)
			(member control start-where-list-options))
		   (third start-where))))

   (if (eq control :not-supplied)
    (progn
      (set-initial-value start-where-gadget :start-where nil)
      (s-value (g-value start-where-gadget :start-where) :selected-ranks nil)
      (set-initial-value start-where-gadget :obj "")
      (set-initial-value start-where-gadget :slot "")
      (set-gray-out-rect-visible start-where-gadget :obj-disable t)
      (set-gray-out-rect-visible start-where-gadget :slot-disable t))
    (progn
      (set-initial-value start-where-gadget :start-where (list control))
      (s-value (g-value start-where-gadget :start-where) :selected-ranks
	       (list (position control start-where-options)))
      (if obj
	(progn
	  (set-initial-value start-where-gadget :obj
			     (let ((kr::*print-as-structure* nil))
			       (prin1-to-string obj)))
	  (set-gray-out-rect-visible start-where-gadget :obj-disable nil))
        (progn
	  (set-gray-out-rect-visible start-where-gadget :obj-disable t)
	  (set-initial-value start-where-gadget :obj "")))
      (if slot
	(progn
	  (set-initial-value start-where-gadget :slot
			     (symbol-name slot))
	  (set-gray-out-rect-visible start-where-gadget :slot-disable nil))
	(progn
	  (set-initial-value start-where-gadget :slot "")
	  (set-gray-out-rect-visible start-where-gadget :slot-disable t)))))))

(defun show-start-where-win (gadget value)
  (declare (ignore value))
  (declare (special start-where-gadget *start-where-win*))
  (let ((start-where (get-start-where (symbol-value (g-value gadget :queue))
				      (g-value gadget :inter)))
	(start-where-parent (g-value gadget :parent :select-box-panel)))
    (parse-start-where start-where)
    ;; gadget should not be selected yet
    (mark-as-invalid gadget :selected)
    (s-value start-where-gadget :queue (g-value gadget :queue))
    (s-value start-where-gadget :inter (g-value gadget :inter))
    (s-value start-where-gadget :start-where-parent start-where-parent)
    (multiple-value-bind (left top)
			 (opal:convert-coordinates (g-value gadget :window)
						   (g-value gadget :left)
						   (opal:bottom gadget) NIL)
    (setf *start-where-win* (gilt:show-in-window start-where-gadget
					    left (+ 10 top) t)))))

;;; ensure that the value is a valid garnet object
(defun garnet-object-p (gadget value)
  (let* ((symbol-name (when (not (string= value ""))
			    (read-from-string value)))
	 (object (when symbol-name (eval symbol-name))))
    (if (and object (schema-p object))
	(s-value gadget :old-value value)
        (progn
	  (inter:beep)
	  (s-value gadget :value (g-value gadget :old-value))
	  (lapidary-error "Value must be the name of a garnet object")))))

;;; ensure that the value is a valid slotname
(defun garnet-slot-p (gadget value)
  (let ((symbol-name (when (not (string= value ""))
			   (read-from-string value))))
    (if (keywordp symbol-name)
	(s-value gadget :old-value value)
        (progn
	  (inter:beep)
	  (s-value gadget :value (g-value gadget :old-value))
	  (lapidary-error "Value must be a keyword")))))

;;; set the visibility of a gray out rectangle
(defun set-gray-out-rect-visible (gadget slot visible)
  (s-value (g-value gadget slot) :visible visible))

;;; enable the start-where object and slot parameters if appropriate;
;;; if an object is selected, store it in the start-where object parameter
(defun start-where-options-final-fn (gadget selection)
  (declare (ignore selection))
  (declare (special start-where-gadget))
  (let ((control (car (g-value gadget :value)))
	(obj (car (g-value *selection-info* :selected))))
    (if (keywordp control)
	(progn
	  (set-gray-out-rect-visible start-where-gadget :obj-disable nil)
	  (when obj
		(set-initial-value start-where-gadget :obj
				   (let ((kr::*print-as-structure* nil))
				     (prin1-to-string obj))))
	  (set-gray-out-rect-visible start-where-gadget :slot-disable
			       (not (member control start-where-list-options))))
        (progn
	  (set-gray-out-rect-visible start-where-gadget :obj-disable t)
	  (set-gray-out-rect-visible start-where-gadget :slot-disable t)))))

;;; construct a start-where expression from the start-where dialog box
;;; parameters
(defun start-where-ok-fn (gadget value-list)
  (declare (special *start-where-win*))
  (let* ((control (car (value-of :start-where value-list)))
	(start-where-obj (value-of :obj value-list))
	(slot (value-of :slot value-list))
	(start-where-parent (g-value gadget :start-where-parent))
	(type (g-value start-where-parent :type-restriction))
	start-where)

    ;; ensure that all required parameters are present
    (when (and (not (eq control t))
	       (not (null control))
	       (string= "" start-where-obj))
	  (lapidary-error "An object must be supplied for the start-where expression")
	  (s-value *start-where-win* :visible t)
	  (return-from start-where-ok-fn))
    (when (and (listp start-where)
	       (member control start-where-list-options)
	       (string= "" slot))
	  (lapidary-error "A slot name must be supplied for the start-where expression")
	  (s-value *start-where-win* :visible t)
	  (return-from start-where-ok-fn))

    ;; construct the start-where expression
    (setf start-where
	  (cond ((or (eq control t) (null control))
		 control)
		((member control start-where-list-options)
		 (list control
		       (gg:careful-string-eval start-where-obj)
		       (read-from-string slot)))
		(t
		 (list control
		       (gg:careful-string-eval start-where-obj)))))

    ;; add the type restriction if necessary
    (if (member control start-where-non-type-options)
	(s-value start-where-parent :type nil)
        (when (g-value start-where-parent :type)
	      (setf start-where (append start-where (list :type type)))))

    (dialog-enqueue :start-where start-where
		    (symbol-value (g-value gadget :queue)))

    ;; store the chosen start-where option in the field string slot
    ;; of the appropriate start-where gadget
    (s-value start-where-parent :value "Other")
    (s-value (g-value start-where-parent :parent :parent)
	     :field-string (symbol-name control))))

;;;*******************************************************************
;;;  TYPE-RESTRICT-FN -- add or remove type restriction from the
;;;    start-where of an interactor
;;;*******************************************************************

(defun prompt-for-type-restrict (gadget value)
  (declare (special prompt-gadget prompt-window))
  (let* ((queue (g-value gadget :queue))
	(start-where (g-value gadget :parent :select-box-panel))
	(type-restriction (g-value start-where :type-restriction)))
    ;; if a type restriction should be added, prompt the user for
    ;; the type restriction and store it in the type restriction
    ;; field of the start-where aggregate. Also push the new start-where
    ;; onto the queue if the value and field-string fields in the
    ;; start-where agg are set
    (if value
	(let* ((start-where-value (get-start-where (symbol-value queue)
						   (g-value gadget :inter)))
	       (control (if (listp start-where-value)
			    (car start-where-value)
			    start-where-value)))
	  (when (or (eq control t) (null control)
		    (eq control :in) (eq control :in-box)
		    (eq control :in-but-not-on))
	    (lapidary-error
	     (format nil "cannot specify a type restrictor for a start-where that begins with ~S" control))
	    (s-value start-where :type nil)
	    (s-value gadget :value nil)
	    (return-from prompt-for-type-restrict))

	  ;; set the type restriction back to nil; until a type restriction
	  ;; is actually entered, the type field should stay nil
	  (s-value gadget :value nil)

	  ;; get the type restriction
	  (s-value prompt-gadget :function-for-ok 'type-restrict-fn)
	  (let* ((kr::*print-as-structure* nil)
		 (type-string (if type-restriction
				  (prin1-to-string type-restriction)
				  "")))
	    ;; lisp won't convert a list to the appropriate string notation
	    ;; so we must do it
	    (when (and type-restriction (listp type-restriction))
		  (setf type-string (concatenate 'string "(list " (string-left-trim "(" type-string))))
	    (set-initial-value prompt-gadget :result type-string))

	  (s-value prompt-gadget :string
		   "Please enter a type restriction. A type restriction can
either be a single object (e.g., opal:circle) or
a list of objects (e.g., (list opal:circle opal:line)).
If you enter a list, it should be an executable list
expression, such as (list ...) or `(...)")
	  (s-value prompt-gadget :calling-gadget gadget)
	  (multiple-value-bind (left top)
			 (opal:convert-coordinates (g-value gadget :window)
						   (g-value gadget :left)
						   (opal:bottom gadget) NIL)
			 (setf prompt-window (gilt:show-in-window prompt-gadget
					      left top t))))

	; else the type restriction should be removed--get the start where
	; and chop off the type information using subseq
	(let ((start-where-value (get-start-where (symbol-value queue)
				     (g-value gadget :inter))))
	  (when (listp start-where-value)
		(dialog-enqueue :start-where
				(subseq start-where-value 0
					(position :type start-where-value))
				(symbol-value queue)))
	  (s-value start-where :type value)))))

(defun type-restrict-fn (gadget value-list)
  (declare (special prompt-window))
  (let* ((type-restriction (value-of :result value-list))
	 (calling-gadget (g-value gadget :calling-gadget))
	 (queue (g-value calling-gadget :queue))
	 (inter (g-value calling-gadget :inter))
	 (start-where-value (get-start-where (symbol-value queue) inter))
	 (start-where-gadget (g-value calling-gadget :parent :select-box-panel)))

    (when (string= type-restriction "")
	  (lapidary-error
	   "Type restriction cannot be an empty string.
Press cancel if you do not want a type restriction")
	  (s-value prompt-window :visible t)
	  (return-from type-restrict-fn))

    (setf type-restriction (eval (read-from-string type-restriction)))

    ;; add the type restriction to the start-where
    (dialog-enqueue :start-where
		    (append (subseq start-where-value
				    0 (position :type start-where-value))
			    (list :type type-restriction))
		    (symbol-value queue))

    ;; add the type restriction information to the start-where aggregate
    (s-value start-where-gadget :type t)
    (s-value start-where-gadget :type-restriction type-restriction)))

(defun insert-inter-into-agg-query-fn (gadget value)
  (cond ((string= value "highlight aggregate")
	 (garnet-debug:flash (g-value gadget :parent :agg-to-insert-into)))
	(t
	 (s-value (g-value gadget :window) :visible nil)
	 (opal:update-all)
	 (inter:interaction-complete (string= value "ok")))))

(create-instance 'START-WHERE-GADGET OPAL:AGGREGADGET
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 384)
  (:WINDOW-HEIGHT 261)
  (:FUNCTION-FOR-OK 'start-where-ok-fn)
  (:EXPORT-P T)
  (:WINDOW-TITLE "Start-Where-Win")
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 384))
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 261))
  (:parts `(
    (:start-where ,GARNET-GADGETS:SCROLLING-MENU
      (:GILT-REF "TYPE-SCROLLING-MENU")
      (:INDICATOR-FONT ,(create-instance nil OPAL:FONT
            (:SIZE :SMALL)))
      (:PAGE-INCR 5)
      (:SCR-INCR 1)
      (:SCROLL-SELECTION-FUNCTION NIL)
      (:MENU-SELECTION-FUNCTION start-where-options-final-fn)
      (:H-ALIGN :LEFT)
      (:SCROLL-ON-LEFT-P NIL)
      (:MIN-FRAME-WIDTH 0)
      (:TEXT-OFFSET 4)
      (:FINAL-FEEDBACK-P T)
      (:INT-MENU-FEEDBACK-P T)
      (:MULTIPLE-P nil)
      (:ITEM-FONT ,OPAL:DEFAULT-FONT)
      (:TITLE-FONT ,(create-instance nil OPAL:FONT
            (:SIZE :LARGE)
            (:FAMILY :SERIF)))
      (:V-SPACING 6)
      (:MIN-SCROLL-BAR-WIDTH 20)
      (:INDICATOR-TEXT-P NIL)
      (:PAGE-TRILL-P T)
      (:SCR-TRILL-P T)
      (:NUM-VISIBLE 5)
      (:INT-SCROLL-FEEDBACK-P NIL)
      (:TITLE "Start Where")
      (:ITEMS (T Nil :In :In-Box :In-But-Not-On :Element-of :Leaf-Element-Of :List-Leaf-Element-Of :List-Element-Of-Or-None :List-Leaf-Element-Of-Or-None :Check-Leaf-But-Return-Element :List-Check-Leaf-But-Return-Element :Check-Leaf-But-Return-Element-Or-None :List-Check-Leaf-But-Return-Element-Or-None))
      (:item-to-string-function
       ,#'(lambda (item)
	   (if item
	       (if (stringp item)
		   item
		   (string-capitalize (string-trim ":" item)))
	       "Nil")))
      (:BOX (30 50 111 145 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 50)))
    (:obj ,GARNET-GADGETS:SCROLLING-LABELED-BOX
      (:GILT-REF "TYPE-SCROLLING-LABELED-BOX")
      (:MIN-FRAME-WIDTH NIL)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 2)
      (:LABEL-OFFSET 5)
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:BOX (30 200 322 18 ))
      (:MIN-WIDTH 20)
      (:GROW-P T)
      (:selection-function garnet-object-p)
      (:LABEL-STRING "Start Where Object")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 200))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 322)))
    (:slot ,GARNET-GADGETS:LABELED-BOX
      (:GILT-REF "TYPE-LABELED-BOX")
      (:LABEL-FONT ,(create-instance nil OPAL:FONT
            (:FACE :BOLD)))
      (:LABEL-OFFSET 5)
      (:MIN-FRAME-WIDTH 120)
      (:selection-function garnet-slot-p)
      (:FIELD-FONT ,OPAL:DEFAULT-FONT)
      (:FIELD-OFFSET 6)
      (:BOX (30 230 97 18 ))
      (:MIN-WIDTH 20)
      (:LABEL-STRING "Slot")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 30))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 230)))
    (:obj-disable ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:DRAW-FUNCTION :AND)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:BOX (25 196 335 30 ))
      (:GROW-P T)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 25))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 196))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 335))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 30)))
    (:title ,OPAL:MULTI-TEXT
      (:GILT-REF "TYPE-TEXT")
      (:FONT ,(create-instance nil OPAL:FONT
            (:SIZE :VERY-LARGE)
            (:FACE :BOLD-ITALIC)
            (:FAMILY :SERIF)))
      (:BOX (10 10 3 3 ))
      (:STRING "Select Start Where")
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 10))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 10)))
    (:slot-disable ,OPAL:RECTANGLE
      (:GILT-REF "TYPE-RECTANGLE")
      (:GROW-P T)
      (:BOX (27 229 160 22 ))
      (:LINE-STYLE ,OPAL:DEFAULT-LINE-STYLE)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:DRAW-FUNCTION :AND)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 27))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 229))
      (:WIDTH ,(o-formula (THIRD (GVL :BOX)) 160))
      (:HEIGHT ,(o-formula (FOURTH (GVL :BOX)) 22)))
    (:stop-group ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:SELECTION-FUNCTION GILT:OKCANCEL-FUNCTION)
      (:GILT-REF "TYPE-OKCANCEL")
      (:ITEMS ("OK" "Cancel" ))
      (:GRAY-WIDTH 3)
      (:FINAL-FEEDBACK-P NIL)
      (:TEXT-OFFSET 2)
      (:SHADOW-OFFSET 5)
      (:DIRECTION :HORIZONTAL)
      (:BOX (229 10 124 21 ))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 229))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 10))))))

(create-instance 'INTER-AGG-QUERY OPAL:AGGREGADGET
  (:FUNCTION-FOR-OK NIL)
  (:PACKAGE-NAME "LAPIDARY")
  (:LEFT 0)
  (:TOP 0)
  (:HEIGHT (o-formula (GVL :WINDOW :HEIGHT) 225))
  (:WINDOW-LEFT 0)
  (:WINDOW-TOP 0)
  (:WINDOW-WIDTH 610)
  (:WINDOW-HEIGHT 225)
  (:WIDTH (o-formula (GVL :WINDOW :WIDTH) 610))
  (:WINDOW-TITLE "inter-agg-query")
  (:EXPORT-P T)
  (:parts `(
    (0 ,OPAL:multi-TEXT
      (:BOX (100 19 3 3 ))
      (:CONSTANT T)
      (:string ,(o-formula
	   (format nil "It appears that this interactor should be inserted into
the aggregate named ~S.

-If you would like to see this aggregate highlighted,
   press the 'highlight aggregate' button.
-If you would like the interactor inserted into the
   aggregate, press the 'insert into aggregate' button.
-If you do not want the interactor inserted into the
   aggregate, press the cancel button." (gvl :parent :agg-to-insert-into))))
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 100))
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 19)))
    (1 ,GARNET-GADGETS:TEXT-BUTTON-PANEL
      (:BOX (28 171 535 44 ))
      (:CONSTANT T)
      (:final-feedback-p nil)
      (:LEFT ,(o-formula (FIRST (GVL :BOX)) 28))
      (:ITEMS ("highlight aggregate" "ok" "cancel" ))
      (:selection-function insert-inter-into-agg-query-fn)
      (:DIRECTION :HORIZONTAL)
      (:TOP ,(o-formula (SECOND (GVL :BOX)) 171))))))
