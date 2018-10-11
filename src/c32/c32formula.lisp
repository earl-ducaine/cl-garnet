;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;
;;;
;;;         The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at
;;; Carnegie Mellon University, and has been placed in the public
;;; domain.  If you are using this code or any part of Garnet,
;;; please contact garnet@cs.cmu.edu to be put on the mailing list.


;;; C32 is a spreadsheet interface for Garnet constraints
;;;
;;; This file contains the code to deal with formula windows.

;;; Designed and implemented by Brad Myers


(in-package :c32)

(defparameter Formula-Win-Width 440)
(defparameter Formula-Win-Height 300)

(defparameter Formula-Wins-Available NIL)
(defparameter formula-win-offset 0
  "Offset for formula windows, so they are not exactly on top of one another")

(create-instance 'My-Text-Button-Panel garnet-gadgets:text-button-panel
		 (:shadow-offset 4)
		 (:text-offset 2)
		 (:final-feedback-p NIL)
		 (:direction :horizontal)
		 (:fixed-width-p NIL)
		 (:gray-width 3))

(create-instance 'Form-Win-Header opal:aggregadget
  (:left 2) (:top 2)
  (:slot NIL) (:obj NIL)
  (:parts
   `((:label ,opal:text
      (:string ,(o-formula (mk-string (gvl :parent :obj))))
      (:left 6) (:top 4))
     (:slot-label ,opal:text
      (:string ,(o-formula (concatenate 'simple-string
					"slot "
					(Mk-Colon-Str-And-Clip
					 (gvl :parent :slot) 99))))
      (:left 6) (:top 18))
     (:OK ,My-Text-Button-Panel
      (:left ,(o-formula (- (gvl :window :width)
			    90;;(gvl :width)
			    5) 100))
      (:top 2)
      (:items (("OK" Do-Form-Ok) ("Cancel" Do-Form-Cancel))))
     (:buttons ,garnet-gadgets:text-button-panel
      (:left 6) (:top 38) (:final-feedback-p nil)
      (:rank-margin 1)
      (:shadow-offset 2) (:text-offset 2) (:gray-width 2)
      (:font ,opal:default-font)
      (:items (("Insert Function" InsertFunction)
	       ("Insert From Spread" InsertRefFromSpread)
	       ("Insert From Mouse" InsertRefFromMouse)))))))


(defparameter Form-Header-Height (+ 3 (g-value Form-Win-Header :height)))

(defparameter *Current-Formula-Win* NIL)
(defparameter *current-formula-obj* nil)
(defparameter *current-formula-slot* nil)


(defun make-scroll-gadget (win1)
  (create-instance NIL garnet-gadgets:scrolling-window-with-bars
    (:left -2) (:top Form-Header-Height)
    ;; width and height overridden with formulas below
    (:width (+ 1 Formula-Win-Width))
    (:height (- Formula-Win-Height Form-Header-Height 2))
    (:parent-window win1)
    (:total-width (o-formula (+ 6 (gvl :inner-aggregate :width))))
    (:total-height (o-formula (+ 6 (gvl :inner-aggregate :height))))))


#|
(defun New-Formula-Win (left top)
  (let* ((agg (create-instance NIL opal:aggregate))
	 (str (create-instance NIL opal:multifont-text
		(:word-wrap-p T) (:auto-scroll-p T)))
	 (win1 (create-instance NIL inter:interactor-window
		 (:left left) (:top top)
		 (:width Formula-Win-Width) (:height Formula-Win-Height)
		 (:aggregate agg) (:title "C32 Formula Window")))
	 (scroll-gadget (make-scroll-gadget win1))
	 (header (create-instance NIL Form-Win-Header))
	 ;; Create a focus interactor so that keyboard events may be entered
	 ;; into the text objects.
	 (focus-inter (create-instance nil inter:focus-multifont-textinter
			(:window `(,win1 ,(g-value win1 :clip-window)
				   ,(g-value win1 :inner-window)))))
	 ;; Create a selection interactor to handle mouse events on the text
	 ;; objects.
	 (mouse-inter (create-instance nil inter:selection-interactor
			(:window `(,win1 ,(g-value win1 :clip-window)
				   ,(g-value win1 :inner-window)))
			(:focus-interactor focus-inter)
			(:text-list `(,str))))
	 inter)
    (s-value mouse-inter :start-where `(:list-element-of ,mouse-inter
					:text-list :type
					,opal:multifont-text))
    (s-value str :text-width (o-formula (- (gvl :parent :window :parent :width)
					   10)))
    (opal:update win1)
    (opal:update scroll-gadget)
    (s-value scroll-gadget :width (formula `(+ 1 (gv ',win1 :width))))
    (s-value scroll-gadget :height (formula `(- (gv ',win1 :height)
					      Form-Header-Height 2)))
    #+COMMENT
    (setq inter (create-instance NIL inter:multifont-text-interactor
		  (:start-where NIL)	;will be started explicitly
		  (:start-event NIL)	;will be started explicitly
		  (:word-wrap-p T)
		  (:main-window win1)
		  ;; window set below
		  (:stop-event :zip)	;never stop
		  (:abort-event NIL)
		  (:running-action
		   #'(lambda(inter obj ev)
		       (setq *Current-Formula-Win*
			     (g-value inter :main-window))
		       (call-prototype-method inter obj ev)))
		  (:obj-to-change str)))
    (opal:add-component agg header)
    (opal:add-component (g-value scroll-gadget :inner-aggregate)
			str)
    (s-value win1 :header header)
    (s-value win1 :inter-windows (list (g-value scroll-gadget :inner-window)
				       (g-value scroll-gadget :clip-window)))
    (s-value win1 :edit-string str)
    (s-value win1 :edit-interactor inter)
    (inter:set-focus focus-inter str)
    (opal:update win1)
    (push win1 *All-Windows*)		; so will be deleted on exit
    win1))
|#



(defun New-Formula-Win (left top)
  ;; Create the main window.
  (let ((win (create-instance nil inter:interactor-window
	       (:title "C32 Formula Window")
	       (:left left) (:top top)
	       (:width Formula-Win-Width) (:height Formula-Win-height)))
	(header (create-instance NIL Form-Win-Header))
	(top (create-instance nil opal:aggregate))
	text1 scroll-win1 focus-inter mouse-inter)
    (s-value win :aggregate top)
    (opal:add-component top header)
    (s-value win :header header)
    (opal:update win)
    ;; Create the internal multifont object
    (setf text1 (create-instance nil opal:multifont-text
		  (:word-wrap-p t)
		  (:auto-scroll-p T)
		  (:lisp-mode-p T)
		  (:match-parens-p T)))
    (s-value text1 :text-width
	     (o-formula (gvl :scrolling-window :clip-window :width)))
    (setf scroll-win1
	  (create-instance nil
	      garnet-gadgets:motif-scrolling-window-with-bars
	    (:left 0) (:top Form-Header-Height)
	    (:width (o-formula (- (gvl :parent-window :width)
				 (* 2 (gvl :border-width)))))
	    (:height (o-formula (- (- (gvl :parent-window :height)
				      Form-Header-Height)
				   (* 2 (gvl :border-width)))))
	    (:total-width (o-formula (+ (gvl :text1 :width)
					(gvl :text1 :left)) 200))
	    (:total-height (o-formula (+ (gvl :text1 :top)
					 (gvl :text1 :height)) 200))
	    (:h-scroll-bar-p nil)
	    (:v-scroll-bar-p t)))
    (s-value text1 :scrolling-window scroll-win1)
    (s-value scroll-win1 :parent-window win)
    (s-value scroll-win1 :text1 text1)
    (opal:update scroll-win1)
    (opal:add-component (g-value scroll-win1 :inner-aggregate) text1)

    ;; Create a focus interactor so that keyboard events may be entered into
    ;; the text objects.
    (setf focus-inter (create-instance nil inter:focus-multifont-textinter
			(:window `(,win ,(g-value scroll-win1 :clip-window)
				   ,(g-value scroll-win1 :inner-window)))))
    ;; Create a selection interactor to handle mouse events.
    (setf mouse-inter
	  (create-instance nil inter:selection-interactor
	    (:window `(,win ,(g-value scroll-win1 :clip-window)
		       ,(g-value scroll-win1 :inner-window)))
	    (:focus-interactor focus-inter)
	    #+COMMENT
	    (:text-list `(,text1))
	    (:start-where `(:in-box ,text1))))
    (s-value win :edit-string text1)
    (s-value win :edit-interactor mouse-inter)
    (inter:set-focus focus-inter text1)
    (opal:update win)
    win))



(defun Do-Form-Cancel (gadget item)
  (if lapidary-p
    ;; Specialized version.
    (lapidary-Do-Form-Cancel gadget item)
    ;; Stand-alone version.
    (let ((win (g-value gadget :window)))
      (setq *Current-Formula-Win* NIL)
      (s-value win :visible NIL)
      (push win Formula-Wins-Available))))



(defun Do-Form-Ok (gadget item)
  (if lapidary-p
    ;; Specialized version.
    (lapidary-Do-Form-Ok gadget item)
    ;; Stand-alone version.
    (let* ((win (g-value gadget :window))
	   (obj (g-value win :c32-obj))
	   (slot (g-value win :c32-slot))
	   (item (g-value win :c32-item))
	   (valstr (opal:get-string (g-value win :edit-string)))
	   (*current-formula-obj* obj)
	   (*current-formula-slot* slot))
      (setq *Current-Formula-Win* NIL)
      (multiple-value-bind (val ok-p)
	  (Convert-Str-To-Formula valstr)
	(case ok-p
	  ((t)
	   (s-value obj slot val)	; formula
	   ;; if formula added or removed, won't necessarily notice
	   (kr:recompute-formula item :formula-p))
	  (:val
	   (destroy-constraint obj slot) ; regular value
	   (s-value obj slot val))
	  (:empty
	   (destroy-constraint obj slot)) ; empty value, use old value
	  ((NIL)
	   (return-from Do-Form-Ok)))	; error already reported
	(s-value win :visible NIL)
	(push win Formula-Wins-Available)))))


#|
(defmacro ignore-error-give-message (form error-message &rest body)
  `(multiple-value-bind (val errorp)
    #+cmu (ignore-errors ,form)
    #+lucid (system::ignore-errors ,form)
    #+allegro (excl::ignore-errors ,form)
    #-(or cmu lucid allegro) ,form

    ;; Report error message, if possible.
    (if (and errorp (not (numberp errorp)))
      #+(or allegro-v4.0 allegro-v4.1)
      (C32error (format nil
			"~A~%~%~A"
			(format nil (slot-value errorp 'excl::format-control)
				(slot-value errorp 'excl::format-arguments))
			,error-message))
      #-(or allegro-v4.0 allegro-v4.1)
      (C32error ,error-message))

    ;; Now process the body.
    ,@body))
|#


;;; Returns a pair of values: new-val OK-P.  If OK, then new-val is
;;; valid, otherwise it failed to be read.
;;; If <check-formula-p>, make sure the expression can be evaluated (as part
;;; of a formula).
;;; If <read-value-too> is true, the first entry may be followed by a
;;; space and then a value.  This can be used, for example, to read a slot
;;; name and value in one operation
;;;
(defun C32-Careful-String-Eval (new-str check-formula-p read-value-too)
  (let ((*package* *c32-package*)
	(checked-p nil)
	(formula-message
	 "Error while parsing formula, please re-edit string or press Cancel")
	final-value formula-value no-error had-value second-value)
    (multiple-value-bind (val errorp)
	(gg:Careful-Eval `(read-from-string ,new-str) error-gadget-object
			 (if check-formula-p
			     formula-message
			     "Please enter a correct value"))
      (multiple-value-setq (final-value no-error)
      (cond ((not (numberp errorp))	; invalid
	     (values new-str NIL))
	    ((and (symbolp val) (boundp val)) ; if a symbol, eval it
	     (values (eval val) T))
	    ((null val)
	     (values NIL T))
	    ((and val (listp val) check-formula-p)
	     ;; check and make sure there are no direct references. Only
	     ;; continue if check-for-direct-ref returns nil
	     (if (null (check-for-direct-ref val))
		 ;; Check and make sure this will work as a formula.
		 (let ((new-formula (formula val))) ; make formula here.
		   (multiple-value-bind (value errorp)
		       (gg:Careful-Eval-Formula-Lambda val error-gadget-object
			 formula-message *current-formula-obj*
			 *current-formula-slot* new-formula T)
		     (setf formula-value value)
		     (setf checked-p T)
		     (if #+(or allegro-V3.1 lucid) errorp
			 #-(or allegro-V3.1 lucid) (typep errorp 'condition)
			 (values val NIL)
			 (values new-formula T))))
		 ;; else there were direct references that the user
		 ;; wants to edit
		 (values val nil)))
	    ((listp val)
	     (if (fboundp (car val))
	       ;; Evaluate this as a function application.
	       (multiple-value-bind (value errorp)
		   (gg:Careful-Eval val error-gadget-object
				    (if check-formula-p
				      formula-message
				      "Please enter a correct value"))
		 (if #+(or allegro-V3.1 lucid) (not errorp)
		     #-(or allegro-V3.1 lucid) (not (typep errorp 'condition))
		     (values value T)))
	       ;; Just return the values
	       (values val T)))
	     (t
	      (values val T))))
      (when (and no-error read-value-too (> (length new-str) errorp))
       (setf new-str (subseq new-str errorp))
       (setf had-value T)
       (setf second-value (C32-Careful-String-Eval new-str NIL NIL)))
      (if no-error
	  (let ((result (if *current-formula-obj*
			  (check-slot-type *current-formula-obj*
					 *current-formula-slot*
					 (if checked-p
					     formula-value
					     final-value)
					 NIL)
			  T)))
	    (unless (eq result T)
	      (c32error result)
	      (setf no-error NIL))))
      (if had-value
	  (values final-value no-error second-value)
	  (values final-value no-error)))))


#|
(defun Careful-Read-From-String (new-str check-formula-p read-value-too)
  (let ((checked-p nil)
	(formula-message
	 "Error while parsing formula, please re-edit string or press Cancel")
	final-value formula-value no-error had-value second-value)
    (ignore-error-give-message
     (let ((*package* *c32-package*))
       (read-from-string new-str))
     (if check-formula-p
       formula-message
       "Please enter a correct value")
     (multiple-value-setq (final-value no-error)
       (cond ((not (numberp errorp))	; invalid
	      (values new-str NIL))
	     ((and (symbolp val) (boundp val)) ; if a symbol, eval it
	      (values (eval val) T))
	     ((null val)
	      (values NIL T))
	     ((and val (listp val) check-formula-p)
	      ;; check and make sure there are no direct references. Only
	      ;; continue if check-for-direct-ref returns nil
	      (if (null (check-for-direct-ref val))
		;; Check and make sure this will work as a formula.
		(let ((value val)
		      (new-formula (formula val))) ; make formula here.
		  (ignore-error-give-message
		   ;; Try out the formula without actually installing it.
		   (let ((kr::*schema-self* *current-formula-obj*)
			 (kr::*schema-slot* *current-formula-slot*)
			 (kr::*current-formula* new-formula)
			 (kr::*warning-on-null-link* T))
		     (setf formula-value (catch 'kr::no-link
					   (eval value)))
		     (setf checked-p T))
		   formula-message
		   (setf val val)	; suppress compiler error message
		   (if (or (null errorp) (numberp errorp))
		     (progn
		       (setf checked-p T)
		       (values new-formula T))
		     (values value NIL))))
		;; else there were direct references that the user
		;; wants to edit
		(values val nil)))
	     ((listp val)
	      (if (fboundp (car val))
		;; Evaluate this as a function application.
		(ignore-error-give-message
		 (eval val)
		 (if check-formula-p
		   formula-message
		   "Please enter a correct value")
		 (if errorp NIL (values val T)))
		;; Just return the values
		(values val T)))
	     (t
	      (values val T))))
     (when (and no-error read-value-too (> (length new-str) errorp))
       (setf new-str (subseq new-str errorp))
       (setf had-value T)
       (setf second-value (careful-read-from-string new-str NIL NIL)))
     (if no-error
       (unless (opal::legal-type-p *current-formula-obj* *current-formula-slot*
				   (if checked-p formula-value final-value))
	 (setf no-error NIL)))
     (if had-value
       (values final-value no-error second-value)
       (values final-value no-error)))))
|#



;; Returns two values.  First is new value.
;; second is control, either T for Formula, :val for reg values, :empty
;; for empty string, or NIL for error.
;;
(defun convert-str-to-formula (str)
  (if (zerop (length str))
    ;; then remove the formula
    (values NIL :empty)
    ;; otherwise check if legal
    (multiple-value-bind (expr ok-p)
	(C32-Careful-String-Eval str T NIL)
      (if ok-p
	(if (formula-p expr)
	  (values expr T)
	  (values expr :val))
	;; else failed to read
	(values NIL NIL)))))


(defparameter formula-y-offset 0
  "Offset from one formula window to the next")


;;; ***** Main entry point for creating a new formula window
;;;
(defun Assign-formula-win (obj slot left top c32item)
  ;; Do we already have a window displaying this formula?
  (dolist (w *all-windows*)
    (when (and (schema-p w)
	       (g-value w :visible)
	       (eq (g-local-value w :c32-obj) obj)
	       (eq (g-local-value w :c32-slot) slot))
      (opal:raise-window w)
      (return-from assign-formula-win w)))
  (let ((win (pop Formula-Wins-Available))
	(orig-form (get-value (g-value c32item :obj) (g-value c32-item :slot)))
	str)
    (unless win
      (incf left formula-win-offset)
      (incf formula-y-offset 40)
      (setq win (New-Formula-Win left (+ top formula-y-offset)))
      (setf formula-win-offset (mod (+ formula-win-offset 40) 160)))
    (s-value win :c32-item c32item)
    (s-value win :visible T)
    (s-value win :c32-obj obj)
    (s-value win :c32-slot slot)
    #+COMMENT
    (let ((Inter (g-value win :edit-interactor))
	  (inter-windows (g-value win :inter-windows)))
      (s-value inter :window inter-windows)
      (inter:start-interactor inter))
    (s-value (g-value win :header) :slot slot)
    (s-value (g-value win :header) :obj obj)
    (setq str (Get-Formula-String orig-form))
    (let ((string (g-value win :edit-string)))
      (opal:set-text string str)
      (s-value string :width (g-value string :text-width))
      (s-value string :cursor-index (length str)))
    (setq *Current-Formula-Win* win)
    (s-value win :copy-edit-string str)
    win))


;;; Returns multiple values: (expr formula-p)
;;;
(defun Get-Formula-Expr (form)
  (if (formula-p form)
      ;; A formula.
      (values (kr::get-lambda form) T)
      ;; else not a formula, just use the value
      (values form NIL)))

(defun Get-Formula-String (form)
  (let ((kr::*print-as-structure* NIL))
    (write-to-string (Get-Formula-Expr form) :pretty T)))

(defun Insert-Obj-Ref-Into-Formula (to-obj to-slot)
  (let (ref)
    (when (and *current-formula-win* to-obj)
      (setq ref (get-reference-for to-obj to-slot
				   (g-value *current-formula-win* :c32-obj)
				   (g-value *current-formula-win* :c32-slot)))
      (let ((text-object (g-value *current-formula-win* :edit-string)))
	(opal:insert-string text-object ref)
	(opal:go-to-prev-char text-object)
	(opal:go-to-prev-char text-object)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *** Top level Routines to add things into the current formula

;;; Called when middle button on a slot
;;; OBSOLETE - no longer called.
;;;
(defun Secondary-select-item (inter item)
  (declare (ignore inter))
  (let ((to-obj (g-value item :obj))
	(to-slot (g-value item :slot)))
    (Insert-Obj-Ref-Into-Formula to-obj to-slot)))


(defun InsertFunction (gadget sel)
  (declare (ignore sel))
  ;; Make this window the current formula window.
  (setf *current-formula-win* (g-value gadget :parent :parent :window))
  (if (and (boundp 'pop-up-functions)
	   (g-value pop-up-functions :window)
	   (g-value pop-up-functions :window :visible))
    (let ((text-object (g-value *current-formula-win* :edit-string))
	  (val (concatenate 'simple-string
			    "(" (car (g-value pop-up-functions :menu :value))
			    " ) ")))
      (opal:insert-string text-object val)
      (opal:go-to-prev-char text-object)
      (opal:go-to-prev-char text-object)
      #+COMMENT
      (s-value (g-value pop-up-functions :window) :visible NIL))
    (gilt:show-in-window pop-up-functions)))



;; Does same thing as middle button, but uses the selected slot, rather than
;; the secondary selection slot.
;;
(defun InsertRefFromSpread (gadget item)
  (declare (Ignore item))
  ;; Make this window the current formula window.
  (setf *current-formula-win* (g-value gadget :parent :parent :window))
  (let ((c32item (get-selected-item)))
    (if c32item
	(let ((to-obj (g-value c32item :obj))
	      (to-slot (g-value c32item :slot)))
	  (Insert-Obj-Ref-Into-Formula to-obj to-slot))
	; else nothing selected
	(inter:beep))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; *** Top level routine to generate references for a slot to another object
;;; to-slot may be NIL if don't know what slot, or T if should use
;;; object itself
(defun Get-Reference-For (to-obj to-slot from-obj from-slot)
  (declare (ignore from-slot))
  (let ((list-from (get-all-aggregadgets from-obj))
	(list-to (get-all-aggregadgets to-obj))
	ref)
    (when (null to-slot) (setq to-slot ""))
    (if (setq ref (Check-aggregadget-ref from-obj to-obj list-from list-to))
	(if (eq to-slot T) ; reference to the object itself
	    (if (string= ref "")
		"(gv :SELF)" ; then return SELF
		(concatenate 'simple-string "(gvl" ref ") "))
	    ;; else use to-slot of object
	    (concatenate 'simple-string "(gvl" ref " "
			 (Mk-Colon-Str-And-Clip to-slot 999) ") "))
	;; else not in an aggregadget, use direct reference
	(if (eq to-slot T) ; reference to the object itself
	    (Mk-String to-obj) ; just use object
	    ;; else access the slot of the object
	    (concatenate 'simple-string "(gv " (Mk-String to-obj) " "
		     (Mk-Colon-Str-And-Clip to-slot 999) ") "
		     )))))

;; returns a list of obj and all :parents of obj (recursively) which are
;; aggregadgets, in reverse order (obj is last in the list)
(defun Get-All-Aggregadgets (obj)
  (let ((p obj)
	(l (list obj)))
    (loop
     (setq p (g-value p :parent))
     (if (and p (is-a-p p opal:aggregadget))
	 (push p l)
	 (return))) ; else exit from loop
    l))

;; return the first element that is in both list1 and list2.  Unfortunately,
;; intersection is not guaranteed to return the list in order
(defun first-same (list1 list2)
  (dolist (item list1)
    (when (member item list2)
      (return item))))

(defun Check-aggregadget-ref (from-obj to-obj list-from list-to)
  (let (l s1 s2)
    (cond ((member to-obj list-from)
	   (generate-up-path to-obj list-from))
	  ((member from-obj list-to)
	   (generate-down-path from-obj list-to to-obj))
	  ((and (and (> (length list-from) 1)
		     (> (length list-to) 1)) ; otherwise, no aggregadgets
		(setq l (first-same list-from list-to))
		(setq s1 (generate-up-path l list-from))
		(setq s2 (generate-down-path l list-to to-obj))) ; may fail
	     (concatenate 'simple-string s1 s2))
	    (t NIL)))) ; otherwise, not related

;; given the obj is a parent on the list l, generate the correct number of
;; :parents as a string
(defun Generate-Up-Path (obj l)
  (let ((num (- (length l)
		(position obj l)
		1))
	(outs ""))
    (dotimes (i num)
      (setq outs (concatenate 'simple-string outs " :parent")))
    outs))

;; given that child is a child, and agg is an aggregadget in the list l, find
;; the slots of the aggregadgets that name the children
(defun Generate-Down-Path (agg l child)
  (declare (ignore child))
  (let* ((pos (position agg l))
	 (l (nthcdr (1+ pos) l))
	 (p agg)
	 (outs "")
	 known-as)
    (dolist (cur-agg l)
      (setq known-as (g-value cur-agg :known-as))
      (if known-as
	  (progn
	    (unless (eq (g-value p known-as)
			cur-agg)
	      (c32error
	       (format NIL "object ~s has known-as slot ~s but parent ~s
doesn't call it that" cur-agg known-as p))
	      (return-from generate-down-path NIL))
	    ; all ok here
	    (setq outs (concatenate 'simple-string outs " :"
				    (symbol-name known-as)))
	    (setq p cur-agg))
	  ;; else no known-as, so exit
	  (return-from generate-down-path NIL)))

    outs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Guess reference slot from an object

(defun Guess-Slot-For-Graphical-object (from-obj from-slot to-obj x y)
  (let* ((from-formula-so-far (g-value *current-formula-win* :edit-string))
	 (candidate-list
	  (case from-slot
	   (:left 0)
	   (:top 1)
	   ((:width :height :line-style :filling-style :foreground-color
	     :visible :justification :fill-background-p :actual-heightp
			:draw-function :font :radius)
	    (if (eq from-obj to-obj) ; if same object, error
		(progn (inter:beep) NIL)
		from-slot))
	   ((:x1 :x2) 0)
	   ((:y1 :y2) 1)
	   (:point-list (if (is-a-p to-obj opal:multipoint)
			    :point-list
			    (if
			     (Check-Formula-For-List-even-odd
			      from-formula-so-far)
			     0
			     1)))
           (:string
	    (cond ((eq from-obj to-obj) ; if same object, error
		   (progn (inter:beep) NIL))
		  ((is-a-p to-obj opal::window) :title) ; else use same slot
		  ((is-a-p to-obj opal:text) :string)
		  (T NIL))) ; otherwise, I don't know!
           ((:title :icon-title)
	    (cond ((eq from-obj to-obj) ; if same object, use other slot
		   (if (eq from-slot :title) :icon-title :title))
		  ((is-a-p to-obj opal::window) from-slot) ; else use same slot
		  ((is-a-p to-obj opal:text) :string)
		  (T NIL))) ; otherwise, I don't know!
	   ((:start-where :running-where :feedback-obj :obj-to-change)
	    T) ; for these slots, want object itself
	   (T NIL)))) ; otherwise, don't know which slot to use!
    (if (numberp candidate-list)
	;; have to pick one from the list (lefttop widthheight x1y1 x2y2)
	(Pick-Complicated-Slot from-obj from-slot to-obj x y candidate-list)
	;; else just use the candidate-list
	candidate-list)))

;; search backwards in the current list in the string to find the
;; number of atoms, and return T if even and NIL if odd
(defun Check-Formula-For-List-even-odd (string-obj)
  (let* ((str (g-value string-obj :string))
	 (pos (g-value string-obj :cursor-index))
	 (cnt (search-back-unbalanced-paren str pos)))
    (if cnt (evenp cnt) T)))

(defun search-back-unbalanced-paren (str pos)
  (let ((paren-cnt 0)
	(atom-cnt 0)
	(in-item NIL)
	indx c)
    (dotimes (i pos)
      (setq indx (- pos i 1))
      (setq c (elt str indx))
      (case c
	(#\(
	     (if (zerop paren-cnt)
		 (progn
		   (when in-item (incf atom-cnt))
		   (return-from search-back-unbalanced-paren atom-cnt))
		 (progn
		   (decf paren-cnt)
		   )))
	(#\) (when (zerop paren-cnt)
	       (setq in-item T))
	     (incf paren-cnt))
	((#\space #\newline) (when (zerop paren-cnt)
			       (when in-item
				 (incf atom-cnt)
				 (setq in-item NIL))))
	(T (setq in-item t))))
    NIL))

;;; x-p is 0 if x and 1 if y
(defun Pick-Complicated-Slot (from-obj from-slot to-obj x y x-p)
  (declare (ignore from-obj from-slot)) ;maybe be smarter and use them someday
  (let ((xory (if (zerop x-p) x y)))
    (if (or (is-a-p to-obj opal:line)
	    (g-value to-obj :line-p))
	(Check-Which-closer xory to-obj (if (zerop x-p) :x1 :y1)
			    (if (zerop x-p) :x2 :y2) NIL)
	;; else see which side of object is closer to mouse
	(Check-Which-Closer xory to-obj (if (zerop x-p) :left :top)
			    (if (zerop x-p) :width :height) T))))

(defmacro swap (a b)
  `(let ((tmp ,a))
    (setq ,a ,b)
    (setq ,b tmp)))

;; IF wh-p then s2 is a width or height (so must be added to s1)
;;
(defun Check-Which-closer (val obj s1 s2 wh-p)
  (let* ((val1 (g-value obj s1))
	 (val2 (g-value obj s2)))
    (when (and (numberp val1)(numberp val2))
      (if wh-p
	  (setq val2 (+ val1 val2))
	  (when (> val1 val2) (swap val1 val2)(swap s1 s2)))
      (let* ((p1 (floor (- val2 val1) 3))
	     (v1 (+ val1 p1))
	     (v2 (- val2 p1)))
	(cond ((< val v1) s1)
	      ((> val v2) s2)
	      (T NIL))))))


;;; Point to an object not in the spreadsheet and put it into the formula
(defun InsertRefFromMouse (gadget item)
  (declare (Ignore item))
  ;; Make this window the current formula window.
  (setf *current-formula-win* (g-value gadget :parent :parent :window))
  (if (schema-p *current-formula-win*)
    (Pop-Up-Request-Point-Object #'Insert-Obj-Ref-Into-Formula
				 #'Guess-Slot-For-Graphical-object
				 (g-value *current-formula-win* :c32-obj)
				 (g-value *current-formula-win* :c32-slot))
    (inter:beep)))



;; Lapidary interface version.
;; Lapidary tries to copy links as well as the formula; however, Lapidary
;; cannot assure that the links will be copied correctly. For example,
;; if a link has the formula (gvl :parent :frame), Lapidary will copy the
;; formula but it will only work if the object that the formula is copied
;; to has the appropriate structure
;;
(defun Start-Copy-Formula (from-c32item to-c32item)
  (let* ((from-obj (g-value from-c32item :obj))
	 (from-slot (g-value from-c32item :slot))
	 (to-obj (g-value to-c32item :obj))
	 (to-slot (g-value to-c32item :slot))
	 (links (g-value from-obj :links))
	 (from-form (get-value from-obj from-slot))
	 (depends-on (kr::i-depend-on from-obj from-slot))
	 slot)
    (multiple-value-bind (expr formula-p)
	(Get-Formula-Expr from-form)
      (if formula-p
	(progn
	  (multiple-value-bind (obj-list slot-list)
	      (recursive-list-find-refs expr NIL NIL)
	    (declare (ignore obj-list))
	    (multiple-value-bind (from-slots to-slots)
		(Map-Slots from-slot to-slot slot-list)
	      (Pop-Up-Confirm-Copy-Formula from-obj from-slot to-obj to-slot
					   from-slots to-slots
					   expr to-c32item)))

	  ;; copy any slots that are in from-obj but not in to-obj to
	  ;; to-obj
	  (dolist (obj-slot depends-on)
	    (setf slot (cdr obj-slot))
	    (when (and (eq (car obj-slot) from-obj)
		       (not (has-slot-p to-obj slot)))

	      ;; copy the value of slot into to-obj
	      (if (formula-p (get-value from-obj slot))
		(s-value to-obj slot
			 (copy-formula (get-value from-obj slot)))
		(s-value to-obj slot (g-value from-obj slot)))

	      ;; if this slot is a link slot, push the link onto
	      ;; to-obj's link list
	      (when (or (member slot links)
			(member slot
				'(:left-over :top-over :width-over
				  :height-over :x1-over :y1-over
				  :x2-over :y2-over)))
		(push slot (g-value to-obj :links))))))

	;; else just copy the value
	(progn
	  (destroy-constraint to-obj to-slot) ; in case used to be a formula
	  (s-value to-obj to-slot expr)
	  ;; * if formula added or removed, won't necessarily notice
	  (kr:recompute-formula to-c32item :formula-p))))))
#|
(defun Start-Copy-Formula (from-c32item to-c32item)
  (let* ((from-obj (g-value from-c32item :obj))
	 (from-slot (g-value from-c32item :slot))
	 (to-obj (g-value to-c32item :obj))
	 (to-slot (g-value to-c32item :slot))
	 (from-form (get-value from-obj from-slot)))
    (multiple-value-bind (expr formula-p)
	(Get-Formula-Expr from-form)
      (if formula-p
	(progn
	  (multiple-value-bind (obj-list slot-list)
	      (recursive-list-find-refs expr NIL NIL)
	    (declare (ignore obj-list))
	    (multiple-value-bind (from-slots to-slots)
		(Map-Slots from-slot to-slot slot-list)
	      (Pop-Up-Confirm-Copy-Formula from-obj from-slot to-obj to-slot
					   from-slots to-slots
					   expr to-c32item))))
	;; else just copy the value
	(progn
	  (destroy-constraint to-obj to-slot) ; in case used to be a formula
	  (s-value to-obj to-slot expr)
	  ;; * if formula added or removed, won't necessarily notice
	  (kr:recompute-formula to-c32item :formula-p))))))
|#


#+sbcl
(defmacro define-constant (name value &optional doc)
       `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                           ,@(when doc (list doc))))
(#+sbcl define-constant
 #-sbcl defconstant
 x-type-slots '(:left :width :x1 :x2))
(#+sbcl define-constant
 #-sbcl defconstant
 y-type-slots '(:top :height :y1 :y2))

;; Maps slots for copy from x to y type or visa-versa
(defun map-slots (from-slot to-slot slot-list)
  (if (or (and (member from-slot x-type-slots)
	       (member to-slot y-type-slots))
	  (and (member to-slot x-type-slots)
	       (member from-slot y-type-slots)))
      ;; then do mapping
      (let (from-slots to-slots pos)
	(dolist (s slot-list)
	  (cond ((setq pos (position s x-type-slots))
		 (push s from-slots)
		 (push (elt y-type-slots pos) to-slots))
		((setq pos (position s y-type-slots))
		 (push s from-slots)
		 (push (elt x-type-slots pos) to-slots))
		(T NIL)))
	(values from-slots to-slots))
      ;; else return NILs
      (values NIL NIL)))



;; call this to actually copy the formula and insert it into the
;; object when the user hits OK in the dialog box
(defun copy-and-set-formula (old-expr to-obj to-slot
				      old-slot-list new-slot-list
				      to-c32item)
  (let ((copy-old-expr (copy-tree old-expr)))
    (when old-slot-list
      (recursive-subst-obj-slot copy-old-expr NIL NIL
				old-slot-list new-slot-list NIL))
    (set-formula-val copy-old-expr to-obj to-slot to-c32item)))


;;; set the value of the slot to the new expression, which should be
;;; put as a formula.  May go into the formula window if there, or else into
;;; the object directly.
;;;
(defun set-formula-val (expr obj slot c32-item)
  (if (and *current-formula-win*
	   (g-value *current-formula-win* :visible)
	   (eq obj (g-value *current-formula-win* :c32-obj))
	   (eq slot (g-value *current-formula-win* :c32-slot)))
    ;; then showing this formula
    (let ((obj (g-value *current-formula-win* :edit-string)))
      (opal:set-text obj (write-to-string expr :pretty T))
      (s-value obj :cursor-index 0))
    ;; else not showing it
    (let ((newform (formula expr)))
      (s-value obj slot newform)
      (kr:recompute-formula c32-item :formula-p))))


(defun recursive-list-find-refs (expr obj-list slot-list)
  (if (or (eq (car expr) 'gv)(eq (car expr) 'gvl))
      (let ((objref (cdr (butlast expr))))
	(when objref
	  (pushnew objref obj-list :test #'equal)
	  (pushnew (car (last expr)) slot-list))
	(values obj-list slot-list))
      ;; else look for lists in expr
      (progn
	(dolist (e expr)
	  (when (listp e)
	    (multiple-value-setq (obj-list slot-list)
	      (recursive-list-find-refs e obj-list slot-list))))
	(values obj-list slot-list))))


;; substitutes objects in new-obj for the corresponding objects in old-obj
;; (objects in old-obj-list should be in a list: ( (:zup :zup) (obj) )
;; new-obj-list is just a one-level list: (zupzup newobj)
;; similarly for slots, but both are one-level lists.
;; Modifies obj-ref-list-so-far to be the right way to reference the
;; objects.  Should be a list of the same length as old-obj-list
(defun recursive-subst-obj-slot (expr old-obj-list new-obj-list
				      old-slot-list new-slot-list
				      obj-ref-list-so-far)
  (if (or (eq (car expr) 'gv)(eq (car expr) 'gvl))
      ; then check if need to substitute
      (progn
	;; first do obj
	(when (and old-obj-list (> (length expr) 2))
	  (let* ((old-obj (cdr (butlast expr)))
		 (indx (position old-obj old-obj-list :test #'equal))
		 new-obj)
	    (when indx
	      (when obj-ref-list-so-far
		(rplaca (nthcdr indx obj-ref-list-so-far)
			(gen-good-obj-ref expr)))
	      (setq new-obj (nth indx new-obj-list))
	      (when (eq (car expr) 'gvl)
		;; change to gv
		(rplaca expr 'gv))
	      (rplaca (cdr expr) new-obj)
	      (rplacd (cdr expr) (last expr)))))
	;; now test slot
	(when old-slot-list
	  (let* ((old-slot (car (last expr)))
		 (indx (position old-slot old-slot-list))
		 new-slot)
	    (when indx
	      (setq new-slot (nth indx new-slot-list))
	      (rplaca (last expr) new-slot)))))
      ;; otherwise, not a gv statement; look for lists in expr
      (progn
	(dolist (e expr)
	  (when (listp e)
	    (recursive-subst-obj-slot e old-obj-list new-obj-list
				      old-slot-list new-slot-list
				      obj-ref-list-so-far))))))


;; returns a good reference for the object in the expression
(defun gen-good-obj-ref (expr)
  (if (eq (car expr) 'gv)
      (if (> (length expr) 3)
	  (butlast expr)
	  (second expr))
      (butlast expr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Insert-Func-Into-Form (gadget val)
  (if (and *current-formula-win* (g-value *current-formula-win* :visible))
      (let ((text-object (g-value *current-formula-win* :edit-string)))
	(setq val (car (g-value gadget :value)))
	(setq val (concatenate 'simple-string "(" val " ) "))
	(opal:insert-string text-object val)
	(opal:go-to-prev-char text-object)
	(opal:go-to-prev-char text-object))
      ;; else no window
      (inter:beep)))
