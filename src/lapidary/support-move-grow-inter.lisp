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
;;;  Support functions for Move/Grow Interactor dialog box
;;;

(in-package "LAPIDARY")

(defvar move-grow-inter-win nil)
(defvar move-grow-inter-agg nil)
(defvar *MOVE-GROW-INTER-QUEUE* NIL)
(defvar *move-grow-box-win* nil)

(defun move-grow-inter-do-stop ()
  (when (and (boundp 'MOVE-GROW-INTER-AGG) MOVE-GROW-INTER-AGG)
	(opal:destroy MOVE-GROW-INTER-AGG)))

(defmacro MOVE-GROW-START-WHERE ()
  `(g-value MOVE-GROW-INTER-MENU :start-where))
(defmacro MOVE-GROW-OBJ-PROTOTYPES ()
  `(g-value MOVE-GROW-INTER-MENU :prototype-objs :obj-prototypes))
(defmacro MOVE-GROW-FEEDBACK-PROTOTYPES ()
  `(g-value MOVE-GROW-INTER-MENU :prototype-objs :feedback-prototypes))
(defmacro MOVE-GROW-FEEDBACK-OBJ ()
  `(g-value MOVE-GROW-INTER-MENU :feedback-obj))
(defmacro MOVE-GROW-GROW-PARM ()
  `(g-value MOVE-GROW-INTER-MENU :grow-parm))
(defmacro MOVE-GROW-MOVE-PARM ()
  `(g-value MOVE-GROW-INTER-MENU :move-parm))
(defmacro MOVE-GROW-OTHER-BOX ()
  `(g-value MOVE-GROW-INTER-MENU :start-where :contents :other-box))
(defmacro MOVE-GROW-OTHER-BUTTON ()
  `(g-value MOVE-GROW-INTER-MENU :start-where :contents :other-button))

;;; determine which slots in the feedback object and the objects that
;;; the interactor operates on should be tied to the :box slot
(defun set-up-box-slots (new-inter slot-value-list)
  (declare (special box-params))
  (let ((feedback-objs
	 (if (formula-p (get-local-value new-inter :feedback-obj))
	     (cdr (assoc :feedback-prototypes slot-value-list))
	     (g-local-value new-inter :feedback-obj)))
	(prototype-objs (or (cdr (assoc :obj-prototypes slot-value-list))
			    (find-start-where-objs 
			     (get-local-value new-inter :start-where)))))
    (s-value box-params :inter new-inter)
    (s-value box-params :feedback-objs feedback-objs)
    (s-value box-params :prototype-objs prototype-objs)
    (s-value (g-value box-params :box-slots) :value nil)
    (setf *move-grow-box-win* (gilt:show-in-window box-params 200 200 t))))

(defvar *box-left-formula* (formula `(first (gvl :box))))
(defvar *box-top-formula* (formula `(second (gvl :box))))
(defvar *box-width-formula* (formula `(third (gvl :box))))
(defvar *box-height-formula* (formula `(fourth (gvl :box))))

;;; place formulas that depend on the box slot in the specified set of
;;; slots for the objects operated on by the current move/grow interactor
(defun set-box-slots (gadget value-list)
  (let ((feedback-objs (g-value gadget :feedback-objs))
	(prototype-objs (g-value gadget :prototype-objs))
	(box-slots (second (car value-list))))
    (dolist (slot box-slots)
	    (let ((box-formula (case slot
				 (:left *box-left-formula*)
				 (:top *box-top-formula*)
				 (:width *box-width-formula*)
				 (:height *box-height-formula*))))
	      (dolist (obj prototype-objs)
		      (undo-save obj slot)
		      (s-value obj slot (formula box-formula)))
	      (if (listp feedback-objs)
		  (dolist (obj feedback-objs)
			  (undo-save obj slot)
			  (s-value obj slot (formula box-formula)))
		  (progn
		    (undo-save feedback-objs slot)
		    (s-value feedback-objs slot (formula box-formula))))))))
	    
(defun MOVE-GROW-INTERACTOR-NAME-FN (gadget interactor-name)
  (declare (ignore gadget))
  (dialog-enqueue :known-as
		  (if (string/= "" interactor-name)
		      (read-from-string
		       (concatenate 'string ":" interactor-name)))
		  *MOVE-GROW-INTER-QUEUE*))

;;;    :start-where is in an aggregate of items
(defun MOVE-GROW-ONE-THIS-AGG-FN (agg-box button-label)
  (declare (special move-grow-inter-menu))
  (declare (ignore agg-box))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(start-where (MOVE-GROW-START-WHERE)))
    (if selection
	(progn
	  (s-value start-where :field-string (name-for-schema selection))
	  (s-value start-where :value button-label)
	  (s-value start-where :type nil)
	  (dialog-enqueue :start-where
			  `(:element-of ,selection 
					,@(g-value start-where :type-restriction))
			  *MOVE-GROW-INTER-QUEUE*))
	(progn
	  (s-value start-where :field-string nil)
	  (s-value start-where :value nil)
	  (s-value start-where :type nil)))))

;;;    :start-where is in a single object
(defun MOVE-GROW-OBJ-PRESS-OVER-FN (obj-box button-label)
  (declare (special move-grow-inter-menu))
  (declare (ignore obj-box))
  (let ((selection (car (g-value *SELECTION-INFO* :selected)))
	(start-where (MOVE-GROW-START-WHERE)))
    (if selection
	(progn
	  (s-value start-where :field-string (name-for-schema selection))
	  (s-value start-where :value button-label)
	  (s-value start-where :type nil)
	  (dialog-enqueue :start-where
			  `(:in-box ,selection)
			  *MOVE-GROW-INTER-QUEUE*))
	(progn
	  (s-value start-where :field-string nil)
	  (s-value start-where :value nil)
	  (s-value start-where :type nil)))))

(defun move-grow-obj-prototypes-fn (inter button-label)
  (declare (special move-grow-inter-menu))
  (declare (ignore inter button-label))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (if selection
	(progn
	  (s-value (MOVE-GROW-OBJ-PROTOTYPES) :value t)
	  (s-value (MOVE-GROW-OBJ-PROTOTYPES) :field-string
		   (let* ((kr::*print-as-structure* nil))
			  (prin1-to-string selection)))
	  (dialog-enqueue :obj-prototypes selection *MOVE-GROW-INTER-QUEUE*))
	(progn
	  (dialog-enqueue :obj-prototypes nil *MOVE-GROW-INTER-QUEUE*)
	  (s-value (MOVE-GROW-OBJ-PROTOTYPES) :field-string nil)
	  (s-value (MOVE-GROW-OBJ-PROTOTYPES) :value nil)))))

(defun move-grow-feedback-prototypes-fn (inter button-label)
  (declare (special move-grow-inter-menu))
  (declare (ignore inter button-label))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (if selection
	(progn
	  (s-value (MOVE-GROW-FEEDBACK-PROTOTYPES) :value t)
	  (s-value (MOVE-GROW-FEEDBACK-PROTOTYPES) :field-string
		   (let* ((kr::*print-as-structure* nil))
			  (prin1-to-string selection)))
	  (dialog-enqueue :feedback-prototypes selection 
			  *MOVE-GROW-INTER-QUEUE*))
	(progn
	  (dialog-enqueue :feedback-prototypes nil *MOVE-GROW-INTER-QUEUE*)
	  (s-value (MOVE-GROW-FEEDBACK-PROTOTYPES) :field-string nil)
	  (s-value (MOVE-GROW-FEEDBACK-PROTOTYPES) :value nil)))))

(defun MOVE-GROW-LINE-P-FN (panel value)
  (if (string= value "<Formula>")
      (create-custom-inter-constraint (g-value panel :window :inter) 
				      :line-p '*move-grow-inter-queue*)
      (dialog-enqueue :line-p 
		      (cond ((string= value "Line") t)
			    ((string= value "Box") nil))
		      *MOVE-GROW-INTER-QUEUE*)))

(defun MOVE-GROW-GROW-P-FN (panel value)
  (if (string= value "<Formula>")
      (create-custom-inter-constraint (g-value panel :window :inter) 
				      :grow-p '*move-grow-inter-queue*)
      (dialog-enqueue :grow-p 
		  (cond ((string= value "Grow") t)
			((string= value "Move") nil))
		  *MOVE-GROW-INTER-QUEUE*)))

(defun grow-parms-final-fn (gadget value)
  (if (string= value "<Formula>")
      (progn
	(lapidary-error (format nil "~%the formula must return a value that is valid for :slots-to-set"))
	(create-custom-inter-constraint (g-value gadget :window :inter) 
					:grow-box-parms 
					'*move-grow-inter-queue*))
      (dialog-enqueue :grow-box-parms
		  (cond ((string= value "Change Height") 
			 '(nil t nil t))
			((string= value "Change Width") 
			 '(t nil t nil))
			((string= value "Change Width and Height") 
			 '(t t t t)))
		  *MOVE-GROW-INTER-QUEUE*)))

(defun move-parms-final-fn (gadget value)
  (if (string= value "<Formula>")
      (progn
	(lapidary-error (format nil "~%the formula must return a value that is valid for :slots-to-set"))
	(create-custom-inter-constraint (g-value gadget :window :inter) 
					:move-box-parms 
					'*move-grow-inter-queue*))
  (dialog-enqueue :move-box-parms
		  (cond ((string= value "Change Left") '(t nil t t))
			((string= value "Change Top") '(nil t t t))
			((string= value "Change Left and Top") 
			 '(t t t t)))
		  *MOVE-GROW-INTER-QUEUE*)))

(defun move-grow-min-length-fn (gadget value)
  (setf *move-grow-inter-queue*
	(enqueue-int-value gadget value :min-length *move-grow-inter-queue*)))

(defun move-grow-min-width-fn (gadget value)
  (setf *move-grow-inter-queue*
	(enqueue-int-value gadget value :min-width *move-grow-inter-queue*)))

(defun move-grow-min-height-fn (gadget value)
  (setf *move-grow-inter-queue*
	(enqueue-int-value gadget value :min-height *move-grow-inter-queue*)))

(defun move-grow-infer-obj-to-change (gadget value)
  (declare (ignore value))
  (let ((obj-to-change (car (g-value lapidary::*selection-info* :selected)))
	parent children formula start-where)
    ;; first make sure that there is a selection and that it is
    ;; a member of the objects acted on by this interactor
    (when (null obj-to-change)
	  (lapidary-error "Must select an object to change")
	  (return-from move-grow-infer-obj-to-change))

    (setf start-where (get-start-where *move-grow-inter-queue*
				       (g-value gadget :window :inter)))
    (when (eq start-where :not-supplied)
	  (lapidary-error "Must supply a start-where before selecting an object to change")
	  (return-from move-grow-infer-obj-to-change))
    (multiple-value-setq (parent children)
			 (find-start-where-objs-for-obj-to-change start-where))
    ;; every child in the start-where objects (by child we mean child of
    ;; an aggregate specified in the start-where or a child of an object
    ;; specified in a list) must be able to link to the obj-to-change, so
    ;; we can arbitrarily choose one to look at. For convenience, we choose
    ;; the first child
    (when (listp children)
	  (setf children (first children)))
    
    ;; we are working with a move-grow aggregate and it is quite possible
    ;; that the object specified in the start-where is an aggregate of
    ;; selection handles, so we first check to see if there is a link
    ;; from the parent object to the obj-to-change
    (when parent
	  ;; first check to see if there is :obj-over slot that points
	  ;; to the obj-to-change. this is the most common case
	  (if (eq (g-value parent :obj-over) obj-to-change)
	      (setf formula (kr::make-into-o-formula
			     (formula `(gvl :first-obj-over 
					    ,@(gilt:make-path children parent)
					    :obj-over))))
	      ;; check all slots in the parent to see if any point to
	      ;; the obj-to-change
	      (doslots (slot parent)
		 ;; no easy way to break out of doslots, so if a formula
		 ;; has been created, skip any further comparisons
		 (when (and (null formula)
			    (eq obj-to-change (g-value parent slot)))
		       (setf formula (kr::make-into-o-formula
				      (formula `(gvl :first-obj-over
						     ,@(gilt:make-path children parent)
						     ,slot))))
		       (s-formula-value formula :links (list slot))
		       (s-formula-value formula :start-where-obj parent)))))

    ;; if we could not find a link to the obj-to-change in the parent,
    ;; try the child
    (when (and (null formula) 
	       children) ; if start-where is t, nil, or :in-but-not-on,
	                 ; there is no child
	  ;; first check to see if there is :obj-over slot that points
	  ;; to the obj-to-change. this is the most common case
	  (if (eq (g-value children :obj-over) obj-to-change)
	      (setf formula (kr::make-into-o-formula
			     (formula `(gvl :first-obj-over :obj-over))))
	      ;; check all slots in the child to see if any point to
	      ;; the obj-to-change
	      (doslots (slot children)
		 ;; no easy way to break out of doslots, so if a formula
		 ;; has been created, skip any further comparisons
		 (when (and (null formula)
			    (eq obj-to-change (g-value children slot)))
		       (setf formula (kr::make-into-o-formula
				      (formula `(gvl :first-obj-over ,slot))))
		       (s-formula-value formula :links (list slot))
		       (s-formula-value formula :start-where-obj children)))))

    formula))
						     

;;;======================================================================
;;;
;;; find the set of leaves in an aggregate--this function could be written
;;; more efficiently, but since it is called so infrequently, it is
;;; easiest to write it simply
;;;
;;;======================================================================

(defun get-agg-leaves (agg)
  (let (leaves)
    (cond ((or (not (is-a-p agg opal:aggregate)) 
	       (g-value agg :pretend-to-be-leaf))
	   (setf leaves (list agg)))
	  (t 
	   (dolist (child (g-value agg :components))
		   (setf leaves (append leaves (get-agg-leaves child))))))
    leaves))

;;;======================================================================
;;;
;;; find the list of objects in the start-where that should be checked 
;;; for a pointer to the obj-to-change
;;;
;;;======================================================================

(defun find-start-where-objs-for-obj-to-change (start-where)
  (let ((control (if (listp start-where) (car start-where) start-where)))
    (case control
	  ((:in :in-box)
	   (values nil (list (second start-where))))
	  ((:element-of :element-of-or-none 
			:check-leaf-but-return-element
			:check-leaf-but-return-element-or-none)
	   (values (second start-where)
		   (g-value (second start-where) :components)))
	  ((:leaf-element-of :leaf-element-of-or-none)
	   (values (second start-where)
		   (get-agg-leaves (second start-where))))
	  ((:list-element-of
	    :list-element-of-or-none
	    :list-check-leaf-but-return-element 
	    :list-check-leaf-but-return-element-or-none)
	   (values nil (g-value (second start-where) (third start-where))))
	  ;; for leaf-elements of a list, only check the leaves
	  ;; of the first component and the first component
	  ((:list-leaf-element-of
	    :list-leaf-element-of-or-none)
	   (values (first (g-value (second start-where) (third start-where)))
		   (get-agg-leaves (first (g-value (second start-where) 
						   (third start-where))))))
	  ;; these are the cases (t nil :in-but-not-on)
	  (t
	   (values nil nil)))))
					
(defun MOVE-GROW-OBJ-TO-CHANGE-FN (panel value)
  (declare (special move-grow-inter-menu lapidary-query-gadget))
  (s-value (g-value panel :parent) :value value) 
  (cond ((string= value "<Formula>")
	 (create-custom-inter-constraint (g-value panel :window :inter) 
					 :obj-to-change
					 '*move-grow-inter-queue*))
	((string= value "Change this object")
	 (let ((obj-to-change (car (g-value lapidary::*selection-info* 
					    :selected)))
	       formula)
	       ;; first make sure that there is a selection and that it is
	       ;; a member of the objects acted on by this interactor
	   (when (null obj-to-change)
		 (lapidary-error "Must select an object to change")
		 (return-from move-grow-obj-to-change-fn))

	   (setf formula (move-grow-infer-obj-to-change panel value))
	   (if formula
	       ;; found link from start-where objects to obj-to-change
	       (dialog-enqueue :obj-to-change formula *move-grow-inter-queue*)
	       ;; need to ask the user whether they want this specific
	       ;; object to be the object to change, or whether they want
	       ;; to enter a formula that specifies a link between the
	       ;; the start-where objects and the obj-to-change
	       (progn
		 (s-value lapidary-query-gadget :selection-function nil)
		 (multiple-value-bind (left top)
		    (opal:convert-coordinates (g-value panel :window)
					      (g-value panel :left)
					      (opal:bottom panel) NIL)
		       (s-value lapidary-query-gadget :window-left left)
		       (s-value lapidary-query-gadget :window-top top)
		       (cond ((equal (garnet-gadgets:display-query-and-wait 
				    lapidary-query-gadget
				    (format nil "Lapidary cannot determine how to derive the
obj-to-change object from the start-where objects.
If there is no relationship and the interactor 
should always change the obj-to-change object,
press the \"Change Obj-to-Change\" button. If the
obj-to-change should be derived from the start-where
objects, press the \"Formula\" button and enter a
formula that derives the obj-to-change from the
start-where objects.")
				    '("Change Obj-to-Change" "Formula"))
				    "Formula")
			      ;; prompt user for a formula
			      (create-custom-inter-constraint 
			       (g-value panel :window :inter)
			       :obj-to-change
			       '*move-grow-inter-queue*))
			      ;; otherwise queue the obj-to-change
			      (t 
			       (dialog-enqueue :obj-to-change 
					       obj-to-change
					       *move-grow-inter-queue*))))))))

	(t (dialog-enqueue :obj-to-change
		      nil
		      *MOVE-GROW-INTER-QUEUE*))))
  
(defun MOVE-GROW-FEEDBACK-OBJ-FN (feedback-obj-box button-label)
  (declare (special move-grow-inter-menu *selection-info*))
  (let ((selection (g-value *SELECTION-INFO* :selected)))
    (cond ((null selection)
	   (s-value (MOVE-GROW-FEEDBACK-OBJ) :field-string nil)
	   (s-value (MOVE-GROW-FEEDBACK-OBJ) :value nil)
	   (lapidary-error "please make a selection, then press the
interim feedback button again"))
	((null (cdr selection)) ; only one selection
	 (s-value (MOVE-GROW-FEEDBACK-OBJ) :field-string
		  (name-for-schema (car selection)))
	 (s-value (MOVE-GROW-FEEDBACK-OBJ) :value button-label)
	 (dialog-enqueue :feedback-obj selection *MOVE-GROW-INTER-QUEUE*))
	(t ; multiple selections
	 (s-value (MOVE-GROW-FEEDBACK-OBJ) :field-string
		  (princ-to-string selection))
	 (s-value (MOVE-GROW-FEEDBACK-OBJ) :value button-label)
	 ;; pop up C32 and ask the user to enter a formula that
	 ;; selects which feedback object to use
	 (get-inter-feedback-formula (g-value feedback-obj-box :window :inter)
				     :feedback-obj
				     selection
				     '*move-grow-inter-queue*)))))

(defun MOVE-GROW-NIL-FEEDBACK-OBJ-FN (button button-label)
  (declare (special move-grow-inter-menu))
  (declare (ignore button))
  (dialog-enqueue :feedback-obj NIL *MOVE-GROW-INTER-QUEUE*)
  (s-value (MOVE-GROW-FEEDBACK-OBJ) :field-string nil)
  (s-value (MOVE-GROW-FEEDBACK-OBJ) :value button-label))

(defun MOVE-GROW-FORMULA-FEEDBACK-OBJ-FN (button button-label)
  (declare (special move-grow-inter-menu))
  (create-custom-inter-constraint (g-value button :window :inter) 
				  :feedback-obj
				  '*MOVE-GROW-INTER-QUEUE*)
  (s-value (MOVE-GROW-FEEDBACK-OBJ) :field-string nil)
  (s-value (MOVE-GROW-FEEDBACK-OBJ) :value button-label))

(defun move-grow-attach-point-fn (inter button)
  (declare (ignore inter))
  (let ((attach-point (g-value button :attach-point)))
    (if (eq attach-point :formula)
	(create-custom-inter-constraint (g-value button :window :inter) 
					:attach-point
					'*move-grow-inter-queue*)
        (dialog-enqueue :attach-point 
		      attach-point
		      *MOVE-GROW-INTER-QUEUE*))))

(defun MOVE-GROW-FINAL-FUNCTION-FN (labeled-box string)
  (declare (ignore labeled-box))
  (dialog-enqueue :final-function
		  (if (string= string "") nil (read-from-string string))
		  *MOVE-GROW-INTER-QUEUE*))
