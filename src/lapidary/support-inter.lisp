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
;;; This file provides the functions that initialize the interactor menus
;;;

;;; CHANGE LOG:
;;;
;;; 07/16/93 bvz - Remove reference to *C32-Custom-Function* in 
;;;                     Formula-Enqueue
;;; 07/14/93 amickish - Declared C32::*C32-Custom-Function* special in
;;;                     Formula-Enqueue

(in-package "LAPIDARY")

(defun set-db-value (agg slot value)
  (s-value (g-value agg slot) :value value))

(defun set-db-string (agg slot string)
  (s-value (g-value agg slot) :string string))

(defun set-db-field-string (agg slot string)
  (s-value (g-value agg slot) :field-string string))

(defun set-db-type (agg slot type)
  (s-value (g-value agg slot) :type type))

(defun set-db-type-restriction (agg slot type-restriction)
  (s-value (g-value agg slot) :type-restriction type-restriction))

(defun START-WHERE-ANYWHERE (button button-label)
  (declare (ignore button-label))
  (let* ((menu (g-value button :menu))
	 (queue (symbol-value (g-value button :queue))))
    (s-value (g-value menu :start-where :contents :other-button) 
	     :value nil)
    (s-value (g-value menu :start-where :contents :type-restriction) 
	     :value nil)
    (setf (symbol-value (g-value button :queue))
	  (dialog-enqueue :start-where t queue))))

;;; ====================================================================
;; convert a string to an integer and store the value in a slot. If the
;; string is non-empty and is not an integer, restore the previous value
;;; ====================================================================
(defun enqueue-int-value (gadget value slot queue)
  (if (string= value "")
      (progn
	(s-value gadget :old-value value)
	(dialog-enqueue slot nil queue))
      (let ((int-val (read-from-string value)))
	(if (integerp int-val)
	    (progn
	      (dialog-enqueue slot int-val queue)
	      (s-value gadget :old-value value))
	  ;; if not an integer, restore value to the previous value
	    (s-value gadget :value (g-value gadget :old-value)))))
  queue)


;;; ======================================================================
;;; set up data structures so that a custom-created feedback formula will be
;;; placed on the appropriate interactor queue. The selected objects must be
;;; enqueued with the formula. It is not safe to check the selected objects 
;;; after the formula has been entered, since C32 does not prevent the
;;; user from selecting and deselecting objects. Thus the selected objects
;;; must be saved before calling c32.
;;; ======================================================================

(defun get-inter-feedback-formula (obj slot feedback-info queue)
  (declare (special *inter-queue* *selection-info*))
  (setf *inter-queue* queue)
  (s-value *selection-info* :feedback-info feedback-info)
  (gg:c32 obj slot :c32-custom-function 'feedback-formula-enqueue)
  (lapidary-error "Please enter a formula that will
determine when to use each of the feedback objects."))


;;; ======================================================================
;;; store a value of the form (slot formula feedback-objs*)
;;; on the appropriate interactor queue
;;; ======================================================================

(defun feedback-formula-enqueue (obj slot value)
  (declare (ignore obj))
  (declare (special *inter-queue* *selection-info*))
  (setf (symbol-value *inter-queue*) 
	(delete slot (symbol-value *inter-queue*) :key #'car))
  (push `(,slot ,value ,@(g-value *selection-info* :feedback-info)) 
	(symbol-value *inter-queue*)))

;;; ======================================================================
;;; set up data structures so that a custom-created formula will be
;;; placed on the appropriate interactor queue
;;; ======================================================================

(defun create-custom-inter-constraint (obj slot queue &key (prompt-msg nil))
  (declare (special *inter-queue*))
  (setf *inter-queue* queue)
  (gg:c32 obj slot :c32-custom-function 'formula-enqueue)
  (when prompt-msg
	(lapidary-error prompt-msg)))

;;; ======================================================================
;;; store a value of the form (slot formula)
;;; on the appropriate interactor queue
;;; ======================================================================

(defun formula-enqueue (obj slot value)
  (declare (ignore obj))
  (declare (special *inter-queue*))
  (setf (symbol-value *inter-queue*) 
	(delete slot (symbol-value *inter-queue*) :key #'car))
  (push (cons slot value) (symbol-value *inter-queue*)))

;;; ============================================================
;;; install a formula in the appropriate interactor slot. 
;;; determine if the objects that go into the links belong to
;;; the same aggregate as the interactor, and if they do, create
;;; paths to access them; otherwise store the objects in the
;;; links.
;;; the formula-list has the form (slot formula) and the formula
;;; contains a list of links and the objects they point to
;;; ============================================================

(defun install-inter-formula (inter formula-list)
  (let* ((slot (car formula-list))
	 (formula (cdr formula-list)))
    ;; process the links first
    (c32::install-links formula inter)
    ;; now install the formula
;    (undo-save inter slot)
    (s-value inter slot formula)))

;;; ============================================================
;;; initialize a string field in an interactor dialog box
;;; ============================================================

(defun initialize-inter-db-string (inter agg slot)
  (if (g-value inter slot)
      (set-db-value agg slot (symbol-name (g-value inter slot)))
      (set-db-value agg slot "")))

;;; ============================================================
;;; initialize the start-where field of an interactor dialog
;;; box
;;; ============================================================

(defun initialize-start-where (inter agg control1 string1 control2 string2)
  (let ((start-where (g-value inter :start-where))
	control type)
    (cond ((eq start-where :not-supplied)
	   (set-db-value agg :start-where nil)
	   (set-db-field-string agg :start-where "")
	   (set-db-type agg :start-where nil)
	   (set-db-type-restriction agg :start-where nil))
	  (t 
	   ;; first set the control option, then the aggregate or object
	   ;; that the interactor operates on, and then the type
	   ;; restriction button
	   (setf control (if (listp start-where) 
			     (first start-where)
			     start-where))
	   (cond ((eq control control1)
		  (set-db-value agg :start-where string1)
		  (when (keywordp control)
			(set-db-field-string agg :start-where 
				       (name-for-schema (second start-where)))))
		 ((eq control control2)
		  (set-db-value agg :start-where string2)
		  (when (keywordp control)
			(set-db-field-string agg :start-where 
				       (name-for-schema (second start-where)))))
		 (t
		  (set-db-value agg :start-where "Other")
		  (set-db-field-string agg :start-where (symbol-name control))))
	   (setf type (when (listp start-where) (member :type start-where)))
	   (if type
	       (progn
		 (set-db-type agg :start-where t)
		 (set-db-type-restriction agg :start-where (second type)))
	       (progn
		 (set-db-type agg :start-where nil)
		 (set-db-type-restriction agg :start-where nil)))))))

;;; set the interactor menu to the values of the interactor
(defun reset-inter-menu (inter)
  (cond ((or (eq inter lapidary-button-interactor)
	     (eq inter lapidary-menu-interactor)
	     (is-a-p inter lapidary-button-interactor)
	     (is-a-p inter lapidary-menu-interactor))
	 (init-choice-inter-menu inter))
	((or (eq inter lapidary-text-interactor)
	     (is-a-p inter lapidary-text-interactor))
	 (init-text-inter-menu inter))
	((or (eq inter lapidary-two-point-interactor)
	     (is-a-p inter lapidary-two-point-interactor))
	 (init-two-point-inter-menu inter))
	((or (eq inter directional-move-grow-interactor)
	     (is-a-p inter directional-move-grow-interactor))
	 (init-move-grow-inter-menu inter))
	((or (eq inter lapidary-angle-interactor)
	     (is-a-p inter lapidary-angle-interactor))
	 (init-angle-inter-menu inter))))

(defun init-text-inter-menu (inter)
  (declare (special text-interactor-win))
  ;; call text-inter-do-go the first time the text interactor menu is
  ;; requested
  (when (or (not (boundp 'text-interactor-win)) (null text-interactor-win))
    (text-inter-do-go))
  (let ((agg (g-value text-interactor-win :aggregate)))
    (dolist (slot '(:known-as :start-where :final-function
		    :cursor-where-press :obj-to-change :start-event))
       (case slot
	  ((:known-as :final-function)
	   (initialize-inter-db-string inter agg slot))
	  (:start-where
	   (initialize-start-where inter agg t "Start Anywhere in Window"
				   :element-of "One of this aggregate"))
	  (:cursor-where-press
	   (if (g-value inter :cursor-where-press)
	       (set-db-value agg :cursor-where-press "where pressed")
	       (set-db-value agg :cursor-where-press "at end of string")))
	  (:obj-to-change
	   (let ((value (get-value inter :obj-to-change)))
	   (cond ((formula-p value)
		  (set-db-value agg :obj-to-change "<formula>"))
		 ((schema-p value)
		  (set-db-value agg :obj-to-change "change this object")
		  (s-value (g-value agg :obj-to-change :contents :choice)
			   :obj-to-change value))
		 (t 
		  (set-db-value agg :obj-to-change "result of start-where")))))))
    (s-value text-interactor-win :inter inter)))

(defun init-choice-inter-menu (inter)
  (declare (special choice-interactor-win))

  ;; call choice-inter-do-go the first time the choice interactor menu is
  ;; requested
  (when (or (not (boundp 'choice-interactor-win)) (null choice-interactor-win))
    (choice-inter-do-go))
  (let ((agg (g-value choice-interactor-win :aggregate)))
    (dolist (slot '(:known-as :start-where :final-function :is-a
		    :how-set :feedback-obj :final-feedback-obj :start-event))
	 (case slot
	  ((:known-as :final-function)
	   (initialize-inter-db-string inter agg slot))
	  (:start-where
	   (initialize-start-where inter agg :element-of "Aggregate of items" 
				   :in-box "Single item"))
	  (:feedback-obj
	   (if (g-value inter :feedback-obj)
	       (progn
		 (set-db-value agg :feedback-obj "Interim Feedback")
		 (set-db-string agg :feedback-obj 
				(name-for-schema (g-value inter :feedback-obj))))
	       (set-db-value agg :feedback-obj "None")))
	  (:final-feedback-obj
	   (if (g-value inter :final-feedback-obj)
	       (progn
		 (set-db-value agg :final-feedback-obj "Final Feedback")
		 (set-db-string agg :final-feedback-obj 
				(name-for-schema (g-value inter :final-feedback-obj))))
	       (set-db-value agg :final-feedback-obj "None")))
	  (:how-set
	   (cond ((formula-p (get-value inter :how-set))
		  (set-db-value agg :how-set "<formula>"))
		 (t
		  (set-db-value agg :how-set (g-value inter :how-set)))))
	  (:is-a
	   (if (eq (g-value inter :is-a) lapidary-button-interactor)
	       (set-db-value agg :inter-type "Button")
	       (set-db-value agg :inter-type "Menu")))))
    (s-value choice-interactor-win :inter inter)))

(defun init-move-grow-inter-menu (inter)
  (declare (special move-grow-inter-menu move-grow-inter-win))
  ;; call move-grow-inter-do-go the first time the text interactor menu is
  ;; requested
  (when (or (not (boundp 'move-grow-inter-win)) 
	    (null move-grow-inter-win))
    (move-grow-inter-do-go))
  (let ((agg move-grow-inter-menu))
    (dolist (slot '(:known-as :start-where :final-function
		    :min-length :min-width :min-height :grow-p :line-p
		    :feedback-obj :attach-point :obj-to-change :start-event
		    :grow-box-parms :move-box-parms))
	(case slot
	  ((:known-as :final-function)
	   (initialize-inter-db-string inter agg slot))
	  (:start-where
	   (initialize-start-where inter agg :in-box "Object to Press Over"
				   :element-of "One of This Aggregate"))
	  (:prototype-objs
	   (s-value (move-grow-obj-prototypes) :value nil)
	   (s-value (move-grow-obj-prototypes) :field-string nil)
	   (s-value (move-grow-feedback-prototypes) :value nil)
	   (s-value (move-grow-feedback-prototypes) :field-string nil))
	  (:line-p
	   (let ((line-p (get-value inter :line-p))
		 (line-p-agg (g-value move-grow-inter-menu :line-p)))
	     (cond ((formula-p line-p)
		    (s-value line-p-agg :value "<Formula>"))
		   (line-p
		    (s-value line-p-agg :value "Line"))
		   (t 
		    (s-value line-p-agg :value "Box")))))
	  (:grow-p
	   (let ((grow-p (get-value inter :grow-p))
		 (grow-p-agg (g-value move-grow-inter-menu :grow-p)))
	     (cond ((formula-p grow-p)
		    (s-value grow-p-agg :value "<Formula>"))
		   (grow-p
		    (s-value grow-p-agg :value "Grow"))
		   (t 
		    (s-value grow-p-agg :value "Move")))))
	  (:feedback-obj
	   (let ((feedback-obj (get-local-value inter :feedback-obj))) 
	     (cond ((formula-p feedback-obj)
		    (set-db-value agg :feedback-obj "<Formula>")
		    (set-db-string agg :feedback-obj nil))
		   (feedback-obj
		    (set-db-value agg :feedback-obj "Interim Feedback")
		    (set-db-string agg :feedback-obj 
				   (name-for-schema feedback-obj)))
		   (t
		    (set-db-value agg :feedback-obj "Change Original")
		    (set-db-string agg :feedback-obj nil)))))
	  (:grow-box-parms
	   (let ((grow-parms-agg (g-value agg :grow-parm :contents 
					  :change-size))
		 (grow-parms (get-value inter :grow-box-parms)))
	     (s-value grow-parms-agg :value
		      (cond ((formula-p grow-parms) "<Formula>")
			    ((eq grow-parms :width) "Change Width")
			    ((eq grow-parms :height) "Change Height")
			    (t "Change Width and Height")))))
	  (:move-box-parms
	   (let ((move-parms-agg (g-value agg :move-parm :contents 
					  :change-size))
		 (move-parms (get-value inter :move-box-parms)))
	     (s-value move-parms-agg :value
		      (cond ((formula-p move-parms) "<Formula>")
			    ((eq move-parms :left) "Change Left")
			    ((eq move-parms :top) "Change Top")
			    (t "Change Left and Top")))))
	  (:min-length
	   (let ((gadget (g-value agg :grow-parm :contents :min-length))
		 (value (if (g-value inter :min-length)
			    (prin1-to-string (g-value inter :min-length))
			    "")))
	     (s-value gadget :value value)
	     (s-value gadget :old-value value)))
	  (:min-width
	   (let ((gadget (g-value agg :grow-parm :contents :min-width))
		 (value (if (g-value inter :min-width)
			    (prin1-to-string (g-value inter :min-width))
			    "")))
	     (s-value gadget :value value)
	     (s-value gadget :old-value value)))
	  (:min-height
	   (let ((gadget (g-value agg :grow-parm :contents :min-height))
		 (value (if (g-value inter :min-height)
			    (prin1-to-string (g-value inter :min-height))
			    "")))
	     (s-value gadget :value value)
	     (s-value gadget :old-value value)))
	  (:attach-point
	   (let* ((attach-pt-agg (g-value agg :attach-point :contents))
		  (feedback-obj (g-value attach-pt-agg :feedback))
		  (attach-point (get-value inter :attach-point)))
	     (cond ((formula-p attach-point)
		    (s-value feedback-obj :obj-over 
			     (g-value attach-pt-agg :formula)))
		   ((eq attach-point :where-hit)
		    (s-value feedback-obj :obj-over
			     (g-value attach-pt-agg :where-hit)))
		   ((and (eq attach-point :center) (g-value inter :line-p))
		    (s-value feedback-obj :obj-over
			     (g-value attach-pt-agg :line-buttons :c)))
		   ((dolist (obj (g-value attach-pt-agg :box-buttons
				  :components))
			    (when (eq (g-value obj :attach-point) attach-point)
				  (s-value feedback-obj :obj-over obj)
				  (return obj))))
		   (t
		    (dolist (obj (g-value attach-pt-agg :line-buttons
				   :components))
			     (when (eq (g-value obj :attach-point) 
				       attach-point)
				   (s-value feedback-obj :obj-over obj)
				   (return obj)))))))
	  (:obj-to-change
	   (cond ((formula-p (get-value inter :obj-to-change))
		  (set-db-value agg :obj-to-change "<Formula>"))
		 ((g-value inter :obj-to-change)
		  (set-db-value agg :obj-to-change "Change this object"))
		 (t
		  (set-db-value agg :obj-to-change "Result of :start-where"))))))
    (s-value (g-value move-grow-inter-menu :window) :inter inter)))

(defun init-two-point-inter-menu (inter)
  (declare (special two-point-inter-win two-point-inter-menu))
  ;; call two-point-inter-do-go the first time the two point interactor menu is
  ;; requested
  (when (or (not (boundp 'two-point-inter-win)) 
	    (null two-point-inter-win))
    (two-point-inter-do-go))
  (let ((agg (g-value two-point-inter-win :aggregate)))
    (dolist (slot '(:known-as :start-where :final-function :feedback-obj
		    :line-p :min-width :min-height :min-length
		    :abort-if-too-small :flip-if-change-side))
	(case slot
	  ((:known-as :final-function)
	   (initialize-inter-db-string inter agg slot))
	  (:start-where
	   (initialize-start-where inter agg t "Start Anywhere in Window"
				   :in-box "Start in Box"))
	  (:line-p
	   (let ((line-p (get-value inter :line-p))
		 (line-p-agg (g-value two-point-inter-menu :line-p)))
	     (cond ((formula-p line-p)
		    (s-value line-p-agg :value "<Formula>"))
		   (line-p
		    (s-value line-p-agg :value "Create Line"))
		   (t 
		    (s-value line-p-agg :value "Create Non-Line")))))
	  (:feedback-obj
	   (if (g-value inter :feedback-obj)
	       (progn
		 (set-db-value agg :feedback-obj "Interim Feedback")
		 (set-db-string agg :feedback-obj 
				(name-for-schema (g-value inter :feedback-obj))))
	       (set-db-value agg :feedback-obj "None")))
	  
	  (:min-length
	   (let ((gadget (g-value agg :line-parameters :contents :min-length))
		 (value (if (g-value inter :min-length)
			    (prin1-to-string (g-value inter :min-length))
			    "")))
	     (s-value gadget :value value)
	     (s-value gadget :old-value value)))
	  (:min-width
	   (let ((gadget (g-value agg :non-line-parameters :contents :min-width))
		 (value (if (g-value inter :min-width)
			    (prin1-to-string (g-value inter :min-width))
			    "")))
	     (s-value gadget :value value)
	     (s-value gadget :old-value value)))
	  (:min-height
	   (let ((gadget (g-value agg :non-line-parameters :contents :min-height))
		 (value (if (g-value inter :min-height)
			    (prin1-to-string (g-value inter :min-height))
			    "")))
	     (s-value gadget :value value)
	     (s-value gadget :old-value value)))
	  (:flip-if-change-side
	   (if (g-value inter :flip-if-change-side)
	       (s-value (g-value agg :non-line-parameters
				 :contents :may-flip-over)
			:value
			"May Flip Over")))
	  (:abort-if-too-small
	   (let ((abort-agg (g-value two-point-inter-menu :abort-if-too-small)))
	     (if (g-value inter :abort-if-too-small)
		 (s-value abort-agg :value "Abort if Too Small")
	         (s-value abort-agg :value "or Increase to Min Size"))))))
    (s-value two-point-inter-win :inter inter)))


(defun init-angle-inter-menu (inter)
  (declare (special angle-inter-win angle-inter-menu))
  ;; call angle-inter-do-go the first time the text interactor menu is
  ;; requested
  (when (or (not (boundp 'angle-inter-win)) 
	    (null angle-inter-win))
    (angle-inter-do-go))
  (let ((agg (g-value angle-inter-win :aggregate)))
    (dolist (slot '(:known-as :start-where :final-function
		    :feedback-obj :attach-point :obj-to-change :start-event))
	(case slot
	  ((:known-as :final-function)
	   (initialize-inter-db-string inter agg slot))
	  (:start-where
	   (initialize-start-where inter agg :in-box "Object to Press Over"
				   t "Start Anywhere in Window"))
	  (:feedback-obj
	   (if (g-value inter :feedback-obj)
	       (progn
		 (set-db-value agg :feedback-obj "Interim Feedback")
		 (set-db-string agg :feedback-obj 
				(name-for-schema (g-value inter :feedback-obj))))
	       (set-db-value agg :feedback-obj "None")))
	  (:center-of-rotation
	   (let* ((rotation-agg (g-value agg :lapidary-rotation :contents))
		  (feedback-obj (g-value rotation-agg :feedback))
		  (attach-point (get-value inter :attach-point)))
	     (cond ((eq attach-point :formula)
		    (s-value feedback-obj :obj-over 
			     (g-value rotation-agg :formula)))
		   ((eq attach-point :pair)
		    (s-value (ANGLE-CENTER-OF-ROTATION) :x 
			     (prin1-to-string (car attach-point)))
		    (s-value (ANGLE-CENTER-OF-ROTATION) :y 
			     (prin1-to-string (second attach-point))))
		   ((dolist (obj (g-value rotation-agg :box-buttons
				  :components))
			    (when (eq (g-value obj :attach-point) attach-point)
				  (s-value feedback-obj :obj-over obj)
				  (s-value (Angle-Center-of-Rotation) :value
					   obj)
				  (return obj))))
		   (t
		    (dolist (obj (g-value rotation-agg :line-buttons
				   :components))
			     (when (eq (g-value obj :attach-point) 
				       attach-point)
				   (s-value feedback-obj :obj-over obj)
				   (s-value (Angle-Center-of-Rotation) :value
					   obj)
				   (return obj)))))))
	  (:obj-to-change
	   (if (get-value inter :obj-to-change)
	       (set-db-value agg :obj-to-change "<Formula>")
	       (set-db-value agg :obj-to-change "Result of :start-where")))
))
    (s-value angle-inter-win :inter inter)))

