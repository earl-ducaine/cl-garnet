;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; CHANGE LOG
;;;
;;; 08/10/93 bvz -- added interactors so that copying and instancing can
;;;                  be done by dragging on a selection handle
;;; 08/25/92 amickish - Commented out declaration of obj-name in make-instance-
;;;                     or-copy because its use is commented out
;;; gadgetcopy.lisp -- make copies and instances of gadgets


(in-package "LAPIDARY")

(defun copy-instance-inter-do-go ()

  ;; create a copy interactor
  (create-instance 'copy-interactor multi-win-interactor
    (:window (o-formula (gv *selection-info* :window)))
    (:start-action 
     #'(lambda (inter obj-to-change pts)
	 (kr::call-prototype-method inter obj-to-change pts)
	 (s-value (g-value inter :feedback-obj) :where-attach
		  (cond ((is-a-p (g-value inter :first-obj-over :parent) 
				 undersized-feedback-obj)
			 (if (g-value inter :line-p) :center :nw))
			(t
			 (g-value inter :first-obj-over :where-attach))))))
(:left (o-formula (case (gvl :where-attach) 
		      ((list :n :s) 
		       (gvl :obj-over :left))
		      (t 
		       (first (gvl :box))))))
  (:top (o-formula (case (gvl :where-attach)
		     ((list :e :w)
		      (gvl :obj-over :top))
		     (t
		      (second (gvl :box))))))
    (:final-function #'(lambda (inter obj-to-change pts)
			 ;; make sure pts list that is passed to the
			 ;; final function is filtered so that if the
			 ;; object was supposed to move in only one
			 ;; direction, it really does only move in one
			 ;; direction
			 (when (and (not (g-value inter :line-p))
				    (not (is-a-p (g-value inter :first-obj-over :parent)
						 undersized-feedback-obj)))
			       (let ((attach-pt (g-value inter :first-obj-over
							 :where-attach)))
				 (if (or (eq attach-pt :n)
					 (eq attach-pt :s))
				     (setf (first pts)
					   (g-value obj-to-change :left)))
				 (if (or (eq attach-pt :e)
					 (eq attach-pt :w))
				     (setf (second pts)
					   (g-value obj-to-change :top)))))
			 (make-instance-or-copy nil obj-to-change 
						(g-value inter :current-window
							 :editor-agg)
						pts)))
    (:line-p (o-formula (is-a-line-p (gvl :obj-to-change))))
    (:feedback-obj 
     (o-formula (if (gvl :line-p)
		    (gvl :current-window :copy-instance-line-feedback)
		    (gvl :current-window :copy-instance-rect-feedback))))
    (:obj-to-change (o-formula (gvl :first-obj-over :parent :obj-over)))
    (:start-where 
     (o-formula (list :list-leaf-element-of
		      *selection-info* :feedback)))
    (:start-event *copy-button*))

  ;; create an instance interactor
  (create-instance 'instance-interactor multi-win-interactor
     (:window (o-formula (gv *selection-info* :window)))
     (:start-action 
      #'(lambda (inter obj-to-change pts)
	  (kr::call-prototype-method inter obj-to-change pts)
	  (s-value (g-value inter :feedback-obj) :where-attach
		   (cond ((is-a-p (g-value inter :first-obj-over :parent) 
				  undersized-feedback-obj)
			  (if (g-value inter :line-p) :center :nw))
			 (t
			  (g-value inter :first-obj-over :where-attach))))))
     (:final-function #'(lambda (inter obj-to-change pts)
;; make sure pts list that is passed to the
			 ;; final function is filtered so that if the
			 ;; object was supposed to move in only one
			 ;; direction, it really does only move in one
			 ;; direction
			 (when (and (not (g-value inter :line-p))
				    (not (is-a-p (g-value inter :first-obj-over :parent)
						 undersized-feedback-obj)))
			       (let ((attach-pt (g-value inter :first-obj-over
							 :where-attach)))
				 (if (or (eq attach-pt :n)
					 (eq attach-pt :s))
				     (setf (first pts)
					   (g-value obj-to-change :left)))
				 (if (or (eq attach-pt :e)
					 (eq attach-pt :w))
				     (setf (second pts)
					   (g-value obj-to-change :top)))))
			  (make-instance-or-copy t obj-to-change 
						 (g-value inter :current-window
							  :editor-agg)
						 pts)))

     (:line-p (o-formula (is-a-line-p (gvl :obj-to-change))))
     (:feedback-obj 
      (o-formula (if (gvl :line-p)
		     (gvl :current-window :copy-instance-line-feedback)
		     (gvl :current-window :copy-instance-rect-feedback))))
     (:obj-to-change (o-formula (gvl :first-obj-over :parent :obj-over)))
     (:start-where
      (o-formula (list :list-leaf-element-of
		       *selection-info* :feedback)))
     (:start-event *instance-button*)))

(defun copy-instance-inter-do-stop ()
  (when (boundp 'instance-interactor) (opal:destroy instance-interactor))
  (when (boundp 'copy-interactor) (opal:destroy copy-interactor)))

(defun make-instance-or-copy (instance-p &optional (obj nil) (agg nil)
					 (pts nil)) 
  (let* ((objs (if obj (list obj)
		       (g-value *selection-info* :selected)))
	 editor-agg result
;	 obj-name
	 )
    (cond ((null objs) (lapidary-error "must select an object first") nil)
	  (t
	   ; this prevents create-instance from copying feedback links
	   (primary-deselect-objects :none)
	   (secondary-deselect-objects :none)

	   (dolist (obj objs)
	     ;; remove corrupted slots from the object
	     (fix-up-obj obj)

	     ;; ask the user to name the new instance
#|
	     (setf obj-name 
		   (keyword-from-string
		    (lapidary-prompt-for-input "please enter object name: ")))
|#

	     (with-constants-disabled
	      (if instance-p
		 ; create the instance
		 (setf result (create-instance nil obj))
	         ; create a copy
	         (setf result (opal:copy-gadget obj nil))))

             ;; generate a name for the new object
             (name-lapidary-obj result)

	     ;;; put the instance in the correct window
	     (setf editor-agg (or agg (g-value obj :window :editor-agg)))
	     (opal:add-component editor-agg result)

	     ;;; destroy any constraints on the position slots of the
	     ;;; new object
	     (dolist (slot (if (is-a-line-p result)
			       '(:x1 :y1 :x2 :y2)
			       '(:left :top)))
	       (gg:cg-destroy-constraint result slot))

	     ;;; if a position is passed in for the object, position it at
	     ;;; the prescribed position; otherwise, move the instance 20 
	     ;;; pixels to the right and below the object it is based on
	     (if pts
		 (if (is-a-line-p result)
		     (progn
		       (s-value result :x1 (first pts))
		       (s-value result :y1 (second pts))
		       (s-value result :x2 (third pts))
		       (s-value result :y2 (fourth pts)))
		     (opal:set-position result (first pts) (second pts)))
	         (if (is-a-line-p result)
		     (progn
		       (s-value result :x1 (+ 20 (g-value obj :x1)))
		       (s-value result :y1 (+ 20 (g-value obj :y1)))
		       (s-value result :x2 (+ 20 (g-value obj :x2)))
		       (s-value result :y2 (+ 20 (g-value obj :y2))))
		     (opal:set-position result (+ 20 (g-value obj :left))
					       (+ 20 (g-value obj :top)))))
	     
	     ;;; Select the instance
	     (primary-select result))))))




