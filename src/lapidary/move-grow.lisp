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
;;;  This file contains the code to either MOVE or GROW objects inside
;;;  the editor drawing window

(in-package "LAPIDARY")

(eval-when (eval load compile)
  (export '(Move-Grow-Do-Go Move-Grow-Do-Stop)))

;;; make sure that slots which are going to be changed by the move
;;; interactor do not have formulas
(defun check-move-slots (obj inter)
  (let ((attach-point (g-value inter :attach-point))
	constrained-slots)
    (if (is-a-line-p obj)
	(dolist (slot '(:x1 :y1 :x2 :y2))
		(when (formula-p (get-value obj slot))
		      (push slot constrained-slots)))
        (progn
	  (when (and (member attach-point '(:nw :ne :se :sw :center :e :w))
		     (formula-p (get-value obj :left)))
		(push :left constrained-slots))
	  (when (and (member attach-point '(:nw :ne :se :sw :center :s :n))
		     (formula-p (get-value obj :top)))
		(push :top constrained-slots))))
    (reverse constrained-slots)))

(define-method :lapidary-check-grow-slots opal:view-object (obj inter)
  (let ((attach-point (g-value inter :attach-point))
	constrained-slots)
    (when (member attach-point '(:nw :ne :se :sw :e :w))
	  (when (formula-p (get-value obj :left))
		(push :left constrained-slots))
	  (when (formula-p (get-value obj :width))
		(push :width constrained-slots)))
    (when (member attach-point '(:nw :ne :se :sw :n :s))
	  (when (formula-p (get-value obj :top))
		(push :top constrained-slots))
	  (when (formula-p (get-value obj :height))
		(push :height constrained-slots)))
    (reverse constrained-slots)))

#|
(define-method :lapidary-check-grow-slots opal:circle (obj inter)
  (let ((attach-point (g-value inter :attach-point))
	constrained-slots)
    (when (member attach-point '(:nw :ne :se :sw :e :w))
	  (when (formula-p (get-value obj :left))
		(push :left constrained-slots))
	  (when (formula-p (get-value obj :suggested-width))
		(push :width constrained-slots)))
    (when (member attach-point '(:nw :ne :se :sw :n :s))
	  (when (formula-p (get-value obj :top))
		(push :top constrained-slots))
	  (when (formula-p (get-value obj :suggested-height))
		(push :height constrained-slots)))
    (reverse constrained-slots)))
|#

(defun check-line-grow-slots (obj inter)
  (let ((attach-point (g-value inter :attach-point))
	constrained-slots)
    (if (eq attach-point 2)
	(progn
	  (when (formula-p (get-value obj :x2))
		(push :x2 constrained-slots))
	  (when (formula-p (get-value obj :y2))
		(push :y2 constrained-slots)))
        (progn
	  (when (formula-p (get-value obj :x1))
		(push :x1 constrained-slots))
	  (when (formula-p (get-value obj :y1))
		(push :y1 constrained-slots))))
    (reverse constrained-slots)))

(define-method :lapidary-check-grow-slots opal:line (obj inter)
  (check-line-grow-slots obj inter))

(define-method :lapidary-check-grow-slots 
  garnet-gadgets:arrow-line (obj inter)
  (check-line-grow-slots obj inter))

(define-method :lapidary-check-grow-slots 
  garnet-gadgets:double-arrow-line (obj inter)
  (check-line-grow-slots obj inter))

(define-method :lapidary-check-grow-slots opal:aggrelist (obj inter)
  (let ((attach-point (g-value inter :attach-point))
	constrained-slots)
    (when (and (member attach-point '(:nw :ne :se :sw :e :w))
	       (formula-p (get-value obj :left)))
	  (push :left constrained-slots))
    (when (and (member attach-point '(:nw :ne :se :sw :n :s))
	       (formula-p (get-value obj :top)))
	  (push :top constrained-slots))
    (reverse constrained-slots)))

;;; ==========================================================
;;; grow-obj inserts the appropriate formulas into the
;;; selected object so that it is resized. 
;;; ==========================================================
        
;;; standard grow function for non-line objects

(define-method :lapidary-grow opal:view-object (inter obj box)
  (let ((attach-point (g-value inter :attach-point)))
    ;; change width if any of the grow boxes on the corners or left
    ;; or right sides are chosen
    (when (member attach-point '(:nw :ne :se :sw :e :w))
	  (gg::cg-destroy-constraint obj :left)
	  (gg::cg-destroy-constraint obj :width)
	  (s-value obj :left (first box))
	  (s-value obj :width (third box)))
    ;; change height if any of the grow boxes on the corners or top
    ;; or bottom sides are chosen
    (when (member attach-point '(:nw :ne :se :sw :n :s))
	   (gg::cg-destroy-constraint obj :top)
	   (gg::cg-destroy-constraint obj :height)
	   (s-value obj :top (second box))
	   (s-value obj :height (fourth box)))))

(defun grow-line (inter obj points)
  (let ((attach-point (g-value inter :attach-point)))
    (case attach-point
	  ((1 :center)
	   (gg::cg-destroy-constraint obj :x1)
	   (gg::cg-destroy-constraint obj :y1)
	   (s-value obj :x1 (first points))
	   (s-value obj :y1 (second points)))
	  (2
	   (gg::cg-destroy-constraint obj :x2)
	   (gg::cg-destroy-constraint obj :y2)
	   (s-value obj :x2 (third points))
	   (s-value obj :y2 (fourth points))))))

(define-method :lapidary-grow opal:line (inter obj points)
  (grow-line inter obj points))

(define-method :lapidary-grow 
  garnet-gadgets:arrow-line (inter obj points)
  (grow-line inter obj points))

(define-method :lapidary-grow 
  garnet-gadgets:double-arrow-line (inter obj points)
  (grow-line inter obj points))

;;; aggrelists have to be handled as a special case. When their
;;; width or height is changed, new items must be added to the
;;; aggrelist and the constraints on the width and height are
;;; not changed (we presume that they are the standard formulas
;;; for computing an aggregate's width and height, and if they're
;;; not, we presume the designer has used formulas that will correctly
;;; compute the width and height, even with the changed number of items)

(define-method :lapidary-grow opal:aggrelist (inter obj box)
(let ((attach-point (g-value inter :attach-point)))
    ;; change left if any of the grow boxes on the corners or left
    ;; or right sides are chosen
    (when (member attach-point '(:nw :ne :se :sw :e :w))
	  (gg::cg-destroy-constraint obj :left)
	  (s-value obj :left (first box)))
    ;; change top if any of the grow boxes on the corners or top
    ;; or bottom sides are chosen
    (when (member attach-point '(:nw :ne :se :sw :n :s))
	   (gg::cg-destroy-constraint obj :top)
	   (s-value obj :height (fourth box)))

    ;; change the number of items in a list
    (s-value obj :items (g-value inter :feedback-obj :items))
    (s-value obj :rank-margin (g-value inter :feedback-obj :rank-margin))
    (opal:notice-items-changed obj)
    (s-value (g-value inter :feedback-obj) :visible nil)))

#|
;;; circles must be handled as a special case since their width and
;;; height must always be the same. if a corner is moved, select the
;;; minimum of the changed height and width.

(define-method :lapidary-grow opal:circle (inter obj box)
  (let* ((attach-point (g-value inter :attach-point))
	 (diameter (case attach-point
			 ((:nw :sw :ne :se) (min (third box) (fourth box)))
			 ((:e :w) (third box))
			 ((:n :s) (fourth box)))))
    ;; it's ok to try to destroy a constraint twice
    (when (member attach-point '(:nw :sw :ne :se :e :w))
	  (gg::cg-destroy-constraint obj :suggested-width)
	  (s-value obj :suggested-width (third box)))

    (when (member attach-point '(:nw :sw :ne :se :n :s))
	  (gg::cg-destroy-constraint obj :suggested-height)
	  (s-value obj :suggested-height (fourth box)))
    
    ;; set the :diameter slot
    (s-value obj :diameter diameter)))
|#
;;; ==========================================================
;;; move-obj inserts the appropriate formulas into the
;;; selected object so that it is moved, and updates
;;; the constraint menu info to reflect this new state of
;;; affairs
;;; ==========================================================

(defun move-obj (obj attach-point)
  (if (is-a-line-p obj)
      ;; move a line
      (let ((points (g-value obj :points)))
	(gg::cg-destroy-constraint obj :x1)
	(gg::cg-destroy-constraint obj :y1)
	(gg::cg-destroy-constraint obj :x2)
	(gg::cg-destroy-constraint obj :y2)
	(s-value obj :x1 (first points))
	(s-value obj :y1 (second points))
	(s-value obj :x2 (third points))
	(s-value obj :y2 (fourth points)))
      ;; move a non-line
      (let ((box (g-value obj :box)))
	(when (member attach-point '(:nw :ne :se :sw :center :e :w))
	      (gg::cg-destroy-constraint obj :left)
	      (s-value obj :left (first box)))
	(when (member attach-point '(:nw :ne :se :sw :center :s :n))
	      (gg::cg-destroy-constraint obj :top)
	      (s-value obj :top (second box))))))


(defun Move-Grow-Do-Go ()
  (create-instance 'move-inter multi-win-interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:active (o-formula (gv editor-menu :build-p)))
   (:continuous T)
   (:waiting-priority inter:high-priority-level)
   (:start-where (o-formula (list :list-leaf-element-of
				  *selection-info* :feedback)))
   (:outside NIL) ; goes back to original position if go outside
   (:running-where t)
   (:feedback-obj 
    (o-formula (if (gvl :line-p)
		   (gvl :current-window :move-grow-line-feedback)
		   (gvl :current-window :move-box-feedback))))
;; the next field causes the object to be changed to be the object that the
;; feedback boxes are defined over, rather than the feedback object itself.
   (:obj-to-change (o-formula (gvl :first-obj-over :parent :obj-over)))
   (:line-p (o-formula (is-a-line-p (gvl :obj-to-change))))
   (:attach-point (o-formula (cond ((is-a-p (gvl :first-obj-over :parent) 
					    undersized-feedback-obj)
				    (if (gvl :line-p) :center :nw))
				   (t
				    (gvl :first-obj-over :where-attach)))))
   (:grow-p nil)
   (:start-event *move-button*)
   (:start-action
    #'(lambda (interactor objbeingchanged newsize)
	(let (constrained-slots)
	(cond ((not (eq (g-value objbeingchanged :parent)
			(g-value objbeingchanged :window :editor-agg)))
	       (lapidary-error 
		"This object is part of an aggregate and thus cannot be moved")
	       (inter:abort-interactor interactor))
	      ((setf constrained-slots
		     (check-move-slots objbeingchanged interactor))
	       (lapidary-error 
		(format nil "To move this object you must first unconstrain
the ~S slots" constrained-slots))
	       (inter:abort-interactor interactor))
	      (t
	       (kr::call-prototype-method interactor objbeingchanged newsize)
	       (s-value (g-value interactor :feedback-obj) :where-attach
			(g-value interactor :attach-point))
	       (s-value (g-value interactor :first-obj-over) :visible NIL))))))
   (:abort-action
    #'(lambda (interactor objbeingchanged)
	(let ((first-obj-over (g-value interactor :first-obj-over)))
	  (when first-obj-over (s-value first-obj-over :visible T)))
	(when objbeingchanged
	  (kr::call-prototype-method interactor objbeingchanged))))
   (:stop-action
    #'(lambda (interactor objbeingchanged newsize)
	(let ((obj-to-change (g-value interactor :obj-to-change)))
	(s-value (g-value interactor :first-obj-over) :visible T)
	(kr::call-prototype-method interactor objbeingchanged newsize)
	;; save slot values for undo
	(reset-undo)
	(undo-save objbeingchanged :left)
	(undo-save objbeingchanged :top)
	;; if the object belongs in another window, move it to its new window
	(when (not (eq (g-value interactor :current-window)
		       (g-value obj-to-change :window)))
	  (let ((new-window (g-value interactor :current-window))
		(old-parent (g-value obj-to-change :parent))
		(p-feedback (g-value obj-to-change :p-feedback-obj))
		(s-feedback (g-value obj-to-change :s-feedback-obj)))
	      (opal:remove-component old-parent obj-to-change)
	      (opal:add-component (g-value new-window :editor-agg)
				  obj-to-change)
	      (when p-feedback
		    (opal:remove-component (g-value p-feedback :parent)
					   p-feedback)
		    (opal:add-component (g-value new-window :feedback-agg)
					p-feedback))
	      (when s-feedback
		    (opal:remove-component (g-value s-feedback :parent)
					   s-feedback)
		    (opal:add-component (g-value new-window :feedback-agg)
					s-feedback))))
	;; move the object
	(move-obj obj-to-change (g-value interactor :attach-point))
	))))

  (create-instance 
   'grow-inter inter:Move-Grow-Interactor
   (:window (o-formula (gv *selection-info* :window)))
   (:active (formula `(gv ',editor-menu :build-p)))
   (:continuous T)
   (:waiting-priority inter:high-priority-level)
   (:start-where 
    (o-formula (list :list-leaf-element-of
		     *selection-info* :feedback)))
   (:outside NIL) ; goes back to original position if go outside
   (:running-where t)
   (:feedback-obj 
    (o-formula (cond ((gvl :line-p)
		      (gvl :current-window :move-grow-line-feedback))
		     ((is-a-p (gvl :obj-to-change) opal:aggrelist)
		      (gvl :current-window :aggrelist-feedback))
		     (t (gvl :current-window :grow-box-feedback)))))
;; the next field causes the object to be changed to be the object that the
;; feedback boxes are defined over, rather than the feedback object itself. 
   (:obj-to-change (o-formula (gvl :first-obj-over :parent :obj-over)))
   (:attach-point (o-formula (cond ((is-a-p (gvl :first-obj-over :parent) 
					    undersized-feedback-obj)
				    (if (gvl :line-p) 1 :nw))
				   (t
				    (if (eq (gvl :first-obj-over :where-attach)
					    :center)
					1
				        (gvl :first-obj-over :where-attach))))))
    (:grow-p t)
    (:line-p (o-formula (is-a-line-p (gvl :obj-to-change))))
    (:min-width (o-formula (if (is-a-p (gvl :feedback-obj) opal:aggrelist)
			       (gvl :feedback-obj :item-prototype :width)
			       0)))
    (:min-height (o-formula (if (is-a-p (gvl :feedback-obj) opal:aggrelist)
			       (gvl :feedback-obj :item-prototype :height)
			       0)))
    (:start-event *grow-button*)
    (:start-action
     #'(lambda (interactor objbeingchanged newsize)
	 (let (constrained-slots)
	 (cond ((not (eq (g-value objbeingchanged :parent)
			 (g-value objbeingchanged :window :editor-agg)))
		(lapidary-error 
		"This object is part of an aggregate and thus cannot be grown")
		(inter:abort-interactor interactor))
	       ((is-a-p objbeingchanged opal:text)
		(lapidary-error
		 "You cannot change the size of a text object")
		(inter:abort-interactor interactor))
	      ((setf constrained-slots
		     (kr-send objbeingchanged :lapidary-check-grow-slots
			      objbeingchanged interactor))
	       (lapidary-error 
		(format nil "To resize this object you must first unconstrain
the ~S slots" constrained-slots))
	       (inter:abort-interactor interactor))
	       (t
		(if (is-a-p (g-value interactor :feedback-obj) opal:aggrelist)
		    (initialize-list-feedback interactor objbeingchanged
					      newsize)
		    (kr::call-prototype-method interactor objbeingchanged 
					       newsize))
		(s-value (g-value interactor :feedback-obj) :where-attach
			 (g-value interactor :attach-point))
		(s-value (g-value interactor :first-obj-over) :visible NIL))))))
    (:running-action
     #'(lambda (interactor objbeingchanged newsize)
	 (if (is-a-p (g-value interactor :feedback-obj) opal:aggrelist)
	     (calculate-howmany interactor objbeingchanged newsize)
	     (kr::call-prototype-method interactor objbeingchanged newsize))))
    (:abort-action
     #'(lambda (interactor objbeingchanged)
	 (let ((first-obj-over (g-value interactor :first-obj-over)))
	   (when first-obj-over (s-value first-obj-over :visible T)))
	 (when objbeingchanged
	   (kr::call-prototype-method interactor objbeingchanged)
	   (when (is-a-p objbeingchanged opal:aggrelist)
		 (s-value (g-value interactor :feedback-obj) :visible nil)))))
    (:stop-action
     #'(lambda (interactor objbeingchanged newsize)
	(let ((obj-to-change (g-value interactor :obj-to-change)))
	  (s-value (g-value interactor :first-obj-over) :visible T)
	  (kr::call-prototype-method interactor objbeingchanged newsize)
	  ;; save slot values for undo
	  (reset-undo)
	  (undo-save objbeingchanged :left)
	  (undo-save objbeingchanged :top)
	  (undo-save objbeingchanged :width)
	  (undo-save objbeingchanged :height)
	  ;; grow the object
	  (kr-send obj-to-change :lapidary-grow interactor obj-to-change 
		   newsize)
	 )))
   ))


(defun Move-Grow-Do-Stop ()
  (when (boundp 'grow-inter) (opal:destroy grow-inter))
  (when (boundp 'move-inter) (opal:destroy move-inter))
  )

