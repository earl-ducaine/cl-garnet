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
;;; This file contains the functions that implement the line constraint
;;; menu functionality.
;;;
;;; CHANGE LOG
;;;
;;; 07/14/93 amickish - Removed gadget from list of ignored variables in
;;;                     LINE-UNCONSTRAIN-FN
;;; 08/24/92 amickish - Removed gadget from list of ignored variables in
;;;                     LINE-CUSTOM-FN

(in-package "GARNET-GADGETS")

(defun ATTACH-LINE-CONSTRAINT (interactor obj)
  (declare (ignore interactor obj))
  (declare (special *constraint-gadget*))
  (let ((p (g-value *constraint-gadget* :obj-to-constrain))
	(s (g-value *constraint-gadget* :obj-to-reference))
	(p-where (g-value LINE-CON-PRIM-SEL-AGG :where-attach))
	(s-where (g-value LINE-CON-SEC-SEL-AGG :where-attach)))
    (when (and p s p-where s-where)
      (if (is-a-line-p p)
	  (if (is-a-line-p s)
	      (attach-line-to-line)
	      (attach-line-to-box))
	  (if (is-a-line-p s)
	      (attach-box-to-line)
	      (attach-box-to-box))))))


(defun ATTACH-BOX-TO-BOX ()
  (constraint-gadget-error
   "Unimplemented: cannot constrain a non-line to a non-line."))

(defun ATTACH-LINE-TO-LINE ()
  (let* ((where-attach-line-1 (g-value LINE-CON-PRIM-SEL-AGG :where-attach))
	 (where-attach-line-2 (g-value LINE-CON-SEC-SEL-AGG :where-attach)))

      (case where-attach-line-1
	(0 (attach-constraint *line-constraint-menu* :x1 :x1-over :x1-offset
			      (aref *x1-to-line* where-attach-line-2))
	   (attach-constraint *line-constraint-menu* :y1 :y1-over :y1-offset
			      (aref *y1-to-line* where-attach-line-2)))
	(1 (attach-constraint *line-constraint-menu* :x2 :x2-over :x2-offset
			      (aref *x2-to-line* where-attach-line-2))
	   (attach-constraint *line-constraint-menu* :y2 :y2-over :y2-offset
			      (aref *y2-to-line* where-attach-line-2))))))

;; The box is the primary selection, and will be set with constraints to the
;; line.
;;
(defun ATTACH-BOX-TO-LINE ()
  (declare (special *constraint-gadget* *line-constraint-menu*))
  (let* ((where-attach-box
	  (g-value LINE-CON-PRIM-SEL-AGG :where-attach))
	 (where-attach-line 
	  (g-value LINE-CON-SEC-SEL-AGG :where-attach))
	 (box (g-value *constraint-gadget* :obj-to-constrain))
	 (constraint-vectors 
	  (cond ((is-a-p box opal:circle) 
		 (aref *box-to-line* 0 where-attach-line))
		((is-a-p box opal:roundtangle)
		 (aref *box-to-line* 1 where-attach-line))
		(t 
		 (aref *box-to-line* 2 where-attach-line))))
	 (left-vector (car constraint-vectors))
	 (top-vector (cdr constraint-vectors)))
    (attach-constraint *line-constraint-menu* :left :left-over :left-offset
		       (nth where-attach-box left-vector))
    (attach-constraint *line-constraint-menu* :top :top-over :top-offset
		       (nth where-attach-box top-vector))))

;;;================================================================
;;;
;;; place a constraint on the endpoint of a line. The pair (x1,y1)
;;; refer to the leftmost point and the pair (x2,y2) refer to the
;;; rightmost point.
;;;
;;;================================================================

(defun ATTACH-LINE-TO-BOX ()
  (declare (special *line-constraint-menu* *constraint-gadget*))
  (let* ((where-attach-box
	  (g-value LINE-CON-SEC-SEL-AGG :where-attach))
	 (where-attach-line 
	  (g-value LINE-CON-PRIM-SEL-AGG :where-attach))
	 (box (g-value *constraint-gadget* :obj-to-reference))
	 (constraint-vectors 
	  (cond ((is-a-p box opal:circle) 
		 (aref *line-to-box* 0 where-attach-line))
		((is-a-p box opal:roundtangle)
		 (aref *line-to-box* 1 where-attach-line))
		(t 
		 (aref *line-to-box* 2 where-attach-line))))
	 (left-vector (car constraint-vectors))
	 (top-vector (cdr constraint-vectors)))

      (case where-attach-line
	(0 (attach-constraint *line-constraint-menu* :x1 :x1-over :x1-offset
			      (nth where-attach-box left-vector))
	   (attach-constraint *line-constraint-menu* :y1 :y1-over :y1-offset
			      (nth where-attach-box top-vector)))
	(1 (attach-constraint *line-constraint-menu* :x2 :x2-over :x2-offset
			      (nth where-attach-box left-vector))
	   (attach-constraint *line-constraint-menu* :y2 :y2-over :y2-offset
			      (nth where-attach-box top-vector))))))
    
;; store an integer position in one of a line's position slots
(defun set-position-slot (gadget value)
  (declare (special *constraint-gadget*))
  (when (valid-integer-p gadget value)
	(let ((obj (g-value *constraint-gadget* :obj-to-constrain))
	      (slot (g-value gadget :slot)))
	  (cg-destroy-constraint obj slot)
	  (s-value obj slot (read-from-string value)))))

(defun set-x-offset (gadget value)
  (declare (special *line-constraint-menu* *constraint-gadget*))

  ;; first determine if the offset is valid
  (when (not (valid-integer-p gadget value))
	(return-from set-x-offset))
    
  (let ((x-offset (read-from-string value)))

    ;; store the offset in the line constraint menu
    (s-value *line-constraint-menu* :x1-offset x-offset)
    (s-value *line-constraint-menu* :x2-offset x-offset)
    (s-value *line-constraint-menu* :left-offset x-offset)

    ;; if the offset should be placed in the appropriate offset slot
    ;; of the primary selection, do so
    (if (g-value LINE-CON-PRIM-SEL-AGG :active)
	(let ((obj (g-value *constraint-gadget* :obj-to-constrain)))
	  (if (is-a-line-p obj)
	      (let ((sel (g-value LINE-CON-PRIM-SEL-AGG :where-attach)))
		(case sel
		      (0 (when (set-offset-p :x1) 
			       (s-value obj :x1-offset x-offset)))
		      (1 (when (set-offset-p :x2)
			       (s-value obj :x2-offset x-offset)))
		      ;; if an endpoint is not selected, determine if
		      ;; either endpoint is constrained
		      (t
		       (cond ((set-offset-p :x1)
			      (s-value obj :x1-offset x-offset))
			     ((set-offset-p :x2)
			      (s-value obj :x2-offset x-offset))))))
	      (when (set-offset-p :left)
		    (s-value obj :left-offset x-offset)))))))

(defun set-y-offset (gadget value)
  (declare (special *line-constraint-menu* *constraint-gadget*))

  ;; first determine if the offset is valid
  (when (not (valid-integer-p gadget value))
	(return-from set-y-offset))

  (let ((y-offset (read-from-string value)))

    ;; store the offset in the line constraint menu
    (s-value *line-constraint-menu* :y1-offset y-offset)
    (s-value *line-constraint-menu* :y2-offset y-offset)
    (s-value *line-constraint-menu* :top-offset y-offset)
    
    ;; if the offset should be placed in the appropriate offset slot
    ;; of the primary selection, do so
    (if (g-value LINE-CON-PRIM-SEL-AGG :active)
	(let ((obj (g-value *constraint-gadget* :obj-to-constrain)))
	  (if (is-a-line-p obj)
	      (let ((sel (g-value LINE-CON-PRIM-SEL-AGG :where-attach)))
		(case sel
		      (0 (when (set-offset-p :y1) 
			       (s-value obj :y1-offset y-offset)))
		      (1 (when (set-offset-p :y2)
			       (s-value obj :y2-offset y-offset)))
		      ;; if an endpoint is not selected, determine if
		      ;; either endpoint is constrained
		      (t
		       (cond ((set-offset-p :y1)
			      (s-value obj :y1-offset y-offset))
			     ((set-offset-p :y2)
			      (s-value obj :y2-offset y-offset))))))
	      (when (set-offset-p :top)
		    (s-value obj :top-offset y-offset)))))))

;; when either the unconstrain or customize buttons are hit, call this
;; function to clear the line buttons

(defun deselect-line-buttons (&optional (only-s-selection-p nil))
  (declare (special LINE-CON-PRIM-SEL-AGG LINE-CON-SEC-SEL-AGG))
  (when (not only-s-selection-p)
	(deselect-constraint-button
	 (g-value LINE-CON-PRIM-SEL-AGG :line :buttons))
	(deselect-constraint-button
	 (g-value LINE-CON-PRIM-SEL-AGG :box :buttons)))
  (deselect-constraint-button
   (g-value LINE-CON-SEC-SEL-AGG :line :buttons))
  (deselect-constraint-button
   (g-value LINE-CON-SEC-SEL-AGG :box :buttons)))

(defun LINE-CUSTOM-FN (gadget string)
  (declare (ignore string))
  (declare (special *constraint-gadget-query-window*))
  (deselect-line-buttons)
  (multiple-value-bind (left top)
    (opal:convert-coordinates (g-value gadget :window)
			      (g-value gadget :left)
			      (opal:bottom gadget)
			      nil)
    (c32 nil nil :left left :top top)))

(defun LINE-UNCONSTRAIN-FN (gadget string)
  (declare (ignore string))
  (declare (special *constraint-gadget*))
  (let ((obj (g-value *constraint-gadget* :obj-to-constrain))
	(reference-obj (g-value *constraint-gadget* :obj-to-reference)))
    (when obj
	  (if (not (is-a-line-p obj))
	      (progn
		(deselect-line-buttons)
		(cg-destroy-constraint obj :left)
		(cg-destroy-constraint obj :top))
	      (let ((sel (g-value LINE-CON-PRIM-SEL-AGG :where-attach)))
		;; if the user has selected an endpoint to unconstrain, or
		;; if it is possible to determine the endpoint to unconstrain
		;; without the user's intervention, unconstrain the endpoint.
		;; if the user has not selected an endpoint to unconstrain
		;; and both endpoints are constrained, ask the user to select
		;; an endpoint to unconstrain
		(when (null sel)
		      (cond (reference-obj
			     (cond ((and (formula-p (get-value obj :x1))
					 (depends-on-p obj reference-obj :x1)
					 (formula-p (get-value obj :x2))
					 (depends-on-p obj reference-obj :x2))
				    (constraint-gadget-error "Both endpoints are constrained. Please select one of
the two endpoints of the line in the primary
selection box and press unconstrain again")
				    (s-value gadget :value nil)
				    (return-from line-unconstrain-fn))
				   ((and (formula-p (get-value obj :x1))
					 (depends-on-p obj reference-obj :x1))
				    (setf sel 0))
				   ((and (formula-p (get-value obj :x2))
					 (depends-on-p obj reference-obj :x2))
				    (setf sel 1))
				   (t nil)))
			    ;; no reference object
			    (t
			     (cond ((and (formula-p (get-value obj :x1))
					 (formula-p (get-value obj :x2)))
				    (constraint-gadget-error "Both endpoints are constrained. Please select one of
the two endpoints of the line in the primary
selection box and press unconstrain again")
				    (s-value gadget :value nil)
				    (return-from line-unconstrain-fn))
				   ((formula-p (get-value obj :x1))
				    (setf sel 0))
				   ((formula-p (get-value obj :x2))
				    (setf sel 1))
				   (t nil)))))
	        (when sel
		  (deselect-line-buttons)
		  (case sel
			(0 (cg-destroy-constraint obj :x1)
			   (cg-destroy-constraint obj :y1))
			(1 (cg-destroy-constraint obj :x2)
			   (cg-destroy-constraint obj :y2)))
			))))))

