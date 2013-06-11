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
;;; 08/24/92 amickish - Removed gadget from list of ignored variable in
;;;                     LINE-CUSTOM-FN

(in-package "LAPIDARY")

(defun ATTACH-LINE-CONSTRAINT (interactor obj)
  (declare (ignore interactor obj))
  (let ((p (car (g-value *SELECTION-INFO* :p-selected)))
	(s (car (g-value *SELECTION-INFO* :s-selected)))
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
  (format t "Unimplemented: cannot constrain a non-line to a non-line.~%"))

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
  (let* ((where-attach-box
	  (g-value LINE-CON-PRIM-SEL-AGG :where-attach))
	 (where-attach-line 
	  (g-value LINE-CON-SEC-SEL-AGG :where-attach))
	 (box (car (g-value *SELECTION-INFO* :p-selected)))
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
  (let* ((where-attach-box
	  (g-value LINE-CON-SEC-SEL-AGG :where-attach))
	 (where-attach-line 
	  (g-value LINE-CON-PRIM-SEL-AGG :where-attach))
	 (box (car (g-value *SELECTION-INFO* :s-selected)))
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

;; if the value is an integer, store it in :old-value of gadget and return
;; the value; otherwise restore the :value slot of gadget to :old-value 
;; and return nil
(defun valid-integer-p (gadget value)
  (let ((number (when (not (string= value "")) (read-from-string value))))
    (if (not (integerp number))
	(progn
	  (s-value gadget :value (g-value gadget :old-value))
	  (lapidary-error "the value must be an integer")
	  nil)
	(s-value gadget :old-value value))))
    
;; store an integer position in one of a line's position slots
(defun set-position-slot (gadget value)
  (when (valid-integer-p gadget value)
	(let ((obj (car (g-value *selection-info* :selected)))
	      (slot (g-value gadget :slot)))
	  (destroy-constraint obj slot)
	  (s-value obj slot (read-from-string value)))))

(defun set-x-offset (gadget value)

  ;; first determine if the offset is valid
  (let ((x-offset (read-from-string value)))
    (when (not (valid-integer-p gadget value))
	  (return-from set-x-offset))
    
    ;; store the offset in the line constraint menu
    (s-value *line-constraint-menu* :x1-offset x-offset)
    (s-value *line-constraint-menu* :x2-offset x-offset)
    (s-value *line-constraint-menu* :left-offset x-offset)

    ;; if the offset should be placed in the appropriate offset slot
    ;; of the primary selection, do so
    (if (g-value LINE-CON-PRIM-SEL-AGG :active)
	(let ((obj (car (g-value *selection-info* :p-selected))))
	  (if (is-a-line-p obj)
	      (let ((sel (g-value LINE-CON-PRIM-SEL-AGG :where-attach)))
		(case sel
		      (0 (when (set-offset-p :x1 :x1-over) 
			       (s-value obj :x1-offset x-offset)))
		      (1 (when (set-offset-p :x2 :x2-over)
			       (s-value obj :x2-offset x-offset)))))
	      (when (set-offset-p :left :left-over)
		    (s-value obj :left-offset x-offset)))))))

(defun set-y-offset (gadget value)

  ;; first determine if the offset is valid
  (let ((y-offset (read-from-string value)))
    (when (not (valid-integer-p gadget value))
	  (return-from set-y-offset))

    ;; store the offset in the line constraint menu
    (s-value *line-constraint-menu* :y1-offset y-offset)
    (s-value *line-constraint-menu* :y2-offset y-offset)
    (s-value *line-constraint-menu* :top-offset y-offset)
    
    ;; if the offset should be placed in the appropriate offset slot
    ;; of the primary selection, do so
    (if (g-value LINE-CON-PRIM-SEL-AGG :active)
	(let ((obj (car (g-value *selection-info* :p-selected))))
	  (if (is-a-line-p obj)
	      (let ((sel (g-value LINE-CON-PRIM-SEL-AGG :where-attach)))
		(case sel
		      (0 (when (set-offset-p :y1 :y1-over) 
			       (s-value obj :y1-offset y-offset)))
		      (1 (when (set-offset-p :y2 :y2-over)
			       (s-value obj :y2-offset y-offset)))))
	      (when (set-offset-p :top :top-over)
		    (s-value obj :top-offset y-offset)))))))

;; when either the unconstrain or customize buttons are hit, call this
;; function to clear the line buttons

(defun deselect-line-buttons ()
  (declare (special LINE-CON-PRIM-SEL-AGG LINE-CON-SEC-SEL-AGG))
  (deselect-constraint-button
   (g-value LINE-CON-PRIM-SEL-AGG :line :buttons))
  (deselect-constraint-button
   (g-value LINE-CON-PRIM-SEL-AGG :box :buttons))
  (deselect-constraint-button
   (g-value LINE-CON-SEC-SEL-AGG :line :buttons))
  (deselect-constraint-button
   (g-value LINE-CON-SEC-SEL-AGG :box :buttons)))

(defun LINE-CUSTOM-FN (gadget string)
  (declare (ignore string))
  (multiple-value-bind (left top)
    (opal:convert-coordinates (g-value gadget :window)
			      (g-value gadget :left)
			      (opal:bottom gadget)
			      nil)
    (c32 nil nil nil :left left :top top)))

(defun LINE-UNCONSTRAIN-FN (gadget string)
  (declare (ignore gadget string))
  (deselect-line-buttons)
  (when (g-value line-con-prim-sel-agg :active)
;	(reset-undo)
	(dolist (obj (g-value *selection-info* :selected))
	  (if (not (is-a-line-p obj))
	      (progn
		(lapidary-destroy-constraint obj :left :left-over :left-offset)
		(lapidary-destroy-constraint obj :top :top-over :top-offset))
	      (let ((sel (g-value LINE-CON-PRIM-SEL-AGG :where-attach)))
	        (if sel
		  (case sel
			(0 (lapidary-destroy-constraint obj :x1 :x1-over 
							:x1-offset)
			   (lapidary-destroy-constraint obj :y1 :y1-over 
							:y1-offset))
			(1 (lapidary-destroy-constraint obj :x2 :x2-over 
							:x2-offset)
			   (lapidary-destroy-constraint obj :y2 :y2-over 
							:y2-offset))
			)))))))
