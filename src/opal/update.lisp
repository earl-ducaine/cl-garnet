;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(in-package "OPAL")

;;; This updates the :update-slots-values slot, which should hold a list
;;; containing the values of the update-slots at the last update.  It also
;;; returns T iff one of them has changed (ie, we need to be updated).
;;; This also sets update-info-force-computation-p to NIL, since we definitely
;;; don't need to do this after running this macro.
;;;
(defun update-slots-values-changed (object first-changed obj-update-info)
  (declare (optimize (speed 3) (safety 1)))
  (let* ((update-slots-values (g-local-value object :update-slots-values))
	 ;; dzg - changed from GET-LOCAL-VALUE to GET-VALUE
	 (start-slot-list (get-value object :update-slots))
	 (first-p (null update-slots-values))
	 changed-p new-value)
    (if first-p
      (setq update-slots-values
	    (s-value object :update-slots-values
		     (make-array (length start-slot-list)
				 :initial-element nil))))
    (setf (update-info-force-computation-p obj-update-info) NIL)
    (dotimes (x first-changed)
      (setq start-slot-list (cdr start-slot-list)))
    (do  ((slot-list start-slot-list (cdr slot-list))
	  (vals-indx first-changed (1+ vals-indx)))
	 ((null slot-list) changed-p)
      (unless (equal (aref update-slots-values vals-indx)
		     (setq new-value (g-value object (car slot-list))))
	(setf (aref update-slots-values vals-indx)
	      (if (listp new-value) (copy-list new-value) new-value))
	(setq changed-p T)))))


;;; This is the same as the previous call, but it only checks if a value has
;;; changed.  If so, it returns the index into update-slots-values of the first
;;; changed entry.  Elsewise, it returns NIL.  This does not alter anything!
;;; It is used in only one place, to check if a fastdraw object has really
;;; changed when it is invalidated.
;;; If there is no update-slots-values entry, it just returns 0.
(defun simple-update-slots-values-changed (object)
 (declare (optimize (speed 3) (safety 1)))
 (let ((update-slots-values (g-local-value object :update-slots-values)))
  (if update-slots-values
	;; ecp - changed from GET-LOCAL-VALUE to GET-VALUE
   (do  ((slot-list (get-value object :update-slots) (cdr slot-list))
	 (vals-indx 0 (1+ vals-indx)))
	((null slot-list) NIL)
	(unless (equal (aref update-slots-values vals-indx)
		       (g-value object (car slot-list)))
	  (return vals-indx)))
   0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now makes the aggregate's :old-bbox valid at all times!!!
;;; DO NOT CALL THIS UNLESS THE AGGREGATE IS DEFINITELY VISIBLE!!!
;;;
(define-method :update aggregate (agg update-info
					   line-style-gc filling-style-gc
					   bbox-1 bbox-2
					   &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 1)))
  (let ((dirty-p (update-info-dirty-p update-info))
	(agg-bbox (update-info-old-bbox update-info)))
    (when
	(or  dirty-p
	     total-p
	     (and (bbox-valid-p agg-bbox)
	          (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (let (child-update-info child-bbox)
	(setf (bbox-valid-p agg-bbox) NIL);; clear the old one!
	(dovalues (child agg :components :local t)
	  (if (g-value child :visible)
	    (progn
	      (setq child-bbox
		    (update-info-old-bbox
		     (setq child-update-info
			   (g-local-value child :update-info))))
	      (if (is-a-p child aggregate)
		(update child child-update-info
			line-style-gc filling-style-gc bbox-1 bbox-2 total-p)
		(update child child-update-info bbox-1 bbox-2 total-p))
	      (merge-bbox agg-bbox child-bbox));; and set the new one!
					; else
	    ;; if the child's dirty bit is set, recursively visit the child
	    ;; and all its children and turn off their dirty bits
	    (let ((child-update-info (g-local-value child :update-info)))
	      (when (update-info-dirty-p child-update-info)
		(clear-dirty-bits child child-update-info)))))
	(if dirty-p (setf (update-info-dirty-p update-info) NIL))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This will not be called unless the gob is already visible!!!
(define-method :update graphical-object (gob update-info
						  bbox-1 bbox-2
						  &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 1)))
  (let ((old-bbox (update-info-old-bbox update-info))
	(a-window (g-value gob :window)))
    ;; Fix for changes from 2.2 to 3.0. ---fmg
    (unless a-window
      (setf a-window (g-value gob :parent :window)))
    (unless (update-info-on-fastdraw-list-p update-info)
      (cond (total-p
		(update-slots-values-changed gob 0 update-info)
		(update-bbox gob old-bbox)
		(draw gob a-window)
		(setf (update-info-dirty-p update-info) NIL))

	    ((update-info-dirty-p update-info)
		(when (update-info-force-computation-p update-info)
		   (update-slots-values-changed gob 0 update-info)
		   (update-bbox gob old-bbox))
		(draw gob a-window)
		(setf (update-info-dirty-p update-info) NIL))

	    (bbox-2			; 2 valid clip-masks?
	     (when (or (bbox-intersect-p old-bbox bbox-1)
		       (bbox-intersect-p old-bbox bbox-2))
	       (draw gob a-window)))
	    ((bbox-intersect-p old-bbox bbox-1)
	       (draw gob a-window)))
      ;; New line added because of new KR 2.0.10 -- ECP 6/23/92
      ;; Without this line, the Save window in garnetdraw does not update.
      (setf (update-info-invalid-p update-info) nil))))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
