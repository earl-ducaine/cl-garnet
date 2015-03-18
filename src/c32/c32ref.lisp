;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: C32; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Garnet User Interface Development Environment
;;; Copyright (c) 1990 Carnegie Mellon University
;;; All rights reserved.  The CMU software License Agreement specifies
;;; the terms and conditions for use and redistribution.
;;;
;;; If you want to use this code or anything developed as part of the
;;; Garnet project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                             $


;;; C32 is a spreadsheet interface for Garnet constraints
;;; This file contains the code to show the uses of slots
;;;
;;; Designed and implemented by Brad Myers


(in-package "C32")

(defparameter *Trace-Lines* NIL)
(defparameter *Trace-C32Items* NIL)

(defun Clear-Traces ()
  (dolist (trace *Trace-Lines*)
    (opal:destroy trace))
  (setq *Trace-Lines* NIL)
  (setq *Trace-C32Items* NIL))


;;; Returns: a list of pairs, (schema . slot), for all objects that
;;; depend on the <slot> of the <object>.
;;;
(defun depend-on-me (object slot)
  (mapcar #'(lambda (x) (cons (kr::on-schema x) (kr::on-slot x)))
	  (kr::get-dependents object slot)))



;; If on-me-p then traces that depend-on-me, else traces I depend on.
;;
(defun Create-Trace (c32item panel-set on-me-p &optional recursive-p)
  (let ((from-obj (g-value c32item :obj))
	(from-slot (g-value c32item :slot))
	dependencies item
	c-sel c-sec-sel)
    (unless recursive-p
      (setf c-sel (g-value *Current-Selection-Feedback* :obj-over))
      (setf c-sec-sel (g-value *Current-Sec-Selection-Feedback* :obj-over))
      ;; Turn off the selection interactors feedback.
      (s-value *Current-Selection-Feedback* :obj-over nil)
      (s-value *Current-Sec-Selection-Feedback* :obj-over nil)
      (opal:update (g-value c32item :window)))
    (Push c32item *Trace-C32Items*)
    (setq dependencies (if on-me-p
			 (depend-on-me from-obj from-slot)
			 (kr::i-depend-on from-obj from-slot)))
    (dolist (dep dependencies)
      (setq item (Create-A-Dependency  panel-set c32item (car dep) (cdr dep)
				       on-me-p))
      ;; recursive
      (when item
	(unless (member item *Trace-C32Items*) ; prevent cycles
	  (Create-Trace item panel-set on-me-p T))))
    (unless recursive-p
      ;; Turn on the selection interactors feedback.
      (s-value *Current-Selection-Feedback* :obj-over c-sel)
      (s-value *Current-Sec-Selection-Feedback* :obj-over c-sec-sel))))


(defparameter arrow-visible-formula
  (o-formula (let* ((self (gv :self))
		    (c32-items (gvl :to-c32-item :parent :components))
		    (to-slot (g-value self :to-slot))
		    (from-slot (g-value self :from-slot))
		    to from)
	       (dolist (item c32-items)
		 (if (eq (g-value item :slot) to-slot)
		   (setf to (s-value self :to-c32-item item))
		   (if (eq (g-value item :slot) from-slot)
		     (setf from
			   (s-value self :from-c32-item item)))))
	       (and to from))))


(create-instance 'I-depend-on-arrow garnet-gadgets:arrow-line
  (:from-c32-item NIL)
  (:to-c32-item NIL)
  (:line-style (create-instance NIL opal:line-style
		 (:line-thickness 2) (:foreground-color opal:red)))
  (:visible (formula arrow-visible-formula))
  (:x1 (o-formula (opal:gv-center-x (gvl :from-c32-item :form-icon))))
  (:y1 (o-formula (opal:gv-center-y (gvl :from-c32-item :form-icon))))
  (:x2 (o-formula (gvl :to-c32-item :value-str :left)))
  (:y2 (o-formula (opal:gv-center-y (gvl :to-c32-item :value-str)))))



(create-instance 'Depend-On-Me-Arrow garnet-gadgets:arrow-line
  (:from-c32-item NIL)
  (:to-c32-item NIL)
  (:line-style (if (g-value opal:color :color-p)
		 (create-instance NIL opal:line-style
		   (:line-thickness 2)
		   (:foreground-color opal:green))
		 opal:line-0))
  (:visible (formula arrow-visible-formula))
  (:x1 (o-formula (opal:gv-center-x (gvl :to-c32-item :form-icon))))
  (:y1 (o-formula (opal:gv-center-y (gvl :to-c32-item :form-icon))))
  (:x2 (o-formula (opal:gv-right (gvl :from-c32-item :value-str))))
  (:y2 (o-formula (opal:gv-center-y
		   (gvl :from-c32-item :value-str)))))


;; returns to-c32item
;;
(defun Create-A-Dependency (panel-set from-c32item to-obj to-slot on-me-p)
  (let ((to-c32item (Find-C32-Item to-obj to-slot panel-set))
	(typ (if on-me-p Depend-on-me-arrow I-depend-on-arrow))
	lin)
    (when to-c32item
      (setq lin (create-instance NIL typ
				 (:from-c32-item from-c32item)
				 (:to-c32-item to-c32item)))
      (s-value lin :to-slot to-slot)	; remember in case c32item is changed.
      (s-value lin :from-slot (g-value from-c32item :slot))
      (push lin *Trace-Lines*)
      (opal:add-component (g-value panel-set :inner-aggregate)
			  lin)
      to-c32item)))


;; should deal with invisible items (scrolled off)
(defun Find-C32-Item (to-obj to-slot panel-set)
  (dolist (panel (g-value panel-set :c32-panels))
    (when (eq to-obj (g-value panel :obj))
      (dolist (item (g-value panel :c32-items))
	(when (eq to-slot (g-value item :slot))
	  (return-from Find-C32-Item 
	    (if (g-value item :visible)
	      item NIL)))))))
