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
;;;
;;; This deals with popping up dialog boxes
;;;
;;; Designed and implemented by Brad Myers


(in-package "C32")

;; l will be a list of 2 lists.  Returns them separately.
;; Turns all strings into atoms.
(defun convert-to-atoms-list (l)
  (if l
      (let ((l1 (car l))
	    (l2 (cadr l))
	    newl1 newl2)
	(dolist (i l1)
	  (if (stringp i)
	      (push (read-from-string i) newl1)
	      (push i newl1)))
	(dolist (i l2)
	  (if (stringp i)
	      (push (read-from-string i) newl2)
	      (push i newl2)))
	(values newl1 newl2))
      ;; else
      (values NIL NIL)))



(defun Pop-Up-Confirm-Copy-Formula (from-obj from-slot to-obj to-slot
					     from-slots to-slots
					     expr from-c32item)
    (Show-copy from-obj from-slot to-obj to-slot from-slots to-slots
	       #'(lambda (pop-up values)
		   (declare (ignore values))
		   (let ((slot-conv (get-one-line-values
					 (g-value pop-up :SLOT-LIST))))
		     (multiple-value-bind (old-slot new-slot)
			 (convert-to-atoms-list slot-conv)
		       (copy-and-set-formula expr to-obj to-slot
					     old-slot new-slot
					     from-c32item))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pops up a window that allows the user to select an object, then
;;; calls the function on the result.  If slot-func then second
;;; argument to func may be a slot, and this function will guess which one.

(defun Pop-Up-Request-Point-Object (func slot-func
					 &optional from-obj from-slot)
  (unless (and (boundp 'ask-object)
	       (schema-p ask-object))
    (create-ask-object))
  (unless (and (boundp 'c32-point-to-obj-feedback)
	       (schema-p c32-point-to-obj-feedback))
    (create-c32-point-to-obj-feedback))
  (s-value ask-object :slot-func slot-func)
  (s-value Ask-object :final-func func)
  (s-value ask-object :from-obj from-obj)
  (s-value ask-object :from-slot from-slot)
  (let ((win (gilt:show-in-window ask-object)))
    (if (and win (is-a-p win inter:interactor-window))
      ;; Make sure window will be deleted.
      (push win *all-windows*))))

;; the next function is in the OK-function slot of ask-object
;;
(defun pop-up-ask-object-ok (gadget values)
  (declare (ignore values))
  (let ((slot-func (g-value gadget :slot-func))
	(final-func (g-value gadget :final-func))
	(obj (g-value gadget :sel-obj))
	(slot (g-value gadget :obj-slot)))
    (if obj
	(if slot-func
	  (funcall final-func obj slot)
	  (funcall final-func obj))
	; else nothing selected
	(inter:beep))))


  
;; OLD: used (garnet-debug:ident)
;;
;;(defun Pop-Up-Request-Point-Object (func slot-func
;;					 &optional from-obj from-slot)
;;  ;;;*** NIY
;;  (let ((l (garnet-debug:ident))
;;	obj slot)
;;    (setq obj (car l))
;;    (if obj
;;	(progn
;;	  (if slot-func
;;	      (progn
;;		(setq slot (funcall slot-func from-obj from-slot obj
;;				    (third l)(fourth l)))
;;		(funcall func obj slot))
;;	      (funcall func obj)))
;;	; else no object
;;	(inter:beep))))
