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
;;; Functions for gadget string editing
;;;
;;; $Id::                                                             $


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(string-set-func)))

;; Used for testing the string-set-func methods
(defmacro string-set-func (gadget-obj str-obj final-event final-string)
  `(let ((obj2 ,gadget-obj))
    (kr-send obj2 :string-set-func obj2 ,str-obj ,final-event ,final-string)))

;;;Set a single slot and return T
(defun set-one-value (gadget-obj slot-name final-string)
  (s-value gadget-obj slot-name
	   (if (string= final-string "") NIL final-string))
  T) ; must return T

(defun DeleteNth (lst n)
  (append (subseq lst 0 n)
	  (subseq lst (1+ n))))

;;;Set a member of an aggrelist
(defun Aggrelist-Edit-String-Func (gadget-obj aggrel str-obj
				   final-event final-string slot-for-rank)
  (let ((str-obj-name (g-value str-obj :known-as))
	rank)
    (dolist (o (g-value aggrel :components))
      (when (eq (g-value o str-obj-name) str-obj)
	(setq rank (g-value o slot-for-rank)) ; use :rank or :real-rank
					      ; depending on type of object
	(return))) ; leave dolist
    (unless rank (return-from Aggrelist-Edit-String-Func NIL))
    ;; Final-string will be a list if we are dealing with a menubar
    (if (string= "" (if (consp final-string)
			(car final-string)
			final-string))
	; delete the rank item
	(opal:remove-nth-item gadget-obj rank)
	; else replace string
	(progn
	  (let ((olditems (g-value gadget-obj :items)))
	  (when (or (not (integerp rank))
		    (< rank 0)
		    (>= rank (length olditems)))
	    (error "rank is not a good number ~s~%" rank))
	  (opal:change-item gadget-obj final-string rank)
	  (when (and (eq (1+ rank) (length olditems))
		     (eq (inter:event-char final-event) :control-\n))
	      ; then add a new item at the end
	    (let ((val (kr-send gadget-obj :new-item-label gadget-obj)))
	      (cond (val (opal:add-item gadget-obj val))
		    (t (setf val (1+ (or (g-value gadget-obj :last-label-used)
					 3)))
		       (s-value gadget-obj :last-label-used val)
		       (opal:add-item gadget-obj (format NIL "Label~a" val))
		       )))))))
    T)) ;return T is successful

;; For the motif-text-button-panel, motif-radio-button-panel, and
;; motif-check-button-panel
(defun Motif-Button-String-Func (gadget-obj str-obj final-event final-string)
  (let ((aggrel (g-value gadget-obj :BUTTON-LIST)))
    (Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				final-event final-string :rank)))


;; For both the option-button and motif-option-button
(defun Option-Button-String-Func (gadget-obj str-obj final-event final-string)
  (if (eq str-obj (g-value gadget-obj :option-button-label))
      ; then is label
      (opal::set-one-value gadget-obj :label final-string)
      ; else is item in the menu
      (let ((aggrel (g-value gadget-obj :OPTION-BUTTON-MENU :MENU-ITEM-LIST)))
	(Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				    final-event final-string :rank))))




