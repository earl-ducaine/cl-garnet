;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file created by the Garnet Interface Builder
;;; on Jun 28, 1990, 2:01 PM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
===============================================================================
Change Log:
     3/4/93 Brad Myers - made work with multiple objects
     4/24/92 Pervin - Changed defvar to proclaim.
     2/18/92 Brad Myers - added constant definitions
     03/01/91 Andrew Mickish - Changed s-value's of :value slot to
                set-initial-value's in Show-Font-Dialog
     02/22/91 Osamu Hashimoto - seperated from particular gadgets
===============================================================================
|#

(in-package "GILT")

(defparameter *Obj-For-Font-To-Modify* NIL)
(declaim (special FONT-PROP))

(defun Show-Font-Dialog (left top obj-or-objs slot)
  (let* ((oldfont (gilt:value-from-one-obj obj-or-objs slot))
	 val)
    (setq *Obj-For-Font-To-Modify* (list oldfont obj-or-objs slot))
    (set-initial-value FONT-PROP :family (or (g-value oldfont :family) :fixed))
    (set-initial-value FONT-PROP :face (case (or (g-value oldfont :face)
						 :roman)
					 (:roman NIL)
					 (:italic (list :italic))
					 (:bold (list :bold))
					 (:bold-italic (list :bold :italic))
					 (T (error "Bad face ~s" val))))
    (set-initial-value FONT-PROP :size (or (g-value oldfont :size) :medium))
    (show-in-window FONT-PROP left top)))

(defun Font-Dialog-OK-Func (gadget item)
  (let* ((oldfont (first *Obj-For-Font-To-Modify*))
	 (obj-or-objs (second *Obj-For-Font-To-Modify*))
	 (slot (third *Obj-For-Font-To-Modify*))
	 newfont)
    (when (and obj-or-objs slot)
      (setq newfont (opal:get-standard-font
		     (or (g-local-value FONT-PROP :family :value)
			 (g-value oldfont :family))
		     (let ((val (g-local-value FONT-PROP :face :value)))
		       (cond ((= (length val) 2) :bold-italic)
			     ((eq (car val) :bold) :bold)
			     ((eq (car val) :italic) :italic)
			     (T :roman)))
		     (or (g-local-value FONT-PROP :size :value)
			 (g-value oldfont :size))))
      (gilt:set-value-of-appropriate-objs obj-or-objs slot newfont))
    (Font-Dialog-Cancel-Func gadget item)
    newfont))

(defun Font-Dialog-Cancel-Func (gadget item)
  (declare (ignore gadget item))
  (let ((win (g-value FONT-PROP :window)))
    (s-value win :visible NIL)
    (opal:update win))
  (setq *Obj-For-Font-To-Modify* NIL))

;;; Function called when the properties button hit with a string selected,
;;; returns T
(defun String-Props (obj)
  (Show-Font-Dialog (+ 30 (g-value obj :window :left))
		    (+ 30 (g-value obj :window :top))
		    obj :font)
  T)

