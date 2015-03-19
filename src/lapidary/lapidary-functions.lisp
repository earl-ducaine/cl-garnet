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
;;;  This file contains code that is exported from lapidary which an
;;;  application constructed using lapidary might use

;;; CHANGE LOG
;;;
;;; 08/25/92 amickish - Added proclaim

(in-package "LAPIDARY")

;; These variables appear in many files
(declaim (special common-lisp-user::*garnet-objects-just-created*
		  common-lisp-user::*garnet-object-just-created*
		  common-lisp-user::*used-gilt-version*
		  common-lisp-user::*used-garnet-version*))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(lapidary-two-point-interactor directional-move-grow-interactor
	    lapidary-angle-interactor lapidary-menu-interactor
	    lapidary-button-interactor lapidary-text-interactor)))

;; rewrite the set of opal:gv-... functions so that they are forgiving
;; of broken links

(defun gv-bottom (gob)
  (if gob (1- (+ (gv gob :top) (gv gob :height)))
          (kr::broken-link-throw nil :top)))

(defun gv-right (gob)
  (if gob (1- (+ (gv gob :left) (gv gob :width)))
          (kr::broken-link-throw nil :left)))

(defun gv-center-x (gob)
  (if gob (+ (gv gob :left) (truncate (gv gob :width) 2))
          (kr::broken-link-throw nil :left)))

(defun gv-center-y (gob)
  (if gob (+ (gv gob :top) (truncate (gv gob :height) 2))
          (kr::broken-link-throw nil :top)))

;;; For formulas that want to set an object's center, right or bottom.

; Gives the value for :left such that (gv-right :self) equals (gv gob :left)
(defun gv-right-is-left-of (gob)
  (if gob (- (gv gob :left) (gvl :width))
          (kr::broken-link-throw nil :left)))

; Gives the value for :top such that (gv-bottom :self) equals (gv gob :top)
(defun gv-bottom-is-top-of (gob)
  (if gob (- (gv gob :top) (gvl :height))
          (kr::broken-link-throw nil :top)))

; Gives the value for :left such that (gv-center-x :self) equals (gv-center-x gob)
(defun gv-center-x-is-center-of (gob)
  (if gob (- (gv-center-x gob) (truncate (gvl :width) 2))
          (kr::broken-link-throw nil :left)))

; Gives the value for :top such that (gv-center-y :self) equals (gv-center-y gob)
(defun gv-center-y-is-center-of (gob)
  (if gob (- (gv-center-y gob) (truncate (gvl :height) 2))
          (kr::broken-link-throw nil :top)))

(defun set-selected-axes (inter obj-being-changed first-points)
  ;; check whether a box is being moved or grown, and if so, whether
  ;; it should only be changed along one axis
  (when (not (g-value inter :line-p))
	;; do different things based on whether the box is being moved
	;; or grown
	(let ((box (g-value obj-being-changed :box)))
	  (if (g-value inter :grow-p)
	      (progn
		(cond ((eq (g-value inter :grow-parms) :width)
		       ;; restore height and top slots to original values
		       (setf (second box) (second first-points))
		       (setf (fourth box) (fourth first-points)))
		      ((eq (g-value inter :grow-parms) :height)
		       ;; restore width and left slots to original values
		       (setf (first box) (first first-points))
		       (setf (third box) (third first-points)))
		      (t nil)))
	    (progn
	      (cond ((eq (g-value inter :move-parms) :top)
		     ;; restore left slot to its original value
		     (setf (first box) (first first-points)))
		    ((eq (g-value inter :move-parms) :left)
		     ;; restore top slot to its original value
		     (setf (second box) (second first-points)))
		    (t nil)))))))

;;; the following four functions guarantee that an object only moves or
;;; grows along the desired axes

(defun dir-move-grow-start-action (inter obj-being-changed first-points)
  (when (and (null (g-value inter :grow-p)) (null (g-value inter :line-p)))
	(setf (third first-points) (g-value obj-being-changed :width))
	(setf (fourth first-points) (g-value obj-being-changed :height)))
  (kr::call-prototype-method inter obj-being-changed first-points))

(defun dir-move-grow-running-action (inter obj-being-changed new-points)
  (kr::call-prototype-method inter obj-being-changed new-points)
  (set-selected-axes inter obj-being-changed (g-value inter :saved-original-points)))

(defun dir-move-grow-stop-action (inter obj-being-changed final-points)
  (kr::call-prototype-method inter obj-being-changed final-points)
  (set-selected-axes inter obj-being-changed (g-value inter :saved-original-points)))

(defun dir-move-grow-back-inside-action (inter outside-control 
					       obj-being-changed new-points)
  (kr::call-prototype-method inter outside-control 
			     obj-being-changed new-points)
  (set-selected-axes inter obj-being-changed 
		     (g-value inter :saved-original-points)))


#|
(create-instance 'directional-move-grow-interactor inter:move-grow-interactor
   (:start-where nil)
   (:start-action #'dir-move-grow-start-action)
   (:running-action #'dir-move-grow-running-action)
   (:stop-action #'dir-move-grow-stop-action)
   (:back-inside-action #'dir-move-grow-back-inside-action)
   (:grow-parms nil)
   (:move-parms nil))
|#
(create-instance 'directional-move-grow-interactor inter:move-grow-interactor
   (:start-where nil)
   (:start-action 'dir-move-grow-start-action)
   (:move-line-parms '(t t t t))
   (:move-box-parms '(t t t t))
   (:grow-box-parms '(t t t t))
   (:grow-line-parms '(t t t t))
   (:move-parms (o-formula (if (gvl :line-p) 
			       (gvl :move-line-parms) 
			       (gvl :move-box-parms))))
   (:grow-parms (o-formula (if (gvl :line-p) 
			       (gvl :grow-line-parms) 
			       (gvl :grow-box-parms))))
   (:slots-to-set (o-formula (if (gvl :grow-p)
				 (gvl :grow-parms) 
			         (gvl :move-parms)))))

(s-value directional-move-grow-interactor :start-where :not-supplied)

;;;====================================================================
;;;
;;; the following code allows a two-point interactor to automatically
;;; create standard feedback, either a rectangle or line, based on
;;; the type of object being created
;;;
;;;====================================================================

(create-instance 'two-pt-box-feedback opal:rectangle
		     (:draw-function :xor)
		     (:fast-redraw-p t)
		     (:name "Interim Rect feedback")
		     (:left (o-formula (first (gvl :box))))
		     (:top (o-formula (second (gvl :box))))
		     (:width (o-formula (third (gvl :box))))
		     (:height (o-formula (fourth (gvl :box))))
		     (:visible nil)
		     (:box '(0 0 0 0))
		     (:line-style opal:dashed-line))
(create-instance 'two-pt-line-feedback opal:line
		     (:draw-function :xor)
		     (:fast-redraw-p t)
		     (:name "Interim Line feedback")
		     (:x1 (o-formula (first (gvl :points))))
		     (:y1 (o-formula (second (gvl :points))))
		     (:x2 (o-formula (third (gvl :points))))
		     (:y2 (o-formula (fourth (gvl :points))))
		     (:visible nil)
		     (:points '(0 0 0 0))
		     (:line-style opal:dashed-line))

(defun lap-two-point-inter-start-action (inter points)
  ;; determine if standard feedback is used. If it is and there are
  ;; no feedback objects, create rectangle and line feedback objects.
  ;; Also, make sure the feedback object is connected to the proper
  ;; aggregate
  (let (feedback-obj parent)
    (when (g-value inter :std-feedback-p) 
	  (cond ((and (null (g-local-value inter :std-box-feedback))
		      (not (g-value inter :line-p)))
		 (s-value inter :std-box-feedback 
			  (create-instance nil two-pt-box-feedback)))
		((and (null (g-local-value inter :std-line-feedback))
		      (g-value inter :line-p))
		 (s-value inter :std-line-feedback
			  (create-instance nil two-pt-line-feedback)))))
    (setf feedback-obj (g-value inter :feedback-obj))
    (setf parent (g-local-value feedback-obj :parent))
    (if parent
	;; have to remove-component to ensure that feedback object is added
	;; to the back of the components list
	(progn
	  (opal:remove-component parent feedback-obj)
	  (opal:add-component parent feedback-obj))
        (opal:add-component (g-value inter :current-window :aggregate) 
			    feedback-obj))
    (kr::call-prototype-method inter points)))

(create-instance 'lapidary-two-point-interactor inter:two-point-interactor
     (:start-where nil)
     (:std-feedback-p t)
     (:feedback-obj (o-formula (if (gvl :line-p)
				   (gvl :std-line-feedback)
				   (gvl :std-box-feedback))))
     (:start-action 'lap-two-point-inter-start-action))

(s-value lapidary-two-point-interactor :start-where :not-supplied)

(create-instance 'lapidary-button-interactor inter:button-interactor
     (:start-where nil))

(create-instance 'lapidary-menu-interactor inter:menu-interactor
     (:start-where nil))

(create-instance 'lapidary-angle-interactor inter:angle-interactor
     (:start-where nil))

(create-instance 'lapidary-text-interactor inter:text-interactor
     (:start-where nil))

(s-value lapidary-button-interactor :start-where :not-supplied)
(s-value lapidary-menu-interactor :start-where :not-supplied)
(s-value lapidary-angle-interactor :start-where :not-supplied)
(s-value lapidary-text-interactor :start-where :not-supplied)

;;; push the new interactors onto the *standard-names* list in opal
;;; so that write-gadget recognizes them

(pushnew 'lapidary:lapidary-button-interactor opal::*standard-names*)
(pushnew 'lapidary:lapidary-text-interactor opal::*standard-names*)
(pushnew 'lapidary:lapidary-angle-interactor opal::*standard-names*)
(pushnew 'lapidary:lapidary-menu-interactor opal::*standard-names*)
(pushnew 'lapidary:lapidary-two-point-interactor opal::*standard-names*)
(pushnew 'lapidary:directional-move-grow-interactor opal::*standard-names*)

;;; temporary fix so that some interactor slots are not dumped

(s-value inter:interactor :do-not-dump-slots 
   (append '(:current-priority-level :copy-old-window :x-off :y-off
	     :final-feed-avail :prev-x :prev-y)
	   (g-value inter:interactor :do-not-dump-slots)))
