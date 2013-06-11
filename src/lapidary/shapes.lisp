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
;;; 08/24/92 amickish - Removed obj-over and added inter to declare ignore
;;;                     list in Shapes-Menu-Handler

(in-package "LAPIDARY")

(defparameter *shape-button-list* NIL)

(defun shapes-do-go ()

(declare (special aggrelist-feedback))
(shapes-do-stop)

(create-instance 'SHAPE-WIN inter:interactor-window
   (:title "shapes")
   (:left (first *shape-menu-dimensions*))
   (:top (second *shape-menu-dimensions*))
   (:width (o-formula (+ 20 (gvl :aggregate :width))))
   (:height (o-formula (+ 20 (gvl :aggregate :height)))))


(s-value SHAPE-WIN :aggregate (create-instance 'SHAPE-TOP-AGG opal:aggregate))
;; I commented this out -- ECP
;; (opal:update SHAPE-WIN)


(setf *shape-button-list*
      (make-button-list `((opal:line :value opal:line
				      :name :line)
			  (garnet-gadgets:arrow-line 
			         :value garnet-gadgets:arrow-line
				 :name :arrow-line)
			  (garnet-gadgets:double-arrow-line 
			         :value garnet-gadgets:double-arrow-line
				 :name :double-arrow-line)
			  (opal:rectangle :value opal:rectangle
					   :name :rectangle)
			  (opal:roundtangle :value opal:roundtangle
					     :name :roundtangle)
			  (opal:circle :value opal:circle
					:name :circle)
			  (opal:text :string "text" :value opal:cursor-text
				      :name :text)
			  (opal:text :string "multi-text" :value opal:cursor-multi-text
				      :name :multi-text)
			  (opal:text :string "window" 
				      :value inter:interactor-window
				      :name :window)
			  (opal:text :string "bitmap" 
				      :value opal:bitmap
				      :name :bitmap)
			  (opal:text :string "horizontal list" 
				      :value opal:aggrelist
				      :name :horizontal-aggrelist)
			  (opal:text :string "vertical list" 
				      :value opal:aggrelist
				      :name :vertical-aggrelist))))

;;  SHAPE-MENU is the top panel of buttons controlling line-styles
;;
(create-instance 'SHAPE-MENU opal:aggrelist
   (:constant '(t :button-width :button-height))
   (:left 10)
   (:top 10)
   (:button-width 120)
   (:button-height 40)
   (:value (o-formula (gvl :selected :label :value)))
   (:name (o-formula (gvl :selected :label :name)))
   (:parts *SHAPE-button-list*)
   (:interactors
    `((:press ,LAPIDARY-MENU-BUTTON-INTER
	      (:final-function shapes-menu-handler)))))

;; set the initial selection to be a rectangle
(let ((initial-selection (fourth (g-value shape-menu :components))))
  (s-value shape-menu :selected initial-selection)
  (s-value shape-menu :old-value initial-selection)
  (s-value initial-selection :selected t))

;; store the shape menu in the aggrelist-feedback prototype
(s-value aggrelist-feedback :shape-menu shape-menu)

(opal:add-components SHAPE-TOP-AGG SHAPE-MENU)
(opal:update SHAPE-WIN))

(defun shapes-do-stop ()
  (when (boundp 'SHAPE-WIN) (opal:destroy SHAPE-WIN)))

;; if window is selected, create a window
(defun shapes-menu-handler (inter obj-over)
  (declare (ignore inter))
  (declare (special read-file))
  (cond ((eq (g-value shape-menu :name) :window)
	 (make-drawing-window)
	 (s-value (g-value shape-menu :old-value) :selected t)
	 (s-value obj-over :selected nil)
	 (s-value shape-menu :selected (g-value shape-menu :old-value)))
	((eq (g-value shape-menu :name) :bitmap)
	 (s-value (g-value shape-menu :old-value) :selected t)
	 (s-value obj-over :selected nil)
	 (s-value shape-menu :selected (g-value shape-menu :old-value))
	 (s-value read-file :function-for-ok 'load-bitmap)
	 (show-read-dialog nil nil))
	;; do nothing if the list option is chosen
	((or (eq (g-value shape-menu :name) :horizontal-aggrelist)
	     (eq (g-value shape-menu :name) :vertical-aggrelist)))
	;; save the current selection so that it can be restored if either
	;; the window or bitmap options are chosen, or so that it can
	;; be used as a prototype if the list option is chosen
	(t (s-value shape-menu :old-value obj-over))))
