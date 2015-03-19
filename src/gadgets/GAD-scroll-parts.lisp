;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;*******************************************************************;;

;;; $Id$
;;


;;;  GAD-scroll-parts
;;
;;   This module is a collection of schema definitions required by the trill
;;   device and all scroll bars and sliders.
;; 
;;   Written by Andrew Mickish


(in-package "GARNET-GADGETS")

;;;  TRILL INTERACTOR AND INCREMENTOR FUNCTIONS
;;

;;  Used to increment (or decrement) the value closer to VAL-1
;;
(defun VAL-1-FN (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((parent (g-value interactor :operates-on :parent))
	 (val-1 (g-value parent :val-1))
	 (val-2 (g-value parent :val-2))
	 (value (g-value parent :value))
	 (inc-by (g-value interactor :operates-on :inc-by)))
    (declare (type (or number null) val-1 val-2 value))
    (cond 

      ;; there is a max and a min
      ((and val-1 val-2)
       (if (< val-1 val-2)
	   (let ((thresh-val (+ val-1 inc-by)))
	     (if (> value thresh-val)
		 (s-value parent :value (- value inc-by))
		 (s-value parent :value val-1)))
	   (let ((thresh-val (- val-1 inc-by)))
	     (if (< value thresh-val)
		 (s-value parent :value (+ value inc-by))
		 (s-value parent :value val-1)))))

      ;; there is no max
      ((and val-1 (not val-2))
       (let ((thresh-val (+ val-1 inc-by)))
	 (if (> value thresh-val)
	     (s-value parent :value (- value inc-by))
	     (s-value parent :value val-1))))

      ;; there is no min
      (t (s-value parent :value (- value inc-by))))))


;; This version is for the scrool wheel interactor.
;; 
(defun VAL-1-WHEEL-FN (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((obj (g-value interactor :operates-on))
	 (val-1 (g-value obj :val-1))
	 (val-2 (g-value obj :val-2))
	 (value (g-value obj :value))
	 (inc-by (g-value interactor :inc-by)))

    (cond 
      ;; there is a max and a min
      ((and val-1 val-2)
       (if (< val-1 val-2)
	   (let ((thresh-val (+ val-1 inc-by)))
	     (if (> value thresh-val)
		 (s-value obj :value (- value inc-by))
		 (s-value obj :value val-1)))
	   (let ((thresh-val (- val-1 inc-by)))
	     (if (< value thresh-val)
		 (s-value obj :value (+ value inc-by))
		 (s-value obj :value val-1)))))
      ;; there is no max
      ((and val-1 (not val-2))
       (let ((thresh-val (+ val-1 inc-by)))
	 (if (> value thresh-val)
	     (s-value obj :value (- value inc-by))
	     (s-value obj :value val-1))))
      ;; there is no min
      (t (s-value obj :value (- value inc-by))))))

;;  Used to increment (or decrement) the value closer to VAL-2
;;	   
(defun VAL-2-FN (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((parent (g-value interactor :operates-on :parent))
	 (val-1 (g-value parent :val-1))
	 (val-2 (g-value parent :val-2))
	 (value (g-value parent :value))
	 (inc-by (g-value interactor :operates-on :inc-by)))
    (cond

      ;; there is a max and a min
      ((and val-1 val-2)
       (if (< val-1 val-2)
	   (let ((thresh-val (- val-2 inc-by)))
	     (if (< value thresh-val)
		 (s-value parent :value (+ value inc-by))
		 (s-value parent :value val-2)))
	   (let ((thresh-val (+ val-2 inc-by)))
	     (if (> value thresh-val)
		 (s-value parent :value (- value inc-by))
		 (s-value parent :value val-2)))))

      ;; there is no min
      ((and (not val-1) val-2)
       (let ((thresh-val (- val-2 inc-by)))
	 (if (< value thresh-val)
	     (s-value parent :value (+ value inc-by))
	     (s-value parent :value val-2))))

      ;; there is no max
      (t (s-value parent :value (+ value inc-by))))))

(defun VAL-2-WHEEL-FN (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let* ((obj (g-value interactor :operates-on))
	 (val-1 (g-value obj :val-1))
	 (val-2 (g-value obj :val-2))
	 (value (g-value obj :value))
	 (inc-by (g-value interactor :inc-by)))
    (cond

      ;; there is a max and a min
      ((and val-1 val-2)
       (if (< val-1 val-2)
	   (let ((thresh-val (- val-2 inc-by)))
	     (if (< value thresh-val)
		 (s-value obj :value (+ value inc-by))
		 (s-value obj :value val-2)))
	   (let ((thresh-val (+ val-2 inc-by)))
	     (if (> value thresh-val)
		 (s-value obj :value (- value inc-by))
		 (s-value obj :value val-2)))))

      ;; there is no min
      ((and (not val-1) val-2)
       (let ((thresh-val (- val-2 inc-by)))
	 (if (< value thresh-val)
	     (s-value obj :value (+ value inc-by))
	     (s-value obj :value val-2))))

      ;; there is no max
      (t (s-value obj :value (+ value inc-by))))))


(create-instance 'TRILL-INTER inter:Button-Interactor
  (:active (o-formula (and (gvl :operates-on :visible)
			   (gvl :window)
			   (gvl :operates-on :parent :scroll-p))))
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:timer-repeat-p T)
  (:start-event :leftdown)
  (:start-where (o-formula (list :in-box (gvl :operates-on :frame))))
  (:extra-function #'VAL-1-FN)
  (:final-function #'(lambda (interactor obj)
		       (kr-send interactor :extra-function
				interactor obj)
		       (kr-send (g-value interactor :operates-on :parent)
				:selection-function
				(g-value interactor :operates-on :parent)
				(g-value interactor :operates-on :parent
					 :value)))))



;;  FRAME FOR TRILL BOXES
;;
(create-instance 'TRILL-FRAME opal:rectangle
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:width (o-formula (gv (kr-path 0 :parent) :width)))
   (:height (o-formula (gv (kr-path 0 :parent) :height)))
   (:visible (o-formula (gv (kr-path 0 :parent) :visible))))



;;  BACKGROUND INDICATOR MOVES IN
;;
(create-instance 'BOUND-BOX opal:rectangle
   (:left (o-formula (gv (kr-path 0 :parent) :bound-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :bound-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :bound-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :bound-height))))



;;  INCDICATOR TEXT
;;
(create-instance 'INDICATOR-TEXT opal:text
   (:left (o-formula (- (+ (gv-fixnum (kr-path 0 :parent :indicator) :left)
			   (floor (gv-fixnum (kr-path 0 :parent :indicator) :width) 2))
			(floor (gvl-fixnum :width) 2))))
   (:top (o-formula (- (+ (gv-fixnum (kr-path 0 :parent :indicator) :top)
			  (floor (gv-fixnum (kr-path 0 :parent :indicator) :height) 2))
		       (floor (gvl-fixnum :height) 2))))
   (:string (o-formula (format NIL (gv (kr-path 0 :parent) :format-string)
			       (gv (kr-path 0 :parent) :value))))
   (:font (o-formula (gv (kr-path 0 :parent) :indicator-font)))
   (:visible (o-formula (and (gv (kr-path 0 :parent) :indicator-text-p)
			     (gv (kr-path 0 :parent) :scroll-p)))))


;; INTERACTORS TO MOVE INDICATOR WITH MOUSE
;;
(defun SLIDE-SEL-FN (interactor obj points)
  (call-prototype-method interactor obj points)
  (when (not (g-value interactor :operates-on :int-feedback-p))
    (slide-final-fn interactor obj points)))

(defun SLIDE-FINAL-FN (interactor obj &optional points)
  ;; Must keep points as an optional parameter because this function is used
  ;; as the final-function in several interactors.
  (declare (ignore obj points))
  (kr-send (g-value interactor :operates-on)
	   :selection-function
	   (g-value interactor :operates-on)
	   (g-value interactor :operates-on :value)))


(create-instance 'SLIDE-INTER inter:Move-Grow-Interactor
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:active (o-formula (and (gvl :window) (gvl :operates-on :scroll-p))))
   (:start-where (o-formula (list :in-box (gvl :operates-on :indicator))))
   (:running-where (o-formula (list :in-box (gvl :operates-on :bounding-area))))
   (:outside NIL)
   (:obj-to-be-moved (o-formula (gvl :operates-on :indicator)))
   (:feedback-obj (o-formula (if (gvl :operates-on :int-feedback-p)
				(gvl :operates-on :int-feedback)
				(gvl :operates-on :indicator))))
   (:waiting-priority inter:high-priority-level)
   (:grow-p NIL)
   (:start-action #'SLIDE-SEL-FN)
   (:running-action #'SLIDE-SEL-FN)
   (:final-function #'SLIDE-FINAL-FN))


(create-instance 'JUMP-INTER inter:Move-Grow-Interactor
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:active (o-formula (and (gvl :window) (gvl :operates-on :scroll-p))))
   (:start-event :leftdown)
   (:start-where (o-formula (list :in-box (gvl :operates-on :bounding-area))))
   (:running-where (o-formula (list :in-box (gvl :operates-on :bounding-area))))
   (:outside NIL)
   (:obj-to-be-moved (o-formula (gvl :operates-on :indicator)))
   (:feedback-obj (o-formula (gvl :operates-on :indicator)))
   (:attach-point :n)
   (:grow-p NIL)
   (:final-function #'SLIDE-FINAL-FN))

;; The WHEEL-INTER is like a cross between the JUMP-INTER and the TRILL-INTER.
;; The main point is that it uses the scroll wheel.
(create-instance 'WHEEL-INTER inter:Scroll-Wheel-Interactor
   (:active (o-formula (and (gvl :window) (gvl :operates-on :scroll-p))))
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula (list :in-box (gvl :operates-on :bounding-area))))
   (:inc-by (o-formula (gv (kr-path 0 :operates-on) :scr-incr)))
   (:running-where (o-formula (list :in-box (gvl :operates-on :bounding-area))))
   (:extra-function #'VAL-1-WHEEL-FN)
   (:final-function #'(lambda (interactor obj)
			(kr-send interactor :extra-function
				 interactor obj)
			(slide-final-fn interactor obj))))

(create-instance 'WHEEL-UP-INTER wheel-inter
  (:start-event :upscrollup))

(create-instance 'WHEEL-DOWN-INTER wheel-inter
  (:start-event :downscrollup)
  (:extra-function #'VAL-2-WHEEL-FN))

;;  Tell the world that GAD-scroll-parts has been loaded
;;
(setf (get :garnet-modules :GAD-scroll-parts) T)

;;  All other dependent "parts" modules must be reloaded
;;
(setf (get :garnet-modules :GAD-slider-parts) NIL)
(setf (get :garnet-modules :GAD-h-boxes) NIL)
(setf (get :garnet-modules :GAD-v-boxes) NIL)
