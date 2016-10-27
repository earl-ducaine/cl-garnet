;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

(defpackage :pixmap-lab
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(in-package :pixmap-lab)
(defparameter agg nil)
(defparameter *top-win* nil)

;;;;(load "src/gem/anti-alias-graphics.lisp")

(defun run-draw-triangle-on-window ()
  (when *top-win*
    (opal:destroy *top-win*))
  (setf *top-win* (create-window 100 110))
    (draw-triangle-on-window *top-win*))

(defparameter *triagle-coordinates*
  '((40 10)
    (10 20)
    (46 30)))

(defun generate-polygon-sides (points)
  ;; closing side.
  (let* ((sides '()))
    (push `( ,@(first (reverse points)) ,@(first points)) sides)
    (reduce (lambda (last-point point)
	      (push `( ,@last-point ,@point) sides)
	      point)
	    points)
    (reverse sides)))

(defun draw-triangle-on-window (win)
  (let* ((height (gv win :height))
	 (width (gv win :width))
	 (cl-vector-image
	  (gem::vector-create-polygon-on-surface
	   height width
	   #(150 200 255)
	   #(30 10 0)
	   (generate-polygon-sides *triagle-coordinates*))))
    (gem::transfer-surface-window win cl-vector-image)))

(defun create-window (height width)
  (let ((top-win (create-instance nil inter:interactor-window
		   (:left 500)
		   (:top 100)
		   (:double-buffered-p t)
		   (:width width)
		   (:height height)
		   (:title "GARNET Animator Demo")
		   (:icon-title "Animator"))))
    (let ((agg (create-instance NIL opal:aggregate)))
;;;      (s-value top-win :aggregate agg)
      (opal:update top-win)
      top-win)))

(defun run-event-loop ()
  (inter:main-event-loop))

;; (defun do-stop ()
;;   ;;(opal:destroy top-win)
;;   )



;; Drawing on the screen using anti eliasing algorithms with
;; transparentcy has the following steps:

;; 1. requires that we get a snapshot of the current window.

;; (get-image <drawable>)

;; (as a zimage), convert the zimage to an array, draw on the array,
;; then replay the window with the new value.
