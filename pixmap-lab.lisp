;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

(defpackage :pixmap-lab
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(in-package :pixmap-lab)
(defparameter agg nil)

(load "src/gem/anti-alias-graphics.lisp")

(defun draw-triangle-on-window ()
  (do-go)
  (let ((my-array (vector-draw-triangle-on-window)))
    (dotimes (i (* 50 50))
      (draw-on-window
       top-win
       i
       (list
	(row-major-aref my-array (* i 3))
	(row-major-aref my-array (+ (* i 3) 1))
	(row-major-aref my-array (+ (* i 3) 2)))))))



(defun do-go (&key dont-enter-main-event-loop (double-buffered-p t))
    (create-instance 'TOP-WIN inter:interactor-window
      (:left 500)
      (:top 100)
       (:double-buffered-p double-buffered-p)
       (:width 50)
       (:height 50)
       (:title "GARNET Animator Demo")
       (:icon-title "Animator"))
    (let ((agg (create-instance NIL opal:aggregate)))
      (s-value top-win :aggregate agg)
      (opal:update top-win)))

(defun run-event-loop ()
  (inter:main-event-loop))

(defun do-stop ()
  (opal:destroy top-win))



;; Drawing on the screen using anti eliasing algorithms with
;; transparentcy has the following steps:

;; 1. requires that we get a snapshot of the current window.

;; (get-image <drawable>)

;; (as a zimage), convert the zimage to an array, draw on the array,
;; then replay the window with the new value.
