;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

(in-package :gem)

(defparameter agg nil)

(defun get-x-window-drawable (win)
  (gv win :drawable))

;;;;;(defparameter

(defun draw-on-window (win x value)
  (let* ((pixmap-array (xlib:get-raw-image
			(get-x-window-drawable win)
			:x 0
			:y 0
			:width (xlib:drawable-width (get-x-window-drawable win))
			:height (xlib:drawable-height (get-x-window-drawable win))
			:format :z-pixmap)))
    (setf (aref pixmap-array (* x 4)) (first value))
    (setf (aref pixmap-array (+ (* x 4) 1)) (second value))
    (setf (aref pixmap-array (+ (* x 4) 2)) (third value))
    (xlib:put-raw-image (gv win :drawable)
			(xlib:create-gcontext :drawable (get-x-window-drawable win))
			pixmap-array
			:depth (xlib:drawable-depth (get-x-window-drawable win))
			:x 0
			:y 0
			:width (xlib:drawable-width (get-x-window-drawable win))
			:height (xlib:drawable-height (get-x-window-drawable win))
			:format :z-pixmap)
    (xlib:display-force-output (gem::the-display win))))

(defun transfer-surface-window (win cl-vector-image)
  (let* ((height (gv win :height))
	 (width (gv win :width)))
    (dotimes (i (* height width))
      (gem::draw-on-window
       win
       i
       (list
	(row-major-aref cl-vector-image (* i 3))
	(row-major-aref cl-vector-image (+ (* i 3) 1))
	(row-major-aref cl-vector-image (+ (* i 3) 2)))))))

(defun create-surface  (height width background-rgb)
  (let ((state (aa:make-state))
	(image (aa-misc:make-image height width background-rgb)))
  (values state image)))

(defun vector-create-polygon-on-surface (height width background-rgb
					 forground-rgb sides)
  (multiple-value-bind (state image) (create-surface height width background-rgb)
    (dolist (side sides)
      (apply #'aa:line-f `(,state ,@side)))
    (aa:cells-sweep state (aa-misc:image-put-pixel image forground-rgb)) ; render it
    (aa-misc:show-image image)
    image))
