;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

(in-package :gem)

(defparameter agg nil)

(defun get-x-window-drawable (win)
  (gv win :drawable))

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

(defun vector-draw-triangle-on-array (height width)
  ;; (let ((image (aa-misc:make-image height width #(255 255 255))))
  ;;   (aa-misc:show-image image)
  ;;   image)
  (let ((state (aa:make-state)))       ; create the state
    (aa:line-f state 40 10 46 30)   ; describe the 3 sides
    (aa:line-f state 46 30 10 20)   ; of the triangle
    (aa:line-f state 10 20 40 10)
    (let* ((image (aa-misc:make-image height width #(255 255 255)))
	   (put-pixel (aa-misc:image-put-pixel image #(0 0 0))))
      (aa:cells-sweep state put-pixel) ; render it
      (aa-misc:show-image image)
      image
      )))

;; (defun vector-draw-triangle-on-window ()
;;   (let ((image (aa-misc:make-image 50 50 #(255 255 255))))
;;     (aa-misc:show-image image)
;;     image)
;;   (let ((state (aa:make-state)))       ; create the state
;;     (aa:line-f state 40 10 46 30)   ; describe the 3 sides
;;     (aa:line-f state 46 30 10 20)   ; of the triangle
;;     (aa:line-f state 10 20 40 10)
;;     (let* ((image (aa-misc:make-image 50 50 #(255 255 255)))
;; 	   (put-pixel (aa-misc:image-put-pixel image #(0 0 0))))
;;       (aa:cells-sweep state put-pixel) ; render it
;;       (aa-misc:show-image image)
;;       image
;;       )))
