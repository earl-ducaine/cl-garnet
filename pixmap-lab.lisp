

;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

(defpackage :pixmap-lab
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(in-package :pixmap-lab)

(defparameter pixmapfilename "eye")


(defparameter *pixmap* (opal:read-xpm-file
			(merge-pathnames
			 "eye1.xpm"
			 common-lisp-user::garnet-pixmap-pathname)))



(defparameter *pixmap-array* nil)

(defun draw-on-pixmap ()
  )


(defparameter agg nil)




(defun draw-on-pixmap (x y value)
  (let* ((xlib-image (g-value pixmap :image))
	 (pixmap-array (xlib:image-z-pixarray xlib-image)))
    (setf (aref pixmap-array x y) value)
    (setf (xlib:image-z-pixarray xlib-image) pixmap-array)
    (s-value pixmap :image xlib-image)
    ;;(opal:remove-component agg pixmap)
    ;; (create-instance 'pixmap opal:pixmap
    ;;   (:left 5)
    ;;   (:top 168)
    ;;   (:count 0)
    ;;   (:image (o-formula pixmap)))
    ;;(opal:add-components agg pixmap)
    (opal:update-all top-win)))

(defun get-x-window-drawable (win)
  (gv top-win :drawable))


(defun translate-to-z-index (x y width height)
  (let* ((row (* y width)))
    (+ row x)))


(defun draw-triangle-on-window ()
  (dotimes (i (* 50 50))
    (draw-on-window
     top-win
     i
     (list
      (row-major-aref my-array (* i 3))
      (row-major-aref my-array (+ (* i 3) 1))
      (row-major-aref my-array (+ (* i 3) 2))))))






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
    (xlib:put-raw-image (gv top-win :drawable)
			(xlib:create-gcontext :drawable (get-x-window-drawable win))
			pixmap-array
			:depth (xlib:drawable-depth (get-x-window-drawable win))
			:x 0
			:y 0
			:width (xlib:drawable-width (get-x-window-drawable win))
			:height (xlib:drawable-height (get-x-window-drawable win))
			:format :z-pixmap)
    (xlib:display-force-output (gem::the-display win))))

(defun run-draw-on-window ()
  (let* ((x-win (get-x-window-drawable top-win))
	(x-win-width (xlib:drawable-width x-win))
	(x-win-height (xlib:drawable-height x-win)))
    (dotimes (i 150)
      (draw-on-window
       top-win
       (translate-to-z-index i i x-win-width x-win-height)
       '(255 0 0)))))



(defun vector-draw-triangle-on-window ()
  (let ((image (aa-misc:make-image 50 50 #(255 255 255))))
    (aa-misc:show-image image)
    image)
  (let ((state (aa:make-state)))       ; create the state
    (aa:line-f state 40 10 46 30)   ; describe the 3 sides
    (aa:line-f state 46 30 10 20)   ; of the triangle
    (aa:line-f state 10 20 40 10)
    (let* ((image (aa-misc:make-image 50 50 #(255 255 255)))
	   (put-pixel (aa-misc:image-put-pixel image #(0 0 0))))
      (aa:cells-sweep state put-pixel) ; render it
      (aa-misc:show-image image)
      image
      )))





;; (defun run-draw-on-window ()
;;   (draw-on-window top-win 7 7 3))

(defun run-draw-on-pixmap ()
  (draw-on-pixmap 1 1 2)
  (draw-on-pixmap 2 2 2)
  (draw-on-pixmap 3 3 3)
  (draw-on-pixmap 4 4 3)
  (draw-on-pixmap 5 5 3)
  (draw-on-pixmap 6 6 3)
  (draw-on-pixmap 7 7 3)
  (draw-on-pixmap 7 7 3))

(defun do-go (&key dont-enter-main-event-loop (double-buffered-p t))
    (create-instance 'TOP-WIN inter:interactor-window
      (:left 500)
      (:top 100)
       (:double-buffered-p double-buffered-p)
       (:width 50)
       (:height 50)
       (:title "GARNET Animator Demo")
       (:icon-title "Animator"))
    (s-value top-win :aggregate
	     (setq agg (create-instance NIL opal:aggregate)))

    ;; If we get clobbered by the window manager, let the demos
    ;; controller know (if it's there).
    (when (fboundp 'common-lisp-user::garnet-note-quitted)
      (pushnew
       (lambda (win)
	   (common-lisp-user::garnet-note-quitted "demo-animator"))
       (g-value top-win :destroy-hooks)))

    (create-instance 'pixmap opal:pixmap
      (:left 0)
      (:top 0)
      (:count 0)
      (:image (o-formula *pixmap*)))

    (opal:add-components agg pixmap)
    (opal:update top-win))


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
