
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

(defun draw-on-window (win x y value)
  (let* ((x-pixmap (gv win :buffer))
	 (pixmap-array (xlib:get-raw-image
			x-pixmap
			:x 0
			:y 0
			:width (xlib:drawable-width x-pixmap)
			:height (xlib:drawable-height x-pixmap)
			:format :z-pixmap)))
    (setf (aref pixmap-array (compute-index x y width height)) value)
    (xlib:put-raw-image x-pixmap
			(xlib:create-gcontext :drawable x-pixmap)
			pixmap-array
			:depth (xlib:drawable-depth x-pixmap)
			:x 0
			:y 0
			:width (xlib:drawable-width x-pixmap)
			:height (xlib:drawable-height x-pixmap)
			:format :z-pixmap)
    (opal:update-all top-win)))

(defun run-draw-on-window ()
  (draw-on-window top-win 7 7 3))

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
       (:width 300)
       (:height 200)
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
      (:left 5)
      (:top 168)
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
