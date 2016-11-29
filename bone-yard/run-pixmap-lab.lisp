

(defpackage :pixmap-lab
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(in-package :pixmap-lab)





(defun draw-triangle-on-window ()
  (do-go)
  (let ((my-array
  (dotimes (i (* 50 50))
    (gem::draw-on-window
     top-win
     i
     (list
      (row-major-aref my-array (* i 3))
      (row-major-aref my-array (+ (* i 3) 1))
      (row-major-aref my-array (+ (* i 3) 2))))))


(defun draw-on-pixmap (x y value)
  (let* ((xlib-image (g-value pixmap :image))
	 (pixmap-array (xlib:image-z-pixarray xlib-image)))
    (setf (aref pixmap-array x y) value)
    (setf (xlib:image-z-pixarray xlib-image) pixmap-array)
    (s-value pixmap :image xlib-image)
    (opal:update-all top-win)))


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

(defun run-draw-on-window ()
  (let* ((x-win (gem::get-x-window-drawable top-win))
	(x-win-width (xlib:drawable-width x-win))
	(x-win-height (xlib:drawable-height x-win)))
    (dotimes (i 150)
      (gem::draw-on-window
       top-win
       (translate-to-z-index i i x-win-width x-win-height)
       '(255 0 0)))))

(defun translate-to-z-index (x y width height)
  (let* ((row (* y width)))
    (+ row x)))




(defun do-go (&key dont-enter-main-event-loop (double-buffered-p t))
  (create-instance 'top-win inter:interactor-window
    (:left 500)
       (:top 500)
       (:width 50)
       (:height 50)
       (:title "GARNET Animator Demo")
       (:icon-title "Animator"))
  (let ((agg (create-instance NIL opal:aggregate)))
    (s-value top-win :aggregate agg)
    (opal:update top-win)))
