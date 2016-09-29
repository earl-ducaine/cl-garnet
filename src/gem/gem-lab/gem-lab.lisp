(in-package :gem)

(defun run-all-garnet-windows ()
  (x-all-garnet-windows *default-x-root*))


;;; create window
;;; draw on window
;;; apply bitmap

(defun the-colormap ()
  (first (xlib::installed-colormaps gem::*default-x-root*)))

(defun run-make-color ()
  (xlib:make-color :red 1 :green 1 :blue 1))


(defun run-query-color ()
  (xlib:query-colors (the-colormap) (list (run-make-color))))


(defun alocate-color ()
  ;; get the pixel (device dependant 32 bit value) of a color (device
  ;; independant triplet of reals).
  (xlib:alloc-color the-colormap run-make-color)
