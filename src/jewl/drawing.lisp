
(in-package :gem)

(defun x-draw-rectangle (window left top width height function
                         line-style fill-style)
  (declare (fixnum left top width height))
  (if (< width 1)
      (setf width 1))
  (if (< height 1)
      (setf height 1))
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window))
         (thickness (if line-style
                        (max (g-value line-style :line-thickness) 1) 0)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let* ((filling-style-gc (display-info-line-style-gc display-info))
	       (gc (gem-gc-gcontext filling-style-gc))
	       (th2 (* 2 thickness)))
	  (set-filling-style fill-style filling-style-gc gc
			     root-window function)
	  (xlib:draw-rectangle drawable gc
			       (+ left thickness) (+ top thickness)
			       (- width th2) (- height th2)
			       t)))
    (if line-style
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc))
	       (half-thickness (truncate thickness 2)))
	  (set-line-style line-style line-style-gc xlib-gc-line
			  root-window function)
	  (xlib:draw-rectangle drawable xlib-gc-line
			       (+ left half-thickness)
			       (+ top half-thickness)
			       (- width thickness)
			       (- height thickness) NIL)))))




(defun aa-x-draw-rectangle (window left top width height function
                         line-style fill-style)
  (declare (fixnum left top width height))
  (if (< width 1)
      (setf width 1))
  (if (< height 1)
      (setf height 1))
  (let* ((display-info (g-value window :display-info))
         (root-window (display-info-root-window display-info))
         (drawable (the-drawable window))
         (thickness (if line-style
                        (max (g-value line-style :line-thickness) 1) 0)))
    (setf function (get function :x-draw-function))
    (if fill-style
	(let* ((filling-style-gc (display-info-line-style-gc display-info))
	       (gc (gem-gc-gcontext filling-style-gc))
	       (th2 (* 2 thickness)))
	  (set-filling-style fill-style filling-style-gc gc
			     root-window function)
	  (xlib:draw-rectangle drawable gc
			       (+ left thickness) (+ top thickness)
			       (- width th2) (- height th2)
			       t)))
    (if line-style
	(let* ((line-style-gc (display-info-line-style-gc display-info))
	       (xlib-gc-line (gem-gc-gcontext line-style-gc))
	       (half-thickness (truncate thickness 2)))
	  (set-line-style line-style line-style-gc xlib-gc-line
			  root-window function)
	  (xlib:draw-rectangle drawable xlib-gc-line
			       (+ left half-thickness)
			       (+ top half-thickness)
			       (- width thickness)
			       (- height thickness) NIL)))))
