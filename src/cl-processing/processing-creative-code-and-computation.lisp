(in-package #:cl-processing)

;; p32
(defun create-processing-window ()
  (size 400 400)
  (background 255)
  (no-stroke)
  (fill 0)
  (rect (/ *width* 4) (/ *height* 4) (/ *width* 2) (/ *height* 2)))

;; p34
(defun setup ()
  (size 400 400)
  (background 255)
  (no-stroke)
  (let ((color (get-random-color)))
    (fill color)
    (rect (/ *width* 4) (/ *height* 4) (/ *width* 2) (/ *height* 2))))

(defun get-random-color ()
  (random 256))


;; p35
(create-schema 'my-rect
  (:x nil)
  (:y nil)
  (:wdth nil)
  (:ht nil))

(defun create-my-rect (x y wdth ht fill-col stroke-col)
  (create-instance nil my-rect
    (:x x)
    (:y y)
    (:wdth wdth)
    (:ht ht)
    (:fill-col fill-col)
    (:stroke-col (cond
		   ((= stroke-col -1)
		    nil)
		   (t
		    (progn
		      (stroke stroke-col)
		      stroke-col))))))

(define-method :draw my-rect (object)
	       (fill (gv object :fill-col))
	       (rect (gv object :x)
		     (gv object :y)
		     (gv object :wdth)
		     (gv object :ht)))


;; p101
(defun create-processing-window ()
  (size 200 200)
  (background 0)
  (no-stroke)
  (fill-three-chanel 80 220 255)
  (ellipse 100 100 100 100)
  (print "hello Earth"))







  
