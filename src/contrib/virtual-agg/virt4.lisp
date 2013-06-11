(declaim (optimize (speed 3) (safety 0)))

(declaim (fixnum *xsize*))
(defvar *xsize* 1000)
(declaim (fixnum *ysize*))
(defvar *ysize* 900)

(defparameter *point-count* 500000)
(defvar *win*)
(defvar *win-virt-agg*)


(defstruct point-item
  (x 0 :type (integer 0 1024))
  (y 0 :type (integer 0 1024))
  color)

(defvar *item-array* (make-array 1 :element-type 'point-item :adjustable t))

(create-instance 'my-point opal:rectangle
  (:left (o-formula (point-item-x (gvl :item-values))))
  (:top (o-formula (point-item-y (gvl :item-values))))
  (:line-style nil)
  (:filling-style (o-formula (point-item-color (gvl :item-values))))
  (:width 1)
  (:height 1))


(defun pick-color (n)
  (declare (fixnum n))
  (case n
    (0 opal:red-fill)
    (1 opal:yellow-fill)
    (2 opal:green-fill)
    (3 opal:blue-fill)
    (4 opal:purple-fill)
    (5 opal:orange-fill)
    (6 opal:cyan-fill)))


(defun add-new-point (x y &optional color)
  (declare (fixnum x y))
  (opal:add-item *win-virt-agg*
		 (make-point-item :x x :y y :color (pick-color (or color (random 7))))))


(defun startup ()
  (setf *win* (create-instance nil inter:interactor-window
		(:title "Points")
		(:left 40) (:top 40)
		(:width *xsize*) (:height *ysize*)
		(:double-buffered-p t)
		(:aggregate (create-instance 'agg opal:aggregate))
		(:foreground-color opal:blue)
		(:background-color opal:black)))

  (setf (aref *item-array* 0)
	(make-point-item
	 :x (the (integer 0 1024) (random *xsize*))
	 :y (the (integer 0 1024) (random *ysize*))
	 :color (pick-color (random 7))))

  (setq *win-virt-agg*
	(create-instance nil opal:virtual-aggregate
			 (:item-prototype my-point)
			 (:item-array *item-array*)
			 (:point-in-item #'(lambda (virtual-aggregate item-values x y)
					     (declare (fixnum x y))
					     (declare (ignore virtual-aggregate))
					     (let* ((item-x (point-item-x item-values))
						    (item-y (point-item-y item-values)))
					       (declare (fixnum item-x item-y))
					       (and (= x item-x) (= y item-y)))))))
  (opal:add-component agg *win-virt-agg*)
  (opal:update *win*)
  (fun)
  )
    

(defun xform (x y)
  (declare (single-float x y))
  (let ((new-x (- (* y (+ (sin (* 0.7 x)) 1.0)) (* 1.2 (sqrt (abs x)))))
	(new-y (- 0.21 x)))
    (declare (single-float new-x new-y))
    (values new-x new-y)))


(defun fun ()
  (labels ((draw-ppict (count x y hw hh)
	     "Recursively draw pretty picture"
	     (declare (single-float x y hw hh)
		      (fixnum count))
	     (unless (zerop count)
	       (let ((xf (truncate (* (+ 1.0 x) hw ))) ;These lines center the picture
		     (yf (truncate (* (+ 0.7 y) hh ))))
		 (declare (fixnum xf yf))
		 (add-new-point xf yf)
		 (when (zerop (mod count 1000))
		   (opal:update *win*))
		 (multiple-value-setq (x y) (xform x y))
		 (draw-ppict (1- count) x y hw hh)))))
    (draw-ppict *point-count* 0.4 0.5 (* *xsize* 0.5) (* *ysize* 0.5)))
  (opal:update *win* t))

