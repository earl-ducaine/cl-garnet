(declaim (optimize (speed 3) (safety 0)))

(declaim (fixnum *xsize*))
(defvar *xsize* 1000)
(declaim (fixnum *ysize*))
(defvar *ysize* 900)

(defparameter *point-count* 20000)

(defvar *win* nil)
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
		 (make-point-item :x x :y y :color (pick-color (or color (random 7)))))
  (opal:update *win*))


(defun startup ()
  (if *win*
      (clear-virtual-aggregate)
      (setf *win* (create-instance nil inter:interactor-window
		    (:title "Points")
		    (:left 40) (:top 40)
		    (:width *xsize*) (:height *ysize*)
		    (:double-buffered-p t)
		    (:aggregate (create-instance 'agg opal:aggregate))
		    (:foreground-color opal:blue)
		    (:background-color opal:black))))


;    (setq *item-array* (make-array  100 :adjustable t))


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
    
			   

(declaim (fixnum *step*))
(defparameter *step* 5)


(defun clear-virtual-aggregate ()
  (dotimes (n *point-count*)
    (opal:remove-item *win-virt-agg* n))
  (opal:update *win* t))


(declaim (single-float *start-x* *start-y*))
(defparameter *start-x* 3.0)
(defparameter *start-y* 0.0)

(declaim (single-float *a* *b*))
(defparameter *a* 0.01)
(defparameter *b* .96)

(defun f (x)
  (declare (single-float x))
  (let* ((c (- 2 (* 2 *a*)))
	 (x-squared (* x x)))
    (declare (single-float c x-squared))
    (+ (* *a* x)
       (/ (* c x-squared)
	  (+ 1 x-squared)))))


(declaim (single-float *scale-factor*))
(defparameter *scale-factor* 32.0)

(defun fun ()
  (let ((hw (* *xsize* 0.5))
	(hh (* *ysize* 0.5))
	(x *start-x*)
	(y *start-y*)
	(w (f *start-x*))
	(u 0.0)
	(z 0.0))
    (declare (single-float u w x y z))

    (dotimes (n *point-count*)
      (declare (fixnum n))
      (when (> n 100)
	(let ((xf (truncate (+ (* *scale-factor* (+ 1.0 x)) hw)))
	      (yf (truncate (+ (* *scale-factor* (+ 0.7 y)) hh))))
	  (add-new-point xf yf)))

      (setf z x)
      (setf x (+ (* *b* y) w))
      (setf u (* x x))
      (setf w (f x))
      (setf y (- w z))))
  (opal:update *win* t))


