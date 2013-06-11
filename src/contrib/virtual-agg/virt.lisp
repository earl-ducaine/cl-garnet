(declaim (optimize (speed 3) (safety 0)))

(declaim (fixnum *xsize*))
(defvar *xsize* 1000)
(declaim (fixnum *ysize*))
(defvar *ysize* 900)

(defvar *rect-count* 5000)

(defvar *w*)
(defvar *w-virt-agg*)


(defstruct rect-item
  (x 0 :type (integer 0 1024))
  (y 0 :type (integer 0 1024))
  color)

(defvar *item-array* (make-array *rect-count* :element-type 'rect-item :adjustable t))

(create-instance 'my-rectangle opal:rectangle
  (:left (o-formula (rect-item-x (gvl :item-values))))
  (:top (o-formula (rect-item-y (gvl :item-values))))
  (:filling-style (o-formula (rect-item-color (gvl :item-values))))
  (:width 5)
  (:height 5))


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


(defun add-new-square (x y &optional color)
  (declare (fixnum x y))
  (opal:add-item *w-virt-agg*
		 (make-rect-item :x x :y y :color (pick-color (or color (random 7))))))


(defun startup ()
  (setf *w* (create-instance nil inter:interactor-window
			     (:title "Rectangles")
			     (:left 40) (:top 40)
			     (:width *xsize*) (:height *ysize*)
			     (:double-buffered-p t)
			     (:aggregate (create-instance 'agg opal:aggregate))
			     (:foreground-color opal:blue)
			     (:background-color opal:white)))

    (dotimes (i *rect-count*)
      (setf (aref *item-array* i)
	    (make-rect-item
	     :x (the (integer 0 1024) (random *xsize*))
	     :y (the (integer 0 1024) (random *ysize*))
	     :color (pick-color (random 7)))))

    (setq *w-virt-agg*
	  (create-instance nil opal:virtual-aggregate
			   (:item-prototype my-rectangle)
			   (:item-array *item-array*)
			   (:point-in-item #'(lambda (virtual-aggregate item-values x y)
					       (declare (fixnum x y))
					       (declare (ignore virtual-aggregate))
					       (let* ((min-x (rect-item-x item-values))
						      (min-y (rect-item-y item-values))
						      (max-x (+ min-x 5))
						      (max-y (+ min-y 5)))
						 (declare (fixnum min-x min-y max-x max-y))
						 (and (>= x min-x) (<= x max-x)
						      (>= y min-y) (<= y max-y)))))))
    (opal:add-component agg *w-virt-agg*)
    (opal:update *w*)

    (fun)
    )
			   

(declaim (fixnum *step*))
(defparameter *step* 100)


(defun fun ()
  (dotimes (i 1000000)
    (declare (fixnum i))
    (when (= (mod i 1000) 0)
      #- (and)
      (when (= (mod i 100000) 0)
	(room t))
      (format t "~A " i)
      (force-output))
    
    (let ((n (truncate (random *rect-count*)))
	  (r1 (truncate (random 500)))
	  (r2 (truncate (random 500))))
      (declare (fixnum n r1 r2))
      (let ((item (aref (g-value *w-virt-agg* :item-array) n)))
	(declare (rect-item item))
	(let ((new-x (if (> r1 249) (+ (rect-item-x item) *step*) (- (rect-item-x item) *step*)))
	      (new-y (if (> r2 249) (- (rect-item-y item) *step*) (+ (rect-item-y item) *step*))))
	  (declare (fixnum new-x new-y))
	  (when (or (<= new-x 0)
		    (>= new-x *xsize*))
	    (setf new-x (truncate (random *xsize*))))
	  (when (or (<= new-y 0)
		    (>= new-y *ysize*))
	    (setf new-y (truncate (random *ysize*))))


	  (setf (rect-item-x item) new-x)
	  (setf (rect-item-y item) new-y))
    
	(opal:change-item *w-virt-agg* item n)
	(opal:update *w*)))))
