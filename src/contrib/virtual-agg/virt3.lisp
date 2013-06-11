(declaim (optimize (speed 3) (safety 0)))

(declaim (fixnum *xsize*))
(defvar *xsize* 1000)
(declaim (fixnum *ysize*))
(defvar *ysize* 900)

(defvar *point-count* 1000)

(defvar *w*)
(defvar *w-virt-agg*)


(defstruct point-item
  (x 0 :type (integer 0 1024))
  (y 0 :type (integer 0 1024))
  color)

(defvar *item-array* (make-array *point-count* :element-type 'point-item :adjustable t))

(create-instance 'my-point opal:rectangle
  (:left (o-formula (point-item-x (gvl :item-values))))
  (:top (o-formula (point-item-y (gvl :item-values))))
  (:line-style nil)
  (:filling-style (o-formula (point-item-color (gvl :item-values))))
  (:width (1+ (random 3)))
  (:height (1+ (random 3))))


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
  (opal:add-item *w-virt-agg*
		 (make-point-item :x x :y y :color (pick-color (or color (random 7))))))


(defun startup ()
  (setf *w* (create-instance nil inter:interactor-window
			     (:title "Points")
			     (:left 40) (:top 40)
			     (:width *xsize*) (:height *ysize*)
			     (:double-buffered-p t)
			     (:aggregate (create-instance 'agg opal:aggregate))
			     (:foreground-color opal:blue)
			     (:background-color opal:black)))


;    (setq *item-array* (make-array  100 :adjustable t))

    (dotimes (i *point-count*)
      (setf (aref *item-array* i)
	    (make-point-item
	     :x (the (integer 0 1024) (random *xsize*))
	     :y (the (integer 0 1024) (random *ysize*))
	     :color (pick-color (random 7)))))

    (setq *w-virt-agg*
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
    (opal:add-component agg *w-virt-agg*)
    (opal:update *w*)
    (fun)
    )
    
			   

(declaim (fixnum *step*))
(defparameter *step* 5)

(defun do-change (va i n)
  (opal:change-item va i n))

(defun do-update (w)
  (opal:update w))


(defun fun ()
  (dotimes (i 1000000)
    (declare (fixnum i))
    (when (= (mod i 1000) 0)
      (when (= (mod i 100000) 0)
	(room t))
      (format t "~A " i)
      (force-output))
    
    (dotimes (n *point-count*)

      (let ((r1 (truncate (random 10)))
	    (r2 (truncate (random 10))))
	(declare (fixnum n r1 r2))
	(let ((item (aref (g-value *w-virt-agg* :item-array) n)))
	  (declare (point-item item))
	  (let* ((old-x (point-item-x item))
		 (old-y (point-item-y item))
		 (new-x (cond ((>= r1 7) (+ old-x *step*))
			      ((>= r1 4) (- old-x *step*))
			      (t old-x)))
		 (new-y (cond ((>= r2 7) (- old-y *step*))
			      ((>= r2 4) (+ old-y *step*))
			      (t old-y))))
	    (declare (fixnum old-x old-y new-x new-y))
	    (when (or (<= new-x 0)
		      (>= new-x *xsize*))
	      (setf new-x (truncate (random *xsize*))))
	    (when (or (<= new-y 0)
		      (>= new-y *ysize*))
	      (setf new-y (truncate (random *ysize*))))

	    (setf (point-item-x item) new-x)
	    (setf (point-item-y item) new-y))
    
      ;; (opal:change-item *w-virt-agg* (make-rect-item :x new-x :y new-y :color new-color) n)
	  (do-change *w-virt-agg* item n))))
	(do-update *w*)))
;;      (opal:change-item *w-virt-agg* item n)
;;      (opal:update *w*))))
