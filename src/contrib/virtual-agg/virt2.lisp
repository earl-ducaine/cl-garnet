(declaim (optimize (speed 3) (safety 0)))

(declaim (fixnum *xsize*))
(defvar *xsize* 1000)
(declaim (fixnum *ysize*))
(defvar *ysize* 900)

(defvar *step* 5)

(defparameter *rect-count* 4000)

(defvar *w*)
(defvar *w-virt-agg*)


(defstruct rect-item
  (x 0 :type (integer 0 1024))
  (y 0 :type (integer 0 1024))
  color)

(defvar *item-array* (make-array *rect-count* :element-type 'rect-item :adjustable t))
(defvar *inter-array* (make-array *rect-count* :initial-element nil :adjustable t))

(kr:create-instance 'my-rectangle opal:rectangle
  (:left (kr:o-formula (rect-item-x (kr:gvl :item-values))))
  (:top (kr:o-formula (rect-item-y (kr:gvl :item-values))))
;;  (:line-style nil)
  (:filling-style (kr:o-formula (rect-item-color (kr:gvl :item-values))))
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


(declaim (inline do-change))
(defun do-change (va i n)
  (opal:change-item va i n))

(defun startup ()
  (setf *w* (kr:create-instance nil inter:interactor-window
			     (:title "Rectangles")
			     (:left 40) (:top 40)
			     (:width *xsize*) (:height *ysize*)
			     ;; (:double-buffered-p t)
			     (:aggregate (kr:create-instance 'agg opal:aggregate))
			     (:foreground-color opal:blue)
			     (:background-color opal:white)))

  (dotimes (i *rect-count*)
    (setf (aref *item-array* i)
	  (make-rect-item
	   :x (the (integer 0 1024) (random *xsize*))
	   :y (the (integer 0 1024) (random *ysize*))
	   :color (pick-color (random 7))))

    (setf (aref *inter-array* i)
	  (kr:create-instance nil inter:animator-interactor
			   (:window *w*)
			   (:timer-repeat-wait (+ .8 (random .8)))
			   (:this-item i)
			   (:timer-handler
			    #'(lambda(inter)
				(let* ((index (kr:g-value inter :this-item))
				       (item (aref *item-array* index))
				       (old-x (rect-item-x item))
				       (old-y (rect-item-y item))
				       (step-x (- (random 65) 32))
				       ;;(step-x (random 20))
				       (step-y (- (random 65) 32))
				       ;;(step-y (random 20))
				       (new-x (+ old-x step-x))
				       (new-y (+ old-y step-y)))
				  (when (or (<= new-x 0)
					    (>= new-x *xsize*))
				    (setf new-x (truncate (random *xsize*))))
				  (when (or (<= new-y 0)
					    (>= new-y *ysize*))
				    (setf new-y (truncate (random *ysize*))))
			    
				  (setf (rect-item-x item) new-x)
				  (setf (rect-item-y item) new-y)
				  (do-change *w-virt-agg* item index)))))))

  (setq *w-virt-agg*
	(kr:create-instance nil opal:virtual-aggregate
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

  (dotimes (i *rect-count*)
    (sleep .001)
    (inter:start-animator (aref *inter-array* i))))


(defun start-em ()    
  (dotimes (i *rect-count*)
    (inter:start-animator (aref *inter-array* i))))

(defun stop-em ()    
  (dotimes (i *rect-count*)
    (inter:stop-animator (aref *inter-array* i))))


