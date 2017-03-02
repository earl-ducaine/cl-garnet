(defclass clx-app-factory ()
  (type
   *instantiated-instances*))


(make-instance 'standard-class
		   :direct-superclasses (list (find-class 'plane))
		   :direct-slots `((:name x
					  :initform 0
					  :initfunction (lambda () 0)
					  :initargs (:x)
					  :readers (position-x)
					  :writers ((setf position-x)))
				   (:name y
					  :initform 0
					  :initfunction (lambda () 0)
					  :initargs (:y)
					  :readers (position-y)
					  :writers ((setf position-y))))
		   :direct-default-initargs '())


(defparameter app-factory-class  (make-instance 'standard-class))

(defmethod generate-app-factory ()
  (ensure-class 'plane :direct-slots (list '(:name slot1)
					   '(:name slot2))
                :direct-superclasses '(t))













(defparameter app-factory-class  (make-instance 'standard-class))





  (ensure-class 'plane
		:direct-slots (list (list :name 'altitude
					   :initform '0
					   :initfunction (lambda () 0 )
					   :readers '(plane-altitude)
					   :writers '((setf plane altitude)))
				     (list ':name 'speed))
		:direct-default-initargs (list (list ':engine
						      '*jet*
						      #'(lambda () *jet*))))
