
;; MOP classes
;; standard-object
;; funcallable-standard-object
;; metaobject
;; generic-function
;; standard-generic-function
;; method
;; standard-method
;; standard-accessor-method
;; standard-reader-method
;; standard-writer-method
;; method-combination
;; slot-definition
;; direct-slot-definition
;; effective-slot-definition
;; standard-slot-definition
;; standard-direct-slot-definition
;; standard-effective-slot-definition
;; specializer
;; eql-specializer
;; class
;; build-in-clas
;; forward-referencedf-class
;; standard-class-funcallabel-standard-class

;; functions and their user macro correspondence
;; ensure class (defclass)
;; ensure-generic-function (defmethod)
;; add-method (defmethod)
;; make-method-lambda
;; slot-boundp-using-class
;; slot-makeunbound-using-class
;; slot value-using-call




(ql:quickload :closer-mop)

(defpackage :pde-clx
  (:documentation "package for all our CLX 'programmed develop environment' tools")
  (:use :cl :closer-mop)
  (:shadowing-import-from :closer-mop :defmethod :defgeneric :standard-generic-function))

(in-package :pde-clx)

(defclass pde-clx ()
  ((introspection-methods :initform '())
   clx-classes))

(defparameter *pde-clx* (make-instance 'pde-clx)
  "Entrypoint to the development environment.  For a list of functions
          evaluate this special variable in the repl.")

(defun create-anonymous-class ()
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
		 :direct-default-initargs '()))
;; some examples
;; fare-mop

(defclass test-class ()
    (slot1 slot2 slot3))




;; Creating a custom class not based on standard class


class-direct-default-initargs
class-direct-slots
class-direct-superclasses
documentation
class-name


;; Create a standard class object that generates objects with a
;; superclass of class
(defparameter base-class-metaobject-class
  (make-instance 'standard-class
		 :direct-default-initargs '()
		 :direct-slots '()
		 :direct-superclasses (list (find-class 'class))
		 :documentation "This class is not based on standard class"))

;; Create an object with a superclass of class.  Note, however this
;; class object is not instantiable, because make-instance has not
;; been specialized to handle it, i.e. base-class-metaobject-class is
;; of type standard-class. But base-class of (anonymous) type
;; base-class-metaobject-class which has no specializers.

(defparameter base-class
  (make-instance base-class-metaobject-class))

(defun name-anonymous-class (class name)
  (setf (class-name class) name)
  (setf (find-class name) class))

(defun run-name-anonymous-class ()
  (name-anonymous-class
   base-class-metaobject-class
   'base-class-metaobject-class))

(defmethod make-instance ((c base-class-metaobject-class) &rest initargs)
  (declare (ignore initargs))
  (make-instance 'standard-object))

;; create an objects which inherits from class

(make-instance (make-instance 'class))


(defclass display-metaclass ()
    ((introspection-methods :initform '())
     clx-classes))


(defparameter display-class
  (let ((class (make-instance
		'standard-class)))
    (finalize-inheritance class)
    (make-instance
     'standard-method :function (lambda () ))
    class))






(defparameter clx-app-class
  (let ((class (make-instance
		'standard-class
		:direct-slots '((:name display-class)
				(:name screen-class)
				(:name window-class)))))

    (finalize-inheritance class)
    class))


(defun mop-add-slot (class)
  (

(defun run-ensure-class ()
  (ensure-class 'display
		:direct-slots (list (list :name 'altitude
					  :initform '0
					  :initfunction (lambda () 0)))
		:direct-default-initargs '()))




(defun run-compute-slots ()
    (format t "~s~%" (compute-slots (find-class  'test-class)))
;;;    (class-of object
    )

(defun run-add-direct-method ()
  ;; tbd
  )

(defun run-allocate-instance ()
  (allocate-instance (find-class 'test-class)))


(defun compute-function (unqualified-symbol-name)
  (symbol-function (find-symbol (symbol-name unqualified-symbol-name))))

(defun run-finalize-class ()
  (readers-for-class-meta-objects clx-app))

(defun readers-for-class-meta-objects (class)
  (let ((class-meta-object-readers '(
				     class-default-initargs
				     class-direct-default-initargs
				     class-direct-slots
				     class-direct-subclasses
				     class-direct-superclasses
				     class-finalized-p
				     class-name
;;				     class-precedence-list
;;				     class-prototype
;;				     class-slots
				     )))
    (dolist (f class-meta-object-readers)
      (format t "Property ~s: ~s~%" f (funcall (compute-function f) class)))))


(defun run-readers-for-class-meta-objects ()
  (readers-for-class-meta-objects (find-class 'test-class)))



	      ;; (funcall (function f) class)))))
