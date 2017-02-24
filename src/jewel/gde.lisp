


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
  (:use :cl :closer-mop))

(in-package :pde-clx)

(defclass pde-clx ()
  ((introspection-methods :initform '())
   clx-classes))

(defparameter *pde-clx* (make-instance 'pde-clx)
  "Entrypoint to the development environment.  For a list of functions
          evaluate this special variable in the repl.")



;; some examples
;; fare-mop

(defclass test-class ()
    (slot1 slot2 slot3))

(defun run-compute-slots ()
    (compute-slots (find-class  'test-class))
    (class-of object))


(defun run-add-direct-method ()
  ;; tbd
  )

(defun run-allocate-instance ()
  (allocate-instance (find-class 'test-class)))

(defun run-readers-for-class-meta-objects ()
  (let ((class (find-class 'test-class))
	(class-meta-object-readers '(class-default-initargs
				     class-direct-default-init-args
				     class-direct-slots
				     class-direct-subclasses
				     class-direct-superclasses
				     class-finalized-p
				     class-name
				     class-precedence-list
				     class-prototype
				     class-slots)))
    (dolist (f class-meta-object-readers)
      (funcall (function f)
