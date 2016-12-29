(defpackage :processing
  (:use :cl))

(in-package :processing)

(defparameter *triagle-coordinates*
  '((100 30)
    (10 40)
    (46 100)
    (100 145)))


(defun proc (name lambda-list body)
  (declare (ignore name)
	   (ignore body)
	   (ignore lambda-list))
  (let ((processing-lambda (lambda ())))
    (declare (ignore processing-lambda))
    ))

(defvar *top-win*)



;;make-thread (function &key name)
;;bordeaux-threads

;;(iterate:iterate



;; Currently no support for defun style declarations.
(defun parse-processing-proc (body doc-string-allowed &optional silent)
  (declare (ignore silent))
  (flet ((doc-string-p (x remaining-forms doc)
           (and (stringp x) doc-string-allowed
                remaining-forms
                (if doc
                    (error "duplicate doc string ~S" x)
                    t))))
    (let ((forms body)
	  (decls (list nil))
	  (doc nil))
      (let ((decls decls))
	(declare (ignore decls))
        (loop (when (endp forms) (return))
	   (let ((form (first forms)))
	     (cond ((doc-string-p form (rest forms) doc)
		    (setq doc form))
		   (t
		    (return))))
	   (setq forms (rest forms))))
      (values forms doc))))

(defmacro proc-macro (name lambda-list &body body)
  (multiple-value-bind (forms cls doc) (parse-processing-proc body t)
    (declare (ignore cls))
    (let* ((lambda-guts `(,lambda-list
                          ,@(when doc (list doc))
                          ,@body

			  ,@forms))
           (lambda `(lambda ,@lambda-guts))
           (named-lambda `(named-lambda ,name ,@lambda)))
      `(progn
         (defun ',name ,named-lambda ,body)))))

(defun transfer-surface-window (win cl-vector-image)
  (let* ((height (xlib:drawable-height win))
	 (width (xlib:drawable-width win))
	 (pixmap-array (xlib:get-image
			win
			:x 0
			:y 0
			:width width
			:height height
			:format :z-pixmap)))
    (let ((image-z-pixarray (xlib:image-z-pixarray pixmap-array)))
      (dotimes (i (* height width))
	(setf (row-major-aref image-z-pixarray i)
	      (xlib-lab::get-color (row-major-aref cl-vector-image (* i 3))
				   (row-major-aref cl-vector-image (+ (* i 3) 1))
				   (row-major-aref cl-vector-image (+ (* i 3) 2)))))
      (setf (xlib:image-z-pixarray pixmap-array) image-z-pixarray)
      (xlib:put-image  win
		       (xlib:create-gcontext :drawable win)
		       pixmap-array
		       :x 0
		       :y 0
		       :width (xlib:drawable-width win)
		       :height (xlib:drawable-height win))
      (xlib-lab::display-force-output))))

(defun basic-background (win)
  (let* ((height (xlib:drawable-height win))
	 (width (xlib:drawable-width win))
	 (cl-vector-image
	  (xlib-lab::vector-create-polygon-on-surface
	   height width
	   #(150 200 255)
	   #(255 200 150)
	   (xlib-lab::generate-polygon-sides xlib-lab::*triagle-coordinates*))))
    (xlib-lab::transfer-surface-window win cl-vector-image)))

(defun size (width height &optional device)
  "Destroy top window if it exists and create one of the specified size."
  (declare (ignore device))
  (when *top-win*
    (xlib:destroy-window *top-win*)
    (setf *top-win* nil))
  (setf *top-win* (xlib-lab::create-window width height)))


(defun draw-triangle-on-window (win)
  (let* ((height (xlib:drawable-height win))
	 (width (xlib:drawable-width win))
	 (cl-vector-image
	  (xlib-lab::vector-create-polygon-on-surface
	   height width
	   #(150 200 255)
	   #(255 200 150)
	   (xlib-lab::generate-polygon-sides *triagle-coordinates*))))
    (xlib-lab::transfer-surface-window win cl-vector-image)))

(defun run-draw-triangle-on-window ()
  (setf *top-win* (xlib-lab::create-window 400 410))
  (draw-triangle-on-window *top-win*))






;; the draw routine is a loop that runs on a separate thread



(defparameter *frame-queue* '())




(defparameter *timeout-queue* '())



(defun wait (seconds)
  (let ((wait-queue (sb-thread:make-waitqueue))
	(wait-mutex (sb-thread:make-mutex)))
    (sb-thread:with-mutex (wait-mutex)
      (sb-thread:condition-wait wait-queue wait-mutex :timeout seconds))))


(defparameter *recursive-lock* (bt:make-recursive-lock "time-lock"))

;; (defun wait (seconds lock)
;;   (let ((wait-mutex ))
;;     (bt::condition-wait *wait-queue* wait-mutex :timeout seconds)))



(defparameter *timeout-condition-variable* (bt:make-condition-variable))
(defparameter *timeout-condition-lock* (bt:make-lock))

(defparameter *stop-p* nil)


(defparameter *timeout-thread* nil)

(defun run-make-thread ()
  (setf *stop-p* nil)
  (setf *timeout-thread*
	(bt:make-thread (lambda ()
			  (condition-wait
			   *timeout-condition-variable*
			   *timeout-condition-lock*))
			:name "timeout-thread")))


(defun condition-wait (timeout-condition-variable timeout-condition-lock)
  (iter:iter
   (iter:until *stop-p*)
   (bt:with-lock-held (timeout-condition-lock)
     (bt:condition-wait timeout-condition-variable timeout-condition-lock :timeout 5))
   (setf *stop-p* t)))





;; processing event:
;; - Get next scheduled in milliseconds
;; - Set event interval: list, function (taking one paramiter iteration), number
;; - event name
;; - event function to be called

(defclass processing-event ()
  ((event-interval :initarg :event-interval)
   (run-index :initform 0)
   (event-function :initform 0 :initarg :event-function)))

(defmethod get-next-scheduled-interval ((event processing-event))
  (with-slots (event-interval run-index) event
    (etypecase event-interval
      (sequence (first event-interval))
      (number event-interval)
      (function (funcall event-interval run-index)))))

;; (defun run-get-next-scheduled-interval ()
;;   (let ((sequence-event
;; 	 (make-instance 'processing-event
;; 			:event-interval '(1 2 1 2)
;; 			:event-function
;; 			(lambda (self)
;; 			  (format t "" (slot-value event 'event-function))))
;; 	  (make-instance 'processing-event
;; 			 :event-interval '(1 2 1 2)
;; 			 :event-function
;; 			 (lambda (self)
;; 			   (format t "" (slot-value event 'event-function))))
;; 	  ))
;;     (number-event (make-instance 'processing-event))
;;     (function-event (make-instance 'processing-event)))
;;   (setf (slot-value sequence-event 'event-interval) '(1 2 1 2))
;;   (setf (slot-value number-event 'event-interval) 1)
;;   (setf (slot-value function-event 'event-interval)
;; 	(lambda (index)
;; 	  index))
;;   (dolist (event (list sequence-event number-event function-event))
;;     (format t "Next scheduled interval ~s~%"
;; 	    (get-next-scheduled-interval event)))))

;; Events run on a single thread, without preemption.  On account of
;; that they need to return before subsequent events can be run.  It
;; is up to the scheduler to decide what to do if an event can't be
;; run because it was still waiting for a prior event to finish.  But
;; in any case, too many long running events will cause some evenents
;; to not be run when they are required to.
(defmethod run-event ((event processing-event))
  (funcall (slot-value event 'event-function) event))



;;; Next event in milliseconds
(defparameter *next-event* 0)

(defun build-events ()
  (list (make-instance
	 'processing-event
	 :event-interval 1
	 :event-function
	 (lambda (self)
	   (format t "Printing processing-event1: ~s~% "
		   (slot-value self 'run-index))))
	(make-instance
	 'processing-event
	 :event-interval 2
	 :event-function
	 (lambda (self)
	   (format t "Printing processing-event2: ~s~% "
		   (slot-value self 'run-index))))
	(make-instance
	 'processing-event
	 :event-interval 3
	 :event-function
	 (lambda (self)
	   (format t "Printing processing-event3: ~s~% "
		   (slot-value self 'run-index))))))

(defparameter *events* (build-events))

(defparameter *event-queue* '())


(defun insert-at (list index newelt)
  (if (= 0 index)
      (cons newelt list)
      (push newelt (cdr (nthcdr (- index 1) list))))
  list)

(defun schedule-event (event)
  (let ((new-event-queue-item
	 (list
	  (slot-value event 'event-interval)
	  event)))
    (if *event-queue*
	(insert-at
	 *event-queue*
	 (do ((segment *event-queue* (cdr segment))
	      (i 0 (+ 1 i)))
	     ((or (null segment)
		  (> (car (car segment))
		     (car new-event-queue-item)))
	      i))
	 new-event-queue-item)
	(push new-event-queue-item *event-queue*))))


Scheduling steps
(




;; (defparameter event-record
;;   (list (get-next-scheduled-interval obj) obj))

;; (defparameter event-record1
;;   (list (get-next-scheduled-interval obj1) obj1))


(defun run-build-queue ()
  (setf *event-queue* '())
  (setf *event-queue* (append  *event-queue* (list event-record)))
  (setf *event-queue* (append  *event-queue* (list event-record1))))



;; (defparameter processing-event
;;   (make-instance
;;    'processing-event
;;    :event-interval '(1 2 1 2)
;;    :event-function
;;    (lambda (self)
;;      (format t "Printing processing-event: ~s~% " (slot-value event 'run-index)))))
