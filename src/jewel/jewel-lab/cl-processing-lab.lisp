(defpackage :processing
  (:use :cl))

(in-package :processing)

(defparameter *triagle-coordinates*
  '((100 30)
    (10 40)
    (46 100)
    (100 145)))

(defparameter *triagle-coordinates2*
  '((100 30)
    (10 60)
    (46 100)
    (100 145)))


(defparameter *frames*
  (let  (result)
    (reverse (dotimes (i 10)
	       (push `((100 30)
		       (,(+ 10 (* i 3)) 40)
		       (46 100)
		       (100 145)) result)))
    result))


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

;; draws on existing window
(defun draw-frame-on-window (win frame)
  (let* ((height (xlib:drawable-height win))
	 (width (xlib:drawable-width win))
	 (cl-vector-image
	  (xlib-lab::vector-create-polygon-on-surface
	   height width
	   #(150 200 255)
	   #(255 200 150)
	   (xlib-lab::generate-polygon-sides frame))))
    (xlib-lab::transfer-surface-window win cl-vector-image)))

(defun run-draw-triangle-on-window ()
  (setf *top-win* (xlib-lab::create-window 400 410))
  (draw-triangle-on-window *top-win*))

(defun flatten (path)
  (if (not (listp path))
      (list path)
      (loop for item in path nconc (flatten item))))

(defun path-map-line (path function)
  "Iterate over all the line on the contour of the path."
  (loop with iterator = (paths:path-iterator-segmented path)
     for previous-knot = nil then knot
     for (interpolation knot end-p) = (multiple-value-list (paths:path-iterator-next iterator))
     while knot
     when previous-knot
     do (funcall function previous-knot knot)
     until end-p
     finally (when knot
               (funcall function knot (nth-value 1 (paths:path-iterator-next iterator))))))

(defun paths-bounding-box (paths &optional (scale 1.0))
  (let ((state (aa-bin:make-state))
        min-x max-x
        min-y max-y)
    (flet ((do-line (p1 p2)
             (aa-bin:line-f state
                     (* scale (paths:point-x p1)) (* scale (paths:point-y p1))
                     (* scale (paths:point-x p2)) (* scale (paths:point-y p2))))
           (do-cell (x y alpha)
             (declare (ignore alpha))
             (cond
               (min-x
                (cond
                  ((< x min-x) (setf min-x x))
                  ((> x max-x) (setf max-x x)))
                (cond
                  ((< y min-y) (setf min-y y))
                  ((> y max-y) (setf max-y y))))
               (t
                (setf min-x x
                      max-x x
                      min-y y
                      max-y y)))))
      (loop for path in (flatten paths)
         do (path-map-line path #'do-line))
      (aa-bin:cells-sweep state #'do-cell (lambda (&rest args) (declare (ignore args)))))
    (when min-x
      (values min-x min-y (1+ max-x) (1+ max-y)))))

(defun rasterize-paths (paths image &optional (color #(0 0 0)) (opacity 1.0) (scale 1.0))
  (let ((state (aa:make-state)))
    (flet ((do-line (p1 p2)
             (aa:line-f state
                     (* scale (paths:point-x p1)) (* scale (paths:point-y p1))
                     (* scale (paths:point-x p2)) (* scale (paths:point-y p2)))))
      (loop for path in (flatten paths)
         do (path-map-line path #'do-line)))
    (aa-bin:cells-sweep state (aa-misc:image-put-pixel image color opacity))))

(defun create-graph (graph &key subgraphs (width 800) (height 600) (auto-size t) (scale 1.0)
                     (background #(255 255 255)))
  (when auto-size
    (let (min-x max-x
          min-y max-y)
      (flet ((update-limits (graph)
               (loop for (color . paths) in graph
                  do (multiple-value-bind (x1 y1 x2 y2) (paths-bounding-box paths scale)
                       (when x1
                         (when (or (null min-x) (< x1 min-x)) (setf min-x x1))
                         (when (or (null max-x) (> x2 max-x)) (setf max-x x2))
                         (when (or (null min-y) (< y1 min-y)) (setf min-y y1))
                         (when (or (null max-y) (> y2 max-y)) (setf max-y y2)))))))
        (when graph
          (update-limits graph))
        (when subgraphs
          (mapcar #'update-limits subgraphs)))
      (ecase auto-size
        (:border
         (setf width (max 1 (+ (max 0 min-x) max-x))
               height (max 1 (+ (max 0 min-y) max-y))))
        (t
         (setf width (max 1 max-x)
               height (max 1 max-y))))))
  (let ((image (aa-misc:make-image width height background)))
    (when graph
      (loop for (color . paths) in graph
         do (rasterize-paths paths image color 1.0 scale)))
    (dolist (subgraph subgraphs)
      (loop for (color . paths) in subgraph
         do (rasterize-paths paths image color 0.3 scale)))
    image))


(defun run-tests ()
  (net.tuxee.vectors-doc::test)
  )

(defparameter string-path
  (zpb-ttf:with-font-loader
      (loader (merge-pathnames "src/jewel/jewel-lab/FreeSerifBoldItalic.ttf"
			 (asdf:system-source-directory :jewel)))
    (paths-ttf:paths-from-string loader "Hello World!"
				 :offset (paths:make-point 200 550)
				 :scale-x 0.3
				 :scale-y -0.3)))

(defun run-show-annotated-path ()
    (let ((path (paths:create-path :polygon)))
    (paths:path-reset path (paths:make-point 25 15))
    (paths:path-extend path (paths:make-straight-line) (paths:make-point 250 25))
    (paths:path-extend path (paths:make-bezier-curve (list (paths:make-point 300 40)
                                               (paths:make-point 400 150)
                                               (paths:make-point 200 100)))
                 (paths:make-point 250 250))
    (paths:path-extend path (paths:make-arc 100 200 :x-axis-rotation -0.8)
                 (paths:make-point 25 250))
    (paths:path-extend path (paths:make-catmull-rom (paths:make-point 10 270)
                                        (list (paths:make-point 10 200)
                                              (paths:make-point 40 160)
                                              (paths:make-point 25 120)
                                              (paths:make-point 60 90))
                                        (paths:make-point 70 40))
                 (paths:make-point 55 55))
    (net.tuxee.vectors-doc::show-annotated-path path)))


(defun my-generate-annotated-path (path)
  (let ((annotated-path (paths:path-annotated path)))
    (net.tuxee.vectors-doc::create-graph annotated-path)))

(defun generate-rasterized-path (path)
    (net.tuxee.vectors-doc::create-graph path))

(defun my-show-annotated-path (path)
  (aa-misc:show-image (generate-rasterized-path path)))

(defun generate-swervy-rectangle ()
  (let ((path (paths:create-path :polygon)))
    (paths:path-reset path (paths:make-point 25 15))
    (paths:path-extend path (paths:make-straight-line) (paths:make-point 250 25))
    (paths:path-extend path (paths:make-bezier-curve (list (paths:make-point 300 40)
							   (paths:make-point 400 150)
							   (paths:make-point 200 100)))
		       (paths:make-point 250 250))
    (paths:path-extend path (paths:make-arc 100 200 :x-axis-rotation -0.8)
		       (paths:make-point 25 250))
    (paths:path-extend path (paths:make-catmull-rom (paths:make-point 10 270)
						    (list (paths:make-point 10 200)
							  (paths:make-point 40 160)
							  (paths:make-point 25 120)
							  (paths:make-point 60 90))
						    (paths:make-point 70 40))
		       (paths:make-point 55 55))
    path))


(defun elipse (x-position y-position width height)
  (let* ((rotation 0.2)
	 (path (paths:make-circle-path x-position y-position width height rotation)))
    (setf *path* path)))
  ;;   (format t "path: ~S~%" path)))
  ;; (show-annotated-path path)))


(defun run-elipse ()
  (elipse 100 50 90 40))






;; The idea is to compute next frame based on the 'wall' clock.  A
;; hacky strategy to smooth out jitter is to:
;;
;; 1) always show all the frames with some potentially human
;;    detectible interval between them.
;; 2) catch up if frames are taking more time than scheduled.
;;


;;internal-time-units-per-second
;; (get-internal-real-time)

(defun compute-duration-next-sleep (remaining-frames remaining-time original-time-each-frame)
  (cond ((< original-time-each-frame  (/ remaining-time remaining-frames))
	 ;; reduce the time by 20%
	 (* (/ remaining-time remaining-frames) (/ 4 5)))
	;; other wise schedule remaining time fairly between frames.
	;; Don't worry if original-time-each-fram is greater than this
	;; number.  It just means that in a previous iteration we over
	;; compensated when we reduced by 20%.  The only time
	;; difference would be large and problematic are cases where
	;; there are a low number of frames with a relatively high
	;; fps.  A case we don't care about from a human perception
	;; perspective.
	(t (/ remaining-time remaining-frames))))


(defun run-animate-triangle-on-window ()
  (setf *top-win* (xlib-lab::create-window 400 410))
  (let* ((frame-count 10)
	 (fps 2)
	 (total-time (/ frame-count fps))
	 (original-time-each-frame (/ 1 fps)))
    ;; (format t "frame-count: ~S; fps ~S;  total-time ~S;  original-time-each-frame ~S"
    ;; 	    frame-count
    ;; 	    fps
    ;; 	    total-time
    ;; 	    original-time-each-frame)
    (dotimes (i frame-count)
      (draw-frame-on-window *top-win* (nth i *frames*))
      (sleep (compute-duration-next-sleep
	      (- 360 i)
	      ;; emulate things getting further behind.
	      (- (* fps (- 360 i)) i)
	      original-time-each-frame)))))


;; the draw routine is a loop that runs on a separate thread



(defparameter *frame-queue* '())
(defparameter *timeout-queue* '())

(defun wait (seconds)
  (let ((wait-queue (sb-thread:make-waitqueue))
	(wait-mutex (sb-thread:make-mutex)))
    (sb-thread:with-mutex (wait-mutex)
      (sb-thread:condition-wait wait-queue wait-mutex :timeout seconds))))


(defparameter *recursive-lock* (bordeaux-threads:make-recursive-lock "time-lock"))

(defparameter *timeout-condition-variable* (bordeaux-threads:make-condition-variable))
(defparameter *timeout-condition-lock* (bordeaux-threads:make-lock))
(defparameter *stop-p* nil)
(defparameter *timeout-thread* nil)

(defun run-make-thread ()
  (setf *stop-p* nil)
  (setf *timeout-thread*
	(bordeaux-threads::make-thread (lambda ()
			  (condition-wait
			   *timeout-condition-variable*
			   *timeout-condition-lock*))
			:name "timeout-thread")))

(defun condition-wait (timeout-condition-variable timeout-condition-lock)
  (iter:iter
    (iter:until *stop-p*)
    (bordeaux-threads::with-lock-held (timeout-condition-lock)
      (bordeaux-threads::condition-wait timeout-condition-variable timeout-condition-lock :timeout 5))
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


;; Scheduling steps
;; (




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

(defun my-create-graph-no-annotation (graph &key subgraphs
					      (width 800)
					      (height 600)
					      (auto-size t)
					      (scale 1.0)
					      (background #(255 255 255)))
  (when auto-size
    (let (min-x max-x
		min-y max-y)
      (flet ((update-limits (graph)
               (loop for (color . paths) in graph
                  do (multiple-value-bind (x1 y1 x2 y2) (paths-bounding-box paths scale)
                       (when x1
                         (when (or (null min-x) (< x1 min-x)) (setf min-x x1))
                         (when (or (null max-x) (> x2 max-x)) (setf max-x x2))
                         (when (or (null min-y) (< y1 min-y)) (setf min-y y1))
                         (when (or (null max-y) (> y2 max-y)) (setf max-y y2)))))))
        (when graph
          (update-limits graph))
        (when subgraphs
          (mapcar #'update-limits subgraphs)))
      (ecase auto-size
        (:border
         (setf width (max 1 (+ (max 0 min-x) max-x))
               height (max 1 (+ (max 0 min-y) max-y))))
        (t
         (setf width (max 1 max-x)
               height (max 1 max-y))))))
  (let ((image (aa-misc:make-image width height background)))
    (when graph
      (loop for (color . paths) in graph
         do (rasterize-paths paths image color 1.0 scale)))
    (dolist (subgraph subgraphs)
      (loop for (color . paths) in subgraph
         do (rasterize-paths paths image color 0.3 scale)))
    image))

(defun run-my-show-annotated-path ()
  (let ((annotated-path (paths:path-annotated (generate-swervy-rectangle))))
  (my-show-path annotated-path)))

(defun show-path (path)
  (aa-misc:show-image (net.tuxee.vectors-doc::create-graph path)))

(defun run-my-show-path ()
  (let ((path  (generate-swervy-rectangle))
	(background #(255 255 255)))
    (multiple-value-bind (x1 y1 x2 y2)
	(net.tuxee.vectors-doc::paths-bounding-box path 1.0)
      (let* ((width (- x2 x1))
	     (height (- y2 y1))
	     (background-color #(230 245 255)))
	(let ((image (aa-misc:make-image width height background)))
          (net.tuxee.vectors-doc::rasterize-paths path image background-color)
	    (aa-misc:show-image image))))))

(defun run-rasterize-path ()
  (let ((path (generate-swervy-rectangle)))
    (net.tuxee.vectors-doc::create-graph path)))

(defun run-save-annotated-path ()
  (setf net.tuxee.vectors-doc::*target*
	(merge-pathnames "src/jewel/jewel-lab/image-tests/"
			 (asdf:system-source-directory :jewel)))
  (setf aa-misc::*external-viewer* "display")
  (let ((string-path
	 (zpb-ttf:with-font-loader
	     (loader (merge-pathnames "src/jewel/jewel-lab/FreeSerifBoldItalic.ttf"
			 (asdf:system-source-directory :jewel)))
	   (paths-ttf:paths-from-string loader "Hello World!"
					:offset (paths:make-point 200 550)
					:scale-x 0.3
					:scale-y -0.3))))
    (net.tuxee.vectors-doc::save-annotated-path "string-path.pnm"
						string-path
						:auto-size :border)))
(defparameter *path* nil)
