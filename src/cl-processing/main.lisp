(in-package :cl-processing)

(defparameter *width* nil)
(defparameter *height* nil)

(defun size (width height)
  (setf *width* width)
  (setf *height* height))

(defun convert-to-color-channels (color-composite)
  (let* ((color (- 255 color-composite))
	 (r (/  (ldb (byte 3 5) color) (expt 2 3)))
	 (g (/ (ldb (byte 3 2) color) (expt 2 3)))
	 (b (/ (ldb (byte 2 0) color) (expt 2 2))))
    (values r g b)))

(defun background (color)
  (multiple-value-bind (r g b) (convert-to-color-channels color)
    (setf *current-background-color*
	  (create-instance nil opal:color
	    (:green g)
	    (:blue b)
	    (:red r)))))

(defun stroke (stroke-col)
  (multiple-value-bind (r g b) (convert-to-color-channels stroke-col)
    (setf *current-stroke-color*
	  (create-instance nil opal:color
	    (:green g)
	    (:blue b)
	    (:red r)))))

(defun no-stroke ()
  (setf *current-stroke-color* nil))

(defun fill-256-color (color)
  (multiple-value-bind (r g b) (convert-to-color-channels color)
  (setf *current-fill-color*
	(create-instance nil opal:color
	  (:green g)
	  (:blue b)
	  (:red r)))))

(defun fill-three-chanel (r g b)
  (setf *current-fill-color*
	(create-instance nil opal:color
	  (:green (/ g 255.0))
	  (:blue (/ b 255.0))
	  (:red (/ r 255.0)))))

(defparameter *current-line-style* nil)
(defparameter *current-drawable* nil)
(defparameter *current-stroke-color* nil)
(defparameter *default-size-and-posision*
  '(:left 10 :top 10 :width 400 :height 300))
(defparameter *current-size-and-posision* *default-size-and-posision*)
(defparameter *default-background-color*
  opal:black)
(defparameter *current-background-color* *default-background-color*)
(defparameter *default-fill-color*
  opal:white)
(defparameter *current-fill-color* *default-background-color*)
(defparameter *win* *default-background-color*)

(defun create-polyline (points)
  (let* ((fill-style (create-instance nil opal:filling-style
		       (:foreground-color *current-fill-color*)))  
	 (line (create-instance 'new-rect opal:polyline
		 (:filling-style fill-style)))
	figure)
    (dolist (point points)
      (push (car point) figure)
      (push (cadr point) figure))
    (s-value line :point-list (reverse figure))
    ))


(defun ellipse (x y width height)
  (let* ((left (- x (/ width 2)))
	 (top (- y (/ height 2)))
	 (width width)
	 (height height))
    (setf *current-drawable*
	  (let* ((fill-style (create-instance nil opal:filling-style
			       (:foreground-color *current-fill-color*))))  
    (create-instance 'new-ellipse opal:oval
      (:left left)
      (:top top)
      (:width width)
      (:height height)
      (:filling-style fill-style)
      (:line-style *current-line-style*))))))
   
(defun rect (left top width height)
  (let* ((p1 (list left top))
	 (p2 (list (+ left width) top))
	 (p3 (list (+ left width) (+ top height)))
	 (p4 (list left (+ top height)))
	 (points (list p1 p2 p3 p4 p1)))
    (create-polyline points)))

(defun create-win (left top width height)
  (setf *win* (create-instance 'win inter:interactor-window))
  (s-value *win* :left left)
  (s-value *win* :top top)
  (s-value *win* :width width)
  (s-value *win* :height height))

(defparameter *last-start-window-point* '(20 20))

(defun do-processing ()
  (create-win (car *last-start-window-point*)
	      (cadr *last-start-window-point*)
	       *width* *height*)
  (s-value win :aggregate (create-instance 'agg opal:aggregate))
  (opal:add-component agg *current-drawable*)
  (opal:update win))

(defun modify-processing-window ()
  ;; Opal also strives to make it easy to change the picture.  To change
  ;; the x position of the rectangle only requires setting the value of
  ;; the :left slot;  Opal handles the refresh:

  ;; change the position
  (s-value HELLO :left 50)  

  ;; cause the change to be visible
  (opal:update WIN)

  (create-instance 'opal:line-style opal:graphic-quality
    (:background-color opal:black)
    (:forground-color opal:black)))
