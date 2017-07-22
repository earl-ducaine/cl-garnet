(in-package :cl-processing)

(defun size (new-width new-height)
  (destructuring-bind (&key left top width height)
      *current-size-and-posision*
    (setf *current-size-and-posision*
	  (list left
		top
		(or new-width width)
		(or new-height height)))))

(defmacro background (r g b)
  `(setf *current-background-color*
	 (create-instance nil opal:color
	   (:green ,g)
	   (:blue ,b)
	   (:red ,r))))

(defun no-stroke ()
  'no-op)

(defmacro proc-fill (r g b)
  `(setf *current-fill-color*
	 (create-instance nil opal:color
	   (:green ,g)
	   (:blue ,b)
	   (:red ,r))))

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
  (let ((line (create-instance 'new-rect opal:polyline))
	figure)
    (dolist (point points)
      (push (car point) figure)
      (push (cadr point) figure))
    ;;(s-value line :point-list '(10 50 50 10 90 10 130 50))
    (s-value line :point-list (reverse figure))
    (s-value line :filling-style opal:light-gray-fill)
    (s-value line :line-style opal:line-4)))

(defmacro rect (left top width height)
  (let* ((p1 (list left top))
	 (p2 (list (+ left width) top))
	 (p3 (list (+ left width) (+ top height)))
	 (p4 (list left (+ top height)))
	 (points (list p1 p2 p3 p4 p1)))
    (create-polyline points)))

(defun create-win (left top width height)
  ;; Create a small window at the upper left corner of the screen
  (setf *win* (create-instance 'win inter:interactor-window))
  (s-value *win* :left left)
  (s-value *win* :top top)
  (s-value *win* :width width)
  (s-value *win* :height height))
    
(defun create-processing-window-new ()
  (rect 10 10 100 25)
  ;;(create-polyline '((10 50) (50 10) (90 10) (130 50)))
  ;; create an aggregate for the window
  (create-win 20 20 300 150)
  (s-value win :aggregate (create-instance 'agg opal:aggregate))
  ;; create the string
  ;; add the string to the aggregate
  ;; (opal:add-component agg hello)
  (opal:add-component agg new-rect)
  ;; Cause the window and string to be displayed
  (opal:update win))



(defun create-processing-window ()
  (rect 10 10 200 50)
  ;; Create a small window at the upper left corner of the screen
  (create-instance 'win inter:interactor-window
    (:left 10)
    (:top 10)
    (:width 200)
    (:height 50))
  ;; create an aggregate for the window
  (s-value win :aggregate (create-instance 'agg opal:aggregate))
  ;; create the string
  (create-instance 'hello opal:text
    (:left 10)
    (:top 20)
    (:background-color opal:black)
    (:foreground-color opal:black)
    (:string "hello world"))
  (create-instance 'line opal:polyline
    (:point-list '(10 50 50 10 90 10 130 50))
    (:filling-style opal:light-gray-fill)
    (:line-style opal:line-4))
  ;; add the string to the aggregate
  ;; (opal:add-component agg hello)
  (opal:add-component agg new-rect)
  ;; Cause the window and string to be displayed
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




;; size(400, 400);
;; background(255);
;; noStroke();
;; fill(0);
;; rect(width/4, height/4, width/2, height/2);
