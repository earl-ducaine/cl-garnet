;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANGLE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains demo code for showing angular rotations in a window
;;;
;;; When loaded, it creates a window that contains angular things
;;;
;;; This is intended as a test and demonstration of the angle interactor
;;; as part of the Garnet project.
;;; 
;;; ** Call (Do-Go) to start and (Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers (modified by Andrew Mickish)

(in-package :DEMO-ANGLE)

(defparameter *test-debug* nil)
(defparameter vp NIL)                  ;; window to be used for the display
(defparameter top-agg4 NIL)            ;; top aggregate in window
(defparameter fnt2 NIL)                ;; small, nice font

(defconstant straight-down-angle (/ (* 3.0 PI) 2.0))
(defconstant right-angle (/ PI 2.0))
(defconstant raddeg (/ 180.0 PI))


;;; ***************************************************************************
;;; Helper position finding procedures
;;; ***************************************************************************


(defun center-x (obj)                               ;; center-x is one-half of
  (+ (gv obj :left) (floor (gv obj :width) 2)))     ;; the width from the left


(defun bottom (obj)
  (+ (gv obj :top) (gv obj :height) ))


(defun pos-angle-clip (angle)                       ;; function guarantees that
  (if (or (null angle)                              ;; the angle will be
	  (< angle 0)                               ;; displayable on the gauge
	  (> angle straight-down-angle))
      0
      (if (> angle PI)
	  PI
	  angle)))

;;; ***************************************************************************
;;; Helper create procedures
;;; ***************************************************************************

(defun make-str-obj (txt whatfnt x y)
    (create-instance nil opal:text (:font whatfnt)
				   (:string txt)
				   (:left x)(:top y)))


(defun make-circle-obj (x y w h)
  (create-instance nil opal:circle
		   (:left x)(:top y)(:width w)(:height h)))


(defun make-semicircle-obj (x y w h)
  (let (obj
	(y+h (+ y h)) (w/2 (floor w 2)) (pi/18 (/ pi 18)))

    ;; Semicircle is an arc with a line connecting the endpoints
    ;;
    (setq obj (create-instance NIL opal:aggregate))
    (opal:add-components obj
	    (create-instance nil opal:arc
	       (:left x) (:top y) (:width w) (:height (+ h h))
	       (:angle1 0.0) (:angle2 pi))
	    (create-instance nil opal:line
	       (:x1 x) (:y1 y+h) (:x2 (+ x w)) (:y2 y+h)))

    ;; Add tick marks to perimeter of semicircle
    (dotimes (i 18)
      (if (> i 0)
	  (opal:add-component obj
		  (create-instance nil opal:line
		     (:x1 (+ x w/2 (round (* w/2 (cos (* pi/18 i))))))
		     (:y1 (- y+h   (round (* h   (sin (* pi/18 i))))))
		     (:x2 (+ x w/2 (round (* w/2 (cos (* pi/18 i)) 0.95))))
		     (:y2 (- y+h   (round (* h   (sin (* pi/18 i)) 0.95))))))))

    (s-value obj :left x)
    (s-value obj :top y)
    (s-value obj :width w)
    (s-value obj :height h)
    obj))


;;; ********************************************************************
;;; Circular Gauge
;;; ********************************************************************

(defun make-gauge (x y label feedback-style)
  (let (feedback box int-feedback str str2 menu-name needle)

    (setq menu-name (create-instance NIL opal:aggregate))
    (when *test-debug* (format T "~%<><><><><><><><>~%created gauge named ~s~%"
			       menu-name))

    ;; BOX is the gauge frame
    (setf box (make-semicircle-obj x y 160 80))

    ;; Coordinates of needle anchor
    (s-value box :center-rot (formula
			      `(list (center-x ',box) (bottom ',box))))
    (when *test-debug* (format T "created box ~s~%" box))

    (setq needle 
     (cond

       ;; Needle1 is an agregadget of a line with an arrowhead
      ;;
      ((eq feedback-style 'needle1)
       (create-instance NIL opal:aggregadget
		   (:x1 (formula `(first (gv ',box :center-rot))))
		   (:y1 (formula `(second (gv ',box :center-rot))))
		   (:x2 (formula `(+ (gvl :x1)
				     (round (* 70.0
				       (cos (Pos-Angle-Clip (gvl :angle))))))))
		   (:y2 (formula `(- (gvl :y1)
				     (round (* 70.0
				       (sin (Pos-Angle-Clip (gvl :angle))))))))
		   (:line-style opal:line-2)
		   (:parts
		    `((:shaft ,opal:line
			  (:x1 ,(o-formula (gvl :parent :x1)))
			  (:x2 ,(o-formula (gvl :parent :x2)))
			  (:y1 ,(o-formula (gvl :parent :y1)))
			  (:y2 ,(o-formula (gvl :parent :y2)))
			  (:line-style ,(o-formula (gvl :parent :line-style)))
			  (:length ,(o-formula (gvl :parent :length))))
		      (:head ,opal:arrowhead
			  (:from-x ,(o-formula (gvl :parent :x1)))
			  (:from-y ,(o-formula (gvl :parent :y1)))
			  (:head-x ,(o-formula (gvl :parent :x2)))
			  (:head-y ,(o-formula (gvl :parent :y2)))
			  (:line-style ,(o-formula (gvl :parent :line-style)))
			  (:length 10)
			  (:diameter 15)
			  (:open-p t))))))

      ;; Needle2 is a poly-line in the shape of an arrow
      ;;
      ((eq feedback-style 'needle2)
       (create-instance NIL opal:polyline
		   (:x1 (formula `(first (gv ',box :center-rot))))
		   (:y1 (formula `(second (gv ',box :center-rot))))
		   (:filling-style opal:gray-fill)

		   (:point-list (formula `(list
			(gvl :x1) (gvl :y1)

			(+ (gvl :x1)
			   (round (* 5.0
				     (cos (- (gvl :angle) right-angle)))))
			(- (gvl :y1)
			   (round (* 5.0
				     (sin (- (gvl :angle) right-angle)))))

		        (+ (gvl :x1)
		           (round (* 57.0
				     (cos (- (gvl :angle) .08)))))
		        (- (gvl :y1)
		           (round (* 57.0
				     (sin (- (gvl :angle) .08)))))

		        (+ (gvl :x1)
		           (round (* 57.0
				     (cos (- (gvl :angle) .2)))))
		        (- (gvl :y1)
		           (round (* 57.0
				     (sin (- (gvl :angle) .2)))))

		        (+ (gvl :x1)
		           (round (* 70.0
				     (cos (Pos-Angle-Clip (gvl :angle))))))
		        (- (gvl :y1)
		           (round (* 70.0
				     (sin (Pos-Angle-Clip (gvl :angle))))))

		        (+ (gvl :x1)
		           (round (* 57.0
				     (cos (+ (gvl :angle) .2)))))
		        (- (gvl :y1)
		           (round (* 57.0
				     (sin (+ (gvl :angle) .2)))))

		        (+ (gvl :x1)
		           (round (* 57.0
				     (cos (+ (gvl :angle) .08)))))
		        (- (gvl :y1)
		           (round (* 57.0
				     (sin (+ (gvl :angle) .08)))))

			(+ (gvl :x1)
			   (round (* 5.0
				     (cos (+ (gvl :angle) right-angle)))))
			(- (gvl :y1)
			   (round (* 5.0
				     (sin (+ (gvl :angle) right-angle)))))

			(gvl :x1) (gvl :y1))))))))



    ;; Long needle (always visible)
    ;;
    (setq feedback (create-instance NIL needle
				    (:angle (/ pi 3))))
    (when *test-debug* (format T "created line feedback ~s~%" feedback))


    ;; Short needle (invisible when not in action)
    ;;
    (setq int-feedback (create-instance NIL opal:line
	                  (:x1 (formula `(first (gv ',box :center-rot))))
			  (:y1 (formula `(second (gv ',box :center-rot))))
			  (:x2 (formula `(+ (gvl :x1)
					    (round (* 40.0 (cos (Pos-Angle-Clip
								 (gvl :angle))))))))
			  (:y2 (formula `(- (gvl :y1)
					    (round (* 40.0 (sin (Pos-Angle-Clip
								 (gvl :angle))))))))
			  (:line-style opal:line-2)
			  (:angle (/ pi 3.0))
			  (:visible (formula `(gvl :obj-over)))))
    (when *test-debug* (format T "created interim feedback ~s~%" int-feedback))


    (setq str (make-str-obj label fnt2 
			    (formula `(+ (floor (- (gv ',box :width)
							 (gvl :width)) 2)
					       (gv ',box :left)))
			    (formula `(+ 5 (bottom ',box)))))
    (when *test-debug* (format T "created str 1 ~s~%" str))


    (setq str2 (make-str-obj
		(formula `(format NIL "~,3F"
				  (* raddeg (or (gv ',feedback :angle) 0.0))))
		fnt2 
		(formula `(+ (floor (- (gv ',str :width)
				       (gvl :width)) 2)
			     (gv ',str :left)))
		(formula `(+ 5 (bottom ',str)))))
    (when *test-debug* (format T "created str 2 ~s~%" str2))


    (s-value menu-name :box box)
    (s-value menu-name :feedback feedback)
    (s-value menu-name :interim-feedback int-feedback)
    (opal:add-components menu-name box str str2 feedback int-feedback)
    (when *test-debug* (format T "done~%"))
    menu-name))

;;; ********************************************************************
;;; Stirring motions
;;; ********************************************************************

(defun myrotate (agg pt)
  (declare (ignore agg))
; (when *test-debug* (format T "myrotate ~s angle = ~s~%" agg (gvl :my-angle)))
  (let* ((angle (gvl :my-angle))
	 (c (cos angle))(s (sin angle))
	 (x (first pt))(y (second pt)))
    (list (round (+ (* x c)(* y s)))
	  (round (- (* y c)(* x s))))))

(defun make-house (x y w obj)
  (let (agg (w2 (floor w 2)))
    (setq agg (create-instance nil opal:aggregate
		     (:left x)(:top y)(:width w)(:height w)
		     (:my-angle (formula `(gv ',obj :my-angle)))
		     (:p1 (formula `(myrotate ',agg ',(list (- w2) 0))))
		     (:p2 (formula `(myrotate ',agg ',(list 0 (- w2)))))
		     (:p3 (formula `(myrotate ',agg ',(list w2 0))))
		     (:p4 (formula `(myrotate ',agg ',(list w2 w2))))
		     (:p5 (formula `(myrotate ',agg ',(list (- w2) w2))))))
    (incf x w2)
    (incf y w2)

    (opal:add-components agg
;    --left
	 (create-instance nil opal:line
		     (:x1 (formula `(+ ,x (first (gv ',agg :p1)))))
		     (:y1 (formula `(+ ,y (second (gv ',agg :p1)))))
		     (:x2 (formula `(+ ,x (first (gv ',agg :p5)))))
		     (:y2 (formula `(+ ,y (second (gv ',agg :p5))))))
;    --top
	 (create-instance nil opal:line
		     (:x1 (formula `(+ ,x (first (gv ',agg :p1)))))
		     (:y1 (formula `(+ ,y (second (gv ',agg :p1)))))
		     (:x2 (formula `(+ ,x (first (gv ',agg :p3)))))
		     (:y2 (formula `(+ ,y (second (gv ',agg :p3))))))
;    --right
	 (create-instance nil opal:line
		     (:x1 (formula `(+ ,x (first (gv ',agg :p3)))))
		     (:y1 (formula `(+ ,y (second (gv ',agg :p3)))))
		     (:x2 (formula `(+ ,x (first (gv ',agg :p4)))))
		     (:y2 (formula `(+ ,y (second (gv ',agg :p4))))))
;    --bottom
	 (create-instance nil opal:line
		     (:x1 (formula `(+ ,x (first (gv ',agg :p5)))))
		     (:y1 (formula `(+ ,y (second (gv ',agg :p5)))))
		     (:x2 (formula `(+ ,x (first (gv ',agg :p4)))))
		     (:y2 (formula `(+ ,y (second (gv ',agg :p4))))))
;    --point left
	 (create-instance nil opal:line
		     (:x1 (formula `(+ ,x (first (gv ',agg :p1)))))
		     (:y1 (formula `(+ ,y (second (gv ',agg :p1)))))
		     (:x2 (formula `(+ ,x (first (gv ',agg :p2)))))
		     (:y2 (formula `(+ ,y (second (gv ',agg :p2))))))
;    --point right
	 (create-instance nil opal:line
		     (:x1 (formula `(+ ,x (first (gv ',agg :p2)))))
		     (:y1 (formula `(+ ,y (second (gv ',agg :p2)))))
		     (:x2 (formula `(+ ,x (first (gv ',agg :p3)))))
		     (:y2 (formula `(+ ,y (second (gv ',agg :p3)))))))
    agg))

(defun make-stirrer (x y)
  (let (menu-name box str house dot)
    (setq menu-name (create-instance NIL opal:aggregate))
    (when *test-debug* (format T "~%<><><><><><><><>~%created gauge named ~s~%"
			       menu-name))
    (setf box (make-circle-obj x y 80 80))
    (s-value box :center-rot (list (+ x 40) (+ y 40)))
    (setf dot (make-circle-obj (+ x 39) (+ y 39) 2 2))
    (setq str (make-str-obj
	       (formula `(concatenate 'string "Angle = "
					  (format NIL "~,3F"
					   (* raddeg (or (gvl :my-angle) 0.0)))))
	       fnt2 
	       (formula `(+ 2 (gv ',box :left)))
	       (formula `(+ 5 (bottom ',box)))))
    (s-value str :my-angle 0.0)
    (setq house (make-house (+ x 100) (+ y 20) 40 str))
    (s-value menu-name :box box)
    (s-value menu-name :str str)
    (opal:add-components menu-name box dot str house)
    (when *test-debug* (format T "done~%"))
    menu-name))

;;; ********************************************************************
;;; Main procedures
;;; ********************************************************************

(defparameter menu1 NIL)
(defparameter menu2 NIL)
(defparameter menu3 NIL)

(defparameter inter1 NIL)
(defparameter inter2 NIL)
(defparameter inter3 NIL)


(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create a viewport
  (create-instance 'vp inter:interactor-window (:left 150) (:top 50)
			    (:width 400) (:height 400)
                                (:double-buffered-p double-buffered-p)
			    (:title "DEMO-ANGLE") (:icon-title "Angle"))

  ;;; create the top level aggregate in the window
  (s-value vp :aggregate
	   (create-instance 'top-agg4 opal:aggregate
					   (:overlapping NIL)))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-ANGLE"))
     (g-value vp :destroy-hooks)))
  
  (create-instance 'fnt2 opal:font)
  
  ;; ** Menu 1 **
  (setq menu1 (make-gauge 10 10 "Pressure" 'needle2))
  (opal:add-components top-agg4 menu1)
  (opal:update vp)
  (create-instance 'inter1 inter:angle-interactor
    (:start-where `(:in-box ,(g-value menu1 :box)))
    (:center-of-rotation (g-value (g-value menu1 :box) :center-rot))
    (:obj-to-change (g-value menu1 :feedback))
    (:feedback-obj (g-value menu1 :interim-feedback))
    (:outside :last)
    (:window vp))

  ;; ** Menu 2 **
  (setq menu2 (make-stirrer 200 10))
  (opal:add-components top-agg4 menu2)
  (opal:update vp)
  (create-instance 'inter2 inter:angle-interactor 
    (:start-where
     `(:in-box ,(g-value menu2 :box)))
    (:obj-to-change (g-value menu2 :str))
    (:start-action NIL)
    (:back-inside-action NIL)
    (:stop-action NIL)
    (:center-of-rotation (g-value menu2 :box :center-rot))
    (:running-action
     #'(lambda(an-interactor obj-to-change new-angle angle-delta)
	 (declare (ignore an-interactor new-angle))
	 (when *test-debug* (format T "delta=~s~%" angle-delta))
	 (incf (g-value obj-to-change :my-angle)
	       (/ angle-delta 5.0))))
    (:window vp))

  ;; ** Menu 3 **
  (setq menu3 (make-gauge 10 150 "Temperature" 'needle1))
  (opal:add-components top-agg4 menu3)
  (opal:update vp)
  (create-instance 'inter3 inter:angle-interactor 
    (:start-where `(:in-box ,(g-value menu3 :box)))
    (:center-of-rotation (g-value (g-value menu3 :box) :center-rot))
    (:obj-to-change (g-value menu3 :feedback))
    (:outside :last)
    (:window vp))

  (opal:update vp)
  (Format T "~%Demo-Angle:
  Press inside either gauge with left and move back and forth.  The top
  gauge uses a feedback object, and the bottom one doesn't.  To rotate the
  house, move mouse while left button pressed in SLOW circles inside the circle.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )


;; **********
;;    STOP
;; **********
(defun Do-Stop ()
  (opal:destroy vp))

