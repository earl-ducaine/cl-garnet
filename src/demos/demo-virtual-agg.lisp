;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-VIRTUAL-AGG; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$


(in-package :DEMO-VIRTUAL-AGG)

(declaim (special MY-CIRCLE W A R DOTS-BBOX LITTLE-STAR FEEDBACK-STAR))

(defvar dots)
(defvar *vp*)
(defvar *input*)
(defvar *item-array*)

(create-instance 'my-circle opal:circle
  (:filling-style (o-formula (fourth (gvl :item-values))))
  (:radius (o-formula (third (gvl :item-values))))
  (:left (o-formula (- (first (gvl :item-values)) (gvl :radius))))
  (:top (o-formula (- (second (gvl :item-values)) (gvl :radius))))
  (:width (o-formula (* 2 (gvl :radius))))
  (:height (o-formula (gvl :width))))

(defun pick-color (n)
  (case n
    (0 opal:red-fill)
    (1 opal:yellow-fill)
    (2 opal:green-fill)
    (3 opal:blue-fill)
    (4 opal:purple-fill)
    (5 opal:orange-fill)
    (6 opal:cyan-fill)))

(defun add-new-dot (x y &optional rad color)
  (opal:add-item dots
                 (list x
	               y
	               (or rad (+ 3 (random 37)))
	               (pick-color (or color (random 7))))))

(defun Create-Circle (dum xy)
  (declare (ignore dum))
  (add-new-dot (first xy) (second xy)))

(defun Destroy-Circle (dum xy)
  (declare (ignore dum))
  (let ((rank (opal:Point-To-Rank dots (first xy) (second xy))))
    (if rank
      (opal:remove-item dots rank)
      (inter::beep))))


(defun random-color-other-than (color)
  (let ((new-color (pick-color (random 7))))
    (if (eq new-color color)
	(random-color-other-than color)
	new-color)))

(defun Change-Color (dum xy)
  (declare (ignore dum))
  (let ((rank (opal:Point-To-Rank dots (first xy) (second xy)))
	item old-color)
    (when rank
      (setq item (aref (g-value dots :item-array) rank))
      (setq old-color (fourth item))
      (opal:change-item dots
	 (list (first item) (second item) (third item) (random-color-other-than old-color))
	 rank))))

(defun Toggle-Visibility (dum xy)
  (declare (ignore dum xy))
  (s-value dots :visible (not (g-value dots :visible))))


(defun Do-Go (&key (num-dots 1000) dont-enter-main-event-loop double-buffered-p)
  (create-instance 'w inter:interactor-window (:top 40) (:left 10)
                                              (:width 650) (:height 650)
     (:double-buffered-p double-buffered-p)
     (:title "DEMO CIRCLE")
     (:aggregate (create-instance 'a opal:aggregate)))
  (setq *vp* w)

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-VIRTUAL-AGG"))
       (g-value *vp* :destroy-hooks)))
  
  (setq *input*
    (open (merge-pathnames "circles.data" common-lisp-user::Garnet-DataFile-PathName)
          :direction :input))

  ;; gray rectangle behind virtual aggregate
  (opal:add-component a (create-instance 'r opal:rectangle
		          (:filling-style opal:gray-fill)
		          (:left 100) (:top 100) (:width 450) (:height 450)))

  (setq *item-array* (make-array (min num-dots 1000) :adjustable t))

  ;; First thousand dots are read from circles.data
  (dotimes (i (min num-dots 1000))
    (when (zerop (mod i 100)) (format t "."))
    (setf (aref *item-array* i)
      (list (read *input*) (read *input*) 
	    (read *input*) (pick-color (read *input*)))))

  (setq dots
    (create-instance nil opal:virtual-aggregate
      (:item-prototype my-circle)
      (:item-array *item-array*)
      (:point-in-item #'(lambda (virtual-aggregate item-values x y)
			  (declare (ignore virtual-aggregate))
			  (<= (+ (expt (- x (first item-values)) 2)
				 (expt (- y (second item-values)) 2))
			      (expt (third item-values) 2))))))

  (opal:add-component a dots)

  ;; The rest are picked randomly
  (dotimes (i (- num-dots 1000))
    (when (zerop (mod i 100)) (format t "."))
    (add-new-dot (+ 20 (random 610)) (+ 20 (random 610))))

  (close *input*)

  (create-instance 'dots-bbox opal:rectangle
    (:left (o-formula (- (gv dots :left) 2)))
    (:top  (o-formula (- (gv dots :top ) 2)))
    (:width  (o-formula (+ (gv dots :width ) 4)))
    (:height (o-formula (+ (gv dots :height) 4)))
    (:fast-redraw-p t)
    (:draw-function :xor)
    (:line-style opal:yellow-line))
  (opal:add-component a dots-bbox)

  (create-instance 'little-star opal:polyline
                   (:box (list 10 80 50 50))
                   (:point-list
                    (formula '(list (+ (first (gvl :box))  25)
                                    (+ (second (gvl :box))  0)
                                    (+ (first (gvl :box))  31)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  49)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  34)
                                    (+ (second (gvl :box)) 28)
                                    (+ (first (gvl :box))  40)
                                    (+ (second (gvl :box)) 45)
                                    (+ (first (gvl :box))  25)
                                    (+ (second (gvl :box)) 35)
                                    (+ (first (gvl :box))  10)
                                    (+ (second (gvl :box)) 45)
                                    (+ (first (gvl :box))  16)
                                    (+ (second (gvl :box)) 28)
                                    (+ (first (gvl :box))   1)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  19)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  25)
                                    (+ (second (gvl :box))  0))))
                   (:line-style opal:line-2)
                   (:filling-style opal:diamond-fill))

  (create-instance 'feedback-star opal:polyline
                   (:draw-function :xor)
                   (:visible (o-formula (gvl :obj-over)))
                   (:obj-over nil)
                   (:point-list
                    (formula '(list (+ (first (gvl :box))  25)
                                    (+ (second (gvl :box))  0)
                                    (+ (first (gvl :box))  31)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  49)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  34)
                                    (+ (second (gvl :box)) 28)
                                    (+ (first (gvl :box))  40)
                                    (+ (second (gvl :box)) 45)
                                    (+ (first (gvl :box))  25)
                                    (+ (second (gvl :box)) 35)
                                    (+ (first (gvl :box))  10)
                                    (+ (second (gvl :box)) 45)
                                    (+ (first (gvl :box))  16)
                                    (+ (second (gvl :box)) 28)
                                    (+ (first (gvl :box))   1)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  19)
                                    (+ (second (gvl :box)) 18)
                                    (+ (first (gvl :box))  25)
                                    (+ (second (gvl :box))  0))))
                   (:fast-redraw-p t)
                   (:box (list 0 0 0 0))
                   (:line-style opal:dashed-line))

  (opal:add-components a little-star feedback-star)

  (opal:update w)

  (create-instance 'CREATOR inter:two-point-interactor
     (:start-event :leftdown)
     (:continuous nil)
     (:start-where T)
     (:window w)
     (:final-function #'Create-Circle))
    
  (create-instance 'MOVER inter:move-grow-interactor
                   (:window *vp*)
		   (:start-event :middledown)
                   (:stop-event '(:any-mouseup :any-keyboard))
                   (:running-where T)
                   (:waiting-priority inter:high-priority-level)
                   (:outside :last)
                   (:start-where `(:in ,little-star))
                   (:obj-to-change nil)
                   (:feedback-obj feedback-star)
                   (:attach-point :where-hit))

  (create-instance 'DESTROYER inter:two-point-interactor
     (:start-event :rightdown)
     (:continuous nil)
     (:start-where T)
     (:window w)
     (:final-function #'Destroy-Circle))

  (create-instance 'COLOR-CHANGER inter:two-point-interactor
     (:start-event :shift-middledown)
     (:continuous nil)
     (:start-where T)
     (:window w)
     (:final-function #'Change-Color))

  (create-instance 'VISIBILITY-TOGGLER inter:two-point-interactor
     (:start-event :shift-rightdown)
     (:continuous nil)
     (:start-where T)
     (:window w)
     (:final-function #'Toggle-Visibility))

  (format t "~%Click with left button to create circle,~%")
  (format t   "           middle button and drag to move little star.~%")
  (format t   "           right button to destroy circle.~%")
  (format t "~%           shift-middle randomly changes color of circle.~%")
  (format t   "           shift-right toggles visibility of virtual-aggregate.~%")
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )


(defun Do-Stop ()
  (opal:destroy w))
