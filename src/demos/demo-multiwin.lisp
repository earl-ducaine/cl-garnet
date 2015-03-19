;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MULTIWIN; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$


;;; 
;;; ** Call (demo-multiwin:Do-Go) to start and (demo-multiwin:Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers


(in-package :DEMO-MULTIWIN)

(declaim (special AGG1 AGG2 AGG3
		  OBJ1 OBJ2 OBJ3 OBJ4 OBJ5 
		  FEED1 FEED2 FEED3 INTER1))

(defvar *win1*)
(defvar *win2*)
(defvar *win3*)

(defparameter *test-debug* NIL)

(defun change-windows (obj new-win)
  (opal:remove-component (g-value obj :parent) obj)
  (opal:add-component (g-value new-win :aggregate) obj))

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create windows
  
  (create-instance '*win1* inter:interactor-window
    (:title "GARNET Multi-win1")
    (:left 10)(:top 30) (:height 150)(:width 200)
    (:double-buffered-p double-buffered-p)
    (:aggregate (create-instance 'agg1 opal:aggregate
		  (:left 0)(:top 0)
		  (:height 200)(:width 200))))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-MULTIWIN")
	 (opal:destroy *win2*)
	 (opal:destroy *win3*))
       (g-value *win1* :destroy-hooks)))
  
  (create-instance '*win2* inter:interactor-window
    (:title "GARNET Multi-win2")
    (:left 10)(:top 190) (:height 150)(:width 200)
    (:double-buffered-p double-buffered-p)
    (:aggregate (create-instance 'agg2 opal:aggregate
		  (:left 0)(:top 0)
		  (:height 200)(:width 200))))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-MULTIWIN")
	 (opal:destroy *win1*)
	 (opal:destroy *win3*))
       (g-value *win2* :destroy-hooks)))
  
  (create-instance '*win3* inter:interactor-window
    (:title "GARNET Multi-win3")
    (:left 10)(:top 330) (:height 150)(:width 200)
    (:double-buffered-p double-buffered-p)
    (:aggregate (create-instance 'agg3 opal:aggregate
		  (:left 0)(:top 0)
		  (:height 200)(:width 200))))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-MULTIWIN")
	 (opal:destroy *win1*)
	 (opal:destroy *win2*))
       (g-value *win3* :destroy-hooks)))

  ;;; create objects in the windows to be moved
  
  (create-instance 'obj1 opal:rectangle
    (:box (list 10 10 40 60))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:line-style opal:line-2)
    (:filling-style opal:light-gray-fill))
  
  (create-instance 'obj2 opal:circle
    (:box (list 70 10 40 40))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:line-style NIL)
    (:filling-style opal:black-fill))
  
  (create-instance 'obj3 opal:polyline
    (:box (list 10 80 50 50))
    (:point-list
     (o-formula (list (+ (first (gvl :box)) 0)
		      (+ (second (gvl :box)) 0)
				    
		      (+ (first (gvl :box)) 50)
		      (+ (second (gvl :box)) 0)
				    
		      (+ (first (gvl :box)) 25)
		      (+ (second (gvl :box)) 35)
				    
		      (+ (first (gvl :box)) 0)
		      (+ (second (gvl :box)) 0))))
    (:draw-function :copy-inverted)
    (:line-style opal:line-2)
    (:filling-style opal:light-gray-fill))
  
  (create-instance 'obj4 opal:line
    (:box (list 120 120 50 50))
    (:x1 (o-formula (+ (first (gvl :box)) 0)))
    (:y1 (o-formula (+ (second (gvl :box)) 50)))
    (:x2 (o-formula (+ (first (gvl :box)) 20)))
    (:y2 (o-formula (+ (second (gvl :box)) 0)))
    (:line-style opal:line-4))
  
  (create-instance 'obj5 opal:text
    (:box (list 60 70 40 60))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:string "Move me"))
  
  (opal:add-components agg1 obj1 obj2)
  (opal:add-components agg2 obj3 obj4)
  (opal:add-components agg3 obj5)
  
  (s-value agg1 :list-of-objs (list obj1 obj2 obj3 obj4 obj5))
  
  ;;; create the feedback objects
  
  (create-instance 'feed1 opal:rectangle
    (:box (list 10 10 40 60))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:visible (o-formula (gvl :obj-over)))
    (:draw-function :xor)
    (:line-style opal:dashed-line))

  (create-instance 'feed2 opal:rectangle
    (:box (list 10 10 40 60))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:visible (o-formula (gvl :obj-over)))
    (:draw-function :xor)
    (:line-style opal:dashed-line))

  (create-instance 'feed3 opal:rectangle
    (:box (list 10 10 40 60))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
    (:draw-function :xor)
    (:visible (o-formula (gvl :obj-over)))
    (:line-style opal:dashed-line))
  
  (opal:add-component agg1 feed1)
  (opal:add-component agg2 feed2)
  (opal:add-component agg3 feed3)
  
  (s-value *win1* :feedback feed1)
  (s-value *win2* :feedback feed2)
  (s-value *win3* :feedback feed3)
  
  ;;; create the interactors
  
  (create-instance 'inter1 inter:move-grow-interactor
    (:window (list *win1* *win2* *win3*))
    (:start-where `(:list-element-of ,agg1 :list-of-objs))
    (:feedback-obj (o-formula (gvl :current-window :feedback)))
    (:running-where T)
    (:old-feedback NIL)
    (:attach-point :where-hit)
    (:running-action
     #'(lambda (inter obj new-box)
	 (let ((old-f (g-value inter1 :old-feedback))
	       (cur-f (g-value inter1 :feedback-obj)))
	   (when (not (eq old-f cur-f))
	     (when *test-debug*
	       (format T "~%**change feedback from ~s to ~s~%"
		       old-f cur-f))
	     (when old-f (s-value old-f :obj-over NIL))
	     (when cur-f (s-value cur-f :obj-over obj))
	     (s-value inter1 :old-feedback cur-f)))
	 (call-prototype-method inter obj new-box)))
    (:stop-action
     #'(lambda (inter obj final-box)
	 (let ((win (g-value inter1 :current-window)))
	   (when *test-debug*
	     (format T "~%My stop  win=~s  obj win=~s~%"
		     win (g-value obj :window)))
	   (when (not (eq win (g-value obj :window)))
	     (change-windows obj win))
	   (call-prototype-method inter obj final-box)))))
  
  
  (opal:update *win1*)
  (opal:update *win2*)
  (opal:update *win3*)

  (Format T "~%Demo-Multiwin: 
  Press on an object with the left button to cause it to start moving, hold
  down and the object can be moved into another window.  The feedback will follow
  to the other window, and the object will be there on release.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )

;; ** STOP **
(defun Do-Stop ()
  (opal:destroy *win1*)
  (opal:destroy *win2*)
  (opal:destroy *win3*))

