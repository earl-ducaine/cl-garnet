;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Designed and implemented by Brad Myers
;;;


(in-package :DEMO-ANIMATOR)

(defvar DEMO-ANIMATOR-INIT
    (dolist (gadget '("gadgets:text-buttons-loader"))
	    (common-lisp-user::garnet-load gadget))
  )

(defparameter pixmapfilename "eye")
(defparameter numpixmapfiles 12)

(defvar pixmaps (let (i filename pics)
		  (format T "Loading pictures...")
		  (force-output)
		  (setq i 0)
		  (dotimes (num numpixmapfiles)
		    (format T "~a..." (1+ i))
		    (force-output)
		    (setq filename
			  (merge-pathnames
			   (format NIL "~a~a.xpm" pixmapfilename
					   (1+ i))
			   common-lisp-user::Garnet-Pixmap-Pathname))
		    (push (opal:read-xpm-file filename) pics)
		    (if (= i 5)
			(setq i 12)
			(incf i)))
		  (format T "~%")
		  (reverse pics)))

(defparameter moving-circle NIL)
(defparameter moving-button NIL)
(defparameter moving-pixmap NIL)
(declaim (special TOP-WIN WRAPPING-CIRCLE ANIMATOR-PIXMAP
		  ANIMATOR-BUTTON BOUNCING-BUTTON FIXED-BUTTON
		  PIXMAP-BUTTON ANIMATOR-CIRCLE ANIMATING-PIXMAP))

(defun do-go (&key dont-enter-main-event-loop (double-buffered-p T))
  (let (agg)
    ;;;create top-level window
    (create-instance 'TOP-WIN inter:interactor-window
       (:left 500) (:top 100)
       (:double-buffered-p double-buffered-p)
       (:width 300) (:height 200)
       (:title "GARNET Animator Demo")
       (:icon-title "Animator"))
    (s-value top-win :aggregate
	     (setq agg (create-instance NIL opal:aggregate)))

    ;; If we get clobbered by the window manager, let the demos
    ;; controller know (if it's there).
    (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
      (pushnew
       #'(lambda (win)
	   (declare (ignore win))
	   (common-lisp-user::Garnet-Note-Quitted "DEMO-ANIMATOR"))
       (g-value top-win :destroy-hooks)))

    (create-instance 'wrapping-circle opal:circle
		     (:left 150)(:top 100)
		     (:filling-style opal:blue-fill)
		     (:line-style NIL)
		     (:draw-function :xor)
		     (:fast-redraw-p T))

    (create-instance 'animating-pixmap opal:pixmap
      (:left 5)(:top 168) (:count 0)
      (:image (o-formula (nth (gvl :count) pixmaps))))

    (create-instance 'animator-circle inter:animator-wrap
		     (:window TOP-WIN)
		     (:width 30)(:height 30)
		     (:obj-to-change wrapping-circle)
		     (:x-inc -2)(:y-inc 3)
		     (:timer-repeat-wait 0.1)) ;seconds

    (create-instance 'animator-button inter:animator-bounce
		     (:window TOP-WIN)
		     (:x-inc 5)(:y-inc 5)
		     (:timer-repeat-wait 0.25))

    (create-instance 'animator-pixmap inter:animator-interactor
		     (:window TOP-WIN)
		     (:moving-right T)
		     (:timer-repeat-wait 0.1)
		     (:timer-handler
		      #'(lambda(inter)
			  (let ((moving-right (g-value inter :moving-right))
				(cnt (g-value animating-pixmap :count))
				(pos (g-value animating-pixmap :left)))
			    (incf cnt)
			    (if (>= cnt (if moving-right 6 12))
				(s-value animating-pixmap :count
					 (if moving-right 0 6))
				(s-value animating-pixmap :count cnt))
			    (incf pos (if moving-right 2 -2))
			    (if moving-right
				(if (>= pos (- (g-value TOP-WIN :width) 32))
				    (progn
				      (s-value inter :moving-right NIL)
				      (s-value animating-pixmap :count 6))
				    (incf pos 2))
				(if (<= pos 0)
				    (progn
				      (s-value inter :moving-right T)
				      (s-value animating-pixmap :count 0))
				    (incf pos -2)))
			    (s-value animating-pixmap :left pos)))))

    (create-instance 'bouncing-button gg:text-button
		     (:left 10)(:top 10)
		     (:string "Push Me")
		     (:constant '(T :except :left :top))
		     (:final-feedback-p NIL)
		     (:selection-function
		      #'(lambda (gadget val)
			  (declare (ignore gadget val))
			  (if moving-button
			      (progn (inter:stop-animator animator-button)
				     (setq moving-button NIL))
			      (progn (inter:start-animator animator-button)
				     (setq moving-button T))))))
    (s-value animator-button :obj-to-change bouncing-button)
    (create-instance 'fixed-button gg:text-button
		     (:top 10)(:left 220)
		     (:string "Push Me")
		     (:constant '(T))
		     (:final-feedback-p NIL)
		     (:selection-function
		      #'(lambda (gadget val)
			  (declare (ignore gadget val))
			  (if moving-circle
			      (progn (inter:stop-animator animator-circle)
				     (setq moving-circle NIL))
			      (progn (inter:start-animator animator-circle)
				     (setq moving-circle T))))))
    (create-instance 'pixmap-button gg:text-button
		     (:top 124)(:left 220)
		     (:string "Push Me")
		     (:constant '(T))
		     (:final-feedback-p NIL)
		     (:selection-function
		      #'(lambda (gadget val)
			  (declare (ignore gadget val))
			  (if moving-pixmap
			      (progn (inter:stop-animator animator-pixmap)
				     (setq moving-pixmap NIL))
			      (progn (inter:start-animator animator-pixmap)
				     (setq moving-pixmap T))))))
    (opal:add-components agg animating-pixmap fixed-button
			 bouncing-button pixmap-button wrapping-circle)
    (opal:update top-win)
    (format T "Click on buttons to start and stop animations~%")
    (unless dont-enter-main-event-loop
      #-(and cmu mp) (inter:main-event-loop))))

(defun do-stop ()
  (opal:destroy top-win))
