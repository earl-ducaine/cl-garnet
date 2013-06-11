;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$


;;;
;;  This file contains the code to handle animators
;; 
;;  Designed and implemented by Brad A. Myers

;;; Change log:
;;    6/3/92 Brad Myers - started


(in-package "INTERACTORS")


;;; Animator-Interactor
;;

(defparameter all-animator-processes NIL)
(defparameter all-animator-inters NIL)

(defun Start-Animator (anim)
  (let ((filter (g-value anim :timer-handler)))
    (unless filter
	    (error "Animator must have an :timer-handler"))
    (s-value anim :current-state :animating)
    (launch-timer-process anim (g-value anim :timer-repeat-wait)
			  NIL)))


(defun Stop-Animator (anim)
  (s-value anim :current-state :start)
  (kill-timer-process anim))
(defun Abort-Animator (anim)
  (stop-animator anim))

;;; Default Procedures to go into the slots
;;

(eval-when (eval load compile)
  (proclaim '(special Animator-Interactor)))

(defun Animator-Interactor-Initialize (new-Animator-schema)
  (if-debug new-Animator-schema (format T "Animator initialize ~s~%"
					new-Animator-schema))
  (Check-Interactor-Type new-Animator-schema animator-interactor)
  (Check-Required-Slots new-Animator-schema)
  (Set-Up-Defaults new-Animator-schema)
  )


(defun Animator-Do-Abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format T "Animator aborting~%"))
  (Abort-Animator an-interactor))

(defun Animator-Do-Start (an-interactor new-obj-over event)
  (declare (ignore new-obj-over event))
  (if-debug an-interactor (format T "Animator starting~%"))
  (Start-Animator an-interactor))

(defun Animator-Do-Stop (an-interactor new-obj-over event)
  (declare (ignore new-obj-over event))
  (if-debug an-interactor (format T "Animator stopping~%"))
  (Stop-Animator an-interactor))
  
(defun Animator-do-outside (an-interactor)
  #-garnet-debug (declare (ignore an-interactor))
  (if-debug an-interactor (format T "Animator outside~%")))

(defun Animator-do-outside-stop (an-interactor event)
  (Animator-do-abort an-interactor NIL event))

(defun Animator-do-back-inside (an-interactor new-obj-over event)
  #+garnet-debug (declare (ignore new-obj-over event))
  #-garnet-debug (declare (ignore an-interactor new-obj-over event))
  (if-debug an-interactor (format T "Animator back-inside~%")))

(defun Animator-do-running (an-interactor new-obj-over event)
  "doesn't do anything"
  #+garnet-debug (declare (ignore new-obj-over event))
  #-garnet-debug (declare (ignore an-interactor new-obj-over event))
  (if-debug an-interactor (format T "Animator running~%")))

(defun Animator-explicit-stop (an-interactor)
  (if-debug an-interactor (format T "Animator explicit stop~%"))
  (Stop-Animator an-interactor))


;;; Animator interactor
;;
(Create-Schema 'animator-interactor
	       (:is-a inter:interactor)
	       (:name :First-Animator-interactor)
	       (:start-event NIL)	; doesn't start
	       (:start-where NIL) 
	       (:start-action NIL)
	       (:running-action NIL)
	       (:stop-action NIL)
	       (:abort-action NIL)
	       (:outside-action NIL)
	       (:back-inside-action NIL)

	       (:timer-handler 'NIL)	; fill this in
	       (:timer-repeat-wait 0.2)	; seconds

	       (:Go 'General-Go) 
	       (:Do-Start 'Animator-Do-Start)
	       (:Do-Running 'Animator-Do-Running)
	       (:Do-Explicit-Stop 'Animator-Explicit-Stop)
	       (:Do-Stop 'Animator-Do-Stop)
	       (:Do-Abort 'Animator-Do-Abort)
	       (:Do-Outside 'Animator-Do-Outside)
	       (:Do-Back-Inside 'Animator-Do-Back-Inside)
	       (:Do-Outside-Stop 'Animator-Do-Outside-Stop)
	       (:initialize #'Animator-Interactor-Initialize))

;; Need special destroy to kill process
(define-method :destroy-me animator-interactor (an-interactor
						&optional (erase T))
  (if-debug an-interactor
	    (format T "Animator special destroy ~s erase=~s~%" an-interactor
		    erase))
  (Kill-Timer-Process an-interactor)
  (call-prototype-method an-interactor erase))

;;;============================================================

(defun Anim-Bounce (anim)
  (let ((win (g-value anim :window))
	(x-inc (g-value anim :x-inc))
	(y-inc (g-value anim :y-inc))
	(obj (g-value anim :obj-to-change))
	)
    (unless obj
      (error "bounce animator-interactor but no :obj-to-change ~s"
	     anim))
    (unless win
      (error "bounce animator-interactor but no :window ~s"
	     anim))
    (let* ((width (g-value win :width))
	   (height (g-value win :height))
	   (x (+ x-inc (g-value obj :left)))
	   (y (+ y-inc (g-value obj :top)))
	   (w (g-value obj :width))
	   (h (g-value obj :height))
	   (r (+ x w))
	   (b (+ y h)))
      (if (< x 0)
	  (progn
	    (s-value anim :x-inc (abs x-inc))
	    (setq x 0))
	  ;; otherwise, check right
	  (when (> r width)
	    (s-value anim :x-inc (- (abs x-inc)))
	    (setq x (- width w))))
      (s-value obj :left x)

      (if (< y 0)
	  (progn
	    (s-value anim :y-inc (abs y-inc))
	    (setq y 0))
	  ;; otherwise, check bottom
	  (when (> b height)
	    (s-value anim :y-inc (- (abs y-inc)))
	    (setq y (- height h))))
      (s-value obj :top y))))

(defun Anim-Wrap (anim)
  (let ((win (g-value anim :window))
	(x-inc (g-value anim :x-inc))
	(y-inc (g-value anim :y-inc))
	(obj (g-value anim :obj-to-change))
	)
    (unless obj
      (error "wrap animator-interactor but no :obj-to-change ~s"
	     anim))
    (unless win
      (error "warp animator-interactor but no :window ~s"
	     anim))
    (let* ((width (g-value win :width))
	   (height (g-value win :height))
	   (x (+ x-inc (g-value obj :left)))
	   (y (+ y-inc (g-value obj :top))))
      (if (< x 0)
	  (setq x width)
	  ;; otherwise, check right
	  (when (> x width) (setq x 0)))
      (s-value obj :left x)

      (if (< y 0)
	  (setq y height)
	  ;; otherwise, check bottom
	  (when (> y height) (setq y 0)))
      (s-value obj :top y))))


(create-instance 'animator-bounce animator-interactor
		 (:x-inc 2)
		 (:y-inc 2)
		 (:timer-handler #'anim-bounce)
		 (:obj-to-change NIL))

(create-instance 'animator-wrap animator-interactor
		 (:x-inc 2)
		 (:y-inc 2)
		 (:timer-handler #'anim-wrap)
		 (:obj-to-change NIL))

