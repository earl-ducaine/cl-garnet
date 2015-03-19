;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-XASPERATE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$ 


(in-package :DEMO-XASPERATE)

(declaim (special *AGG* *WIN* *BIG-TEXT* *SCORE-WIN* *SCORE-AGG*
		  *PLAY-WIN* *PLAY-AGG* *PROTO-OBJECT* *SCORE-TEXT*))

(defvar *margin* 15)
(defvar *space* 2)
(defvar *hole2* nil)
(defvar *hole1* nil)
(defvar *inter* nil)
(defvar *score* 0)

(defmacro half(x) `(round (* ,x 0.5)))

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  (create-instance '*win* inter:interactor-window
    (:left 300)(:top 100)
    (:height 300)
    (:width  300)
    (:title "GARNET Xasperate")
    (:aggregate (create-instance '*agg* opal:aggregate)))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-XASPERATE"))
     (g-value *win* :destroy-hooks)))

  (opal:add-component *agg*
		      (create-instance '*big-text* opal:text
			(:string "XASPERATE")
			(:font (create-instance '*big-font* opal:font
				 (:size :very-large)))
			(:top (o-formula (half (- (gv *win* :height)
						  (gvl :height)))))
			(:left (o-formula (half (- (gv *win* :width)
						   (gvl :width)))))))
  (opal:update *win*)
  (inter:beep)
  ;;(stall for some amount of time..)
  (s-value *big-text* :visible NIL)
  (create-instance '*score-win* opal::window
    (:top *margin*)
    (:left *margin*)
    (:width  (o-formula (max (- (gv *win* :width) 30) 1)))
    (:height 23)
    (:parent *win*)
    (:aggregate (create-instance '*score-agg* opal:aggregate)))
  (opal:update *score-win*)
  (opal:add-component *score-agg*
		      (create-instance '*score-text* opal:text
			(:string (o-formula (princ-to-string (gvl :score))))
			(:score (setq *score* 0))
			(:top 3)
			(:left (o-formula (- (gv *score-win* :width) (gvl :width) 25)))))
  (create-instance '*play-win* inter:interactor-window
    (:double-buffered-p double-buffered-p)
    (:top (o-formula (+ (* 2 (gv *score-win* :top))
			(- (gv *score-win* :height)
			   (gv *score-win* :top-border-width)
			   (gv *score-win* :bottom-border-width))
			5)))
    (:left *margin*)
    (:width  (o-formula (- (gv *score-win* :width)
			   (gv *score-win* :left-border-width)
			   (gv *score-win* :right-border-width))))
    (:height (o-formula (max (- (gv *win* :height) (gvl :top) *margin*) 
			     1)))
    (:x-portion (o-formula (max (round (* (- (gvl :width) (* 3 *space*))
					  0.25))
				2)))
    (:y-portion (o-formula (max (round (* (- (gvl :height) (* 4 *space*))
					  0.20))
				2)))
    (:parent *win*)
    (:aggregate (create-instance '*play-agg* opal:aggregate)))

  (create-instance '*proto-object* opal:rectangle
    (:x-pos 0)	    ;; from 0 to 3  LEFT
    (:y-pos 0)	    ;; from 0 to 4  TOP
    (:x-size 1)	    ;; from 1 to 2
    (:y-size 1)	    ;; from 1 to 2
    (:filling-style opal:light-gray-fill)

    (:left (o-formula (* (gvl :x-pos) (+ (gv *play-win* :x-portion)
					 *space*))))
    (:top  (o-formula (* (gvl :y-pos) (+ (gv *play-win* :y-portion)
					 *space*))))
    (:width (o-formula
	     (let ((xsize (gvl :x-size)))
	       (+ (* xsize (gv *play-win* :x-portion))
		  (* (1- xsize) *space*)))))
    (:height (o-formula
	      (let ((ysize (gvl :y-size)))
		(+ (* ysize (gv *play-win* :y-portion))
		   (* (1- ysize) *space*))))))

  ;; The 4 little guys
  (dolist (i '(1 2))
    (dolist (j '(3 4))
      (opal:add-component *play-agg*
			  (create-instance nil *proto-object*
			    (:x-pos i)
			    (:y-pos j)))))

  ;; The 4 tall guys
  (dolist (i '(0 3))
    (dolist (j '(0 2))
      (opal:add-component *play-agg*
			  (create-instance nil *proto-object*
			    (:x-pos i)
			    (:y-pos j)
			    (:y-size 2)
			    (:filling-style opal:gray-fill)))))

  ;; The 1 wide guy
  (opal:add-component *play-agg*
		      (create-instance nil *proto-object*
			(:x-pos 1)
			(:y-pos 2)
			(:x-size 2)
			(:filling-style opal:gray-fill)))

  ;; The 1 big guy
  (opal:add-component *play-agg*
		      (create-instance nil *proto-object*
			(:x-pos 1)
			(:y-pos 0)
			(:x-size 2)
			(:y-size 2)
			(:filling-style opal:dark-gray-fill)))

  ;; The 'blank' guys...
  (setq *hole1* (create-instance nil *proto-object*
		  (:x-pos 0)
		  (:y-pos 4)))
  (setq *hole2* (create-instance nil *proto-object*
		  (:x-pos 3)
		  (:y-pos 4)))
  (opal:update *win*)

  (create-instance '*inter* inter:two-point-interactor
    (:window *play-win*)
    (:start-where T)
    (:line-p T)
    (:abort-if-too-small nil)
    (:running-action nil)
    (:final-function
     #'(lambda (an-interactor final-point-list)
	 (declare (ignore an-interactor))
	 (let* ((first-x (first final-point-list))
		(first-y (second final-point-list))
		(last-x  (third final-point-list))
		(last-y  (fourth final-point-list))
		(from-obj (opal:point-to-component *play-agg* first-x first-y)))
	   (when from-obj
	     (cond
	       ((opal:point-in-gob *hole1* last-x last-y)
		(do-move from-obj *hole1*))
	       ((opal:point-in-gob *hole2* last-x last-y)
		(do-move from-obj *hole2*))
	       ((eq (opal:point-to-component *play-agg* last-x last-y) from-obj)
		(if (adjacent? from-obj *hole1*)
		    (do-move from-obj *hole1*)
		    (do-move from-obj *hole2*)))))))))
  (format t "~%~%")
  (format t "Demo Xasperate!!!!!~%~%")
  (format t "Move objects with the left mouse button by either clicking on the object,~%")
  (format t "or -- if there is more than one legal move from an object -- dragging from~%")
  (format t "the object to its destination.~%~%")
  (format t "The object is to move the large dark rectangle so that it is resting on the~%")
  (format t "bottom of the grid.  This can be done in 86 moves.~%~%")
  (format t "This demo is a Garnet version of the game 'xasperate' found in X11tra.  There~%")
  (format t "are two differences, however:~%")
  (format t "   (1) Just clicking on an object moves it in this version; and~%")
  (format t "   (2) If you change the size of the window, the game pieces change size!~%~%")
  (format t "Good luck, and enjoy!!!!~%~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )

(defun adjacent? (obj1 obj2)
  (let ((xpos1 (g-value obj1 :x-pos))  (xsize1 (g-value obj1 :x-size))
	(xpos2 (g-value obj2 :x-pos))  (xsize2 (g-value obj2 :x-size))
	(ypos1 (g-value obj1 :y-pos))  (ysize1 (g-value obj1 :y-size))
	(ypos2 (g-value obj2 :y-pos))  (ysize2 (g-value obj2 :y-size)))
    (or  (and
	   (= xpos1 xpos2)
	   (or
	     (= ypos2 (+ ypos1 ysize1))
	     (= ypos1 (+ ypos2 ysize2))))
	 (and
	   (= ypos1 ypos2)
	   (or
	     (= xpos2 (+ xpos1 xsize1))
	     (= xpos1 (+ xpos2 xsize2)))))))

(defun do-move (obj hole)
  (let* ((xpos1 (g-value obj  :x-pos))  (xsize1 (g-value obj  :x-size))
	 (xpos2 (g-value hole :x-pos))  (xsize2 (g-value hole :x-size))
	 (ypos1 (g-value obj  :y-pos))  (ysize1 (g-value obj  :y-size))
	 (ypos2 (g-value hole :y-pos))  (ysize2 (g-value hole :y-size))
	 (ohole (if (eq hole *hole1*) *hole2* *hole1*))
	 (ohole-xpos (g-value ohole :x-pos))
	 (ohole-ypos (g-value ohole :y-pos))
	 (beep-needed? T)
	 (fix-other-hole? NIL)
	 temp)
    (if (= xpos1 xpos2)
     (if (= ypos2 (+ ypos1 ysize1))
	; code for block above hole, first check for ohole above, to right...
	(when (or (= xsize1 1)
		  (and (= ohole-xpos (1+ xpos1))
		       (= ohole-ypos ypos2)
		       (setq fix-other-hole? T)))
		(setq beep-needed? nil)
	        (s-value hole :y-pos ypos1)
		(if fix-other-hole? (s-value ohole :y-pos ypos1))
		(s-value obj :y-pos (1+ ypos1))
		(s-value *score-text* :score (incf *score*)))
	(if (= ypos1 (+ ypos2 ysize2))
	  ; code for block below hole, first check for ohole below, to right..
	  (when (or (= xsize1 1)
		    (and (= ohole-xpos (1+ xpos1))
		         (= ohole-ypos ypos2)
			 (setq fix-other-hole? T)))
		  (setq beep-needed? nil)
	          (s-value hole :y-pos (setq temp (1- (+ ypos1 ysize1))))
		  (if fix-other-hole? (s-value ohole :y-pos temp))
		  (s-value obj :y-pos (1- ypos1))
		  (s-value *score-text* :score (incf *score*)))))
     ; so not in same column...
    (if (= ypos1 ypos2)
     (if (= xpos2 (+ xpos1 xsize1))
	; code for block left of hole
	(when (or (= ysize1 1)
		  (and (= ohole-ypos (1+ ypos1))
		       (= ohole-xpos xpos2)
		       (setq fix-other-hole? T)))
		(setq beep-needed? nil)
	        (s-value hole :x-pos xpos1)
		(if fix-other-hole? (s-value ohole :x-pos xpos1))
		(s-value obj :x-pos (1+ xpos1))
		(s-value *score-text* :score (incf *score*)))
	(if (= xpos1 (+ xpos2 xsize2))
	  ; code for block right of hole
	  (when (or (= ysize1 1)
		    (and (= ohole-ypos (1+ ypos1))
		         (= ohole-xpos xpos2)
		         (setq fix-other-hole? T)))
		  (setq beep-needed? nil)
	          (s-value hole :x-pos (setq temp (1- (+ xpos1 xsize1))))
		  (if fix-other-hole? (s-value ohole :x-pos temp))
		  (s-value obj :x-pos (1- xpos1))
		  (s-value *score-text* :score (incf *score*)))))))
    (if beep-needed?
     (inter:beep))))

(defun do-stop()
  (opal:destroy *win*))
