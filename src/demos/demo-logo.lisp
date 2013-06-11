;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-LOGO; Base: 10 -*-
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
;;; Garnet logo designed by MaryJo Dowling at CMU
;;;

#|
============================================================
Change log:
   2/15/94 Andrew Mickish - Made fast implementation for Mac:
             - Put window at :top = 45
             - "GARNET" letters are XOR and fast-redraw
             - Other strings are redraw-type fast-redraw
             - DoColors sets the :fast-redraw-line-style of the other strings
   5/20/92 Brad Myers - When click, animation starts over with Star big
   4/14/92 Brad Myers - In black and white, star should be filled with white
   4/13/92 Brad Myers - started based on Demo-Fade
============================================================
|#



(in-package :DEMO-LOGO)

(declaim (special PANTONE192 PANTONE193 PANTONE194 PANTONE196
		  MYCOLOR1 MYCOLOR2 MYCOLOR3 MYCOLOR4 MYCOLOR5
		  PANTONE192-FILL PANTONE193-FILL PANTONE194-FILL
		  PANTONE196-FILL MYCOLOR1-FILL MYCOLOR2-FILL
		  MYCOLOR3-FILL MYCOLOR4-FILL MYCOLOR5-FILL FADER))

(create-instance 'pantone192 opal:color
  (:BLUE  0.35000002)
  (:GREEN  0.060000002)
  (:RED  1))

(create-instance 'pantone193 opal:color
  (:BLUE  0.23500001)
  (:GREEN  0)
  (:RED  0.885))

(create-instance 'pantone194 opal:color
  (:BLUE  0.10)
  (:GREEN  0)
  (:RED  0.66))

(create-instance 'pantone196 opal:color
  (:BLUE  0.94)
  (:GREEN  0)
  (:RED  1))

(create-instance 'mycolor1 opal:color
  (:BLUE  0.27500004)
  (:GREEN  0.014999986)
  (:RED  0.925))

(create-instance 'mycolor2 opal:color
  (:BLUE  0.7)
  (:GREEN  0.7)
  (:RED  1.0))

(create-instance 'mycolor3 opal:color
  (:BLUE  0.22)
  (:GREEN  0)
  (:RED  0.82))

(create-instance 'mycolor4 opal:color
  (:BLUE  0.84)
  (:GREEN  0.84)
  (:RED  1.0))
(create-instance 'mycolor5 opal:color
  (:BLUE  0.5)
  (:GREEN  0.5)
  (:RED  1.0))


(create-instance 'pantone192-fill opal:filling-style
		 (:foreground-color pantone192))
(create-instance 'pantone193-fill opal:filling-style
		 (:foreground-color pantone193))
(create-instance 'pantone194-fill opal:filling-style
		 (:foreground-color pantone194))
(create-instance 'pantone196-fill opal:filling-style
		 (:foreground-color pantone196))
(create-instance 'mycolor1-fill opal:filling-style
		 (:foreground-color mycolor1))
(create-instance 'mycolor2-fill opal:filling-style
		 (:foreground-color mycolor2))
(create-instance 'mycolor3-fill opal:filling-style
		 (:foreground-color mycolor3))
(create-instance 'mycolor4-fill opal:filling-style
		 (:foreground-color mycolor4))
(create-instance 'mycolor5-fill opal:filling-style
		 (:foreground-color mycolor5))

(defparameter new-point-list
 `(
   (,pantone192-fill
    (83 41 104 55 139 194 126 217 56 234 34 220 0 81 13 59 83 41) ;; L1
    )
   (,pantone194-fill
    (104 55 93 68 85 70 83 62 83 41 104 55) ;; L2
    )
   (,opal:no-fill
    (0 81 17 88 24 86 22 77 13 59 0 81) ;; L3
    )
   (,pantone194-fill
    (34 220 46 207 53 205 56 213 56 234 34 220) ;; L4
    )
   (,pantone194-fill
    (58 196 53 205 24 86 32 91 58 196) ;; L5
    )
   (,opal:no-fill
    (139 194 122 187 115 189 116 198 126 217 139 194) ;; L6
    )
   (,mycolor2-fill
    (107 184 115 189 85 70 81 79 107 184) ;; L7
    )
   (,mycolor1-fill
    (104 55 139 194 122 187 93 68 104 55) ;; L8
    )
   (,pantone193-fill
    (0 81 34 220 46 207 17 88 0 81) ;; L9
    )
   (,pantone193-fill
    (83 41 83 62 22 77 13 59 83 41) ;; L10
    )
   (,opal:no-fill
    (126 217 116 198 56 213 56 234 126 217) ;; L11
    )
   (,mycolor4-fill
    (85 70 24 86 32 91 81 79 85 70) ;; L12
    )
   (,mycolor5-fill
    (115 189 53 205 58 196 107 184 115 189) ;; L13
    )
   (,mycolor5-fill
    (115 189 122 187 93 68 85 70 115 189) ;; L14
    )
   (,opal:white-fill
    (93 68 83 62 85 70 93 68) ;; L15
    )
   (,opal:white-fill
    (84 42 55 32 82 38 73 31 82 36 78 0 86 38 92 24
     88 40 114 44 88 42 92 68 84 42) ;; L16, star
    )
   (,pantone193-fill
    (53 205 115 189 116 198 56 213 53 205) ;; L17
    )
   (,mycolor1-fill
    (46 207 53 205 24 86 17 88 46 207) ;; L18
    )
   (,mycolor1-fill
    (24 86 22 77 83 62 85 70 24 86) ;; L19
    )
   ))



(defparameter polylinelist NIL)

(defun Create-Logo (agg)
  (let ((cnt 0))
    (setq polylinelist NIL)
    (dolist (i new-point-list)
      (let ((color (car i))
	    pl)
	(incf cnt)
	(setq pl (create-instance NIL opal:polyline
				  (:point-list (cadr i))
				  (:copy-point-list (cadr i))
				  (:filling-style
				   ;; if color screen, then use defined color
				   (if (g-value opal:color :color-p)
				       color
				       ;; else if black and white screen,
				       ;; use white fill or no fill
				       (if (eq color opal:white-fill)
					   opal:white-fill
					   NIL)))))
	(push pl polylinelist)
	(opal:add-component agg pl)))
    (setq polylinelist (nreverse polylinelist))))

(defun shrink-logo (amount numtimes win)
  (let ((inc (/ (- 1 (/ 1.0 amount)) numtimes)))
    (dotimes (i numtimes)
      (let ((npl new-point-list)
	    (thisinc (- 1.0 (* inc (1+ i))))
	    thispl)
	(dolist (pl polylinelist)
	  (setq thispl (car npl))
	  (setq npl (cdr npl))
	  (let ((l nil))
	    (dolist (p (cadr thispl))
	      (push (round (* thisinc p)) l))
	    (setq l (nreverse l))
	    (s-value pl :point-list l))))
      (opal:update win))))

(defun reset-logo-size ()
  (dolist (pl polylinelist)
    (s-value pl :point-list (g-value pl :copy-point-list))))

(defparameter top-agg NIL)
(defparameter font (opal:get-standard-font NIL :bold :very-large))

(defparameter white-to-black NIL)
(defparameter black-to-white NIL)

(defparameter violet-to-red NIL)
(defparameter red-to-violet NIL)
(defparameter rgbvalues 
  '((1.00 0.00 0.52) (1.00 0.01 0.85) (0.82 0.03 1.00) (0.49 0.01 1.00) 
    (0.14 0.01 1.00) (0.00 0.19 1.00) (0.01 0.52 1.00) (0.03 0.86 1.00)
    (0.00 1.00 0.82) (0.02 1.00 0.49) (0.00 1.00 0.15) (0.18 1.00 0.01)
    (0.52 1.00 0.02) (0.83 1.00 0.00) (1.00 0.81 0.00) (1.00 0.48 0.00)
    (1.00 0.07 0.00)))

(defparameter GarnetStringList
  '("Generating an" "Amalgam of" "Real-time," "Novel" "Editors and" "Toolkits"))

(defparameter FirstLetterList NIL)
(defparameter StringList NIL)
(defparameter FirstLetterObjs NIL)
(defparameter FirstLetterOffset 0)
(defparameter maxwh 0)
(defparameter objs-list NIL)

(defparameter char-origin-x 35)
(defparameter char-origin-y 60)

(defun Create-Lists (strings)
  (setq FirstLetterList NIL)
  (setq StringList NIL)
  (dolist (s (reverse strings))
    (push (subseq s 0 1) FirstLetterList)
    (push (subseq s 1) StringList)))


(defun Create-Color-List (numcolors)
  (let ((inc (floor 100 (1- numcolors)))
	(val 0)
	l)
    (cond ((g-value opal:color :color-p)
	   (dotimes (i (1- numcolors))
	     (let* ((triplet (nth val rgbvalues))
		    (red (first triplet))
		    (green (second triplet))
		    (blue (third triplet)))
	       (push (create-instance NIL opal:line-style
			(:foreground-color
			 (create-instance NIL opal:color
			    (:red red) (:green green) (:blue blue))))
		     l)
	       (incf val 1)))
           (push opal:default-line-style l))
	  (T (dotimes (i numcolors)
	       (push (cond ((= i 0) opal:white-fill)
			   ((= i (1- numcolors)) opal:black-fill)
			   (T (opal:halftone val)))
		     l)
	       (incf val inc))))
    (setq black-to-white l)
    (setq white-to-black (reverse l))))

(defun Create-String-Objs (init-x init-y font agg)
  (let ((cur-y init-y)
	objs-list obj)
    (dolist (s StringList)
      (setq obj
	    (create-instance NIL opal:text (:string s)
			     (:left (+ FirstLetterOffset init-x))
			     (:top cur-y)
			     (:font font)
			     (:visible NIL)
                             (:fast-redraw-p :redraw)
                             (:fast-redraw-line-style opal:white-line)))
      (opal:add-component agg obj)
      (push obj objs-list)
      (incf cur-y maxwh))
    (Reverse objs-list)))

(defun Create-First-Letter-Objs (init-x init-y font agg)
  (let ((maxw 0) (cur-x init-x)
	w h objs-list obj)
    (dolist (s FirstLetterList)
      (setq obj
	    (create-instance NIL opal:text (:string s)
			     (:left 0) ; left is set below
			     (:top init-y)
			     (:font font)
			     (:visible T)
                             #+apple (:fast-redraw-p T)
                             #+apple (:draw-function :xor)))
      
      (when (> (setq h (g-value obj :height)) maxwh)
	(setq maxwh h))
      (when (> (setq w (g-value obj :width)) maxwh)
	(setq maxwh w))
      (when (> w maxw)
	(setq maxw w))
      (opal:add-component agg obj)
      (push obj objs-list))
    (setq objs-list (reverse objs-list))
    ; now set the lefts
    (dolist (obj objs-list)
      (s-value obj :left cur-x)
      (s-value obj :initial-x cur-x) 
      (s-value obj :initial-y init-y) 
      (incf cur-x maxwh))
    (setq FirstLetterObjs objs-list)
    (setq FirstLetterOffset (+ 2 maxw))))



(defparameter Fader NIL)
(defparameter win NIL)
;;; status cycles between :beginning, :little and :letters-are-down
(defparameter status :beginning) 

;;; Restore to :beginning
(defun Reset ()
  (dolist (obj FirstLetterObjs)
    (s-value obj :left (g-value obj :initial-x))
    (s-value obj :top (g-value obj :initial-y))
    (s-value obj :visible NIL))
  (dolist (obj objs-list)
    (s-value obj :visible NIL))
  (reset-logo-size)
  (opal:update win)
  (setq status :beginning))

(defun Circle-Down (numtimes)
  (let ((inc (/ PI 2 (1- numtimes)))
	(angle 0)
	offset)
    (dotimes (i numtimes)
      (setq offset 0)
      (dolist (obj FirstLetterObjs)
	(s-value obj :top
		 (+ char-origin-y (Round (* offset (Sin angle)))))
	(s-value obj :left
		 (+ char-origin-x (Round (* offset (Cos angle)))))
	(incf offset maxwh))
      (opal:update win)
      (if (= i (- numtimes 2))
	  ; then next time will be last
	  (setq angle (/ PI 2))
	  (incf angle inc)))))

(defun DoFade ()
  (s-value fader :visible T)
  (dolist (lin objs-list)
    (s-value fader :left (g-value lin :left))
    (s-value fader :top (g-value lin :top))
    (s-value fader :width (g-value lin :width))
    (s-value fader :height (g-value lin :height))
    (s-value lin :visible T)
    (dolist (color white-to-black)
      (s-value fader :filling-style color)
      (opal:update win)))
  (s-value fader :visible NIL)
  (opal:update win))

(defun DoColors (obj)
  (s-value obj :visible T)
  (dolist (color white-to-black)
    ;; Trick update into just drawing the object again (during the erase phase)
    (s-value obj :fast-redraw-line-style (g-value obj :line-style))
    (s-value obj :line-style color)
    (opal:update win)
    (sleep #+hpux .08 #-hpux 0.01))
  (s-value obj :fast-redraw-line-style opal:white-line)
  ;(s-value obj :line-style opal:default-line-style)
  (opal:update win))

(defun Go-To-Next-Status ()
  (case status
    (:beginning (shrink-logo 2 5 win)
		(dolist (obj FirstLetterObjs)
		  (s-value obj :visible T))
		(opal:update win)
		(setf status :little))
    (:little 
     (Circle-Down 30)
     (if (g-value opal:color :color-p)
	 (dolist (obj objs-list)
	   (DoColors obj))
	 (DoFade))
     (setf status :letters-are-down))
    (:letters-are-down (reset))
    (T (error "status bad ~s" status))))
     
(defun Re-Animate ()
  (loop
   (go-to-next-status)
   (if (eq status :letters-are-down)
     (return-from re-animate)
     (sleep 1))))

(defun Do-Go (&key (strings GarnetStringList)
		   (numFades 17)
		   dont-enter-main-event-loop
		   (double-buffered-p T))
  (Create-Lists strings) ; convert string list into appropriate form
  (setq win (create-instance NIL inter:interactor-window
		      (:title "Garnet Logo")
		      (:left 0)(:top #-apple 0 #+apple 45)
                      (:width 270)(:height 235)
		      (:double-buffered-p double-buffered-p)
		      (:aggregate (setq top-agg
					(create-instance NIL opal:aggregate)))))
  (Create-Logo top-agg)
  (opal:update win)
  (Create-Color-List numFades)
  (sleep 3)
  (setq objs-list nil)       ;; needed if you stop
  (setq FirstLetterObjs nil) ;; and restart demo-logo.
  (Go-To-Next-Status) ;; will shrink
  (Create-First-Letter-Objs char-origin-x char-origin-y font top-agg)
  (setq objs-list (create-string-objs char-origin-x char-origin-y
				      font top-agg))
  (unless (g-value opal:color :color-p)
    (create-instance 'Fader Opal:Rectangle
		     (:visible NIL)
		     (:filling-style NIL)(:Line-style NIL)
		     (:draw-function :AND))
    (opal:add-component top-agg fader))
  (opal:update win)
  (sleep 2)
  (Go-To-Next-Status) ;; will circle-down and blink
  (create-instance 'starter inter:button-interactor
		   (:start-where `(:in ,win))
		   (:start-event '(:any-keyboard :any-mousedown))
		   (:window win)
		   (:continuous NIL)
		   (:final-function
		    #'(lambda (&rest args)
			(declare (ignore args))
			(Go-To-Next-Status))))
  "Press in window to start, press again to reset"
  (unless dont-enter-main-event-loop
      (inter:main-event-loop)))

(defun Do-Stop ()
  (setf status :beginning)
  (opal:destroy win))
