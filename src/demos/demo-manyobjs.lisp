;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MANYOBJS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains demo code that creates a lot of objects, for testing
;;; if Opal and the Interactors can lots of objects.
;;;
;;; This is intended as a test and demonstration of the Garnet system
;;; 
;;; ** Call (Do-Go :number-of-rectangles n) to start and (Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers

#|
============================================================
Change log:
       05/29/94 Marty Geier - Changed window position in do-go
       05/27/92 Ed Pervin - The latest CMUCL requires that the
                            argument to random be declared an integer.
       03/25/92 Andrew Mickish - Get-Values ---> G-Value
       12/05/91 Andrew Mickish - Added dzg's change to Move function, removed
                  'time' calls apparently for debugging.
	6/27/90 Ed Pervin - Made boxes not be scrambled when
			    window changes size.
        8/18/89 Brad Myers - Created
============================================================
|#

(in-package :DEMO-MANYOBJS)

(declaim (special BOXANDARROW))

(defvar *test-debug* NIL)

(defparameter Win NIL) ; the window that this will all be in
(defparameter GloSwitch NIL) ; dummy object

(defparameter ObjType Opal:Rectangle) ; user can change this to
				      ; Opal:Roundtangle, etc.


(defun x-center-me-in (obj)
  (+ (gv obj :Left)
     (floor (- (gv obj :width)(gvl :width)) 2)))

(defun y-center-me-in (obj)
  (+ (gv obj :top)
     (floor (- (gv obj :height)(gvl :height)) 2)))

 
(create-instance 'BoxAndArrow opal:aggregadget
	(:line-type Opal:thin-line)
	(:fill-type Opal:no-fill)
	(:prev-item NIL)
	(:name "Obj1")
	(:parts `((:outline ,ObjType
			    (:box ,(o-formula
				      (progn
				        (gv GloSwitch :selected) ;just set up
			   		   			;a dependency
					(list
					  (random 
					    (- (the integer (g-value Win :width)) 50)) ;left
					  (random
					    (- (the integer (g-value Win :height)) 50)) ;top
					  50 50))))
			    (:left ,(o-formula (first (gvl :box))))
			    (:top ,(o-formula (second (gvl :box))))
			    (:width 50)
			    (:height 50)
			    (:line-style ,(o-formula (gvl :parent :line-type)))
			    (:filling-style ,(o-formula (gvl :parent :fill-type)))
			    (:select-outline-only NIL)
			    ;; (:draw-function :xor)
			    ;; (:fast-redraw-p T)
			    )
		  (:arrow ,opal:line
			  (:visible ,(o-formula (gvl :parent :prev-item)))
			  (:x1 ,(o-formula (opal:gv-center-x
					    (gvl :parent :outline))))
			  (:y1 ,(o-formula (opal:gv-center-y
					    (gvl :parent :outline))))
			  (:x2 ,(o-formula (opal:gv-center-x
					    (gvl :parent :prev-item :outline))))
			  (:y2 ,(o-formula (opal:gv-center-y
					    (gvl :parent :prev-item :outline))))
			  ;; (:draw-function :xor)
			  ;; (:fast-redraw-p T)
			  )
		  (:label ,opal:text
			  (:string ,(o-formula (gvl :parent :name)))
			  (:left
			   ,(o-formula (x-center-me-in (gvl :parent :outline))))
			  (:top
			   ,(o-formula (y-center-me-in (gvl :parent
							    :outline))))
			  ;; (:draw-function :xor)
			  ;; (:fast-redraw-p T)
			  ))))

(defparameter agg NIL)
(defparameter prev NIL)

(defun do-go (&key (number-of-rectangles 5) dont-enter-main-event-loop double-buffered-p)
"Creates number-of-rectangles rectangles attached by lines.  Lots of formulas.
Good values of number-of-rectangles are 3..50"
  (let (obj)
    (setf Win (create-instance NIL inter:interactor-window (:left 10) (:top 40)
			 (:width 550) (:height 450)
 			 (:title "GARNET Many OBJECTS") (:icon-title "Many")
                         (:double-buffered-p double-buffered-p)
			 (:aggregate
			  (setf agg (create-instance NIL opal:aggregate
						     (:left 0)(:top 0)
						     (:width 550)(:height 450))))))
    (setf GloSwitch (create-instance NIL Opal:rectangle (:left 0)
				     (:top 0)(:width 20)(:height 20)
				     (:filling-style opal:gray-fill)))
    (opal:add-component agg GloSwitch)

    (setq prev NIL)
    (dotimes (i number-of-rectangles)
      (setq obj (create-instance NIL BoxAndArrow
		   (:prev-item prev)
		   (:name (concatenate 'string "Obj" (prin1-to-string i)))))
      (opal:add-component agg obj)
      (setq prev obj))

    (create-instance 'inter1 inter:move-grow-interactor
		     (:start-where `(:leaf-element-of ,agg :type ,ObjType))
		     (:window Win))
    (create-instance 'inter2 inter:button-interactor
		     (:window Win)
		     (:start-where `(:in ,GloSwitch))
		     (:start-event :rightdown)
		     (:continuous NIL)
		     (:how-set :toggle))
    (opal:update Win)

    (format T "~%DEMO-MANYOBJS:
  Press on any object with the left button to start moving it.
  Press on gray rectangle with right button to generate new random
  locations for all objects.
  Execute the function (Demo-Manyobjs:Move n) to move an object n times (e.g., 100)~%")
    )
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
)
#|
(Defun Move (n)
  (let* ((obj (third (g-value agg :components)))
	 (outline (g-value obj :outline))
	 (box (g-value outline :box)))
    (dotimes (i n)
      (setf (first box) (* i 4))
      (mark-as-changed outline :box)
      (opal:update win))))
|#

;; dzg - for new version of KR
(defun Move (n)
  (let ((outline (g-value (third (g-value agg :components)) :outline)))
    (dotimes (i n)
      (s-value outline :left (* i 4))
      (opal:update win))))


(defun do-stop ()
  (opal:destroy win))
