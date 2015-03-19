;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Arrow-line and Double-arrow-line
;;;
;;;
;;; Arrow-line
;;; ==========
;;; Features and operation of the arrow-line:
;;;   1)  An arrowhead appears at one end of the line.
;;;   2)  The endpoints of the arrow-line may be customized formulae which
;;;       depend on the position of other objects.  Such constraints would
;;;       cause the arrow-line to stay attached to the specified objects
;;;       when the objects are moved.
;;;   3)  No interactors are defined in the arrow-line prototype.
;;;
;;; Customizable slots:  (see the documentation for lines and arrowheads in
;;;                       the Opal manual)
;;;   1)  :x1, :y1 - source end point
;;;   2)  :x2, :y2 - end point with the arrowhead
;;;   3)  :line-style - thickness of all lines (default: line-0)
;;;   4)  :filling-style - filling of arrowhead (default: no-fill)
;;;   5)  :open-p - whether arrowhead is open or not  (default: T)
;;;   6)  To set other parameters of the arrowhead (such as :length and
;;;       :diameter), the arrowhead object must be accessed through the
;;;       slot :arrowhead.
;;;
;;; Note:  If the line is too short for an arrowhead to be drawn at one "end",
;;;        the arrowhead is drawn anyway (there is no special handling for
;;;        this case).
;;;
;;;
;;; Double-Arrow-line
;;; =================
;;; Features and operation of the double-arrow-line:
;;;   1)  Arrowheads may appear at either or both ends of the line.
;;;   2)  The slot determining the location of the arrowheads may be set
;;;       directly.  This feature is convenient for use in editors where the
;;;       user may turn on and off the arrowheads.
;;;   3)  The endpoints of the double-arrow-line may be customized formulae
;;;       which depend on the position of other objects.  Such constraints
;;;       would cause the arrows to stay attached to the specified objects
;;;       when the objects are moved.
;;;   4)  No interactors are defined in the double-arrow-line prototype.
;;;
;;; Customizable slots:  (see the documentation for lines and arrowheads in
;;;                       the Opal manual)
;;;   1)  :x1, :y1 - one end point
;;;   2)  :x2, :y2 - other end-point
;;;   3)  :line-style - thickness of all lines (default: line-0)
;;;   4)  :filling-style - filling of arrowheads (default: no-fill)
;;;   5)  :open-p - whether arrowheads are open or not  (default: T)
;;;   6)  :arrowhead-p  - where the arrow heads should be.  Legal values are:
;;;             0          - no arrowheads
;;;             1          - arrowhead at end-point 1
;;;             2          - arrowhead at end-point 2
;;;             3          - arrowhead at both end-points
;;;             (default = 3)
;;;   7)  To set other parameters of the arrowheads (such as :length and
;;;       :diameter), the arrowhead objects must be accessed through the
;;;       slots :arrowhead1 and :arrowhead2 of the double-arrow-line object.
;;;   8)  The default formulae in the arrowheads may also be overriden
;;;       (e.g., for two arrowheads with different :filling-styles).
;;;
;;; Note:  If only one arrowhead is desired, Arrow-line should be used.
;;;
;;; Arrow-line and Double-arrow-line demo
;;; =====================================
;;;   This module contains a function which creates a window with an arrow-line
;;;   and a double-arrow-line object in the window.  There are extra
;;;   interactors defined so that the arrows may be moved with the mouse.
;;;   To run the demo, enter (GARNET-GADGETS:arrow-line-go).  To stop, enter
;;;   (GARNET-GADGETS:arrow-line-stop).
;;;
;;;  Designed and written by Brad Myers

;;;
;;; CHANGE LOG:
;;;
;;; 02/23/93  Andrew Mickish - Added :line-p slots
;;; 12/14/92  Andrew Mickish - Added type and parameter declarations
;;; 07/15/92  Andrew Mickish - Changed :visible formulas of arrowheads in
;;;             DOUBLE-ARROW-LINE to consider visibility of parent
;;; 02/17/92  Andrew Mickish - Added :maybe-constant lists
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Arrow-Line Double-Arrow-Line)))
#+garnet-test
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(arrow-line-go arrow-line-stop arrow-line-win arrow-line-agg)))

;;
;; Functions to determine whether the point (x,y) is inside the object
;;
(defun ArrowPointInGob (obj x y)
  (when (call-prototype-method obj x y)
    (or (let ((lin (g-value obj :line)))
	  (KR-Send lin :point-in-gob lin x y))
	(let ((hed (g-value obj :arrowhead)))
	  (KR-Send hed :point-in-gob hed x y)))))

(defun Arrow2PointInGob (obj x y)
  (when (call-prototype-method obj x y)
    (or (let ((lin (g-value obj :line)))
	  (KR-Send lin :point-in-gob lin x y))
	(let ((hed (g-value obj :arrowhead1)))
	  (KR-Send hed :point-in-gob hed x y))
	(let ((hed (g-value obj :arrowhead2)))
	  (KR-Send hed :point-in-gob hed x y)))))

;;;
;;;  ARROW-LINE
;;;

(create-instance 'ARROW-LINE opal:aggregadget
   :declare ((:parameters :x1 :y1 :x2 :y2 :open-p :line-style :filling-style)
	     (:type (integer :x1 :y1 :x2 :y2)
		    (kr-boolean :open-p)
		    ((or (is-a-p opal:line-style) null) :line-style)
		    ((or (is-a-p opal:filling-style) null) :filling-style))
	     (:maybe-constant :x1 :y1 :x2 :y2 :open-p :line-style
			      :filling-style :visible))
   ; Customizable slots
   (:X1 0)
   (:Y1 0)
   (:X2 20)
   (:Y2 20)
   (:line-style opal:default-line-style)
   (:open-p T)
   (:filling-style NIL)

   ; Generally non-customizable slots
   (:line-p T)
   (:point-in-gob #'ArrowPointInGob)
   (:left (o-formula (min (gvl :line :left) (gvl :arrowhead :left))))
   (:top (o-formula (min (gvl :line :top) (gvl :arrowhead :top))))
   (:width (o-formula (let ((line-right (+ (gvl :line :left)
					   (gvl :line :width)))
			    (arrow-right (+ (gvl :arrowhead :left)
					    (gvl :arrowhead :width))))
			(- (max line-right arrow-right) (gvl :left)))))
   (:height (o-formula (let ((line-bottom (+ (gvl :line :top)
					     (gvl :line :height)))
			     (arrow-bottom (+ (gvl :arrowhead :top)
					      (gvl :arrowhead :height))))
			 (- (max line-bottom arrow-bottom) (gvl :top)))))
   (:parts
    `((:line ,Opal:Line
	     (:x1 ,(o-formula (gv (kr-path 0 :parent) :x1)))
	     (:y1 ,(o-formula (gv (kr-path 0 :parent) :y1)))
	     (:x2 ,(o-formula
		    (if (and (gv (kr-path 0 :parent :arrowhead) :open-p)
			     (null (gv (kr-path 0 :parent :arrowhead)
				       :filling-style)))
			; then go to head
			(gv (kr-path 1 :parent) :x2)
			; else go to connect-x
			(gv (kr-path 0 :parent :arrowhead) :connect-x))))
	     (:y2 ,(o-formula
		    (if (and (gv (kr-path 0 :parent :arrowhead) :open-p)
			     (null (gv (kr-path 0 :parent :arrowhead)
				       :filling-style)))
			; then go to head
			(gv (kr-path 1 :parent) :y2)
			; else go to connect-y
			(gv (kr-path 0 :parent :arrowhead) :connect-y))))
	     (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style))))
      (:arrowhead ,Opal:Arrowhead
             (:constant (:length :diameter))
	     (:from-x ,(o-formula (gv (kr-path 0 :parent) :x1)))
	     (:from-y ,(o-formula (gv (kr-path 0 :parent) :y1)))
	     (:head-x ,(o-formula (gv (kr-path 0 :parent) :x2)))
	     (:head-y ,(o-formula (gv (kr-path 0 :parent) :y2)))
	     (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))
	     (:open-p ,(o-formula (gv (kr-path 0 :parent) :open-p)))
	     (:filling-style ,(o-formula (gv (kr-path 0 :parent)
					     :filling-style)))))))

;;;
;;;  DOUBLE-ARROW-LINE
;;;

(create-instance 'DOUBLE-ARROW-LINE opal:aggregadget
   :declare ((:parameters :x1 :y1 :x2 :y2 :arrowhead-p :open-p
			  :line-style :filling-style)
	     (:type (integer :x1 :y1 :x2 :y2)
		    ((member 0 1 2 3) :arrowhead-p)
		    (kr-boolean :open-p)
		    ((or (is-a-p opal:line-style) null) :line-style)
		    ((or (is-a-p opal:filling-style) null) :filling-style))
	     (:maybe-constant :x1 :y1 :x2 :y2 :arrowhead-p :open-p
			      :line-style :filling-style :visible))
   ;; Customizable slots
   (:X1 0)
   (:Y1 0)
   (:X2 40)
   (:Y2 40)
   (:line-style opal:default-line-style)
   (:open-p T)
   (:filling-style NIL)
   (:arrowhead-p 3)      ; legal values are:
			 ;  0          - no arrowheads
			 ;  1          - arrowhead at end-point 1
			 ;  2          - arrowhead at end-point 2
			 ;  3          - arrowhead at both end-points

   ; Generally non-customizable slots
   (:line-p T)
   (:left (o-formula (min (gvl :line :left)
			  (gvl :arrowhead1 :left)
			  (gvl :arrowhead2 :left))))
   (:top (o-formula (min (gvl :line :top)
			 (gvl :arrowhead1 :top)
			 (gvl :arrowhead2 :top))))
   (:width (o-formula (let ((line-right (+ (gvl :line :left)
					   (gvl :line :width)))
			    (arrow1-right (+ (gvl :arrowhead1 :left)
					     (gvl :arrowhead1 :width)))
			    (arrow2-right (+ (gvl :arrowhead2 :left)
					     (gvl :arrowhead2 :width))))
			(- (max line-right arrow1-right arrow2-right)
			   (gvl :left)))))
   (:height (o-formula (let ((line-bottom (+ (gvl :line :top)
					     (gvl :line :height)))
			     (arrow1-bottom (+ (gvl :arrowhead1 :top)
					       (gvl :arrowhead1 :height)))
			     (arrow2-bottom (+ (gvl :arrowhead2 :top)
					       (gvl :arrowhead2 :height))))
			 (- (max line-bottom arrow1-bottom arrow2-bottom)
			    (gvl :top)))))
   (:point-in-gob #'Arrow2PointInGob)
   (:arrow1-p (o-formula (case (gvl :arrowhead-p)
			   (0 NIL)
			   (1 T)
			   (2 NIL)
			   (3 T))))
   (:arrow2-p (o-formula (case (gvl :arrowhead-p)
			      (0 NIL)
			      (1 NIL)
			      (2 T)
			      (3 T))))
   (:parts
    `((:line ,Opal:Line
	     (:x1 ,(o-formula
		    (if (or (not (gv (kr-path 0 :parent) :arrow1-p))
			    (and (gv (kr-path 1 :parent :arrowhead1) :open-p)
				 (null (gv (kr-path 1 :parent :arrowhead1)
					   :filling-style))))
			; then go to head
			(gv (kr-path 0 :parent) :x1)
			; else go to connect-x
			(gv (kr-path 1 :parent :arrowhead1) :connect-x))))
	     (:y1 ,(o-formula
		    (if (or (not (gv (kr-path 0 :parent) :arrow1-p))
			    (and (gv (kr-path 1 :parent :arrowhead1) :open-p)
				 (null (gv (kr-path 1 :parent :arrowhead1)
					    :filling-style))))
			; then go to head
			(gv (kr-path 0 :parent) :y1)
			; else go to connect-y
			(gv (kr-path 1 :parent :arrowhead1) :connect-y))))
	     (:x2 ,(o-formula
		    (if (or (not (gv (kr-path 0 :parent) :arrow2-p))
			    (and (gv (kr-path 1 :parent :arrowhead2) :open-p)
				 (null (gv (kr-path 1 :parent :arrowhead2)
					   :filling-style))))
			; then go to head
			(gv (kr-path 0 :parent) :x2)
			; else go to connect-x
			(gv (kr-path 1 :parent :arrowhead2) :connect-x))))
	     (:y2 ,(o-formula
		    (if (or (not (gv (kr-path 0 :parent) :arrow2-p))
			    (and (gv (kr-path 1 :parent :arrowhead2) :open-p)
				 (null (gv (kr-path 1 :parent :arrowhead2)
					   :filling-style))))
			; then go to head
			(gv (kr-path 0 :parent) :y2)
			; else go to connect-y
			(gv (kr-path 1 :parent :arrowhead2) :connect-y))))
	     (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style))))
      (:arrowhead1 ,Opal:Arrowhead
             (:constant (:length :diameter))
	     (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
				       (gv (kr-path 0 :parent) :arrow1-p))))
	     (:from-x ,(o-formula (gv (kr-path 0 :parent) :x2)))
	     (:from-y ,(o-formula (gv (kr-path 0 :parent) :y2)))
	     (:head-x ,(o-formula (gv (kr-path 0 :parent) :x1)))
	     (:head-y ,(o-formula (gv (kr-path 0 :parent) :y1)))
	     (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))
	     (:open-p ,(o-formula (gv (kr-path 0 :parent) :open-p)))
	     (:filling-style ,(o-formula (gv (kr-path 0 :parent) :filling-style))))
      (:arrowhead2 ,Opal:Arrowhead
             (:constant (:length :diameter))
	     (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
				       (gv (kr-path 0 :parent) :arrow2-p))))
	     (:from-x ,(o-formula (gv (kr-path 0 :parent) :x1)))
	     (:from-y ,(o-formula (gv (kr-path 0 :parent) :y1)))
	     (:head-x ,(o-formula (gv (kr-path 0 :parent) :x2)))
	     (:head-y ,(o-formula (gv (kr-path 0 :parent) :y2)))
	     (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))
	     (:open-p ,(o-formula (gv (kr-path 0 :parent) :open-p)))
	     (:filling-style ,(o-formula (gv (kr-path 0 :parent)
					     :filling-style)))))))



;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test
(defun arrow-line-go ()
  (create-instance 'arrow-line-win inter:interactor-window
		   (:left 700) (:top 10) (:width 300) (:height 300)
		   (:aggregate
		    (create-instance 'arrow-line-agg opal:aggregate)))
  ;;
  ;; Buttons which change the appearance of the arrow lines
  ;;
  (create-instance 'arrowbutton Opal:text
		   (:selected 2)
		   (:string (o-formula (case (gvl :selected)
					 (0 "No Arrows")
					 (1 "At initial end")
					 (2 "At final end")
					 (3 "At both ends")
					 (T (Error "bad value")))))
		   (:left 20)(:top 10))
  (create-instance 'arrowfillbutton Opal:text
		   (:selected 0)
		   (:fill (o-formula (case (gvl :selected)
					 (0 NIL)
					 (1 Opal:Black-Fill)
					 (2 Opal:Light-Gray-Fill)
					 (T (Error "bad value")))))
		   (:string (o-formula (case (gvl :selected)
					 (0 "No Fill")
					 (1 "Black Fill")
					 (2 "Light-Gray Fill")
					 (T (Error "bad value")))))
		   (:left 20)(:top 30))
  (create-instance 'arrowOpenpbutton Opal:text
		   (:selected T)
		   (:string (o-formula (if (gvl :selected)
					   "Open"
					   "Not Open")))
		   (:left 200)(:top 10))

  ;;
  ;; Objects that the arrows are attached to
  ;;
  (create-instance 'single-circle opal:circle
     (:box (list 10 100 20 20))  ; changed by the interactor
     (:left (o-formula (first (gvl :box))))
     (:top (o-formula (second (gvl :box))))
     (:width (o-formula (third (gvl :box))))
     (:height (o-formula (fourth (gvl :box))))
     (:right (o-formula (+ (gvl :left) (gvl :width))))
     (:center-y (o-formula (+ (gvl :top)
			      (floor (gvl :height) 2)))))
  (create-instance 'single-rect opal:rectangle
     (:box (list 150 110 30 30))  ; changed by the interactor
     (:left (o-formula (first (gvl :box))))
     (:top (o-formula (second (gvl :box))))
     (:width (o-formula (third (gvl :box))))
     (:height (o-formula (fourth (gvl :box)))))
  (create-instance 'double-circle opal:circle
     (:box (list 10 170 20 20))  ; changed by the interactor
     (:left (o-formula (first (gvl :box))))
     (:top (o-formula (second (gvl :box))))
     (:width (o-formula (third (gvl :box))))
     (:height (o-formula (fourth (gvl :box))))
     (:right (o-formula (+ (gvl :left) (gvl :width))))
     (:center-y (o-formula (+ (gvl :top)
			      (floor (gvl :height) 2)))))
  (create-instance 'double-rect opal:rectangle
     (:box (list 150 180 30 30))  ; changed by the interactor
     (:left (o-formula (first (gvl :box))))
     (:top (o-formula (second (gvl :box))))
     (:width (o-formula (third (gvl :box))))
     (:height (o-formula (fourth (gvl :box)))))

  ;;
  ;; Definition of the arrow line instances
  ;;
  (create-instance 'myarrow1 Arrow-line
		   (:x1 (o-formula (gv single-circle :right)))
		   (:y1 (o-formula (gv single-circle :center-y)))
		   (:x2 (o-formula (gv single-rect :left)))
		   (:y2 (o-formula (gv single-rect :top)))
		   (:open-p (o-formula (gv arrowOpenpbutton :selected)))
		   (:filling-style (o-formula (gv arrowfillbutton :fill))))

  (create-instance 'myarrow2 Double-arrow-line
		   (:x1 (o-formula (gv double-circle :right)))
		   (:y1 (o-formula (gv double-circle :center-y)))
		   (:x2 (o-formula (gv double-rect :left)))
		   (:y2 (o-formula (gv double-rect :top)))
		   (:open-p (o-formula (gv arrowOpenpbutton :selected)))
		   (:arrowhead-p (o-formula (gv arrowbutton :selected)))
		   (:filling-style (o-formula (gv arrowfillbutton :fill))))

  (opal:add-components arrow-line-agg arrowbutton arrowOpenpbutton
		       arrowfillbutton myarrow1 myarrow2 single-circle
		       single-rect double-circle double-rect)
  (opal:update arrow-line-win)

  ;;
  ;; Interactors for the mode buttons
  ;;
  (create-instance 'interarrowbutton Inter:button-interactor
		   (:continuous NIL)
		   (:start-where `(:in ,arrowbutton))
		   (:window arrow-line-win)
		   ;; use a list of 2 numbers and interactor will do MOD
		   (:waiting-priority inter:high-priority-level)
		   (:how-set (list 1 4)))
  (create-instance 'interarrowfillbutton Inter:button-interactor
		   (:continuous NIL)
		   (:start-where `(:in ,arrowfillbutton))
		   (:window arrow-line-win)
		   (:waiting-priority inter:high-priority-level)
		   (:how-set (list 1 3)))
  (create-instance 'interarrowopenpbutton Inter:button-interactor
		   (:continuous NIL)
		   (:start-where `(:in ,arrowOpenpbutton))
		   (:window arrow-line-win)
		   (:waiting-priority inter:high-priority-level)
		   (:how-set :toggle))

  ;;
  ;; Interactors to move the objects that the arrows are attached to
  ;;
  (create-instance 'intersinglecirc Inter:Move-Grow-Interactor
		   (:start-where `(:in ,single-circle))
		   (:running-where T)
		   (:window arrow-line-win)
		   (:line-p NIL))
  (create-instance 'intersinglerect Inter:Move-Grow-Interactor
		   (:start-where `(:in ,single-rect))
		   (:window arrow-line-win)
		   (:running-where T)
		   (:line-p NIL))
  (create-instance 'interdoublecirc Inter:Move-Grow-Interactor
		   (:start-where `(:in ,double-circle))
		   (:running-where T)
		   (:window arrow-line-win)
		   (:line-p NIL))
  (create-instance 'interdoublerect Inter:Move-Grow-Interactor
		   (:start-where `(:in ,double-rect))
		   (:window arrow-line-win)
		   (:running-where T)
		   (:line-p NIL))

  (format T "ArrowLines:
   Press on the text at the top of the window with the left mouse button
   to change modes.
   Press on and drag one of the circles or rectangles to move it.~%")

  (opal:update arrow-line-win))


#+garnet-test
(defun arrow-line-stop ()
  (opal:destroy arrow-line-win))
