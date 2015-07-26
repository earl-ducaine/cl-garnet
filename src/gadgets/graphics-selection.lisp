;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$


;;;  Graphics-Selection:  Selection squares around graphic objects
;;;
;;;  Features and operation of the Graphics Selection object:
;;;    1)  Given a list of graphical objects, the graphics-selection
;;;        aggregadget will cause selection squares to appear on the bounding
;;;        box of selected objects.
;;;    2)  Only one item may be selected at a time.
;;;    3)  A built-in interactor displays the selection squares around an
;;;        object at the time of a specified event (such as clicking a mouse
;;;        button on the object).
;;;    4)  Each selection square allows the user to move or grow the object
;;;        by dragging the selection square.
;;;


;;; Programmer Interface
;;; ====================
;;;
;;; Create an instance of a GARNET-GADGETS:Graphics-Selection and supply the
;;; :start-where slot with a valid list that can be passed to an
;;; interactor.  This :start-where must return the items to be selected.
;;; It should be an ...-or-none form, such as element-of-or-none.  An
;;; example of the parameter to :start-where is:
;;; 		(list :element-of-or-none myagg)
;;;
;;; The :value slot of the GARNET-GADGETS:Graphics-Selection object will be
;;; set with the object the user selects.  Also, a :selection-function can be
;;; supplied and will be called each time the selection changes.  It takes
;;; these parameters: (Gadget New-Selection)
;;; The NewSelection may be NIL if the selection changes to be no objects.
;;;
;;; The user can change the size and/or position of the objects.  If the
;;; :check-line slot is non-nil, then the :line-p slot in the object returned
;;; by start-where will be g-valued, and if it is non-nil then the interactor
;;; will change the object as a line.  Note that the programmer must set the
;;; :line-p slots of the objects (since often composite objects like arrowlines
;;; will be used, and just asking (is-a-p obj Opal:line) on these will
;;; return false).  The programmer can supply a :modify-function that will
;;; be called after an object is modified.  It takes these
;;; parameters: (gadget selected-object new-points)
;;; The new-points will be a list of 4 numbers, either
;;; left,top,width,height or x1,y1,x2,y2
;;; 
;;; In summary, public slots of the Graphics-Selection objects are:
;;;    :start-where - supply a valid start-where here
;;;    :check-line - if T, the objects are checked for their :line-p slot
;;; 			and if that is non-NIL, then move or grown as a line
;;;    :running-where - if supplied, then this is the area in which the
;;; 			objects can move and grow
;;;    :value - current object selected
;;;    :selection-function - this is called when the selection changes
;;;    :modify-function - this is called when an object is changed size or
;;;                       position
;;;    :movegrow-boxes-p - whether it is OK to move and grow non-lines.  If
;;; 			   NIL, then will beep if press on a square to
;;; 			   change a non-line.
;;;    :movegrow-lines-p - whether it is OK to move and grow lines.  If
;;; 			   NIL, then will beep if press on a square to
;;; 			   change a line.
;;;
;;; Slots of the objects that can be selected are:
;;;    :line-p - this should be T if the object should be moved as a line,
;;; 		and NIL if as a rectangle
;;;    :points - if :line-p is T, then the :points slot of the object is
;;; 	        changed as the object is moved or grown.
;;;    :box - if :line-p is NIL, then the :box slot of the object is
;;; 	        changed as the object is moved or grown.
;;;


;;; End User Operation
;;; ==================
;;;
;;; The user can press on any object with the left button, and it will
;;; become selected.  Pressing on the background, causes no object to be
;;; selected (the current object becomes de-selected).  Since only one
;;; object can be selected at a time, selecting an object causes the
;;; previous object to be de-selected.
;;;
;;; Once an object is selected, it can be grown by pressing with the left
;;; button on one of the black boxes or moved by pressing on a white box.
;;; While moving and growing, if the mouse goes outside of :running-where
;;; or if the ^G key is pressed, the operation aborts.
;;;
;;; Test and debugging routines are NOT in this module.  See demo-grow for an
;;; example of how to use this gadget.
;;;
;;;  Designed and written by Brad Myers
;;;


(in-package "GARNET-GADGETS")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(Graphics-Selection)))

;; These are the size of the selection boxes
(defparameter size 7)  ; should be odd
(defparameter sizeD2 (floor size 2))

(create-instance 'Grow-Selection-Box Opal:rectangle
      (:visible (o-formula (gv (kr-path 0 :parent) :value)))
      (:width size)
      (:height size)
      (:draw-function :xor)
      (:fast-redraw-p T)
      (:grow-p T)
      (:filling-style opal:black-fill)
      (:line-style NIL))

(create-instance 'Move-Selection-Box Opal:rectangle
      (:visible (o-formula (gv (kr-path 0 :parent) :value)))
      (:width size)
      (:height size)
      (:draw-function :xor)
      (:fast-redraw-p T)
      (:grow-p NIL)
      (:filling-style NIL)
      (:line-style Opal:Thin-Line))

;;This creates the selection rectangles
(defun Make-Boxes (self)
  (declare (ignore self))
  (let ((names '(:nw-grow :n-move :n-grow :ne-grow :e-move :e-grow :se-grow
		 :s-grow :s-move :sw-grow :w-grow :w-move))
	boxes obj)
    (do ((x '(l m1 m2 r r r r m2 m1 l l l) (cdr x))
	 (y '(t t t t m1 m2 b b b b m2 m1) (cdr y))
	 (attach '(:nw :where-hit :n :ne :where-hit :e :se :s :where-hit :sw
		       :w :where-hit) (cdr attach))
	 (grow-p '(t nil t t nil t t t nil t t nil) (cdr grow-p)))
	((null x)) ; end test
      (setq obj
	    (create-instance NIL (if (car grow-p)
				     Grow-Selection-Box
				     Move-Selection-Box)
		   (:left (case (car x)
			    (l (o-formula (gv (kr-path 0 :parent) :left)))
			    (m1 (o-formula
				 (+ (gv (kr-path 0 :parent) :left)
				    (floor (gv (kr-path 0 :parent) :width) 3)
				    (- sizeD2))))
			    (m2 (o-formula
				 (+ (gv (kr-path 0 :parent) :left)
				    (floor (* 2 (gv (kr-path 0 :parent) :width)) 3)
				    (- sizeD2))))
			    (r (o-formula (+ (gv (kr-path 0 :parent) :left)
					     (gv (kr-path 0 :parent) :width)
					     (- size))))))
		   (:top (case (car y)
			   ((t) (o-formula (gv (kr-path 0 :parent) :top)))
			   (m1 (o-formula
				(+ (gv (kr-path 0 :parent) :top)
				   (floor (gv (kr-path 0 :parent) :height) 3)
				   (- sizeD2))))
			   (m2 (o-formula
				(+ (gv (kr-path 0 :parent) :top)
				   (floor (* 2 (gv (kr-path 0 :parent) :height)) 3)
				   (- sizeD2))))
			   (b (o-formula (+ (gv (kr-path 0 :parent) :top)
					    (gv (kr-path 0 :parent) :height)
					    (- size))))))
		   (:where-attach (car attach))))
      (setq boxes (cons obj boxes)))
    (values boxes names))) ; return the rectangles and the names for them

(create-instance 'rect-selection-boxes Opal:Aggregadget
    (:value NIL)
    (:left (o-formula (- (gvl :value :left) sizeD2) 0))
    (:top (o-formula (- (gvl :value :top) sizeD2) 0))
    (:width (o-formula (+ (gvl :value :width) size) 0))
    (:height (o-formula (+ (gvl :value :height) size) 0))
    (:visible (o-formula (gvl :value)))
    ;; create the selection boxes in a function
    (:parts (list #'Make-Boxes)))

(create-instance 'line-selection-boxes Opal:Aggregadget
    (:value NIL)
    (:x1 (o-formula (- (gvl :value :x1) sizeD2) 0))
    (:y1 (o-formula (- (gvl :value :y1) sizeD2) 0))
    (:x2 (o-formula (- (gvl :value :x2) sizeD2) 0))
    (:y2 (o-formula (- (gvl :value :y2) sizeD2) 0))
    (:visible (o-formula (gvl :value)))
    (:parts `((:box1 ,Grow-Selection-Box
		   (:left ,(o-formula (gv (kr-path 0 :parent) :x1)))
		   (:top ,(o-formula (gv (kr-path 0 :parent) :y1)))
		   (:where-attach 1))
	      (:box2 ,Grow-Selection-Box
		   (:left ,(o-formula (gv (kr-path 0 :parent) :x2)))
		   (:top ,(o-formula (gv (kr-path 0 :parent) :y2)))
		   (:where-attach 2))
	      (:boxcenter ,Move-Selection-Box
		   (:left ,(o-formula (+ (gv (kr-path 0 :parent) :x1)
					(floor (- (gv (kr-path 0 :parent) :x2)
						  (gv (kr-path 0 :parent) :x1))
					       2))))
		   (:top ,(o-formula (+ (gv (kr-path 0 :parent) :y1)
				       (floor (- (gv (kr-path 0 :parent) :y2)
						 (gv (kr-path 0 :parent) :y1))
					      2))))
		   (:where-attach :center)))))

(create-instance 'sel-line-movegrow-feedback Opal:Line
   (:points (list 0 0 10 10))  ; some initial values (x1 y1 x2 y2)
   (:obj-over NIL)
   (:visible (o-formula (gvl :obj-over)))
   (:x1 (o-formula (first (gvl :points))))
   (:y1 (o-formula (second (gvl :points))))
   (:x2 (o-formula (third (gvl :points))))
   (:y2 (o-formula (fourth (gvl :points))))
   (:draw-function :xor)
   (:fast-redraw-p T)
   (:line-style opal:dashed-line))

(create-instance 'sel-rect-movegrow-feedback opal:rectangle
   (:box '(80 20 100 150))
   (:obj-over NIL)
   (:visible (o-formula (gvl :obj-over)))
   (:left (o-formula (first (gvl :box))))
   (:top (o-formula (second (gvl :box))))
   (:width (o-formula (third (gvl :box))))
   (:height (o-formula (fourth (gvl :box))))
   (:draw-function :xor)
   (:fast-redraw-p T)
   (:line-style opal:dashed-line)
   (:filling-style NIL))

(create-instance 'Graphics-Selection opal:aggregadget
  :declare ((:parameters :active-p :start-where :start-event :running-where
			 :check-line :movegrow-boxes-p :movegrow-lines-p
			 :value :modify-function :selection-function)
	    (:type (kr-boolean :active-p :check-line :movegrow-boxes-p
		    :movegrow-lines-p)
	           ((or list (member T)) :start-where :running-where)
		   ((or keyword character list) :start-event)
		   ((or null (satisfies schema-p)) :value)
		   ((or null function symbol)
		    :selection-function :modify-function)))
		  
     ;; programmer-settable slots
     (:start-where NIL) ;; supply a valid start-where here
     (:start-event :leftdown)
     (:running-where T) ;; if supplied, then this is the area in which the
			;; objects can move and grow
     (:active-p T)
     (:selection-function NIL) ;; this is called when the selection changes
     (:modify-function NIL) ;; called when an object is changed size or position
     (:check-line T)
     (:movegrow-boxes-p T)  ;; whether it is OK to move and grow non-lines
     (:movegrow-lines-p T)  ;; whether it is OK to move and grow lines

     ;; slot the programmer can access
     (:value NIL)  ;;current object selected

     ;; internal slots
     (:is-line (o-formula (and (gvl :check-line)
			       (gvl :value)
			       (gvl :value :line-p))))
     (:cur-sel-feedback (o-formula (if (gvl :is-line)
				       (gvl :line-selection-boxes)
				       (gvl :rect-selection-boxes))))
     (:cur-movegrow-feedback (o-formula (if (gvl :is-line)
					    (gvl :line-movegrow-feedback)
					    (gvl :rect-movegrow-feedback))))
     (:parts `((:line-movegrow-feedback ,sel-line-movegrow-feedback)
	       (:rect-movegrow-feedback ,sel-rect-movegrow-feedback)
	       (:line-selection-boxes ,line-selection-boxes
		   (:value ,(o-formula
				(if (gv (kr-path 0 :parent) :is-line)
				    (gv (kr-path 0 :parent) :value)
				    NIL))))
	       (:rect-selection-boxes ,rect-selection-boxes
                    (:value ,(o-formula
				 (if (gv (kr-path 0 :parent) :is-line)
				     NIL
				     (gv (kr-path 0 :parent) :value)))))))
     (:interactors
      `((:select-it ,inter:button-interactor
	   (:window ,(o-formula (gv-local :self :operates-on :window)))
	   (:continuous NIL)
	   (:start-where ,(o-formula (gvl :operates-on :start-where)))
	   (:start-event ,(o-formula (gvl :operates-on :start-event)))
	   (:active ,(o-formula (and (gv-local :self :window)
				     (gvl :operates-on :active-p))))
	   (:final-function
	    ,(function (lambda(an-interactor objUnderMouse)
			 (let ((oper (g-value an-interactor :operates-on)))
			   (s-value oper :value
				    (if (eq :none objUnderMouse)
				      NIL
				      objUnderMouse))
			   (kr-send oper :selection-function oper
				    (g-value oper :value)))))))

	(:move-grow-it ,inter:Move-Grow-Interactor
	   (:window ,(o-formula (gv-local :self :operates-on :window)))
	   (:active ,(o-formula (gvl :operates-on :value)))
	   (:continuous T)
	   (:waiting-priority ,inter:high-priority-level)
           (:abort-event (:control-g :control-\g))
	   (:start-where ,(o-formula
			   (list :element-of (gvl :operates-on :cur-sel-feedback))))
	   (:running-where ,(o-formula (gvl :operates-on :running-where)))
	   (:outside NIL)
	   (:feedback-obj ,(o-formula
			    (gvl :operates-on :cur-movegrow-feedback)))
	   (:obj-to-change ,(o-formula (gvl :operates-on :value)))
	   ;; the various attachpoints are stored in a slot in each selection
	   ;; box and the box is returned set into the :first-obj-over slot of
	   ;; the interactor
	   (:attach-point ,(o-formula (gvl :first-obj-over :where-attach)))
	   (:grow-p ,(o-formula (gvl :first-obj-over :grow-p)))
	   (:line-p ,(o-formula (gvl :operates-on :is-line)))
	   ;; have a special start-action to check to see if it is OK to
	   ;; start, and if not, then beep and abort.
	   (:start-action
	    ,(function
	      (lambda (inter obj-being-changed first-points)
		(if (if (g-value inter :line-p)
			(g-value inter :operates-on :movegrow-lines-p)
			(g-value inter :operates-on :movegrow-boxes-p))
		    ; then OK to move or grow it
		    (call-prototype-method inter obj-being-changed first-points)
		    ; else not OK, so abort
		    (progn (inter::beep)
		      (Inter:Abort-Interactor inter))))))
	   ;; turn off selection after move or grow
	   (:final-function
	    ,(function (lambda(an-interactor objUnderMouse newsize)
			 (declare (ignore objUnderMouse))
			 (let ((oper (g-value an-interactor :operates-on)))
			   (kr-send oper :modify-function oper
				    (g-value oper :value)
				    newsize)
			   (s-value oper :value NIL)))))))))

