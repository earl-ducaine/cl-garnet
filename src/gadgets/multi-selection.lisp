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
;;;  Multi-Graphics-Selection:  Selection squares around graphic objects
;;;
;;;  Features and operation of the Multi Graphics Selection object:
;;;    * This is somewhat like the graphics-selection.  The major
;;;      difference is that multiple objects can be selected and
;;;      manipulated by the user, and that the
;;; 	 programmer must use a function to set the :value slot.
;;;      Another difference
;;; 	 is the way that checks whether move and grow is allowed.
;;;      Of course, the multi-graphics-selection supports lots of additional
;;;      features like selecting in a region, undo-ing the move/grow and
;;;      modifying aggregates and polygons.
;;;    1) Given a list of graphical objects, the Multi-Graphics-Selection
;;;        aggregadget will cause selection squares to appear on the
;;;        bounding box of selected objects.
;;;    2)  One or more than one item may be selected at a time.
;;;    3)  A built-in interactor displays the selection squares around an
;;;        object at the time of a specified event (such as clicking a mouse
;;;        button on the object).
;;;    4)  Each selection square allows the user to move or grow the object
;;;        by dragging the selection square.
;;;    5) When a group of objects is selected, special grow handles
;;;        appear at the corners of the whole group for growing all
;;;        the objects.
;;; 
;;; Programmer Interface
;;; ====================
;;;
;;; Create an instance of a GARNET-GADGETS:multi-Graphics-Selection
;;; and supply the
;;; :start-where slot with a valid list that can be passed to an
;;; interactor.  This :start-where must return the items to be selected.
;;; It should be an ...-or-none form, such as :element-of-or-none.  An
;;; example of the parameter to :start-where is:
;;; 		(list :element-of-or-none myagg)
;;;
;;; The :value slot of the GARNET-GADGETS:Graphics-Selection object supplies
;;; the object(s) the user selects.  If :multiple-select-p is NIL
;;; then it is a single object or NIL.  If :multiple-select-p
;;; is T (the default), then will always be a list or NIL (even if
;;; only one object is selected).   Also, a :selection-function can be
;;; supplied and will be called each time the selection changes.  It takes
;;; these parameters: (Gadget New-Selection), where New-selection is the
;;; new value of :value.
;;;
;;; The user can change the size and/or position of the objects by
;;; pressing on the selection handles (see below).  If the
;;; :check-line slot is non-nil, then the :line-p slot in the object returned
;;; by start-where will be g-valued, and if it is non-nil then the interactor
;;; will change the object as a line.  Note that the programmer must set the
;;; :line-p slots of the objects (since often composite objects like arrowlines
;;; will be used, and just asking (is-a-p obj Opal:line) on these will
;;; return false).  Similarly, for polygons, the gadget will check
;;; :polygon-p and for groups (aggregates) it will check :group-p.
;;; The programmer can supply a :modify-function that will
;;; be called after an object is modified.  It takes these
;;; parameters: (gadget selected-object new-points)
;;; The new-points will be a list of 4 numbers, either
;;; left,top,width,height or x1,y1,x2,y2 (even for polygons).
;;; 
;;; The public slots of the Graphics-Selection objects are:
;;;    :start-where - supply a valid start-where here
;;;    :check-line - if T, the objects are checked for their :line-p slot
;;; 			and if that is non-NIL, then move or grown as a line
;;;    :check-polygon - if T, the objects are checked for their :polygon-p slot
;;; 			and if that is non-NIL, then move or grown as a polygon
;;;    :check-group - if T, the objects are checked for their :group-p slot
;;; 			and if that is non-NIL, then the individual components
;;;                     of the group are modified.
;;;    :multiple-select-p - if T, then multiple objects can be
;;;                     selected. Default=NIL
;;;    :running-where - if supplied, then this is the area in which the
;;; 			objects can move and grow
;;;    :active-p - if T, then operates.  If NIL, then none of the
;;;                interactors will work.  Setting to NIL does NOT
;;;                clear selection, however.
;;;    :selection-function - this is called when the selection changes
;;;    :modify-function - this is called when an object is changed size or
;;;                       position
;;;    :input-filter - filter to pass to move-grow (to do gridding, etc.)
;;;    :check-grow-p - If T, then checks in each object to see if :grow-p is T,
;;; 			   and if so, then will grow, else won't.  If NIL, then
;;; 			   everything can grow.  Default NIL.
;;;    :check-move-p - If T, then checks in each object to see if :move-p is T,
;;; 			   and if so, then will move, else won't.  If NIL, then
;;; 			   everything can move.  Default NIL.
;;;    :move-multiple-p - If T, then if multiple items are selected and you
;;; 			press on a move box, then moves all of the objects.
;;; 			If NIL, then just moves the object you point to.
;;; 			Default=T.
;;;    :grow-multiple-p - If T, then if multiple items are selected then
;;;                     grow boxes appear at the corners of the whole
;;;                     selection, and pressing there will grow all
;;;                     the objects.  If NIL, then those handles don't appear.
;;; 			Default=T.
;;;    :want-undo - If T, then saves information (conses) so that can
;;;                 you can call Undo-Last-Move-Grow.
;;;    :report-function - If non-NIL, then calls the function at each
;;;                       object manipulation or edit, in addition to
;;;                       the selection function. :want-undo MUST be T also.
;;;                       See below.
;;;    :allow-component-select - if T, then pressing with CONTROL will select
;;;                     the component under the selected object (see below)
;;;                     Default=NIL.
;;;    :down-to-component-function - supply with a function to get the appropriate
;;;                     component out of the object under the mouse.  This
;;;                     function might call a method in the selected object.
;;;                     Parameters are (lambda obj x y).  Should return the
;;;                     object to be selected, or NIL.
;;;                     Default: Opal:Point-To-Component
;;;    :move-components-p - whether components are allowed to be moved.  Often,
;;;                     even though top-level objects can move, components can't.
;;;                     Therefore, this can be set to NIL which means components
;;;                     can never be moved.  This only works if :check-move-p is
;;;                     T.  Default=NIL.
;;;    :grow-components-p - whether components are allowed to be grown.  Often,
;;;                     even though top-level objects can grow, components can't.
;;;                     Therefore, this can be set to NIL which means components
;;;                     can never be grown.  This only works if :check-grow-p is
;;;                     T.  Default=NIL.
;;;
;;;    :other-multi-graphics-selection - when non-NIL, this holds a
;;;                     list of "linked" multi-graphics-selection
;;;                     objects for use in multiple windows.  All of
;;;                     the objects will have identical :value's (so
;;;                     you can get the selected objects by
;;;                     g-value-ing any of the graphics-selection),
;;;                     but move-grow only works on the objects in a
;;;                     single window.  NOTE: ALL of the
;;;                     graphics-selection objects must have the SAME
;;;                     list in the :other-multi-graphics-selection
;;;                     slot.  Thus, the list should include this
;;;                     object also.
;;; 
;;; SLOTS that can be accessed:
;;;    :value - set with list of the current selections, in reverse order
;;;            the user selected the objects (first selected object is
;;;            last in the list).
;;;            *DO NOT SET*, use the function Set-Selection (see below).
;;;    :current-selected-object - set with new selection before other
;;;                     slots are set.
;;;
;;; Slots of the objects that can be selected are:
;;;    :line-p - this should be T if the object should be moved as a line,
;;; 		and NIL otherwise.
;;;    :polygon-p - this should be T if the object should be moved as
;;;             a polygon, and NIL otherwise.
;;;    :group-p - this should be T if the object should be moved as a group.
;;;    :points - if :line-p is T, then the :points slot of the object is
;;; 	        changed as the object is moved or grown.
;;;    :box - if :line-p is NIL, then the :box slot of the object is
;;; 	        changed as the object is moved or grown.
;;;    :point-list - if :polygon-p is T, then the :point-list slot of
;;;             the object is changed as the object is moved or grown.
;;;    :grow-p - if this object can be changed size
;;;    :move-p - if this object can be moved
;;;
;;; USEFUL FUNCTIONS:
;;; 
;;;    (Garnet-Gadgets:Set-Selection gadget new-selection)
;;; 
;;;      Gadget should be a Multi-Graphics-Selection gadget, and new-selection
;;;      is a list of objects that should be selected, or a single object to be
;;;      selected, or NIL to turn off all selections.  The list passed
;;;      in is not damaged.
;;;
;;;    (Garnet-Gadgets:Undo-Last-Move-Grow gadget)
;;; 
;;;      Gadget should be a Multi-Graphics-Selection gadget.  This
;;;      un-does the last move or grow operation and restores the
;;;      selection.  You have to be careful not to have deleted any
;;;      objects since they were grown.  Returns NIL if successful or
;;;      else an error string.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REPORT-FUNCTIONS are called as
;;;         (lambda (gadget main-op parameter obj oldval newval)
;;;    where main-op is one of
;;;       :select :move :grow
;;;    and if :select, then parameter is one of
;;;       :become-select :toggle-select :clear-select
;;;       :region-select :region-toggle :downlevel-select :downlevel-toggle
;;;       :explicit-select (due to a call to set-selection)
;;;    and if :move or :grow then the parameter is where-hit
;;;    * For the selection operations, the obj will be the object or objects
;;;    affected, the oldval will be the original full selection value before
;;;    the operation, and the newval will be the value afterwards.
;;;    * For move and grow, the obj will be a list of objects
;;;    changed, and the oldval will be a list of the original :box, :points, or
;;;    :point-list values, and newval will be a list of the new ones.
;;; * The lists passed should not be destructively modified by the
;;;   application, but they can be used directly (and not copied)
;;; NOTE THAT THIS OPERATION CONSES A LOT.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; End User Operation
;;; ==================
;;;
;;; The user can press on any object with the left button, and it will
;;; become selected.  Pressing on the background, causes no object to be
;;; selected (the current object becomes de-selected).  Selecting an object
;;; with the left button causes the previous object to be de-selected.
;;; If the application allows multiple selection, then clicking with
;;; shift-left or middle on an object toggles it in the selection set.
;;; Also pressing and dragging out a rectangle selects everything
;;; entirely enclosed (not the object directly under the mouse when started).
;;;
;;; Once an object is selected, it can be grown by pressing with the left
;;; button on one of the black boxes or moved by pressing on a white box.
;;; While moving and growing, if the mouse goes outside of :running-where
;;; or if the ^G key is pressed, the operation aborts.
;;;
;;; If the programmer has set :allow-component-select to T (it is NIL by
;;; default), then by pressing :CONTROL-LEFT over a selected object, that object
;;; will be deselected, and its component will be selected instead.  Similarly,
;;; if CONTROL-MIDDLE or CONTROL-SHIFT-LEFT is hit over a selected object, then
;;; that object is de-selected, and the object underneath is added to the
;;; selection set.
;;; 
;;; Test and debugging routines are at the end of this module.
;;;
;;;  Designed and written by Brad Myers
;;;
#|
============================================================
Change log:
  6-Feb-94 Brad Myers - made to work with multiple windows
 10-Mar-93 Brad Myers - set-selection functions calls the :selection-function
  1-Mar-93 Andrew Mickish - Removed superfluous quotes from :slots-to-set
 14-Dec-92 Andrew Mickish - Added type and parameter declarations
 20-Nov-92 Andrew Mickish - Removed s-value of :move-p and :grow-p in
                            down-component select function.
 10-Sep-92 Brad Myers - added reporting of the editing operation (for Marquise)
                        if :report-function
 10-Jul-92 Brad Myers - added growing of multiple objects, thanks to
                            algorithm by Vivek Gupta when :grow-multiple-p
                      - added :active-p slot
                      - added "grouped" objects with :group-p
                      - added selection in region
                      - added support for moving polygons if :polygon-p
                      - Undo-Last-Move-Grow if :want-undo

 20-Mar-92 Ed Pervin - eliminate control chars.
  7-Feb-92 Brad Myers - made work with constant formulas (feedback obj not in
                               aggregate)
                      - use new features of interactors
                      - export grid ability for moving/growing
                      - don't set the :selected fields of objects
 29-Jul-91 Brad Myers - Added ability to select the components of the top object
 20-Mar-91 Brad Myers - Fixed so set-selection will work with multiple objs
 13-Jan-91 Brad Myers - Fixed so newly selected object always put
                        at end of list, selection list order is
                        always the same as the order the user
                        selected objects in.
 15-Nov-90 Brad Myers - support multiple selection, modified from
 			graphics selection
============================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Multi-Graphics-Selection Set-Selection Undo-Last-Move-Grow)))

;; These are the size of the selection boxes
(defparameter multi-size 7)  ; should be odd
(defparameter multi-sizeD2 (floor multi-size 2))
(defparameter minus-multi-sizeD2 (- multi-sizeD2))
(defparameter minus-multi-size (- multi-size))

;;Use this for the grow boxes that are used for both lines and rectangles
(defparameter grow-vis 
  (o-formula (and (gvl :parent :obj-over)
		  (gvl :parent :show-grow))))

;;Use this for the move boxes that are used for both lines and rectangles
(defparameter move-vis (o-formula (gvl :parent :obj-over)))

;;Use this for the grow boxes that are used only for rectangles 
(defparameter grow-vis-not-line 
  (o-formula (and (gvl :parent :obj-over)
		  (gvl :parent :show-grow)
		  (not (gvl :parent :line-p)))))

;;Use this for the move boxes that are used only for rectangles 
(defparameter move-vis-not-line 
  (o-formula (and (gvl :parent :obj-over)
		  (not (gvl :parent :line-p)))))

(create-instance 'Multi-Grow-Selection-Box opal:rectangle
  ; This type declaration will allow the current :top formulas to return
  ; NIL when the grow boxes are invisible
  :declare (:type ((or null integer) :top))
      (:visible (formula grow-vis-not-line))
      (:width multi-size)
      (:height multi-size)
      (:draw-function :xor)
      (:fast-redraw-p T)
      (:grow-p T)
      (:filling-style opal:black-fill)
      (:line-style NIL))

(create-instance 'Multi-Move-Selection-Box opal:rectangle
  ; This type declaration will allow the current :top formulas to return
  ; NIL when the move boxes are invisible
  :declare (:type ((or null integer) :top))
      (:visible (formula move-vis-not-line))
      (:width multi-size)
      (:height multi-size)
      (:draw-function :xor)
      (:fast-redraw-p T)
      (:grow-p NIL)
      (:filling-style NIL)
      (:line-style Opal:Thin-Line))

;; This one is only used when multiple objects are selected and they
;; can be grown
(create-instance 'special-multi-feedback Opal:aggregadget
  (:visible (o-formula (gvl :parent :special-multi-feedback-vis)))
  (:box (o-formula
	   (let ((obj (gvl :parent :fake-rect)))
	     (if obj
		 (let ((left (gv obj :left))
		       (top (gv obj :top))
		       (width (gv obj :width))
		       (height (gv obj :height)))
		   (list (- left multi-sizeD2) ;first
			 (- top multi-sizeD2)  ;second
			 (+ left width minus-multi-sizeD2 -1) ;right, third
			 (+ top height minus-multi-sizeD2 -1))) ;bottom, fourth
		 ;; else no obj-over
		 (list 0 0 0 0)))))
  (:parts `((:nw-grow ,Multi-Grow-Selection-Box
	     (:visible ,(o-formula (gvl :parent :visible)))
	     (:left ,(o-formula (first (gv (kr-path 0 :parent) :box))))
	     (:top ,(o-formula (second (gv (kr-path 0 :parent) :box))))
	     (:where-attach :nw))
	    (:ne-grow ,Multi-Grow-Selection-Box
	     (:visible ,(o-formula (gvl :parent :visible)))
	     (:left ,(o-formula (third (gv (kr-path 0 :parent) :box))))
	     (:top ,(o-formula (second (gv (kr-path 0 :parent) :box))))
	     (:where-attach :ne))
	    (:se-grow ,Multi-Grow-Selection-Box
	     (:visible ,(o-formula (gvl :parent :visible)))
	     (:left ,(o-formula (third (gv (kr-path 0 :parent) :box))))
	     (:top ,(o-formula (fourth (gv (kr-path 0 :parent) :box))))
	     (:where-attach :se))
	    (:sw-grow ,Multi-Grow-Selection-Box
	     (:visible ,(o-formula (gvl :parent :visible)))
	     (:left ,(o-formula (first (gv (kr-path 0 :parent) :box))))
	     (:top ,(o-formula (fourth (gv (kr-path 0 :parent) :box))))
	     (:where-attach :sw)))))

(create-instance 'sel-selection-feedback Opal:Aggregadget
    (:obj-over NIL) ; set by interactor
    (:show-grow (o-formula (and (gvl :parent :parent :grow-ok)
				(if (gvl :parent :parent :check-grow-p)
				    ; then do whatever the object says
				    (gvl :obj-over :grow-p)
				    T))))   ; else always show growable
    (:line-p (o-formula (and (gvl :parent :parent :check-line)
			     (gvl :obj-over :line-p))))
    (:box (o-formula 
	   (let ((obj (gvl :obj-over)))
	     (if obj
	       (if (gvl :line-p)
		 (list (- (gv obj :x1) multi-sizeD2)
		       (- (gv obj :y1) multi-sizeD2)
		       (- (gv obj :x2) multi-sizeD2)
		       (- (gv obj :y2) multi-sizeD2))
		 ;; else not a line-p
		 (let ((left (gv obj :left)) ; get M1 and M2 also
		       (top (gv obj :top))
		       (width (gv obj :width))
		       (height (gv obj :height)))
		   (list (- left multi-sizeD2) ;first
			 (- top multi-sizeD2)  ;second
			 (+ width multi-size)  ;third
			 (+ height multi-size) ;fourth
			 
			 (+ left (floor width 3) minus-multi-sizeD2)   ;M1 left, 5
			 (+ left (floor (+ width width) 3)	       ;M2 left, 6
			    minus-multi-sizeD2)
			 (+ left width minus-multi-sizeD2 -1)          ;right,   7
			 (+ top (floor height 3) minus-multi-sizeD2)   ;M1top,   8
			 (+ top (floor (+ height height) 3)	       ;m2 top,  9
			    minus-multi-sizeD2)
			 (+ top height minus-multi-sizeD2 -1))))    ;bottom, 10
	       ;; else no obj-over
	       (list 0 0 0 0)))))
    (:visible (o-formula (gvl :obj-over)))
    (:parts `((:nw-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (first (gv (kr-path 0 :parent) :box))))
		  (:top ,(o-formula (second (gv (kr-path 0 :parent) :box))))
		  (:visible ,(formula grow-vis))
		  (:where-attach ,(o-formula (if (gv (kr-path 0 :parent) :line-p)
						 1 :nw))))
	      (:n-move ,Multi-Move-Selection-Box
		       ;; This box is at the M1 if normal, at the left if not move,
		       ;; or at the center if a line!
		  (:left ,(o-formula
			   (let ((box (gv (kr-path 0 :parent) :box)))
			     (if (gv (kr-path 0 :parent) :line-p) ; then center
				 (+ (first box)
				    (floor (- (third box) (first box)) 2))
				 ; else if grow-p then m1 else left
				 (if (gv (kr-path 0 :parent) :show-grow)
				     (fifth box) (first box))))))
		  (:top ,(o-formula
			  (let ((box (gv (kr-path 0 :parent) :box)))
			    (if (gv (kr-path 0 :parent) :line-p) ; then center
				(+ (second box)
				   (floor (- (fourth box) (second box)) 2))
				; else at top
				(second box)))))
		  (:visible ,(formula move-vis))
		  (:where-attach ,(o-formula
				   (if (gv (kr-path 0 :parent) :line-p) ; then center
				       :center :where-hit))))
	      (:n-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (sixth (gv (kr-path 0 :parent) :box)))) ; m2left
		  (:top ,(o-formula (second (gv (kr-path 0 :parent) :box)))) ;top
		  (:where-attach :n))
	      (:ne-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (seventh (gv (kr-path 0 :parent) :box))))
		  (:top ,(o-formula (second (gv (kr-path 0 :parent) :box))))
		  (:where-attach :ne))
	      (:e-move ,Multi-Move-Selection-Box
		  (:left ,(o-formula (seventh (gv (kr-path 0 :parent) :box))))
		       ;; This box is at the M1 if normal, at the top if not move
		  (:top ,(o-formula
			   (let ((box (gv (kr-path 0 :parent) :box)))
			     (if (gv (kr-path 0 :parent) :show-grow)
				 (eighth box) (second box)))))
		  (:where-attach :where-hit))
	      (:e-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (seventh (gv (kr-path 0 :parent) :box))))
		  (:top ,(o-formula (ninth (gv (kr-path 0 :parent) :box)))) ;m2top
		  (:where-attach :e))
	      (:se-grow ,Multi-Grow-Selection-Box  ; use this one for lines also
		  (:left ,(o-formula (if (gv (kr-path 0 :parent) :line-p)
					 (third (gv (kr-path 0 :parent) :box))
					 (seventh (gv (kr-path 0 :parent) :box)))))
		  (:top ,(o-formula (if (gv (kr-path 0 :parent) :line-p)
					 (fourth (gv (kr-path 0 :parent) :box))
					 (tenth (gv (kr-path 0 :parent) :box)))))
		  (:visible ,(formula grow-vis))
		  (:where-attach ,(o-formula (if (gv (kr-path 0 :parent) :line-p)
						 2 :se))))
	      (:s-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (sixth (gv (kr-path 0 :parent) :box)))) ;m2left
		  (:top ,(o-formula (tenth (gv (kr-path 0 :parent) :box)))) ;bottom
		  (:where-attach :s))
	      (:s-move ,Multi-Move-Selection-Box
		       ;; This box is at the M1 if normal, at the right if not move
		  (:left ,(o-formula
			   (let ((box (gv (kr-path 0 :parent) :box)))
			     (if (gv (kr-path 0 :parent) :show-grow)
				 (fifth box) (seventh box)))))
		  (:top ,(o-formula (tenth (gv (kr-path 0 :parent) :box)))) ;bottom
		  (:where-attach :where-hit))
	      (:sw-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (first (gv (kr-path 0 :parent) :box)))) ;left
		  (:top ,(o-formula (tenth (gv (kr-path 0 :parent) :box)))) ;bottom
		  (:where-attach :sw))
	      (:w-grow ,Multi-Grow-Selection-Box
		  (:left ,(o-formula (first (gv (kr-path 0 :parent) :box)))) ;left
		  (:top ,(o-formula (ninth (gv (kr-path 0 :parent) :box)))) ;m2top
		  (:where-attach :w))
	      (:w-move ,Multi-Move-Selection-Box
		  (:left ,(o-formula (first (gv (kr-path 0 :parent) :box)))) ;left
		       ;; This box is at the M1 if normal, at the bottom if not move
		  (:top ,(o-formula
			   (let ((box (gv (kr-path 0 :parent) :box)))
			     (if (gv (kr-path 0 :parent) :show-grow)
				 (eighth box) (tenth box)))))
		  (:where-attach :where-hit)))))  
  
(create-instance 'multi-sel-line-movegrow-feedback Opal:Line
   (:obj-over NIL)
   (:points (list 0 0 10 10))  ; some initial values (x1 y1 x2 y2)
   (:visible (o-formula (gvl :obj-over)))
   (:x1 (o-formula (first (gvl :points))))
   (:y1 (o-formula (second (gvl :points))))
   (:x2 (o-formula (third (gvl :points))))
   (:y2 (o-formula (fourth (gvl :points))))
   (:draw-function :xor)
   (:fast-redraw-p T)
   (:line-style opal:dashed-line))

(create-instance 'multi-sel-rect-movegrow-feedback opal:rectangle
   (:obj-over NIL)
   (:box '(80 20 100 150))
   (:visible (o-formula (gvl :obj-over)))
   (:left (o-formula (first (gvl :box))))
   (:top (o-formula (second (gvl :box))))
   (:width (o-formula (third (gvl :box))))
   (:height (o-formula (fourth (gvl :box))))
   (:draw-function :xor)
   (:fast-redraw-p T)
   (:line-style opal:dashed-line)
   (:filling-style NIL))

;; The new-selection should be a single object or a list of object that
;; should be selected, or NIL for there to be no selection.
(defun Set-Selection (gadget new-selection)
  "Call this to change which objects are selected."
  (let ((want-report (g-value gadget :report-function)))
    (unless (listp new-selection) ; so can pass in a single object
      (setq new-selection (list new-selection)))
    (when want-report
      (Call-Report-Function-Select want-report NIL :explicit gadget
				   NIL new-selection))
    (Internal-Set-Selection gadget new-selection)
    (kr-send gadget :selection-function gadget new-selection)))

;;; Doesn't call report-function, and new-selection must be a list
(defun Internal-Set-Selection (gadget new-selection)
  (let ((others (g-value gadget :other-multi-graphics-selection)))
    (if others
	(dolist (o others)
	  (let ((gadgetwin (g-value o :window))
		(l NIL))
	    ;; first, get the list of objects in new-selection that
	    ;; share a window with the gadget
	    (dolist (n new-selection)
	      (when (eq (g-value n :window) gadgetwin)
		(push n l)))
	    (Int-Set-Local-Value-Selection o l)
	    (Int-Set-Value-Selection o new-selection)))
	;; else, only one gadget
	(progn
	  (Int-Set-Local-Value-Selection gadget new-selection)
	  (Int-Set-Value-Selection gadget new-selection)))))

;;; This just sets the :value slot.  You should have already called
;;; Int-Set-Local-Value-Selection to set the :my-value slot.
;;; new-selection must be a list.
(defun Int-Set-Value-Selection (gadget new-selection)
  (s-value gadget :value new-selection)
  ;; sometimes :value is same list so doesn't notice
  (mark-as-changed gadget :value))

;;; Call this to set the internal :my-value slot.  new-selection must
;;; be a list.
(defun Int-Set-Local-Value-Selection (gadget new-selection)
  (let ((selinter (g-value gadget :select-it))
	(old-selection (g-value gadget :my-value)))
    (when (g-value gadget :multiple-select-p)
      (s-value selinter :start-char :middle-down)) ;; *HACK to make sure that
					;; how-set set correctly so selectobj
                                        ;; will allow multiple selections
    ;; first remove any objects that should not be selected
    (dolist (oldsel old-selection)
      (unless (member oldsel new-selection)
	(inter:DeSelectObj selinter oldsel)))
    ;; now add any objects that weren't selected
    (dolist (newsel new-selection)
      (unless (member newsel old-selection)
	(inter:SelectObj selinter newsel)))
    (s-value gadget :my-value new-selection)
    ;; sometimes :my-value is same list so doesn't notice
    (mark-as-changed gadget :my-value)
    ;; when multiple objects selected, keep fake rect up to date
    (when (> (length (g-value gadget :my-value)) 1)
      (Multi-Set-Rect-Multiple gadget))))

;; Find the bounding box of all the selected objects, and set it into the
;; fake rectangle.  Return the rectangle
(defun Multi-Set-Rect-Multiple (gadget)
  (let ((rect (g-value gadget :fake-rect))
	(minleft 9999)
	(mintop 9999)
	(maxright -9999)
	(maxbottom -9999)
	(check-move (g-value gadget :check-move-p))
	(check-grow (and (g-value gadget :grow-multiple-p)
			 (g-value gadget :check-grow-p)))
	one-moveable one-growable left top)
    (dolist (obj (g-value gadget :my-value))
      (when (and check-move (g-value obj :move-p))
	(setq one-moveable T))
      (when (and check-grow (g-value obj :grow-p))
	(setq one-growable T))
      (setq minleft (MIN minleft (setq left (g-value obj :left))))
      (setq mintop (MIN mintop (setq top (g-value obj :top))))
      (setq maxright (MAX maxright (+ left (g-value obj :width))))
      (setq maxbottom (MAX maxbottom (+ top (g-value obj :height)))))
    (s-value rect :orig-left minleft) ; used to compute the offsets
    (s-value rect :left minleft)
    (s-value rect :orig-top mintop)   ; used to compute the offsets
    (s-value rect :top mintop)
    (s-value rect :orig-width (- maxright minleft))
    (s-value rect :width (- maxright minleft))
    (s-value rect :orig-height (- maxbottom mintop))
    (s-value rect :height (- maxbottom mintop))
    (s-value rect :move-p (if check-move one-moveable T)) ; when check move,
					;  then ok when any can move
    (s-value rect :grow-p one-growable)
    rect))
  
;;; Get the new position from the fake rectangle, and move all the selected
;;; objects by the correct amount
(defun Multi-Adjust-all-objects (gadget)
  (let* ((rect (g-value gadget :fake-rect))
	 (box (g-value rect :box))
	 (offsetx (- (first box) (g-value rect :orig-left)))
	 (offsety (- (second box) (g-value rect :orig-top)))
	 (check-line (g-value gadget :check-line))
	 (check-group (g-value gadget :check-group))
	 (check-polygon (g-value gadget :check-polygon))
	 (check-ok-move (g-value gadget :check-move-p)))
    (dolist (obj (g-value gadget :my-value))
      (when (or (null check-ok-move) 
		(g-value obj :move-p))
	(cond ((or (and check-group (g-value obj :group-p))
		   (and check-polygon (g-value obj :polygon-p)))
	       (s-value obj :box (List (+ offsetx (g-value obj :left))
				       (+ offsety (g-value obj :top))
				       (g-value obj :width)
				       (g-value obj :height)))
	       (Move-Grow-Adjust-One-Object gadget obj NIL))

	      ((and check-line (g-value obj :line-p))
	       ;; then set both end-points, so will move
	       (incf (first (g-value obj :points)) offsetx)
	       (incf (second (g-value obj :points)) offsety)
	       (incf (third (g-value obj :points)) offsetx)
	       (incf (fourth (g-value obj :points)) offsety)
	       (mark-as-changed obj :points))
	      ((g-value obj :pie-piece-p)
	       (s-value obj :box (List (+ offsetx (g-value obj :left))
				       (+ offsety (g-value obj :top))
				       (g-value obj :width)
				       (g-value obj :height)))
	       (kr-send obj :move-grow obj))
	      (T;; else move as a box
	       (incf (first (g-value obj :box)) offsetx)
	       (incf (second (g-value obj :box)) offsety)
	       (mark-as-changed obj :box)))))
    (Multi-Set-Rect-Multiple gadget)))


	
;; opal:point-to-component is a macro, so need to have a function so
;; can funcall it.
(defun point-to-comp (obj x y)
  (opal:point-to-component obj x y))

;; custom WHERE method that returns the component object under the
;; selected object, if any, else returns NIL.  Also sets the currently
;; selected object into the :deselect-this-obj slot of the interactor
(defun Multi-Select-Get-Child (gadget inter event)
  (let ((selected-objs (g-value gadget :my-value))
	(x (inter:event-x event))
	(y (inter:event-y event))
	in-obj)
    (when (eq (g-value gadget :window) ; first check if windows are equal
	      (inter:event-window event))
      (if (listp selected-objs)
	  (dolist (obj selected-objs)
	    (when (opal:point-in-gob obj x y)
	      (setq in-obj obj)
	      (return)))
	  ; else not a list
	  (when (opal:point-in-gob selected-objs x y)
	      (setq in-obj selected-objs)))
      ;; here have object point is in, or NIL
      (when in-obj
	(let ((method (g-value gadget :down-to-component-function))
	      obj)
	  (setq obj (funcall method in-obj x y))
	  (s-value inter :deselect-this-obj in-obj)
	  obj)))))

(create-instance 'Multi-Graphics-Selection opal:aggregadget
  :declare ((:parameters :active-p :start-where :running-where
			 :check-line :check-polygon :check-group
			 :check-move-p :check-grow-p :move-multiple-p
			 :grow-multiple-p :input-filter :want-undo
			 :multiple-select-p :allow-component-select
			 :down-to-component-function :modify-function
			 :selection-function :other-multi-graphics-selection)
	    (:type (kr-boolean :active-p :check-line :check-polygon
		    :check-group :check-move-p :check-grow-p :move-multiple-p
		    :grow-multiple-p :want-undo :multiple-select-p
		    :allow-component-select)
		   ((or list (member T)) :start-where :running-where)
		   ((or null function symbol)
		    :selection-function :modify-function)
		   ((or function symbol) :down-to-component-function)
		   ((or list integer function symbol) :input-filter)))
		  
     ;; programmer-settable slots
     (:start-where NIL) ;; supply a valid start-where here
     (:running-where T) ;; if supplied, then this is the area in which the
			;; objects can move and grow
     (:selection-function NIL) ;; this is called when the selection changes
     (:modify-function NIL) ;; called when an object is changed size or position
     (:input-filter NIL)   ;; default is no gridding
     (:check-line T)       ;; whether to check for :line-p in objects
     (:check-group T)      ;; where to check for :group-p in objects
     (:check-polygon T)    ;; where to check for :polygon-p in objects
     (:check-grow-p NIL)   ;; whether check for :grow-p in object
     (:check-move-p NIL)   ;; whether check for :move-p in object
     (:move-multiple-p T)  ;; whether move multiple as a group
     (:grow-multiple-p T)  ;; whether grow multiple as a group

     (:want-undo NIL)      ;; whether want to save copies for undo
     (:report-function NIL);; if want to find out what happens,
			   ;; :want-undo must be T for :report-function to work
     
     (:active-p T)         ;; whether this is operational or not

     (:multiple-select-p T) ;;if T, then multiple objects can be selected.

     (:allow-component-select NIL) ; set to T if can "push" selection to a child
     (:down-to-component-function #'Point-To-Comp)
     (:move-components-p NIL) ; whether components are allowed to move
     (:grow-components-p NIL) ; and grow.  Only valid if :check-grow-p
			      ; or :check-move-p

     ;; slots the programmer can access
     (:current-selected-object NIL) ; set with new selection or object to be moved
				    ; or grown before other slots are set.
     (:value NIL)  ;;current object or objects selected **DO NOT SET**

     (:other-multi-graphics-selection NIL) ; for multiple windows

     ;; internal slots

     (:my-value NIL) ; the selected objects only for this window

     (:grow-ok (o-formula  ; ok to grow if only one selection
		(if (gvl :multiple-select-p)    
		    (<= (length (gvl :my-value)) 1)
		    T)))
     (:is-line (o-formula (and (gvl :check-line)
			       (gvl :current-selected-object)
			       (gvl :current-selected-object :line-p))))
     (:cur-movegrow-feedback (o-formula (if (gvl :is-line)
					    (gvl :line-movegrow-feedback)
					    (gvl :rect-movegrow-feedback))))
     (:using-fake-rectangle ; this is T when moving or growing multiple objects
      (o-formula (and (or (gvl :move-multiple-p)(gvl :grow-multiple-p))
		      (> (length (gvl :my-value)) 1))))
     (:special-multi-feedback-vis (o-formula (and (gvl :using-fake-rectangle)
						  (gvl :fake-rect :grow-p))))

     (:save-list-for-undo NIL) ; maintained if want undo

     ;;; selection-boxes can't be part of the aggregadget since the
     ;;; interactor will make instances of it, so create it in the interactor
     (:parts `((:selection-boxes ,opal:aggregate) ; added explicitly
	       (:special-multi-feedback ,special-multi-feedback)
	       (:line-movegrow-feedback ,multi-sel-line-movegrow-feedback)
	       (:rect-movegrow-feedback ,multi-sel-rect-movegrow-feedback)
	       ;; fake-rect is used to get the bounding box of all the
	       ;; selected objects, so the move-grow interactor will have
	       ;; something to operate on
	       (:fake-rect ,opal:rectangle (:visible NIL)
			   (:grow-p T) 
			   (:move-p T))))
     (:interactors
      `((:select-it ,inter:button-interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))
	 (:start-event (:leftdown :shift-leftdown :middledown))
	 (:slots-to-set (:gg-interim-selected :gg-selected :gg-selected))
	 (:continuous T)
	 (:running-where T)
	 (:active ,(o-formula (gvl :operates-on :active-p)))
	 (:start-where ,(o-formula (gvl :operates-on :start-where)))

	 (:how-set ,(o-formula (if (gvl :operates-on :multiple-select-p)
				   (if (eq (gvl :start-char) :leftdown)
				       :set :list-toggle)
				   :set)))
	 (:final-feedback-obj NIL)
	 (:do-start
	  ,#'(lambda(inter obj event)
	       ;;; have to create it explicitly the first time because
	       ;;; if part of aggregadget then complains because not constant!
	       (unless (g-value inter :final-feedback-obj)
		 (let ((gadget (g-value inter :operates-on))
		       sel)
		   (setq sel (create-instance NIL sel-selection-feedback))
		   (opal:add-component (g-value gadget :selection-boxes)
				       sel)
		   (s-value inter :final-feedback-obj sel)))
	       (call-prototype-method inter obj event)))
	 (:final-function
	  ,#'(lambda (inter objUnderMouse)
	       (let* ((oper (g-value inter :operates-on))
		      (how-set (g-value inter :how-set))
		      (newval (g-value inter :main-aggregate :gg-selected))
		      (newval-l (if (listp newval)
				    newval
				    (list newval)))
		      (want-report (g-value oper :report-function))
		      (others (g-value oper :other-multi-graphics-selection))
		      (newval-all newval-l))
		 (s-value oper :current-selected-object objUnderMouse)
		 (s-value oper :my-value newval-l)
		 (when others
		   (if (eq how-set :set)
		       ;; then a single value.  It is in oper, so others
		       ;; should go off.
		       (dolist (o others)
			 (unless (eq o oper)
			   (Int-Set-Local-Value-Selection o NIL)))
		       ;; else add or remove from a set
		       (dolist (o others)
			 ;; don't need to change the other's :my-value
			 (unless (eq o oper)
			   (setq newval-all (append (g-value o :my-value)
						    newval-all))))))
		 ;; must be called before set the :value slot
		 (when want-report
		   (Call-Report-Function-Select want-report
						(g-value inter :start-char)
						:click oper
						objUnderMouse newval-all))
		 ;; now set all the :value slots (including me)
		 (if others
		     (dolist (o others)
		       (Int-Set-Value-Selection o newval-all))
		     (Int-Set-Value-Selection oper newval-all))

		 ;; when multiple objects selected, keep fake rect up to date
		 (when (g-value oper :using-fake-rectangle)
		   (Multi-Set-Rect-Multiple oper))
		 (kr-send oper :selection-function oper
			  (g-value oper :value))))))
	;; DESIGN: When the left button goes down, both the :select-it
	;; and :select-in-region interactors start.  If the
	;; :select-in-region interactor gets a large enough area, then
	;; it aborts the :select-it interactor.
	(:select-in-region ,inter:two-point-interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))
	 (:start-event (:leftdown :shift-leftdown :middledown))
	 (:continuous T)
	 (:start-where ,(o-formula (list :in (gvl :operates-on :window))))
	 (:abort-if-too-small T)
	 (:active ,(o-formula (gvl :operates-on :active-p)))
	 (:outside NIL)
	 (:running-where T)
	 (:min-width 10)
	 (:min-height 10)
	 (:line-p NIL)
	 (:feedback-obj ,(o-formula (gvl :operates-on
					 :rect-movegrow-feedback)))
	 (:aborted-select-it NIL) ;; set when abort :select-it
	 (:running-action
	  ,#'(lambda (inter new-points)
	       (when (and new-points
			  (null (g-value inter :aborted-select-it)))
		 ;; then got a big enough area, so abort other
		 (s-value inter :aborted-select-it T)
		 (s-value inter :abort-if-too-small NIL) ;going small
					;now doesn't count
		 (inter:abort-interactor
		  (g-value inter :operates-on :select-it)))
	       (call-prototype-method inter new-points)))
	 (:final-function
	  ,#'(lambda (inter final-points)
	       (s-value inter :aborted-select-it NIL)
	       (s-value inter :abort-if-too-small T)
	       (when final-points
		 ;; then was big enough area
		 (let ((oper (g-value inter :operates-on)))
		   (Select-All-Objs-In-Area oper final-points
					    (g-value inter :start-char))
		   (kr-send oper :selection-function oper
			    (g-value oper :value))))))
	 (:abort-action ,#'(lambda (inter)
			     (s-value inter :aborted-select-it NIL)
			     (s-value inter :abort-if-too-small T)
			     (call-prototype-method inter))))
	(:down-level-select-it ,inter:button-interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))
	 (:active ,(o-formula (and (gvl :operates-on :active-p)
				   (gv-local :self :operates-on
					     :allow-component-select))))
	 (:start-event (:control-leftdown :shift-control-leftdown
					  :control-middledown))
	 (:continuous NIL)
	 (:slots-to-set (:gg-interim-selected :gg-selected :gg-selected))
	 (:start-where ,(o-formula (list :custom
					 (gvl :operates-on)
					 'Multi-Select-Get-Child)))
	 (:how-set ,(o-formula (if (gvl :operates-on :multiple-select-p)
				   (if (eq (gvl :start-char) :control-leftdown)
				       :set :list-toggle)
				   :set)))
	 (:final-function
	  ,#'(lambda (inter objUnderMouse)
	       (let* ((oper (g-value inter :operates-on))
		      (objToUnSelect (g-value inter :deselect-this-obj))
		      (how-set (g-value inter :how-set))
		      (want-report (g-value oper :report-function))
		      (others (g-value oper :other-multi-graphics-selection))
		      new-selection)
		 (if others  ;; then selecting in multiple windows 
		     (if (eq :set how-set) ; then single selection
			 (progn
			   (setq new-selection (list objUnderMouse))
			   (Int-Set-Local-Value-Selection oper new-selection)
			   (dolist (o others)
			     (unless (eq o oper)
			       (Int-Set-Local-Value-Selection o NIL))))
			 ;; else don't need to change the other's :my-value
			 (progn
			   (setq new-selection
			     (cons objUnderMouse
				(remove objToUnSelect (g-value oper :value))))
			   (Int-Set-Local-Value-Selection oper
			       (cons objUnderMouse
				     (remove objToUnSelect
					     (g-value oper :my-value))))))
		   ;; else no others
		   (progn
		     (if (eq :set how-set)
			 (setq new-selection (list objUnderMouse))
			 ;; else needs to be a list
			 (setq new-selection
			       (cons objUnderMouse
			     (remove objToUnSelect (g-value oper :my-value)))))
		     (Int-Set-Local-Value-Selection oper new-selection)))
		 ;; must be called before set the :value slot
		 (when want-report
		   (Call-Report-Function-Select want-report
						(g-value inter :start-char)
						:down-level oper objToUnSelect
						new-selection))
		 ;; now set all the :value slots (including me)
		 (if others
		     (dolist (o others)
		       (Int-Set-Value-Selection o new-selection))
		     (Int-Set-Value-Selection oper new-selection))

;		 (unless (g-value oper :grow-components-p)
;		   (s-value objUnderMouse :grow-p NIL))
;		 (unless (g-value oper :move-components-p)
;		   (s-value objUnderMouse :move-p NIL))

		 (kr-send oper :selection-function oper new-selection)))))

	(:move-grow-it ,inter:Move-Grow-Interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))
	 (:active ,(o-formula (and (gvl :operates-on :my-value)
				   (gvl :operates-on :active-p))))
	 (:continuous T)
	 (:waiting-priority ,inter:high-priority-level)
	 (:input-filter ,(o-formula (gvl :operates-on :input-filter)))
	 (:abort-event (:control-g :control-\g))
	 (:start-where ,(o-formula
			 (list :list-leaf-element-of 
			       (gvl :operates-on :select-it)
			       :final-feed-inuse)))
	 (:running-where ,(o-formula (gvl :operates-on :running-where)))
	 (:outside NIL)
	 (:feedback-obj ,(o-formula (gvl :operates-on :cur-movegrow-feedback)))
	 ;; change the object that the selection boxes are attached to
	 (:using-fake-rectangle ; this is T when moving multiple objects
	  ,(o-formula (gvl :operates-on :using-fake-rectangle)))
	 (:obj-to-change 
	  ,(o-formula (let ((oper (gvl :operates-on)))
			(if (gvl :using-fake-rectangle)
			    (gv oper :fake-rect)
			    ;;otherwise use the obj under the selection handles
			    (gvl :first-obj-over :parent :obj-over)))))

	 ;; the various attachpoints are stored in a slot in each selection
	 ;; box and the box is returned set into the :first-obj-over slot of
	 ;; the interactor
	 (:attach-point ,(o-formula (if (gvl :using-fake-rectangle)
					:where-hit ; then always use :where-hit
					(gvl :first-obj-over :where-attach))))
	 (:grow-p ,(o-formula (if (gvl :using-fake-rectangle)
				  NIL ; then always move 
				  (gvl :first-obj-over :grow-p))))
	 (:line-p ,(o-formula (if (gvl :using-fake-rectangle)
				  NIL ; then not a line
				  (gvl :first-obj-over :parent :line-p))))
	 
	 ;; have a special start-action to check to see if it is OK to
	 ;; start, and if not, then beep and abort.  
	 (:start-action
	  ,#'(lambda (inter obj-being-changed first-points)
	       ;; obj-being-changed object underneath
	       (let ((oper (g-value inter :operates-on)))
		 ;; first, make sure have saved info for poly's or groups
		 (Set-Old-In-Poly-or-Group oper (g-value oper :my-value))
		 (s-value oper :current-selected-object obj-being-changed)
		 (if (if (g-value inter :grow-p)
			 (if (g-value oper :check-grow-p)
			     (g-value obj-being-changed :grow-p)
			     T)
			 (if (g-value oper :check-move-p)
			     (g-value obj-being-changed :move-p)
			     T))
		     ;; then OK to move or grow it
		     (progn
		       (Save-Selection-For-Undo oper)
		       (Save-Selection-For-Report-Before-M-G oper)
		       (call-prototype-method inter obj-being-changed
					      first-points))
		     ;; else not OK, so abort
		     (progn (inter::beep)
			    (Inter:Abort-Interactor inter))))))
	 ;; check if fake-rect, then do all objects,
	 ;; call the modify method, if any.
	 (:final-function
	  ,#'(lambda(inter objUnderMouse newsize)
	       (let* ((oper (g-value inter :operates-on))
		      (want-report (g-value oper :report-function)))
		 (if (eq objUnderMouse (g-value oper :fake-rect))
		     (progn
		       (Multi-Adjust-all-objects oper)
		       (kr-send oper :modify-function
				oper (g-value oper :my-value)
				newsize))
		     (let ((obj (g-value oper :current-selected-object)))
		       (Move-Grow-Adjust-One-Object oper obj
						    (g-value inter :grow-p))
		       (kr-send oper :modify-function oper obj newsize)))
		 (when want-report
		   (Call-Report-Function-MoveGrow oper (g-value inter :grow-p)
						  (g-value inter :attach-point)
						  want-report))))))
	;;; just used to grow multiple objects
	(:grow-multiple ,inter:Move-Grow-Interactor
	 (:window ,(o-formula (gv-local :self :operates-on :window)))
	 (:active ,(o-formula (and (gvl :operates-on :active-p)
				   (gvl :operates-on
					:special-multi-feedback-vis))))
	 (:continuous T)
	 ;; needs to be higher than "high", since move-grow-it is "high"
	 (:waiting-priority ,inter:running-priority-level)
	 (:input-filter ,(o-formula (gvl :operates-on :input-filter)))
	 (:abort-event (:control-g :control-\g))
	 (:start-where ,(o-formula
			 (list :leaf-element-of 
			       (gvl :operates-on :special-multi-feedback))))
	 (:running-where ,(o-formula (gvl :operates-on :running-where)))
	 (:outside NIL)
	 (:feedback-obj ,(o-formula (gvl :operates-on
					 :rect-movegrow-feedback)))
	 (:obj-to-change ,(o-formula (gvl :operates-on :fake-rect)))
	 ;; the various attachpoints are stored in a slot in each selection
	 ;; box and the box is returned set into the :first-obj-over slot of
	 ;; the interactor
	 (:attach-point ,(o-formula (gvl :first-obj-over :where-attach)))
	 (:grow-p T)
	 (:line-p NIL)
	 ;; check if fake-rect, then do all objects,
	 ;; call the modify method, if any.
	 (:start-action
	  ,#'(lambda (inter obj-being-changed first-points)
	       (let ((oper (g-value inter :operates-on)))
		 (Save-Selection-For-Undo oper)
		 (Save-Selection-For-Report-Before-M-G oper)
		 (call-prototype-method inter obj-being-changed
					first-points))))
	 (:final-function
	  ,#'(lambda(inter objUnderMouse newsize)
	       (declare (ignore objUnderMouse))
	       (let* ((oper (g-value inter :operates-on))
		      (want-report (g-value oper :report-function)))
		 (Multi-Adjust-all-objects-Grow oper)
		 (when want-report
		   (Call-Report-Function-MoveGrow oper T
						  (g-value inter :attach-point)
						  want-report))
		 (kr-send oper :modify-function oper (g-value oper :my-value)
			  newsize))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Report-Function Stuff
;;;   called as (lambda (gadget main-op param obj oldval newval)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; must be called BEFORE the selection gadget is set and ONLY when
;; want a report
(defun Call-Report-Function-Select (report-func button type gadget obj newval)
  (let ((parameter (ecase type
		     (:explicit :explicit-select)
		     (:region (if (eq :leftdown button)
				  :region-select
				  :region-toggle))
		     (:click (if (eq :leftdown button)
				 (if (eq obj :none)
				     :clear-select
				     :become-select)
				 :toggle-select))
		     (:down-level (if (eq :leftdown button)
				      :downlevel-select
				      :downlevel-toggle)))))
    (funcall report-func gadget :select parameter obj
	     (copy-list (g-value gadget :value))
	     (copy-list newval))))


;;; Assign into the gadget's slot :report-func-old-vals a copy of the
;;; selected objects current :box, :points or whatever, so can be
;;; reported after the move.
(defun Save-Selection-For-Report-Before-M-G (gadget)
  (when (g-value gadget :report-function)
    (s-value gadget :report-func-old-vals
	     (Get-Cur-Pos-Desc (g-value gadget :my-value) gadget))))

(defun Get-Cur-Pos-Desc (obj-list gadget)
  (let (cur-vals box)
    (dolist (obj obj-list)
      (cond ((and (g-value gadget :check-polygon)
		  (g-value obj :polygon-p))
	     (push (copy-list (g-value obj :point-list)) cur-vals))
	    ((and (g-value gadget :check-line)
		  (g-value obj :line-p))
	     (push (copy-list (g-value obj :points)) cur-vals))
	    (T (setq box (g-value obj :box))
	       (if box (setq box (copy-list box))
		   ;; else might be NIL if newly created group
		   (setq box (list (g-value obj :left)
				 (g-value obj :top)
				 (g-value obj :width)
				 (g-value obj :height))))
	       (push box cur-vals))))
    (nreverse cur-vals)))

(defun Call-Report-Function-MoveGrow (gadget grow-p attach-point report-func)
  (let* ((sel-list (g-value gadget :my-value))
	 (old-vals (g-value gadget :report-func-old-vals))
	 (new-vals (Get-Cur-Pos-Desc sel-list gadget)))
    (funcall report-func gadget (if grow-p :grow :move) attach-point
	     sel-list old-vals new-vals)
    (s-value gadget :report-func-old-vals NIL)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; If the object is a box or line, it has been handled automatically
;;; by the interactor, but if it is a group or a polygon, then some
;;; extra work is needed. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The :*orig slots are set just before the move or grow starts (as
;; the start action of the move-grow-it interactor).  Not needed for
;; the group interactor, since that one uses the fake rect's box and
;; left and new and old.

(defun Move-Grow-Adjust-One-Object (gadget obj growing)
  (let ((check-group (g-value gadget :check-group))
	(check-polygon (g-value gadget :check-polygon)))
    (cond ((and check-group (g-value obj :group-p))
	   (adjust-all-items-of-group-size obj (g-value obj :box)
					   (g-value obj :*orig-left)
					   (g-value obj :*orig-top)
					   (g-value obj :*orig-width)
					   (g-value obj :*orig-height)
					   (g-value gadget :check-line)
					   check-polygon
					   (and (g-value gadget :check-grow-p)
						growing)))
	  ((and check-polygon (g-value obj :polygon-p))
	   (polyline-correction-fun obj (g-value obj :box)
				    (g-value obj :*orig-left)
				    (g-value obj :*orig-top)
				    (g-value obj :*orig-width)
				    (g-value obj :*orig-height)
				    (and (g-value gadget :check-grow-p)
						growing)))
	  ((g-value obj :pie-piece-p) (kr-send obj :move-grow obj)))
    (Multi-Set-Rect-Multiple gadget)))

;; For polygons and groups, need to save old :box, so can determine how to
;; modify when moved or grown.  This is called when start growing or moving.
(defun Set-Old-In-Poly-or-Group (gadget sel-list)
  (let ((check-group (g-value gadget :check-group))
	(check-polygon (g-value gadget :check-polygon)))
    (internal-Set-Old-In-Poly-or-Group
     (if (listp sel-list) sel-list (list sel-list))
     check-group check-polygon)))

(defun Internal-Set-Old-In-Poly-or-Group (obj-list check-group check-polygon)
  (let (group-p)
    (dolist (obj obj-list)
      (when (or (setq group-p (and check-group (g-value obj :group-p)))
		(and check-polygon (g-value obj :polygon-p)))
	(s-value obj :*orig-left (g-value obj :left))
	(s-value obj :*orig-top (g-value obj :top))
	(s-value obj :*orig-width (g-value obj :width))
	(s-value obj :*orig-height (g-value obj :height)))
      (when group-p
	(internal-Set-Old-In-Poly-or-Group (g-value obj :components)
					   check-group check-polygon)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The next set of functions are to allow growing of multiple objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; grow all the objects that are selected
;;; This should only be called when it is OK to move and grow multiple objects.
(defun Multi-Adjust-all-objects-Grow (gadget)
  (let* ((rect (g-value gadget :fake-rect))
	 (new-points (g-value rect :box))
	 (check-line (g-value gadget :check-line))
	 (check-group (g-value gadget :check-group))
	 (check-polygon (g-value gadget :check-polygon))
	 (check-ok-grow (g-value gadget :check-grow-p))
	 )
    (internal-modifyfun-group (g-value gadget :my-value)
			      new-points
			      (g-value rect :orig-left)
			      (g-value rect :orig-top)
			      (g-value rect :orig-width)
			      (g-value rect :orig-height)
			      check-line check-group check-polygon
			      check-ok-grow)
    (Multi-Set-Rect-Multiple gadget)))

;;; Main internal function for growing a collection of selected objects
(defun internal-modifyfun-group (selected-objects new-points
				 old-left old-top old-width old-height
				 check-line check-group check-polygon
				 check-ok-grow)

  ;; first, make sure width and height are greater than 2
  (when (< (third new-points) 2)
      (setf (third new-points) 2))
  (when (< (fourth new-points) 2)
      (setf (fourth new-points) 2))
  
  ;; now adjust all objects in group
  (dolist (main-item selected-objects)
    (adjust-one-item-size main-item new-points
			  old-left old-top old-width old-height check-line
			  check-group check-polygon check-ok-grow)))

(defun adjust-one-item-size (one-item new-points
			     old-left old-top old-width old-height
			     check-line check-group check-polygon
			     check-ok-grow)
  (cond ((and check-group (g-value one-item :group-p))
	 (adjust-all-items-of-group-size one-item new-points old-left
					 old-top old-width old-height
					 check-line check-polygon
					 check-ok-grow))
	((and check-line (g-value one-item :line-p))
	 (line-correction-fun one-item new-points old-left old-top
			      old-width old-height check-ok-grow)) 
	((and check-polygon (g-value one-item :polygon-p))
	 (polyline-correction-fun one-item new-points old-left old-top
				  old-width old-height check-ok-grow)) 
	(T (box-correction-fun one-item new-points old-left old-top
			       old-width old-height check-ok-grow)))) 


(defun adjust-all-items-of-group-size (group-item new-points old-left
				       old-top old-width old-height
				       check-line check-polygon check-ok-grow)
  (dolist (one-item (g-value group-item :components))
    (adjust-one-item-size one-item new-points
			  old-left old-top old-width old-height
			  check-line T check-polygon check-ok-grow)))

;;; allocates a new list which is the current points modified by the
;;; difference between the group's new-points and the old-points
(defun calculate-new-box (orig-l orig-t orig-w orig-h new-points
			  old-left old-top old-width old-height ok-grow)
  (list (round (+ (+ (* (/ (third new-points)
			   old-width)
			(- orig-l
			   old-left))
		     old-left)
		  (- (first new-points)
		     old-left)))
	     
	(round (+ (+ (* (/ (fourth new-points)
			   old-height)
			(- orig-t
			   old-top))
		     old-top)
		  (- (second new-points)
		     old-top)))
	     
	(if ok-grow
	    (round (* (/ (third new-points)
			 old-width)
		      orig-w))
	    old-width)
	    
  
	(if ok-grow
	    (round (* (/ (fourth new-points)
			 old-height)
		      orig-h))
	    old-height)))


;; Calculates the new pointlist for a polygon by destructively
;; modifying the current-points list.  Better have an even number of entries!
(defun calculate-new-pointlist (current-points old-left new-left old-top
			        new-top width-ratio height-ratio)
  (let (newx newy)
    (do* ((l current-points (nthcdr 2 l))
	  (old-x (first l)(first l))
	  (old-y (second l)(second l))
	  (cnt 0 (+ 2 cnt)))
	 ((null l))
      (setq newx (round (+ (+ (* (- old-x
				    old-left)
				 width-ratio)
			      old-left)
			   (- new-left old-left))))
      (setq newy (round (+ (+ (* (- old-y
				    old-top)
				 height-ratio)
			      old-top)
			   (- new-top old-top))))
      (setf (first l) newx)
      (setf (second l) newy))
    current-points))
      
(defun polyline-correction-fun (poly-obj new-points old-left old-top
					 old-width old-height check-ok-grow)
  (let* ((growp (if check-ok-grow (g-value poly-obj :grow-p) T))
	 (widthratio (if growp (/ (third new-points) old-width)
			 1))
	 (heightratio (if growp (/ (fourth new-points) old-height)
			 1))
	 (new-pointlist (calculate-new-pointlist
			(g-value poly-obj :point-list)
			old-left
			(first new-points)
			old-top
			(second new-points)
			widthratio
			heightratio)))			
    (s-value poly-obj :point-list new-pointlist)
    (mark-as-changed poly-obj :point-list)))

(defun line-correction-fun (current-line new-points old-left old-top
					 old-width old-height check-ok-grow)
  (let* ((growp (if check-ok-grow (g-value current-line :grow-p) T))
	 (newbox (calculate-new-box (g-value current-line :x1)
				    (g-value current-line :y1)
				     (- (g-value current-line :x2)
					(g-value current-line :x1))
				     (- (g-value current-line :y2)
					(g-value current-line :y1))
				     new-points
				     old-left old-top old-width old-height
				     growp))
	 (newx2 (+ (first newbox) (third newbox)))
	 (newy2 (+ (second newbox) (fourth newbox))))
    (s-value current-line :points
	     (list (first newbox) (second newbox) newx2 newy2))))

(defun box-correction-fun (obj new-points old-left old-top old-width
			       old-height check-ok-grow)
  (let* ((growp (if check-ok-grow (g-value obj :grow-p) T))
	 (newbox (calculate-new-box (g-value obj :left)
				    (g-value obj :top)
				    (g-value obj :width)
				    (g-value obj :height)
				    new-points old-left old-top
				    old-width old-height growp)))
    (s-value obj :box newbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions to support Undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
The :save-list-for-undo slot of the gadget will contain
   (  (list of all selected objects)
     ((obj1 old-vals)
      (obj2 old-vals)
     )
   )
|#

;;; Call this right before each move or grow.
(defun Save-Selection-For-Undo (gadget)
  (when (g-value gadget :want-undo)
    (let ((sel-list (g-value gadget :my-value))
	  (check-group (g-value gadget :check-group))
	  (check-polygon (g-value gadget :check-polygon))
	  (check-line (g-value gadget :check-line)))
      (s-value gadget :save-list-for-undo
	       (list (copy-list sel-list)
		     (Internal-Save-Selection-For-Undo
		      (if (listp sel-list) sel-list (list sel-list))
		      check-group check-polygon check-line))))))

(defun Internal-Save-Selection-For-Undo (sel-list check-group check-polygon
						  check-line)
  (let (new-undo-list)
    (dolist (obj sel-list)
      (cond ((and check-group (g-value obj :group-p))
	     ;; put all sub objects onto the list.  Add group AFTER its objects
	     (setq new-undo-list (nconc (Internal-Save-Selection-For-Undo
					 (g-value obj :components)
					 check-group check-polygon
					 check-line)
					(list (list obj))
					new-undo-list)))
	    ((and check-polygon (g-value obj :polygon-p))
	     (push (list obj (copy-list (g-value obj :point-list)))
		   new-undo-list))
	    ((and check-line (g-value obj :line-p))
	     (push (list obj (copy-list (g-value obj :points)))
		   new-undo-list))
	    (T (push (list obj (copy-list (g-value obj :box)))
		     new-undo-list))))
    new-undo-list))

;;; First do some error checking, if fails, return a helpful? string,
;;; so NIL means everything was successful.  Call update after this to
;;; see results.
(defun Undo-Last-Move-Grow (gadget)
  (let ((undo-list (g-value gadget :save-list-for-undo)))
    ;; now check and do.
    (cond ((not (g-value gadget :want-undo))
	   "Selection Gadget was not configured to save Undo Information")
	  ((or (null undo-list)
	       (null (car undo-list)))
	   "Nothing saved to undo")
	  ((dolist (o (cadr undo-list))
	     (unless (schema-p (car o))
	       (return (format NIL
			"Object ~s saved on undo list but now not an object"
			(car o))))))
	  (T
	   ;; first, set up to undo the undo (toggle)
	   (let ((sel-list (car undo-list))
		 (check-group (g-value gadget :check-group))
		 (check-polygon (g-value gadget :check-polygon))
		 (check-line (g-value gadget :check-line)))
	     (s-value gadget :save-list-for-undo
		      (list sel-list
			    (Internal-Save-Selection-For-Undo sel-list
				  check-group check-polygon check-line)))
	     (Internal-Restore-Undo-List (cadr undo-list) check-group
					 check-polygon check-line)
	     (Internal-Set-Selection gadget sel-list)
	     (kr-send gadget :selection-function gadget sel-list)
	     NIL)))))

(defun Internal-Restore-Undo-List (undo-list check-group check-polygon
					     check-line)
  (dolist (desc undo-list)
    (let ((obj (car desc))
	  (vals (cadr desc)))
      (cond ((and check-group (g-value obj :group-p))
	     ;; all of its objects should be restored already, so :left
	     ;; etc. should be fine, so can g-value them to get :*orig
	     (s-value obj :*orig-left (g-value obj :left))
	     (s-value obj :*orig-top (g-value obj :top))
	     (s-value obj :*orig-width (g-value obj :width))
	     (s-value obj :*orig-height (g-value obj :height)))

	    ((and check-polygon (g-value obj :polygon-p))
	     (s-value obj :point-list vals)
	     (s-value obj :*orig-left (g-value obj :left))
	     (s-value obj :*orig-top (g-value obj :top))
	     (s-value obj :*orig-width (g-value obj :width))
	     (s-value obj :*orig-height (g-value obj :height)))
	  
	    ((and check-line (g-value obj :line-p))
	     (s-value obj :points vals))

	    (T (s-value obj :box vals))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selecting objects in an area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Select-All-Objs-In-Area (gadget final-points button)
  (let ((start-where (g-value gadget :start-where))
	(want-report (g-value gadget :report-function))
	(others (g-value gadget :other-multi-graphics-selection))
	objs-in-rect newval)
    (setq objs-in-rect (Returns-objs-in-rect start-where final-points
					     (g-value gadget :window)))

    (if others
	(progn ;; then selecting in multiple windows 
	  (if (eq button :leftdown) ; then single selection
	      (progn
		(setq newval objs-in-rect)
		(Int-Set-Local-Value-Selection gadget newval)
		(dolist (o others)
		  (unless (eq o gadget)
		    (Int-Set-Local-Value-Selection o NIL))))
	      ;; else don't need to change the other's :my-value
	      ;; toggle the objects in the rectangle with the selection
	      (let ((old-l-selection (g-value gadget :my-value))
		    (old-all-selection (g-value gadget :value)))
		(Int-Set-Local-Value-Selection gadget
					       (set-exclusive-or objs-in-rect
					          old-l-selection))
		(setq newval (set-exclusive-or objs-in-rect
					       old-all-selection))))
	  ;; now set all the :value slots (including me)
	  (dolist (o others)
	    (Int-Set-Value-Selection o newval)))
	
	;; else no others
	(progn
	  (if (eq button :leftdown)
	      (setq newval objs-in-rect)
	      ;; else toggle the objects in the rectangle with the selection
	      (let ((old-selection (g-value gadget :my-value)))
		(setq newval (set-exclusive-or objs-in-rect old-selection))))
	  (Int-Set-Local-Value-Selection gadget newval)
	  (Int-Set-Value-Selection gadget newval)))
    
    (when want-report
      (Call-Report-Function-Select want-report button :region gadget
				   NIL newval)) 
    ))

;; used for
;;   :list-element-of
;;   :list-element-of-or-none
;;   :list-check-leaf-but-return-element-or-none
;;   :list-check-leaf-but-return-element
(defun list-element-of-branch (agg slot type win left top right bottom)
  (let ((lst (g-value agg slot))
	objs-inside)
    (dolist (i lst)
      (when (and (inter::checkobjtype i type)
		 (eq (g-value i :window) win)
		 (opal:obj-in-rectangle i top left bottom right :type type
					:INTERSECT NIL)
		 )
	(push i objs-inside)))
    objs-inside))

;; used for
;;   :list-leaf-element-of
;;   :list-leaf-element-of-or-none
(defun list-leaf-element-of-branch (agg slot type win left top right bottom)
  (let ((lst (g-value agg slot))
	objs-inside)
    (dolist (i lst)
      (when
	  (and (eq (g-value i :window) win)
	       (if (is-a-p i opal:aggregate)
		   ;; if aggregate, then if point-to-leaf
		   (setq objs-inside
			 (nconc objs-inside
				(opal:leaf-objects-in-rectangle
				 i top left bottom right :type type
				 :INTERSECT NIL)))
		   ;; if not aggregate, then if inside
		   (when (and (inter::checkobjtype i type)
			      (opal:obj-in-rectangle i top left bottom right
						     :type type
						     :INTERSECT NIL))
		     (push i objs-inside))))))
    objs-inside))

(defun Returns-objs-in-rect (where rect win)
  (let* ((left (first rect))
	 (top (second rect))
	 (right (+ left (third rect) -1))
	 (bottom (+ top (fourth rect) -1))
	 result)
    (setq result
      (cond ((eq t where) NIL)
	    ((null where) NIL)
	    ((listp where)
	     (let ((control (first where))
		   (agg (second where))
		   ;; T as type => everything
		   (type (or (inter::GetNextItem :type where) T))
		   objwin slot)
	       (unless (schema-p agg)
		 (error "obj ~s not valid" agg))
	       (setf objwin (and agg (g-value agg :window)))
	       (if (or (eq control :list-element-of)
		       (eq control :list-leaf-element-of)
		       (eq control :list-check-leaf-but-return-element)
		       (eq control :list-element-of-or-none)
		       (eq control :list-leaf-element-of-or-none)
		       (eq control :list-check-leaf-but-return-element-or-none)
		       )
		; If a list, then objects might be in different
  	        ; windows, so do test inside the case statement.
		; If Custom, let the user's procedure do the test.
		   (setf slot (third where))
		   ;; else check if window of object eq window of event
		   (unless (eq win objwin) ; otherwise test here
		      (return-from Returns-objs-in-rect NIL)))
	       (case control
		 (:custom 
		  (error "Custom not valid start-where for multi-selection"))
		 ((:check-leaf-but-return-element
		   :check-leaf-but-return-element-or-none
		   :element-of
		   :element-of-or-none)
		  (opal:components-in-rectangle agg top left bottom right
						:type type
						:INTERSECT NIL))
		 ((:list-element-of
		   :list-element-of-or-none
		   :list-check-leaf-but-return-element-or-none
		   :list-check-leaf-but-return-element)
		  (list-element-of-branch agg slot type win left top
					  right bottom))
		 ((:list-leaf-element-of
		   :list-leaf-element-of-or-none
		   )
                  (list-leaf-element-of-branch agg slot type win left top
					       right bottom))
		 ((:leaf-element-of-or-none
		   :leaf-element-of)
		  (opal:leaf-objects-in-rectangle agg top left bottom right
						  :type type :INTERSECT NIL))
		 ((:in-box
		   :in
		   :in-but-not-on)
		  (when (opal:obj-in-rectangle agg top left bottom right
					       :type type :INTERSECT NIL)
		    (list agg)))

		 (t (error "** illegal where control: ~s" where)))))
	  (t (error "** illegal where control: ~s" where))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+garnet-test
(eval-when (:execute :load-toplevel :compile-toplevel)
 (export '(Multi-Graphics-Selection-Go
	   Multi-Graphics-Selection-Obj
	   Multi-Graphics-Selection-Stop)))
#+garnet-test (defparameter Multi-Graphics-Selection-Wins NIL)
#+garnet-test
(defun Multi-Graphics-Selection-Stop ()
  (dolist (w Multi-Graphics-Selection-Wins)
    (opal:destroy w)))

#+garnet-test
(defun Multi-Graphics-Selection-Go ()
  (let ((firsttime T)
	agg l l2)
  (setq Multi-Graphics-Selection-Wins
	(list
	 (create-instance NIL inter:interactor-window
	  (:title "1 Multi Graphics Selection Test")
	  (:aggregate
	   (create-instance 'Multi-Graphics-Selection-Agg1 opal:aggregate)))
	 (create-instance NIL inter:interactor-window
	  (:title "2 Multi Graphics Selection Test")
	  (:left 400)
	  (:aggregate
	   (create-instance 'Multi-Graphics-Selection-Agg2 opal:aggregate)))))
  (dolist (w Multi-Graphics-Selection-Wins)
    (setq l (list
	   (create-instance NIL opal:rectangle (:box (list 40 50 60 70))
			    (:grow-p T)
			    (:line-style opal:line-2)
			    (:left (o-formula (first (gvl :box))))
			    (:filling-style (if firsttime opal:green-fill
						opal:red-fill))
			    (:top (o-formula (second (gvl :box))))
			    (:width (o-formula (third (gvl :box))))
			    (:height (o-formula (fourth (gvl :box)))))
	   (create-instance NIL opal:circle (:box (list 250 20 30 30))
			    (:grow-p T)
			    (:line-style opal:line-2)
			    (:filling-style (if firsttime opal:green-fill
						opal:red-fill))
			    (:left (o-formula (first (gvl :box))))
			    (:top (o-formula (second (gvl :box))))
			    (:width (o-formula (third (gvl :box))))
			    (:height (o-formula (fourth (gvl :box)))))
	   (create-instance NIL opal:text (:box (list 200 150))
			    (:line-style (if firsttime opal:green-line
						opal:red-line))
			    (:grow-p NIL) (:string "Move me")
			    (:left (o-formula (first (gvl :box))))
			    (:top (o-formula (second (gvl :box)))))
	   (create-instance NIL opal:line (:points (list 10 50 300 200))
			    (:grow-p T)
			    (:line-p T)
			    (:line-style (if firsttime opal:green-line
					     opal:red-line))
			    (:X1 (o-formula (first (gvl :points))))
			    (:y1 (o-formula (second (gvl :points))))
			    (:x2 (o-formula (third (gvl :points))))
			    (:y2 (o-formula (fourth (gvl :points)))))
	   (create-instance NIL opal:polyline
	     (:grow-p T)
	     (:POINT-LIST (list 70 90 70 240 120 170 160 230 160 90 70 90 ))
	     (:filling-style opal:gray-fill)
	     (:polygon-p T)
	     (:line-style (if firsttime opal:green-line opal:red-line)))
	   (setq agg (create-instance NIL opal:aggregate
		       (:grow-p T)
		       (:group-p T)))))
  (setq l2 (list
	   (create-instance NIL opal:rectangle (:box (list 120 120 30 30))
			    (:grow-p T)
			    (:filling-style (if firsttime opal:green-fill
						opal:red-fill))
			    (:left (o-formula (first (gvl :box))))
			    (:top (o-formula (second (gvl :box))))
			    (:width (o-formula (third (gvl :box))))
			    (:height (o-formula (fourth (gvl :box)))))
	   (create-instance NIL opal:text (:box (list 100 150))
			    (:line-style (if firsttime opal:green-line
					     opal:red-line))
			    (:grow-p NIL) (:string "In Group")
			    (:left (o-formula (first (gvl :box))))
			    (:top (o-formula (second (gvl :box)))))
	   (create-instance NIL opal:circle (:box (list 100 100 10 10))
			    (:grow-p T)
			    (:filling-style (if firsttime opal:green-fill
						opal:red-fill))
			    (:left (o-formula (first (gvl :box))))
			    (:top (o-formula (second (gvl :box))))
			    (:width (o-formula (third (gvl :box))))
			    (:height (o-formula (fourth (gvl :box)))))
	   (create-instance NIL opal:line (:points (list 100 100 160 150))
			    (:grow-p T)
			    (:line-p T)
			    (:line-style (if firsttime opal:green-line
					     opal:red-line))
			    (:X1 (o-formula (first (gvl :points))))
			    (:y1 (o-formula (second (gvl :points))))
			    (:x2 (o-formula (third (gvl :points))))
			    (:y2 (o-formula (fourth (gvl :points)))))
	   (create-instance NIL opal:polyline
	     (:grow-p T)
	     (:POINT-LIST (list 100 100 100 170 120 120 150 170 190 140
			    140 120 200 100 140 110 100 100))
	     (:line-style (if firsttime opal:green-line opal:red-line))
	     (:polygon-p T)
	     (:line-style opal:red-line))))
  (dolist (o l2)
    (opal:add-component agg o))
  (dolist (o l)
    (opal:add-component (g-value w :aggregate) o))
  (s-value (g-value w :aggregate) :list-of-objs (append l2 l))
  (setq firsttime NIL))
  
  (create-instance 'Multi-Graphics-Selection-Obj1 Multi-Graphics-Selection
    (:check-line T)
    (:check-group T)
    (:check-polygon T)
    (:check-grow-p T)
    (:want-undo T)
    (:allow-component-select T)
    (:start-where `(:list-element-of-or-none
		    ,Multi-Graphics-Selection-Agg1 :list-of-objs))
    (:selection-function #'(lambda (gadget new-sel)
			     (declare (ignore gadget))
			     (format T "1 new selection is ~s~%" new-sel)))
    (:modify-function #'(lambda (gadget obj new-val)
			  (declare (ignore gadget))
			  (format T "* 1 size for ~s now ~s~%" obj new-val))))
  (opal:add-components Multi-Graphics-Selection-Agg1
		       Multi-Graphics-Selection-Obj1)
  (create-instance 'Multi-Graphics-Selection-Obj2 Multi-Graphics-Selection
    (:check-line T)
    (:check-group T)
    (:allow-component-select T)
    (:check-polygon T)
    (:check-grow-p T)
    (:want-undo T)
    (:start-where `(:list-element-of-or-none
		    ,Multi-Graphics-Selection-Agg2 :list-of-objs))
    (:selection-function #'(lambda (gadget new-sel)
			     (declare (ignore gadget))
			     (format T "2 new selection is ~s~%" new-sel)))
    (:modify-function #'(lambda (gadget obj new-val)
			  (declare (ignore gadget))
			  (format T "* 2 size for ~s now ~s~%" obj new-val)))
    )

  (s-value Multi-Graphics-Selection-Obj1 :other-multi-graphics-selection
	   (list Multi-Graphics-Selection-Obj1
		 Multi-Graphics-Selection-Obj2))
  (s-value Multi-Graphics-Selection-Obj2 :other-multi-graphics-selection
	   (list Multi-Graphics-Selection-Obj1
		 Multi-Graphics-Selection-Obj2))
  (opal:add-components Multi-Graphics-Selection-Agg2
		       Multi-Graphics-Selection-Obj2)
  (create-instance 'Multi-Graphics-Selection-Undo-Inter inter:button-interactor
    (:start-where T)
    (:window Multi-Graphics-Selection-Wins)
    (:start-event '(#\u :control-\u))
    (:continuous NIL)
    (:final-function #'(lambda (inter val)
			 (declare (ignore val))
			 (Undo-Last-Move-Grow
			  (g-value inter :current-window
				   :Multi-Graphics-Selection-Obj)))))
  

  (dolist (w Multi-Graphics-Selection-Wins)
    (opal:update w))
  (s-value (g-value Multi-Graphics-Selection-Obj1 :window)
	   :Multi-Graphics-Selection-Obj Multi-Graphics-Selection-Obj1)
  (s-value (g-value Multi-Graphics-Selection-Obj2 :window)
	   :Multi-Graphics-Selection-Obj Multi-Graphics-Selection-Obj2)
  (format T "Click with mouse to select.  Point to square to move/grow.
Hit 'u' to undo last move or grow. control-leftdown to select components.")
  ))

#|
BUGS:
-----

If s-value the width of one of the objects when multiple selected,
doesn't change multi-grow boxes position

Across multiple windows, selection order is not preserved

Allow dragging objects among multiple windows.

Support Macintosh-like move-grow
|#
