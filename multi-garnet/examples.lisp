;;;-*- Package: :user; Syntax: Common-Lisp; Mode: Lisp -*-;

;; =================================================================================

;; This file contains a set of Multi-Garnet examples, separated by lines of
;; equal-signs (like above).  To try them out, load this file (which will
;; load a few auxiliary functions), and then select and execute the
;; examples below.

(in-package :multi-garnet)

;; If the Garnet background process for updating windows is not
;; reliable, the following advice can be used to update the window
;; every time the constraint graph changes, so you don't need to
;; manually update the window all of the time when running the example
;; code.
;;
;; (excl:advise mg::mg-add-constraint :after nil nil (update))
;; (excl:advise mg::mg-remove-constraint :after nil nil (update))
;;
;; to reverse these changes, do
;;
;; (excl:unadvise mg::mg-add-constraint)
;; (excl:unadvise mg::mg-remove-constraint)
;;
;; to see message when cycle detected, do:
;;
;; (setq mg::*sky-blue-cycle-warning* t)
;;
;; to see message on backtracking, do:
;;
;; (setq mg::*sky-blue-backtracking-warning* t)

(defvar *demo-window* nil)

(defun run ()
  (update)
  (cond ((or (opal:main-event-loop-process-running-p)
	     (find :cmu cl:*features*))
	 (cerror "cont" "try interacting with the demo window, then continue from this error"))
	(t
	 (format t "~&try interacting with the demo window, than press F1 to continue~%")
	 (inter:main-event-loop)))
  )

(defun update ()
  (if (is-a-p *demo-window* inter:interactor-window)
      (opal:update *demo-window* t)))

(defun clear-demo-window (&key (width 600)
			       (height 200)
			       (top 100)
			       (left 500)
			       (double-buffered-p nil))
  (let* ((win (if (is-a-p *demo-window* inter:interactor-window)
		  *demo-window*
		(create-instance nil inter:interactor-window
				 (:double-buffered-p double-buffered-p)
				 (:title "Multi-Garnet Demo Window")
				 (:width width)
				 (:height height)
				 (:top top)
				 (:left left)
				 (:position-by-hand nil)
				 )))
         (agg (create-instance nil opal:aggregate
                               (:window win)
                               (:left 0)
                               (:top 0)
                               (:width (o-formula (gvl :window :width)))
                               (:height (o-formula (gvl :window :height)))))
         )
    (s-value win :aggregate agg)
    (opal:update win t)
    (inter::destroy-all-interactors win)
    (setq *demo-window* win)))

(defun add-to-demo-window (&rest objs)
  (apply #'opal:add-components (g-value *demo-window* :aggregate) objs))

(defun remove-from-demo-window (&rest objs)
  (apply #'opal:remove-components (g-value *demo-window* :aggregate) objs))

;;;#||
;; all examples are commented out, so only the auxiliary fns are loaded
;; when this file is loaded

;;
;; =================================================================================
;; Simple example of equality and stay constraints.

(create-instance 'foo nil
		 (:a 5)
		 (:b 6)
		 (:c (m-constraint :strong (a b) (setf a b) (setf b a)))
		 )
;; check values of slots: constraint has set them equal
(list :a (g-value foo :a) :b (g-value foo :b))

;; changing each slot propagates to the other
(s-value foo :a "new a value")
(list :a (g-value foo :a) :b (g-value foo :b))
(s-value foo :b "new b value")
(list :a (g-value foo :a) :b (g-value foo :b))

;; adding a max stay to :a keeps :a from changing, and
;; :a's old value propagates back to :b after it is set.
(s-value foo :a-stay-cn (m-stay-constraint :max a))
(s-value foo :b 23)
(list :a (g-value foo :a) :b (g-value foo :b))

;; adding another max cn to set :a causes a max-max conflict
(s-value foo :a-cn2 (m-constraint :max (a) (setf a "new-value")))
(list :a (g-value foo :a) :b (g-value foo :b))

;; after removing the old constraint, the new one is added
(s-value foo :a-stay-cn nil)
(list :a (g-value foo :a) :b (g-value foo :b))

;;
;; =================================================================================
;; Example from Multi-Garnet overview showing how multi-way constraints
;; address several problems with one-formulas

(clear-demo-window)
(create-instance 'rect-proto opal:rectangle
   (:left 0)
   (:top 0)
   (:width 10)
   (:height 10)
   (:right 10)
   (:left-right-width-cn
      (m-constraint :max
            (left right width)
         (setf left (- right width))
         (setf right (+ left width))
         (setf width (- right left))))
   (:width-stay
      (m-stay-constraint :medium width))
   )

(create-instance 'rect1 rect-proto
   (:left 50)
   (:top 10)
   (:right 60)
   )

(create-instance 'rect2 rect-proto
   (:left 50)
   (:top 30)
   (:right 60)
   (:main rect1)
   (:left-cn
      (m-constraint :max
            ((main-left (gvl :main :left))
             left)
         (setf left main-left)
         (setf main-left left)))
   )
(add-to-demo-window rect1 rect2)
(update)

;; Setting either rect1 or rect2's :left slot will propagate to the other
;; to keep the two left-aligned.
(s-value rect1 :left 30)
(s-value rect2 :left 100)
;; changing rect1's :right slot propagates to the :left slot, because
;; of the stay on the :width slot.
(s-value rect1 :right 300)
;; if the :width slot is set, either the :left or :right slot can be changed
;; to maintain the constraint.
(s-value rect1 :width 40)

;; any two of the :left, :right, and :width slots can be set, and the
;; third one will be calculated.
(with-slots-set ((rect1 :left 10)(rect1 :right 300)))
(with-slots-set ((rect1 :left 100)(rect1 :width 45)))
(with-slots-set ((rect1 :width 100)(rect1 :right 300)))

;; a third rectangle can be created left-aligned with rect1 and hence rect2
;; (inheriting :main and :left-cn from rect2)
(create-instance 'rect3 rect2
   (:left 50)
   (:top 50)
   (:right 60)
   )
(add-to-demo-window rect3)
(update)
(s-value rect3 :left 300)

;; a forth rectangle can be left-aligned with rect3 (by inheriting
;; :left-cn, and setting :main), demonstrating that all of the left-aligned
;; rectangle don't have to be linked to rect1.
(create-instance 'rect4 rect2
		 (:left 50)
		 (:top 70)
		 (:main rect3))
(add-to-demo-window rect4)
(update)
(s-value rect4 :left 50)

;;
;; =================================================================================
;; example rectangle with useful constraints between left, right, width,
;; and center-x

(clear-demo-window)
(create-instance 'rect opal:rectangle
		 (:top 10)
		 (:height 30)
		 (:left 50)
		 (:width 100)
		 (:right 150)
		 (:left-right-width-cn (m-constraint :max (left right width)
						     (setf width (- right left))
						     (setf left (- right width))
						     (setf right (+ left width))))
		 (:center-x-cn (m-constraint :max (center-x left width)
					     (setf center-x (+ left (truncate width 2)))
					     (setf left (- center-x (truncate width 2)))
					     (setf width (* 2 (- center-x left)))))
		 ;; want stronger stay on width than on other slots
		 (:width-stay-cn (m-stay-constraint :medium width)))
(add-to-demo-window rect)
(update)

;; try changing single slots
(s-value rect :left 100)
(s-value rect :right 100)
(s-value rect :center-x 100)
(s-value rect :width 50)
(s-value rect :center-x 200)
(s-value rect :left 100)
(s-value rect :right 200)

;; try changing some slots, while keeping other slots constant
(with-stays ((rect :right)) (s-value rect :left 20))
(with-stays ((rect :left)) (s-value rect :right 100))
(with-stays ((rect :center-x)) (s-value rect :left 40))
(with-stays ((rect :left)) (s-value rect :center-x 100))

;; Since there is a potential cycle between the two constraints, there are
;; cases where the constraint system cannot succeed.  The following cause
;; cycles.
(with-stays ((rect :right)) (s-value rect :center-x 150))
(with-stays ((rect :center-x)) (s-value rect :right 200))

;; replacing the two :left-right-width-cn and :center-x-cn constraints
;; with a single constraint (with multi-output methods) removes the
;; possible cycle.
(s-value rect :left-right-width-cn nil)
(s-value rect :center-x-cn nil)
(s-value rect :left-right-width-center-x-cn
	(m-constraint :max (left right width center-x)
		      (setf (left right) (values (- center-x (truncate width 2))
						 (+ center-x (truncate width 2))))
		      (setf (left width) (values (- center-x (- right center-x))
						 (* 2 (- right center-x))))
		      (setf (left center-x) (values (- right width)
						    (- right (truncate width 2))))
		      (setf (right width) (values (+ left (* 2 (- center-x left)))
						  (* 2 (- center-x left))))
		      (setf (right center-x) (values (+ left width)
						     (+ left (truncate width 2))))
		      (setf (width center-x) (values (- right left)
						     (truncate (+ left right) 2)))
		      ))

;; now these don't cause cycles
(with-stays ((rect :right)) (s-value rect :center-x 150))
(with-stays ((rect :center-x)) (s-value rect :right 200))

;;
;; =================================================================================
;; Example of fancy alignments between an "aggregate" rectangle and some
;; "component" rectangles

(clear-demo-window)
(create-instance 'rect-proto opal:rectangle
		 (:top 10) (:height 100) (:left 100) (:width 100) (:right 200)
		 (:width-stay-cn (m-stay-constraint :medium width))
		 (:left-right-width-cn (m-constraint :max (left right width)
						     (setf width (- right left))
						     (setf left (- right width))
						     (setf right (+ left width))))
		 )
(create-instance 'agg-rect rect-proto
		 (:top 5) (:height 65) (:left 100) (:width 100) (:right 200)
		 )
(create-instance 'left-rect rect-proto
		 (:top 10) (:height 15) (:left 250) (:width 50) (:right 300)
		 (:filling-style opal:gray-fill)
		 )
(create-instance 'right-rect rect-proto
		 (:top 30) (:height 15) (:left 250) (:width 60) (:right 310)
		 (:filling-style opal:light-gray-fill)
		 )
(create-instance 'center-rect rect-proto
		 (:top 50) (:height 15) (:left 250) (:width 70) (:right 320)
		 (:filling-style opal:dark-gray-fill)
		 )
(add-to-demo-window agg-rect left-rect right-rect center-rect)
(update)

;; left-align left-rect with agg-rect
(s-value left-rect :agg-rect agg-rect)
(s-value left-rect :calign (m-constraint :max ((oleft (gvl :agg-rect :left))
					       left)
					 (setf left oleft)
					 (setf oleft left)))

;; right-align right-rect with agg-rect
(s-value right-rect :agg-rect agg-rect)
(s-value right-rect :calign (m-constraint :max ((oright (gvl :agg-rect :right))
						right)
					  (setf right oright)
					  (setf oright right)))

;; center center-rect with agg-rect
(s-value center-rect :agg-rect agg-rect)
(s-value center-rect :calign (m-constraint :max ((owidth (gvl :agg-rect :width))
						 (oleft (gvl :agg-rect :left))
						 left width)
					   (setf left (+ oleft
							 (truncate owidth 2)
							 (- (truncate width 2))))
					   (setf oleft (+ left
							  (- (truncate owidth 2))
							  (truncate width 2)))
					   ))

;; set agg-rect's width to max of other rects' widths
(s-value agg-rect :left-rect left-rect)
(s-value agg-rect :right-rect right-rect)
(s-value agg-rect :center-rect center-rect)
(s-value agg-rect :cwidth (m-constraint :max ((wa (gvl :left-rect :width))
					      (wb (gvl :right-rect :width))
					      (wc (gvl :center-rect :width))
					      width)
					(setf width (max wa wb wc))
					(setf (wa wb wc)
					  (values
					   ;; if agg-rect's width is set greater than the max of
					   ;; its components, arbitrarily enlarge left-rect.
					   (if (> width (max wa wb wc))
					       width
					     (min width wa))
					   ;; other components will shrink if necessary to fit
					   ;; in new agg-rect width
					   (min width wb)
					   (min width wc)))
					))

;; change widths
(s-value left-rect :width 100)
(s-value right-rect :width 120)
(s-value center-rect :width 140)
(s-value agg-rect :width 160)

;; set left and right of each rectangle
(s-value agg-rect :left 200)
(s-value agg-rect :right 200)
(s-value left-rect :left 200)
(s-value left-rect :right 200)
(s-value right-rect :left 200)
(s-value right-rect :right 200)
(s-value center-rect :left 200)
(s-value center-rect :right 200)

(with-stays ((right-rect :left)) (s-value right-rect :right 250))
(with-stays ((agg-rect :right))
	    (s-value right-rect :width 150))

;; causes cycles
(with-stays ((left-rect :left)(right-rect :left)) (s-value agg-rect :right 300))
(with-slots-set ((left-rect :left 100)
		 (left-rect :right 120)
		 (right-rect :left 240)
		 (right-rect :right 260)))

;;
;; =================================================================================
;; Example that creates three draggable boxes, and tries adding different
;; constraints between them.

(clear-demo-window)
(create-instance '*draggable-box* opal:rectangle
		 (:box (list 0 0 10 10))
		 (:left 0) (:top 0) (:width 10) (:height 10)
		 (:box-cn (m-constraint :max (left top width height box)
					(setf box (list left top width height))
					(setf (left top width height)
					  (values (first box)
						  (second box)
						  (third box)
						  (fourth box)))))
		 )
(create-instance 'rect1 *draggable-box*
		 (:box (list 10 10 10 10))
		 (:left 10) (:top 10) (:width 10) (:height 10)
		 (:filling-style opal:gray-fill)
		 )
(create-instance 'rect2 *draggable-box*
		 (:box (list 30 30 10 10))
		 (:left 30) (:top 30) (:width 10) (:height 10)
		 (:filling-style opal:light-gray-fill)
		 )
(create-instance 'rect3 *draggable-box*
		 (:box (list 50 50 10 10))
		 (:left 50) (:top 50) (:width 10) (:height 10)
		 (:filling-style opal:dark-gray-fill)
		 )
(add-to-demo-window rect1 rect2 rect3)
(update)

;; create an interactor to allow dragging these boxes around.
;; drag them around independently
(create-instance nil inter:move-grow-interactor
		 (:window *demo-window*)
		 (:start-where (list :leaf-element-of
				     (g-value *demo-window* :aggregate)
				     :type *draggable-box*))
		 )
(run)

;; define useful box-manipulation functions
(defun box-plus (a b) (mapcar #'+ a b))
(defun box-minus (a b) (mapcar #'- a b))
(defun box-truncate (a val) (mapcar #'(lambda (x) (truncate x val)) a))

;; create constraints that maintain "delta" variables, specifying the list
;; difference between the :box slots of rect1 and rect2, and rect2 and
;; rect3.
(create-instance 'delta-obj nil
		 (:rect1 rect1)
		 (:rect2 rect2)
		 (:rect3 rect3)
		 (:delta12 (list 20 20 0 0))
		 (:delta23 (list 20 20 0 0))
		 (:delta12-cn (m-constraint :max ((box1 (gvl :rect1 :box))
						  (box2 (gvl :rect2 :box))
						  (delta (gvl :delta12)))
					    (setf box2 (box-plus box1 delta))
					    (setf box1 (box-minus box2 delta))
					    (setf delta (box-minus box2 box1))))
		 (:delta23-cn (m-constraint :max ((box1 (gvl :rect2 :box))
						  (box2 (gvl :rect3 :box))
						  (delta (gvl :delta23)))
					    (setf box2 (box-plus box1 delta))
					    (setf box1 (box-minus box2 delta))
					    (setf delta (box-minus box2 box1))))
		 )

;; stays can be used to hold relative distances constant during
;; interactions

;; dragging any of the boxes drags the others at the same relative distance
(with-stays ((delta-obj :delta12) (delta-obj :delta23)) (run))


;; add weak stays to :delta12 and :delta23, and a stronger stay to rect1 :box
;; now, if rect1 is dragged the other rects will follow, but dragging
;; rect2 or rect3 will not drag rect1
(with-stays ((delta-obj :delta12 :weak) (delta-obj :delta23 :weak) (rect1 :box :medium)) (run))

;; add a constraint to make two the deltas the same
(s-value delta-obj :delta-eq-cn (m-constraint :max (delta12 delta23)
					      (setf delta12 delta23)
					      (setf delta23 delta12)))

;; if a stay is added to rect2 :box, can move rect1 or rect3 around rect2
(with-stays ((rect2 :box)) (run))

;; trying to "pin" rect1 or rect3 may cause cycle
(with-stays ((rect1 :box)) (run))

;; remove the three constraints :delta12-cn, :delta23-cn, and :delta-eq-cn,
;; which cause the cycle, and replace them with a constraint that specifies
;; that rect2 should be midway between rect1 and rect2
(s-value delta-obj :delta12-cn nil)
(s-value delta-obj :delta23-cn nil)
(s-value delta-obj :midway-cn (m-constraint :max ((box1 (gvl :rect1 :box))
						  (box2 (gvl :rect2 :box))
						  (box3 (gvl :rect3 :box)))
					 (setf box3 (box-plus box2 (box-minus box2 box1)))
					 (setf box1 (box-minus box2 (box-minus box3 box2)))
					 (setf box2 (box-plus box1 (box-truncate (box-minus box3 box1) 2)))
					 ))

;; Now, the user can drag *any* of the three rectangles around, and the
;; other two rectangles will move to maintain the constraint.
(run)

;; the :midway-cn constraint doesn't specify exactly which rectangles
;; should be changed to satisfy the constraint, as a rectangle is moved.
;; By adding stays, the behavior can be modified in useful ways.  The
;; "with-stays" macro allows stays to be added temporarily, during a
;; particular interaction.  Of course, stays could also be added
;; perminantly.

;; Given a choice between moving rect1 and another rect, will move the
;; other rect.
(with-stays ((rect1 :box :medium)) (run))

;; Same as above, but also will not allow rect1 to be moved by the
;; interactor (by default, the interactor changes the :box slot with a
;; strength of :strong).
(with-stays ((rect1 :box :strong)) (run))

;; If rect1 is dragged, then rect2 will be moved to maintain the
;; constraint.  If rect2 or rect3 is dragged, rect1 will be moved.
(with-stays ((rect3 :box :medium) (rect2 :box :weak)) (run))

;;
;; =================================================================================
;; In this example, an interactor sets a slot on an indirect reference
;; path, causing constraints to be added and removed during an interaction

(clear-demo-window)
(create-instance '*draggable-box* opal:rectangle
		 (:box (list 0 0 10 10))
		 (:left 0) (:top 0) (:width 10) (:height 10)
		 (:box-cn (m-constraint :max (left top width height box)
					(setf box (list left top width height))
					(setf (left top width height)
					  (values (first box)
						  (second box)
						  (third box)
						  (fourth box)))))
		 )
(create-instance nil inter:move-grow-interactor
		 (:window *demo-window*)
		 (:start-where (list :leaf-element-of
				     (g-value *demo-window* :aggregate)
				     :type *draggable-box*))
		 )
;; create 10 randomly-placed draggable boxes
(dotimes (i 10)
  (let ((left (+ 30 (random 100)))
	(top (+ 30 (random 100))))
    (add-to-demo-window
     (create-instance nil *draggable-box*
		      (:box (list left top 10 10))
		      (:left left) (:top top) (:width 10) (:height 10)))))
(update)

;; the following string will display the box slot of whatever object is in
;; its :obj-over slot
(create-instance '*box-string* opal:text
		 (:left 10) (:top 10)
		 (:string "nothing")
		 (:obj-over nil)
		 (:text-c (m-constraint :max ((box (gvl :obj-over :box))
					      string)
					(setf string (princ-to-string box))))
		 )
(add-to-demo-window *box-string*)
(update)

;; the following interactor will use *box-string* as the feedback object as
;; the mouse is dragged over the *draggable-box*'s.
(create-instance nil inter:menu-interactor
		 (:start-event :rightdown)
		 (:start-where (list :element-of-or-none
				     (g-value *demo-window* :aggregate)
				     :type *draggable-box*))
		 (:feedback-obj *box-string*)
		 (:window *demo-window*))

;; Drag boxes by clicking and dragging the left button.  Hold down the
;; right button and move the mouse to display the :box slot of whichever
;; *draggable-box* the mouse is over.
(run)

;; =================================================================================
;; Example using m-constraints in the parts of an aggregadget
;; (adapted from demo-manyobjs.lisp)

(clear-demo-window)

(create-instance
 'BoxAndArrow opal:aggregadget
 (:prev-item NIL)
 (:name "Obj1")
 (:parts `((:outline ,Opal:Rectangle
		     (:line-style ,Opal:thin-line)
		     (:filling-style ,Opal:no-fill)
		     (:box ,(list 100 100 50 50))
		     (:left 100) (:top 100) (:width 50) (:height 50)
		     (:box-cn ,(m-constraint :max (left top width height box)
					     (setf box (list left top width height))
					     (setf (left top width height)
					       (values (first box)
						       (second box)
						       (third box)
						       (fourth box)))))
		     )
	   (:arrow ,opal:line
		   (:visible nil)
		   (:visible-c ,(m-constraint :max (visible (prev (gvl :parent :prev-item)))
					      (setf visible (if prev t nil))))
		   (:x1 100)
		   (:x1-c ,(m-constraint :max ((oleft (gvl :parent :outline :left))
					       (owidth (gvl :parent :outline :width))
					       x1)
					 (setf x1 (+ oleft (truncate owidth 2)))))
		   (:y1 100)
		   (:y1-c ,(m-constraint :max ((otop (gvl :parent :outline :top))
					       (oheight (gvl :parent :outline :height))
					       y1)
					 (setf y1 (+ otop (truncate oheight 2)))))
		   (:x2 200)
		   (:x2-c ,(m-constraint :max ((oleft (gvl :parent :prev-item :outline :left))
					       (owidth (gvl :parent :prev-item :outline :width))
					       x2)
					 (setf x2 (+ oleft (truncate owidth 2)))))
		   (:y2 200)
		   (:y2-c ,(m-constraint :max ((otop (gvl :parent :prev-item :outline :top))
					       (oheight (gvl :parent :prev-item :outline :height))
					       y2)
					 (setf y2 (+ otop (truncate oheight 2)))))
		   )
	   (:label ,opal:text
		   (:string "none")
		   (:string-c ,(m-constraint :max (string (pstr (gvl :parent :name)))
					     (setf string pstr)))
		   (:left 0)
		   (:left-c ,(m-constraint :max ((oleft (gvl :parent :outline :left))
						 (owidth (gvl :parent :outline :width))
						 (width (gvl :width))
						 left)
					   (setf left (+ oleft (floor (- owidth width) 2)))))
		   (:top 0)
		   (:top-c ,(m-constraint :max ((otop (gvl :parent :outline :top))
						(oheight (gvl :parent :outline :height))
						(height (gvl :height))
						top)
					  (setf top (+ otop (floor (- oheight height) 2)))))
		   )
	   )))

;; create and add a bunch of rectangles to the window at random places
(setq rect-list (loop for cnt from 1 to 10 collect
		      (create-instance NIL BoxAndArrow
				       (:prev-item nil)
				       (:name (concatenate 'string "Obj"
							   (prin1-to-string cnt))))))
;; position rects at random places
(loop for obj in rect-list do
      (s-value (g-value obj :outline) :box
	       (list (random (- (g-value *demo-window* :width) 50))
		     (random (- (g-value *demo-window* :height) 50))
		     50 50))
      (add-to-demo-window obj))
;; add links between objects
(loop for (prev-obj obj) on rect-list do
      (when obj (s-value obj :prev-item prev-obj)))
(update)

;; allow dragging boxes with left button
(create-instance
 'inter1 inter:move-grow-interactor
 (:start-where `(:leaf-element-of
		 ,(g-value *demo-window* :aggregate)
		 :type ,Opal:Rectangle))
 (:window *demo-window*))

;; as the boxes are dragged, the lines will be updated to link them
;; together.
(run)

;;
;; =================================================================================
;; Example using m-constraints in the elements of an aggrelist.  This tests
;; constraining a slot defined via formula (:rank).

(clear-demo-window)
(create-instance
 '*stack-of-blocks* opal:aggrelist
 (:item-prototype
  `(,opal:rectangle
    (:height 10)
    (:width 10)
    (:width-c ,(m-constraint :max ((items (gvl :parent :items))
				   rank width)
			     (setf width (nth rank items))))
    ))
 )

(create-instance 'stack1 *stack-of-blocks*
		 (:left 100)
		 (:top 10)
		 (:items (loop for x from 10 by 10 to 100 collect x)))
(add-to-demo-window stack1)
(update)

;;
;; =================================================================================
;; Testing infinite loop with updating indirect reference paths

(create-instance 'bar nil)
(create-instance 'baz nil)
(s-value bar :b baz)
(s-value baz :b bar)
(create-instance 'foo nil
		 (:a bar))
;; adding following constraint causes multi-level cycle
(s-value foo :c (m-constraint :strong ((yy (gvl :a))
				       (xx (gvl :a :b)))
			      (setf yy xx)))

;; another infinite loop
(create-instance 'w nil)
(create-instance 'x nil)
(create-instance 'y nil)
(create-instance 'z nil)
(loop for obj in (list w x y z) do
      (s-value obj :w w)
      (s-value obj :x x)
      (s-value obj :y y)
      (s-value obj :z z)
      )
(s-value w :w-weak-set-cn (m-constraint :weak (val z) (setf val z)))
(s-value x :x-weak-set-cn (m-constraint :weak (val w) (setf val w)))
(s-value x :x-req-set-cn (m-constraint :max ((val (gvl :val :val)) x) (setf val x)))
(s-value w :w-req-set-cn (m-constraint :max ((val (gvl :val :val)) y) (setf val y)))

;; another infinite loop with no directed loop between the involved cns
(create-instance 'w nil)
(create-instance 'x nil)
(create-instance 'y nil)
(create-instance 'z nil)
(loop for obj in (list w x y z) do
      (s-value obj :w w)
      (s-value obj :x x)
      (s-value obj :y y)
      (s-value obj :z z)
      )
(s-value w :w-med-double-set-cn (m-constraint :medium (val lock z) (setf (lock val) (values z z))))
(s-value w :w-weak-set-cn (m-constraint :weak (val x) (setf val x)))
(s-value x :x-med-double-set-cn (m-constraint :medium (val lock w) (setf (lock val) (values w w))))
(s-value x :x-weak-set-cn (m-constraint :weak (val y) (setf val y)))
(s-value x :x-req-set-cn (m-constraint :max ((val (gvl :val :lock)) x) (setf val x)))
(s-value w :w-req-set-cn (m-constraint :max ((val (gvl :val :lock)) y) (setf val y)))

;;
;; =================================================================================
;; Two rectangles, joined together so the right-middle of the oval meets
;; the left middle of the rectangle: each can be reshaped.

(clear-demo-window)
(create-instance 'rect-proto opal:rectangle
		 (:box (list 150 50 100 100))
		 (:left 150) (:width 100) (:center-x 200) (:right 250)
		 (:top 50) (:height 100) (:center-y 100) (:bottom 150)
		 (:box-cn (m-constraint :max (box left top width height)
					(setf box (list left top width height))
					(setf (left top width height)
					  (values-list box))))
		 (:lrw-cn (m-constraint :max (left right width)
					(setf right (+ left width))
					(setf left (- right width))
					(setf width (- right left))))
                 (:lrw-cn (m-constraint :max (left right width)
					(setf right (+ left width))
					(setf left (- right width))
					(setf width (- right left))))
                 (:center-x-cn (m-constraint :max (left width center-x)
					     (setf center-x (+ left (truncate width 2)))
					     (setf width (* 2 (- center-x left)))
					     (setf left (- center-x (truncate width 2)))))
		 (:center-y-cn (m-constraint :max (top height center-y)
					     (setf center-y (+ top (truncate height 2)))
					     (setf height (* 2 (- center-y top)))
					     (setf top (- center-y (truncate height 2)))))
		 (:width-stay (m-stay-constraint :medium width))
		 (:height-stay (m-stay-constraint :medium height))
		 )
(create-instance 'rect1 rect-proto
		 (:filling-style opal:dark-gray-fill)
		 (:box (list 150 50 100 100))
		 (:left 150) (:width 100) (:center-x 200) (:right 250)
		 (:top 50) (:height 100) (:center-y 100) (:bottom 150)
		 )
(create-instance 'rect2 rect-proto
		 (:filling-style opal:light-gray-fill)
		 (:box (list 150 50 100 100))
		 (:left 250) (:width 100) (:center-x 300) (:right 350)
		 (:top 50) (:height 100) (:center-y 100) (:bottom 150)
		 )
(add-to-demo-window rect1 rect2)
(update)

;; reshape rects independently
(create-instance
 '*move-inter* inter:move-grow-interactor
 (:window *demo-window*)
 (:start-where (list :leaf-element-of
		     (g-value *demo-window* :aggregate)))
 (:grow-p t)
 (:line-p nil)
 )
(run)

;; add constraints to link rectangles together
(create-instance 'cn-obj nil
		 (:rect1 rect1)
		 (:rect2 rect2)
		 (:pos-c (m-constraint :max ((right1 (gvl :rect1 :right))
					     (left2 (gvl :rect2 :left)))
				       (setf right1 left2)
				       (setf left2 right1)))
		 (:pos2-c (m-constraint :max ((c1 (gvl :rect1 :center-y))
					      (c2 (gvl :rect2 :center-y)))
					(setf c1 c2)
					(setf c2 c1)))
		 )
(run)

;; can't reshape rect1, since interactor sets :box with strong cn,
;; but reshaping rect will not change rect1's :left.
(with-stays ((rect1 :left)) (run))
(with-stays ((rect1 :left)(rect1 :top)) (run))

;;
;; =================================================================================
;; Example where order that Multi-Garnet updates indirect reference paths
;; causes unusual behavior.

(clear-demo-window)
(create-instance 'rect opal:rectangle
		 (:left 250) (:top 50) (:width 100) (:height 100)
		 )
(create-instance 'circ opal:circle
		 (:left 50) (:top 50) (:width 100) (:height 100)
		 )
(add-to-demo-window rect circ)
(update)

;; this next object is meant to constrain the color of whatever object is
;; in its :obj slot, so it will be red if it is a circle, otherwise green.
(create-instance 'cn-obj nil
		 (:obj nil)
		 (:cn (m-constraint :max (obj (color (gvl :obj :filling-style)))
				    (setf color (if (is-a-p obj opal:circle)
						    opal:red-fill
						  opal:green-fill))))
		 )
(update)

;; fill circle with red
(s-value cn-obj :obj circ)

;; now, try filling rect with green
(s-value cn-obj :obj rect)

;; in this case, both get filled with green, since the constraint sets the
;; circle's color before the constraint is removed and added.

;;
;; =================================================================================
;; Example that attaches the :value slots of a few different gadgets using
;; multi-way constraints.

;;;(load (merge-pathnames "labeled-box-loader" user::Garnet-Gadgets-PathName))
;;;(load (merge-pathnames "h-slider-loader" user::Garnet-Gadgets-PathName))
;;;(load (merge-pathnames "gauge-loader" user::Garnet-Gadgets-PathName))
;;;(load (merge-pathnames "radio-buttons-loader" user::Garnet-Gadgets-PathName))
(garnet-load "gadgets:labeled-box-loader")
(garnet-load "gadgets:h-slider-loader")
(garnet-load "gadgets:gauge-loader")
(garnet-load "gadgets:radio-buttons-loader")
(clear-demo-window)
(create-instance 'lbox garnet-gadgets:labeled-box
		 (:top 50)
		 (:left 10)
		 (:label-string "Type a value:")
		 (:value "0")
		 )
(create-instance 'lbox2 garnet-gadgets:labeled-box
		 (:top 80)
		 (:left 10)
		 (:label-string "Type another value:")
		 (:value "0")
		 )
(create-instance 'hslide garnet-gadgets:h-slider
		 (:top 110) (:left 10))
(create-instance 'gauge garnet-gadgets:gauge
		 (:top 40)
		 (:left 330))
(create-instance 'rbuttons garnet-gadgets:radio-button-panel
		 (:items (loop for x from 0 by 10 to 90 collect (princ-to-string x)))
		 (:left 10) (:top 10)
		 (:direction :horizontal))
(add-to-demo-window lbox lbox2 hslide gauge rbuttons)
(update)
(run)  ;; manipulate independent gadgets

;; now, tie all of the gadgets together

(create-instance 'cn-obj nil
		 (:num 0)   ;; numerical value for gadgets that take one
		 (:str "0") ;; string value for gadgets that take one
		 (:cn0 (m-constraint :max (num str)
				     (setf str (princ-to-string num))
				     (setf num (let ((n (read-from-string str nil 0)))
						 (if (numberp n) n 0)))))
		 (:lbox lbox)
		 (:cn1 (m-constraint :max ((val (gvl :lbox :value))
					   str)
				     (setf val str)
				     (setf str val)))
		 (:lbox2 lbox2)
		 (:cn2 (m-constraint :max ((val (gvl :lbox2 :value))
					   str)
				     (setf val str)
				     (setf str val)))
		 (:hslide hslide)
		 (:cn3 (m-constraint :max ((val (gvl :hslide :value))
					   num)
				     (setf val num)
				     (setf num val)))
		 (:gauge gauge)
		 (:cn4 (m-constraint :max ((val (gvl :gauge :value))
					   num)
				     (setf val num)
				     (setf num val)))
		 (:rbuttons rbuttons)
		 (:cn5 (m-constraint :max ((val (gvl :rbuttons :value))
					   str)
				     (setf val str)
				     (setf str val)))
		 )
(update)

;; now run gadgets, all tied together
(run)

;;
;; =================================================================================
;; Example where an object bounces between its original position and mouse
;; position as an input constraint is added and removed

(clear-demo-window)
(create-instance '*draggable-box* opal:rectangle
		 (:box (list 0 0 10 10))
		 (:left 0) (:top 0) (:width 10) (:height 10)
		 (:box-cn (m-constraint :max (left top width height box)
					(setf box (list left top width height))
					(setf (left top width height)
					  (values (first box)
						  (second box)
						  (third box)
						  (fourth box)))))
		 )
(create-instance nil inter:move-grow-interactor
		 (:window *demo-window*)
		 (:start-where (list :leaf-element-of
				     (g-value *demo-window* :aggregate)
				     :type *draggable-box*))
		 )
(create-instance 'rect1 *draggable-box*
		 (:filling-style opal:gray-fill)
		 (:box (list 10 10 10 10))
		 (:left 0) (:top 10) (:width 10) (:height 10)
		 (:save-box (list 10 10 10 10))
		 (:set-box-cn (m-constraint :medium (box save-box)
				      (setf box (copy-list save-box))))
		 )
(add-to-demo-window rect1)
(update)

;; if the window is updated on every constraint action, using advise like
;; (excl:advise mg::add-constraint :after nil nil (opal:update-all))
;; (excl:advise mg::remove-constraint :after nil nil (opal:update-all))
;; then dragging the box will cause it to "bounce" between its original
;; :save-box position and the mouse position, as the interactor repeatly
;; sets the :box, and this slot is reset to its original value when the
;; s-value's input constraint is removed and set-box-cn is re-enabled.
(run)

;;
;; =================================================================================
;; The old inscribed-quadrilateral example.  A quadrilateral is created by
;; joining lines between four points.  Another quadrilateral is inscribed
;; in the first by joining lines between the midpoints of the first
;; quadrilateral's lines.  A third quadrilateral is inscribed in the
;; second.  Any of the points can be moved, and the relationships are
;; maintained.  Note that the inner quadrilaterals are always
;; parallelograms

(clear-demo-window)
(create-instance '*draggable-box* opal:rectangle
		 (:box (list 0 0 10 10))
		 (:left 0) (:top 0) (:width 10) (:height 10)
		 (:box-cn (m-constraint :max (left top width height box)
					(setf box (list left top width height))
					(setf (left top width height)
					  (values (first box)
						  (second box)
						  (third box)
						  (fourth box)))))
		 )
(create-instance nil inter:move-grow-interactor
		 (:window *demo-window*)
		 (:start-where (list :leaf-element-of (g-value *demo-window* :aggregate)
				     :type *draggable-box*))
		 )
(defun box-plus (a b) (mapcar #'+ a b))
(defun box-minus (a b) (mapcar #'- a b))
(defun box-truncate (a val) (mapcar #'(lambda (x) (truncate x val)) a))
(defun box-center-xy (box) (list (+ (first box) (truncate (third box) 2))
				 (+ (second box) (truncate (fourth box) 2))))
(create-instance '*inscribed-quad-line* opal:line
		 (:points (list 10 10 20 20))
		 (:x1 10) (:y1 10) (:x2 20) (:y2 20)
		 (:end1 nil) (:end2 nil) (:mid nil)
		 (:points-cn (m-constraint :max (x1 y1 x2 y2 points)
					   (setf points (list x1 y1 x2 y2))
					   (setf (x1 y1 x2 y2)
					     (values (first points)
						     (second points)
						     (third points)
						     (fourth points)))))
		 (:points-to-ends-cn (m-constraint :max ((box1 (gvl :end1 :box))
							 (box2 (gvl :end2 :box))
							 points)
						   (setf points (append (box-center-xy box1)
									(box-center-xy box2)))))
		 (:mid-to-ends-cn (m-constraint :max ((boxm (gvl :mid :box))
						      (box1 (gvl :end1 :box))
						      (box2 (gvl :end2 :box)))
						(setf box2 (box-plus boxm (box-minus boxm box1)))
						(setf box1 (box-minus boxm (box-minus box2 boxm)))
						(setf boxm (box-plus box1 (box-truncate (box-minus box2 box1) 2)))
						))
		 )

(defun make-inscribed-quad (&optional (level 3) (super-edges nil))
  (cond ((< level 1)
	 nil)
	(t
	 (let* ((nodes (loop for cnt from 1 to 4
			   as left in '(50 150 150 50)
			   as top in '(50 50 150 150)
			   collect (create-instance nil *draggable-box*
						    (:box (list left top 10 10))
						    (:left left) (:top top) (:width 10) (:height 10)
						    )))
		(edges (loop for (a b) on nodes
			   collect (create-instance nil *inscribed-quad-line*
						    (:end1 a)
						    (:end2 (if b b (car nodes)))
						    )))
		sub-nodes)
	   (when super-edges
	     ;; link these nodes to midpoints of super-edges
	     ;; (which should position the nodes, given that the
	     ;; super-nodes have stays on them).
	     (loop for super-edge in super-edges
		 as node in nodes
		 do (s-value super-edge :mid node)))
	   (with-stays (((first nodes) :box)
			((second nodes) :box)
			((third nodes) :box)
			((fourth nodes) :box))
	     (setq sub-nodes (make-inscribed-quad (1- level) edges)))
	   (loop for node in nodes do (add-to-demo-window node))
	   (loop for edge in edges do (add-to-demo-window edge))
	   (cons nodes sub-nodes))
	 )))

(setq nodes-list (make-inscribed-quad 3))
(update)
(run)

;; note: the speed may be particularly slow because the methods are not
;; compiled.

;; note: some sequences of dragging points will cause cycles, which will
;; leave some of the constraints unsatisfied.

;;
;; =================================================================================
;; old fahrenheit-centigrade temperature converter example, using fancy garnet widgets

(garnet-load "gadgets:v-slider-loader")
(clear-demo-window)
(create-instance 'fahrenheit-text opal:text
		 (:top 10) (:left 50)
		 (:string "Fahrenheit"))
(create-instance 'fahrenheit-slider garnet-gadgets:v-slider
		 (:top 30) (:left 50)
		 (:val-1 -100.0) (:val-2 300.0)
		 (:format-string "~,1F")
		 )
(create-instance 'centigrade-text opal:text
		 (:top 10) (:left 150)
		 (:string "Centigrade"))
(create-instance 'centigrade-slider garnet-gadgets:v-slider
		 (:top 30) (:left 150)
		 (:val-1 -100.0) (:val-2 300.0)
		 (:format-string "~,1F")
		 )
(create-instance 'f-c-connector nil
		 (:f-slider fahrenheit-slider)
		 (:c-slider centigrade-slider)
		 (:f 0)
		 (:c 0)
		 (:get-f-cn (m-constraint :max (f (slider-val (gvl :f-slider :value)))
					  (setf f slider-val) (setf slider-val f)))
		 (:get-c-cn (m-constraint :max (c (slider-val (gvl :c-slider :value)))
					  (setf c slider-val) (setf slider-val c)))
		 (:f-c-cn (m-constraint :max (c f)
					(setf f (+ 32.0 (* 1.8 c)))
					(setf c (/ (- f 32.0) 1.8))))
		 )
(add-to-demo-window fahrenheit-text fahrenheit-slider centigrade-text centigrade-slider)
(update)
(run)


;;
;; =================================================================================
;; x,y <-> rho, theta coordinate conversion

(garnet-load "gadgets:h-slider-loader")
(clear-demo-window)
(create-instance
 'dot opal:circle
 (:x 0)
 (:y 0)
 (:box (list 0 0 12 12))
 (:box-cn
  (m-constraint :max (box x y)
		(setf (x y) (values (float (+ (first box) 6))
				    (float (+ (second box) 6))))
		(setf box (list (round (- x 6))
				(round (- y 6))
				12 12))))
 (:left 0) (:top 0) (:width 12) (:height 12)
 (:unpack-box-cn
  (m-constraint :max (box left top width height)
		(setf (left top width height)
		  (values-list box))
		(setf box (list left top width height))))
 )
(create-instance 'frame opal:rectangle
		 (:left 10) (:top 10) (:width 200) (:height 200)
		 (:center-x 110) (:center-y 110)
		 (:center-x-cn (m-constraint :max (center-x left width)
					     (setf center-x (+ left (truncate width 2)))
					     (setf left (- center-x (truncate width 2)))
					     (setf width (* 2 (- center-x left)))))
		 (:center-y-cn (m-constraint :max (top height center-y)
					     (setf center-y (+ top (truncate height 2)))
					     (setf height (* 2 (- center-y top)))
					     (setf top (- center-y (truncate height 2)))))
		 (:dot dot)
		 (:x 0)
		 (:y 0)
		 (:x-cn (m-constraint :max (center-x x (screen-x (gvl :dot :x)))
					  (setf x (- screen-x center-x))
					  (setf screen-x (+ x center-x))))
		 (:y-cn (m-constraint :max (y center-y (screen-y (gvl :dot :y)))
					  (setf y (- center-y screen-y))
					  (setf screen-y (- center-y y))))
		 (:rho 0)
		 (:theta 0)
		 (:xyrt-cn (m-constraint :max (x y rho theta)
					 (setf (x y) (values (* (cos theta) rho)
							     (* (sin theta) rho)))
					 (setf (rho theta)
					   (values (sqrt (+ (* x x) (* y y)))
						   (if (and (zerop x) (zerop y))
						       0.0
						     (atan y x))))))
		 )
(create-instance 'x-axis-line opal:line
		 (:x1 10) (:y1 110) (:x2 209) (:y2 110))
(create-instance 'y-axis-line opal:line
		 (:x1 110) (:y1 10) (:x2 110) (:y2 209))



(create-instance 'x-text opal:text
		 (:top 10) (:left 250)
		 (:string "X"))
(create-instance 'x-slider garnet-gadgets:h-slider
		 (:top 10) (:left 270)
		 (:val-1 -100.0) (:val-2 100.0)
		 (:format-string "~,1F")
		 (:enum-format-string "~,1F")
		 (:num-marks 5)
		 (:frame frame)
		 (:val-cn (m-constraint :max (value (x (gvl :frame :x)))
					(setf x value) (setf value x)))
		 )
(create-instance 'y-text opal:text
		 (:top 50) (:left 250)
		 (:string "Y"))
(create-instance 'y-slider garnet-gadgets:h-slider
		 (:top 50) (:left 270)
		 (:val-1 -100.0) (:val-2 100.0)
		 (:format-string "~,1F")
		 (:enum-format-string "~,1F")
		 (:num-marks 5)
		 (:frame frame)
		 (:val-cn (m-constraint :max (value (y (gvl :frame :y)))
					(setf y value) (setf value y)))
		 )
(create-instance 'rho-text opal:text
		 (:top 100) (:left 250)
		 (:string "Rho"))
(create-instance 'rho-slider garnet-gadgets:h-slider
		 (:top 100) (:left 290)
		 (:val-1 0.0) (:val-2 200.0)
		 (:format-string "~,1F")
		 (:enum-format-string "~,1F")
		 (:num-marks 5)
		 (:frame frame)
		 (:val-cn (m-constraint :max (value (rho (gvl :frame :rho)))
					(setf rho value) (setf value rho)))
		 )
(create-instance 'theta-text opal:text
		 (:top 140) (:left 250)
		 (:string "Theta"))
(create-instance 'theta-slider garnet-gadgets:h-slider
		 (:top 140) (:left 290)
		 (:val-1 -3.2) (:val-2 3.2)
		 (:num-marks 5)
		 (:format-string "~,1F")
		 (:enum-format-string "~,1F")
		 (:frame frame)
		 (:val-cn (m-constraint :max (value (theta (gvl :frame :theta)))
					(setf theta value) (setf value theta)))
		 )

(add-to-demo-window dot frame x-axis-line y-axis-line
		    x-text x-slider y-text y-slider
		    rho-text rho-slider theta-text theta-slider)
(update)
(create-instance nil inter:move-grow-interactor
		 (:window *demo-window*)
		 (:start-where (list :leaf-element-of (g-value *demo-window* :aggregate)
				     :type dot))
		 )

(run)


;;
;; =================================================================================
;; dashed-line dialog box

(clear-demo-window)

(create-instance
 'dash-controller opal:aggregadget
 (:prev nil)
 (:next nil)
 (:parts `((:main ,Opal:Rectangle
		  (:box ,(list 0 0 10 10))
		  (:width 10) (:height 10)
		  (:left 0) (:top 0)
		  (:unpack-box-cn
		   ,(m-constraint :max (box left top width height)
				  (setf left (max 0 (first box)))
				  (setf box (list left top width height))))
		  (:prev-box-cn
		   ,(m-constraint :max (left (pleft (gvl :parent :prev :main :left)))
				  (setf left (if (> pleft left) pleft left))
				  (setf pleft (if (< left pleft) left pleft))))
		  (:top-default-cn ,(m-constraint :weak (top) (setf top 10)))
		  (:top-cn ,(m-constraint :max (top (ptop (gvl :parent :prev :main :top)))
					  (setf top (+ ptop 15))))
		  )
	   (:line ,opal:line
		  (:x1 0) (:y1 0)
		  (:x2 0) (:y2 100)
		  (:x1-cn ,(m-constraint :max (x1 (mleft (gvl :parent :main :left)))
					 (setf x1 (+ 5 mleft))
					 (setf mleft (- mleft 5))))
		  (:x2-cn ,(m-constraint :max (x1 x2) (setf x1 x2) (setf x2 x1)))
		  )
	   (:dash ,opal:rectangle
		  (:left 0) (:top 120)
		  (:width 0) (:height 20)
		  (:left-cn ,(m-constraint :max (left (lleft (gvl :parent :line :x1)))
					   (setf left lleft)
					   ))
		  (:width-cn ,(m-constraint :max (width
						  (lleft (gvl :parent :line :x1))
						  (nleft (gvl :parent :next :line :x1)))
					    (setf width (- nleft lleft))
					    ))
		  ;; (:width-stay ,(m-stay-constraint :weak width))
		  (:filled-p t)
		  (:filled-p-cn
		   ,(m-constraint :max (filled-p
					width
					(prev-p (gvl :parent :prev :dash :filled-p)))
				  (setf filled-p (if (zerop width) prev-p (not prev-p)))))
		  (:filling-style ,opal:white-fill)
		  (:filling-style-cn
		   ,(m-constraint :max (filled-p filling-style)
				  (setf filling-style (if filled-p opal:black-fill opal:white-fill))))
		  (:line-style ,opal:no-line)
		  (:line-style-cn
		   ,(m-constraint :max (filled-p line-style)
				  (setf line-style (if filled-p opal:no-line opal:line-0))))
		  )
	   ))
 )

;; interesting feature: rightward dashes move when a leftward dash sweeps
;; over it, but on sweeping back they return to their original positions
;; stored in their :box slot

(setq controllers (loop for x from 1 to 5 collect (create-instance nil dash-controller)))
(loop for (a b) on controllers when b do (s-value b :prev a) (s-value a :next b))

(loop for c in controllers do (add-to-demo-window c))

(update)
(create-instance nil inter:move-grow-interactor
		 (:window *demo-window*)
		 (:start-where (list :leaf-element-of (g-value *demo-window* :aggregate)
				     :type opal:rectangle))
		 )

;; note: dash pattern cannot contain zeros: causes weird X error

;; note: use two line styles to defeat bug in garnet where new line-dashes
;; are only xmitted if line-style is not eql to previous one.

(create-instance
 'example-line opal:line
 (:x1 0) (:y1 150)
 (:x2 2000) (:y2 150)
 (:x1-cn (m-constraint :max (x1 (c1left (gvl :c1 :dash :left)))
		       (setf x1 c1left)))
 (:line-style (create-instance nil opal:line-style
			       (:line-style :dash)
			       (:constant nil)))
 (:line-style-save (create-instance nil opal:line-style
				    (:line-style :dash)
				    (:constant nil)))
 (:dash-cn (m-constraint :max
			 (line-style line-style-save
			  (d1 (gvl :c1 :dash :width))
			  (d2 (gvl :c2 :dash :width))
			  (d3 (gvl :c3 :dash :width))
			  (d4 (gvl :c4 :dash :width))
			  (d5 (gvl :c5 :dash :width)))
			 (setf (line-style line-style-save)
			   ;; set dash pattern in other line-style, and swap,
			   ;; to get around garnet bug where new line-style
			   ;; fields are only noticed if line-style is not eql
			   ;; to previous one.
			   (s-value line-style-save :dash-pattern
				    (remove 0 (list d1 d2 d3 d4 d5)))
			   (s-value line-style-save :line-style
				    (if (g-value line-style-save :dash-pattern) :dash :solid))
			   ;; (format t "~&~S~%" (g-value line-style-save :dash-pattern))
			   (values
			    line-style-save
			    line-style))
			 ))
 (:c1 (first controllers))
 (:c2 (second controllers))
 (:c3 (third controllers))
 (:c4 (fourth controllers))
 (:c5 (fifth controllers))
 )

(add-to-demo-window example-line)
(update)
(run)


;;||#
