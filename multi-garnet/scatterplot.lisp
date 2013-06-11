;;;-*- Mode: COMMON-LISP; Package: COMMON-LISP-USER -*-

(in-package :COMMON-LISP-USER)

(use-package '(:kr :kr-debug :garnet-debug :mg))

;; =================================================================================
;; This file contains code for a large Multi-Garnet example: a scatterplot
;; displaying a set of points, with several different interaction modes.
;; This is implemented with multi-way Multi-Garnet constraints between the
;; data values, the screen positions of the points, and the positions of
;; the X and Y-axes.

;; To try this example, compile and load this file.  Then, execute
;; (user::create-scatterplot-demo) to create the window displaying the
;; scatterplot.  Then, try manipulating the plot points and axes,
;; specifying different interaction modes via the buttons on the left of
;; the window.


;; =================================================================================
;; global variables used later in the file (defined here to prevent warning messages)

(defvar *scatterplot-demo-window* nil)
(defvar *data* nil)
(defvar *plot-1* nil)
(defvar *plot-2* nil)

;;
;; =================================================================================
;; *scatterplot-demo-parameters* contains fonts and line-styles and filling-styles used
;; in the scatterplots.  These are connected to the graphic objects by
;; formulas.  They can be easily changed by changing this one structure.

(create-instance
 '*scatterplot-demo-parameters* nil
 (:demo-font (create-instance nil opal:font
			      (:family :sans-serif)
			      (:face :bold)
			      (:size :large)))
 (:demo-button-label-font (create-instance nil opal:font
					   (:family :sans-serif)
					   (:face :bold)
					   (:size :large)))
 (:a-b-pt-fill opal:red-fill)
 (:a-b-pt-line opal:line-2)
 (:c-a-pt-fill opal:green-fill)
 (:c-a-pt-line opal:line-2)
 )

;; text objects used in scatterplot that inheriting font info

(create-instance
 '*scatterplot-demo-text* opal:text
 (:font (o-formula (gv *scatterplot-demo-parameters* :demo-font))))

(create-instance
 '*scatterplot-demo-multi-text* opal:multi-text
 (:font (o-formula (gv *scatterplot-demo-parameters* :demo-font))))

;;
;; =================================================================================
;; Scatterplot axis objects

;; width of axis
(defvar *axis-width* 4)

;; hack to prevent divide-by-zero errors
(defun safe/ (a b)
  (/ a (if (zerop b) 0.001 b)))

;; creates "vertical string" by inserting CR between every two letters
(defun create-vertical-string (hstr)
  (let ((len (length hstr))
	(vstr ""))
    (loop for index from 1 to (1- len) do
	  (setq vstr (format nil "~A~A~%" vstr (subseq hstr (1- index) index))))
    (when (> len 0)
	  (setq vstr (format nil "~A~A" vstr (subseq hstr (1- len) len))))
    vstr))

;; An axis line is actually a rectangle, so it will always be horizontal or
;; vertical (no arbitrary angles), and so it can be resized by
;; move-grow-interactor.  It is filled in so it looks like a line.
(create-instance
 '*axis-rectangle* Opal:Rectangle
 (:line-style Opal:thin-line)
 (:filling-style Opal:black-fill)
 (:box (list 100 100 100 100))
 (:left 100)
 (:top 100)
 (:width 100)
 (:height 100)
 ;; one-directional constraints unpacking :box
 (:left-cn (m-constraint :max (box left) (setf left (first box))))
 (:top-cn (m-constraint :max (box top) (setf top (second box))))
 (:width-cn (m-constraint :max (box width) (setf width (third box))))
 (:height-cn (m-constraint :max (box height) (setf height (fourth box))))
 )

;; *v-axis* is a vertical axis aggregadget, consisting of the axis line
;; (actually a rectangle), lower and upper limit numbers, and a label.  The
;; numbers strings and the label are to the left of the line.  The label is
;; printed "vertically" along the line.
(create-instance
 '*v-axis* opal:aggregadget
 (:parts `((:main ,*axis-rectangle*
		  (:box ,(list 20 20 *axis-width* 100))
		  (:width ,*axis-width*)
		  ;; don't inherit :width-cn: keep width constant, ignoring :box value
		  (:width-cn ,(m-stay-constraint :max width))
		  )
	   (:low-label ,*scatterplot-demo-text*
		       (:string "none")
		       (:left 0)
		       (:top 0)
		       (:string-cn ,(m-constraint :max ((low (gvl :parent :world-min))
							string)
						 (setf string (format nil "~,2F" low))))
		       (:left-cn ,(m-constraint :max ((oleft (gvl :parent :main :left))
						      (width (gvl :width))
						      left)
						(setf left (- oleft width 10))))
		       (:top-cn ,(m-constraint :max ((screen-min (gvl :parent :screen-min))
						     (height (gvl :height))
						     top)
					       (setf top (- screen-min (floor height 2)))))
		       )
	   (:high-label ,*scatterplot-demo-text*
			(:string "none")
			(:left 0)
			(:top 0)
			(:string-cn ,(m-constraint :max ((high (gvl :parent :world-max))
							 string)
						   (setf string (format nil "~,2F" high))))
			(:left-cn ,(m-constraint :max ((oleft (gvl :parent :main :left))
						       (width (gvl :width))
						       left)
						 (setf left (- oleft width 10))))
			(:top-cn ,(m-constraint :max ((screen-max (gvl :parent :screen-max))
						      (height (gvl :height))
						      top)
						(setf top (- screen-max (floor height 2)))))
			)
	   (:axis-label ,*scatterplot-demo-multi-text*
			(:justification :center)
			(:string "none")
			(:left 0)
			(:top 0)
			(:string-cn ,(m-constraint :max ((label (gvl :parent :label))
							 string)
						   (setf string (create-vertical-string label))))
			(:left-cn ,(m-constraint :max ((oleft (gvl :parent :main :left))
						       (width (gvl :width))
						       left)
						 (setf left (- oleft width 10))))
			(:top-cn ,(m-constraint :max ((screen-min (gvl :parent :screen-min))
						      (screen-max (gvl :parent :screen-max))
						      (height (gvl :height))
						      top)
						(setf top (- (floor (+ screen-min screen-max) 2)
							     (floor height 2)))))
			)
	   ))
 ;; the axis label
 (:label "")
 ;; :screen-min and :screen-max are the screen y-positions of the extreme
 ;; ends of the axis line.  :world-min and :world-max are the world values
 ;; corresponding to these two ends.  Although the two ends are labeled
 ;; "min" and "max", this is just a convenient labeling.  :screen-max can
 ;; be less than :screen-min, and :world-max can be less than :world-min.
 (:screen-min 0)
 (:screen-max 100)
 (:world-min 0.0)
 (:world-max 1.0)
 ;; scale and offset are defined so that data-value can be translated to screen value
 ;; by (data*scale)+offset, rather than (data - world-min)*scale + screen-min.
 (:offset 0.0)
 (:scale 1.0)
 ;; the :axis-cn constraint maintains the relationship between the screen
 ;; positions of the end of the axis (:screen-min and :screen-max), the
 ;; world values corresponding to these positions (:world-min and
 ;; :world-max), and the :scale and :offset slots used to position data
 ;; points relative to the scale.  This constraint defines methods to
 ;; calculate the values of each of these three pairs of slots from the
 ;; values of the other four.
 (:axis-cn (m-constraint :max (screen-min screen-max world-min world-max offset scale)
			 (setf (offset scale)
			   (let* ((scale (safe/ (- screen-max screen-min)
						(- world-max world-min)))
				  (offset (- screen-min (* scale world-min))))
			     (values offset scale)))
			 (setf (world-min world-max)
			   (let* ((world-min (safe/ (- screen-min offset) scale))
				  (world-max (+ (safe/ (- screen-max screen-min) scale)
						world-min)))
			     (values world-min world-max)))
			 (setf (screen-min screen-max)
			   (let* ((screen-min (+ (* world-min scale) offset))
				  (screen-max (+ (* (- world-max world-min) scale)
						  screen-min)))
			     (values (round screen-min) (round screen-max))))
			 ))
 ;; :box-cn relates the axis rectangle :box (left top width height) to
 ;; :screen-min, :screen-max, and :x.  Floats are rounded before
 ;; being stored in :box.
 (:x 20)
 (:box-cn (m-constraint :max ((box (gvl :main :box))
			      screen-min screen-max x)
			(setf (screen-min screen-max x)
			  (let* ((left (first box))
				 (top (second box))
				 (height (fourth box))
				 (bottom (+ top height)))
			    (values bottom top left)))
			(setf box (list (round x)
					(round screen-max)
					*axis-width*
					(round (- screen-min screen-max))
					))
			))
 )

;; *h-axis* is a horizontal axis aggregadget, consisting of the axis line
;; (actually a rectangle), lower and upper limit numbers, and a label.  The
;; numbers strings and the label are positioned below the line.  This
;; inherits some slots from *v-axis*, though all of the :parts items have
;; to be respecified to include different positioning constraints.
(create-instance
 '*h-axis* *v-axis*
 (:parts `((:main ,*axis-rectangle*
		  (:box ,(list 20 120 100 *axis-width*))
		  (:height ,*axis-width*)
		  ;; don't inherit :height-cn: keep height constant, ignoring :box value
		  (:height-cn ,(m-stay-constraint :max height))
		  )
	   (:low-label ,*scatterplot-demo-text*
		       (:string "none")
		       (:left 0)
		       (:top 0)
		       (:string-cn ,(m-constraint :max ((low (gvl :parent :world-min))
							string)
						 (setf string (format nil "~,2F" low))))
		       (:left-cn ,(m-constraint :max ((screen-min (gvl :parent :screen-min))
						      (width (gvl :width))
						      left)
						(setf left (- screen-min (floor width 2)))))
		       (:top-cn ,(m-constraint :max ((otop (gvl :parent :main :top))
						     (oheight (gvl :parent :main :height))
						     top)
					       (setf top (+ otop oheight 10))))
		       )
	   (:high-label ,*scatterplot-demo-text*
			(:string "none")
			(:left 0)
			(:top 0)
			(:string-cn ,(m-constraint :max ((high (gvl :parent :world-max))
							 string)
						   (setf string (format nil "~,2F" high))))
			(:left-cn ,(m-constraint :max ((screen-max (gvl :parent :screen-max))
						       (width (gvl :width))
						       left)
						 (setf left (- screen-max (floor width 2)))))
			(:top-cn ,(m-constraint :max ((otop (gvl :parent :main :top))
						      (oheight (gvl :parent :main :height))
						      top)
						(setf top (+ otop oheight 10))))
			)
	   (:axis-label ,*scatterplot-demo-multi-text*
			(:justification :left)
			(:string "none")
			(:left 0)
			(:top 0)
			(:string-cn ,(m-constraint :max ((label (gvl :parent :label))
							 string)
						   (setf string label)))
			(:left-cn ,(m-constraint :max ((screen-min (gvl :parent :screen-min))
						       (screen-max (gvl :parent :screen-max))
						       (width (gvl :width))
						       left)
						 (setf left (- (floor (+ screen-min screen-max) 2)
							       (floor width 2)))))
			(:top-cn ,(m-constraint :max ((otop (gvl :parent :main :top))
						      (oheight (gvl :parent :main :height))
						      top)
						(setf top (+ otop oheight 10))))
			)
	   ))
 
 ;; :screen-min, :screen-max, :world-min, :axis-cn etc are inherited from
 ;; *v-axis*.  In *h-axis*, the screen coords refer to x-coordinates,
 ;; rather than y-coordinates.
 ;; :box-cn relates the axis rectangle :box (left top width height) to
 ;; :screen-min, :screen-max, and :y.
 (:y 120)
 (:box-cn (m-constraint :max ((box (gvl :main :box))
			      screen-min screen-max y)
			(setf (screen-min screen-max y)
			  (let* ((left (first box))
				 (top (second box))
				 (width (third box))
				 (right (+ left width)))
			    (values left right top)))
			(setf box (list (round screen-min)
					(round y)
					(round (- screen-max screen-min))
					*axis-width*))
			))
 )

;;
;; =================================================================================
;; Scatterplot point objects

;; *screen-point* is the prototype object used for points in the
;; scatterplot.  It contains constraints to maintain its screen position in
;; correspondence to the data value it represents, and the position
;; and ranges of its X and Y axis.
(create-instance
 '*screen-point* opal:circle
 ;; X and Y-axis objects used to position this point.
 (:x-axis nil)
 (:y-axis nil)
 ;; the screen position within the window  of the center of this point.
 (:x 0)
 (:y 0)
 ;; :datum holds the data object that this point represents, and :world-x
 ;; and :world-y hold the two world values that are being measured on the X
 ;; and Y axes.  When *screen-point* is instantiated, it will include
 ;; constraints to set :world-x and :world-y as some function of the datum
 ;; value.
 (:datum nil)
 (:world-x 0.0)
 (:world-y 0.0)
 ;; :x-cn and :y-cn maintain the relationship between this point's screen
 ;; position (:x and :y), the datum values being represented (:world-y and
 ;; :world-y), and the :offset and :scale slots of the axes.
 (:x-cn
  (m-constraint :max ((offset (gvl :x-axis :offset))
		      (scale (gvl :x-axis :scale))
		      x world-x)
		(setf x (+ offset (* scale world-x)))
		(setf world-x (float (safe/ (- x offset) scale)))
		(setf offset (- x (* scale world-x)))
		(setf scale (safe/ (- x offset) world-x))))
 (:y-cn
  (m-constraint :max ((offset (gvl :y-axis :offset))
		      (scale (gvl :y-axis :scale))
		      y world-y)
		(setf y (+ offset (* scale world-y)))
		(setf world-y (float (safe/ (- y offset) scale)))
		(setf offset (- y (* scale world-y)))
		(setf scale (safe/ (- y offset) world-y))))
 ;; :box-cn maps between the :box list (left top width height) of the
 ;; point object, and the :x,:y positions of the center of the point.
 (:box (list 0 0 12 12))
 (:box-cn
  (m-constraint :max (box x y)
		(setf (x y) (values (float (+ (first box) 6))
				    (float (+ (second box) 6))))
		(setf box (list (round (- x 6))
				(round (- y 6))
				12 12))))
 (:left 0) (:top 0) (:width 12) (:height 12)
 ;; :unpack-box-cn unpacks the :box list (left top width height) of the point into
 ;; the four slots.
 (:unpack-box-cn
  (m-constraint :max (box left top width height)
		(setf (left top width height)
		  (values-list box))
		(setf box (list left top width height))))
 )

;; *abstract-datum* is the abstract data object used in this demo.
(create-instance
 '*abstract-datum* nil
 (:a 1.0)
 (:b 1.0)
 (:c 1.0)
 (:d 1.0))


;; *a-b-pt* and *c-a-pt* are two different types of screen points that
;; display different combinations of the fields of an abstract point.

;; *a-b-pt* uses the value of the datum :a slot as its :world-x value, and
;; the value of the :b slot as its :world-y value.
(create-instance
 '*a-b-pt* *screen-point*
 (:filling-style (o-formula (gv *scatterplot-demo-parameters* :a-b-pt-fill)))
 (:line-style (o-formula (gv *scatterplot-demo-parameters* :a-b-pt-line)))
 (:datum nil)
 (:datum-x-cn (m-constraint :max ((datum-x (gvl :datum :a))
				  world-x)
			    (setf datum-x world-x)
			    (setf world-x datum-x)))
 (:datum-y-cn (m-constraint :max ((datum-y (gvl :datum :b))
				  world-y)
			    (setf datum-y world-y)
			    (setf world-y datum-y)))
 )

;; *c-a-pt* uses the value of the datum :c slot as its :world-x value, and
;; the value of the :a slot as its :world-y value.
(create-instance
 '*c-a-pt* *screen-point*
 (:filling-style (o-formula (gv *scatterplot-demo-parameters* :c-a-pt-fill)))
 (:line-style (o-formula (gv *scatterplot-demo-parameters* :c-a-pt-line)))
 (:datum nil)
 (:datum-x-cn (m-constraint :max ((datum-x (gvl :datum :c))
				  world-x)
			    (setf datum-x world-x)
			    (setf world-x datum-x)))
 (:datum-y-cn (m-constraint :max ((datum-y (gvl :datum :a))
				  world-y)
			    (setf datum-y world-y)
			    (setf world-y datum-y)))
 )

;;
;; =================================================================================
;; Scatterplot demo buttons, used to set the different interaction modes

(eval-when (eval compile load)
  (load (merge-pathnames "radio-buttons-loader" Garnet-Gadgets-PathName))
  (load (merge-pathnames "x-buttons-loader" Garnet-Gadgets-PathName))
  (load (merge-pathnames "text-buttons-loader" Garnet-Gadgets-PathName))
  )

(defvar *inter-modes-strings-and-keywords*
    '("Change Data"             :change-points
      "Move Points"             :move-points
      "Scale Points"            :scale-points
      "Move Axis"               :move-axis
      "Scale Axis"              :scale-axis
      "Move Axis & Points"      :move-axis-points
      "Scale Axis & Points"     :scale-axis-points
      ))

(defun mode-string-to-keyword (str)
  (loop for (mode-string mode-keyword) on *inter-modes-strings-and-keywords* by #'CDDR
      when (equal str mode-string)
      do (return mode-keyword)
      finally (return nil)))

(defun create-scatterplot-buttons ()
  ;; radio button panel, used to set interaction mode
  (create-instance
   '*scatterplot-inter-modes* garnet-gadgets:radio-button-panel
   (:left 50)
   (:top 50)
   (:font (o-formula (gv *scatterplot-demo-parameters* :demo-button-label-font)))
   (:text-on-left-p t)
   (:shadow-offset 2)
   (:items (loop for (str key) on *inter-modes-strings-and-keywords* by #'CDDR
	       collect str))
   (:mode nil)
   (:mode-cn (m-constraint :max (value mode)
			   (setf mode (mode-string-to-keyword value))))
   )
  ;; 
  ;; The "X-Scale = Y-Scale" button adds an equality constraint between the
  ;; X-axis and Y-axis scales of *plot-1* if it is set.
  (create-instance
   '*xy-scale-eq* garnet-gadgets:x-button
   (:font (o-formula (gv *scatterplot-demo-parameters* :demo-button-label-font)))
   (:left (o-formula (- (opal:gv-right *scatterplot-inter-modes*) (gvl :width))))
   (:top (o-formula (+ (opal:gv-bottom *scatterplot-inter-modes*) 30)))
   (:direction :horizontal)
   (:string "X-Scale = Y-Scale")
   (:shadow-offset 2)
   (:x-axis nil)
   (:y-axis nil)
   (:equal-scale-cn
    (m-constraint :strong ((x-scale (gvl :x-axis :scale))
			   (y-scale (gvl :y-axis :scale)))
		  (setf x-scale (- y-scale))
		  (setf y-scale (- x-scale))))
   ;; these constraints prevent a strange propagation path from occurring.
   (:x-offset-stay (m-stay-constraint :weak (gvl :x-axis :offset)))
   (:y-offset-stay (m-stay-constraint :weak (gvl :y-axis :offset)))
   (:selection-function
    #'(lambda (obj val)
	;; if this button is selected, set the :x-axis and :y-axis slots,
	;; which will enable the :equal-scale-cn constraint.
	(s-value obj :x-axis (if val (g-value *plot-1* :x-axis) nil))
	(s-value obj :y-axis (if val (g-value *plot-1* :y-axis) nil))))
   )
  ;; The "Use Plans" button causes the interactors to construct and use
  ;; plans if it is set.
  (create-instance
   '*use-plan-button* garnet-gadgets:x-button
   (:font (o-formula (gv *scatterplot-demo-parameters* :demo-button-label-font)))
   (:left (o-formula (- (opal:gv-right *xy-scale-eq*) (gvl :width))))
   (:top (o-formula (+ (opal:gv-bottom *xy-scale-eq*) 10)))
   (:direction :horizontal)
   (:string "Use Plans")
   (:shadow-offset 2)
   )
  ;; initially want use-plans button set
  (g-value *use-plan-button* :value)
  (s-value *use-plan-button* :value (g-value *use-plan-button* :string))
  (opal:add-components (g-value *scatterplot-demo-window* :aggregate)
		       *scatterplot-inter-modes*
		       *xy-scale-eq*
		       *use-plan-button*
		       )
  (update)
  )

;;
;; =================================================================================
;; Scatterplot demo interactors.  There is one interactor for each
;; interaction mode.  Each interactor is active iff its corresponding
;; interaction mode is enabled (as determined by the :mode slot of the
;; *scatterplot-inter-modes* gadget).

;; *drag-cn-obj-inter* is a slightly modified version of
;; inter:move-grow-interactor that is instantiated for all of the
;; interactors used to move scatterplot points (if :cn-obj-type =
;; *screen-point*) or move or reshape scatterplot axes (if :cn-obj-type =
;; *axis-rectangle*).  The default :start-action, :running-action, and
;; :stop-action routines are modified to do two things: (1) The :cn-obj
;; slot of the interactor is set to the object being moved/reshaped while
;; the interactor is running.  Most of the instance interactors include
;; constraints (mostly stays) with indirect reference paths that use this
;; slot, so these constraints will be enabled when these interactors are
;; running, and disabled when the interactor stops.  (2) If the
;; :cn-obj-use-plan slot is non-NIL (as determined by the *use-plan-button*
;; gadget being selected), the instance interactors will generate and reuse
;; a plan to set the :box slot of the object being moved/reshaped (and
;; maintain all constraints that refer to this :box slot).

(create-instance
 '*drag-cn-obj-inter* inter:move-grow-interactor
 (:window nil)
 (:line-p nil)
 (:grow-p nil)
 ;; allow resized rectangles to have negative width, height
 (:min-width nil)
 (:min-height nil)
 (:start-event :leftdown)
 (:start-where (o-formula (list :leaf-element-of (gvl :window :aggregate)
				:type (gvl :cn-obj-type))))
 (:cn-obj nil)
 (:cn-obj-type *screen-point*)
 (:cn-obj-plan-cn nil)
 (:cn-obj-plan-cn-value nil) 
 (:cn-obj-use-plan (o-formula (gv *use-plan-button* :value)))
 (:start-action
   #'(lambda (inter obj pts)
       ;; activate stay constraints by setting cn-obj
       (s-value inter :cn-obj obj)
       (cond ((g-value inter :cn-obj-use-plan)
	      ;; save first value for :box
	      (s-value inter :cn-obj-plan-cn-value (copy-list pts))
	      ;; install constraint that sets :box of object from saved value
	      ;; (to be used as root of plan)
	      (s-value inter :cn-obj-plan-cn
		       (m-constraint :strong ((box (gvl :cn-obj :box)))
				     ;; use g-value so it will only update when plan run
				     (setf box (g-value inter :cn-obj-plan-cn-value))))
	      )
	     (t
	      (call-prototype-method inter obj pts)))
       ))
 (:running-action
   #'(lambda (inter obj pts)
       (cond ((g-value inter :cn-obj-use-plan)
	      ;; update saved value for :box
	      (s-value inter :cn-obj-plan-cn-value (copy-list pts))
	      ;; generate plan (if not cached) and run plan from root
	      ;; constraint to propagate new :box value through the network.
	      (propagate-plan-from-cn (g-value inter :cn-obj-plan-cn))
	      )
	     (t
	      (call-prototype-method inter obj pts)))
       ))
 (:stop-action
  #'(lambda (inter obj pts)
      ;; make sure that :running-action is called on last points
      (kr-send inter :running-action inter obj pts)
      (cond ((g-value inter :cn-obj-use-plan)
	     ;; remove constraint to set :box value
	     (s-value inter :cn-obj-plan-cn nil))
	    (t
	     (call-prototype-method inter obj pts)))
      ;; deactivate stay constraints that reference :cn-obj
      (s-value inter :cn-obj nil)
      ))
 )

;; *sp-change-point-inter* allows changing the actual data value of a point
;; by moving the screen point.  When running, it adds stay constraints to
;; the :offset and :scale slots of the X and Y-axes, so that neither of the
;; axes will be changed at all.

(create-instance
 '*sp-change-point-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :change-points (gv *scatterplot-inter-modes* :mode))))
 (:cn-obj nil)
 (:cn-obj-type *screen-point*)
 (:obj-offset-stay (m-stay-constraint :max
				      (gvl :cn-obj :x-axis :offset)
				      (gvl :cn-obj :y-axis :offset)))
 (:obj-scale-stay (m-stay-constraint :max
				     (gvl :cn-obj :x-axis :scale)
				     (gvl :cn-obj :y-axis :scale)))
 )

;; *sp-move-points-inter* allows moving all of the screen points in a plot,
;; as a cloud, as one point is moved.  When running, it adds stay
;; constraints to the world-coordinates of the point being dragged (so it's
;; value won't be changed), to the :scale slots of the axes (so the scales
;; won't change), and to the screen-position slots of the axes (so the axes
;; won't move).  As a result, the axis :offset slots are changed, causing
;; the other points in the plot to be offset by the same amount, and the
;; plot range numbers to change.

(create-instance
 '*sp-move-points-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :move-points (gv *scatterplot-inter-modes* :mode))))
 ;; stay constraints used to fix world-position scale while moving object screen position
 (:cn-obj nil)
 (:cn-obj-type *screen-point*)
 (:obj-world-stay (m-stay-constraint :max
				     (gvl :cn-obj :world-x)
				     (gvl :cn-obj :world-y)))
 (:obj-scale-stay (m-stay-constraint :max
				     (gvl :cn-obj :x-axis :scale)
				     (gvl :cn-obj :y-axis :scale)))
 (:obj-axis-stay (m-stay-constraint :max
				    (gvl :cn-obj :x-axis :screen-min)
				    (gvl :cn-obj :x-axis :screen-max)
				    (gvl :cn-obj :y-axis :screen-min)
				    (gvl :cn-obj :y-axis :screen-max)))
 )

;; *sp-move-points-inter-alt* is an alternate form of
;; *sp-move-points-inter* (activated by buttoning :control-leftdown instead
;; of :leftdown) that anchors the upper and lower ranges of the axes,
;; instead of their screen position.  Therefore, as the point cloud is
;; moved, the axes will move to keep the same ranges.

(create-instance
 '*sp-move-points-inter-alt* *sp-move-points-inter*
 (:start-event :control-leftdown)
 (:obj-axis-stay (m-stay-constraint :max
				    (gvl :cn-obj :x-axis :world-min)
				    (gvl :cn-obj :x-axis :world-max)
				    (gvl :cn-obj :y-axis :world-min)
				    (gvl :cn-obj :y-axis :world-max)))
 
 )

;; *sp-scale-points-inter* allows scaling all of the screen points in a
;; plot, in both X and Y-directions, as a single point is moved.  When
;; running, it adds stay constraints to the world-coordinates of the point
;; being dragged (so it's value won't be changed), to the :offset slots of
;; the axes (so the offsets won't change), and to the screen-position slots
;; of the axes (so the axes won't move).  As a result, the axis :scale
;; slots are changed, causing the other points in the plot to be moved
;; (scaled), and the plot range numbers to change.

(create-instance
 '*sp-scale-points-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :scale-points (gv *scatterplot-inter-modes* :mode))))
 ;; stay constraints used to fix world-position scale while moving object screen position
 (:cn-obj nil)
 (:cn-obj-type *screen-point*)
 (:obj-world-stay (m-stay-constraint :max
				     (gvl :cn-obj :world-x)
				     (gvl :cn-obj :world-y)))
 (:obj-offset-stay (m-stay-constraint :max
				      (gvl :cn-obj :x-axis :offset)
				      (gvl :cn-obj :y-axis :offset)))
 (:obj-axis-stay (m-stay-constraint :max
				     (gvl :cn-obj :x-axis :screen-min)
				     (gvl :cn-obj :x-axis :screen-max)
				     (gvl :cn-obj :y-axis :screen-min)
				     (gvl :cn-obj :y-axis :screen-max)))
 )

;; *sp-scale-points-inter-alt* is an alternate form of
;; *sp-scale-points-inter* (activated by buttoning :control-leftdown
;; instead of :leftdown) that anchors the upper and lower ranges of the
;; axes, instead of their screen position.  Therefore, as the points are
;; scaled, the axes will be resized so that the axes will mark the same
;; ranges.

(create-instance
 '*sp-scale-points-inter-alt* *sp-scale-points-inter*
 (:start-event :control-leftdown)
 (:obj-axis-stay (m-stay-constraint :max
				    (gvl :cn-obj :x-axis :world-min)
				    (gvl :cn-obj :x-axis :world-max)
				    (gvl :cn-obj :y-axis :world-min)
				    (gvl :cn-obj :y-axis :world-max)))
 
 )

;; *sp-move-axis-inter* allows moving an axis.  The :offset and :scale
;; slots are anchored, so none of the displayed points are changed.  As the
;; axis is moved, the lower and upper range numbers are changed to
;; correspond to the range being marked by the axis.

(create-instance
 '*sp-move-axis-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :move-axis (gv *scatterplot-inter-modes* :mode))))
 (:grow-p nil)
 (:cn-obj nil)
 (:cn-obj-type *axis-rectangle*)
 (:obj-offset-stay (m-stay-constraint :max (gvl :cn-obj :parent :offset)))
 (:obj-scale-stay (m-stay-constraint :max (gvl :cn-obj :parent :scale)))
 )

;; *sp-scale-axis-inter* allows reshaping an axis.  The :offset and :scale
;; slots are anchored, so none of the displayed points are changed.  As the
;; axis is reshaped, the lower and upper range numbers are changed to
;; correspond to the range being marked by the axis.  This is essentially
;; the same as *sp-move-axis-inter*, except that the :grow-p slot is set to
;; T, rather than NIL, so the interactor will reshape the :box of the axis
;; rectangle, instead of just moving it.

(create-instance
 '*sp-scale-axis-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :scale-axis (gv *scatterplot-inter-modes* :mode))))
 (:grow-p t)
 (:cn-obj nil)
 (:cn-obj-type *axis-rectangle*)
 (:obj-offset-stay (m-stay-constraint :max (gvl :cn-obj :parent :offset)))
 (:obj-scale-stay (m-stay-constraint :max (gvl :cn-obj :parent :scale)))
 )

;; *sp-move-axis-points-inter* allows moving an axis, along with the points
;; in the plot.  The axis range slots (:world-min and :world-max) are
;; anchored, so as the axis is moved, the :scale and :offset slots will be
;; updated, which will cause the data points screen positions to be
;; changed.  Note: since the length of the axis is not changed as it is
;; moved, the :scale will not be changed either (i.e. it will be
;; recalculated, but the constraint method will always calculate the same
;; number).

(create-instance
 '*sp-move-axis-points-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :move-axis-points (gv *scatterplot-inter-modes* :mode))))
 (:grow-p nil)
 (:cn-obj nil)
 (:cn-obj-type *axis-rectangle*)
 (:obj-stay (m-stay-constraint :max
			       (gvl :cn-obj :parent :world-min)
			       (gvl :cn-obj :parent :world-max)))
 )

;; *sp-move-axis-points-inter* allows reshaping an axis, along with the
;; points in the plot.  The axis range slots (:world-min and :world-max) are
;; anchored, so as the axis is reshaped, the :scale and :offset slots will
;; be updated, which will cause the data points to be changed.  This is the
;; same as *sp-move-axis-points-inter*, except that the :grow-p slot is set
;; to T, rather than NIL, so the interactor will reshape the :box of the
;; axis rectangle, instead of just moving it.  Since the length of the axis
;; is changed, _both_ the :offset and :scale slots will be changed.

(create-instance
 '*sp-scale-axis-points-inter* *drag-cn-obj-inter*
 (:active (o-formula (eql :scale-axis-points (gv *scatterplot-inter-modes* :mode))))
 (:grow-p t)
 (:cn-obj nil)
 (:cn-obj-type *axis-rectangle*)
 (:obj-stay (m-stay-constraint :max
			       (gvl :cn-obj :parent :world-min)
			       (gvl :cn-obj :parent :world-max)))
 )


(defun create-scatterplot-interactors ()
  (create-instance nil *sp-change-point-inter*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-move-points-inter*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-move-points-inter-alt*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-scale-points-inter*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-scale-points-inter-alt*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-move-axis-inter*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-scale-axis-inter*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-move-axis-points-inter*
		   (:window *scatterplot-demo-window*))
  (create-instance nil *sp-scale-axis-points-inter*
		   (:window *scatterplot-demo-window*))
  )

;;
;; =================================================================================
;; Fns to create the scatterplot demo

;; create-xy-plot creates an x-y scatterplot displaying the specified data,
;; by creating a screen point for each datum (instantiating the supplied
;; screen-point-proto), and creating X and Y-axes.  It returns an aggregate
;; containing both axes, and all of the points.  The axis labels and low
;; and high ranges are specified by optional arguments.  The bbox argument
;; specifies the bounding box that the axes should inhabit (ignoring the
;; labels and range numbers).  The X-axis is positioned on the left edge,
;; and the Y-axis along the bottom, with their ends just touching.
(defun create-xy-plot (data screen-point-proto
		       &key (x-axis-label "X")
			    (x-axis-low 0.0)
			    (x-axis-high 100.0)
			    (y-axis-label "Y")
			    (y-axis-low 0.0)
			    (y-axis-high 100.0)
			    (bbox '(0 0 100 100)))			    
  (let* ((x-axis (create-instance nil *h-axis*
				  (:label x-axis-label)))
	 (y-axis (create-instance nil *v-axis*
				  (:label y-axis-label)))
	 (agg (create-instance nil opal:aggregate
			       (:x-axis x-axis)
			       (:y-axis y-axis)))
	 (bbleft (first bbox))
	 (bbtop (second bbox))
	 (bbwidth (third bbox))
	 (bbheight (fourth bbox))
	 (y-axis-width (g-value y-axis :main :width))
	 (x-axis-height (g-value x-axis :main :height))
	 (x-axis-box (list (+ bbleft y-axis-width)
			   (+ bbtop bbheight (- x-axis-height))
			   (- bbwidth y-axis-width)
			   x-axis-height))
	 (y-axis-box (list bbleft bbtop y-axis-width
			   (- bbheight x-axis-height)))
	 pts)
    (with-slots-set (((g-value x-axis :main) :box x-axis-box)
		     ((g-value y-axis :main) :box y-axis-box)
		     (x-axis :world-min x-axis-low)
		     (x-axis :world-max x-axis-high)
		     (y-axis :world-min y-axis-low)
		     (y-axis :world-max y-axis-high))
      ;; with-slots-set is used to make sure that the x,y-axis slots are
      ;; held constant as the data points are created.  Otherwise, it is
      ;; possible that the axis positions or range could be modified to
      ;; make the data world-values correspond with the default
      ;; screen-point positions.
      (setq pts (loop for datum in data collect
		      (create-instance nil screen-point-proto
				       (:datum datum)
				       (:x-axis x-axis)
				       (:y-axis y-axis))))
      (s-value agg :pts pts)
      )
    (opal:add-components agg x-axis y-axis)
    (loop for pt in pts do (opal:add-component agg pt))
    agg))

(defun create-scatterplot-demo-window (&key (width 600)
					    (height 200)
					    (top 100)
					    (left 600)
					    (double-buffered-p nil))
  (let* ((win (create-instance nil inter:interactor-window
			       (:double-buffered-p double-buffered-p)
                               (:title "Multi-Garnet Scatterplot Demo Window")
                               (:width width)
                               (:height height)
                               (:top top)
                               (:left left)
                               (:position-by-hand nil)
                               ))
         (agg (create-instance nil opal:aggregate)))
    (s-value win :aggregate agg)
    (opal:update win t)
    (setq *scatterplot-demo-window* win)))

(defun update ()
  (if (is-a-p *scatterplot-demo-window* inter:interactor-window)
      (opal:update *scatterplot-demo-window* t)))

;; random data values for demo: first data values are constant so we can
;; find point at upper left in scatterplot.
(defvar *data-values* (cons (list 98.6 97.5 98.7 (* 0.7 97.5))
			    (loop for cnt from 1 to 20 collect
				  (let* ((a (random 100))
					 (b (random 100))
					 (c (random 100))
					 (d (* 0.7 (max 0 (+ -5 (random 10) b)))))
				    (list a b c d)))))

(defun create-scatterplot-demo ()
  (let* ()
    (create-scatterplot-demo-window :height 380 :width 1000 :top 30 :left 30 :double-buffered-p t)
    ;; create data objects, using *data-values* values.  Add weak stays on
    ;; values, so screen point positions will be changed in preference to
    ;; changing the actual data values.
    (setq *data* (loop for (a b c d) in *data-values*
		     collect (create-instance nil *abstract-datum*
					      (:a a) (:b b) (:c c) (:d d)
					      (:a-stay (m-stay-constraint :weak a))
					      (:b-stay (m-stay-constraint :weak b))
					      (:c-stay (m-stay-constraint :weak c))
					      (:d-stay (m-stay-constraint :weak d))
					      )))
    (setq *plot-1* (create-xy-plot *data* *a-b-pt*
				   :x-axis-label "A"
				   :y-axis-label "B"
				   :bbox (list 370 50 200 250)))
    (setq *plot-2* (create-xy-plot *data* *c-a-pt*
				   :x-axis-label "C"
				   :y-axis-label "A"
				   :bbox (list 720 50 200 250)))
    (opal:add-components (g-value *scatterplot-demo-window* :aggregate) *plot-1* *plot-2*)
    (update)
    (create-scatterplot-buttons)
    (create-scatterplot-interactors)
    (update)
    ))

