;;;
;;; plotxy.lisp
;;;

;;; A Garnet program for plotting points in XY coordinates.  Demos appear at the
;;; end of the file.

;;; Ported by Jim Davis 4 Aug 92 from some code I wrote for the Lisp Machine long ago.

(defpackage "CLWIN"
  (:use "COMMON-LISP" "KR")
  (:export
   ;; aggregates defined here, you might build upon them
   coordinate-transform-window		;window with coord xforms
   graphing-window			;further specialized for graphing a function
   plotting-axis			;axes for plotting
   horizontal-ticks plotting-x-axis plotting-y-axis
   graphing-aggregate			;contains axes and graphing-window

   plot-points				;standalone function for simple plotting
   line-connected-data-points

   coordinate-transform-function
   coordinate-transform-inverse))

(in-package :clwin)

;;;(use-package :kr)



; This window allows you to work in XY coordinates where the origin is
; in the lower left corner, and X increases to the right and Y
; increases upwards, in other words the more familiar graphing
; coordinates.  It defines a pair of functions x-to-u and y-to-v which
; take X and Y (user) coordinates into U and V pixel (window)
; coordinates, applying scaling and translation.  You may specify the
; XY origin to be anywhere (in pixels, relative to upper left corner)

; There are two KLUDGES here.  Maybe someone can tell me how to fix them?
; You will notice them below.  The kludges are :TRANSFORMS-CHANGED and
; the UPDATE method for CLWIN windows.

(defun coordinate-transform-function (agg origin scale)
  "Return a function which takes w (user coordinates) into window coordinates"
  (let ((agg agg))
    (function (lambda (w) (+ (g-value agg origin)
			     (truncate (* w (g-value agg scale))))))))

(defun coordinate-transform-inverse (agg origin scale)
   (let ((agg agg))
     (function (lambda (p) (/ (- p (g-value agg origin)) (g-value agg scale))))))

; This is a window so that clipping can happen
(create-instance 'coordinate-transform-window inter:interactor-window
  (:omit-title-bar-p nil)
  (:visible #f (not (null (gvl :aggregate))))
  (:origin-x 0)				;in pixels, relative to upper left
  (:origin-y #f (gvl :height))		;default places origin in lower left corner

  ;;  scale factors in pixels/coordinate
  (:scale 10)
  (:xscale #f (gvl :scale))
  (:yscale #f (- (gvl :scale)))		;so that Y goes up,

  ;; Coordinate transform:  Take XY (user coordinates) to UV (window pixel coords)

  (:x-to-u #f (coordinate-transform-function (gv :self) :origin-x :xscale))
  (:y-to-v #f (coordinate-transform-function (gv :self) :origin-y :yscale))

  (:u-to-x #f (coordinate-transform-inverse (gv :self) :origin-x :xscale))
  (:v-to-y #f (coordinate-transform-inverse (gv :self) :origin-y :yscale))
  )

; these functions can be used only by objects within a
; coordinate-transform-window

(export '(x-to-u y-to-v u-to-x v-to-y))

(defun x-to-u (x)
  (funcall (gvl :window :x-to-u) x))

(defun y-to-v (y)
  (funcall (gvl :window :y-to-v) y))

(defun u-to-x (u)
  (funcall (gvl :window :u-to-x) u))

(defun v-to-y (v)
  (funcall (gvl :window :v-to-y) v))



;; Polyline drawn through datapoints.

; This must be used within a window which defines :x-to-u and :y-to-v.

(create-instance 'line-connected-data-points opal:polyline
  (:x-function #'first)			;extract X from an ITEM
  (:y-function #'second)
  (:data-points nil)
  (:point-list
   #f (mapcan (function (lambda (point)
			  (list (x-to-u (funcall (gvl :x-function) point))
				(y-to-v (funcall (gvl :y-function) point)))))
	      (gvl :data-points))))


; Unfortunately, the formula for the point-list does not record the
; dependency on x-to-u (or y-to-v), and so if x-to-u changes it does not
; automatically recompute the point list.   So instead I need to explicitly
; tell the graphic object to recompute itself.

(define-method :transforms-changed line-connected-data-points (self)
  (recompute-formula self :point-list))

; recursively recompute all transforms within self.
(define-method :transforms-changed opal:aggregate (self)
  (opal:do-components self
    #'(lambda (component)
	(kr-send component :transforms-changed component))))

;; After changing scale factor, must cause all objects to redisplay.

(define-method :set-scale coordinate-transform-window (self scale)
  (s-value self :scale scale)
  (kr-send (g-value self :aggregate) :transforms-changed (g-value self :aggregate))
  (opal:update self))

(define-method :set-scale-y coordinate-transform-window (self yscale)
  (s-value self :yscale yscale)
  (kr-send (g-value self :aggregate) :transforms-changed (g-value self :aggregate))
  (opal:update self))

(define-method :set-scale-x coordinate-transform-window (self xscale)
  (s-value self :xscale xscale)
  (kr-send (g-value self :aggregate) :transforms-changed (g-value self :aggregate))
  (opal:update self))

(define-method :set-origin coordinate-transform-window (window x0 y0)
  (s-value window :origin-x x0)
  (s-value window :origin-y y0)
  (kr-send (g-value window :aggregate) :transforms-changed (g-value window :aggregate))
  (opal:update window))



#|  ; test and example code for coordinate-transform window

(create-instance 'w1 coordinate-transform-window
  (:xscale (/ 400 20))
  (:yscale -100)
  (:left 700)
  (:origin-y #f (truncate (gvl :height) 2))
  (:top 200)
  (:width 400)
  (:height 400)
  (:aggregate (create-instance nil opal:aggregate)))

(define-method :show-points w1 (self list)
  (opal:add-component (g-value self :aggregate)
		      (create-instance nil line-connected-data-points 
			  (:data-points list))))

(defun sine-points (&key (res 36)
			 (freq 1)
			 (amplitude 1)
			 (cycles 2))
  (do* ((inc (/ (* 2 pi) res))
	(i 0 (+ i inc))
	(limit (* 2 pi cycles))
	(list nil))
      ((> i limit)
       (reverse list))
    (push (list i (* amplitude (sin (* freq i)))) list)))

(kr-send w1 :show-points w1 (sine-points))

(opal:update w1)

(create-instance 'click-xy inter:two-point-interactor
  (:window w1)
  (:operates-on w1)
  (:start-where #f `(:in ,(gvl :operates-on)))
  (:start-event :leftdown)
  (:continuous nil)
  (:final-function 
   #'(lambda (interactor pl)
       (format t "~&~D,~D -> ~,2f,~,2f~%"
	       (first pl)
	       (second pl)
	       (funcall (g-value interactor :window :u-to-x) (first pl))
	       (funcall (g-value interactor :window :v-to-y) (second pl)))))
  )

(opal:destroy click-xy)

;; add a second set of points to the display

(kr-send w1 :show-points w1 (sine-points :amplitude .5 :freq 2))

(kr-send w1 :set-scale-y w1 -50)

(kr-send w1 :set-scale-y w1 -100)

(g-value w1 :xscale)

(kr-send w1 :set-scale-x w1 40)

(kr-send w1 :set-scale-x w1 -100)


;; This code is to help me figure out why the formula's dependencies dont work

(setf p (first (g-value (g-value w1 :aggregate :components))))

; changing xscale won't cause an update
(s-value w1 :xscale 40)
(opal:update w1)

; changing DATA POINTS is sufficient to cause an update.
(s-value p :data-points (cons (car (g-value p :data-points))
			      (cdr (g-value p :data-points))))

; changing x-to-u is sufficient, also.
(s-value w1 :x-to-u (g-value w1 :x-to-u))

(opal:destroy w1)

|#

#|
;; THIS IS OUT OF DATE.  MAYBE I WILL REVIVE IT SOMEDAY

; Coordinate Transform which chooses scale so as to display all the
; items in a list.  It chooses scales for x and y to completely fill
; the graph.  If you want different ranges, then specify explicit
; x-min and x-max (etc) which will change the way the auto-scaling is
; done.

; Data points can be in ANY format, you must provide a function
; which extracts the X and Y from the format.  By default points are
; stored as a list of (X Y), so the required functions are just
; #'first and #'second but if e.g. your data is in a structure you can
; supply the required accessors.

; to do: implement autoscale-p NIL to turn off autoscaling.

(create-instance 'autoscale-coordinate-transform coordinate-transform
  (:items nil)
  (:autoscale-p t)
  (:x-function #'first)			;extract X from an ITEM
  (:y-function #'second)
  ;; extremes of X and Y
  (:x-min #f (extreme-value (gvl :items) :key (gvl :x-function) :test #'<))
  (:x-max #f (extreme-value (gvl :items) :key (gvl :x-function) :test #'>))
  (:y-min #f (extreme-value (gvl :items) :key (gvl :y-function) :test #'<))
  (:y-max #f (extreme-value (gvl :items) :key (gvl :y-function) :test #'>))
  ;; ranges of X and Y
  (:dx #f (- (gvl :x-max) (gvl :x-min)))
  (:dy #f (- (gvl :y-max) (gvl :y-min)))
  ;; scale factor chosen to show all X and Y values across full width.
  (:umax #f (gvl :width))
  (:vmax #f 0)
  (:xscale #f (/ (- (gvl :umax) (gvl :origin-x)) (gvl :dx)))
  (:yscale #f (/ (- (gvl :vmax) (gvl :origin-y)) (gvl :dy)))
  )

(defun extreme-value (sequence &key (key 'identity) (test #'>))
  (let ((val nil))
    (map nil #'(lambda (elt)
		 (when (and (funcall key elt)
			    (or (null val)
				(funcall test (funcall key elt) val)))
		   (setf val (funcall key elt))))
	 sequence)
    val))

|#


;; Plotting points in a window with axes.

; The GRAPHING-AGGREGATE holds a pair of axes and a subwindow (graphing-window).
; The axes have ticks and labels.
; Here, W means either X or Y (for horizontal axis, it is X, for vertical, Y)
; W-TO-LABEL is a function from a W to a string for a tick
; mark at that position.  The default function just calls FORMAT with
; the value of :TICK-FORMAT, which defaults to a string which displays W 
; as a floating point.

; If you want plain integer labels, change tick-format to "~D".
; If you want nominal labels instead, make w-to-label a function like
;   (lambda (x) (nth (round x) '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"))))

; it would be nice if the Y axis text ran vertically.  It would be
; nice to have more flexible control over placement of axes.  For
; instance, you might want the axes drawn at 0,0 even if it's off the
; screen.

; The parent of the tick mark is a tick-list.  The parent of the
; tick-list is an axis.  The parent of the axis is the graph
; aggregate.  The graph aggregate specifies the actual position of the
; axes.

(create-instance 'TICK-MARK opal:aggregadget
  (:parent nil)				;an AXIS
  (:horizontal-p #f (gvl :parent :horizontal-p))
  (:rank 0)
  (:length #f (gvl :parent :tick-size))
  (:u1 #f (if (gvl :horizontal-p)
	    (+ (gvl :parent :x1)
	       (* (gvl :rank)
		  (gvl :parent :tick-separation)))
	    (gvl :parent :x1)))
  (:v1 #f (if (gvl :horizontal-p)
	    (gvl :parent :y1)
	    (+ (gvl :parent :y1)
	       (* (gvl :rank)
		  (gvl :parent :tick-separation)))))
  (:u2 #f (if (gvl :horizontal-p)
	    (gvl :u1)
	    (- (gvl :u1) (gvl :length))))
  (:v2 #f (if (gvl :horizontal-p)
	    (+  (gvl :v1) (gvl :length))
	    (gvl :v1)))
  (:value #f (if (gvl :horizontal-p)
	       (funcall (gvl :parent :graph :u-to-x)
			(- (gvl :u1) (gvl :parent :x1)))
	       (funcall (gvl :parent :graph :v-to-y)
			(- (gvl :v1) (gvl :parent :parent :y2)))))
  (:w-to-label #f (gvl :parent :w-to-label))
  (:tick-format #f (gvl :parent :tick-format))
  (:label-string #f (funcall (gvl :w-to-label)
			     (gvl :value)))
  (:parts
   `(
     (:line ,opal:line
	    (:x1 ,#f (gvl :parent :u1))
	    (:y1 ,#f (gvl :parent :v1))
	    (:x2 ,#f (gvl :parent :u2))
	    (:y2 ,#f (gvl :parent :v2)))
     (:label ,opal:text
	     (:string ,#f (gvl :parent :label-string))
	     (:left ,#f (if (gvl :parent :horizontal-p)
			  (gvl :parent :u1)
			  (- (gvl :parent :u2)
			     (gvl :width)
			     2)))
	     (:top ,#f (if (gvl :parent :horizontal-p)
			 (gvl :parent :v2)
			 (gvl :parent :v2))))
     )))

(create-instance 'tick-list opal:aggregate
  (:horizontal-p #f (gvl :parent :horizontal-p))
  (:x1 #f (gvl :parent :x1))
  (:y1 #f (gvl :parent :y1))
  (:x2 #f (if (gvl :horizontal-p)
	    (gvl :parent :x2)
	    (gvl :parent :x1)))
  (:y2 #f (if (gvl :horizontal-p)
	    (gvl :parent :y1)
	    (gvl :parent :y2)))
  (:delta  #f (if (gvl :horizontal-p)
		    (- (gvl :x2) (gvl :x1))
		    (- (gvl :y2) (gvl :y1))))
  (:tick-separation #f (if (zerop (gvl :tick-count))
			 0
			 (truncate (gvl :delta)
				   (gvl :tick-count))))
  (:ticks nil)
  (:tick-size #f (gvl :parent :tick-size))
  (:tick-count #f (gvl :parent :tick-count))
  (:tick-format #f (gvl :parent :tick-format))
  (:w-to-label #f (gvl :parent :w-to-label))
  (:graph #f (gvl :parent :graph))
  )

(define-method :initialize tick-list (self)
  (kr-send self :create-tick-marks self)
  (call-prototype-method self))

(define-method :create-tick-marks tick-list (self)
  (let ((list nil))
    (dotimes (i (g-value self :tick-count))
      (push (create-instance nil tick-mark
	      (:rank i))
	    list))
    (s-value self :ticks (nreverse list))
    (dolist (tick  (g-value self :ticks))
      (opal:add-component self tick))))
			       



; This axis is positioned relative to corner of the window.
(create-instance 'plotting-axis opal:aggregadget
  (:horizontal-p nil)
  (:label-string "label")
  (:tick-size 10)
  (:tick-count 0)
  (:w-to-label #f				;function from X or Y value to string
	       #'(lambda (value)
		   (format nil "~@?" (gvl :tick-format) value)))
  (:tick-format "~,2F")			;how to display values (for format)

  (:graph #f (gvl :parent :graph-subwindow))

  (:x1 #f (gvl :parent :graph-left))	;these are garnet coordinates
  (:y1 #f (gvl :parent :graph-bottom))
  ;; upper right
  (:x2 #f (gvl :parent :graph-right))
  (:y2 #f (gvl :parent :graph-top))

  (:parts
   `((:line ,opal:line
	    (:x1 ,#f (gvl :parent :x1))
	    (:y1 ,#f (gvl :parent :y1))
	    ;; other endpoint is determined by specialization
	    )
     (:ticks ,tick-list)
     (:label ,opal:text
	     (:string ,#f (gvl :parent :label-string)) )
     ))
  )

;; X and Y axes are specializations to obtain tick format etc
;; from the parent aggregate.
(create-instance 'plotting-x-axis plotting-axis
  (:horizontal-p t)
  (:parts
   `((:line :modify
	    (:x2 ,#f  (gvl :parent :x2))
	    (:y2 ,#f  (gvl :parent :y1)))
     :ticks
     (:label :modify
	     (:top ,#f (+ (gvl :parent :y1) 5))
	     (:left ,#f (truncate (gvl :parent :x2) 2)))
     )))

(create-instance 'plotting-y-axis plotting-axis
  (:horizontal-p nil)
  (:parts
   `((:line :modify
	    (:x2 ,#f  (gvl :parent :x1))
	    (:y2 ,#f  (gvl :parent :y2)))
     :ticks
     (:label :modify
	     (:top ,#f  (truncate (gvl :parent :y1) 2))
	     (:left 5)))))
   


; A coordinate-transform window for use inside a larger aggregate
; which presumably draws axes around it.  Setting border-width to 0 so
; you don't notice that it is a window.  The window size and position
; are determined by the CONTAINER, which is the aggregate the window
; is inside of.  Note that the PARENT of a window must be another
; window, so I use CONTAINER to point to the aggregate.  left, top,
; width and height must ALWAYS be a positive integer, even if
; container is NIL, because opal will try to update the window, and
; create an xlib drawable for it.

(create-instance 'graphing-window coordinate-transform-window
  (:border-width 0)
  (:visible #f (and (gvl :aggregate)	;normally, having an aggregate suffices
		    (gvl :parent)	;but this window is to be a subwindow
		    (and		;this is really more of a sanity check
		     (gvl :container)	;than essential
		     (eq (gvl :container :window)
			 (gvl :parent)))
		    (gvl :parent :visible)))
  (:container nil)
  (:left #f (if (gvl :container)
		(+ (gvl :container :graph-left) 1)
	      0))
  (:top #f (if (gvl :container)
	       (gvl :container :graph-top)
	     0))
  (:width #f (if (gvl :container)
		 (gvl :container :graph-width)
	       200))			;must always be an integer,(see below)
  (:height #f (if (gvl :container)
		  (gvl :container :graph-height)
		200)))



;; Aggregate which draws coordinate axes and has a GRAPH-SUBWINDOW
;; where coordinates and transformed.

(create-instance 'graphing-aggregate opal:aggregadget
  (:width 400)				;needs fixed size so subwindow position
  (:height 400)				;is predictable
  (:top 0)
  (:left 0)
  (:graph-left #f (+ (gvl :left) (gvl :axis-x-margin)))
  (:graph-width #f (- (gvl :width) (* (gvl :axis-x-margin) 2)))
  (:graph-right #f (+ (gvl :graph-left) (gvl :graph-width)))
  (:graph-top #f (+ (gvl :top) (gvl :axis-y-margin)))
  (:graph-height #f (- (gvl :height) (* (gvl :axis-y-margin) 2)))
  (:graph-bottom #f (+ (gvl :graph-top) (gvl  :graph-height)))

  (:tick-size 10)			;in pixels
  (:axis-x-margin 50)
  (:axis-y-margin 50)

  ;; specification for X axis
  (:draw-x-axis? t)
  (:x-tick-count 4)
  (:x-tick-size #f (gvl :tick-size))
  (:x-tick-format "~,2F")
  (:x-label "x axis")
  (:x-to-label #'(lambda (value) (format nil "~@?" (gvl :tick-format) value)))

  ;; and for Y
  (:draw-y-axis? t)
  (:y-tick-count 4)
  (:y-tick-size #f (gvl :tick-size))
  (:y-tick-format "~,2F")
  (:y-label "y axis")
  (:y-to-label #'(lambda (value) (format nil "~@?" (gvl :tick-format) value)))

  (:graph-subwindow-prototype graphing-window)
  (:graph-subwindow-agg-prototype opal:aggregate)
  (:graph-subwindow)

  (:parts
   `((:x-axis ,plotting-x-axis
	      (:label-string ,#f (gvl :parent :x-label))
	      (:tick-size ,#f (gvl :parent :x-tick-size))
	      (:tick-count ,#f (gvl :parent :x-tick-count))
	      (:tick-format ,#f (gvl :parent :x-tick-format))
	      (:w-to-label ,#f (gvl :parent :x-to-label)))
     (:y-axis ,plotting-y-axis
	      (:label-string ,#f (gvl :parent :y-label))
	      (:tick-size ,#f (gvl :parent :y-tick-size))
	      (:tick-count ,#f (gvl :parent :y-tick-count))
	      (:tick-format ,#f (gvl :parent :y-tick-format))
	      (:w-to-label ,#f (gvl :parent :y-to-label)))

     ))
  )

(define-method :initialize graphing-aggregate (self)
  (call-prototype-method self)
  (kr-send self :create-graph-subwindow self)
  (kr-send self :update-ticks self))


; It is safe to create the subwindow when the aggregate is created.
; But at initialize time, the container does not yet have a window,
; so the :parent formula yields NIL.  Thus although the subwindow 
; eventually does get a parent, the parent does not know the child.

(define-method :create-graph-subwindow graphing-aggregate (self)
  (let ((container self))
    (s-value self :graph-subwindow 
	     (create-instance nil (g-value self :graph-subwindow-prototype)
	       (:aggregate (create-instance nil
			       (g-value self :graph-subwindow-agg-prototype)))
	       (:container container)
	       (:parent #f (gvl :container :window))))))

(define-method :update-ticks graphing-aggregate (self)
  (kr-send (g-value self :x-axis :ticks)
	   :create-tick-marks (g-value self :x-axis :ticks))
  (kr-send (g-value self :y-axis :ticks)
	   :create-tick-marks (g-value self :y-axis :ticks)))




;;; Kludge Dept

; The subwindow knows its parent (because the formula tells it) but
; the parent does not know the child.  I think that's because when the
; window is created, its container does not yet have a window.  So when
; the aggregate DOES get a window, that would be a good time to set
; the window for the subwindow.  But I can't find out when that happens.
; Instead, I do it when the main window is updated.  

#| ; I tried to make the window assignment happen when the aggregate
; was updates, but this does not work because the update function for window
; calls update-method-aggregate directly, thus bypassing KR's inheritance
; mechanism.

(define-method :update graphing-aggregate (agg update-info
					       line-style-gc filling-style-gc
					       drawable root-window
					       clip-mask bbox-1 bbox-2
					       &optional (total-p NIL))
  (when (g-value agg :graph-subwindow)
    (add-self-to-parent (g-value agg :graph-subwindow)))
  (call-prototype-method agg update-info line-style-gc filling-style-gc
			 drawable root-window clip-mask bbox-1 bbox-2 total-p))

|#

; I define CLWIN elsewhere, this form is here for the sake of anyone
; who might want to use PLOTXY without the rest of CLWIN.

(export 'clwin)

(when (or (not (boundp 'clwin))
	  (not (schema-p clwin)))
  (create-instance 'clwin  inter:interactor-window))

(define-method :update clwin (a-window &optional (total-p NIL))
  (when (g-value a-window :aggregate)
    (ensure-subwindows-have-parents (g-value a-window :aggregate)))
  (call-prototype-method a-window total-p))

(defun ensure-subwindows-have-parents (aggregate)
  (when (and (g-value aggregate :graph-subwindow)
	     (kr:schema-p (g-value aggregate :graph-subwindow)))
    (add-self-to-parent (g-value aggregate :graph-subwindow)))
  (opal:do-components aggregate #'ensure-subwindows-have-parents))


(defun add-self-to-parent (a-window)
  (when (and (g-value a-window :parent)
	     (kr:schema-p (g-value a-window :parent))
	     (not (find a-window (g-value a-window :parent :child))))
    (warn "~A's parent does not know its child, adding it~%" a-window)
    (push a-window (g-value a-window :parent :child))))


;; This is a simple example of how to plot.

(defun plot-points (points &key window
			   (line-style opal:default-line-style))
  (if window
    (progn
      (s-value window :data-points points)
      (opal:update window)
      )
    (let* ((agg (create-instance nil graphing-aggregate))
	   (win (create-instance nil inter:interactor-window
		  (:title "Plot")
		  (:data-points points)
		  (:left 700)
		  (:top 100)
		  (:aggregate agg)
		  (:height (g-value agg :height))
		  (:width (g-value agg :width))
		  )))
      ;; the subwindow's parent does not know the child.
      ;; so we have to fix it.
      (when (null (g-value win :child))
	(opal:update win)
	(s-value win :child (list (g-value win :aggregate :graph-subwindow))))

      (opal:add-component
       (g-value win :aggregate :graph-subwindow :aggregate)
       (create-instance nil line-connected-data-points 
	 (:line-style line-style)
	 (:data-points #f (gvl :window :parent :window :data-points))))

      (kr-send (g-value win :aggregate) :update-ticks
	       (g-value win :aggregate))

      (opal:update win)
      win)))

(defun zoom (window &optional (factor 2))
  (let ((sub (g-value  window :aggregate :graph-subwindow)))
    (kr-send sub :set-scale sub (* (g-value sub :scale)  factor))))

(defun scroll-x (window &key (pixels  100))
  (let ((sub (g-value  window :aggregate :graph-subwindow)))
    (kr-send sub :set-origin
	     sub
	     (+ (g-value sub :origin-x) pixels)
	     (+ (g-value sub :origin-y) 0))))

    
#|
(setf www (plot-points  '((1 1) (2 2) (4 5)  (8 10) (10 9))))

(setf www
      (plot-points '((1 1) (2 2) (4 5)  (8 10) (10 9) (20 18) (21 17) (22 1)
		     (25 30) (30 30) (40 1) (41 0) (42 -5) (43 -10) (50 2) (55 10)
		     (100 20)
		     )
		   :line-style (create-instance nil opal:line-style
				 (:foreground-color opal:red)
				 (:line-style :dash)
				 (:line-thickness 5)
				 (:dash-pattern '(1 1 1 1 3 1)))))

(s-value www :aggregate :x-tick-format "~d")
(opal:update www)

(zoom www .25)
(zoom www 2)

(progn
  (dotimes (i 10)
    (scroll-x www :pixels -20)
    )
  (dotimes (i 10)
    (scroll-x www :pixels 20)
    (sleep .1)
    ))

(opal:destroy www)

||#
