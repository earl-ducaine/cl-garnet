;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;;  Gauge
;; 
;;   This gauge features:
;;      1)  Choice of a polygon needle or an arrowhead needle
;;      2)  Interim feedback of a short needle
;;      3)  Enumerated tic-marks around the perimeter of the gauge
;;      4)  Text feedback indicating current gauge position
;;      5)  The :value slot is the current chosen value and may be set
;;          directly.
;; 
;;   Customizable slots:
;;      1)  Top, Left, Radius
;;      2)  Polygon-needle-p
;;      3)  Int-feedback-p -- When T, a short needle follows the mouse
;;      4)  Num-marks  --  Number of marks around gauge, includes ends
;;      5)  Tic-marks-p -- Whether to put tic marks around gauge perimeter
;;      6)  Enumerate-p -- Whether to add numbers to the tic marks
;;      7)  Value-feedback-p -- Whether to numerically display the value
;;      8)  Text-offset -- The distance between the gauge and the text below
;;      9)  Enum-font -- Font in which to show perimeter values
;;     10)  Value-font -- Font in which to report current gauge value
;;     11)  Title-font -- Font for the title of the gauge
;;     12)  Title -- The label to appear under the gauge (NIL implies no title)
;;     13)  Value  --  The currently selected value
;;     14)  Selection-function -- Function called when :value changes
;;     15)  Val-1, Val-2 -- The range of :value and the tic marks.
;;                          Val-1 corresponds to the right side of the gauge.
;; 
;;   Demo:
;;     This module includes a function which demonstrates the circular gauge.
;;      To start, enter (GARNET-GADGETS:gauge-go).
;;      To quit, enter (GARNET-GADGETS:gauge-stop).
;; 
;;   Designed and written by Brad Myers
;;   Garnet Gadgets version written by Andrew Mickish


;;;  CHANGE LOG:
;;   05/31/93 Andrew Mickish - Added :outside-action to angle interactor
;;   05/28/93 Kosbie/Mickish - Reduced trigonometry required in :point-list
;;   05/27/93 Kosbie/Mickish - Added quite a few miscellaneous optimizations
;;   05/20/93 Andrew Mickish - Added :constants list to aggrelist
;;   05/11/93 David Kosbie - Fixed custom :start-where of rotate interactor
;;              to return object on success (instead of T, which was a bug)
;;   02/23/93 Andrew Mickish - Added :string-set-func
;;   01/25/93 Andrew Mickish - Added :format-string and :enum-format-string
;;   12/14/92 Andrew Mickish - Added type and parameter declarations
;;   09/17/92 Andrew Mickish - Added :height formula to GAUGE-VALUE-FEEDBACK to
;;              reduce recomputations
;;   04/30/92 Andrew Mickish - Called get-standard-font for :enum-font
;;   02/11/92 Andrew Mickish - Added :maybe-constant list
;;   04/19/91 Andrew Mickish - Formula :circle-width considers :enumerate-p
;;   02/07/91 Andrew Mickish - :Radius is no longer a supported settable slot.
;;              Now the :width should be set.
;;   01/24/91 Andrew Mickish - Removed Gauge-Clip-And-Map since
;;              inter:Clip-And-Map now works for real numbers
;;   06/16/90 Andrew Mickish - Changed formulas that compute :left and :top
;;              of tic-mark numbers.  Added :text-offset parameter and changed
;;              :top slots of GAUGE-TITLE and GAUGE-VALUE-FEEDBACK accordingly.
;;   01/25/90 Andrew Mickish - Added Gauge-Clip-And-Map,
;;              Changed :angle slots of NEEDLE1 and NEEDLE2,
;;              Removed clipping from :x2 and :y2 slots of INT-FEED-NEEDLE,
;;              Clipped :value slot of GAUGE-VALUE-FEEDBACK,
;;              Added :hit-threshold to SEMICIRCLE-OBJ and its components,
;;   12/05/89 ECP  Removed extra right parentheses.


(in-package "GARNET-GADGETS")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(Gauge))
  #+garnet-test
  (export '(Gauge-Go Gauge-Stop Gauge-Obj Gauge-Top-Agg Gauge-Win)))

(create-instance 'GAUGE-BASE-LINE opal:line
   (:constant '(:line-style))
   (:x1 (o-formula (gv (kr-path 0 :parent) :circle-left)))
   (:y1 (o-formula (gv (kr-path 0 :parent) :center-y)))
   (:x2 (o-formula (+ (gv (kr-path 0 :parent) :circle-left)
		      (gv (kr-path 0 :parent) :circle-width))))
   (:y2 (o-formula (gvl :y1))))

(create-instance 'GAUGE-SEMI-CIRC opal:arc
   (:left   (o-formula (gv (kr-path 0 :parent) :circle-left)))
   (:top    (o-formula (gv (kr-path 0 :parent) :circle-top)))
   (:width  (o-formula (gv (kr-path 0 :parent) :circle-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :circle-width)))
   (:angle1 0.0) (:angle2 gu:short-PI))

(create-instance 'GAUGE-TIC-MARKS opal:aggrelist
  (:constant :direction :h-spacing :v-spacing :indent :rank-margin
	     :pixel-margin :fixed-width-p :fixed-height-p :fixed-width-size
	     :fixed-height-size)
  (:left (o-formula (gv (kr-path 0 :parent) :left)))
  (:top (o-formula (gv (kr-path 0 :parent) :top)))
  (:items (o-formula (gv (kr-path 0 :parent) :num-marks)))
  (:direction NIL)
  (:item-prototype
   `(,opal:aggregadget
     (:perimeter-location ,(o-formula
			    (* (gvl :rank)
			       (/ gu:short-PI (- (gv (kr-path 0 :parent)
						  :items) 1)))))
     (:parts
      ((:MARK ,opal:line
	      (:constant (:line-style))
	      (:x1 ,(o-formula (let* ((p (kr-path 0 :parent :parent :parent))
				      (radius (gv p :radius)))
				 (+ (gv p :circle-left) radius
				    (round (* radius
					      (cos (gv (kr-path 1 :parent)
						       :perimeter-location))))))))
	      (:y1 ,(o-formula (let ((p (kr-path 0 :parent :parent :parent)))
				 (- (gv p :center-y)
				    (round (* (gv p :radius)
					      (sin (gv (kr-path 1 :parent)
						       :perimeter-location))))))))
	      (:x2 ,(o-formula (let* ((p (kr-path 0 :parent :parent :parent))
				      (radius (gv p :radius)))
				 (+ (gv p :circle-left) radius
				    (round (* .95 radius
					      (cos (gv (kr-path 2 :parent)
						       :perimeter-location))))))))
	      (:y2 ,(o-formula (let ((p (kr-path 0 :parent :parent :parent)))
				 (- (gv p :center-y)
				    (round (* .95 (gv p :radius)
					      (sin (gv (kr-path 1 :parent)
						       :perimeter-location))))))))
	      (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					(gv (kr-path 1 :parent :parent :parent)
					    :tic-marks-p)))))
       (:TEXT ,opal:text
	      (:constant (:actual-heightp))
	      (:left ,(o-formula
		       (let ((x1 (gv (kr-path 0 :parent :mark) :x1))
			     (loc (gv (kr-path 1 :parent) :perimeter-location)))
			 (if (> (* 2 (/ gu:short-PI 3)) loc (/ gu:short-PI 3))
			     (- x1 (round (gvl :width) 2))
			     (if (> loc (/ gu:short-PI 2))
				 (- x1 (gvl :width) 5)
				 (+ x1 5))))))
	      (:top ,(o-formula
		      (let ((loc (gv (kr-path 0 :parent) :perimeter-location)))
			(- (gv (kr-path 1 :parent :mark) :y1)
			   (gvl :height)
			   (if (> (* 2 (/ gu:short-PI 3)) loc (/ gu:short-PI 3)) 3 0)))))
	      (:tic-value
	       ,(o-formula (inter:Clip-and-Map
			    (gv (kr-path 0 :parent) :perimeter-location)
			    0 gu:short-PI
			    (gv (kr-path 1 :parent :parent :parent) :val-1)
			    (gv (kr-path 1 :parent :parent :parent) :val-2))))
	      (:string ,(o-formula
			 (format NIL (gv (kr-path 0 :parent :parent :parent)
					 :enum-format-string)
				 (gvl :tic-value))))
	      (:font ,(o-formula (gv (kr-path 0 :parent :parent :parent)
				     :enum-font)))
	      (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					(gv (kr-path 1 :parent :parent :parent)
					    :enumerate-p))))))))))

(create-instance 'GAUGE-NEEDLE1 opal:aggregadget
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (not (gv (kr-path 0 :parent) :polygon-needle-p)))))
  (:angle (o-formula (let ((p (kr-path 0 :parent)))
		       (inter:Clip-And-Map (gv p :value)
					   (gv p :val-1) (gv p :val-2)
					   0 gu:short-PI))))
  (:x2 (o-formula (+ (gv (kr-path 0 :parent) :center-x)
		     (round (* (gv (kr-path 0 :parent) :needle-length)
			       (cos (gvl :angle)))))))
  (:y2 (o-formula (- (gv (kr-path 0 :parent) :center-y)
		     (round (* (gv (kr-path 0 :parent) :needle-length)
			       (sin (gvl :angle)))))))
  (:parts
   `((:shaft ,opal:line
	     (:x1 ,(o-formula (gv (kr-path 0 :parent :parent) :center-x)))
	     (:y1 ,(o-formula (gv (kr-path 0 :parent :parent) :center-y)))
	     (:x2 ,(o-formula (gv (kr-path 0 :parent) :x2)))
	     (:y2 ,(o-formula (gv (kr-path 0 :parent) :y2)))
	     (:line-style ,opal:line-2))
     (:head ,opal:arrowhead
	    (:from-x ,(o-formula (gv (kr-path 0 :parent :parent) :center-x)))
	    (:from-y ,(o-formula (gv (kr-path 0 :parent :parent) :center-y)))
	    (:head-x ,(o-formula (gv (kr-path 0 :parent) :x2)))
	    (:head-y ,(o-formula (gv (kr-path 0 :parent) :y2)))
	    (:line-style ,opal:line-2)
	    (:length ,(o-formula (floor (gv (kr-path 0 :parent :parent)
					    :needle-length) 8)))
	    (:diameter ,(o-formula (floor (gv (kr-path 0 :parent :parent)
					      :needle-length) 7)))
	    (:open-p t)))))

(create-instance 'GAUGE-NEEDLE2 opal:polyline
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (gv (kr-path 0 :parent) :polygon-needle-p))))
  (:point-list
   (o-formula
    (let* ((p               (kr-path 0 :parent))
	   (angle           (gv p :angle))
	   (needle-length   (gv p :needle-length))
	   (inv-base-length (gv p :inv-base-length))
	   (sin-angle       (sin angle))
	   (cos-angle       (cos angle))
	   (x1              (gv p :center-x))
	   (y1              (gv p :center-y))
	   (delta-x2          (round sin-angle inv-base-length))
	   (delta-y2          (round (- cos-angle) inv-base-length))
	   (delta-x4          (+ delta-x2 delta-x2))
	   (delta-y4          (+ delta-y2 delta-y2))
	   (delta-x5          (round (* needle-length cos-angle)))
	   (delta-y5          (round (* needle-length sin-angle)))
	   (x5                (+ x1 delta-x5))
	   (y5                (- y1 delta-y5))
	   (mid-x             (+ x1 (round delta-x5 1.33)))
	   (mid-y             (- y1 (round delta-y5 1.33)))
	   )
      (list x1 y1 (+ x1 delta-x2)    (- y1 delta-y2)
	    (+ mid-x delta-x2) (- mid-y delta-y2)
	    (+ mid-x delta-x4) (- mid-y delta-y4)
	    x5 y5
	    (- mid-x delta-x4) (+ mid-y delta-y4)
	    (- mid-x delta-x2) (+ mid-y delta-y2)  
	    (- x1 delta-x2)    (+ y1 delta-y2)
	    x1 y1))))
  (:filling-style opal:gray-fill))


(create-instance 'GAUGE-INT-FEEDBACK opal:line
  (:needle-length (o-formula (floor (gv (kr-path 0 :parent)
					:needle-length) 2)))
  (:x1 (o-formula (gv (kr-path 0 :parent) :center-x)))
  (:y1 (o-formula (gv (kr-path 0 :parent) :center-y)))
  (:x2 (o-formula (+ (gvl :x1)
		     (round (* (gvl :needle-length)
			       (cos (gvl :angle)))))))
  (:y2 (o-formula (- (gvl :y1)
		     (round (* (gvl :needle-length)
			       (sin (gvl :angle)))))))
  (:line-style opal:line-2)
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (gvl :obj-over)))))


(create-instance 'GAUGE-TITLE opal:text
  (:constant '(:actual-heightp))
  (:left (o-formula (- (gv (kr-path 0 :parent) :center-x)
		       (floor (gvl :width) 2))))
  (:top (o-formula (+ (gv (kr-path 0 :parent) :text-offset)
		      (gv (kr-path 0 :parent) :center-y))))
  (:string (o-formula (or (gv (kr-path 0 :parent) :title)
			  "")))
  (:font (o-formula (gv (kr-path 0 :parent) :title-font)))
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (gv (kr-path 0 :parent) :title)))))


(create-instance 'GAUGE-VALUE-FEEDBACK opal:text
  (:constant '(:actual-heightp))
  (:left (o-formula (- (gv (kr-path 0 :parent) :center-x)
		       (floor (gvl :width) 2))))
  (:top (o-formula (let* ((p (kr-path 0 :parent)))
		     (+ (gv p :text-offset)
			(if (gv p :title)
			    (opal:gv-bottom
			     (kr-path 1 :parent :gauge-title))
			    (gv p :center-y))))))
  (:string 
   (o-formula
    (format NIL (gv (kr-path 0 :parent) :format-string)
	    (inter:clip-and-map
	     (gv (kr-path 0 :parent) :angle)
	     0 PI
	     (gv (kr-path 0 :parent) :val-1)
	     (gv (kr-path 0 :parent) :val-2)))))
  (:height (o-formula (opal:string-height (gvl :font) "0")))
  (:font (o-formula (gv (kr-path 0 :parent) :value-font)))
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (gv (kr-path 0 :parent) :value-feedback-p)))))

(defun GAUGE-FINAL-FN (interactor angle)
  (declare (ignore angle))
  (let ((gauge (g-value interactor :operates-on)))
    (kr-send gauge :selection-function
	     gauge (g-value gauge :value))))

(create-instance 'GAUGE opal:aggregadget
  :declare ((:parameters :left :top :width :val-1 :val-2 :num-marks
			 :tic-marks-p :enumerate-p :value-feedback-p
			 :polygon-needle-p :int-feedback-p :text-offset
			 :title :title-font :value-font :enum-font
			 :format-string :enum-format-string
			 :value :selection-function :visible)
	    (:type (number :val-1 :val-2 :value)
		   ((integer 2) :num-marks)
		   ((integer 0) :text-offset)
		   (kr-boolean :tic-marks-p :enumerate-p
			       :value-feedback-p :polygon-needle-p :int-feedback-p)
		   (string :format-string :enum-format-string)
		   ((or null string) :title)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :title-font :value-font :enum-font)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :polygon-needle-p
			     :int-feedback-p :title :title-font :value-font
			     :enum-font :num-marks :tic-marks-p :enumerate-p
			     :value-feedback-p :text-offset :val-1 :val-2
			     :format-string :enum-format-string
			     :visible))
		 
  ;; Customizable slots
  (:left 0) (:top 0)
  (:width 230)
  (:polygon-needle-p T)
  (:int-feedback-p T)
  (:title "Gauge")
  (:title-font opal:default-font)
  (:value-font opal:default-font)
  (:enum-font (opal:get-standard-font :fixed :roman :small))
  (:num-marks 10)			; Includes endpoints
  (:tic-marks-p T)
  (:enumerate-p T)
  (:value-feedback-p T)
  (:format-string "~a")
  (:enum-format-string "~a")
  (:text-offset 5)
  (:val-1 0)
  (:val-2 180)
  (:selection-function NIL)

;;;  Generally non-customizable slots

  ;; Slot set by angle interactor
  (:angle (o-formula
	   (inter:clip-and-map
	    (gvl :value)
	    (gvl :val-1) (gvl :val-2)
	    0 PI)
	   (/ gu:short-PI 3)))
  (:value (o-formula (inter:Clip-and-Map
		      (gvl :angle)
		      0 #-cmu PI #+cmu (coerce PI 'short-float)
		      (gvl :val-1) (gvl :val-2))
		     (/ gu:short-PI 3)))
  (:needle-length (o-formula (* (gvl :radius) .8)))
  (:inv-base-length (o-formula (/ 15.0 (gvl :needle-length))))
  (:val-1-width (o-formula (opal:string-width (gvl :enum-font)
					      (format NIL (gvl :enum-format-string)
						      (gvl :val-1)))))
  (:val-2-width (o-formula (opal:string-width (gvl :enum-font)
					      (format NIL (gvl :enum-format-string)
						      (gvl :val-2)))))
  (:enum-height (o-formula (opal:string-height (gvl :enum-font) "0")))
  (:circle-left (o-formula (if (gvl :enumerate-p)
			       (+ 5 (gvl :left) (gvl :val-2-width))
			       (gvl :left))))
  (:circle-top (o-formula (if (gvl :enumerate-p)
			      (+ 8 (gvl :top) (gvl :enum-height))
			      (gvl :top))))
  (:circle-width (o-formula (- (gvl :width)
			       (if (gvl :enumerate-p)
				   (+ (gvl :val-1-width)
				      (gvl :val-2-width)
				      (* 2 (gvl :text-offset)))
				   0))))
  (:radius (o-formula (round (gvl :circle-width) 2)))
  (:center-x (o-formula (+ (gvl :circle-left) (gvl :radius))))
  (:center-y (o-formula (+ (gvl :circle-top) (gvl :radius))))

  (:height (o-formula (- (if (gvl :value-feedback-p)
			     (opal:gv-bottom (gvl :value-feedback))
			     (if (gvl :title)
				 (opal:gv-bottom (gvl :gauge-title))
				 (+ (gvl :center-y)
				    ;; Consider extruding needle
				    (round (* (gvl :needle-length) .2)))))
			 (gvl :top))))

  (:parts
   `((:SEMI-CIRC ,gauge-semi-circ)
     (:BASE-LINE ,gauge-base-line)
     (:TIC-MARKS ,gauge-tic-marks)
     (:NEEDLE1 ,gauge-needle1)
     (:NEEDLE2 ,gauge-needle2)
     (:INT-FEEDBACK ,gauge-int-feedback)
     (:GAUGE-TITLE ,gauge-title)
     (:VALUE-FEEDBACK ,gauge-value-feedback)))

  (:interactors
   `((:ROTATE ,inter:angle-interactor 
	      (:window ,(o-formula (gv-local :self :operates-on :window)))
	      (:outside :last)
	      (:start-where
	       ,(o-formula
		 (list :custom
		       (gv-local :self :operates-on)
		       #'(lambda (gauge inter event)
			   (declare (ignore inter))
			   (let* ((mouse-x (inter:event-x event))
				  (mouse-y (inter:event-y event))
				  (center-x (g-value gauge :center-x))
				  (center-y (g-value gauge :center-y)))
			     (and (<= mouse-y center-y)
				  (< (sqrt (+ (expt (- center-x mouse-x) 2)
					      (expt (- center-y mouse-y) 2)))
				     (g-value gauge :radius))
				  gauge))))))
	      (:center-of-rotation
	       ,(o-formula (list (gv (kr-path 0 :operates-on) :center-x)
				 (gv (kr-path 0 :operates-on) :center-y))))
	      (:obj-to-change ,(o-formula (kr-path 0 :operates-on)))
	      (:feedback-obj
	       ,(o-formula (when (gv (kr-path 0 :operates-on) :int-feedback-p)
			     (gv (kr-path 0 :operates-on) :int-feedback))))
	      (:outside-action
	       ,#'(lambda (an-inter outside-control obj)
		    (declare (ignore outside-control))
		    (let* ((angle-obj (or (g-value an-inter :feedback-obj)
					  obj))
			   (new-angle (if (> (g-value angle-obj :angle) 1.5)
					  gu:short-PI 0)))
		      (s-value an-inter :saved-last-angle
			       (s-value angle-obj :angle new-angle)))))
	      (:running-action
	       ,#'(lambda (interactor obj angle delta)
		    (call-prototype-method interactor obj angle delta)
		    (let ((gauge (g-value interactor :operates-on)))
		      (when (not (g-value gauge :int-feedback-p))
			(kr-send gauge :selection-function
				 gauge (g-value gauge :value))))))
               (:final-function
                ,#'(lambda (interactor angle)
                     (declare (ignore angle))
                     (let ((gauge (g-value interactor :operates-on)))
                       (kr-send gauge :selection-function
                                gauge (g-value gauge :value))))))

     (:WHEEL ,inter:button-interactor
	     (:window ,(o-formula (gv-local :self :operates-on :window)))
	     (:continuous NIL)
	     (:obj-to-change ,(o-formula (kr-path 0 :operates-on)))
	     (:inc-by ,(o-formula (gv (kr-path 0 :parent) :scr-incr)))
	     (:start-where
	      ,(o-formula
		(list :custom
		      (gv-local :self :operates-on)
		      #'(lambda (gauge inter event)
			  (declare (ignore inter))
			  (let* ((mouse-x (inter:event-x event))
				 (mouse-y (inter:event-y event))
				 (center-x (g-value gauge :center-x))
				 (center-y (g-value gauge :center-y)))
			    (and (<= mouse-y center-y)
				 (< (+ (expt (- center-x mouse-x) 2)
				       (expt (- center-y mouse-y) 2))
				    (expt (g-value gauge :radius) 2))
				 gauge))))))
	     (:start-event (:upscrollup :downscrollup)) ;; Val-1 event, Val-2 event
	     (:final-function
	      ;; This is from MOTIF-KEY-TRILL-FN. It's copied here to avoid
	      ;; introducing weird dependencies.
	      ,#'(lambda (interactor obj)
		   (declare (ignore obj))
		   (declare (optimize (speed 2) (safety 3) (debug 3)))
		   (let* ((bar (g-value interactor :operates-on))
			  (value (g-value bar :value))
			  (val-1 (g-value bar :val-1))
			  (val-2 (g-value bar :val-2))
			  (inc-by (or (g-value bar :scr-incr) 5))
			  (up-or-left (first (g-value interactor :start-event))))
		     (if (eq (inter:event-char inter:*current-event*) up-or-left)
			 (if (< val-1 val-2)
			     (let ((thresh-val (+ val-1 inc-by)))
			       (if (> value thresh-val)
				   (s-value bar :value (- value inc-by))
				   (s-value bar :value val-1)))
			     (let ((thresh-val (- val-1 inc-by)))
			       (if (< value thresh-val)
				   (s-value bar :value (+ value inc-by))
				   (s-value bar :value val-1))))
			 (if (< val-1 val-2)
			     (let ((thresh-val (- val-2 inc-by)))
			       (if (< value thresh-val)
				   (s-value bar :value (+ value inc-by))
				   (s-value bar :value val-2)))
			     (let ((thresh-val (+ val-2 inc-by)))
			       (if (> value thresh-val)
				   (s-value bar :value (- value inc-by))
				   (s-value bar :value val-2)))))
		     (kr-send bar :selection-function bar (g-value bar :value)))))))))


(define-method :string-set-func GAUGE
  (gadget-obj str-obj final-event final-string)
  (declare (ignore final-event))
  (if (eq str-obj (g-value gadget-obj :gauge-title))
      ;; then is title
      (opal::set-one-value gadget-obj :title final-string)
      ;; else return NIL
      NIL))



;;;  DEMO FUNCTIONS
;;

#+garnet-test
(defun Gauge-Go ()
  (create-instance 'GAUGE-WIN inter:interactor-window
     (:title "Garnet Gauge")
     (:left 650) (:top 10) (:width 270) (:height 200))
  (s-value GAUGE-WIN
	   :aggregate
	   (create-instance 'GAUGE-TOP-AGG opal:aggregate))
  (create-instance 'GAUGE-OBJ GAUGE
     (:left 20) (:top 20)
     (:title "Pressure")
     )
  (opal:add-components GAUGE-TOP-AGG GAUGE-OBJ)
  (opal:update GAUGE-WIN))

#+garnet-test
(defun Gauge-Stop ()
  (opal:destroy GAUGE-WIN))
