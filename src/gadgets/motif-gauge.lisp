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


;;;  Motif-Gauge
;; 
;;   This gauge features:
;;      1)  Enumerated tic-marks around the perimeter of the gauge
;;      2)  Text feedback indicating current gauge position and title
;;      3)  The :value slot is the current chosen value and may be set
;;          directly.
;; 
;;   Customizable slots:
;;      1)  Left, Top, Width
;;      2)  Val-1, Val-2 -- The range of :value and the tic marks.
;;                          Val-1 corresponds to the right side of the gauge.
;;      3)  Scr-Incr -- The amount the value changes with arrow keys
;;      4)  Num-marks  --  Number of marks around gauge, includes endpoints
;;      5)  Tic-marks-p -- Whether to put tic marks around gauge perimeter
;;      6)  Enumerate-p -- Whether to add numbers to the tic marks
;;      7)  Value-feedback-p -- Whether to numerically display the value
;;      8)  Text-offset -- The distance between the gauge and the text below
;;      9)  Enum-font -- Font in which to show perimeter values
;;     10)  Value-font -- Font in which to report current gauge value
;;     11)  Title-font -- Font for the title of the gauge
;;     12)  Title -- The label to appear under the gauge (NIL implies no title)
;;     13)  Keyboard-Selection-P -- Whether to enable use of arrow keys
;;     14)  Foreground-Color
;;     15)  Value  --  The currently selected value
;;     16)  Selection-function -- Function called when :value changes
;; 
;;   Demo:
;;     This module includes a function which demonstrates the circular gauge.
;;      To start, enter (GARNET-GADGETS:motif-gauge-go).
;;      To quit, enter (GARNET-GADGETS:motif-gauge-stop).
;; 
;;   Written by Andrew Mickish


;;;  CHANGE LOG:
;; 
;;   02/01/94 Andrew Mickish - Added :active formula to interactor
;;   01/08/94 Andrew Mickish - short-PI ---> gu:short-PI
;;   05/31/93 Andrew Mickish - Added :outside-action to angle interactor
;;   05/27/93 Kosbie/Mickish - Added quite a few miscellaneous optimizations
;;   05/20/93 Andrew Mickish - Added constant declarations to the aggrelist;
;;              returned the gauge object from the custom :start-where
;;   03/03/93 Andrew Mickish - Added :string-set-func
;;   01/25/93 Andrew Mickish - Added :format-string and :enum-format-string
;;   12/15/92 Andrew Mickish - Added type and parameter declarations
;;   09/17/92 Andrew Mickish - Added :height formula to M-G-VALUE-FEEDBACK to
;;              reduce invalidations and recomputations
;;   02/11/92 Andrew Mickish - Added :maybe-constant list
;;   04/19/91 Andrew Mickish - Formula :circle-width considers :enumerate-p
;;   03/01/91 Andrew Mickish - Created


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Gauge))
  #+garnet-test
  (export '(Motif-Gauge-Go Motif-Gauge-Stop
	    Motif-Gauge-Win Motif-Gauge-Top-Agg Demo-Motif-Gauge)))

(defmacro sqr (x)
  (cond ((symbolp x) `(* ,x ,x))
	((numberp x) (* x x))
	(T (let ((x-sym (gensym)))
             `(let ((,x-sym ,x)) (* ,x-sym ,x-sym))))))

;; the angle at which the line styles flip
(defvar lstyle-flip-angle (* gu:short-PI .6))

(create-instance 'MOTIF-GAUGE-BASE-LINE opal:line
  (:x1 (o-formula (gv (kr-path 0 :parent) :circle-left)))
  (:y1 (o-formula (gv (kr-path 0 :parent) :center-y)))
  (:x2 (o-formula (let ((p (kr-path 0 :parent)))
		    (+ (gv p :circle-left)
		       (gv p :circle-width)))))
  (:y2 (o-formula (gvl :y1)))
  (:line-style (o-formula (gv (kr-path 0 :parent) :highlight-line-style))))

(create-instance 'MOTIF-GAUGE-SEMI-CIRC opal:arc
  (:left   (o-formula (gv (kr-path 0 :parent) :circle-left)))
  (:top    (o-formula (gv (kr-path 0 :parent) :circle-top)))
  (:width  (o-formula (gv (kr-path 0 :parent) :circle-width)))
  (:height (o-formula (gv (kr-path 0 :parent) :circle-width)))
  (:angle1 0.0)
  (:angle2 gu:short-PI)
  (:line-style (o-formula (gv (kr-path 0 :parent) :shadow-line-style)))
  (:filling-style (o-formula (gv (kr-path 0 :parent) :background-fill))))

(create-instance 'MOTIF-GAUGE-TIC-MARKS opal:aggrelist
  (:constant :direction :h-spacing :v-spacing :indent :rank-margin
	     :pixel-margin :fixed-width-p :fixed-height-p :fixed-width-size
	     :fixed-height-size)
  (:left (o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
  (:top (o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
  (:items (o-formula (gv (kr-path 0 :parent) :num-marks)))
  (:perimeter-offset (o-formula (/ gu:short-PI (1- (gvl :items)))))
  (:direction NIL)
  (:item-prototype
   `(,opal:aggregadget
     (:perimeter-location ,(o-formula (* (gvl :rank)
					 (gv (kr-path 0 :parent)
					     :perimeter-offset))))
     (:parts
      ((:MARK ,opal:line
	      (:constant (:line-style))
	      (:x1 ,(o-formula (let* ((p (kr-path 0 :parent :parent :parent))
				      (radius (gv p :radius)))
				 (+ (gv p :center-x)
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
				 (+ (gv p :center-x)
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
		       (let ((x1  (gv (kr-path 0 :parent :mark) :x1))
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
	      (:font ,(o-formula (gv (kr-path 0 :parent :parent :parent) :enum-font)))
	      (:visible ,(o-formula (if (gv (kr-path 0 :parent) :visible)
					(gv (kr-path 1 :parent :parent :parent)
					    :enumerate-p))))))))))

(create-instance 'MOTIF-GAUGE-NEEDLE1 opal:polyline
  (:point-list (o-formula (car (gv (kr-path 0 :parent) :point-lists))))
  (:line-style (o-formula
		(let ((p (kr-path 0 :parent)))
		  (if (< (gv p :angle) lstyle-flip-angle)
		      (gv p :shadow-line-style)
		      (gv p :highlight-line-style)))))
  (:filling-style (o-formula (gv (kr-path 0 :parent) :foreground-fill))))

(create-instance 'MOTIF-GAUGE-NEEDLE2 opal:polyline
  (:point-list (o-formula (cdr (gv (kr-path 0 :parent) :point-lists))))
  (:line-style (o-formula
		(let ((p (kr-path 0 :parent)))
		  (if (< (gv p :angle) lstyle-flip-angle)
		      (gv p :highlight-line-style)
		      (gv p :shadow-line-style)))))
  (:filling-style (o-formula (gv (kr-path 0 :parent) :foreground-fill))))

(create-instance 'MOTIF-GAUGE-TITLE opal:text
  (:constant '(:actual-heightp))
  (:left (o-formula (- (gv (kr-path 0 :parent) :center-x)
		       (ash (gvl :width) -1))))
  (:top (o-formula (let ((p (kr-path 0 :parent)))
		     (+ (gv p :text-offset)
			(gv p :center-y)))))
  (:string (o-formula (or (gv (kr-path 0 :parent) :title) "")))
  (:font (o-formula (gv (kr-path 0 :parent) :title-font)))
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (gvl :string)))))

(create-instance 'MOTIF-GAUGE-VALUE-FEEDBACK opal:text
  (:left (o-formula (- (gv (kr-path 0 :parent) :center-x)
		       (ash (gvl :width) -1))))
  (:top (o-formula (let* ((p (kr-path 0 :parent)))
		     (+ (gv p :text-offset)
			(if (gv p :title)
			    (opal:gv-bottom
			     (kr-path 1 :parent :gauge-title))
			    (gv p :center-y))))))
  (:string (o-formula
	    (let ((p (kr-path 0 :parent)))
              (format NIL (gv p :format-string)
		      (inter:clip-and-map
		       (gv p :angle)
		       0 gu:short-PI
		       (gv p :val-1)
		       (gv p :val-2))))))
  (:height  (o-formula (opal:string-height (gvl :font) "0")))
  (:font    (o-formula (gv (kr-path 0 :parent) :value-font)))
  (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			   (gv (kr-path 0 :parent) :value-feedback-p)))))

(create-instance 'MOTIF-GAUGE MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :width :val-1 :val-2 :scr-incr :title
			 :foreground-color :title-font :value-font :enum-font
			 :num-marks :tic-marks-p :enumerate-p :value-feedback-p
			 :format-string :enum-format-string
			 :keyboard-selection-p :text-offset :value
			 :selection-function :visible)
	    (:type (number :val-1 :val-2 :scr-incr :value)
		   ((integer 0) :num-marks)
		   (integer :text-offset)
		   ((or null string) :title)
		   ((is-a-p opal:color) :foreground-color)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :title-font :value-font :enum-font)
		   (string :format-string :enum-format-string)
		   (kr-boolean :tic-marks-p :enumerate-p :value-feedback-p
			       :keyboard-selection-p)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :title :foreground-color
			     :title-font :value-font :enum-font :num-marks
			     :tic-marks-p :enumerate-p :value-feedback-p
			     :format-string :enum-format-string
			     :text-offset :val-1 :val-2 :scr-incr :visible))

  ;; Customizable slots
  (:left 0) (:top 0)
  (:width 230)
  (:title "Motif Gauge")
  (:foreground-color opal:MOTIF-GRAY)
  (:title-font opal:default-font)
  (:value-font opal:default-font)
  (:enum-font (opal:get-standard-font NIL NIL :small))
  (:num-marks 10)			; Includes endpoints
  (:tic-marks-p T)
  (:enumerate-p T)
  (:value-feedback-p T)
  (:format-string "~a")
  (:enum-format-string "~a")
  (:text-offset 5)
  (:val-1 0)
  (:val-2 180)
  (:scr-incr 5)
  (:keyboard-selection-p NIL)
  (:value (o-formula (inter:Clip-and-Map (gvl :angle)
					 0 gu:short-PI
					 (gvl :val-1) (gvl :val-2))
		     (/ gu:short-PI 3)))
  (:selection-function NIL)

					; Generally non-customizable slots
  ;; Slot set by angle interactor
  (:angle (o-formula
	   (inter:clip-and-map
	    (gvl :value)
	    (gvl :val-1) (gvl :val-2)
	    0 gu:short-PI)
	   (/ gu:short-PI 3)))
  (:needle-length   (o-formula (round (gvl :radius) 1.25)))
  (:inv-base-length (o-formula (/ 15.0 (gvl :needle-length))))
  (:val-1-width (o-formula (opal:string-width (gvl :enum-font)
					      (format NIL (gvl :enum-format-string)
						      (gvl :val-1)))))
  (:val-2-width (o-formula (opal:string-width (gvl :enum-font)
					      (format NIL (gvl :enum-format-string)
						      (gvl :val-2)))))
  (:enum-height (o-formula (opal:string-height (gvl :enum-font) "0")))
  (:circle-left (o-formula (+ 2 (if (gvl :enumerate-p)
				    (+ 5 (gvl :left) (gvl :val-2-width))
				    (gvl :left)))))
  (:circle-top (o-formula (+ 2 (if (gvl :enumerate-p)
				   (+ 8 (gvl :top) (gvl :enum-height))
				   (gvl :top)))))
  (:circle-width (o-formula (- (gvl :width) 4
			       (if (gvl :enumerate-p)
				   (+ (gvl :val-1-width)
				      (gvl :val-2-width)
				      (* 2 (gvl :text-offset)))
				   0))))
  (:radius (o-formula (round (gvl :circle-width) 2)))
  (:center-x (o-formula (+ (gvl :circle-left) (gvl :radius))))
  (:center-y (o-formula (+ (gvl :circle-top) (gvl :radius))))
  (:height (o-formula (+ 4 (- (if (gvl :value-feedback-p)
				  (opal:gv-bottom (gvl :value-feedback))
				  (if (gvl :title)
				      (opal:gv-bottom (gvl :gauge-title))
				      (+ (gvl :center-y)
					 ;; Consider extruding needle
					 (round (gvl :needle-length) 5))))
			      (gvl :top)))))
  (:active-p T)
  (:point-lists
   (o-formula
    (let* ((angle             (gvl :angle))
	   (needle-length     (gvl :needle-length))
	   (x1                (gvl :center-x))
	   (y1                (gvl :center-y))
	   (inv-base-length   (gvl :inv-base-length))
	   (cos-angle         (cos angle))
	   (sin-angle         (sin angle))
	   (delta-x2          (round sin-angle inv-base-length))
	   (delta-y2          (round (- cos-angle) inv-base-length))
	   (delta-x4          (+ delta-x2 delta-x2))
	   (delta-y4          (+ delta-y2 delta-y2))
	   (delta-x5          (round (* needle-length cos-angle)))
	   (delta-y5          (round (* needle-length sin-angle)))
	   (x5                (+ x1 delta-x5))
	   (y5                (- y1 delta-y5))
	   (mid-x             (+ x1 (round delta-x5 1.33)))
	   (mid-y             (- y1 (round delta-y5 1.33))))
      (cons
       (list x1 y1
	     (+ x1 delta-x2)
	     (- y1 delta-y2)
	     (+ mid-x delta-x2)
	     (- mid-y delta-y2)
	     (+ mid-x delta-x4)
	     (- mid-y delta-y4)
	     x5 y5)
       (list x5 y5
	     (- mid-x delta-x4)
	     (+ mid-y delta-y4)
	     (- mid-x delta-x2)
	     (+ mid-y delta-y2)
	     (- x1 delta-x2)
	     (+ y1 delta-y2)
	     x1 y1)))))
  (:parts
   `((:BASE-LINE ,motif-gauge-base-line)
     (:SEMI-CIRC ,motif-gauge-semi-circ)
     (:TIC-MARKS ,motif-gauge-tic-marks)
     (:NEEDLE1 ,motif-gauge-needle1)
     (:NEEDLE2 ,motif-gauge-needle2)
     (:GAUGE-TITLE ,motif-gauge-title)
     (:VALUE-FEEDBACK ,motif-gauge-value-feedback)
     (:SEL-BOX ,MOTIF-SELECTION-BOX
	       (:obj-over ,(o-formula (gvl :parent))))))
  (:interactors
   `((:ROTATE ,inter:angle-interactor
	      (:active ,(o-formula (let ((p (gv-local :self :operates-on)))
				     (and (gv-local p :window)
					  (gv-local p :visible)
					  (gv p :active-p)))))
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
				  (< (+ (sqr (- center-x mouse-x))
					(sqr (- center-y mouse-y)))
				     (sqr (g-value gauge :radius)))
				  gauge))))))
	      (:center-of-rotation
	       ,(o-formula (list (gv (kr-path 0 :operates-on) :center-x)
				 (gv (kr-path 0 :operates-on) :center-y))))
	      (:obj-to-change ,(o-formula (kr-path 0 :operates-on)))
	      (:outside-action
	       ,#'(lambda (an-inter outside-control obj)
		    (declare (ignore outside-control))
		    (let ((new-angle (if (> (g-value obj :angle) 1.5)
					 gu:short-PI 0)))
		      (s-value an-inter :saved-last-angle
			       (s-value obj :angle new-angle)))))
	      (:running-action
	       ,#'(lambda (interactor obj angle delta)
		    (call-prototype-method interactor obj angle delta)
		    (let ((gauge (g-value interactor :operates-on)))
		      (kr-send gauge :selection-function
			       gauge (g-value gauge :value)))))
	      (:final-function
	       ,#'(lambda (interactor angle)
		    (declare (ignore angle))
		    (let ((gauge (g-value interactor :operates-on)))
		      (kr-send gauge :selection-function
			       gauge (g-value gauge :value))))))

     ;; WHEEL is like KEY only it uses the scroll wheel.
     ;; It also operates in the bounding box of the scroll bar.
     (:WHEEL ,inter:button-interactor
	     (:active ,(o-formula (let ((p (gv-local :self :operates-on)))
				    (and (gv-local p :window)
					 (gv-local p :visible)
					 (gv p :active-p)))))
	     (:window ,(o-formula (gv-local :self :operates-on :window)))
	     (:continuous NIL)
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
				 (< (+ (sqr (- center-x mouse-x))
				       (sqr (- center-y mouse-y)))
				    (sqr (g-value gauge :radius)))
				 gauge))))))
	     (:start-event (:upscrollup :downscrollup)) ;; Val-1 event, Val-2 event
	     (:final-function MOTIF-KEY-TRILL-FN))

     (:KEY ,inter:button-interactor
	   (:active ,(o-formula (and (gvl :window)
				     (gv (kr-path 0 :operates-on)
					 :keyboard-selection-p))))
	   (:window ,(o-formula (gv-local :self :operates-on :window)))
	   (:continuous NIL)
	   (:start-where T)
	   (:start-event (:rightarrow :leftarrow)) ;; Val-1 event, Val-2 event
	   (:final-function MOTIF-KEY-TRILL-FN)))))

(define-method :string-set-func MOTIF-GAUGE
  (gadget-obj str-obj final-event final-string)
  (declare (ignore final-event))
  (if (eq str-obj (g-value gadget-obj :gauge-title))
					; then is title
      (opal::set-one-value gadget-obj :title final-string)
					; else return NIL
      NIL))


;;;  DEMO FUNCTIONS
;;

#+garnet-test
(defun Motif-Gauge-Go (&key dont-enter-main-event-loop not-double-buffered-p)
  (create-instance 'MOTIF-GAUGE-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Gauge")
     (:left 800) (:top 10) (:width 280) (:height 200))
  (s-value MOTIF-GAUGE-WIN
	   :aggregate
	   (create-instance 'MOTIF-GAUGE-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-GAUGE MOTIF-GAUGE
     (:left 20) (:top 20)
     (:title "Temperature")
     (:keyboard-selection-p T))
  (opal:add-components MOTIF-GAUGE-TOP-AGG
		       (create-instance NIL MOTIF-BACKGROUND)
		       DEMO-MOTIF-GAUGE)
  (opal:update MOTIF-GAUGE-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop)))


#+garnet-test
(defun Motif-Gauge-Stop ()
  (opal:destroy MOTIF-GAUGE-WIN))
