;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; Changes:
;;  02-Dec-94 Mickish -- Removed stippled-p parameter from gem:draw-image
;;  21-Feb-94 Mickish -- Use diameter instead of width and height for circles
;;  23-Aug-93 Mickish -- Changed hit-threshold bindings in point-in-gob
;;                methods to conform to documented specifications
;;  30-Jun-93 Mickish -- Moved text stuff to cursor-text.lisp
;;   6-Apr-93 koz     -- Converted with-*-styles macros to set-*-style fns
;;                And omitted "clip-mask" as argument to draw function.
;;  23-Feb-93 Mickish -- Added :string-set-func for opal:text
;;  18-Jan-93 Mickish -- Added char-width
;;  13-Jan-93 Mickish -- Optimized string-width for fixed-width fonts;
;;                Now string-height uses :xfont and :font-height slots of font
;;   5-Oct-92 koz/Mickish -- get-thickness --> get-old-thickness in :draw
;;                methods
;;  28-Sep-92 Mickish -- Now string-width and string-height handle multiple
;;                line strings
;;  07-Aug-92 Mickish/Kutty -- Renamed Between to Between-Polyline-Points
;;  27-Aug-92 Landay -- Added wrap-around case to Point-In-Arc
;;  10-Aug-92 Landay,Mickish -- Added :point-in-gob method for opal:arc
;;  10-Aug-92 Mickish -- Rewrote arc :draw method to stay inside bbox
;;  10-Jul-92 Salisbury,ecp -- wrote :point-in-gob for opal:bitmap
;; 		  that only returns true if the cursor is over a
;; 		  foreground pixel of the bitmap (when :select-outline-only
;; 		  is non-NIL).
;;  29-Jun-92 ecp Fixed :point-in-gob of circles to use real center of
;; 		  circle, not center-x and center-y.
;;  11-Jun-92 ecp Wrote :point-in-gob method for polylines.
;;   2-Jun-92 ecp The value of an :is-a slot must be a list, not just an atom.
;; 		  So :rotate of rectangle wasn't working.
;;  15-Apr-92 ecp The with-lne-styles macro should take only variables for
;; 		  arguments (except first argument).
;;  10-Apr-92 ecp Do not draw polyline or multipoint unless point list is
;;                non-NIL.
;;  10-Apr-92 amickish  In :destroy-me method of View-Object, destroy the
;;                "known-as" slot in the parent aggregadget.
;;  25-Mar-92 amickish  Get-Local-Values ---> Get-Local-Value
;;  16-Mar-92 dzg,amickish  Removed redundant initialize methods for opal:line,
;;                opal:bitmap, and opal:circle.
;;  27-Feb-92 szekely In :destroy method of view-object, do not do a
;; 			copy-list of the instances.
;;  20-Feb-92 ecp When drawing a rectangle, oval, or circle that is smaller
;; 		  than its line-width, use x-draw-fn rather than boole-1
;;  17-Oct-91 ecp Fixed another minor bug introduced 16-Aug-91!
;;  29-Aug-91 ecp Fixed minor bug introduced 16-Aug-91.
;;  16-Aug-91 ecp Draw method of bitmap uses xlib:draw-rectangle if
;; 		:fill-style of :filling-style is :stipple.
;;   1-Aug-91 ecp Draw method of bitmap uses filling-style, not line-style.
;;  25-Mar-91 ecp Changed get-values -> get-local-values for :components.
;;   4-Mar-91 d'souza Removed nickname "MO" of package Opal.
;;  25-Oct-90 ecp Made get-cursor-index more robust.
;;   7-Aug-90 ecp In draw method of bitmap, reverted to doing a put-image.
;;  11-Jul-90 ecp new :destroy-me method
;;  26-Jun-90 ecp Changed 0 to 0.0 in draw-arc due to temporary xbug.
;;   1-May-90 ecp Only draw bitmap if :image is not NIL.
;;  16-Apr-90 ecp Moved center-x, center-y from basics.lisp to objects.lisp
;;  12-Apr-90 ecp  When rotating a rectangle (which turns it into a polyline)
;; 		   I must reset the :top, :left, :width, :height,
;; 		   :update-slots, and :update-slots-values slots.
;; 		   Rotating anything by an angle of 0 is a noop.
;;  28-Mar-90 ecp  New slot :already-tried-to-destroy added to objects
;; 		   to avoid destroying twice.
;;  23-Mar-90 ecp  New slot :fill-background-p for text objects.
;;  19-Mar-90 ecp  Changed tile to stipple.
;;  14-Mar-90 ecp  Get-index much more accurate.
;;  13-Feb-90 ecp  Finally merged objects.lisp with the
;; 		   "temporary" file eds-objects.lisp
;;   5-Dec-89 ecp  Removed a declare from draw-method for rectangle.
;; 


(in-package "OPAL")

;; This is called by the destroy methods.  It will carefully erase an object,
;; or return NIL if it could not do so (some values were illegal, etc..)
;; Note:  object is not *actually* erased, but its bbox is simply added to
;; its enclosing window's bbox -- you must call (update <that-window>)
(defun carefully-erase (object the-window)
 (let* ((update-info (g-local-value object :update-info))
        (old-bbox    (if (update-info-p update-info)
                          (update-info-old-bbox update-info)))
        (object-erased T)
        window-bbox win-uinfo)
  (if the-window
      (cond ((not (bbox-p old-bbox))       ; if bbox isn't there, ie,something
	     (setq object-erased NIL))	   ; broke, then we can't erase it!
            ((not (bbox-valid-p old-bbox)) ; If it wasn't visible, do nothing
	     NIL)
	    ;; now check if all entries are numbers
            ((not (and (numberp (bbox-x1 old-bbox))
                       (numberp (bbox-y1 old-bbox))
                       (numberp (bbox-x2 old-bbox))
                       (numberp (bbox-y2 old-bbox))))
	     (setq object-erased NIL))	; if not, couldn't erase it
	    ;; now make sure window's old-bbox is
	    ;; not destroyed...
            ((or (not (update-info-p (setq win-uinfo
					   (g-local-value the-window
                                                          :update-info))))
                 (not (bbox-p (setq window-bbox
				    (update-info-old-bbox win-uinfo)))))
	     (setq object-erased NIL))	; if not, couldn't erase it
	    ;; Finally, we know we can erase it!
            (T
	     (merge-bbox window-bbox old-bbox)))
    (setq object-erased NIL))		; No window, so couldn't erase it!
  object-erased))

(define-method :destroy-me opal:view-object (object &optional (top-level-p T))
 (if object
  (let* ((the-window (g-value object :window))
	 (parent  (g-local-value object :parent))
	 (erase-p (and top-level-p the-window parent
		    (not (g-local-value object :already-tried-to-destroy)))))
    (if (and top-level-p parent)
	(let ((known-as (g-local-value object :known-as)))
	  (s-value parent :components
		   (delete object (g-local-value parent :components)))
	  (mark-as-changed parent :components)
	  (if known-as (destroy-slot parent known-as))))
    (s-value object :already-tried-to-destroy t)
    (if erase-p
	(update the-window (not (carefully-erase object the-window))))
    (destroy-schema object))))

(define-method :destroy opal:view-object (object &optional (top-level-p T))
  (dolist (instance (g-local-value object :is-a-inv))
    (destroy instance top-level-p))
  (destroy-me object top-level-p))

(define-method :draw opal:line (gob a-window)
  (let* ((update-vals  (g-local-value gob :update-slots-values))
	 (lstyle (aref update-vals +line-lstyle+)))
    (if lstyle
      (gem:draw-line a-window
		     (aref update-vals +line-x1+)
		     (aref update-vals +line-y1+)
		     (aref update-vals +line-x2+)
		     (aref update-vals +line-y2+)
		     (aref update-vals +line-draw-function+)
		     lstyle))))


;;; Calculate approximate distance to the line by using similar triangles
;;; to calculate the point on the horizontal (or vertical) that the query
;;; point shares for mostly vertical (or horizontal) lines.
;;; 
(define-method :point-in-gob opal:line (gob x y)
 (and (g-value gob :visible)
  (let ((x1 (g-value gob :x1))
	(x2 (g-value gob :x2))
	(y1 (g-value gob :y1))
	(y2 (g-value gob :y2))
	(threshold (max (g-value gob :hit-threshold)
			(ceiling (get-thickness gob) 2))))
    (when (and (<= (- (min x1 x2) threshold) x (+ (max x1 x2) threshold))
	       (<= (- (min y1 y2) threshold) y (+ (max y1 y2) threshold)))
      (let* ((a (- y1 y2))                 ; equation for line is
	     (b (- x2 x1))                 ;  ax + by + c = 0
	     (c (- (* x1 y2) (* x2 y1)))
	     (d (+ (* a x) (* b y) c)))    ; d/sqrt(a^2+b^2) is the distance
	(<= (* d d)                        ; between line and point <x,y>
	    (* threshold threshold (+ (* a a) (* b b)))))))))

;;; The following functions allow access and setting to the gobs center
;;; position.

(defun center-x (gob)
  (+ (g-value gob :left) (truncate (g-value gob :width) 2)))

(defun center-y (gob)
  (+ (g-value gob :top) (truncate (g-value gob :height) 2)))

(define-method :rotate opal:line (gob angle &optional (center-x (center-x gob))
				(center-y (center-y gob)))
 (unless (zerop angle)
  (let* ((x1 (g-value gob :x1))
	 (x2 (g-value gob :x2))
	 (y1 (g-value gob :y1))
	 (y2 (g-value gob :y2))
	 (rx1 (- x1 center-x))
	 (ry1 (- y1 center-y))
	 (rx2 (- x2 center-x))
	 (ry2 (- y2 center-y))
	 (cos-angle (cos angle))
	 (sin-angle (sin angle)))
    (setf (g-value gob :x1)
	  (round (+ center-x (* rx1 cos-angle) (* -1 ry1 sin-angle))))
    (setf (g-value gob :y1)
	  (round (+ center-y (* ry1 cos-angle) (* rx1 sin-angle))))
    (setf (g-value gob :x2)
	  (round (+ center-x (* rx2 cos-angle) (* -1 ry2 sin-angle))))
    (setf (g-value gob :y2)
	  (round (+ center-y (* ry2 cos-angle) (* rx2 sin-angle)))))))



;;; Rectangles
(define-method :draw opal:rectangle (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (width (aref update-vals opal::+rect-width+))
	 (height (aref update-vals opal::+rect-height+))
	 (min-width-height (min width height))
	 (fstyle (aref update-vals opal::+rect-fstyle+))
	 (lstyle (aref update-vals opal::+rect-lstyle+))
	 (thickness (opal::get-old-thickness gob opal::+rect-lstyle+
					     update-vals)))
    (when (plusp min-width-height)	; only draw if width, height > 0
      (if (>= (* 2 thickness) min-width-height) ; if rectangle too small,
					; just draw solid rectangle
	(setf lstyle nil
	      fstyle opal:black-fill))
      (gem:draw-rectangle a-window
			  (aref update-vals opal::+rect-left+)
			  (aref update-vals opal::+rect-top+)
			  width height
			  (aref update-vals opal::+rect-draw-function+)
			  lstyle fstyle))))



(define-method :point-in-gob opal:rectangle (gob x y)
 (and (g-value gob :visible)
  (let* ((thickness (get-thickness gob))
	 (width (g-value gob :width))
	 (height (g-value gob :height))
	 (select-outline-only (g-value gob :select-outline-only))
	 (threshold (g-value gob :hit-threshold))
	 (t+t (+ thickness threshold))
	 (left (g-value gob :left))
	 (top (g-value gob :top))
	 (right (+ left width))
	 (bottom (+ top height)))
    (and (point-in-rectangle x y (- left threshold) (- top threshold)
			     (+ right threshold) (+ bottom threshold))
	 (not (and select-outline-only
		   (point-in-rectangle x y
				       (+ left t+t)
				       (+ top t+t)
				       (- right t+t)
				       (- bottom t+t))))))))

;;; The rotate method for rectangles has the sometimes nasty side effect of
;;; turning the rectangle into a polygon.

(define-method :rotate opal:rectangle (gob angle &optional
					   (center-x (center-x gob))
					   (center-y (center-y gob)))
  (unless (zerop angle)
    (let* ((top (g-value gob :top))
	   (left (g-value gob :left))
	   (right (+ left (g-value gob :width)))
	   (bottom (+ top (g-value gob :height))))
      ; convert into polyline and build point list.
      (s-value gob :is-a (list opal:polyline))
      (s-value gob :point-list 
	       (list left bottom right bottom right top left top left bottom))
      ; rebuild :top, :left, :width, :height slots
      (dolist (slot '(:top :left :width :height))
	(kr:destroy-slot gob slot))
;;    (kr::copy-down-formulas gob)	
      ; rebuild :update-slots and :update-slots-values slots
      (s-value gob :update-slots (g-value opal:polyline :update-slots))
      (s-value gob :update-slots-values nil)
      ; do the actual rotation
      (rotate gob angle center-x center-y))))


(defun point-in-ellipse (x y cx cy rx ry)
; Tells whether point <x,y> lies in ellipse with center <cx,cy>,
; horizontal radius rx and vertical radius ry
  (and (> rx 0)
       (> ry 0)
       (let ((dx (- cx x))
	     (dy (- cy y)))
	 (< (+ (* rx rx dy dy) (* ry ry dx dx)) (* rx rx ry ry)))))

;;; Multipoint objects
;;; 

;;; For a raw multipoint, just draw the points, all unimplemented
;;; multipoints inherit this method.
;;; 
(define-method :draw opal:multipoint (gob a-window)
  (let* ((update-vals  (g-local-value gob :update-slots-values))
	 (point-list (aref update-vals +multi-point-list+)))
    (when point-list
      (gem:draw-points a-window point-list
		       (aref update-vals +multi-draw-function+)
		       (aref update-vals +multi-lstyle+)))))


(define-method :rotate opal:multipoint (gob angle &optional
					    (center-x (center-x gob))
					    (center-y (center-y gob)))
  "rotates a multipoint object about (center-x,center-y) by angle radians"
  (unless (zerop angle)
    (let ((sin-angle (sin angle))
	  (cos-angle (cos angle)))
      (do ((point (g-value gob :point-list) (cddr point)))
	  ((null point) (mark-as-changed gob :point-list))
	(let ((rx (- (car point) center-x))
	      (ry (- (cadr point) center-y)))
	  (setf (car point)
		(round (+ center-x (* rx cos-angle) (* -1 ry sin-angle))))
	  (setf (cadr point)
		(round (+ center-y (* ry cos-angle) (* rx sin-angle)))))))))


;;; Polyline objects
;;; 

(define-method :draw opal:polyline (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (point-list (aref update-vals +polyline-point-list+)))
    (when point-list
      (gem:draw-lines a-window point-list
		      (aref update-vals +polyline-draw-function+)
		      (aref update-vals +polyline-lstyle+)
		      (aref update-vals +polyline-fstyle+)))))


;;; Returns T if point <x2,y2> is within distance "threshold" of the line
;;; segment with endpoints <x1,y1> and <x3,y3>.
(defun between-polyline-points (x1 y1 x2 y2 x3 y3 threshold)
  (when (and (<= (- (min x1 x3) threshold) x2 (+ (max x1 x3) threshold))
	     (<= (- (min y1 y3) threshold) y2 (+ (max y1 y3) threshold)))
    (let* ((a (- y1 y3))                 ; equation for line is
	   (b (- x3 x1))                 ;  ax + by + c = 0
	   (c (- (* x1 y3) (* x3 y1)))
	   (d (+ (* a x2) (* b y2) c)))   ; d/sqrt(a^2+b^2) is the distance
      (<= (* d d)                         ; between line and point <x,y>
	  (* threshold threshold (+ (* a a) (* b b)))))))


;;; Returns non-zero if the line segment with endpoints <x1,y1> and <x3,y3>
;;; crosses the ray pointing to the right of <x2,y2>.
(defun crosses-to-right-of (x1 y1 x2 y2 x3 y3)
  (cond ((and (< y1 y2 y3)
	      (< (* (- x3 x2) (- y1 y2)) (* (- x1 x2) (- y3 y2))))
	 1)
	((and (< y3 y2 y1)
	      (< (* (- x1 x2) (- y3 y2)) (* (- x3 x2) (- y1 y2))))
	 -1)
	(t 0)))

;;; Returns T if point <x,y> is inside, or within distance "threshold", of
;;; polyline containing vertices "points".
(defun point-in-polyline (x y points threshold outline-only full-interior)
  (let ((crossings 0))
    (do ((ptr points (cddr ptr)))
	((null ptr))
      ;; return T if P is near an edge.
      (when (between-polyline-points (first ptr) (second ptr)
		     x y
		     (or (third ptr) (first points))
		     (or (fourth ptr) (second points))
	             threshold)
        (return-from point-in-polyline T))
      (unless outline-only
	(incf crossings
	  (crosses-to-right-of (first ptr) (second ptr)
			       x y
			       (or (third ptr) (first points))
			       (or (fourth ptr) (second points))))))
    (if outline-only
	nil
	(if full-interior
            (not (zerop crossings))
	    (oddp crossings)))))
  

(define-method :point-in-gob opal:polyline (gob x y)
  (and (g-value gob :visible)
    (point-in-polyline x y
		       (g-value gob :point-list)
		       (max (g-value gob :hit-threshold)
                            (ceiling (get-thickness gob) 2))
		       (g-value gob :select-outline-only)
		       (g-value gob :hit-full-interior-p))))



;;; Bitmaps

(define-method :draw opal:bitmap (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (image (aref update-vals +bm-image+)))
    (when image
      (multiple-value-bind (width height)
	  (gem:image-size a-window image)
	(gem:draw-image a-window
			(aref update-vals +bm-left+)
			(aref update-vals +bm-top+)
			width height
			image
			(aref update-vals +bm-draw-function+)
			(aref update-vals +bm-fstyle+))))))


(defun image-bit-on? (image x y root-window)
  (multiple-value-bind (width height)
      (gem:image-size root-window image)
    (unless (or (null image)
		(< x 0) (< y 0)
		(>= x width)
		(>= y height))
      (gem:image-bit root-window image x y))))


(define-method :point-in-gob opal:bitmap (gob x y)
  (if (g-value gob :select-outline-only)
    (and (g-value gob :visible)
	 (image-bit-on? (g-value gob :image)
			(- x (g-value gob :left))
			(- y (g-value gob :top))
			(g-value gob :window)))
    (point-in-gob-method-view-object gob x y)))


;;; Arcs
(define-method :draw opal:arc (gob a-window)
  (let ((update-vals (g-local-value gob :update-slots-values)))
    (gem:draw-arc a-window
		  (aref update-vals opal::+arc-left+)
		  (aref update-vals opal::+arc-top+)
		  (aref update-vals opal::+arc-width+)
		  (aref update-vals opal::+arc-height+)
		  (aref update-vals opal::+arc-angle1+)
		  (aref update-vals opal::+arc-angle2+)
		  (aref update-vals opal::+arc-draw-function+)
		  (aref update-vals opal::+arc-lstyle+)
		  (aref update-vals opal::+arc-fstyle+))))


(define-method :rotate opal:arc (gob &optional center-x center-y)
  (declare (ignore gob center-x center-y))
  "This isn't a trivial computation, so we aren't going to do it at all.")

;; normalize-angle converts the given angle (in radians) to be
;; between 0 and 2PI. The converted angle is returned.
;;
;; Parameters:
;;    angle - angle to normalize
;;
(defun normalize-angle (angle)
  (if (< angle 0)
      (decf angle (* (truncate (/ (- angle +twopi+) +twopi+))
		     +twopi+))
    (if (> angle +twopi+)
	(decf angle (* (truncate (/ angle +twopi+)) +twopi+))))
  angle)

	
;; point-in-arc returns T if the given point is inside the given arc.
;; Otherwise returns NIL.
;;
;; Parameters:
;;    x,y    - point to check
;;    cx,cy  - center of ellipse that arc is a slice of
;;    rx,rx  - horizontal and vertical radius of ellipse
;;    angle1 - angle that arc starts at
;;    angle2 - number of radians counterclockwise from :angle1 that arc ends at
;;             (NOTE: angles must be normalized to be between 0 and 2PI)
;;
(defun point-in-arc (x y cx cy rx ry angle1 angle2)
  ;; point must lay in the same quadrant as the arc AND within the ellipse
  (and (opal::point-in-ellipse x y cx cy rx ry)
       (let ((dx (- x cx))
	     (dy (- cy y)))
	 (or (and (zerop dx) (zerop dy))
	     (let ((angle (normalize-angle (atan dy dx)))
		   (end-angle (normalize-angle (+ angle1 angle2))))
	       (if (> end-angle angle1)
		   (and (>= angle angle1)          ;; normal case
			(<= angle end-angle))
		 (or                               ;; wrapped around 0
		  (and (<= angle end-angle)            ;; angle in Quadrant I
		       (<= angle angle1))
		  (and (>= angle end-angle)            ;; angle in Quandrant IV
		       (>= angle angle1)))))))))


(define-method :point-in-gob opal:arc (gob x y)
 (and (g-value gob :visible)
  (let* ((rx (/ (g-value gob :width) 2))
	 (ry (/ (g-value gob :height) 2))
	 (thickness (get-thickness gob))
	 (threshold (g-value gob :hit-threshold))
	 (outer-rx (+ rx threshold))
	 (outer-ry (+ ry threshold))
	 (cx (center-x gob))
	 (cy (center-y gob))
	 (angle1 (g-value gob :angle1))
	 (angle2 (g-value gob :angle2)))
    (and (point-in-arc x y cx cy outer-rx outer-ry angle1 angle2)
	 (not (and (g-value gob :select-outline-only)
		   (let ((inner-rx (- rx thickness threshold))
			 (inner-ry (- ry thickness threshold)))
		     (point-in-arc x y cx cy inner-rx inner-ry
				   angle1 angle2))))))))



;;;   Ovals
;;;
(define-method :draw opal:oval (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (left   (aref update-vals +arc-left+))
	 (top    (aref update-vals +arc-top+))
	 (width  (aref update-vals +arc-width+))
	 (height (aref update-vals +arc-height+))
	 (lstyle (aref update-vals +arc-lstyle+))
	 (fstyle (aref update-vals +arc-fstyle+))
	 (thickness (get-old-thickness gob +arc-lstyle+ update-vals))
	 (fill-width (- width (* 2 thickness)))
	 (fill-height (- height (* 2 thickness))))
    (when (and (plusp width) (plusp height)) ; only draw if width, height > 0
      (if (or (< fill-width 1) (< fill-height 1))
	;; if oval too small, just draw black oval
	(setf fstyle opal:black-fill))
      (gem:draw-arc a-window left top width height 0.0 +twopi+
		    (aref update-vals +arc-draw-function+) lstyle fstyle))))


(define-method :point-in-gob opal:oval (gob x y)
 (and (g-value gob :visible)
  (let* ((rx (/ (g-value gob :width) 2))
	 (ry (/ (g-value gob :height) 2))
	 (thickness (get-thickness gob))
	 (threshold (g-value gob :hit-threshold))
	 (outer-rx (+ rx threshold))
	 (outer-ry (+ ry threshold))
	 (cx (center-x gob))
	 (cy (center-y gob)))
    (and (point-in-ellipse x y cx cy outer-rx outer-ry)
	 (not (and (g-value gob :select-outline-only)
		   (let ((inner-rx (- rx thickness threshold))
			 (inner-ry (- ry thickness threshold)))
		     (point-in-ellipse x y cx cy inner-rx inner-ry))))))))
	   


;;; Circles

(define-method :draw opal:circle (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (width  (aref update-vals opal::+circle-width+))
	 (height (aref update-vals opal::+circle-height+))
	 (lstyle (aref update-vals opal::+circle-lstyle+))
	 (fstyle (aref update-vals opal::+circle-fstyle+))
	 (thickness (opal::get-old-thickness gob opal::+circle-lstyle+
					     update-vals))
	 (diameter (min width height))
	 (fill-diameter (- diameter (* 2 thickness))))
    (when (plusp diameter)		;don't draw unless diameter > 0
      (if (not (plusp fill-diameter))	; if circle is too small,
					; just draw black circle
	  (setf lstyle nil
		fstyle opal:black-fill))
      (gem:draw-arc a-window
		(aref update-vals opal::+circle-left+)
		(aref update-vals opal::+circle-top+)
		diameter diameter 0.0 opal::+twopi+
		(aref update-vals opal::+circle-draw-function+)
		lstyle fstyle))))


(define-method :point-in-gob opal:circle (gob x y)
 (and (g-value gob :visible)
  (let* ((r (/ (min (g-value gob :width)
		    (g-value gob :height)) 2))
	 (thickness (get-thickness gob))
	 (threshold (g-value gob :hit-threshold))
	 (outer-r (+ r threshold))
	 ;; These next two values used to be (center-x gob) and
	 ;; (center-y gob), but that doesn't work for a circle
	 ;; whose width and height are unequal.
	 (cx (+ (g-value gob :left) r))
	 (cy (+ (g-value gob :top) r)))
    (and (point-in-ellipse x y cx cy outer-r outer-r)
	 (not (and (g-value gob :select-outline-only)
		   (let ((inner-r (- r thickness threshold)))
		     (point-in-ellipse x y cx cy inner-r inner-r))))))))
