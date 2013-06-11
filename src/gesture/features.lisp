;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; features.lisp
;;;
;;; Routines for calculating the feature vector of a gesture example. 
;;; (based on Dean Rubine's C implementation)
;;;
;;; Designed and implemented by James A. Landay

#|
============================================================
Change log:
    06/08/95 James Landay - fixed features () to return proper bbox!
    09/23/93 James Landay - intialize attribute vector even if less than 3 pts
    06/15/93 James Landay - don't require a fill-pointer in the points-array
    03/24/92 James Landay - allow features () to return a feature
                            vector with all zeros on single point gesture
    01/31/92 James Landay - changed names of attributes structure
                            to gest-attributes
    01/22/92 James Landay - made features () return the attributes
                            vector, in addition to the feature vector
    12/27/91 James Landay - changed features () to expect an array of
                            points rather than a list
    12/21/91 James Landay - started
============================================================
|#

(in-package "INTERACTORS")


;; structure definitions
;; structure to store an attribute vector
(defstruct gest-attributes 
    (startx 0)          ;; starting point
    (starty 0)
    (initial-sin 0)     ;; initial angle to the x axis
    (initial-cos 0)
    (dx2 0)             ;; differences: endx-prevx 
    (dy2 0)             ;;              endy-prevy
    (magsq2 0)          ;; dx2*dx2 + dy2*dy2
    (endx 0)            ;; last point added
    (endy 0)
    (minx 0)            ;; bounding box
    (maxx 0)
    (miny 0)
    (maxy 0)
    (path-r 0)          ;; total length (in rads)
    (path-th 0)         ;; total rotation (in rads)
    (abs-th 0)          ;; sum of absolute values of path angles
    (sharpness 0))      ;; sum of non-linear function of abs vals of
                        ;;   path angles counting acute angles heavier

;; structure to store a feature vector 
;; make it of type vector so it can be referenced as an array
(defstruct (features (:type vector))
    (initial-cos 0)     ;; initial angle to the x axis
    (initial-sin 0)
    (bbox-len 0)        ;; length of bounding box diagnol
    (bbox-th 0)         ;; angle of bounding box diagnol
    (se-len 0)          ;; length between start and end points
    (se-cos 0)          ;; angle between start and end points
    (se-sin 0)           
    (path-r 0)          ;; arc length of path (in rads)
    (path-th 0)         ;; total angle traversed (in rads)
    (abs-th 0)          ;; sum of absolute values of angles traversed
    (sharpness 0))      ;; sum of squares of angles traversed
   

;; constant definitions
(defconstant NUM-FEATURES 11)       ;; number of features in vector
(defconstant DISTSQ-THRESHOLD 9)    ;; distance square threshold
(defconstant TH-ROLLOFF 16)         ;; rolloff
(defconstant EPS 1.0e-4)          


;; global variable declarations
;; this implies that clients of functions that return these values
;; must copy them if they need to do more processing
(defparameter *fv* (make-features))                 ;; features vector
(defparameter *av* (make-gest-attributes))          ;; attributes vector


;; init-features initializes the fields of the given features structure
;;
;; Parameters:
;;     fv - feature vector to initialize 
;;
(defun init-features (fv)
    (setf (features-initial-cos fv) 0)   
    (setf (features-initial-sin fv) 0)
    (setf (features-bbox-len fv) 0)
    (setf (features-bbox-th fv) 0)
    (setf (features-se-len fv) 0)
    (setf (features-se-cos fv) 0)
    (setf (features-se-sin fv) 0)
    (setf (features-path-r fv) 0)   
    (setf (features-path-th fv) 0) 
    (setf (features-abs-th fv) 0) 
    (setf (features-sharpness fv) 0))
   
;; init-gest-attributes initializes the fields of the given gest-attributes 
;; structure
;;
;; Parameters:
;;     av - gest-attributes vector to initialize
;;
(defun init-gest-attributes (av)
    (setf (gest-attributes-startx av) 0)    
    (setf (gest-attributes-starty av) 0)
    (setf (gest-attributes-initial-sin av) 0)
    (setf (gest-attributes-initial-cos av) 0)
    (setf (gest-attributes-dx2 av) 0)         
    (setf (gest-attributes-dy2 av) 0)          
    (setf (gest-attributes-magsq2 av) 0)        
    (setf (gest-attributes-endx av) 0)           
    (setf (gest-attributes-endy av) 0)
    (setf (gest-attributes-minx av) 0) 
    (setf (gest-attributes-maxx av) 0)
    (setf (gest-attributes-miny av) 0)
    (setf (gest-attributes-maxy av) 0)
    (setf (gest-attributes-path-r av) 0)
    (setf (gest-attributes-path-th av) 0)
    (setf (gest-attributes-abs-th av) 0)  
    (setf (gest-attributes-sharpness av) 0))


;; features returns the feature vector and the attributes vector
;; of the given gesture.  These two vectors are globals and the
;; caller should copy them if they wish to keep the values.
;; Returns NIL if the points array has less than 3 points.
;;
;; Parameters:
;;     g-points - array of points [x1 y1 x2 y2 ...] making up the gesture
;;
(defun features (g-points)
    (cond 
        ((null g-points) nil)
        ((< (length g-points) 6)        ;; need at least 3 pts
           (init-gest-attributes *av*)  ;; get rid of last used values 
	   ;; save values for first point only 
           (setf (gest-attributes-startx *av*) (aref g-points 0))
           (setf (gest-attributes-starty *av*) (aref g-points 1))
           (setf (gest-attributes-endx *av*) (gest-attributes-startx *av*))
           (setf (gest-attributes-endy *av*) (gest-attributes-starty *av*))
           (setf (gest-attributes-minx *av*) (gest-attributes-startx *av*))
           (setf (gest-attributes-maxx *av*) (gest-attributes-startx *av*))
           (setf (gest-attributes-miny *av*) (gest-attributes-starty *av*))
           (setf (gest-attributes-maxy *av*) (gest-attributes-starty *av*))
           (init-features *fv*)   
           (values *fv* *av*))

        (t (init-gest-attributes *av*)  ;; get rid of last used values 
           ;; use the 1st point in the path to init. attributes vector
           (setf (gest-attributes-startx *av*) (aref g-points 0))
           (setf (gest-attributes-starty *av*) (aref g-points 1))
           (setf (gest-attributes-endx *av*) (gest-attributes-startx *av*))
           (setf (gest-attributes-endy *av*) (gest-attributes-starty *av*))
           (setf (gest-attributes-minx *av*) (gest-attributes-startx *av*))
           (setf (gest-attributes-maxx *av*) (gest-attributes-startx *av*))
           (setf (gest-attributes-miny *av*) (gest-attributes-starty *av*))
           (setf (gest-attributes-maxy *av*) (gest-attributes-starty *av*))

           ;; process the rest of the points
           (do* ((cur 2)
                 (index 2 (+ index 2)))
                ((>= index (length g-points)))  ;; exit clause

               (setf cur (process-point *av* (aref g-points index)
                                             (aref g-points (1+ index))
                                             cur)))  ;; do next point 

           ;; calculate and return the feature and attribute vectors
           (values (calc-fv *av*) *av*))))


;; process-point modifies the attribute vector associated with the 
;; gesture of the given unprocessed point.  Returns the next 
;; point number to process.
;;
;; Parameters:
;;     av         - the attribute vector for points already processed 
;;     x, y       - a point to be processed 
;;     point-num  - point number  
;;
(defun process-point (av x y point-num)
  (let ((dx1 0) (dy1 0) (magsq1 0))

         ;; check for mins,maxs (even if we ignore point below)
         ;; this is a change from rubine's algorithm -- need correct bbox!
         (if (< x (gest-attributes-minx av))  
	     (setf (gest-attributes-minx av) x))
	 (if (> x (gest-attributes-maxx av))
	     (setf (gest-attributes-maxx av) x)) 
	 (if (< y (gest-attributes-miny av))
	     (setf (gest-attributes-miny av) y)) 
	 (if (> y (gest-attributes-maxy av))
	     (setf (gest-attributes-maxy av) y))
	 
         ;; ignore point if it is not within threshold
         (setf dx1 (- x (gest-attributes-endx av)))
         (setf dy1 (- y (gest-attributes-endy av)))
         (setf magsq1 (+ (* dx1 dx1) (* dy1 dy1)))
         (cond 
	  ((<= magsq1 DISTSQ-THRESHOLD) av)        ;; ignore point, else
                                                      ;; use point
                 ;; calculate angles     
          (t (when (>= point-num 3) 
	       (when (= point-num 3)
		 (let ((dx 0) (dy 0) (magsq 0) (recip 0))
		   (setf dx (- x (gest-attributes-startx av)))
		   (setf dy (- y (gest-attributes-starty av)))
		   (setf magsq (+ (* dx dx) (* dy dy)))
		   ;; avoid divide by zero if the 1st and
		   ;; 3rd points are too close to each 
		   ;; other (use 2nd and 3rd instead)
		   (when (> magsq DISTSQ-THRESHOLD)
		     ;; find angle w.r.t. positive x axis 
		     (setf recip (/ 1 (sqrt magsq)))
		     (setf (gest-attributes-initial-cos av)
		       (* dx recip))
		     (setf (gest-attributes-initial-sin av)
		       (* dy recip)))))
    
	       (let ((dot 0) (cos2 0) (th 0))
		 (setf dot (+ (* dx1 (gest-attributes-dx2 av))
			      (* dy1 (gest-attributes-dy2 av))))
		 (setf cos2 (/ (* dot dot) 
			       (* magsq1 (gest-attributes-magsq2 av))))
		 (if (< dot 0)
		     (setf cos2 (- cos2)))
                      
		 (setf th (invcos cos2))
		 (setf (gest-attributes-abs-th av)
		   (+ (gest-attributes-abs-th av) th))
		 (setf (gest-attributes-sharpness av)
		   (+ (gest-attributes-sharpness av) (* th th)))
		 
		 ;; sin th < 0 ?
		 (if (< (* dx1 (gest-attributes-dy2 av))  
			(* dy1 (gest-attributes-dx2 av)))
		     (setf th (- th)))
		 (setf (gest-attributes-path-th av) 
		   (+ (gest-attributes-path-th av) th))))

	     (setf (gest-attributes-path-r av)
	       (+ (gest-attributes-path-r av) (sqrt magsq1)))

	     (setf (gest-attributes-endx av) x)
	     (setf (gest-attributes-endy av) y)
	     (setf (gest-attributes-dx2 av) dx1)
	     (setf (gest-attributes-dy2 av) dy1)
	     (setf (gest-attributes-magsq2 av) magsq1)
	     (setf point-num (+ 1 point-num)))))

  point-num)                   ;; return next point number to process 


;; calc-fv calculates and returns the feature vector form the
;; given attribute vector.
;;
;; Parameters:
;;     av - the attribute vector for the gesture 
;;
(defun calc-fv (av)
    (let ((factor 0))
        (setf (features-initial-cos *fv*) (gest-attributes-initial-cos av)) 
        (setf (features-initial-sin *fv*) (gest-attributes-initial-sin av)) 
        (setf (features-bbox-len *fv*) 
              (hypot (- (gest-attributes-maxx av) 
                        (gest-attributes-minx av))
                     (- (gest-attributes-maxy av) 
                        (gest-attributes-miny av))))
        (if (> (* (features-bbox-len *fv*) (features-bbox-len *fv*))
                  DISTSQ-THRESHOLD)
            (setf (features-bbox-th *fv*) 
                  (my-atan (- (gest-attributes-maxy av) 
                              (gest-attributes-miny av))
                           (- (gest-attributes-maxx av) 
                              (gest-attributes-minx av)))))

        (setf (features-se-len *fv*) 
              (hypot (- (gest-attributes-endx av) 
                        (gest-attributes-startx av))
                     (- (gest-attributes-endy av) 
                        (gest-attributes-starty av))))

        (setf factor (/ (* (features-se-len *fv*) (features-se-len *fv*))
                        TH-ROLLOFF))
        (if (> factor 1) 
            (setf factor 1))

        (if (> (features-se-len *fv*) EPS)
            (setf factor (/ factor (features-se-len *fv*)))
            (setf factor 0))

        (setf (features-se-cos *fv*) 
              (* (- (gest-attributes-endx av) (gest-attributes-startx av)) 
                 factor)) 
        (setf (features-se-sin *fv*)
              (* (- (gest-attributes-endy av) (gest-attributes-starty av)) 
                 factor))
        (setf (features-path-r *fv*) (gest-attributes-path-r av)) 
        (setf (features-path-th *fv*) (gest-attributes-path-th av)) 
        (setf (features-abs-th *fv*) (gest-attributes-abs-th av)) 
        (setf (features-sharpness *fv*) (gest-attributes-sharpness av)) 

        *fv*))                                   ;; return feature vector


;; given m = cos^2 (t) * sin (cos (t)) returns t
;; t should be between 0 and pi
;; m should be between -1 and 1
;;
;; Parameters:
;;    m -  
;;
(defun invcos (m) 
    (if (< m 0)
        (acos (- (sqrt (- m))))
        (acos (sqrt m))))


;; given x and y returns sqrt (x*x + y*y)
;;
(defun hypot (x y)
    (sqrt (+ (* x x) (* y y))))


;; given x and y returns atan (y x)
;; if x=y=0 returns 0
;;
(defun my-atan (y x)
    (if (= 0 y x) 
        0
        (atan y x)))
