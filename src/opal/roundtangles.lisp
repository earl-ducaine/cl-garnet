;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Changes:
;;; 23-Aug-93 amickish Changed hit-threshold binding in point-in-gob method
;;;               to conform to documented specifications
;;;  6-Apr-93 koz Converted with-*-styles macros to set-*-style fns
;;;               And omitted "clip-mask" as argument to draw function.
;;;  5-Oct-92 koz/amickish get-thickness --> get-old-thickness in :draw methods
;;; 28-Apr-92 ecp Improved draw-filled-roundtangle.
;;;  4-Apr-92 amickish Removed redundant :initialize method for roundtangles
;;;  4-Mar-91 d'souza Removed nickname "MO" of package Opal.
;;; 26-Jun-90 ecp Changed 0 to 0.0 in draw-arcs due to temporary xbug.

(in-package "OPAL")

;;; Roundtangles

(define-method :draw roundtangle (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (r (aref update-vals +roundt-draw-radius+)))
    (gem:draw-roundtangle a-window
			  (aref update-vals +roundt-left+)
			  (aref update-vals +roundt-top+)
			  (max 0 (aref update-vals +roundt-width+))
			  (max 0 (aref update-vals +roundt-height+))
			  r r
			  (aref update-vals +roundt-draw-function+)
			  (aref update-vals +roundt-lstyle+)
			  (aref update-vals +roundt-fstyle+))))


(defun point-in-roundtangle (x y left top right bottom radius)
  (if (point-in-rectangle x y left top right bottom)
    (or (<= radius 0)
	(cond
	  ((point-in-rectangle x y left top (+ left radius) (+ top radius))
	   (point-in-ellipse x y (+ left radius) (+ top radius) radius radius))
	  ((point-in-rectangle x y (- right radius) top right (+ top radius))
	   (point-in-ellipse x y (- right radius) (+ top radius)
			     radius radius))
	  ((point-in-rectangle x y left (- bottom radius) (+ left radius)
			       bottom)
	   (point-in-ellipse x y (+ left radius) (- bottom radius)
			     radius radius))
	  ((point-in-rectangle x y (- right radius) (- bottom radius) right
			       bottom)
	   (point-in-ellipse x y (- right radius) (- bottom radius)
			     radius radius))
	  (t t)
	  ))))


(define-method :point-in-gob roundtangle (gob x y)
 (and (g-value gob :visible)
  (let* ((thickness (get-thickness gob))
	 (width (g-value gob :width))
	 (height (g-value gob :height))
	 (select-outline-only (g-value gob :select-outline-only))
	 (threshold (g-value gob :hit-threshold))
	 (radius (g-value gob :draw-radius))
	 (left (g-value gob :left))
	 (top (g-value gob :top))
	 (right (+ left width))
	 (bottom (+ top height)))
    (and (point-in-roundtangle x y (- left threshold) (- top threshold) 
			     (+ right threshold) (+ bottom threshold) 
			     (+ radius threshold))
	 (not (and select-outline-only
		   (point-in-roundtangle x y
				       (+ left thickness threshold)
				       (+ top thickness threshold)
				       (- right thickness threshold)
				       (- bottom thickness threshold)
				       (- radius thickness threshold))))))))

