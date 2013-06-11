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
;;;  GAD-v-arrows
;;;
;;;  This module is a collection of schemata definitions required by vertical
;;;  sliders and veritcal scroll bars.
;;;
;;;  Written by Andrew Mickish

#|
============================================================
Change log:
   5/29/92 Brad Myers - made so arrows go black when :interim-selected
============================================================
|#

(in-package "GARNET-GADGETS")

;;;
;;;  VERTICAL SINGLE TRILL ARROWS
;;;

(create-instance 'UP-ARROW opal:polyline
   (:constant :line-style)
   (:width (o-formula (gv (kr-path 0 :parent) :width)))
   (:width/7 (o-formula (round (gvl :width) 7)))
   (:width/3 (o-formula (round (gvl :width) 3)))
   (:width/2 (o-formula (round (gvl :width) 2)))
   (:bottom (o-formula (+ (gv (kr-path 0 :parent) :top)
			  (gv (kr-path 0 :parent) :height))))
   (:right (o-formula (+ (gv (kr-path 0 :parent) :left)
			 (gv (kr-path 0 :parent) :width))))
   (:y1 (o-formula (+ (gv (kr-path 0 :parent) :top) (gvl :width/7))))
   (:y2 (o-formula (+ (gv (kr-path 0 :parent) :top) (gvl :width/2))))
   (:y4 (o-formula (- (gvl :bottom) (gvl :width/7))))
   (:filling-style (o-formula (if (gvl :parent :frame :interim-selected)
				  opal:black-fill
				  opal:white-fill)))
   (:point-list (o-formula
		 (let* ((left (gv (kr-path 0 :parent) :left))
			(right (gvl :right))
			(width/3 (gvl :width/3))
			(width/7 (gvl :width/7))
			(x1 (+ left (gvl :width/2)))
			(y1 (gvl :y1))
			(x2 (+ left width/7))
			(y2 (gvl :y2))
			(x3 (+ left width/3))
			(y4 (gvl :y4))
			(x5 (- right width/3))
			(x7 (- right width/7)))
		   (list x1 y1 x2 y2 x3 y2 x3 y4 x5 y4 x5 y2 x7 y2 x1 y1)))))


(create-instance 'DOWN-ARROW up-arrow
   (:y1 (o-formula (- (gvl :bottom) (gvl :width/7))))
   (:y2 (o-formula (- (gvl :bottom) (gvl :width/2))))
   (:y4 (o-formula (+ (gv (kr-path 0 :parent) :top) (gvl :width/7)))))


;;;
;;;  VERTICAL DOUBLE PAGE ARROWS
;;;

(create-instance 'PAGE-UP-ARROWHEAD opal:polyline
   (:constant :line-style)
   (:right (o-formula (+ (gv (kr-path 0 :parent) :left)
			 (gv (kr-path 0 :parent) :width))))
   (:width (o-formula (gv (kr-path 0 :parent) :width)))
   (:width/7 (o-formula (round (gvl :width) 7)))
   (:width/5 (o-formula (round (gvl :width) 5)))
   (:width/3 (o-formula (round (gvl :width) 3)))
   (:width/2 (o-formula (round (gvl :width) 2)))
   (:y2 (o-formula (+ (gvl :y-origin) (gvl :width/3))))
   (:filling-style (o-formula (if (gvl :parent :parent :frame :interim-selected)
				  opal:black-fill
				  opal:white-fill)))
   (:point-list (o-formula
		 (let* ((left (gv (kr-path 0 :parent) :left))
			(width/7 (gvl :width/7))
			(x1 (+ left (gvl :width/2)))
			(y1 (gvl :y-origin))
			(x2 (+ left width/7))
			(y2 (gvl :y2))
			(x3 (- (gvl :right) width/7)))
		   (list x1 y1 x2 y2 x3 y2 x1 y1))))
   )


(create-instance 'PAGE-DOWN-ARROWHEAD page-up-arrowhead
   (:y2 (o-formula (- (gvl :y-origin) (gvl :width/3)))))

(create-instance 'PAGE-UP-ARROW opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:width (o-formula (gv (kr-path 0 :parent) :width)))
   (:parts
    `((:top-arrowhead ,page-up-arrowhead
		      (:y-origin ,(o-formula (+ (gv (kr-path 0 :parent) :top)
						(gvl :width/5)))))
      (:bot-arrowhead ,page-up-arrowhead
		      (:y-origin ,(o-formula (+ (gv (kr-path 0 :parent) :top)
						(* 2 (gvl :width/5)))))))))

(create-instance 'PAGE-DOWN-ARROW opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:bottom (o-formula (+ (gv (kr-path 0 :parent) :top)
			  (gv (kr-path 0 :parent) :height))))
   (:width (o-formula (gv (kr-path 0 :parent) :width)))
   (:parts
    `((:top-arrowhead ,page-down-arrowhead
		      (:y-origin ,(o-formula (- (gv (kr-path 0 :parent) :bottom)
						(gvl :width/5)))))
      (:bot-arrowhead ,page-down-arrowhead
		      (:y-origin ,(o-formula (- (gv (kr-path 0 :parent) :bottom)
						(* 2 (gvl :width/5)))))))))


;;  Tell the world that GAD-v-arrows has been loaded
;;
(setf (get :garnet-modules :GAD-v-arrows) T)

;;  All other dependent "parts" modules must be reloaded
;;
(setf (get :garnet-modules :GAD-v-boxes) NIL)
