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
;;;  GAD-h-arrows
;;;
;;;  This module is a collection of schemata definitions required by horizontal
;;;  scroll bars, horizontal sliders, and the trill device.
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
;;;  HORIZONTAL SINGLE TRILL ARROWS
;;;

(create-instance 'LEFT-ARROW opal:polyline
   (:constant :line-style)
   (:height (o-formula (gvl :parent :height)))
   (:height/7 (o-formula (round (gvl :height) 7)))
   (:height/3 (o-formula (round (gvl :height) 3)))
   (:height/2 (o-formula (round (gvl :height) 2)))
   (:bottom (o-formula (+ (gvl :parent :top)
			 (gvl :parent :height))))
   (:right (o-formula (+ (gvl :parent :left)
			(gvl :parent :width))))

   (:x1 (o-formula (+ (gvl :parent :left) (gvl :height/7))))
   (:y1 (o-formula (+ (gvl :parent :top) (gvl :height/2))))
   (:x2 (o-formula (+ (gvl :parent :left) (gvl :height/2))))
   (:y2 (o-formula (- (gvl :bottom) (gvl :height/7))))
   (:x3 (o-formula (gvl :x2)))
   (:y3 (o-formula (- (gvl :bottom) (gvl :height/3))))
   (:x4 (o-formula (- (gvl :right) (gvl :height/7))))
   (:y4 (o-formula (gvl :y3)))
   (:x5 (o-formula (gvl :x4)))
   (:y5 (o-formula (+ (gvl :parent :top) (gvl :height/3))))
   (:x6 (o-formula (gvl :x2)))
   (:y6 (o-formula (gvl :y5)))
   (:x7 (o-formula (gvl :x2)))
   (:y7 (o-formula (+ (gvl :parent :top) (gvl :height/7))))
   (:x8 (o-formula (gvl :x1)))
   (:y8 (o-formula (gvl :y1)))
   (:filling-style (o-formula (if (gvl :parent :frame :interim-selected)
				  opal:black-fill
				  opal:white-fill)))
   (:point-list (o-formula (list
			   (gvl :x1) (gvl :y1) (gvl :x2) (gvl :y2)
			   (gvl :x3) (gvl :y3) (gvl :x4) (gvl :y4)
			   (gvl :x5) (gvl :y5) (gvl :x6) (gvl :y6)
			   (gvl :x7) (gvl :y7) (gvl :x8) (gvl :y8)))))


(create-instance 'RIGHT-ARROW left-arrow
   (:x1 (o-formula (- (gvl :right) (gvl :height/7))))
   (:x2 (o-formula (- (gvl :right) (gvl :height/2))))
   (:x4 (o-formula (+ (gvl :parent :left) (gvl :height/7)))))



;;;
;;;  HORIZONTAL DOUBLE PAGE ARROWS
;;;

(create-instance 'PAGE-LEFT-ARROWHEAD opal:polyline
   (:constant :line-style)
   (:bottom (o-formula (+ (gvl :parent :top) (gvl :parent :height))))
   (:height (o-formula (gvl :parent :height)))
   (:height/7 (o-formula (round (gvl :height) 7)))
   (:height/5 (o-formula (round (gvl :height) 5)))
   (:height/3 (o-formula (round (gvl :height) 3)))
   (:height/2 (o-formula (round (gvl :height) 2)))
   (:x1 (o-formula (gvl :x-origin)))
   (:y1 (o-formula (+ (gvl :parent :top) (gvl :height/2))))
   (:x2 (o-formula (+ (gvl :x1) (gvl :height/3))))
   (:y2 (o-formula (+ (gvl :parent :top) (gvl :height/7))))
   (:x3 (o-formula (gvl :x2)))
   (:y3 (o-formula (- (gvl :bottom) (gvl :height/7))))
   (:filling-style (o-formula (if (gvl :parent :parent :frame :interim-selected)
				  opal:black-fill
				  opal:white-fill)))
   (:point-list (o-formula (list
			   (gvl :x1) (gvl :y1) (gvl :x2) (gvl :y2)
			   (gvl :x3) (gvl :y3) (gvl :x1) (gvl :y1))))
   (:visible (o-formula (gvl :parent :visible))))

(create-instance 'PAGE-RIGHT-ARROWHEAD page-left-arrowhead
   (:x2 (o-formula (- (gvl :x1) (gvl :height/3)))))

(create-instance 'PAGE-LEFT-ARROW opal:aggregadget
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:height (o-formula (gvl :parent :height)))
   (:visible (o-formula (gvl :parent :visible)))
   (:parts
    `((:left-arrowhead ,page-left-arrowhead
		      (:x-origin ,(o-formula (+ (gvl :parent :left)
						(gvl :height/5)))))
      (:right-arrowhead ,page-left-arrowhead
			(:x-origin ,(o-formula (+ (gvl :parent :left)
						  (* 2 (gvl :height/5)))))))))

(create-instance 'PAGE-RIGHT-ARROW opal:aggregadget
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:right (o-formula (+ (gvl :parent :left) (gvl :parent :width))))
   (:height (o-formula (gvl :parent :height)))
   (:visible (o-formula (gvl :parent :visible)))
   (:parts
    `((:right-arrowhead ,page-right-arrowhead
		      (:x-origin ,(o-formula (- (gvl :parent :right)
						(gvl :height/5)))))
      (:left-arrowhead ,page-right-arrowhead
		      (:x-origin ,(o-formula (- (gvl :parent :right)
						(* 2 (gvl :height/5)))))))))


;;  Tell the world that GAD-h-arrows has been loaded
;;
(setf (get :garnet-modules :GAD-h-arrows) T)

;;  All other dependent "parts" modules must be reloaded
;;
(setf (get :garnet-modules :GAD-h-boxes) NIL)
