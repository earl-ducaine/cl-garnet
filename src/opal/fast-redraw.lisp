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
;;; 10-Aug-93 Mickish - Created with methods taken from update-window.lisp

(in-package "OPAL")

;;; NOTES ON FAST-REDRAW
;;;    There are three allowed values for :fast-redraw-p -- T, :rectangle,
;;; and :redraw.  The T case assumes that the object has an :xor draw
;;; function, so the object is just redrawn -- an object XOR'ed on top
;;; of itself will disappear. 
;;;    The other two cases require that the user specify a filling-style
;;; and a line style in :fast-redraw-filling-style and :fast-redraw-
;;; line-style.  These styles should be the same color as the background
;;; behind the fast redraw object.  Then, for the value :rectangle,
;;; the bounding box of the object will be covered by a rectangle filled
;;; with the fast-redraw-filling-style to erase the object.  For the value
;;; :redraw, the object will be entirely redrawn using the background styles,
;;; causing it to disappear.  The :set-style methods implement the temporary
;;; setting and resetting of the object's filling and line styles during
;;; this procedure.

;;;    SET-STYLES is a method which is called by the fast-redraw
;;; algorithm while erasing an object.  We need a method for this procedure
;;; because the update-slots-values array is different for every object.

(define-method :set-styles opal:line (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +line-lstyle+) line-style)))

(define-method :set-styles opal:rectangle (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +rect-fstyle+) filling-style)
    (setf (aref update-vals +rect-lstyle+) line-style)))

(define-method :set-styles opal:multipoint (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +multi-lstyle+) line-style)))

(define-method :set-styles opal:polyline (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +polyline-fstyle+) filling-style)
    (setf (aref update-vals +polyline-lstyle+) line-style)))

(define-method :set-styles opal:text (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +text-lstyle+) line-style)))

(define-method :set-styles opal:bitmap (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +bm-fstyle+) filling-style)))

(define-method :set-styles opal:arc (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +arc-fstyle+) filling-style)
    (setf (aref update-vals +arc-lstyle+) line-style)))

(define-method :set-styles opal:oval (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +arc-fstyle+) filling-style)
    (setf (aref update-vals +arc-lstyle+) line-style)))

(define-method :set-styles opal:circle (obj line-style filling-style)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (vector update-vals)
    (setf (aref update-vals +circle-fstyle+) filling-style)
    (setf (aref update-vals +circle-lstyle+) line-style)))




;;; This is the FAST-REDRAW-RECTANGLE's update-slots-values, and is
;;; used by the set-frr-bbox methods
(defvar frr-update-vals
   (g-local-value fast-redraw-rectangle :update-slots-values))

(defun set-frr-bbox-fn (left top width height)
  (setf (aref frr-update-vals +rect-left+) left)
  (setf (aref frr-update-vals +rect-top+) top)
  (setf (aref frr-update-vals +rect-width+) width)
  (setf (aref frr-update-vals +rect-height+) height))

(define-method :set-frr-bbox opal:line (obj)
  (let* ((update-vals (g-local-value obj :update-slots-values))
	 (x1 (aref update-vals +line-x1+))
	 (x2 (aref update-vals +line-x2+))
	 (y1 (aref update-vals +line-y1+))
	 (y2 (aref update-vals +line-y2+))
	 (line-style (aref update-vals +line-lstyle+))
	 (projecting-p (when line-style
			 (eq (g-value line-style :cap-style) :projecting)))
	 (line-thickness (safe-max 1 (and line-style
				       (g-value line-style :line-thickness))))
	 (lt/2 (floor line-thickness 2))
	 (left (- (safe-min x1 x2)
		  (if projecting-p line-thickness lt/2)))
	 (top (- (safe-min y1 y2)
		 (if projecting-p line-thickness lt/2)))
	 (width (+ (abs (- (or x1 0) (or x2 0)))
		   (* (if projecting-p 2 1) line-thickness)))
	 (height (+ (abs (- (or y1 0) (or y2 0)))
		    (* (if projecting-p 2 1) line-thickness))))
    (set-frr-bbox-fn left top width height)))


(define-method :set-frr-bbox opal:rectangle (obj)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (set-frr-bbox-fn (aref update-vals +rect-left+)
	    (aref update-vals +rect-top+)
	    (aref update-vals +rect-width+)
	    (aref update-vals +rect-height+))))

(define-method :set-frr-bbox opal:roundtangle (obj)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (set-frr-bbox-fn (aref update-vals +roundt-left+)
	    (aref update-vals +roundt-top+)
	    (aref update-vals +roundt-width+)
	    (aref update-vals +roundt-height+))))

(define-method :set-frr-bbox opal:multipoint (obj)
  (let* ((update-vals (g-local-value obj :update-slots-values))
	 (point-list (aref update-vals +multi-point-list+))
	 (line-style (g-value obj :line-style))
	 (line-thickness (safe-max 1 (and line-style
				       (g-value line-style :line-thickness))))
	 (2lt (* line-thickness 2)) (4lt (* line-thickness 4))
	 (left (do ((min-x 9999)
		    (point point-list (cddr point)))
		   ((null point) (- min-x 2lt))
		 (setf min-x (min min-x (car point)))))
	 (top (do ((min-y 9999)
		   (point point-list (cddr point)))
		  ((null point) (- min-y 2lt))
		(setf min-y (min min-y (cadr point)))))
	 (width (do ((max-x 0) (min-x 9999)
		     (point point-list (cddr point)))
		    ((null point) (+ (- max-x min-x) 4lt))
		  (setf min-x (min min-x (car point)))
		  (setf max-x (max max-x (car point)))))
	 (height (do ((min-y 9999) (max-y 0)
		      (point point-list (cddr point)))
		     ((null point) (+ (- max-y min-y) 4lt))
		   (setf min-y (min min-y (cadr point)))
		   (setf max-y (max max-y (cadr point))))))
    (set-frr-bbox-fn left top width height)))


(define-method :set-frr-bbox opal:text (obj)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (set-frr-bbox-fn (aref update-vals +text-left+)
	    (aref update-vals +text-top+)
	    (aref update-vals +text-width+)
	    (aref update-vals +text-height+))))


(define-method :set-frr-bbox opal:bitmap (obj)
  (let ((update-vals (g-local-value obj :update-slots-values))
	(image (g-value obj :image))
	(width 0)
	(height 0))
    (if image
      (multiple-value-setq (width height)
	(gem:image-size (gvl :window) image)))
    (set-frr-bbox-fn (aref update-vals +bm-left+) (aref update-vals +bm-top+)
		     width height)))


(define-method :set-frr-bbox opal:arc (obj)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (set-frr-bbox-fn (aref update-vals +arc-left+)
	    (aref update-vals +arc-top+)
	    (aref update-vals +arc-width+)
	    (aref update-vals +arc-height+))))


(define-method :set-frr-bbox opal:circle (obj)
  (let ((update-vals (g-local-value obj :update-slots-values)))
    (set-frr-bbox-fn (aref update-vals +circle-left+)
	    (aref update-vals +circle-top+)
	    (aref update-vals +circle-width+)
	    (aref update-vals +circle-height+))))
