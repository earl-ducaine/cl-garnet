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
;;; 17-Dec-93 amickish Halftone-X-Image ---> Halftone-Device-Image
;;; 16-Jul-93 amickish Moved Set-Draw-Functions to defs.lisp
;;; 22-Jul-92 bvz Rewrote gv-right, etc. functions to use broken-link-throw
;;; 16-Mar-92 dzg,amickish  Removed copy-down of :update-slots and
;;;               :fast-redraw-p in :intialize method of opal:view-object
;;; 10-Mar-92 ecp Rewrote halftone, halftone-darker, halftone-lighter
;;;		  so that if you call halftone twice with the same argument
;;;		  you'll get the same answer.
;;;  6-Aug-91 dzg Added error checking in bottom, right, etc.
;;; 20-May-90 ecp New :percent slot in opal:bitmap tells what percent it is.
;;; 19-Jun-90 ecp New functions gv-center-x-is-center-of, gv-center-y-is-center-of,
;;;		  gv-right-is-left-of, gv-bottom-is-top-of.
;;;  5-Jun-90 dzg Changed update-info structure to reduce storage allocation.
;;; 16-Apr-90 ecp Moved center-x, center-y from basics.lisp to objects.lisp
;;; 19-Mar-90 ecp Changed :tile to :stipple
;;; 13-Feb-90 dzg Changed certain macros to defuns.
;;;               Added new arguments to halftone (for color).

(in-package "OPAL")

;;; The following allow access and setting to the gobs center
;;; position.

(defun center (gob)
  (values (center-x gob) (center-y gob)))

;;; The accessors for the bottom and right of the gob, make it easier to
;;; adjust the far side of the gob's bounding box.
;;; Used to be macros, but were changed to defuns for efficiency.

(defun bottom (gob)
  (when gob (1- (+ (g-value gob :top) (g-value gob :height)))))

(defun right (gob)
  (when gob (1- (+ (g-value gob :left) (g-value gob :width)))))

(defun gv-bottom (gob)
  (if gob (1- (+ (gv gob :top) (gv gob :height)))
          (kr::broken-link-throw nil :top)))

(defun gv-right (gob)
  (if gob (1- (+ (gv gob :left) (gv gob :width)))
          (kr::broken-link-throw nil :left)))

(defun gv-center-x (gob)
  (if gob (+ (gv gob :left) (truncate (gv gob :width) 2))
          (kr::broken-link-throw nil :left)))

(defun gv-center-y (gob)
  (if gob (+ (gv gob :top) (truncate (gv gob :height) 2))
          (kr::broken-link-throw nil :top)))

;;; For formulas that want to set an object's center, right or bottom.

; Gives the value for :left such that (gv-right :self) equals (gv gob :left)
(defun gv-right-is-left-of (gob)
  (if gob (- (gv gob :left) (gvl :width))
          (kr::broken-link-throw nil :left)))

; Gives the value for :top such that (gv-bottom :self) equals (gv gob :top)
(defun gv-bottom-is-top-of (gob)
  (if gob (- (gv gob :top) (gvl :height))
          (kr::broken-link-throw nil :top)))

; Gives the value for :left such that (gv-center-x :self) equals (gv-center-x 
; gob)
(defun gv-center-x-is-center-of (gob)
  (if gob
      (if (and (is-a-p gob WINDOW)
	       (or (eq (gvl :window) gob) (eq (gvl :parent) gob)))
	  ;; If I am trying to center myself within a window, and I am going
	  ;; to be drawn w.r.t the window's coordinate system (i.e., I am an
	  ;; object or child in gob), then I want to ignore the window's :left
	  (truncate (- (gv gob :width) (gvl :width)) 2)
	  (- (gv-center-x gob) (truncate (gvl :width) 2)))
      (kr::broken-link-throw nil :left)))

; Gives the value for :top such that (gv-center-y :self) equals (gv-center-y
; gob)
(defun gv-center-y-is-center-of (gob)
  (if gob
      (if (and (is-a-p gob WINDOW)
	       (or (eq (gvl :window) gob) (eq (gvl :parent) gob)))
	  ;; If I am trying to center myself within a window, and I am going
	  ;; to be drawn w.r.t the window's coordinate system (i.e., I am an
	  ;; object or child in gob), then I want to ignore the window's :top
	  (truncate (- (gv gob :height) (gvl :height)) 2)
	  (- (gv-center-y gob) (truncate (gvl :height) 2)))
      (kr::broken-link-throw nil :top)))

;;; bounding-box just returns the current cached value of the bounding box
;;; as four values (top left width height)
(defun bounding-box (gob)
  (values (g-value gob :left) (g-value gob :top)
	  (g-value gob :width) (g-value gob :height)))

;;; Unified setting methods
;;;
;;; These set more than one property of a gob, and may be much faster than
;;; calling numerous methods.

;;;
;;; Currently they aren't.

(defun set-center (gob x y)
  (setf (center-x gob) x)
  (setf (center-y gob) y))


(defun set-position (gob left top)
  (setf (g-value gob :left) left)
  (setf (g-value gob :top) top))

(defun set-size (gob width height)
  (setf (g-value gob :width) width)
  (setf (g-value gob :height) height))

(defun set-bounding-box (gob left top width height)
  (set-size gob width height)
  (set-position gob left top))


;;; Methods on view objects
;;; This sets up the update-info slot (allocates memory for it), and then
;;; copies down the :update-slots value to make it local.
;;; -- dzg,amickish -- removed copy-down of :update-slots and :fast-redraw-p
(define-method :initialize opal:view-object (gob)
  (let ((temp-info (make-update-info)))
    (setf (update-info-bits temp-info) 0)
    (setf (update-info-old-bbox temp-info) (make-bbox))
    (s-value gob :update-info temp-info)))


;;; Methods on graphical objects

;;; Initialize sets up the default values of objects
(define-method :initialize opal:graphical-object (gob)
  (call-prototype-method gob)
  ;; This is not an aggregate!  Used by update algorithm for efficiency
  (setf (update-info-aggregate-p (g-local-value gob :update-info)) NIL))

(define-method :point-in-gob opal:view-object (gob x y)
 (and (g-value gob :visible)
  (let ((top (g-value gob :top))
	(left (g-value gob :left))
	(width (g-value gob :width))
	(height (g-value gob :height))
	(hit (g-value gob :hit-threshold)))
    (and (>= x (- left hit))
	 (< x (+ left width hit))
	 (>= y (- top hit))
	 (< y (+ top height hit))))))

(defun assign-draw-function (f n)
  (let ((pair (assoc f *function-alist*)))
    (when pair
	(setf (get f :x-draw-function) n)
	(rplacd pair n))))


;;; Halftone creation functions
(defun halftone (percent &key (foreground-color opal:black)
			      (background-color opal:white))
  (let* ((halftone (aref *halftone-table* (find-halftone percent)))
	 (fstyle (halftone-filling-style halftone)))
    (unless fstyle
      (setf (halftone-filling-style halftone)
        (setq fstyle
          (create-instance NIL opal:filling-style
            (:fill-style :opaque-stippled)
            (:stipple
	      (create-instance NIL opal:bitmap
	        (:percent (halftone-percent halftone))
	        (:image (halftone-device-image halftone))))))))
    (values
      ;; the filling-style
      (if (and (eq foreground-color opal:black)
	       (eq background-color opal:white))
	fstyle
        (create-instance NIL fstyle
	   (:foreground-color foreground-color)
	   (:background-color background-color)))
      ;; the real percentage
      (halftone-percent halftone))))



(defun halftone-darker (percent &key (foreground-color opal:black)
                                     (background-color opal:white))
  (halftone (halftone-percent
               (aref *halftone-table*
		     (min (1- *halftone-table-size*)
			  (1+ (find-halftone percent)))))
            :foreground-color foreground-color
            :background-color background-color))


(defun halftone-lighter (percent &key (foreground-color opal:black)
                                      (background-color opal:white))
  (halftone (halftone-percent
               (aref *halftone-table*
		     (max 0 (1- (find-halftone percent)))))
            :foreground-color foreground-color
            :background-color background-color))
