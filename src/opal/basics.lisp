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

(in-package "OPAL")

;;; The following allow access and setting to the gobs center
;;; position.

(declaim (inline center))
(defun center (gob)
  (values (center-x gob) (center-y gob)))

;;; The accessors for the bottom and right of the gob, make it easier to
;;; adjust the far side of the gob's bounding box.
;;; Used to be macros, but were changed to defuns for efficiency.

(declaim (inline bottom))
(defun bottom (gob)
  (when gob (1- (+ (g-value-fixnum gob :top)
		   (g-value-fixnum gob :height)))))

(declaim (inline right))
(defun right (gob)
  (when gob (1- (+ (g-value-fixnum gob :left)
		   (g-value-fixnum gob :width)))))

(declaim (inline unchecked-gv-bottom))
(defun unchecked-gv-bottom (gob)
  (1- (+ (gv-fixnum gob :top) (gv-fixnum gob :height))))

(declaim (inline gv-bottom))
(defun gv-bottom (gob)
  (if gob (unchecked-gv-bottom gob)
      (kr::broken-link-throw nil :top)))

(declaim (inline unchecked-gv-right))
(defun unchecked-gv-right (gob)
  (1- (+ (gv-fixnum gob :left) (gv-fixnum gob :width))))

(declaim (inline gv-right))
(defun gv-right (gob)
  (if gob (unchecked-gv-right gob)
          (kr::broken-link-throw nil :left)))

(declaim (inline unchecked-gv-center-x))
(defun unchecked-gv-center-x (gob)
  (+ (gv-fixnum gob :left) (truncate (gv-fixnum gob :width) 2)))

(declaim (inline gv-center-x))
(defun gv-center-x (gob)
  (if gob (unchecked-gv-center-x gob)
      (kr::broken-link-throw nil :left)))


(declaim (inline unchecked-gv-center-y))
(defun unchecked-gv-center-y (gob)
  (+ (gv-fixnum gob :top) (truncate (gv-fixnum gob :height) 2)))

(declaim (inline gv-center-y))
(defun gv-center-y (gob)
  (if gob (unchecked-gv-center-y gob)
      (kr::broken-link-throw nil :top)))

;;; For formulas that want to set an object's center, right or bottom.

;; Gives the value for :left such that (gv-right :self) equals
;; (gv gob :left)

(declaim (inline unchecked-gv-right-is-left-of))
(defun unchecked-gv-right-is-left-of (gob)
  (- (gv-fixnum gob :left) (gvl-fixnum :width)))

(declaim (inline gv-right-is-left-of))
(defun gv-right-is-left-of (gob)
  (if gob (unchecked-gv-right-is-left-of gob)
          (kr::broken-link-throw nil :left)))

(declaim (inline unchecked-gv-bottom-is-top-of))
(defun unchecked-gv-bottom-is-top-of (gob)
  (- (gv-fixnum gob :top) (gvl-fixnum :height)))

;; Gives the value for :top such that (gv-bottom :self) equals 
;; (gv gob :top)
(declaim (inline gv-bottom-is-top-of))
(defun gv-bottom-is-top-of (gob)
  (if gob (unchecked-gv-bottom-is-top-of gob)
      (kr::broken-link-throw nil :top)))

;; Gives the value for :left such that (gv-center-x :self) equals
;; (gv-center-x gob)
(defun gv-center-x-is-center-of (gob)
  (if gob
      (if (and (is-a-p gob WINDOW)
	       (or (eq (gvl :window) gob) (eq (gvl :parent) gob)))
	  ;; If I am trying to center myself within a window, and I am going
	  ;; to be drawn w.r.t the window's coordinate system (i.e., I am an
	  ;; object or child in gob), then I want to ignore the window's :left
	  (truncate (- (gv-fixnum gob :width) (gvl-fixnum :width)) 2)
	  (- (the fixnum (gv-center-x gob)) (truncate (gvl-fixnum :width) 2)))
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
	  (truncate (- (gv-fixnum gob :height) (gvl-fixnum :height)) 2)
	  (- (the fixnum (unchecked-gv-center-y gob)) (truncate (gvl-fixnum :height) 2)))
      (kr::broken-link-throw nil :top)))

;;; bounding-box just returns the current cached value of the bounding box
;;; as four values (top left width height)
(defun bounding-box (gob)
  (values (g-value-fixnum gob :left) (g-value-fixnum gob :top)
	  (g-value-fixnum gob :width) (g-value-fixnum gob :height)))

;;; Unified setting methods
;;;
;;; These set more than one property of a gob, and may be much faster than
;;; calling numerous methods.

;;;
;;; Currently they aren't.

(defun set-center (gob x y)
  (declare (fixnum x y))
  (setf (center-x gob) x)
  (setf (center-y gob) y))


(defun set-position (gob left top)
  (declare (fixnum left top))
  (setf (g-value gob :left) left)
  (setf (g-value gob :top) top))

(defun set-size (gob width height)
  (declare (fixnum width height))
  (setf (g-value gob :width) width)
  (setf (g-value gob :height) height))

(defun set-bounding-box (gob left top width height)
  (declare (fixnum left top width height))
  (set-size gob width height)
  (set-position gob left top))


;;; Methods on view objects
;;; This sets up the update-info slot (allocates memory for it), and then
;;; copies down the :update-slots value to make it local.
;;; -- dzg,amickish -- removed copy-down of :update-slots and :fast-redraw-p
(define-method :initialize view-object (gob)
  (let ((temp-info (make-update-info)))
    (setf (update-info-bits temp-info) 0)
    (setf (update-info-old-bbox temp-info) (make-bbox))
    (s-value gob :update-info temp-info)))


;;; Methods on graphical objects

;;; Initialize sets up the default values of objects
(define-method :initialize graphical-object (gob)
  (call-prototype-method gob)
  ;; This is not an aggregate!  Used by update algorithm for efficiency
  (setf (update-info-aggregate-p (g-local-value gob :update-info)) NIL))

(define-method :point-in-gob view-object (gob x y)
 (and (g-value gob :visible)
  (let ((top (g-value gob :top))
	(left (g-value gob :left))
	(width (g-value gob :width))
	(height (g-value gob :height))
	(hit (g-value gob :hit-threshold)))
    (declare (fixnum top left width height))
    (and (>= x (- left hit))
	 (< x (+ left width hit))
	 (>= y (- top hit))
	 (< y (+ top height hit))))))

(defun assign-draw-function (f n)
  (let ((pair (assoc f gem:*function-alist*)))
    (when pair
	(setf (get f :x-draw-function) n)
	(rplacd pair n))))


;;; Halftone creation functions
(defun halftone (percent &key (foreground-color black)
			      (background-color white))
  (let* ((halftone (aref *halftone-table* (find-halftone percent)))
	 (fstyle (halftone-filling-style halftone)))
    (unless fstyle
      (setf (halftone-filling-style halftone)
        (setq fstyle
          (create-instance NIL filling-style
            (:fill-style :opaque-stippled)
            (:stipple
	      (create-instance NIL bitmap
	        (:percent (halftone-percent halftone))
	        (:image (halftone-device-image halftone))))))))
    (values
      ;; the filling-style
      (if (and (eq foreground-color black)
	       (eq background-color white))
	fstyle
        (create-instance NIL fstyle
	   (:foreground-color foreground-color)
	   (:background-color background-color)))
      ;; the real percentage
      (halftone-percent halftone))))



(defun halftone-darker (percent &key (foreground-color black)
                                     (background-color white))
  (halftone (halftone-percent
               (aref *halftone-table*
		     (min (1- *halftone-table-size*)
			  (1+ (find-halftone percent)))))
            :foreground-color foreground-color
            :background-color background-color))


(defun halftone-lighter (percent &key (foreground-color black)
                                      (background-color white))
  (halftone (halftone-percent
               (aref *halftone-table*
		     (max 0 (1- (find-halftone percent)))))
            :foreground-color foreground-color
            :background-color background-color))
