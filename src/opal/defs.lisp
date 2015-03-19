;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id$


;;; This file contains all the defvars, defconstants, defstructs, etc.,
;;  which are used by Opal.  This does not contain any defmacros, however.

(in-package "OPAL")


;;; DefConstants
;;

(defconstant +twopi+ (min (* 2 pi) (coerce (* 2 pi) 'short-float)))


;;; DefParameters

(defvar *colormap-index-table*
  ;;arguments to make-hash-table NOT well thought out! [1995/09/12:goldman]
  (make-hash-table :size 256))
  


;;; DefVars
;;

(declaim (fixnum *halftone-table-size*))
(defvar *halftone-table-size* 17)
(defvar *halftone-table* nil)		; used to be set to (build-halftone-table)
					; but now that's a forward reference.  So,
					; now we setq this after defining that fn.

(defvar *default-text-extents* (make-list 9 :initial-element 0))

(defvar no-fill nil)
(defvar no-line nil)

(defvar *garnet-windows* NIL)

;; debugging tools
(defvar *event-debug* nil)
(defvar *expose-throw-aways* 0)

(declaim (fixnum *opal-window-count*))
(defvar *opal-window-count* 0)

(defvar diamond-fill NIL)		;; set in halftones.lisp

;;; DefStructs
;;

;; This defstruct generates the functions Make-Halftone, Copy-Halftone,
;; Halftone-Percent, Halftone-Device-Image, Halftone-Filling-Style, and
;; Halftone-P.
(defstruct (HALFTONE (:print-function halftone-print))
  (percent 0)
  (device-image nil)
  (filling-style nil))

;;; This defstruct generates the functions Make-Cut-String, Copy-Cut-String,
;;; Cut-String-String, Cut-String-Width, and Cut-String-Left-Bearing.
(defstruct CUT-STRING
  (string)
  (width 0 :type fixnum)
  (left-bearing))


;;; DefSetfs
;;

;;; Accessors that do calculation from basic gob properties

;; The accessors for the bottom and right of the gob, make it easier to
;; adjust the far side of the gob's bounding box.

(defsetf bottom (gob) (value)
  `(setf (g-value ,gob :top) (1+ (- ,value (g-value-fixnum ,gob :height)))))

(defsetf right (gob) (value)
  `(setf (g-value ,gob :left) (1+ (- ,value (g-value-fixnum ,gob :width)))))

;; The accessors for the sides of the gob adjust both the dimensions, and
;; position of the gob based on the given value.

(defsetf left-side (gob) (value)
  `(progn
     (setf (g-value ,gob :width)
           (- (g-value-fixnum ,gob :width) (- ,value (g-value-fixnum ,gob :left))))
     (setf (g-value ,gob :left) ,value)))

(defsetf right-side (gob) (value)
  `(setf (g-value ,gob :width)
         (+ (g-value-fixnum ,gob :width) (- ,value (right ,gob)))))

(defsetf top-side (gob) (value)
  `(progn
     (setf (g-value ,gob :height)
           (- (g-value-fixnum ,gob :height) (- ,value (g-value-fixnum ,gob :top))))
     (setf (g-value ,gob :top) ,value)))

(defsetf bottom-side (gob) (value)
  `(setf (g-value ,gob :height)
         (+ (g-value-fixnum ,gob :height) (- ,value (bottom ,gob)))))

;; The following allow access and setting to the gobs center
;; position.

(defsetf center-x (gob) (value)
  `(setf (g-value ,gob :left)
         (- ,value (truncate (g-value-fixnum ,gob :width) 2))))

(defsetf center-y (gob) (value)
  `(setf (g-value ,gob :top)
         (- ,value (truncate (g-value-fixnum ,gob :height) 2))))
