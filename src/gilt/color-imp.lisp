;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file originally created by Gilt, then hacked

#|
============================================================
Change log:
     3/4/93 Brad Myers - made work with multiple objects
     4/24/92 Pervin - Changed defvar's to proclaim.
     2/18/92 Brad Myers - put (constant T) in generated colors
     3/1/91 Brad Myers - created Show-Color-Dialog-For 
     2/20/91 Osamu Hashimoto - separated it from particular gadgets
    12/8/90 Brad Myers - made to work with Gilt
    11/1/90 Osamu Hashimoto - created
============================================================
|#

(in-package "GILT")

(declaim (special COLOR-PROP MY-COLOR COLOR-BOXES))

;; This is either called with a for-obj, in which case it sets that
;; object's slot (Show-Color-Dialog-For), or else it is called with a 
;; special function to execute (Show-Color-Prop).
(defun color-prop-ok (top-gadget values)
  (declare (ignore values))
  (let ((color (g-value my-color :real-color-name))
	(obj-or-objs (g-value top-gadget :for-obj))
	(slot (g-value top-gadget :slot)))
    (unless color
      (let ((kr::*constants-disabled* T))
	(setq color (create-instance NIL opal:color
				     ;; constant will be written out
				     (:constant '(T))
				     (:red (g-value my-color :red))
				     (:green (g-value my-color :green))
				     (:blue (g-value my-color :blue))))))
    (if obj-or-objs
	(gilt:set-value-of-appropriate-objs obj-or-objs slot color)
	(kr-send color-prop :selection-function color))))

;; takes the specified color and sets it into other-filling-style and sets
;; that as the filling style for the object.
;; * Design: there is an interesting feature in Opal that requires that the
;; filling-style of an object change if the filling-style itself needs to
;; be reevaluated, so by swapping filling-styles, we make sure opal notices
;; that it should use a new one.
;; Obj should be the :current-color object
(defun Use-Color (obj newcolor)
  (let ((cur-fill (g-value obj :filling-style))
	(other-fill (g-value obj :other-filling-style)))
    (opal:update-all) ; try to separate the previous changes from the
		      ; changes to the color object
    (s-value other-fill :foreground-color newcolor)
    (s-value obj :filling-style other-fill)
    (s-value obj :other-filling-style cur-fill)
    ))

;;; This is called as the final function when the user hits on a particular color
;;; box.
(defun definite-color (inter obj)
  (declare (ignore inter))
  (let* ((fore (g-value obj :filling-style :foreground-color))
	 (red (g-value fore :red))
	 (green (g-value fore :green))
	 (blue (g-value fore :blue)))
    (s-value (g-value color-prop :red-slider)   :value (round (* 100 red)))
    (s-value (g-value color-prop :green-slider) :value (round (* 100 green)))
    (s-value (g-value color-prop :blue-slider)  :value (round (* 100 blue)))
    
    (s-value my-color :red red)
    (s-value my-color :green green)
    (s-value my-color :blue blue)
    
    ;; keep track of the color name
    (s-value my-color :real-color-name
	     (g-value obj :filling-style :foreground-color))
    
    (Use-color (g-value color-prop :current-color) my-color)))

;;; This is called as the selection function when the user moves a slider
(defun slider-value (gadget value)
  (let ((color-type (g-value gadget :color-type)))
    (case color-type
      (:RED (s-value my-color :red (/ value 100.0)))
      (:GREEN (s-value my-color :green (/ value 100.0)))
      (:BLUE (s-value my-color :blue (/ value 100.0)))
      (T (error "bad slider")))
    (find-and-set-color)))

;; sets the initial color based on the color in my-color
(defun find-and-set-color ()
  (let ((red (g-value my-color :red))
	(green (g-value my-color :green))
	(blue (g-value my-color :blue))
	(def-color-p NIL)
	fore)
    (dovalues (def-color color-boxes :components)
	  (setq fore (g-value def-color :filling-style :foreground-color))
	  (when (and
		 (equal red (g-value fore :red))
		 (equal green (g-value fore :green))
		 (equal blue (g-value fore :blue)))
	    (s-value (g-value color-prop :feedback) :obj-over def-color)
	    (setq def-color-p fore)
	    (return)))
      ;; keep track of the color name
      (s-value my-color :real-color-name def-color-p)
      (unless def-color-p
	(s-value (g-value color-prop :feedback) :obj-over NIL))
    (Use-color (g-value color-prop :current-color) my-color)))

(defun init-color (orig-color)
  (let ((red (g-value orig-color :red))
	(green (g-value orig-color :green))
	(blue (g-value orig-color :blue)))

    ;; first do g-values so the constraints are set up correctly
    (g-value color-prop :red-slider :value)
    (g-value color-prop :green-slider :value)
    (g-value color-prop :blue-slider :value)

    (s-value (g-value color-prop :red-slider)   :value (round (* 100 red)))
    (s-value (g-value color-prop :green-slider) :value (round (* 100 green)))
    (s-value (g-value color-prop :blue-slider)  :value (round (* 100 blue)))
    
    (s-value my-color :red red)
    (s-value my-color :green green)
    (s-value my-color :blue blue)

    (find-and-set-color)))

;;; Top level function to display the color prop dialog box when a
;;; function is to be called as the final result.  When
;;; finished, the selection-function is called with the final color
;;; chosen.  If the color is NOT one of the standard opal colors, then
;;; a new color object is allocated.
(defun Show-Color-Prop (orig-color x y selection-function)
  (init-color orig-color)
  (s-value color-prop :selection-function selection-function)
  (s-value color-prop :for-obj NIL)  ; This tells color-prop-ok to
				     ; call the selection function.
  (show-in-window color-prop x y))
  
;;; Top level function to display the color prop dialog box when a
;;; slot of the object is to be changed.  When
;;; finished, the slot of the object is set with the final color.
;;; If the color is NOT one of the standard opal colors, then
;;; a new color object is allocated.
(defun Show-Color-Dialog-For (left top obj-or-objs slot)
  (let* ((orig-color (gilt:value-from-one-obj obj-or-objs slot)))
    (init-color orig-color)
    (s-value color-prop :selection-function NIL)
    (s-value color-prop :for-obj obj-or-objs)  ; This tells color-prop-ok to
				     ; set the slot
    (s-value color-prop :slot slot)
    (show-in-window color-prop left top)))
  
