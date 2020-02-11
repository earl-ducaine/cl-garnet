

(in-package :kr)

(declaim (notinline bottom))
(declaim (notinline right))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(gv-fixnum gvl-fixnum g-value-fixnum)))

;; FMG Some common idioms supporting optimization.
(defmacro gv-fixnum (object &rest slots)
  `(the fixnum (gv ,object ,@slots)))

(defmacro gvl-fixnum (&rest slots)
  `(the fixnum (gvl ,@slots)))

(defmacro g-value-fixnum (object &rest slots)
  `(the fixnum (g-value ,object ,@slots)))



(in-package "OPAL")


;; Wrappers for KR-SEND.
(defmacro add-component (schema &rest args)
  `(let ((the-schema ,schema))
     (kr-send the-schema :add-component the-schema ,@args)))

(defmacro remove-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :remove-component the-schema ,@args)))

(defmacro move-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :move-component the-schema ,@args)))

(defmacro do-all-components (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :do-all-components the-schema ,@args)))

;; Added do-items because it would be very helpful to operate over the
;; items of a virtual-aggregate or an aggrelist. [2003/09/16:rpg]

(defmacro do-items (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :do-items the-schema ,@args)))

(defmacro point-to-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :point-to-component the-schema ,@args)))

(defmacro point-to-leaf (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :point-to-leaf the-schema ,@args)))

(defmacro fix-update-slots (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :fix-update-slots the-schema ,@args)))

(defmacro initialize (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :initialize the-schema ,@args)))

(defmacro destroy-me (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :destroy-me the-schema ,@args)))

(defmacro destroy (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :destroy the-schema ,@args)))

(defmacro rotate (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :rotate the-schema ,@args)))

(defmacro update (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :update the-schema ,@args)))

(defmacro draw (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :draw the-schema ,@args)))


(defmacro set-styles (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :set-styles the-schema ,@args)))

(defmacro set-frr-bbox (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :set-frr-bbox the-schema ,@args)))

(defmacro dothings ((varname &rest things) &body body)
 "Same as 'dolist', except 'things' are not a list.  Does not cons."
 (let ((count (length things))
       (tagname   (gensym "TOP-TAG"))
       (countname (gensym "COUNT"))
       case-entries)
  (dolist (thing things)
    (push (list (decf count) thing) case-entries))
  (setq case-entries (nreverse case-entries))
  `(let ((,countname ,(length things))
         ,varname)
    (tagbody
      ,tagname
      (unless (zerop ,countname)
	(setq ,varname (case (decf ,countname) ,@case-entries))
        ,@body
        (go ,tagname))))))

(declaim (inline get-thickness))
(defun get-thickness (gob)
  (let* ((line-style (g-value gob :line-style))
	 (thickness  (and line-style (g-value line-style :line-thickness))))
    (if thickness (max thickness 1) 0)))

;; This version of get-thickness AREFs the update-vals array for the
;; line thickness, rather than g-valuing the :line-style slot. Thus,
;; we get the "old" line thickness.
(declaim (inline get-old-thickness))
(defun get-old-thickness (gob line-style-index update-vals)
  (declare (ignore gob))
  (let* ((line-style (aref update-vals line-style-index))
	 (thickness  (and line-style (g-value line-style :line-thickness))))
    (if thickness (max thickness 1) 0)))

(declaim (inline point-in-rectangle))
(defun point-in-rectangle (x y left top right bottom)
  (and (<= left x right)
       (<= top y bottom)))


;;  TEXT MACROS

(declaim (inline the-width))
(defun the-width (text-extents)
  (first text-extents))

(declaim (inline the-actual-ascent))
(defun the-actual-ascent (text-extents)
  (second text-extents))

(declaim (inline the-actual-descent))
(defun the-actual-descent (text-extents)
  (third text-extents))

(declaim (inline the-left-bearing))
(defun the-left-bearing (text-extents)
  (fourth text-extents))

(declaim (inline the-right-bearing))
(defun the-right-bearing (text-extents)
  (fifth text-extents))

(declaim (inline the-font-ascent))
(defun the-font-ascent (text-extents)
  (sixth text-extents))

(declaim (inline the-font-descent))
(defun the-font-descent (text-extents)
  (seventh text-extents))

(declaim (inline left-side))
(defun left-side (gob)
  (g-value gob :left))

(declaim (inline top-side))
(defun top-side (gob)
  (g-value gob :top))
