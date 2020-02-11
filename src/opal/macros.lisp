

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
