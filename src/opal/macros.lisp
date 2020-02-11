

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


;; ;; Wrappers for KR-SEND.
;; (defmacro add-component (schema &rest args)
;;   `(let ((the-schema ,schema))
;;      (kr-send the-schema :add-component the-schema ,@args)))
