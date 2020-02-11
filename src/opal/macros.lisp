

(in-package :kr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(gv-fixnum g-value-fixnum)))

;; ;; FMG Some common idioms supporting optimization.
;; (defmacro gv-fixnum (object &rest slots)
;;   `(the fixnum (gv ,object ,@slots)))

;; (defmacro g-value-fixnum (object &rest slots)
;;   `(the fixnum (g-value ,object ,@slots)))
