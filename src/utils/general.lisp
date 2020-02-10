(in-package :garnet-utils)

(defvar *debug-utils-mode* t)

(defconstant pi/2 (/ pi 2))
(defconstant pi3/2 (* 3 (/ pi 2)))
(defconstant 2PI (* 2 PI))
(defconstant -2PI (- (* 2 PI)))
(defconstant short-PI (coerce PI 'short-float))

(defmacro while (test &rest body)
  "Loop while test is true. If already not true, don't loop at all."
  `(do ()
     ((not ,test))
     ,@body))

(defmacro till (test &body body)
  "Loop until test is true. If already true, don't loop at all."
  `(do ()
       (,test)
     ,@body))

;; Original Garnet version (loops at least once).
(defmacro until (test &body body)
  "Loop until test is true. Loops at least once."
  `(loop ,@body
      (when ,test (return))))

(defmacro string+ (&rest args) `(concatenate 'string ,@args))

(defun safe-functionp (fn)
  (or (functionp fn)
      (and (symbolp fn) (fboundp fn))))
