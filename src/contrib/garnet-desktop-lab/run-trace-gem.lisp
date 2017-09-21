
(in-package :gem)

(defparameter *standard-output-bak* *standard-output*)
(defparameter *error-output-bak* *error-output*)
(defparameter *trace-output-bak* *trace-output*)

(setf *trace-output* (make-string-output-stream))

(trace-gem :x)

;; (defun print-stuff (x y)
;;   (format t "standard output ~a" x)
;;   (format *error-output* "error output ~a" y)
;;   (+ x y))

(defun run-do-go ()
  (demo-3d:do-go)
  (get-output-stream-string *trace-output*))
