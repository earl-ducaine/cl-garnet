
(in-package :gem)
(trace-gem :x)
(defparameter demo-3d-do-go (demo-3d:do-go))

(defparameter *standard-output-bak* *standard-output*)
(defparameter *error-output-bak* *error-output*)
(defparameter *trace-output-bak* *trace-output*)




(defparameter *standard-output* (make-string-output-stream))
(defparameter *error-output* (make-string-output-stream))
(defparameter *trace-output* (make-string-output-stream))
)

*trace-output*

;; (defun print-stuff (x y)
;;   (format t "standard output ~a" x)
;;   (format *error-output* "error output ~a" y)
;;   (+ x y))

(defun capture (x y)
  (let ((*standard-output* (make-string-output-stream))
        (*error-output* (make-string-output-stream)))
    (values (demo-3d:do-go)
            (get-output-stream-string *standard-output*)
            (get-output-stream-string *error-output*))))
