
(in-package :xlib-lab)

(defun size (width height &optional device)
  "Destroy top window if it exists and create one of the specified size."
  (declare (ignore device))
  (when *top-win*
    (xlib:destroy-window *top-win*)
    (setf *top-win* nil))
  (setf *top-win* (create-window :width width :height height)))
