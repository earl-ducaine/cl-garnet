

(in-package :xlib)

(defun pixmap-p (object)
  (typep object 'pixmap))

(defun image-z-p (object)
  (typep object 'image-z))

(export 'xlib::image-z-p :xlib)

(defun pixmap-plist (pixmap)
  (xlib:drawable-plist pixmap))

(defun (setf pixmap-plist) (value window)
  (setf (drawable-plist window) value))
