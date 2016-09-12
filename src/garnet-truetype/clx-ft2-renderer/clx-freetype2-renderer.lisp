;;;; clx-freetype2-renderer.lisp

(in-package #:clx-freetype2-renderer)

;;; "clx-freetype2-renderer" goes here. Hacks and glory await!

;; Taken from clx-truetype.lisp
(defun screen-dpi (screen)
  "Returns dpi for @var{screen}. ((pixel width)/(mm width))*25.4 mm/inch"
  (values (floor (* (xlib:screen-width screen) 25.4)
		 (xlib:screen-width-in-millimeters screen))
	  (floor (* (xlib:screen-height screen) 25.4)
		 (xlib:screen-height-in-millimeters screen))))

(defun set-face-size (font screen &optional size)
  (multiple-value-bind (dpi-x dpi-y)
	(screen-dpi screen)
    (with-slots (face-size ft-face) font
      (ft2:set-char-size ft-face (* (or size face-size) 64) 0 dpi-x dpi-y))))

(defun get-drawable-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ft2-surface)
      (setf (getf (xlib:drawable-plist drawable) :ft2-surface)
	    (xlib:render-create-picture
	     drawable
	     :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
								 :depth (xlib:drawable-depth drawable)))))))
(defun get-drawable-pen-picture (drawable)
  (or (getf (xlib:drawable-plist drawable) :ft2-pen)
      (setf (getf (xlib:drawable-plist drawable) :ft2-pen)
	    (xlib:render-create-picture
	     (or (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
		 (setf (getf (xlib:drawable-plist drawable) :ft2-pen-surface)
		       (xlib:create-pixmap
			:drawable drawable
			:depth (xlib:drawable-depth drawable)
			:width 1 :height 1)))
	     :format (first (xlib::find-matching-picture-formats (xlib:drawable-display drawable)
								 :depth (xlib:drawable-depth drawable)))
	     :repeat :on))))
(defun display-alpha-picture-format (display)
  (or (getf (xlib:display-plist display) :ft2-alpha-format)
      (setf (getf (xlib:display-plist display) :ft2-alpha-format)
	    (first
	     (xlib:find-matching-picture-formats
	      display
	      :depth 8 :alpha 8 :red 0 :blue 0 :green 0)))))

;; cf src/toy.lisp in cl-freetype2
(defun flat-array (arr)
  (make-array (apply #'* (array-dimensions arr))
	      :element-type (cadr (type-of arr))
              :displaced-to arr ))

(defun row-width (arr)
  (let ((dim (array-dimensions arr)))
    (if (> (array-rank arr) 2)
        (* (car dim) (caddr dim))
        (car dim))))

(defun ablit (arr1 arr2 &key (x 0) (y 0))
  "Destructivly copy arr2 into arr1 for 2- and 3-dimensional (Y:X, Y:X:RGB(A))
arrays.  X and Y may be specified as a 2D offset into ARR1."
  (assert (= (array-rank arr1) (array-rank arr2)))
  (let ((flat1 (flat-array arr1))
        (flat2 (flat-array arr2))
        (height1 (row-width arr1))
        (height2 (row-width arr2))
        (width1 (array-dimension arr1 1))
        (width2 (array-dimension arr2 1))
        (xoff (* x (if (= (array-rank arr1) 3)
                       (array-dimension arr1 2)
                       1))))
    (loop for y2 from 0 below height2
          for y1 from y below height1
          do (let ((x1 (+ (* y1 width1) xoff))
                   (x2 (* y2 width2)))
               (replace flat1 flat2
                        :start1 x1
                        :end1 (* (1+ y1) width1)
                        :start2 x2
                        :end2 (+ x2 width2)))))
  arr1)
(defun get-bearing-x (char face)
  "Returns the bearing-x of char. TODO: make sure this does what I want"
  (ft2:load-char face char '(:no-bitmap))
  (ft2::ft-glyph-metrics-hori-bearing-x 
   (ft2::ft-glyphslot-metrics 
    (ft2::ft-face-glyph face))))
;; StumpWM Font API wrappers

(defun open-font (display font-name)
  (declare (ignorable display))
  (make-instance 'font :family font-name))
(defun close-font (font))

(defun font-ascent (font)
  (round (ft2:face-ascender-pixels (font-face font))))
(defun font-descent (font)
  (round (ft2:face-descender-pixels (font-face font))))
(defun font-height (font)
  (- (font-ascent font) (font-descent font)))

