;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

;;

;; (defpackage :xlib-lab
;;   (:use :common-lisp :kr)
;;   (:export do-go do-stop))

(in-package :xlib-lab)
(defparameter agg nil)
(defparameter *top-win* nil)
(defparameter *xlib-display* nil)
(defparameter *pixmap-lab-std-out* *standard-output*)

;; (load "/home/rett/dev/garnet/cl-garnet/macro-patch.lisp")

;;;;(load "src/gem/anti-alias-graphics.lisp")
;; (xlib::describe-trace (get-the-xlib-display *top-window*))

(defun get-the-xlib-display (garnet-window)
  (gem::the-display garnet-window))


(defun display-trace-history ()
  (xlib::display-trace-history (get-the-xlib-display *top-window*)))

(defun run-draw-triangle-on-window ()
  (setf *top-win* (create-window 400 410))
  (draw-triangle-on-window *top-win*))

(defun interactive ()
  (ql:quickload :xoanon.gui.garnet)
  (load "/home/rett/dev/garnet/cl-garnet/pixmap-lab.lisp")
  (in-package :pixmap-lab)

  (trace xlib::get-put-items)
  (trace xlib::put-raw-image :break t)
  (run-draw-triangle-on-window)
  (sb-ext:exit)
  (run-draw-triangle-on-window))

(defun generate-polygon-sides (points)
  ;; closing side.
  (let* ((sides '()))
    (push `( ,@(first (reverse points)) ,@(first points)) sides)
    (reduce (lambda (last-point point)
	      (push `( ,@last-point ,@point) sides)
	      point)
	    points)
    (reverse sides)))

(defun draw-triangle-on-window (win)
  (let* ((height (xlib:drawable-height win))
	 (width (xlib:drawable-width win))
	 (cl-vector-image
	  (vector-create-polygon-on-surface
	   height width
	   #(150 200 255)
	   #(255 200 150)
	   (generate-polygon-sides *triagle-coordinates*))))
    (transfer-surface-window win cl-vector-image)))

(defparameter *pixmap-array* nil)

;;; (xlib:image-z-pixarray *pixmap-array*)

(defun translate-color (&key red green blue )
  (xlib:alloc-color *color-map*
		    (xlib:make-color :red red :green green :blue blue)))


(defparameter *triagle-coordinates*
  '((100 30)
    (10 40)
    (46 100)
    (100 145)))

(defun draw-on-window (image-z-pixarray x value)
    (setf (row-major-aref image-z-pixarray x)
	  value)
    image-z-pixarray)

(defun vector-create-polygon-on-surface (height width background-rgb
					 forground-rgb sides)
  (multiple-value-bind (state image) (gem::create-surface width height background-rgb)
    (dolist (side sides)
      (apply #'aa:line-f `(,state ,@side)))
    (aa:cells-sweep state (aa-misc:image-put-pixel image forground-rgb))
    image))

(defparameter *color-cache* (make-hash-table))

(defun get-color (r g b)
  (let* ((hash-key (+ (ash r 16) (ash g 8) b))
	 (hash-value (gethash hash-key *color-cache*)))
    (or hash-value
	(setf (gethash hash-key *color-cache*)
	      (xlib:alloc-color *color-map* (xlib:make-color :red (/ r 255)
							     :green (/ g 255)
							     :blue (/ b 255)))))))
(defun transfer-surface-window (win cl-vector-image)
  (let* ((height (xlib:drawable-height win))
	 (width (xlib:drawable-width win))
	 (pixmap-array (xlib:get-image
			win
			:x 0
			:y 0
			:width width
			:height height
			:format :z-pixmap)))
    (setf *pixmap-array* pixmap-array)
    (let ((image-z-pixarray (xlib:image-z-pixarray *pixmap-array*)))
      (dotimes (i (* height width))
	(setf (row-major-aref image-z-pixarray i)
	      (get-color (row-major-aref cl-vector-image (* i 3))
			 (row-major-aref cl-vector-image (+ (* i 3) 1))
			 (row-major-aref cl-vector-image (+ (* i 3) 2)))))
      (setf (xlib:image-z-pixarray *pixmap-array*) image-z-pixarray)
      (xlib:put-image  win
		       (xlib:create-gcontext :drawable win)
		       pixmap-array
		       :x 0
		       :y 0
		       :width (xlib:drawable-width win)
		       :height (xlib:drawable-height win))
      (display-force-output))))

(defun run-event-loop ()
  (inter:main-event-loop))


(defparameter xlib::*trace-log-list* '())

(defun xlib::write-to-trace-log (vector start end)
  (push (copy-seq (subseq vector start end)) xlib::*trace-log-list*))

(deftype buffer-bytes () `(simple-array (unsigned-byte 8) (*)))


(defparameter my-array
  (make-array 5
	      :element-type 'character
	      :adjustable t
	      :fill-pointer 3))

;; (make-array 6 :element-type 'sequence
;; 	    :fill-pointer 3)


(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

;; (print (starts-with-p "foobar" "foo")) ; T
;; (print (starts-with-p "foobar" "bar")) ; NIL

(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))

(defun get-symbols-defined-in-package (package)
  (let ((xlib-symbols '()))
    (do-symbols (symbol package)
      (if (string-equal (symbol-name :xlib)
			(package-name (symbol-package symbol)))
	  (push symbol xlib-symbols)))
    xlib-symbols))

(defun get-x-message-types-symbols ()
  (reduce (lambda (symbols symbol)
	    (let ((symbol-name (symbol-name symbol)))
	      (if (and (starts-with-p symbol-name "+X-")
		       ;; exclude x-render symbols
		       (not (starts-with-p symbol-name "+X-RENDER"))
		       (ends-with-p symbol-name "+"))
		  (cons symbol symbols)
		  symbols)))
	  (get-symbols-defined-in-package :xlib)
	  :initial-value '()))

(defun get-x-message-types-map ()
  (let ((hash-table (make-hash-table)))
    (dolist (symbol (get-x-message-types-symbols))
      (if (and (atom (symbol-value symbol))
	       (not (gethash (symbol-value symbol) hash-table)))
	  (setf (gethash (symbol-value symbol) hash-table)
		(symbol-name symbol))
	  (progn
	    (format t "Warning: (atom (symbol-value symbol)): ~S(~S)~%"
		    symbol (atom (symbol-value symbol)))
	    (format t "Warning: (not (gethash (symbol-value symbol) hash-table)): ~S(~S)~%"
		    symbol (not (gethash (symbol-value symbol) hash-table))))))
    hash-table))

(defparameter xlib-symbols (get-symbols-defined-in-package :xlib))
(defparameter x-message-types-map (get-x-message-types-map))

;;  (defconstant +x-polyfillarc+ 71))

(defun dissassemble-x-message (message-bytes)
  )
;;  (defconstant +x-polyfillarc+                  71)

(defun xlib::buffer-write (vector buffer start end)
  ;; Write out VECTOR from START to END into BUFFER
  ;; Internal function, MUST BE CALLED FROM WITHIN WITH-BUFFER
  (declare (type xlib::buffer buffer)
	   (type xlib::array-index start end))
  (when (xlib::buffer-dead buffer)
    (xlib::x-error 'closed-display :display buffer))
  (xlib::write-to-trace-log vector start end)
  (xlib::wrap-buf-output (buffer)
    (funcall (xlib::buffer-write-function buffer)
	     vector buffer start end))
  nil)

;; (defun do-stop ()
;;   ;;(opal:destroy top-win)
;;   )



;; Drawing on the screen using anti eliasing algorithms with
;; transparentcy has the following steps:

;; 1. requires that we get a snapshot of the current window.

;; (get-image <drawable>)

;; (as a zimage), convert the zimage to an array, draw on the array,
;; then replay the window with the new value.
;;(gem::the-display win)
