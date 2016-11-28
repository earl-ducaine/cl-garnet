;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-

;;

(defpackage :pixmap-lab
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(in-package :xlib-lab)
(defparameter agg nil)
(defparameter *top-win* nil)
(defparameter *xlib-display* nil)
(defparameter *pixmap-lab-std-out* *standard-output*)

(load "/home/rett/dev/garnet/cl-garnet/macro-patch.lisp")


;;;;(load "src/gem/anti-alias-graphics.lisp")
;; (xlib::describe-trace (get-the-xlib-display *top-window*))

(defun get-the-xlib-display (garnet-window)
  (gem::the-display garnet-window))


(defun display-trace-history ()
  (xlib::display-trace-history (get-the-xlib-display *top-window*)))

(defun run-draw-triangle-on-window ()
  (when *top-win*
    (opal:destroy *top-win*))
  (setf *top-win* (create-window 400 410))
    (draw-triangle-on-window *top-win*))

(defparameter *triagle-coordinates*
  '((40 30)
    (10 40)
    (46 60)
    (50 45)))



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
  (let* ((height (gv win :height))
	 (width (gv win :width))
	 (cl-vector-image
	  (gem::vector-create-polygon-on-surface
	   height width
	   #(150 200 255)
	   #(30 10 0)
	   (generate-polygon-sides *triagle-coordinates*))))
    (gem::transfer-surface-window win cl-vector-image)))

(defun create-window (height width)
  (let ((top-win (create-instance nil opal::window
		   (:left 500)
		   (:top 100)
		   (:double-buffered-p t)
		   (:width width)
		   (:height height)
		   (:title "GARNET Animator Demo")
		   (:icon-title "Animator"))))
;;    (let ((agg (create-instance NIL opal:aggregate)))
;;;      (s-value top-win :aggregate agg)
      (opal:update top-win)
      top-win))

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
