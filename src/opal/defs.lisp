
(in-package "OPAL")

(defconstant +twopi+ (min (* 2 pi) (coerce (* 2 pi) 'short-float)))

(defvar *colormap-index-table*
  (make-hash-table :size 256))

(declaim (fixnum *halftone-table-size*))
(defvar *halftone-table-size* 17)
(defvar *halftone-table* nil)		; used to be set to (build-halftone-table)
					; but now that's a forward reference.  So,
					; now we setq this after defining that fn.

(defvar *default-text-extents* (make-list 9 :initial-element 0))

(defvar no-fill nil)
(defvar no-line nil)

(defvar *garnet-windows* NIL)

;; debugging tools
(defvar *event-debug* nil)
(defvar *expose-throw-aways* 0)

(defvar diamond-fill NIL)

(defstruct (HALFTONE (:print-function halftone-print))
  (percent 0)
  (device-image nil)
  (filling-style nil))

(defstruct CUT-STRING
  (string)
  (width 0 :type fixnum)
  (left-bearing))


(defsetf bottom (gob) (value)
  `(setf (g-value ,gob :top) (1+ (- ,value (g-value-fixnum ,gob :height)))))

(defsetf right (gob) (value)
  `(setf (g-value ,gob :left) (1+ (- ,value (g-value-fixnum ,gob :width)))))

;; The accessors for the sides of the gob adjust both the dimensions, and
;; position of the gob based on the given value.

(defsetf left-side (gob) (value)
  `(progn
     (setf (g-value ,gob :width)
           (- (g-value-fixnum ,gob :width) (- ,value (g-value-fixnum ,gob :left))))
     (setf (g-value ,gob :left) ,value)))

(defsetf top-side (gob) (value)
  `(progn
     (setf (g-value ,gob :height)
           (- (g-value-fixnum ,gob :height) (- ,value (g-value-fixnum ,gob :top))))
     (setf (g-value ,gob :top) ,value)))

;; The following allow access and setting to the gobs center
;; position.

(defsetf center-x (gob) (value)
  `(setf (g-value ,gob :left)
         (- ,value (truncate (g-value-fixnum ,gob :width) 2))))


(defstruct (update-info (:print-function update-info-print-function))
	window
	old-bbox
	bits)

(create-instance 'view-object nil
  :declare ((:type
		   (fixnum :hit-threshold)
		   (known-as-type :known-as)
		   (kr-boolean :visible))
	    (:local-only-slots (:window nil) (:parent nil))))

(create-instance 'graphical-object view-object
  :declare ((:type
	     ((or (is-a-p line-style) null) :line-style)
	     ((or (is-a-p filling-style) null) :filling-style)
	     ((member  :copy :xor :no-op :or :clear :set :copy-inverted
		      :invert :and :equiv :nand :nor :and-inverted
		      :and-reverse :or-inverted :or-reverse)
	      :draw-function))))

(create-instance 'rectangle graphical-object
  :declare ((:update-slots :fast-redraw-p :top)))

(define-method :initialize view-object (gob)
    (s-value gob :update-info (make-update-info)))
