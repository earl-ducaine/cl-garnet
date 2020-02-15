
(in-package "OPAL")

(defstruct bbox
  (x1 0 :type fixnum)
  (y1 0 :type fixnum)
  (x2 0 :type fixnum)
  (y2 0 :type fixnum)
  (valid-p nil :type (or t nil)))

(defstruct (update-info (:print-function update-info-print-function))
	window
	old-bbox
	bits)
(defconstant number-of-slots-of-update-info-struct 3)

(defmacro bit-setter (object bit-position value)
  (cond ((eq value T)
	 ;; Value is T at compile time.
	 `(setf (update-info-bits ,object)
		(logior (update-info-bits ,object) ,(ash 1 bit-position))))
	((null value)
	 ;; Value is NIL at compile time.
	 `(setf (update-info-bits ,object)
		(logand (update-info-bits ,object)
			,(lognot (ash 1 bit-position)))))
	(t
	 ;; Value is not known at compile time
	 `(if ,value
	      (setf (update-info-bits ,object)
		    (logior (update-info-bits ,object) ,(ash 1 bit-position)))
	      (setf (update-info-bits ,object)
		    (logand (update-info-bits ,object)
			    ,(lognot (ash 1 bit-position))))))))

(defmacro update-info-dirty-p (object)
  `(logbitp 0 (update-info-bits ,object)))

(defsetf update-info-dirty-p (object) (value)
  `(bit-setter ,object 0 ,value))


(defmacro update-info-aggregate-p (object)
  `(logbitp 1 (update-info-bits ,object)))

(defsetf update-info-aggregate-p (object) (value)
  `(bit-setter ,object 1 ,value))

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
  (let ((temp-info (make-update-info)))
    (setf (update-info-bits temp-info) 0)
    (setf (update-info-old-bbox temp-info) (make-bbox))
    (s-value gob :update-info temp-info)))
