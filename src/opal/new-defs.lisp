
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


(defmacro update-info-invalid-p (object)
  `(logbitp 2 (update-info-bits ,object)))

(defsetf update-info-invalid-p (object) (value)
  `(bit-setter ,object 2 ,value))


(defmacro update-info-force-computation-p (object)
  `(logbitp 3 (update-info-bits ,object)))

(defsetf update-info-force-computation-p (object) (value)
  `(bit-setter ,object 3 ,value))
