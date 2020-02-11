
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


(defmacro update-info-on-fastdraw-list-p (object)
  `(logbitp 4 (update-info-bits ,object)))

(defsetf update-info-on-fastdraw-list-p (object) (value)
  `(bit-setter ,object 4 ,value))


(defun update-info-print-function (struct stream depth)
  (declare (ignore depth))
  (format stream "#<Update-Info dirty-p ~A invalid-p ~A>"
	(update-info-dirty-p struct)
	(update-info-invalid-p struct)))

(defstruct (win-update-info (:print-function win-update-info-print-function))
        fix-update-slots-objects
	invalid-view-objects
	invalid-objects
	invalid-xor-fastdraws
	invalid-copy-fastdraws
	invalid-slots
	new-bbox
	clip-mask-1
	clip-mask-2
	old-aggregate
        width
        height
	exposed-bbox)

(defun win-update-info-print-function (struct stream depth)
  (declare (ignore depth))
  (format stream "#<Win-UI v ~A o ~A x ~A c ~A s ~A f ~A>"
     (win-update-info-invalid-view-objects struct)
     (win-update-info-invalid-objects struct)
     (win-update-info-invalid-xor-fastdraws struct)
     (win-update-info-invalid-copy-fastdraws struct)
     (win-update-info-invalid-slots struct)
     (win-update-info-fix-update-slots-objects struct)
     ))

(defvar *free-cons* NIL)

(defvar *font-hash-table* (make-hash-table
			   :test #'equal
			   #+sb-thread :synchronized #+sb-thread t))
