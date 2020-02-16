
(in-package :kr)

(defun formula-fn (form &optional (initial-value nil) meta)
  (locally (declare #.*special-kr-optimization*)
    (let ((formula (make-a-formula)))
      (set-formula-number formula 0)
      (setf (schema-name formula) (incf *schema-counter*))
      (setf (cached-value formula) initial-value)
      (setf (a-formula-meta formula) meta)
      (if (formula-p form)
	  ;; We were passed an object which is already a formula.  Link to it.
	  (progn
	    (setf (a-formula-is-a formula) form)
	    (setf (a-formula-function formula) (a-formula-function form))
	    (setf (a-formula-lambda formula) (a-formula-lambda form))
	    (push-one-or-list formula (a-formula-is-a-inv form)))
	  (progn
	    (setf (a-formula-function formula)
		  #-(or CMU ANSI-CL)
		  `(lambda () ,form)
		  #+(or CMU ANSI-CL)
		  (compile nil `(lambda () ,form)))
	    (setf (a-formula-lambda formula) form)))
      formula)))

(defmacro formula (form &optional (initial-value nil) &rest slots)
  (if slots
      `(formula-fn ,form ,initial-value (create-schema nil ,@slots))
      `(formula-fn ,form ,initial-value NIL)))

(declaim (inline slot-is-not-constant))
(defun slot-is-not-constant (schema slot)
  (let ((entry (slot-accessor schema slot)))
    (when entry
      (not (is-constant (sl-bits entry))))))

(defun gv-value-fn (schema slot)
  (locally (declare #.*special-kr-optimization*)
    (when (or (null schema) (deleted-p schema))
      nil
      )
    (let* ((setup T)
	   (entry (slot-accessor schema slot))
	   (value (if entry (sl-value entry) *no-value*)))
      (when (eq value *no-value*)
	(setf entry (slot-accessor schema slot))
	(when entry (setf value (sl-value entry))))
      (when *check-constants*
	(if (and entry (is-constant (sl-bits entry)))
	    (setf setup NIL)
	    (setf *is-constant* NIL))
	(setf *accessed-slots* T))
      (when (and setup *current-formula*)
	(unless entry
	  (setf entry (set-slot-accessor schema slot *no-value* 0 NIL)))
	(unless (full-sl-p entry)
	  (let ((full-entry (make-full-sl)))
	    (setf (gethash slot (schema-bins schema)) full-entry)
	    (setf (sl-name full-entry) slot)
	    (if entry
		(setf (sl-value full-entry) (sl-value entry)
		      (sl-bits full-entry) (sl-bits entry))
		(setf (sl-value full-entry) value
		      (sl-bits full-entry) *local-mask*))
	    (setf entry full-entry))))
      (unless (eq value *no-value*) value))))

(declaim (inline invalidate-demon))
(defun invalidate-demon (schema slot save)
  "This is the default invalidate demon."
  (kr-send schema :UPDATE-DEMON schema slot save))

(defun is-a-p (schema type)
  (locally (declare #.*special-kr-optimization*)
    (unless (and schema (schema-p schema))
      (return-from is-a-p nil))
    (when (or (eq type T)
	      (eq schema type))
      (return-from is-a-p T))
    (if (formula-p schema)
	(if (eq (a-formula-is-a schema) type)
	    T
	    (is-a-p (a-formula-is-a schema) type))
	(dolist (parent (g-value schema :IS-A))
	  (when (or (eq parent type)
		    ;; Not directly in the IS-A list: how about the parents?
		    (is-a-p parent type))
	    (return T))))))

(ADD-NEW-TYPE "KR-BOOLEAN" 'T 'T-P "Any value is legal")

(ADD-NEW-TYPE "KNOWN-AS-TYPE" '(OR KEYWORD NULL)
              #'(LAMBDA (VALUE)
                  (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
                  (OR (KEYWORDP VALUE) (NULL VALUE)))
              "[keyword]")


(ADD-NEW-TYPE "FIXNUM" '(SATISFIES SB-INT:FIXNUMP) 'SB-INT:FIXNUMP
              "Potential efficiency hack.")

(in-package "OPAL")

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
