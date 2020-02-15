
(in-package :kr)

(defvar *setup-dependencies* T
  "If T (the default), dependencies are set up whenever GV and GVL are
   evaluated inside formulas.  If nil, no dependencies are set up.")

(defun fixed-path-accessor (schema slots path-number)
  (let* ((current (a-formula-path *current-formula*))
	 (length (length current)))
    (or (and (< path-number length)
	     (elt current path-number))
	(progn
	  (dolist (slot slots)
	    (setf schema (g-value schema slot))
	    (when (listp schema)
	      ;; This handles relation slots, which are ALWAYS stored as
	      ;; a list.
	      (setf schema (first schema))))
	  (unless (> length path-number)
	    ;; create more storage
	    (setf current
		  (setf (a-formula-path *current-formula*)
			(append current
				(make-list (- path-number length -1))))))
	  (setf (elt current path-number) schema)
	  schema))))

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

(defun i-depend-on (object slot)
  "Given an object and a slot, if the <slot> contains a formula it returns
all the slots upon which the formula depends.  The result is a list of
dotted pairs, where each pair consists of a schema and a slot."
  (locally (declare #.*special-kr-optimization*)
    (if (schema-p object)
	(let ((formula (LOCALLY
		     (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
		   (LET* ((.local-var-alt. (SLOT-ACCESSOR SCHEMA SLOT))
			  (.local-var.
			   (IF .local-var-alt.
			       (IF (IS-INHERITED (SL-BITS .local-var-alt.))
				   (IF (A-FORMULA-P (SL-VALUE .local-var-alt.))
				       (SL-VALUE .local-var-alt.))
				   (SL-VALUE .local-var-alt.))
			       *NO-VALUE*)))
		     (IF (EQ .local-var. *NO-VALUE*)
			 (IF .local-var-alt.
			     (SETF .local-var. NIL)
			     (IF (NOT
				  (FORMULA-P (SETF .local-var. (G-VALUE-INHERIT-VALUES SCHEMA SLOT))))
				 (SETF .local-var. NIL))))
		     (IF (A-FORMULA-P .local-var.)
			 NIL
			 .local-var.))))
	      (dependencies nil))
	  (when (formula-p formula)
	    (do-one-or-list (schema (a-formula-depends-on formula))
	      (iterate-slot-value (schema T T T)
		(unless (eq value *no-value*)
		  (do-one-or-list (f (slot-dependents
				      kr::iterate-slot-value-entry))
		    (when (eq f formula)
		      (push (cons schema slot) dependencies)))))))
	  dependencies)
	;; An error
	(cerror
	 "Return NIL"
	 "I-DEPEND-ON called on the ~:[non-~;destroyed ~]object ~S."
	 (is-schema object) object))))


(declaim (inline self-old-value))
(defun self-old-value ()
  "Returns the cached value of a formula."
  (when *current-formula*
    (let ((value (a-formula-cached-value *current-formula*)))
      (if (eq value *no-value*)
	  NIL
	  value))))



(def-kr-type kr-no-type () '(satisfies no-type-error-p)
	     "No type defined for this slot")

;; We want 0 to mean "no type".
(setf (aref types-array 0) NIL)

;; Make this the first type
(def-kr-type kr-boolean () T
	     "Any value is legal")

(dolist (type '(null string keyword integer number list cons schema))
  (encode-type type))
