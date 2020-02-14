
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

(defun make-new-formula ()
  (let ((f (formula-pop)))
    (if f
	;; Reuse a formula
	(progn
	  (setf (a-formula-depends-on f) nil)
	  (setf (a-formula-cached-value f) nil)
	  (setf (a-formula-path f) nil)
	  (setf (a-formula-is-a f) nil)
	  (setf (a-formula-function f) nil)
	  (setf (a-formula-lambda f) nil)
	  (setf (a-formula-is-a-inv f) nil))
	;; No formulas to reuse
	(setf f (make-a-formula))
	)
    (set-formula-number f 0)
    f))

(defun formula-fn (form &optional (initial-value nil) meta)
  "Creates an interpreted formula.  The <form> can be either a Lisp expression
 (which is used as the body of the formula), or another formula.  In the
 latter case, the other formula is made the parent, and this function
 creates an inherited formula.  The <initial-value>, which defaults to nil,
 is used as the initial cached value before the formula is evaluated."
  (locally (declare #.*special-kr-optimization*)
    (let ((formula (make-new-formula)))
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

(defun change-formula (schema slot form)
  (let ((formula (get-value schema slot)))
    (when (formula-p formula)
      (when (a-formula-is-a formula)
	;; This function was inherited.  Cut the IS-A link.
	(let* ((parent (a-formula-is-a formula))
	       (inv (a-formula-is-a-inv parent)))
	  (setf (a-formula-is-a-inv parent)
		(if (listp inv)
		    (delete formula inv)
		    (if (eq inv formula) NIL inv))))
	(setf (a-formula-is-a formula) NIL))
      (setf (a-formula-function formula) `(lambda () ,form))
      (setf (a-formula-lambda formula) form))))


(defun copy-formula (formula)
  "Makes and returns a copy of the <formula>, keeping the same initial value
and the same parent (if any)."
  (let* ((parent (a-formula-is-a formula))
	 (value (a-formula-cached-value formula))
	 (new (formula parent value)))
    (unless parent
      ;; Copy lambda expression and compiled lambda.
      (setf (a-formula-function new) (a-formula-function formula))
      (setf (a-formula-lambda new) (a-formula-lambda formula)))
    (let ((meta (a-formula-meta formula)))
      (when meta
	(let ((new-meta (create-schema nil)))
	  (setf (a-formula-meta new) new-meta)
	  (doslots (slot meta)
	    (s-value new-meta slot (g-value meta slot))))))
    new))

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
      (when (a-formula-p value)
	;; we are working with a formula
	(setf value (g-value-formula-value schema slot value entry)
	      entry (slot-accessor schema slot)))
      (when *check-constants*
	(if (and entry (is-constant (sl-bits entry)))
	    ;; Constant, so do NOT set up dependencies.
	    (setf setup NIL)
	    ;; Not constant
	    (setf *is-constant* NIL))
	(setf *accessed-slots* T))
      ;; Now set up the dependencies.
      (when (and setup *current-formula*) ; do we need to set up dependencies?
	(unless entry
	  (setf entry (set-slot-accessor schema slot *no-value* 0 NIL)))
	(unless (full-sl-p entry)
	  ;; We did have an entry, but it was too small.
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

(defun initialize-kr ()
  "Called once at the 'beginning.'"
  (setf *relations* nil)
  (setf *inheritance-relations* nil)
  (create-relation :IS-A T :IS-A-INV)
  (create-schema 'PRINT-SCHEMA-CONTROL
    (:sorted-slots :left :top :width :height)
    (:limit-values '(:IS-A-INV 5) '(:COMPONENTS 20))
    (:global-limit-values 10)))

(initialize-kr)

(defun is-a-p (schema type)
  "Tests whether the <schema> is linked via :IS-A to schema <type>, either
  directly or through several links.  Note that (is-a-p <schema> T) returns
  true if <schema> is a schema. FMG Check that it actually is a schema, otherwise
  return NIL. This seems not to break anything but changes the behavior of this
  function."
  (locally (declare #.*special-kr-optimization*)
    ;; Profiling indicated that this function is expensive, so I tried to
    ;; avoid unnecessary repetition of tests and exit as early as possible.

    (unless (and schema (schema-p schema))
      (return-from is-a-p nil))

    ;; We know we've got something, and it's a schema. So if type is T
    ;; or if the schema is eq to the type, return T.
    (when (or (eq type T)
	      (eq schema type))		; (is-a-p foo foo) is true
      (return-from is-a-p T))

    ;; The schema itself is not eq to TYPE so we have to check
    ;; the parents (inheritance).
    (if (formula-p schema)
	;; A formula (a formula is a schema).
	(if (eq (a-formula-is-a schema) type)
	    T
	    ;; No multiple inheritance, so there's only
	    ;; one parent.
	    (is-a-p (a-formula-is-a schema) type))
	;; A schema.

	;; This seems to implement a breadth-first search. I'm not sure how much
	;; it matters but it seems like the IS-A tree would be bushy downward,
	;; not upward, so it's better to just iterate through the IS-A list
	;; once, calling is-a-p on the parents if they aren't eq to TYPE.
	#-(and)
	(or (dolist (parent (g-value schema :IS-A))
	      (when (eq parent type)
		(return T)))
	    ;; Not directly in the list: how about the parents?
	    (dolist (parent (g-value schema :IS-A))
	      (when (is-a-p parent type)
		(return t))))

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
	(let ((formula (get-value object slot))
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
