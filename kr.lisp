
(defun g-value-inherit-values (schema slot)
  (declare (ftype (function (t &optional t) t) formula-fn))
  (dolist (parent (get-local-value schema :IS-A))
    (let ((entry (slot-accessor parent slot))
	  (value *no-value*)
	  bits
	  (intermediate-constant NIL))
      (when entry
	(setf value (sl-value entry))
	(when (is-constant (sl-bits entry))
	  (setf intermediate-constant T)))
      (if (eq value *no-value*)
	  (multiple-value-setq (value bits)
	    (g-value-inherit-values parent slot))
	  (setf bits (sl-bits entry)))
      (unless (eq value *no-value*)
	(return-from g-value-inherit-values (values value bits)))))
  (set-slot-accessor schema slot
		     nil
		     *is-parent-mask*
		     (slot-dependents nil))
  *no-value*)

(defun g-value-no-copy (schema slot &optional skip-local)
  (unless skip-local
    (let ((value (slot-accessor schema slot)))
      (when value
	(return-from g-value-no-copy (sl-value value)))))
  (dolist (*schema-self* (get-local-value schema :IS-A))
    (unless (eq *schema-self* schema)
      (let ((value (g-value-no-copy *schema-self* slot)))
	(when value
	  (return-from g-value-no-copy value))))))

(defun link-in-relation (schema slot values)
  (let ((inverse (if (eq slot :is-a)
		     :is-a-inv
		     (cadr (assocq slot *relations*)))))
    (when inverse
      (dolist (value values)
	(let* ((entry (if (eq slot :is-a)
			  (slot-accessor value :is-a-inv)
			  (slot-accessor value inverse)))
	       (previous-values (when entry (sl-value entry))))
	  (if entry
	      (if (or *schema-is-new*
		      (not (memberq schema previous-values)))
		  (if (eq (sl-value entry) *no-value*)
		      (setf (sl-value entry) (list schema))
		      (push schema (sl-value entry))))
	      (set-slot-accessor value inverse (list schema) *local-mask* nil)))))))

(defun check-relation-slot (schema slot values)
  (unless (listp values)
    (format
     t "S-VALUE: relation ~s in schema ~S should be given a list of values!~%"
     slot schema)
    (if (schema-p values)
	(setf values (list values))	; go ahead, use anyway.
	(return-from check-relation-slot *no-value*)))
  (dolist (value values)
    (unless (is-schema value)
      (when-debug
       (format
	t
	"S-VALUE: value ~s for relation ~s in ~s is not a schema!  Ignored.~%"
	value slot schema))
      (return-from check-relation-slot *no-value*)))
  (do ((value values (cdr value)))
      ((null value))
    (when (memberq (car value) (cdr value))
      (format
       t
       "Trying to set relation slot ~S in schema ~S with duplicate value ~S!~%"
       slot schema (car value))
      (format t "  The slot was not set.~%")
      (return-from check-relation-slot *no-value*)))
  values)


(declaim (inline inherited-p))
(defun inherited-p (schema slot)
  "Similar to HAS-SLOT-P, but when there is a formula checks whether this is
an inherited formula."
  (let ((entry (slot-accessor schema slot)))
    (when entry
      (or (is-inherited (sl-bits entry))
	  (and (formula-p (sl-value entry))
	       (formula-p (a-formula-is-a (sl-value entry))))))))

;;; encode types
(defparameter *types-table* (make-hash-table :test #'equal)
  "Hash table used to look up a Lisp type and returns its code")

(declaim (fixnum *types-array-inc*))
(defparameter *types-array-inc* 255) ;; allocate in blocks of this size

(declaim (fixnum *next-type-code*))
(defparameter *next-type-code*  0)   ;; next code to allocate

(defparameter types-array NIL
  "Array used to decode a number into its corresponding Lisp type.")

(defparameter type-fns-array NIL
  "Array used to decode a number into its corresponding type-fn.")

(defparameter type-docs-array NIL
  "Array used to decode a number into its corresponding documentation string.")

(declaim (inline code-to-type
		 code-to-type-fn))

(defun code-to-type (type-code)
  (svref types-array type-code))

(defun code-to-type-fn (type-code)
  (svref type-fns-array type-code))


(declaim (inline find-lisp-predicate))
(defun find-lisp-predicate (simple-type)
  "Given simple type ('NULL, 'KEYWORD, etc...), returns the name of
the lisp predicate to test this ('NULL, 'KEYWORDP, etc....)"
  (let ((p-name (concatenate 'string (symbol-name simple-type) "P"))
	(-p-name (concatenate 'string (symbol-name simple-type) "-P")))
    (cond ((memberq simple-type '(NULL ATOM)) simple-type)
	  (T (or (find-symbol p-name 'common-lisp)
		 (find-symbol -p-name 'common-lisp)
		 (find-symbol p-name)
		 (find-symbol -p-name)
		 (error "Could not find predicate for simple-type ~S~%"
			simple-type))))))

(defun type-to-fn (type)
  (let (code)
    (cond ((consp type)			; complex type
	   (if (eq (car type) 'SATISFIES)
	       (let ((fn-name (second type)))
		 `',fn-name) ;; koz
	       `(function (lambda (value)
		  (declare #.*special-kr-optimization*)
		  ,(make-lambda-body type)))))
	  ((setq code (gethash (symbol-name type) *types-table*))
	   (code-to-type-fn code))
	  (T
	   `',(find-lisp-predicate type)))))

(declaim (inline copy-extend-array))
(defun copy-extend-array (oldarray oldlen newlen)
  (let ((result (make-array newlen)))
    (dotimes (i oldlen)
      (setf (svref result i) (svref oldarray i)))
    result))

(defun get-next-type-code ()
  (let ((curlen (length types-array)))
    (when (>= *next-type-code* curlen)
      (let ((newlen (+ curlen *types-array-inc*)))
	(setf types-array (copy-extend-array types-array
					     curlen newlen)
	      type-fns-array (copy-extend-array
			      type-fns-array curlen newlen)
	      type-docs-array (copy-extend-array
			       type-docs-array
			       curlen
			       newlen))))
    (prog1
	*next-type-code*
      (incf *next-type-code*))))

(defun add-new-type (typename type-body type-fn)
  (let ((code (get-next-type-code)))
    (setf (gethash typename *types-table*) code)
    (setf (gethash type-body *types-table*) code)
    (setf (svref types-array code)
	  (if typename
	      (if (stringp typename)
		  (intern typename (find-package "CL-USER"))
		  typename)
	      type-body))
    (setf (svref type-fns-array  code)
	  (if (and (symbolp type-fn) ;; koz
		   (fboundp type-fn))
	      (symbol-function type-fn)
	      type-fn))
    code))

(declaim (inline slot-constant-p))
(defun slot-constant-p (schema slot)
  (let ((entry (slot-accessor schema slot)))
    (when entry
      (is-constant (sl-bits entry)))))

(defun s-value-fn (schema slot value)
  (locally (declare #.*special-kr-optimization*)
    (unless (schema-p schema)
      (return-from s-value-fn (values value t)))
    (let* ((entry (slot-accessor schema slot)))
      (let ((is-formula nil)
	    (is-relation nil)
	    (was-formula (formula-p nil)))
	(when (and (setf is-relation (relation-p slot))
		   (eq (setf value (check-relation-slot schema slot value)) *no-value*))
	  (return-from s-value-fn (values nil nil)))
	(when (formula-p value)
	  (setf is-formula T)
	  (setf (on-schema value) schema)
	  (setf (on-slot value) slot)
	  (unless (schema-name value)
	    (incf *schema-counter*)
	    (setf (schema-name value) *schema-counter*)))
	(cond
	  ((and was-formula (not is-formula))
	   (setf (cached-value nil) value)
	   (set-cache-is-valid nil NIL))
	  (t
	   (when (and is-formula (null (cached-value value)))
	     (setf (cached-value value)
		   (if was-formula (cached-value nil) nil)))
	   (when is-relation
	     (link-in-relation schema slot value))
	   (let ((new-bits (or the-bits *local-mask*)))
	     (if entry
		 (setf (sl-value entry) value
		       (sl-bits entry) new-bits)
		 (setf entry (set-slot-accessor schema
						slot value new-bits nil))))))
	(when (and the-bits (is-parent the-bits))
	  (update-inherited-values schema slot value T))
	(when (and was-formula (not is-formula))
	  (set-cache-is-valid nil T))
	(values value nil)))))

(defun internal-s-value (schema slot value)
  (let ((is-formula (formula-p value))
	(is-relation (relation-p slot)))
    (when is-relation
      (unless (listp value)
	(setf value (list value)))
      ;; Check for special cases in relation slots.
      (when (eq (setf value (check-relation-slot schema slot value)) *no-value*)
	(return-from internal-s-value NIL)))
    (when is-formula
      (setf (on-schema value) schema)
      (setf (on-slot value) slot))
    (set-slot-accessor schema slot value *local-mask* nil)
    ;; Take care of relations.
    (when is-relation
      (link-in-relation schema slot value))
    value))

(defun set-is-a (schema value)
  (when (eq (setf value (check-relation-slot schema :is-a value)) *no-value*)
    (return-from set-is-a NIL))
  (set-slot-accessor schema :IS-A value *local-mask* NIL)
  (link-in-relation schema :IS-A value)
  value)

(defun find-parent (schema slot)
  (dolist (a-parent (get-local-value schema :IS-A))
    (when a-parent
      (let ((value (LOCALLY
		       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
		     (LET* ((.a-local-var-alt. (SLOT-ACCESSOR A-PARENT SLOT))
			    (.a-local-var.
			     (IF .a-local-var-alt.
				 (IF (IS-INHERITED (SL-BITS .a-local-var-alt.))
				     (IF (A-FORMULA-P (SL-VALUE .a-local-var-alt.))
					 (SL-VALUE .a-local-var-alt.))
				     (SL-VALUE .a-local-var-alt.))
				 *NO-VALUE*)))
		       (IF (EQ .a-local-var. *NO-VALUE*)
			   (IF .a-local-var-alt.
			       (SETF .a-local-var. NIL)
			       (IF (NOT
				    (FORMULA-P
				     (SETF .a-local-var. (G-VALUE-INHERIT-VALUES A-PARENT SLOT))))
				   (SETF .a-local-var. NIL))))
		       (IF (A-FORMULA-P .a-local-var.)
			   NIL
			   .a-local-var.)))))
	(if value
	    (return-from find-parent (values value a-parent))
	    (multiple-value-bind (value the-parent)
		(find-parent a-parent slot)
	      (when value
		(return-from find-parent (values value the-parent)))))))))

(defun kr-init-method (schema  &optional the-function )
  (if the-function
      nil
      (multiple-value-setq (the-function)
	(find-parent schema :INITIALIZE)))
  (when the-function
    (funcall the-function schema)))

(defun allocate-schema-slots (schema)
  (locally (declare #.*special-kr-optimization*)
    (setf (schema-bins schema)
	  (make-hash-table :test #'eq
			   )))
  schema)

(defun make-a-new-schema (name)
  (locally (declare #.*special-kr-optimization*)
    (eval `(defvar ,name))
    (let ((schema (make-schema)))
      (allocate-schema-slots schema)
      (set name schema))))

(defun process-constant-slots (the-schema parents constants do-types)
  (locally (declare #.*special-kr-optimization*)
    (dolist (slot (g-value-no-copy the-schema :UPDATE-SLOTS))
      (let ((entry (slot-accessor the-schema slot)))
	(if entry
	    (setf (sl-bits entry) (set-is-update-slot (sl-bits entry)))
	    (set-slot-accessor the-schema slot *no-value*
			       (set-is-update-slot *local-mask*)
			       NIL))))
    (dolist (parent parents)
      (dolist (local (g-value-no-copy parent :LOCAL-ONLY-SLOTS))
	(unless (listp local)
	  (cerror "Ignore the declaration"
		  "create-instance (object ~S, parent ~S):  :local-only-slots
declarations should consist of lists of the form (:slot T-or-NIL).  Found
the expression ~S instead."
		  the-schema parent local)
	  (return))
	;; Set the slots marked as local-only
	(let ((slot (car local)))
	  (unless (slot-accessor the-schema slot)
	    (if (second local)
		;; Copy down the parent value, once and for all.
		(let* ((entry (slot-accessor parent slot))
		       (value (if entry (sl-value entry))))
		  (unless (formula-p value)
		    ;; Prevent inheritance from ever happening
		    (internal-s-value the-schema slot (g-value parent slot))))
		;; Avoid inheritance and set the slot to NIL.
		(internal-s-value the-schema slot NIL))))))
    (when do-types
      ;; Copy type declarations down from the parent(s), unless overridden
      ;; locally.
      (dolist (parent parents)
	(iterate-slot-value (parent T T nil)
	  value						 ;; suppress warning
	  (let ((bits (sl-bits iterate-slot-value-entry))) ; get parent's bits
	    ;; keep only the type information
	    (setf bits (logand bits *type-mask*))
	    (unless (zerop bits)
	      (let ((the-entry (slot-accessor the-schema slot)))
		(if the-entry
		    (let ((schema-bits (sl-bits the-entry)))
		      (when (zerop (extract-type-code schema-bits))
			;; Leave type alone, if one was declared locally.
			(setf (sl-bits the-entry)
			      (logior (logand schema-bits *all-bits-mask*)
				      bits))))
		    (set-slot-accessor the-schema slot *no-value* bits NIL))))))))))



(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun process-slots (slots)
    (let ((output nil)
	  (is-a nil))
      (do ((head slots (cdr head))
	   (types nil)
	   (had-types nil)
	   slot)
	  ((null head)
	   (if types
	       (push `(quote ,types) output)
	       (if had-types
		   (push :NONE output)
		   (push NIL output))))
	(setf slot (car head))
	(cond
	  ((keywordp slot)
	   (when (eq slot :DECLARE)
	     (pop head)
	     (dolist (declaration (if (listp (caar head))
				      (car head)
				      (list (car head))))
	       (format t "declaration ~s~%" declaration)
	       (case (car declaration)
		 (:TYPE
		  (setf had-types T)
		  (dolist (spec (cdr declaration))
		    (push spec types)))
		 ((:CONSTANT :IGNORED-SLOTS :LOCAL-ONLY-SLOTS
			     :MAYBE-CONSTANT :PARAMETERS :OUTPUT
			     :SORTED-SLOTS :UPDATE-SLOTS)
		  (setf output (merge-declarations declaration
						   (car declaration)
						   output)))))))
	  ((listp slot)
	   (if (eq (car slot) :IS-A)
	       (setf is-a (cadr slot))
	       (push `'(,(car slot) . ,(cadr slot)) output)))))
      (cons is-a output))))

(defun do-schema-body (schema is-a generate-instance override types
		       &rest slot-specifiers)
  (unless (listp is-a)
    (setf is-a (list is-a)))
  (when is-a
    (let ((*schema-is-new* T))
      (set-is-a schema is-a)))
  (do* ((slots slot-specifiers (cdr slots))
	(slot (car slots) (car slots)))
       ((null slots)
	(unless (eq types :NONE)
	  (dolist (type types)
	      (dolist (slot (cdr type))
		(set-slot-accessor schema slot *no-value* 33 nil))))
	(process-constant-slots schema is-a nil
				(not (eq types :NONE)))
	(kr-init-method schema))
    (let ((slot-name (car slot))
	  (slot-value (cdr slot)))
      (internal-s-value schema slot-name slot-value))))

(defun do-schema-body-alt (schema is-a &rest slot-specifiers)
  (let ((types nil))
    (setf is-a (list is-a))
    (set-is-a schema is-a)
    (do* ((slots slot-specifiers (cdr slots))
	  (slot (car slots) (car slots)))
	 ((null slots)
	  (process-constant-slots schema is-a nil (not (eq types :NONE)))
	  (dolist (slot slot-specifiers)
	    (when (and (listp slot)
		       (memberq (car slot)
				'(:ignored-slots :local-only-slots
				  :maybe-constant :parameters :output
				  :sorted-slots :update-slots)))))
	  (kr-init-method schema)
	  schema)
      (let ((slot-name (car slot))
	    (slot-value (cdr slot)))
	(internal-s-value schema slot-name slot-value)))))

(do-schema-body (make-a-new-schema 'graphical-object) nil t nil
                '(((or (is-a-p filling-style) null) :filling-style)
                  ((or (is-a-p line-style) null) :line-style)))

(do-schema-body (make-a-new-schema 'rectangle) graphical-object t nil nil
                (cons :update-slots '(:fast-redraw-p)))

(define-method :initialize graphical-object (gob)
	       (s-value-fn gob :update-info 'a))

(defstruct (sb-constraint)
  variables
  strength
  methods
  selected-method
  mark
  other-slots
  set-slot-fn)

(defun get-sb-constraint-slot (obj slot)
  (getf (sb-constraint-other-slots obj) slot nil))

(defmacro cn-variables (cn) `(sb-constraint-variables ,cn))

(defun set-sb-constraint-slot (cn slot val)
  (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn slot val)
  (setf (getf (sb-constraint-other-slots cn) slot nil) val)
  val)

(defsetf cn-variables (cn) (val)
  `(let ((cn ,cn)
	 (val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :variables val)
     (setf (sb-constraint-variables cn) val)))

(defstruct (sb-variable)
  constraints
  other-slots
  set-slot-fn)

(defun set-sb-variable-slot (var slot val)
  (call-set-slot-fn (sb-variable-set-slot-fn var) var slot val)
  (setf (getf (sb-variable-other-slots var) slot nil) val))

(defun call-set-slot-fn (fns obj slot val)
  (cond ((null fns)
	 nil)
	((listp fns)
	 (loop for fn in fns do (call-set-slot-fn fn obj slot val)))
	(t
	 (funcall fns obj slot val))))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun get-save-fn-symbol-name (sym)
    (cond ((symbolp sym)
	   (intern (concatenate 'string (symbol-name sym) "-SAVE-FN-SYMBOL")
		   (find-package :cl-user)))
	  ((and (consp sym)
		(eq :quote (first sym))
		(symbolp (second sym)))
	   (get-save-fn-symbol-name (second sym)))
	  (t (cerror "cont" "get-save-fn-symbol-name: bad symbol ~S" sym)))))

(defun install-hook (fn hook)
  (if  (fboundp fn)
       (let ((save-fn (get-save-fn-symbol-name fn)))
	 (unless (fboundp save-fn)
	   (setf (symbol-function save-fn)
		 (symbol-function fn)))
	 (setf (symbol-function fn)
	       (symbol-function hook)))))

(defmacro os (obj slot) `(cons ,obj ,slot))

(defmacro os-p (os)
  `(let ((os ,os))
     (and (consp os)
	  (schema-p (car os)))))

(defmacro os-object (os) `(car ,os))
(defmacro os-slot (os) `(cdr ,os))
(defmacro cn-os (v) `(get-sb-constraint-slot ,v :mg-os))
(defmacro cn-connection (v) `(get-sb-constraint-slot ,v :mg-connection))
(defmacro cn-variable-paths (c) `(get-sb-constraint-slot ,c :mg-variable-paths))
(defsetf cn-os (v) (val) `(set-sb-constraint-slot ,v :mg-os ,val))
(defsetf cn-connection (v) (val) `(set-sb-constraint-slot ,v :mg-connection ,val))

(defsetf cn-variable-paths (c) (val)
  `(set-sb-constraint-slot ,c :mg-variable-paths ,val))

(defun create-mg-constraint (&key variable-paths variable-names)
  (let ((cn (make-sb-constraint)))
    (setf (cn-connection cn) :unconnected)
    (setf (cn-variable-paths cn) variable-paths)
    cn))

(defsetf var-os (v) (val)
  `(set-sb-variable-slot ,v :mg-os ,val))

(defun create-mg-variable ()
  (let* ((var (make-sb-variable)))
    (setf (VAR-os var) nil)
    var))

(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (CN-connection obj)))))

(defun add-constraint-to-slot (obj slot cn)
  (setf (CN-os cn) (os obj slot))
  (connect-constraint cn))

(defun kr-init-method-hook (schema)
  (let ((parent (car (get-value schema :is-a))))
    (doslots (slot parent)
      (let ((value (get-local-value parent slot)))
	(s-value-fn-save-fn-symbol schema slot value))))
  (activate-new-instance-cns schema))

(defun activate-new-instance-cns (schema)
  (doslots (slot schema)
    (let ((value (get-local-value schema slot)))
      (if (constraint-p value)
	  (add-constraint-to-slot schema slot value)))))

(defun connect-constraint (cn)
  (let* ((cn-var-paths (CN-variable-paths cn)))
    (let* ((root-obj (os-object (CN-os cn)))
	   (cn-path-links nil)
	   (paths-broken nil)
	   var-os-list)
      (setf var-os-list
	    (loop for path in cn-var-paths collect
		 (let ((obj root-obj))
		   (loop for (slot next-slot) on path do
			(when (null next-slot)
			  (return (os obj slot)))
			(set-object-slot-prop obj slot :sb-path-constraints
					      (adjoin cn nil))
			(push (os obj slot) cn-path-links)
			(s-value-fn-save-fn-symbol obj slot (g-value obj slot))
			(setf obj (g-value obj slot))))))
      (setf (CN-variables cn)
	    (loop for var-os in var-os-list
	       collect (create-object-slot-var
			(os-object var-os)
			(os-slot var-os))))
      (setf (CN-connection cn) :connected))))

(defun set-object-slot-prop (obj slot prop val)
  (let* ((os-props (g-value-body OBJ :SB-OS-PROPS))
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    val))

(defun create-object-slot-var (obj slot)
  (let ((var (create-mg-variable)))
    (set-object-slot-prop obj slot :sb-variable var)
    (s-value-fn-save-fn-symbol obj slot (g-value obj slot))
    var))

(eval-when (:load-toplevel :execute)
  (loop for (fn hook-fn) on '(s-value-fn s-value-fn-save-fn-symbol
			      kr-init-method kr-init-method-hook) by #'CDDR
     do (install-hook fn hook-fn)))

(do-schema-body-alt
    (make-a-new-schema '*axis-rectangle*) rectangle
    (cons :height-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:height))
	   :variable-names '(box height)))
    (cons :width-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:width))
	   :variable-names
	   '(box width)))
    (cons :top-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:top))
	   :variable-names '(box top)))
    (cons :left-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:left))
	   :variable-names '(box left))))
