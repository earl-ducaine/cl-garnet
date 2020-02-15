
(in-package :kr)

(defun g-value-inherit-values (schema slot)
  (declare (ftype (function (t &optional t) t) formula-fn))
  (let (has-parents)
    (dolist (relation *inheritance-relations*)
      (dolist (parent (if (eq relation :IS-A)
			  (get-local-value schema :IS-A)
			  (get-local-value schema relation)))
	(setf has-parents T)
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
	    (return-from g-value-inherit-values (values value bits))))))
    (set-slot-accessor schema slot
		       nil
		       *is-parent-mask*
		       (slot-dependents nil))
    *no-value*))

(defun g-value-no-copy (schema slot &optional skip-local)
  (unless skip-local
    ;; Is there a local value?
    (let ((value (slot-accessor schema slot)))
      (when value
	(return-from g-value-no-copy (sl-value value)))))
  ;; Now try inherited values.
  (dolist (relation *inheritance-relations*)
    (dolist (*schema-self* (if (eq relation :IS-A)
			       (get-local-value schema :IS-A)
			       (get-local-value schema relation)))
      (unless (eq *schema-self* schema)	; avoid infinite loops!
	(let ((value (g-value-no-copy *schema-self* slot)))
	  (when value
	    (return-from g-value-no-copy value)))))))

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
  "We are setting the <slot> (a relation) to <values>.  Check that the
latter contains valid relation entries.
RETURNS: <values> (or a list of a single value, if <values> is not a list)
if success; *no-value* if failure."
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

(defmacro with-types-table-lock-held ((table) &body body)
  `(let ((,table *types-table*))
     ,@body))

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

(declaim (inline code-to-type code-to-type-fn code-to-type-doc check-kr-type))

(defun code-to-type (type-code)
  (svref types-array type-code))

(defun code-to-type-fn (type-code)
  (svref type-fns-array type-code))

(defun code-to-type-doc (type-code)
  (svref type-docs-array type-code))

(defun check-kr-type (value code)
  (funcall (code-to-type-fn code) value))


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

(defun make-lambda-body (complex-type)
  (with-types-table-lock-held (types-table)
    (let (code)
      (cond ((consp complex-type)	;; complex type (a list)
	     (let ((fn   (first complex-type))
		   (args (rest  complex-type)))
	       (case fn
		 ((OR AND NOT)
		  (cons fn (mapcar #'make-lambda-body args)))
		 (MEMBER
		  `(memberq value ',args))
		 ((IS-A-P IS-A)
		  `(is-a-p value ,(second complex-type)))
		 (SATISFIES
		  `(,(second complex-type) value))
		 ((INTEGER REAL)
		  (let* ((pred (find-lisp-predicate fn))
			 (lo (first args))
			 (lo-expr (when (and lo (not (eq lo '*)))
				    (if (listp lo)
					`((< ,(car lo) value))
					`((<= ,lo value)))))
			 (hi (second args))
			 (hi-expr (when (and hi (not (eq hi '*)))
				    (if (listp hi)
					`((> ,(car hi) value))
					`((>= ,hi value))))))
		    (if (or lo-expr hi-expr)
			`(and (,pred value) ,@lo-expr ,@hi-expr)
			`(,pred value))))
		 (T  (error "Unknown complex-type specifier: ~S~%" fn)))))
	    ((setq code (gethash (symbol-name complex-type) types-table))
	     (make-lambda-body (code-to-type code)))
	    (T
	     (list (find-lisp-predicate complex-type) 'value))))))

(defun type-to-fn (type)
  "Given the Lisp type, construct the lambda expr, or return the
   built-in function"
  (with-types-table-lock-held (types-table)
    (let (code)
      (cond ((consp type)			; complex type
	     (if (eq (car type) 'SATISFIES)
		 (let ((fn-name (second type)))
		   `',fn-name) ;; koz
		 `(function (lambda (value)
		    (declare #.*special-kr-optimization*)
		    ,(make-lambda-body type)))))
	    ((setq code (gethash (symbol-name type) types-table))
	     ;; is this a def-kr-type?
	     (code-to-type-fn code))
	    (T
	     `',(find-lisp-predicate type))))))

(declaim (inline copy-extend-array))
(defun copy-extend-array (oldarray oldlen newlen)
  (let ((result (make-array newlen)))
    (dotimes (i oldlen)
      (setf (svref result i) (svref oldarray i)))
    result))


(defun get-next-type-code ()
  "Return the next available type-code, and extend the type arrays
if necessary."
  (let ((curlen (length types-array)))
   (when (>= *next-type-code* curlen)
    ;; out of room, allocate more space
    (let ((newlen (+ curlen *types-array-inc*)))
     (setf types-array     (copy-extend-array types-array     curlen newlen)
           type-fns-array  (copy-extend-array type-fns-array  curlen newlen)
           type-docs-array (copy-extend-array type-docs-array curlen newlen))))
   ;; in any case, return current code, then add one to it
   (prog1
       *next-type-code*
     (incf *next-type-code*))))

(defun add-new-type (typename type-body type-fn &optional type-doc)
  "This adds a new type, if necessary
Always returns the CODE of the resulting type (whether new or not)"
  (with-types-table-lock-held (types-table)
    (let ((code (gethash (or typename type-body) types-table)))
      (if code
	  (if (equal (code-to-type code) type-body)
	      (return-from add-new-type code)
	      nil)
	  (progn
	    (setq code (or (gethash type-body types-table)
			   (get-next-type-code)))
	    (setf (gethash typename types-table) code)))
      (unless (gethash type-body types-table)
	(setf (gethash type-body types-table) code))
      (setf (svref types-array code)
	    (if typename
		(if (stringp typename)
		    (intern typename (find-package "KR"))
		    typename)
		type-body))
      (setf (svref type-docs-array code) (or type-doc NIL))
      (setf (svref type-fns-array  code)
	    (if (and (symbolp type-fn) ;; koz
		     (fboundp type-fn))
		(symbol-function type-fn)
		type-fn))
      code)))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun encode-type (type)
    "Given a LISP type, returns its encoding."
    (with-types-table-lock-held (types-table)
      ;; if there, just return it!
      (cond ((gethash type types-table))
	    ((and (listp type) (eq (car type) 'SATISFIES))
	     ;; add new satisfies type
	     (add-new-type NIL type (type-to-fn type)))
	    ((symbolp type)
	     (or (gethash (symbol-name type) types-table)
		 (let ((predicate (find-lisp-predicate type)))
		   (when predicate
		     (add-new-type NIL type predicate)))
		 nil
		 ))
	    (T)))))

(defun set-type-documentation (type string)
  "Add a human-readable description to a Lisp type."
   (setf (aref type-docs-array (encode-type type)) string))


(defun get-type-documentation (type)
  "RETURNS: the documentation string for the internal number <type>."
  (aref type-docs-array (encode-type type)))

(declaim (inline slot-is-constant))
(defun slot-is-constant (schema slot)
  (let ((entry (slot-accessor schema slot)))
    (is-constant (sl-bits entry))))

(declaim (inline mark-as-changed))
(defun mark-as-changed (schema slot)
  (let ((entry (slot-accessor schema slot)))
    (when (and entry (is-parent (sl-bits entry)))
      (update-inherited-values schema slot (sl-value entry) T)))
  (propagate-change schema slot))

(defun propagate-change (schema slot)
  (let ((entry (slot-accessor schema slot)))
    (do-one-or-list (formula (slot-dependents entry) T)
      (if (and (not-deleted-p formula) (cache-is-valid formula))
	(let* ((new-schema (on-schema formula))
	       (new-slot (on-slot formula))
	       (schema-ok (schema-p new-schema))
	       (new-entry  NIL))
	  (unless (and new-schema new-slot)
	    (continue-out))
	  (if schema-ok
	      (setf new-entry (slot-accessor new-schema new-slot)))
	  (set-cache-is-valid formula nil)
	  (if (and schema-ok new-entry)
	    (if (slot-dependents new-entry)
	      (propagate-change new-schema new-slot))))))))

(defun visit-inherited-values (schema a-slot function)
  (let* ((entry (slot-accessor schema a-slot))
	 (parent-entry (when entry (sl-value entry))))
    (dolist (inverse *inheritance-inverse-relations*)
      (dolist (child (if (eq inverse :IS-A-INV)
			 (get-local-value schema :IS-A-INV)
			 (get-local-value schema inverse)))
	(let* ((entry (slot-accessor child a-slot))
	       (value (when entry (sl-value entry))))
	  (when (and value
		     (is-inherited (sl-bits entry))
		     (eq value parent-entry))
	    (visit-inherited-values child a-slot function)
	    (funcall function child a-slot)))))))

(declaim (inline slot-constant-p))
(defun slot-constant-p (schema slot)
  (let ((entry (slot-accessor schema slot)))
    (when entry
      (is-constant (sl-bits entry)))))

(defun s-value-fn (schema slot value)
  (locally (declare #.*special-kr-optimization*)
    (unless (schema-p schema)
      (return-from s-value-fn (values value t)))
    (let* ((entry (slot-accessor schema slot))
	   (old-value
	    nil
	     )
	   is-depended)
      (let ((is-formula nil) (is-relation nil)
	    (was-formula (formula-p old-value)))
	(when (and (setf is-relation (relation-p slot))
		   (eq (setf value (check-relation-slot schema slot value)) *no-value*))
	  (return-from s-value-fn (values old-value nil)))
	(when (formula-p value)
	  (setf is-formula T)
	  (setf (on-schema value) schema)
	  (setf (on-slot value) slot)
	  (unless (schema-name value)
	    (incf *schema-counter*)
	    (setf (schema-name value) *schema-counter*)))
	(when (and (slot-accessor schema slot)
		   (is-update-slot (sl-bits entry)))
	  (get-value schema :invalidate-demon))
	(cond
	  ((and was-formula (not is-formula))
	   (setf (cached-value old-value) value)
	   (set-cache-is-valid old-value NIL))
	  (t
	   (when (and is-formula (null (cached-value value)))
	     (setf (cached-value value)
		   (if was-formula (cached-value old-value) old-value)))
	   (when is-relation
	     (link-in-relation schema slot value))
	   (let ((new-bits (or the-bits *local-mask*)))
	     (if entry
		 ;; This is a special slot - just set it
		 (setf (sl-value entry) value
		       (sl-bits entry) new-bits)
		 ;; This is not a special slot.
		 (setf entry (set-slot-accessor schema
						slot value new-bits nil))))))
	(when (and the-bits (is-parent the-bits))
	  (update-inherited-values schema slot value T))
	(when is-depended
	  (propagate-change schema slot)) ;;)
	(when (and was-formula (not is-formula))
	  (set-cache-is-valid old-value T))
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
  (dolist (relation *inheritance-relations*)
    (dolist (a-parent (if (eq relation :is-a)
			  (get-local-value schema :IS-A)
			  (get-local-value schema relation)))
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
		  (return-from find-parent (values value the-parent))))))))))

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

(defun merge-declarations (declaration keyword output)
  (let ((old (find keyword output :key #'second)))
    (if old
	(setf (cadr (third old)) (union (cdr declaration)
					(cadr (third old))))
	(push `(cons ,keyword ',(cdr declaration)) output)))
  output)


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

(defun do-schema-body (schema is-a generate-instance override types &rest slot-specifiers)
  (when (equal is-a '(nil))
    (format
     t
     "*** (create-instance ~S) called with an illegal (unbound?) class name.~%"
     schema)
    (setf is-a NIL))
  (unless (listp is-a)
    (setf is-a (list is-a)))
  (when is-a
    (let ((*schema-is-new* T))
      (set-is-a schema is-a)))
  (do* ((slots slot-specifiers (cdr slots))
	(slot (car slots) (car slots))
	(initialize-method NIL)
	(constants NIL)
	(had-constants NIL)
	(cancel-constants (find '(:constant) slot-specifiers :test #'equal))
	(parent (car is-a))
	(slot-counter (if is-a 1 0)))
       ((null slots)
	(unless (eq types :NONE)
	  (dolist (type types)
	    (if (cdr type)
		(let ((n (encode-type (car type))))
		  (dolist (slot (cdr type))
		    (set-slot-accessor schema slot *no-value* 33 nil)))
		(format t "*** ERROR - empty list of slots in type declaration ~
                          for object ~S:~%  ~S~%" schema (car type)))))
	(format t "had-constants: ~s; constants: ~s; formula-p: ~s~%" had-constants constants (formula-p constants))
	(process-constant-slots
	 schema is-a
	 had-constants
	 (not (eq types :NONE)))
	(dolist (slot slot-specifiers)
	  (when (and (listp slot)
		     (memberq (car slot)
			      '(:IGNORED-SLOTS :LOCAL-ONLY-SLOTS
				:MAYBE-CONSTANT :PARAMETERS :OUTPUT
				:SORTED-SLOTS :UPDATE-SLOTS)))
	    ))
	(when generate-instance
	  (kr-init-method schema))
	schema)
    (cond ((eq slot :NAME-PREFIX)
	   (pop slots))
	  ((consp slot)
	   (let ((slot-name (car slot))
		 (slot-value (cdr slot)))
	     (case slot-name
	       (:INITIALIZE
		(when slot-value
		  (setf initialize-method slot-value)))
	       (:CONSTANT
		(setf constants (cdr slot))
		(setf had-constants T)))
	     ;; Check that the slot is not declared constant in the parent.
	     (when (and (not cancel-constants) (not *constants-disabled*)
			(not *redefine-ok*))
	       (when (and parent (slot-constant-p parent slot-name))
		 (cerror
		  "If continued, the value of the slot will change anyway"
		  "Slot ~S in ~S was declared constant in prototype ~S!~%"
		  slot-name schema (car is-a))))
	     (if override
		 (s-value schema slot-name slot-value)
		 (setf slot-counter
		       (internal-s-value schema slot-name slot-value)))))
	  (T
	   (format t "Incorrect slot specification: object ~S ~S~%"
		   schema slot)))))

(defun do-schema-body-alt (schema is-a &rest slot-specifiers)
  (let ((types nil))
    (unless (listp is-a)
      (setf is-a (list is-a)))
    (when is-a
      (let ((*schema-is-new* T))
	(set-is-a schema is-a)))
    (do* ((slots slot-specifiers (cdr slots))
	  (slot (car slots) (car slots))
	  (initialize-method NIL)
	  (constants NIL)
	  (had-constants NIL)
	  (cancel-constants (find '(:constant) slot-specifiers :test #'equal))
	  (slot-counter (if is-a 1 0)))
	 ((null slots)
	  (unless (eq types :NONE)
	    (dolist (type types)
	      (if (cdr type)
		  (let ((n (encode-type (car type))))
		    (dolist (slot (cdr type))
		      (set-slot-accessor schema slot *no-value* 33 nil)))
		  (format t "*** ERROR - empty list of slots in type declaration ~
                          for object ~S:~%  ~S~%" schema (car type)))))
	  ;; Process the constant declarations, and check the types.
	  (process-constant-slots schema is-a nil (not (eq types :NONE)))
	  (dolist (slot slot-specifiers)
	    (when (and (listp slot)
		       (memberq (car slot)
				'(:IGNORED-SLOTS :LOCAL-ONLY-SLOTS
				  :MAYBE-CONSTANT :PARAMETERS :OUTPUT
				  :SORTED-SLOTS :UPDATE-SLOTS)))
	      ))
	  (kr-init-method schema)
	  schema)
      (cond ((eq slot :NAME-PREFIX)
	     (pop slots))
	    ((consp slot)
	     (let ((slot-name (car slot))
		   (slot-value (cdr slot)))
	       (case slot-name
		 (:CONSTANT
		  (setf constants (cdr slot))
		  (setf had-constants T)))
	       (internal-s-value schema slot-name slot-value)))
	    (T
	     (format t "Incorrect slot specification: object ~S ~S~%"
		     schema slot))))))

(defun T-P (value)
  (declare #.*special-kr-optimization*
	   (ignore value))
  T)

(defun no-type-error-p (value)
  (cerror "Return T"
          "KR typechecking called on value ~S with no type"
          value)
  T)
