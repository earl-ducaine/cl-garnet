
(in-package :kr)

(defun clear-one-slot (schema slot entry)
  "Completely clear a slot, including dependencies, inherited, etc...
   BUT... Leave around the declarations (constant, type, update,...)"
  (locally (declare #.*special-kr-optimization*)
    (let ((the-entry (or entry (slot-accessor schema slot))))
      (when the-entry
	(setf (sl-value the-entry) *no-value*
	      (sl-bits the-entry)  (logand (sl-bits the-entry)
					   *clear-slot-mask*))))))

(declaim (inline clear-schema-slots))
(defun clear-schema-slots (schema)
  "Completely clear ALL the slots in the <schema>."
  (locally (declare #.*special-kr-optimization*)
    (clrhash (schema-bins schema))))

(defun value-fn (schema slot)
  "Does the actual work of G-VALUE."
  (g-value-body schema slot T T))

(defun g-local-value-fn (schema slot)
  "Similar to g-value-fn, but no inheritance."
  (g-value-body schema slot NIL T))

(let ((list-of-one (list nil)))
  (defun get-dependents (schema slot)
    "RETURNS: the formulas which depend on the <slot> of the <schema>."
    (let ((value (slot-dependents (slot-accessor schema slot))))
      (if (listp value)
	  value
	  (progn
	    (setf (car list-of-one) value)
	    list-of-one)))))

(declaim (inline get-lambda))
(defun get-lambda (formula)
  "Returns the lambda expression in a formula, or NIL."
  (when (formula-p formula)
    (a-formula-lambda formula)))

(defun enable-a-demon (demon)
  "Turns ON a demon if it was turned off.  If all demons are currently
disabled, the variable *demons-disabled* is made of the form
(T demon), where the names following the T are, in fact, enabled."
  (cond ((eq *demons-disabled* T)
	 (list T demon))
	((eq *demons-disabled* NIL))	; nothing is disabled
	((listp *demons-disabled*)
	 ;; A list
	 (if (eq (car *demons-disabled*) T)
	     ;; Special format
	     (if (memberq demon (cdr *demons-disabled*))
		 *demons-disabled*	; nothing is needed
		 (cons T (cons demon (cdr *demons-disabled*))))
	     ;; Normal format
	     (if (memberq demon *demons-disabled*)
		 (remove demon *demons-disabled*)
		 *demons-disabled*)))
	((eq demon *demons-disabled*)
	 NIL)
	(t
	 *demons-disabled*)))

(defun disable-a-demon (demon)
  (if (eq *demons-disabled* T)
      T					; everything is already turned off
      (if (eq *demons-disabled* NIL)
	  demon
	  (if (listp *demons-disabled*)
	      ;; A list
	      (if (eq (car *demons-disabled*) T)
		  ;; Special format used by with-demon-enable
		  (if (memberq demon *demons-disabled*)
		      (let ((new-value (delete demon *demons-disabled*)))
			(if (null (cdr new-value))
			    T
			    new-value))
		      ;; Already disabled
		      *demons-disabled*)
		  ;; Normal format
		  (cons demon *demons-disabled*))
	      ;; A single value - make a list.
	      (list demon *demons-disabled*)))))


(defun demon-is-disabled (demon)
  "Is the <demon> currently enabled?"
  (if (listp *demons-disabled*)
      (if (eq (car *demons-disabled*) T)
	  ;; Special format
	  (not (memberq demon (cdr *demons-disabled*)))
	  ;; Normal format
	  (memberq demon *demons-disabled*))
      (eq demon *demons-disabled*)))


(defun g-value-inherit-values (schema slot is-leaf slot-structure)
  "Search up the tree for inherited slot.
RETURNS: the inherited value, or NIL."
  (declare (ftype (function (t &optional t) t) formula-fn))
  (let (has-parents)
    (when (a-local-only-slot slot)	; These CANNOT be inherited.
      (return-from g-value-inherit-values NIL))
    (dolist (relation *inheritance-relations*)
      (dolist (parent (if (eq relation :IS-A)
			  (get-local-value schema :IS-A)
			  (get-local-value schema relation)))
	(setf has-parents T)
	(let ((entry (slot-accessor parent slot))
	      (value *no-value*)
	      bits			; parent bits
	      (intermediate-constant NIL))
	  (when entry
	    (setf value (sl-value entry))
	    (when (is-constant (sl-bits entry))
	      (setf intermediate-constant T)))
	  (if (eq value *no-value*)
	      ;; Attempt to inherit from its ancestors.
	      (multiple-value-setq (value bits)
		(g-value-inherit-values parent slot NIL nil))
	      ;; If value, just set bits.
	      (setf bits (sl-bits entry)))
	  (unless (eq value *no-value*)
	    ;; (if (and bits (is-parent bits))
	    ;; 	;; Clear the parent bit, since we will set the child.
	    ;; 	(setf bits (logand bits *not-parent-mask*))
	    ;; 	;; Set the bit in the parent which says that the value was
	    ;; 	;; inherited by someone.
	    ;; 	(if entry
	    ;; 	    ;; Destructively set the bits.
	    ;; 	    (setf (sl-bits entry) (logior bits *is-parent-mask*))
	    ;; 	    (set-slot-accessor parent slot value
	    ;; 			       (logior bits *is-parent-mask*) nil)))
	    ;; Copy the value down to the inheriting slot, unless the value
	    ;; contains a formula.
	    (let ((was-formula (formula-p value)))
	      (when was-formula
		;; Inherit the formula, making a copy of it.
		(setf value (formula-fn value (a-formula-cached-value value)))
		(setf (a-formula-schema value) schema)
		(setf (a-formula-slot value) slot)
		(set-cache-is-valid value NIL))
	      ;; Copy down, mark as inherited if inherited
	      (when (and is-leaf slot-structure)	; slot had constant bit
		(setf bits (logior bits (sl-bits slot-structure))))
	      ;; (setf bits (logior *inherited-mask* bits
	      ;; 			 #+TEST
	      ;; 			 (logand bits *not-parent-constant-mask*)))
	      (when intermediate-constant
		(setf bits (logior *constant-mask* bits)))
	      ;; (set-slot-accessor schema slot value bits
	      ;; 			 (slot-dependents slot-structure))
	      )
	    (return-from g-value-inherit-values (values value bits))))))
    ;; We didn't find anything, so return an appropriate null value and set
    ;; the local cache (even though we have no value) to avoid further
    ;; inheritance search.
    (set-slot-accessor schema slot
		       (if has-parents NIL *no-value*)
		       (cond (is-leaf
			      (if slot-structure
				  (logior *inherited-mask*
					  (sl-bits slot-structure))
				  *inherited-mask*))
			     (has-parents *inherited-parent-mask*)
			     (t		; top-level, no parents
			      *is-parent-mask*))
		       (slot-dependents slot-structure))
    *no-value*))

;; G-CACHED-VALUE
;;
(declaim (inline g-cached-value))
(defun g-cached-value (schema slot)
  "Returns the value of the <slot> in the <schema>.  If this is a formula, it
  returns the cached value of the formula, without ever recomputing the
  formula."
  ;; Note use of GET-VALUE
  (let ((g-cached-value-val (get-value schema slot)))
    (if (formula-p g-cached-value-val)
	(cached-value g-cached-value-val)
	g-cached-value-val)))


(defun g-value-no-copy (schema slot &optional skip-local)
  "This is a specialized function which does inheritance but does NOT copy
values down.  It is used by the :INITIALIZE method, which is called exactly
once per object and should NOT copy down anything (since the method will
never be used again)."
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


;;; PRINTING AND DEBUGGING

(declaim (fixnum *debug-names-length* *debug-index*))
(defparameter *debug-names-length* 500)

(defvar *debug-names* (make-array *debug-names-length* :initial-element nil))

(defvar *debug-index* -1)

(defvar *intern-unnamed-schemata* T
  "This variable may be set to NIL to prevent PS from automatically creating
  any unnamed schemata it prints out.")

(defun cache-schema-name (schema name)
  "This does not cause any creation of symbols.  It simply records
the schema in an array, thus creating a semi-permanent way to refer
to a schema."
  (unless (find-if #'(lambda (x)
		       (and x (eql (schema-name x) name)))
		   *debug-names*)
    ;; A new schema.  Store it in the next position (cycle if
    ;; we reach the end of the array).
    (setf (aref *debug-names*
		(setf *debug-index*
		      (mod (incf *debug-index*) *debug-names-length*)))
	  schema)))

;;
(defun make-new-schema-name (schema name)
  "Creates symbols for all automatic schema names that happen to
be printed out."
  (let* (
	 parent
	 (symbol
	  ;; (intern (cond ((stringp name)
	  ;; 		 ;; a name-prefix schema
	  ;; 		 (format nil "~A-~D"
	  ;; 			 name (incf *schema-counter*)))
	  ;; 		((setf parent
	  ;; 		       (if (formula-p schema)
	  ;; 			   (a-formula-is-a schema)
	  ;; 			   (when (not-deleted-p schema)
	  ;; 			     (car (get-local-value schema :IS-A)))))
	  ;; 		 (let ((parent-name (when parent (schema-name parent))))
	  ;; 		   (when (or (integerp parent-name)
	  ;; 			     (stringp parent-name))
	  ;; 		     ;; Parent is unnamed yet - force a name.
	  ;; 		     (with-output-to-string
	  ;; 			 (bit-bucket)
	  ;; 		       (print-the-schema parent bit-bucket 0))
	  ;; 		     (setf parent-name (schema-name parent)))
	  ;; 		   (format nil "~A-~D" parent-name name)))
	  ;; 		(t
	  ;; 		 (format nil "~C~D"
	  ;; 			 (if (formula-p schema) #\F #\S)
	  ;; 			 name)))
	  ;; 	  debug-package)
	   ))
    ;; (set symbol schema)
    ;; (setf (schema-name schema) symbol)
    ;; (export symbol debug-package)
    ))

(defun print-the-slot (slot stream level)
  (declare (ignore level))
  (format stream "<slot ~S  value ~S, bits ~S"
	  (sl-name slot) (sl-value slot) (sl-bits slot))
  (if (full-sl-p slot)
      (format stream ",  dependents ~S>" (full-sl-dependents slot))
      (format stream ">")))

(defun print-the-schema (schema stream level)
  (declare (ignore level))
  (let ((name (schema-name schema))
	(destroyed (not (not-deleted-p schema))))
    ;; This version is for debugging.  Record the latest schemata in the
    ;; array.
    (cond ((or (integerp name) (stringp name))
	   ;; This is a nameless schema.  Print it out, and record it in the
	   ;; debugging array.
	   (when *intern-unnamed-schemata*
	     (make-new-schema-name schema name))
	   (cache-schema-name schema name)
	   ;; This gives control over whether unnamed schemata are interned.
	   (setf name (schema-name schema)))
	  ((null name)
	   ;; This was a deleted schema
	   (setf name '*DESTROYED*)))
    (when destroyed (format stream "*DESTROYED*(was "))
    (if *print-as-structure*
	(progn
	  (format stream "#k<~S" name)
	  (dolist (slot *print-structure-slots*)
	    (let ((value (g-value schema slot)))
	      (when value
		(format stream " (~S ~S)" slot value))))
	  (format stream ">")
	  (when destroyed (format stream ")")))
	(progn
	  (format stream "~S" name)
	  (when destroyed (format stream ")"))))))


(defun name-for-schema (schema)
  "Given a schema, returns its printable name as a string.  The string
CANNOT be destructively modified.
Note that this returns the pure name, without the #k<> notation."
  (let ((name (schema-name schema)))
    (when (or (integerp name) (stringp name))
      ;; This is a nameless schema.  Print it out, and record it in the
      ;; debugging array.
      ;; (when *intern-unnamed-schemata*
      ;; 	(make-new-schema-name schema name))
      (cache-schema-name schema name)
      ;; This gives control over whether unnamed schemata are interned.
      (setf name (schema-name schema)))
    (symbol-name name)))


(defun s (number)
  "This is a debugging function which returns a schema, given its internal
number.  It only works if the schema was printed out rather recently,
i.e., if it is contained in the temporary array of names."
  (setf number (format nil "~D" number))
  (find-if #'(lambda (x)
	       (and x
		    (symbolp (schema-name x))
		    (do* ((name (symbol-name (schema-name x)))
			  (i (1- (length name)) (1- i))
			  (j (1- (length number)) (1- j)))
			 ((minusp j)
			  (unless (digit-char-p (schar name i))
			    x))
		      (unless (char= (schar name i) (schar number j))
			(return nil)))))
	   *debug-names*))



;;; RELATIONS

(defun unlink-one-value (schema slot value)	    ; e.g., child :is-a parent
  "Remove the inverse link from <value> to <schema>, following the inverse
of <slot>."
  (let ((inverse (cadr (assocq slot *relations*)))) ; e.g., is-a-inv
    (when inverse
      ;; If the relation has an INVERSE slot, remove <schema> from the
      ;; inverse slot.
      (let ((entry (slot-accessor value inverse)) ; e.g., A child B
	    values)
	(when entry
	  (setf values (sl-value entry))
	  (if (eq (car values) schema)
	      ;; <schema> is first in the inverse list
	      (set-slot-accessor value inverse (delete schema values)
				 (sl-bits entry) (slot-dependents entry))
	      ;; just do a destructive operation
	      (setf (cdr values) (delete schema (cdr values)))))))))

(defun unlink-all-values (schema slot)
  "Same as above, but unlinks all schemata that are in <slot>."
  (let ((inverse (cadr (assocq slot *relations*))))
    (when inverse
      (let ((entry (if (eq slot :IS-A)
		       (slot-accessor schema :IS-A)
		       (if (eq slot :IS-A-INV)
			   (slot-accessor schema :IS-A-INV)
			   (slot-accessor schema slot)))))
	(when entry
	  (dolist (parent (sl-value entry))
	    (when (not-deleted-p parent) ; parent is not destroyed
	      ;; If the terminal has an INVERSE slot, remove <schema> from the
	      ;; inverse slot.
	      (let ((entry (if (eq inverse :is-a-inv)
			       (slot-accessor parent :is-a-inv) ; e.g., A child B
			       (slot-accessor parent inverse)))
		    values)
		(when entry
		  (setf values (sl-value entry))
		  (if (eq (car values) schema)
		      (pop (sl-value entry))
		      (setf (cdr values) (delete schema (cdr values)))))))))))))


(defun link-in-relation (schema slot values)
  "Since the <values> are being added to <slot>, see if we need to put in an
inverse link to <schema> from each of the <values>.
This happens when <slot> is a relation with an inverse."
  (let ((inverse (if (eq slot :is-a)
		     :is-a-inv
		     (cadr (assocq slot *relations*)))))
    (when inverse
      ;; <values> is a list: cycle through them all
      (dolist (value values)
	(let* ((entry (if (eq slot :is-a)
			  (slot-accessor value :is-a-inv)
			  (slot-accessor value inverse)))
	       (previous-values (when entry (sl-value entry))))
	  (if entry
	      ;; Create the back-link.  We use primitives here to avoid looping.
	      (if (or *schema-is-new*
		      (not (memberq schema previous-values)))
		  ;; Handle an important special case efficiently.
		  (if (eq (sl-value entry) *no-value*)
		      ;; There was no value after all!
		      (setf (sl-value entry) (list schema))
		      ;; There were real values.
		      (push schema (sl-value entry))))
	      ;; There was no inverse in the parent yet.
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

(defun formula-push (f)
;;  (bordeaux-threads:with-lock-held  (*formula-lock*)
    (push f *formula-pool*))

;;; encode types
(defparameter *types-table* (make-hash-table :test #'equal)
  "Hash table used to look up a Lisp type and returns its code")

;; (defparameter *types-table-lock* (bordeaux-threads:make-recursive-lock)
;;   "Lock to synchonize access to *types-table*")

(defmacro with-types-table-lock-held ((table) &body body)
  `(let ((,table *types-table*))
;;     (bordeaux-threads:with-recursive-lock-held (*types-table-lock*)
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
	     ;; is this a def-kr-type?
	     (make-lambda-body (code-to-type code)))
	    (T ;; simple-type
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
	  ;; redefining same name
	  (if (equal (code-to-type code) type-body)
	      ;; redefining same name, same type
	      (progn
		(format t "Ignoring redundant def-kr-type of ~S to ~S~%"
			typename type-body)
		(return-from add-new-type code))
	      ;; redefining same name, new type --> replace it!
	      (format t "def-kr-type redefining ~S from ~S to ~S~%"
		      typename (code-to-type code) type-body))
	  ;; defining a new name, establish new code
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

(defun kr-type-error (type)
  (error "Type ~S not defined; use~% (def-kr-type ... () '~S)~%" type type))

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
		 (kr-type-error type)))
	    (T (kr-type-error type))))))

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


(declaim (fixnum *warning-level*))
(defparameter *warning-level* 0)

(defun g-value-formula-value (schema-self slot formula entry)
  (let ((*schema-self* schema-self))
    (if (cache-is-valid formula)
	(a-formula-cached-value formula)
	(progn
	  (unless *within-g-value*
	    ;; Bump the sweep mark only at the beginning of a chain of formula
	    ;; accesses.  Increment by 2 since lower bit is "valid" flag.
	    (incf *sweep-mark* 2))
	  (if (= (cache-mark formula) *sweep-mark*)
	      (progn
		(when *warning-on-circularity*
		  (format t "Warning - circularity detected on ~S, slot ~S~%"
			  *schema-self* slot))
		(unless *setting-formula-p*
		  (set-cache-is-valid formula T))
		(a-formula-cached-value formula)))))))

(defun copy-to-all-instances (schema a-slot value &optional (is-first T))
  "Forces the <value> to be physically copied to the <a-slot> of all
instances of the <schema>, even though local values were defined.
However, if there was a local formula, do nothing."
  (s-value schema a-slot value)
  ;; Do not create copies of formulas, but set things up for inheritance
  (when (and is-first (formula-p value))
    (setf value *no-value*))
  (dolist (inverse *inheritance-inverse-relations*)
    (let ((children (if (eq inverse :IS-A-INV) ; for efficiency
			(let ((entry (slot-accessor schema :IS-A-INV)))
			  (when entry (sl-value entry)))
			(get-local-value schema inverse))))
      (unless (eq children *no-value*)
	(dolist (child children)
	  ;; force new inheritance
	  (unless (formula-p (get-value child a-slot))
	    ;; Do not override if the user has specified a local formula!
	    (copy-to-all-instances child a-slot value NIL)))))))

(defun update-inherited-internal (child a-slot entry)
  (let ((old-value (sl-value entry)))
    (unless (eq old-value *no-value*)
      (let ((child-bits (sl-bits entry)))
	(when (is-inherited child-bits)
	  ;; NOTE: we erase the inherited value in all cases, even if it might
	  ;; have been inherited from somewhere else (in the case of multiple
	  ;; inheritance).  In any case, this is correct; at worst, it may
	  ;; cause the value to be needlessly inherited again.
	  ;; Force the children to re-inherit.
	  (when (formula-p old-value)
	    (delete-formula old-value T))
	  (clear-one-slot child a-slot entry)
	  ;; Recursively change children.
	  (update-inherited-values child a-slot *no-value* NIL))))))

(defun update-inherited-values (schema a-slot value is-first)
  "This function is used when a value is changed in a prototype.  It makes
sure that any child schema which inherited the previous value is updated
with the new value.
INPUTS:
  - <value>: the new (i.e., current) value for the <schema>
  - <old-bits>: the setting of the slot bits for the <schema>, before the
    current value-setting operation.
  - <is-first>: if non-nil, this is the top-level call.
"
  (let ((*schema-self* schema))
    (unless is-first
      ;; Invoke demons and propagate change around.
      (run-pre-set-demons schema a-slot value NIL :INHERITANCE-PROPAGATION)
      (run-invalidate-demons schema a-slot NIL)
      (propagate-change schema a-slot))
    (dolist (inverse *inheritance-inverse-relations*)
      (let ((children (if (eq inverse :IS-A-INV) ; for efficiency
			  (let ((entry (slot-accessor schema :IS-A-INV)))
			    (when entry (sl-value entry)))
			  (get-local-value schema inverse))))
	(unless (eq children *no-value*)
	  (dolist (child children)
	    (let ((entry (slot-accessor child a-slot)))
	      (when entry
		;; If child had no value, no need to propagate down
		(setf is-first NIL)
		;; force new inheritance
		(update-inherited-internal child a-slot entry)))))))))


;;; Slot and formula change  code.


(declaim (inline mark-as-changed))
(defun mark-as-changed (schema slot)
  "Forces formulas which depend on the <slot> in the <schema> to be
invalidated.  Mostly used for internal implementation.
This function can be used when manually changing a slot (without using
s-value).  It will run the demons and propagate the invalidate wave
to all the ordinary places."
  (let ((entry (slot-accessor schema slot)))
    (run-invalidate-demons schema slot entry)
    (when (and entry (is-parent (sl-bits entry)))
      (update-inherited-values schema slot (sl-value entry) T)))
  (propagate-change schema slot))


(declaim (inline mark-as-invalid))
(defun mark-as-invalid (schema slot)
  "Invalidates the value of the formula at <position> in the <slot> of the
  <schema>.  If the value is not a formula, nothing happens."
  (let ((value (get-value schema slot)))
    (when (formula-p value)
      (set-cache-is-valid value NIL))))

(defun propagate-change (schema slot)
  "Since the <slot> of the <schema> was modified, we need to propagate the
change to all the formulas which depended on the old value."
  (let ((entry (slot-accessor schema slot)))
    ;; access the dependent formulas.
    (do-one-or-list (formula (slot-dependents entry) T)
      ;; Stop propagating if this dependent formula was already marked dirty.
      (if (and (not-deleted-p formula) (cache-is-valid formula))
	(let* ((new-schema (on-schema formula))
	       (new-slot (on-slot formula))
	       (schema-ok (schema-p new-schema))
	       (new-entry  NIL))
	  (unless (and new-schema new-slot)
	    (when *warning-on-disconnected-formula*
	      (format
	       t
	       "Warning: disconnected formula ~S in propagate-change ~S ~S~%"
	       formula schema slot))
	    (continue-out))
	  (if schema-ok
	    (progn
	      (setf new-entry (slot-accessor new-schema new-slot))
	      (run-invalidate-demons new-schema new-slot new-entry))
	    #+GARNET-DEBUG
	    (progn
	      (format
	       t
	       "propagate-change: formula ~S on destroyed object ~S ~S~%    ~
	from change in schema ~S, slot ~S.~%"
	       formula new-schema new-slot schema slot)))
	  ;; The formula gets invalidated here.
	  (set-cache-is-valid formula nil)
	  ;; Notify all children who used to inherit the old value of the
	  ;; formula.
	  (if (and schema-ok new-entry)
	    (if (slot-dependents new-entry)
	      (propagate-change new-schema new-slot))))))))


(defun visit-inherited-values (schema a-slot function)
  "Similar to update-inherited-values, but used when the hierarchy is
modified or when an inheritable slot is destroyed.
SIDE EFFECTS:
 - the <function> is called on all children which actually inherit the
   values in the <a-slot> of the <schema>.  This is determined by a fast
   check (the list of values should be EQ to that of the parent).
Note that the <function> is called after all children have been visited..
This allows it to be a destructive function."
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

(defun constant-slot-error (schema slot)
  (cerror "Set the slot anyway"
	  "Schema ~S - trying to set slot ~S, which is constant."
	  schema slot))


(declaim (inline check-not-constant))
(defun check-not-constant (schema slot entry)
  "Signals an error if the <slot> of the <schema> is not constant."
  (and (not *constants-disabled*)
       entry
       (is-constant (sl-bits entry))
       (constant-slot-error schema slot)))


(declaim (inline slot-constant-p))
(defun slot-constant-p (schema slot)
  "RETURN: T if the <slot> in the <schema> is constant, nil otherwise"
  (let ((entry (slot-accessor schema slot)))
    (when entry
      (is-constant (sl-bits entry)))))


(defun set-formula-error (schema slot formula)
  "Called to give error message on multiply-installed formulas."
  ;; Formulas can only be installed on one slot!
  (format t "(s-value ~S ~S): formula ~S is already installed on~%~
	schema ~S, slot ~S.  Ignored.~%"
	  schema slot formula (on-schema formula) (on-slot formula))
  formula)


(defun s-value-fn (schema slot value)
  (locally (declare #.*special-kr-optimization*)
    (unless (schema-p schema)
      (return-from s-value-fn (values value t)))
    (let* ((entry (slot-accessor schema slot))
	   (old-value
	    nil
	     )
	    is-depended)
      ;; give error if setting constant slot
      (check-not-constant schema slot entry)
      ;; (if entry
      ;; 	  ;; (setf the-bits (sl-bits entry)
      ;; 	  ;; 	is-depended (slot-dependents entry))
      ;; 	  ;; (setf the-bits 0)
      ;; 	  )
      (when (and the-bits
		 (not (is-inherited the-bits))
		 (eq old-value value)
		 value)
	;; We are setting to the same value as the old one!  Do nothing.
	;; RGA --- Error return function
	(return-from s-value-fn (values value nil)))
      (when (and *types-enabled* (not (formula-p value)))
	(multiple-value-bind (result error-p)
	    (check-slot-type schema slot value T entry)
	  (cond ((not error-p))  ; Everything is OK
		((eq error-p T)
		 ;; A type error - user wants to do nothing
		 ;; RGA --should return old-value, added a second
		 ;; value indicating error.
		 (return-from s-value-fn (values old-value t)))
		(T
		 ;; A type error - user supplied new value
		 (setf value result)))))
      (let ((is-formula nil) (is-relation nil)
	    (was-formula (formula-p old-value)))
	;; Check for special cases in relation slots.
	(when (and (setf is-relation (relation-p slot))
		   (eq (setf value (check-relation-slot schema slot value)) *no-value*))
	  ;; RGA --- added no-error return code
	  (return-from s-value-fn (values old-value nil)))
	;; If we are installing a formula, make sure that the formula
	;; points to the schema and slot.
	(when (formula-p value)
	  (when (on-schema value)
	    ;; RGA --- added error return code.
	    (return-from s-value-fn
	      (values (set-formula-error schema slot value) T)))
	  (setf is-formula T)
	  (setf (on-schema value) schema)
	  (setf (on-slot value) slot)
	  (unless (schema-name value)
	    ;; This is an obscure case.  It may happen if somebody stores a
	    ;; formula away, deletes the formula from its original slot, and
	    ;; then restores the formula.  This is generally bad practice, but
	    ;; there are cases when it may be necessary.
	    (incf *schema-counter*)
	    (setf (schema-name value) *schema-counter*)))
	(unless is-formula
	  (run-pre-set-demons schema slot value NIL :S-VALUE))
	;; Now we can set the new value.
	;; (setf the-bits (logand the-bits *not-inherited-mask*))
	(run-invalidate-demons schema slot entry)
	;; Now set the value in the slot to be <value>.
	(cond
	  ((and was-formula (not is-formula))
	   (when (zerop (a-formula-number old-value))
	     (format t "*** Warning: you are setting the value of slot ~S of
 object ~S.  This slot contains a formula which was never evaluated.
 The formula is now valid, but its dependencies are not set up properly.  As
 a result, the formula will never be evaluated.
 In order for this formula to work properly, you should have used
 (g-value ~S ~S) before using S-VALUE.  If you want to fix things now,
 re-install the formula using s-value.~%"
		     slot schema schema slot))
	   ;; This is the case when we allow temporary overwriting
	   (setf (cached-value old-value) value)
	   ;; Set this to NIL, temporarily, in order to cause propagation
	   ;; to leave the value alone.  It will be validated by s-value.
	   (set-cache-is-valid old-value NIL))
	  (t
	   ;; All other cases
	   (when (and is-formula (null (cached-value value)))
	     ;; place old value in the cache only if an initial value
	     ;; was not provided for the new formula
	     ;; Set value, but keep formula invalid.
	     (setf (cached-value value)
		   (if was-formula (cached-value old-value) old-value)))
	   ;; Take care of relations.
	   (when is-relation
	     (when old-value (unlink-all-values schema slot))
	     (link-in-relation schema slot value))
	   (let ((new-bits (or the-bits *local-mask*)))
	     (if entry
		 ;; This is a special slot - just set it
		 (setf (sl-value entry) value
		       (sl-bits entry) new-bits)
		 ;; This is not a special slot.
		 (setf entry (set-slot-accessor schema
						slot value new-bits nil))))))
	;; Now propagate the change to all the children which used to
	;; inherit the previous value of this slot from the schema.
	(when (and the-bits (is-parent the-bits))
	  (let ((*setting-formula-p* T))
	    (update-inherited-values schema slot value T)))
	;; Notify all dependents that the value changed.
	(when is-depended
	  (let ((*warning-on-disconnected-formula* nil))
	    (propagate-change schema slot)))
	(when (and was-formula (not is-formula))
	  ;; We validate now, rather than earlier, because of a technicality
	  ;; in demons-and-old-values.
	  (set-cache-is-valid old-value T))
	;; Was the old value a formula?
	(when (and was-formula is-formula)
	  ;; This is replacing a formula with another.  Eliminate the dependency
	  ;; to the old one.
	  (delete-formula old-value T))
	(values value nil)))))


(defun internal-s-value (schema slot value)
  "This is a stripped-down version of s-value-fn which is used by
create-schema and friends.  It skips a lot of the stuff that is
unnecessary at schema creation time."
  (let ((is-formula (formula-p value))
	(is-relation (relation-p slot)))
    (when is-relation
      (unless (listp value)
	(setf value (list value)))
      ;; Check for special cases in relation slots.
      (when (eq (setf value (check-relation-slot schema slot value)) *no-value*)
	(return-from internal-s-value NIL)))

    ;; If we are installing a formula, make sure that the formula
    ;; points to the schema and slot.
    (when is-formula
      (when (on-schema value)
	(return-from internal-s-value (set-formula-error schema slot value)))
      (setf (on-schema value) schema)
      (setf (on-slot value) slot))

    (set-slot-accessor schema slot value *local-mask* nil)

    ;; Take care of relations.
    (when is-relation
      (link-in-relation schema slot value))
    value))


(defun set-is-a (schema value)
  "A specialized version of internal-s-value"
  ;; Check for special cases in relation slots.
  (when (eq (setf value (check-relation-slot schema :is-a value)) *no-value*)
    (return-from set-is-a NIL))
  ;; Set slot
  (set-slot-accessor schema :IS-A value *local-mask* NIL)
  (link-in-relation schema :IS-A value)
  value)


(defun eliminate-formula-dependencies (formula except-schema)
  "If <except-schema> is non-nil, it indicates that a schema is in the
process of being destroyed, and hence dependencies to THAT schema should
not be tracked down."
  (do-one-or-list (schema (a-formula-depends-on formula))
    (unless (or (eq schema except-schema)
		(deleted-p schema))	; schema is destroyed
      (iterate-slot-value (schema T T T)
	slot value			; suppress warning
	(let ((formulas (slot-dependents iterate-slot-value-entry)))
	  (if (listp formulas)
	      ;; Several dependents
	      (when (memberq formula formulas)
		(setf (full-sl-dependents iterate-slot-value-entry)
		      (delete formula formulas)))
	      ;; One dependent
	      (when (eq formula formulas)
		(setf (full-sl-dependents iterate-slot-value-entry) NIL))))))))


(defun delete-formula (formula remove-from-parent)
  "Eliminate all dependency pointers from the <formula>, since it is no
longer installed on a slot.

INPUTS:
 - <formula>: the formula to get rid of
 - <hard-p>: if T, do a more thorough job of deleting everything, and
   destroy the <formula> schema itself."
  (when (a-formula-number formula)
    (eliminate-formula-dependencies formula NIL)
    (when remove-from-parent
      ;; Eliminate the <formula> from its parent's list of children.
      (let ((parent (a-formula-is-a formula)))
	(when parent
	  (delete-one-or-list formula (a-formula-is-a-inv parent)))))
    ;; Formula was not destroyed yet
    (setf (a-formula-bins formula) nil	; mark as destroyed.
	  (a-formula-schema formula) nil
	  (a-formula-slot formula) nil
	  (a-formula-lambda formula) nil
	  (a-formula-depends-on formula) nil)
    (let ((meta (a-formula-meta formula)))
      (when meta
	(setf (a-formula-meta formula) NIL)
	(destroy-schema meta)))
    (formula-push formula)))

(defun delete-schema (schema recursive-p)
  "Internal function.  If <recursive-p>, this is being called from within
recursive-destroy-schema, so there is no need to maintain upwards
relations properly."
  (when (not-deleted-p schema)	; do nothing if schema is already destroyed
    ;; Remove all inverse links.
    (if (formula-p schema)
	;; Formulas do not use regular relations.
	(let ((parent (a-formula-is-a schema))
	      children)
	  (when parent
	    (setf children (a-formula-is-a-inv parent))
	    (setf (a-formula-is-a-inv parent)
		  (if (listp children)
		    (delete schema children)
		    (if (eq schema children)
		      NIL
		      children))))
	  (do-one-or-list (child (a-formula-is-a-inv schema))
	    ;; ? What exactly should happen here ?
	    (setf (a-formula-is-a child) NIL)))
	;; A normal schema
	(progn
	  (unless recursive-p
	    (iterate-slot-value (schema NIL NIL NIL)
	      value                     ; eliminate warning
	      (when (relation-p slot)
		(unlink-all-values schema slot))))
	  (iterate-slot-value (schema NIL NIL NIL)
	    slot value			; eliminate warning
	    ;; Delete any formula value.
	    (when (formula-p value)
	      ;; This is a formula.  Get rid of it.
	      (delete-formula value (not recursive-p))
	      (delete-schema value recursive-p)))
	  ;; Physically delete all the slots
	  (clear-schema-slots schema)))
    ;; Now wipe out the symbol value as well.
    (when (symbolp (schema-name schema))
      (makunbound (schema-name schema)))
    ;; This is used as a marker for deleted schemas.
    (setf (schema-bins schema) nil)))

(defun find-direct-dependency (expression target)
  "RETURNS: T if the given <expression>, or one of its subexpressions,
directly depends on the <target>.  This must be a direct dependency,
i.e., one which does not use a link."
  (when (listp expression)
    (or (and (eq (car expression) 'GV)
	     (eq (cadr expression) target))
	(dolist (thing expression)
	  (when (find-direct-dependency thing target)
	    (return T))))))

(defun destroy-schema (schema &optional (send-destroy-message NIL) recursive-p)
  "Destroys the <schema>, eliminates all dependencies to and from it."
  (unless (schema-p schema)
    ;; If schema is already destroyed, do nothing.
    (return-from destroy-schema))
  (let ((done nil)
	bizarre)
    (iterate-slot-value (schema T T NIL)
      slot				; eliminate warning
      (unless (eq value *no-value*)
	;; Look at all formulas which depend on this slot.
	(do-one-or-list (formula (slot-dependents iterate-slot-value-entry))
	  (when (and formula		; defensive programming
		     (not (memberq formula done)))
	    ;; If this is a value depended on by others, replace their
	    ;; value by the current value.  Do this, however, only if the
	    ;; dependency is a DIRECT one, i.e., if the name of the
	    ;; schema we are destroying is wired into the formula.  If
	    ;; this is a link, leave things as they are.
	    (let ((the-form
		   (or (a-formula-lambda formula) ; for o-formulas
		       (and (setf bizarre
				  ;; This should always be a
				  ;; list, but be prudent just
				  ;; in case.
				  (a-formula-function formula))
			    (listp bizarre)
			    (cddr bizarre)))))
	      (when (find-direct-dependency the-form schema)
		;; This is indeed a direct-dependency formula.  Install the
		;; appropriate value.
		(s-value (on-schema formula) (on-slot formula)
			 (g-value (on-schema formula) (on-slot formula)))
		(push formula done)
		;; The formula now commits suicide.
		(delete-formula formula (not recursive-p))))))
	;; If this is a formula, eliminate dependencies to it, so we
	;; do not get warnings in propagate-change.
	(when (formula-p value)
	  (delete-formula value T))))
    (when send-destroy-message
      ;; Call the :DESTROY method.
      (kr-call-initialize-method schema :DESTROY))
    ;; Physically delete the schema.
    (delete-schema schema recursive-p)))

(defun print-one-value (value type)
  (let ((string (cond ((formula-p value)
		       (let ((cached (cached-value value))
			     (valid (cache-is-valid value)))
			 (if (or valid cached)
			     (format nil "~S(~S . ~D)"
				     value
				     cached
				     valid)
			     (format nil "~S(nil . NIL)" value))))
		      ((eq value *no-value*)
		       "")
		      (t
		       (format nil "~S" value)))))
    (when type
      (setf string (concatenate 'simple-string string
				(format nil "  ~([~S]~)" type))))
    (write-string string)
    (length string)))


(defun print-one-slot-helper (value column indent space-p type)
  (when (> column 78)
    (format t "~%    ")
    (setf column (indent-by indent)))
  (when space-p
    (write-string " "))
  (incf column (print-one-value value type))
  column)


(defun print-meta (formula)
  "Print the meta-information associated with a formula."
  (let ((meta (a-formula-meta formula)))
    (when (and meta (schema-p meta))
      (format t "  ---- meta information (~A):~%" meta)
      (call-on-ps-slots meta 'SLOT-PRINTER))))


(defun indent-by (indent)
  (dotimes (i indent)
    (write-string "   "))
  (* indent 3))


(defun force-down-helper (schema original-slots slots)
  (iterate-slot-value (schema T T NIL)
    value				; eliminate warning
    (unless (memberq slot original-slots)
      (pushnew slot slots)))
  (dolist (parent (get-local-value schema :IS-A))
    (setf slots (force-down-helper parent original-slots slots)))
  slots)


(defun force-down-all-inheritance (schema)
  "A potentially VERY expensive operation.  It is done by PS when it wants
to print out all inherited and inheritable slots of an object."
  (let ((original-slots nil))
    (iterate-slot-value (schema T NIL NIL)
      value				; eliminate warning
      (push slot original-slots))
    (dolist (slot (force-down-helper schema original-slots nil))
      (get-value schema slot))))


(defun call-func-on-one-slot (schema slot inherited-ok function
				     types-p indent limits)
  "Helper function for the following.
The <function> is called with:
(schema slot formula inherited valid real-value types-p indent limits)"
  (let* ((entry (slot-accessor schema slot))
	 (values (when entry (sl-value entry)))
	 (bits (when entry (sl-bits entry)))
	 form valid real-value)
    (when bits
      (let ((are-inherited (and (is-inherited bits)
				;; inherited formulas are printed anyway.
				(not (formula-p values)))))
	(unless (and (not inherited-ok) are-inherited)
	  (unless (eq values *no-value*)
	    (if (formula-p values)
		(let ((cached (cached-value values))
		      (is-valid (cache-is-valid values)))
		  (setq form values)
		  (setq valid is-valid)
		  (setq real-value cached))
		;; else not a formula
		(setq real-value values))
	    (funcall function
		     schema slot form are-inherited valid real-value
		     types-p bits indent limits)))
	;; Indicate that the function was called.
	T))))


(defun call-on-ps-slots (schema function
			 &key (control t)
			      inherit
			      (indent NIL)
			      types-p
			      all-p)
  "Apply the <function> to slots, the way PS would."
  (declare (special print-schema-control))
  (let ((is-ps (eq function 'SLOT-PRINTER))) ; true if inside PS
    (when (null indent)
      (setf indent 0))
    (when (numberp schema)
      (setf schema (s schema)))
    (unless (or (schema-p schema) (formula-p schema))
      (when is-ps
	(format t "~S~%" schema))
      (return-from call-on-ps-slots nil))
    (when is-ps
      (indent-by indent))
    (cond ((formula-p schema)
	   (setf control NIL))
	  ((eq control :default)
	   ;; use default control schema
	   (setf control PRINT-SCHEMA-CONTROL))
	  ((eq control T)
	   ;; use schema itself as the control schema (i.e., use hierarchy)
	   (setf control schema)))
    (let ((slots-ignored (when control (g-value-no-copy control :IGNORED-SLOTS)))
	  (sorted (when control (g-value-no-copy control :SORTED-SLOTS)))
	  (limit-values (when control (g-value-no-copy control :LIMIT-VALUES)))
	  (global-limit (if control
			    (g-value-no-copy control :GLOBAL-LIMIT-VALUES)
			    most-positive-fixnum))
	  (*print-as-structure*
	   (if (and control (g-value-no-copy control :print-as-structure))
	       ;; value is defined
	       (g-value-no-copy control :print-as-structure)
	       ;; value is undefined
	       *print-as-structure*))
	  (*print-structure-slots*
	   (when control (g-value-no-copy control :print-slots))))
      (when is-ps
	(format t "{~S~%" schema))
      ;; Print out all the sorted slots, first.
      (dolist (o sorted)
	(call-func-on-one-slot schema o inherit function types-p indent
			       (or (second (assocq o limit-values)) global-limit)))
      ;; Now print the remaining slots.
      (unless (listp slots-ignored)
	(setf slots-ignored (list slots-ignored)))
      ;; Pre-inherit all slots that are inheritable.
      (unless (a-formula-p schema)
	(when inherit
	  (force-down-all-inheritance schema))
	(if all-p
	    (iterate-slot-value (schema T T NIL)
	      (unless (or (memberq slot slots-ignored) (memberq slot sorted)
			  (eq value *no-value*))
		(call-func-on-one-slot
		 schema slot inherit function types-p indent
		 (or (second (assocq slot limit-values)) global-limit))))
	    (iterate-slot-value (schema T NIL NIL)
	      (unless (or (memberq slot slots-ignored) (memberq slot sorted)
			  (eq value *no-value*))
		(call-func-on-one-slot
		 schema slot inherit function types-p indent
		 (or (second (assocq slot limit-values)) global-limit))))))
      (when (and slots-ignored is-ps)
	(indent-by indent)
	(format t "  List of ignored slots:  ~{ ~A~}~%" slots-ignored))
      ;; special formula slots?
      (when (a-formula-p schema)
	(if is-ps
	    (progn
	      (indent-by indent)
	      (format t "  lambda:        ~(~S~)~%" (a-formula-lambda schema))
	      (format t "  cached value:  (~S . ~S)~%"
		      (cached-value schema) (cache-is-valid schema))
	      (format t "  on schema ~S, slot ~S~%"
		      (on-schema schema) (on-slot schema))
	      (indent-by indent))
	    (dolist (name '(:lambda :cached-value-valid :cached-value
			    :schema :slot))
	      (funcall function schema name nil nil T ; valid
		       (g-formula-value schema name) nil 0 indent nil)))))
    (when is-ps
      (format t "  }~%"))))


(defun the-bits (bits)
  (if (integerp bits)
      ;; The normal case
      (let ((type (extract-type-code bits)))
	(format t "~:[-~;p~]~:[-~;l~]~:[-~;C~]~:[-~;P~]~:[-~;u~]~:[-~;i~] "
		(is-parameter bits) (is-local-only bits)
		(is-constant bits) (is-parent bits)
		(is-update-slot bits) (is-inherited bits))
	(unless (zerop type)
	  (format t "[~(~S~)]   " (code-to-type type))))
      ;; A special case for formula slots which are stored in a special way
      (format t "---- ")))


(defun full-normal-slot (schema slot)
  "Helper function for FULL."
  (format t "~(~24S~) " slot)
  (let* ((entry (slot-accessor schema slot))
	 (values (if entry (sl-value entry)))
	 (bits (if entry (sl-bits entry)))
	 (dependents (slot-dependents entry)))
    (the-bits bits)
    (if entry
	;; Slot is there
	(let ((first t))
	  (when (eq values *no-value*)
	    (setf values NIL))
	  (if (and (listp values) (listp (cdr values)))
	      (when values
		(format t " (")
		(dolist (value values)
		  (if first
		      (setf first nil)
		      (write-string " "))
		  (print-one-value value NIL))
		(format t ")"))
	      (progn
		(write-string " ")
		(print-one-value values NIL)))
	  ;; Show dependent formulas, if any
	  (when dependents
	    (format t "   ****--> ")
	    (do-one-or-list (f dependents)
	      (format t " ~s" f)))
	  (terpri))
	;; No slot???
	(terpri)))
  (values))

(defun find-parent (schema slot)
  "Find a parent of <schema> from which the <slot> can be inherited."
  (dolist (relation *inheritance-relations*)
    (dolist (a-parent (if (eq relation :is-a)
			  (get-local-value schema :IS-A)
			  (get-local-value schema relation)))
      (when a-parent
	(let ((value (g-local-value a-parent slot)))
	  (if value
	      (return-from find-parent (values value a-parent))
	      (multiple-value-bind (value the-parent)
		  (find-parent a-parent slot)
		(when value
		  (return-from find-parent (values value the-parent))))))))))


(defun old-kr-send-function (schema slot &rest args)
  "Same as KR-SEND, but as a function."
  (let ((the-function (g-value schema slot)))
    (when the-function
      ;; Bind these in case call prototype method is used.
      (let ((*kr-send-self* schema)
	    (*kr-send-slot* slot)
	    (*demons-disabled* T))
	(apply the-function args)))))


(defun kr-call-initialize-method (schema slot)
  "This is similar to kr-send-function, except that it is careful NOT to
inherit the method, which is only used once.  This is to reduce unnecessary
storage in every object."
  (let ((the-function (g-value-no-copy schema slot)))
    (when the-function
      ;; Bind these in case call prototype method is used.
      (let ((*kr-send-self* schema)
	    (*kr-send-slot* slot)
	    (*kr-send-parent* NIL)
	    (*demons-disabled* T))
	(funcall the-function schema)))))


(defun kr-init-method (schema the-function)
  "Similar, but even more specialized.  It is only called by create-schema
and friends, which know whether an :initialize method was specified locally."
  (let ((*kr-send-parent* nil))
    (if the-function
      (setf *kr-send-parent* schema)
      (multiple-value-setq (the-function *kr-send-parent*)
	(find-parent schema :INITIALIZE)))
    (when the-function
      ;; Bind these in case call prototype method is used.
      (let ((*kr-send-self* schema)
	    (*kr-send-slot* :INITIALIZE)
            (*kr-send-parent* NIL)
	    #-(and) (*demons-disabled* T))
	(funcall the-function schema)))))

(defun allocate-schema-slots (schema)
  (locally (declare #.*special-kr-optimization*)
    (setf (schema-bins schema)
	  (make-hash-table :test #'eq #+sbcl :synchronized #+sbcl t)))
  schema)


(defun make-a-new-schema (name)
  "Creates a schema with the given <name>, making sure to destroy the old
one by that name if it exists.  The initial number of slots is
<needed-slots>."
  (locally (declare #.*special-kr-optimization*)
    (when (keywordp name)
      (setf name (symbol-name name)))
    (cond ((null name)
	   ;; An unnamed schema.
	   (let ((schema (make-schema)))
	     (setf *schema-counter* (1+ *schema-counter*))
	     (setf (schema-name schema) *schema-counter*)
	     (allocate-schema-slots schema)
	     schema))
	  ((stringp name)
	   ;; This clause must precede the next!
	   (let ((schema (make-schema)))
	     (allocate-schema-slots schema)
	     (setf (schema-name schema) name)
	     schema))
	  ;; Is this an existing schema?  If so, destroy the old one and its
	  ;; children.
	  ((and (boundp name)
		(symbolp name))
	   (let ((schema (symbol-value name)))
	     (setf (schema-name schema) name)
	     (set name schema)))
	  ((symbolp name)
	   (eval `(defvar ,name))
	   (let ((schema (make-schema)))
	     (allocate-schema-slots schema)
	     (setf (schema-name schema) name)
	     (set name schema)))
	  (t
	   (format t "Error in CREATE-SCHEMA - ~S is not a valid schema name.~%"
		   name)))))

(defun process-one-constant (schema slot)
  "The <slot> in <schema> was declared constant."
  ;; set slot information
  (let ((entry (slot-accessor schema slot)))
    (if (null entry)
	;; Slot is not present - create it, mark constant.
	(set-slot-accessor schema slot *no-value* *constant-mask* nil)
	;; Slot is present
	(setf (sl-bits entry) (logior *constant-mask* (sl-bits entry))))))


(defun declare-constant (schema slot)
  "Declare slot constants AFTER instance creation time."
  (unless *constants-disabled*
    (if (eq slot T)
      ;; This means that all constants declared in :MAYBE-CONSTANT should be
      ;; made constant
      (let ((maybe (g-value-no-copy schema :MAYBE-CONSTANT)))
	(dolist (m maybe)
	  (declare-constant schema m)))
      ;; This is the normal case - only 1 slot.
      (let ((constant-list (g-value schema :CONSTANT))
	    (positive T))
	(do ((list constant-list (if (listp list) (cdr list) NIL))
	     (prev nil list)
	     c)
	    ((null list)
	     (setf constant-list (cons slot (if (listp constant-list)
					      constant-list
					      (list constant-list))))
	     (s-value schema :CONSTANT constant-list)
	     (process-one-constant schema slot))
	  (setf c (if (listp list) (car list) list))
	  (cond ((eq c :EXCEPT)
		 (setf positive NIL))
		((eq c slot)
		 (when positive
		   ;; Slot is already marked constant, so there's nothing
		   ;; to do.
		   (process-one-constant schema slot)
		   (return nil))
		 ;; Slot was explicitly excepted from constant list.
		 (setf (cdr prev) (cddr prev)) ; remove from :EXCEPT
		 (when (and (null (cdr prev))
			    (eq (car prev) :EXCEPT))
		   ;; We are removing the last exception to the constant list
		   (let ((end (nthcdr (- (length constant-list) 2)
				      constant-list)))
		     (setf (cdr end) nil)))
		 (setf constant-list (cons c constant-list))
		 (s-value schema :CONSTANT constant-list)
		 (process-one-constant schema slot)
		 (return))))))))

(defun process-constant-slots (the-schema parents constants do-types)
  "Process local-only and constant declarations."
  (locally (declare #.*special-kr-optimization*)
    ;; Install all update-slots entries, and set their is-update-slot bits
    (dolist (slot (g-value-no-copy the-schema :UPDATE-SLOTS))
      (let ((entry (slot-accessor the-schema slot)))
	(if entry
	    (setf (sl-bits entry) (set-is-update-slot (sl-bits entry)))
	    (set-slot-accessor the-schema slot *no-value*
			       (set-is-update-slot *local-mask*)
			       NIL))))
    ;; Mark the local-only slots.
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
    (when (and do-types *types-enabled*)
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
		    (set-slot-accessor the-schema slot *no-value* bits NIL)))))))
      ;; Typecheck
      (iterate-slot-value (the-schema T T nil)
	(unless (eq value *no-value*)
	  (unless (formula-p value)	; don't bother with formulas.
	    (multiple-value-bind (new-value result)
		(check-slot-type the-schema slot value)
	      (when (eq result :REPLACE)
		(s-value the-schema slot new-value)))))))))


(defun add-update-slot (schema slot &optional turn-off)
  "Turn the <slot> of the <schema> into an :update-slot; add the <slot> to
the contents of :update-slots, and turn on the internal bit.  If
<turn-off> is T, make the <slot> be no longer an update slot."
  (let ((entry (slot-accessor schema slot)))
    (if entry
      ;; There is an entry - manipulate the bits directly
      (if turn-off
	;; Turn bit off
	(setf (sl-bits entry) (logand (sl-bits entry)
				      (lognot *is-update-slot-mask*)))
	;; Turn bit on
	(setf (sl-bits entry) (set-is-update-slot (sl-bits entry))))
      ;; There is no entry
      (unless turn-off
	(set-slot-accessor schema slot *no-value*
			   (set-is-update-slot *local-mask*)
			   NIL))))
  (if turn-off
    (setf (g-value schema :UPDATE-SLOTS)
	  (delete slot (g-value schema :UPDATE-SLOTS)))
    (pushnew slot (g-value schema :UPDATE-SLOTS))))


(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun cannot-be-quoted (value)
    (or (listp value)
	(and (symbolp value)
	     (not (keywordp value))))))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun process-slot-descriptor (x)
    (if (listp x)
	(if (find-if #'cannot-be-quoted (cdr x))
	    (cons 'list x)
	    `',x)
	x)))

(defun merge-declarations (declaration keyword output)
  (let ((old (find keyword output :key #'second)))
    (if old
	(setf (cadr (third old)) (union (cdr declaration)
					(cadr (third old))))
	(push `(cons ,keyword ',(cdr declaration)) output)))
  output)


(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun process-slots (slots)
    "This function processes all the parameters of CREATE-INSTANCE and returns
an argument list suitable for do-schema-body.  It is called at compile time.

RETURNS: a list, with elements as follows:
 - FIRST: the prototype (or list of prototypes), or NIL;
 - SECOND: the list of type declarations, in the form (type slot slot ...)
   or NIL (if there were no type declarations) or :NONE (if the declaration
   (type) was used, which explicitly turns off all types declared in the
   prototype(s).
 - REST OF THE LIST: all slot specifiers, with :IS-A removed (because that
   information is moved to the prototype list)."
    (let ((output nil)
	  (is-a nil))
      (do ((head slots (cdr head))
	   (types nil)
	   (had-types nil)		; true if there is a declaration
	   slot)
	  ((null head)
	   (if types
	       (push `(quote ,types) output)
	       (if had-types
		   (push :NONE output)
		   (push NIL output))))
	(setf slot (car head))
	(cond ((null slot)
	       ;; This is an error.
	       (cerror
		"Ignore the specification"
		"Error in CREATE-SCHEMA: NIL is not a valid slot ~
		 specifier; ignored.~%~
	         Object ~S, slot specifiers are ~S~%"
		kr::*create-schema-schema* head))
	      ((keywordp slot)
	       ;; Process declarations and the like.
	       (case slot
		 (:NAME-PREFIX
		  (pop head))
		 (:DECLARE
		  (pop head)
		  (dolist (declaration (if (listp (caar head))
					   (car head)
					   (list (car head))))
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
							output)))
		      (t
		       (cerror
			"Ignore the declaration"
			"Unknown declaration (~S) in object creation:~%~S~%"
			(car declaration)
			declaration)))))))
	      ((listp slot)
	       ;; Process slot descriptors.
	       (if (eq (car slot) :IS-A)
		   (setf is-a (if (cddr slot)
				  `(list ,@(cdr slot))
				  (cadr slot)))
		   (if (listp (cdr slot))
		       (if (find-if #'cannot-be-quoted (cdr slot))
			   (if (cddr slot)
			       (push `(list ,(car slot) . ,(cdr slot)) output)
			       (push `(cons ,(car slot) . ,(cdr slot)) output))
			   (if (cddr slot)
			       (push `'(,(car slot) . ,(cdr slot)) output)
			       (push `'(,(car slot) . ,(cadr slot)) output)))
		       (push (cdr slot) output))))
	      (T
	       (cerror
		"Ignore the specification"
		"A slot specification should be of the form ~
		 (:name [values]*) ;~%found ~S instead.  Object ~S, slots ~S."
		slot kr::*create-schema-schema* slots))))
      (cons is-a output))))


(defun handle-is-a (schema is-a generate-instance override)
  (if (or (eq schema is-a)
	  (memberq schema is-a))
      (format t "~A: cannot make ~S an instance of itself!  ~
    			Using NIL instead.~%"
	      (if generate-instance
		  "CREATE-INSTANCE" "CREATE-SCHEMA")
	      schema)
      ;; Make sure :override does not duplicate is-a-inv contents.
      (let ((*schema-is-new* (not override)))
	(set-is-a schema is-a))))


(defun do-schema-body (schema is-a generate-instance do-constants override
		       types &rest slot-specifiers)
  "Create-schema and friends expand into a call to this function."
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
      (handle-is-a schema is-a generate-instance override)))
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
		    (set-slot-type schema slot n)))
		(format t "*** ERROR - empty list of slots in type declaration ~
                          for object ~S:~%  ~S~%" schema (car type)))))
	;; Process the constant declarations, and check the types.
	(when do-constants
	  (process-constant-slots
	   schema is-a
	   (if had-constants
	       (if constants
		   (if (formula-p constants)
		       (g-value-formula-value schema :CONSTANT constants NIL)
		       constants)
		   :NONE)
	       ;; There was no constant declaration.
	       NIL)
	   (not (eq types :NONE))))
	;; Merge prototype and local declarations.
	(dolist (slot slot-specifiers)
	  (when (and (listp slot)
		     (memberq (car slot)
			      '(:IGNORED-SLOTS :LOCAL-ONLY-SLOTS
				:MAYBE-CONSTANT :PARAMETERS :OUTPUT
				:SORTED-SLOTS :UPDATE-SLOTS)))
	    ))
	(when generate-instance
	  ;; We are generating code for a CREATE-INSTANCE, really.
	  (kr-init-method schema initialize-method))
	schema)
    (cond ((eq slot :NAME-PREFIX)
	   ;; Skip this and the following argument
	   (pop slots))
	  ((consp slot)
	   (let ((slot-name (car slot))
		 (slot-value (cdr slot)))
	     (case slot-name		; handle a few special slots.
	       (:INITIALIZE
		(when slot-value
		  ;; A local :INITIALIZE method was provided
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

(defun do-schema-body-alt (schema is-a generate-instance do-constants override
		       types &rest slot-specifiers)
  (unless (listp is-a)
    (setf is-a (list is-a)))
  (when is-a
    (let ((*schema-is-new* T))
      (handle-is-a schema is-a generate-instance override)))
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
		    (set-slot-type schema slot n)))
		(format t "*** ERROR - empty list of slots in type declaration ~
                          for object ~S:~%  ~S~%" schema (car type)))))
	;; Process the constant declarations, and check the types.
	(when do-constants
	  (process-constant-slots
	   schema is-a
	   (if had-constants
	       (if constants
		   (if (formula-p constants)
		       (g-value-formula-value schema :CONSTANT constants NIL)
		       constants)
		   :NONE)
	       ;; There was no constant declaration.
	       NIL)
	   (not (eq types :NONE))))
	;; Merge prototype and local declarations.
	(dolist (slot slot-specifiers)
	  (when (and (listp slot)
		     (memberq (car slot)
			      '(:IGNORED-SLOTS :LOCAL-ONLY-SLOTS
				:MAYBE-CONSTANT :PARAMETERS :OUTPUT
				:SORTED-SLOTS :UPDATE-SLOTS)))
	    ))
	(when generate-instance
	  ;; We are generating code for a CREATE-INSTANCE, really.
	  (kr-init-method schema initialize-method))
	schema)
    (cond ((eq slot :NAME-PREFIX)
	   ;; Skip this and the following argument
	   (pop slots))
	  ((consp slot)
	   (let ((slot-name (car slot))
		 (slot-value (cdr slot)))
	     (case slot-name		; handle a few special slots.
	       (:INITIALIZE
		(when slot-value
		  ;; A local :INITIALIZE method was provided
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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun creation-message (name)
    (when *print-new-instances*
      (when (and (listp name)
		 (eq (car name) 'QUOTE))
	(format *standard-output* "~&Object ~S~%" (eval name))))))


(defun end-create-instance (schema)
  "Processes the second half of a create-instance.  Begin-create-instance must
have been called on the <schema>."
  (process-constant-slots schema (get-local-value schema :IS-A)
			  (get-local-value schema :CONSTANT)
			  nil)
  (kr-init-method schema (get-local-value schema :INITIALIZE)))

(declaim (inline get-slot-type-code))
(defun get-slot-type-code (object slot)
  (let ((entry (slot-accessor object slot)))
    (and entry
	 (get-entry-type-code entry))))


(declaim (inline g-type))
(defun g-type (object slot)
  (let ((type (get-slot-type-code object slot)))
    (and type
	 (code-to-type type))))


(defun check-slot-type (object slot value &optional (error-p T) entry)
  "Check whether <value> has the right type for the <slot> in the <object>.

RETURNS:
 if error-p is non-nil: multiple values:
 - a replacement value, if the user chose to continue and supply a
   replacement;  T if no error;  NIL otherwise;
 - T (if type error and the user did not supply a value), or
   NIL (if there was no type error), or
   :REPLACE, if a replacement value was supplied by the user.
 if error-p is nil:
 an error string describing what error condition was found.
"
  (let ((type-code (if entry
		       (get-entry-type-code entry)
		       (get-slot-type-code object slot))))
    (or (null type-code)
        (zerop type-code)
	(check-kr-type value type-code)
	(let* ((type (code-to-type type-code))
	       (readable-type (get-type-documentation type))
	       (message
		(format
		 nil
		 "bad KR type: value ~S~:[~*~;, a ~S,~] is not valid for slot ~S in~%  object ~S.  The slot is declared of type ~S~@[,~%  i.e., ~A~].~@[~%  The value was computed by a formula.~]~%"
		 value value (type-of value)
		 slot object type
		 readable-type
		 (formula-p (get-local-value object slot)))))
	  (if error-p
	      (progn
		(cerror "Retain old value in the slot" message)
		(values T T))
	      message)))))

(defun set-slot-type (object slot type)
  (let ((entry (or (slot-accessor object slot)
		   (set-slot-accessor object slot *no-value* type NIL))))
    (setf (sl-bits entry)
	  (logior (logand (sl-bits entry) *all-bits-mask*) type))))


(defun s-type (object slot type &optional (check-p T))
  "Adds a type declaration to the given <slot>, eliminating any previous
type declarations.  If <check-p> is true, checks whether the value
already in the <slot> satisfies the new type declaration."
  (set-slot-type object slot (if type
			       (encode-type type)
			       ;; 0 means "no type declarations"
			       0))
  (when check-p
    (let* ((entry (slot-accessor object slot))
	   (value (if entry (sl-value entry))))
      (unless (or (eq value *no-value*)
		  (formula-p value))
	(multiple-value-bind (new-value result)
	    (check-slot-type object slot value T entry)
	  (cond ((eq result :REPLACE)
		 (s-value object slot new-value)))))))
  type)


(defun get-declarations (schema declaration)
  "RETURNS: a list of all slots in the <schema> which are declared as
<declaration>.

Example: (get-declarations A :type)"
  (let ((slots nil))
    (case declaration
      (:CONSTANT nil)
      (:TYPE nil)
      ((:IGNORED-SLOTS :SORTED-SLOTS :MAYBE-CONSTANT
		       :PARAMETERS :OUTPUT :UPDATE-SLOTS)
       (return-from get-declarations (g-value schema declaration)))
      (:LOCAL-ONLY-SLOTS
       (return-from get-declarations (g-local-value schema declaration)))
      (t
       (return-from get-declarations NIL)))
    ;; Visit all slots, construct information
    (iterate-slot-value (schema T T NIL)
      value ;; suppress warning
      (let ((bits (sl-bits iterate-slot-value-entry)))
	(case declaration
	  (:CONSTANT
	   (when (is-constant bits)
	     (push slot slots)))
	  (:TYPE
	   (let ((type (extract-type-code bits)))
	     (unless (zerop type)
	       (push (list slot (code-to-type type)) slots)))))))
    slots))


(defun get-slot-declarations (schema slot)
  (let* ((entry (slot-accessor schema slot))
	 (bits (if entry (sl-bits entry) 0))
	 (declarations nil))
    (if (is-constant bits)
      (push :CONSTANT declarations))
    (if (memberq slot (g-value schema :update-slots))
      (push :UPDATE-SLOTS declarations))
    (if (memberq slot (g-value schema :local-only-slots))
      (push :LOCAL-ONLY-SLOTS declarations))
    (if (memberq slot (g-value schema :maybe-constant))
      (push :MAYBE-CONSTANT declarations))
    (let ((type (extract-type-code bits)))
      (unless (zerop type)
	(push (list :type (code-to-type type)) declarations)))
    ;; Now process all declarations that are not stored in the slot bits.
    (dolist (s-slot '(:IGNORED-SLOTS :PARAMETERS :OUTPUT :SORTED-SLOTS))
      (let ((values (g-value schema s-slot)))
	(if (memberq slot values)
	  (push s-slot declarations))))
    declarations))


(defun T-P (value)
  (declare #.*special-kr-optimization*
	   (ignore value))
  T)

(defun no-type-error-p (value)
  (cerror "Return T"
          "KR typechecking called on value ~S with no type"
          value)
  T)


(defun get-type-definition (type-descriptor)
  "Given the symbol which names a KR type (e.g., 'KR-BOOLEAN), this function
returns the type expression that was used to define the type.

Example:
 (base-type-for 'bitmap-or-nil) ==>
 (OR NULL (IS-A-P OPAL:BITMAP))
"
  (let* ((name (if (symbolp type-descriptor) (symbol-name type-descriptor)))
	 (code (gethash name kr::types-table)))
    (when name
      (maphash #'(lambda (key value)
		   (when (and (eq value code)
			    (not (stringp key)))
		       (return-from get-type-definition key)))
	       kr::types-table))))

(defun find-meta (formula)
  "Returns, or inherits, the meta-schema associated with the <formula>, or
NIL if none exists."
  (let ((meta (a-formula-meta formula)))
    (unless meta
      ;; Try to inherit the meta-information from the formula's parent(s).
      (do ((f (a-formula-is-a formula) (a-formula-is-a f)))
	  ((null f))
	(if (setf meta (a-formula-meta f))
	    ;; Do not copy down the meta-schema, to reduce storage.
	    (return))))
    meta))


(defun g-formula-value (formula slot)
  "RETURNS:
the value of the meta-slot <slot> in the <formula>, or NIL if none
exists.  The value may be inherited from the <formula>'s parent(s), but
no new meta-schema is created as a result of this operation."
  (when (formula-p formula)
    (case slot
      (:NUMBER (a-formula-number formula))
      (:VALID (cache-is-valid formula))
      (:DEPENDS-ON (a-formula-depends-on formula))
      (:SCHEMA (a-formula-schema formula))
      (:SLOT (a-formula-slot formula))
      (:CACHED-VALUE (a-formula-cached-value formula))
      (:PATH (a-formula-path formula))
      (:IS-A (a-formula-is-a formula))
      (:FUNCTION (a-formula-function formula))
      (:LAMBDA (a-formula-lambda formula))
      (:IS-A-INV (a-formula-is-a-inv formula))
      (:META (a-formula-meta formula))
      (T
       ;; Normal case: this is not a built-in formula slot.  Use meta-slots.
       (let ((meta (find-meta formula)))
	 (if meta
	     (g-value meta slot)))))))
