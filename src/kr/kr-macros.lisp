
(in-package :kr)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *special-kr-optimization*
    '(optimize
      (speed 3)
      (safety 0)
      (space 0)
      (debug 0))))

(defstruct (schema (:predicate is-schema))
  name
  bins)

(declaim (inline schema-p))
(defun schema-p (obj)
  (locally (declare #.*special-kr-optimization*)
    (and (is-schema obj)
	 (hash-table-p (schema-bins obj))
	 T)))

(defstruct (a-formula (:include schema))
  depends-on
  schema
  slot
  cached-value
  path
  is-a
  function
  lambda
  is-a-inv
  meta)

(defstruct (sl)
  name
  value
  (bits 0 :type fixnum))


(defstruct (full-sl (:include sl))
  dependents)

(defvar *within-g-value* nil
  "Set to non-nil within a sub-formula evaluation")

(declaim (fixnum *sweep-mark*))
(defvar *sweep-mark* 0
  "Used as a sweep mark to detect circularities")

(defvar *constants-disabled* NIL
  "May be bound to NIL to cause constant declarations to be ignore in
  create-instance.")

(defvar *redefine-ok* NIL
  "May be bound to T to allow create-instance to redefine slots that were
  declare constant in the prototype.")

(defvar *pre-set-demon* nil
  "May be bound to a function to be called as a slot is set in a schema
  with the slots new-value.")

(defvar *slot-setter-debug* nil)

(defvar *schema-self* nil
  "The schema being acted upon by the accessor functions.")

(defvar *schema-slot* nil
  "The slot in *schema-self* being acted upon by the accessor functions.")

(defvar *current-formula* nil)

(defvar *last-formula* nil
  "Similar to *current-formula*, used for debugging only.")

(defvar *inheritance-relations* '()
  "All relations in this list perform inheritance.")

(defvar *inheritance-inverse-relations* '()
  "Inverses of all relations which perform inheritance.")

(defvar *relations* '()
  "An a-list of relations known to the system, with their inverse(s).
   Used for the creation of automatic reverse-links.")

(defvar *formula-pool* nil)

(defun formula-push (f)
    (push f *formula-pool*))

(defun formula-pop ()
    (and *formula-pool* (pop *formula-pool*)))

(defvar *schema-is-new* nil
  "If non-nil, we are inside the creation of a new schema.  This guarantees
  that we do not have to search for inverse links when creating relations,
  and avoids the need to scan long is-a-inv lists.")

(defvar *print-as-structure* T
  "If non-nil, schema names are printed as structure references.")

(defvar *print-structure-slots* nil
  "List of slots that should be printed when printing schemata as structures.")

(defparameter *no-value* '(:no-value)
  "A cons cell which is used to mark the value of non-existent slots.")

(declaim (fixnum *schema-counter*))
(defvar *schema-counter* 0
  "This variable is used to generate schema numbers for schemata that
  are created with (create-schema NIL).")


(declaim (fixnum *type-bits* *type-mask* *inherited-bit*
		 *is-parent-bit* *is-constant-bit* *is-update-slot-bit*
		 *is-local-only-slot-bit* *is-parameter-slot-bit*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *type-bits* 10)  ;; # of bits for encoding type
  (defparameter *type-mask* (1- (expt 2 *type-bits*))) ;; to extract type
  (defparameter *inherited-bit*          *type-bits*)
  (defparameter *is-parent-bit*          (1+ *inherited-bit*))
  (defparameter *is-constant-bit*        (1+ *is-parent-bit*))
  (defparameter *is-update-slot-bit*     (1+ *is-constant-bit*))
  (defparameter *is-local-only-slot-bit* (1+ *is-update-slot-bit*))
  (defparameter *is-parameter-slot-bit*  (1+ *is-local-only-slot-bit*)))


(declaim (fixnum *local-mask* *constant-mask* *is-update-slot-mask*
		 *inherited-mask* *is-parent-mask* *clear-slot-mask*
		 *inherited-parent-mask* *not-inherited-mask*
		 *not-parent-mask* *not-parent-constant-mask*
		 *all-bits-mask*))
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defparameter *local-mask* 0)
  (defparameter *constant-mask* (ash 1 *is-constant-bit*))
  (defparameter *is-update-slot-mask* (ash 1 *is-update-slot-bit*))
  (defparameter *inherited-mask* (ash 1 *inherited-bit*))
  (defparameter *is-parent-mask* (ash 1 *is-parent-bit*))
  (defparameter *clear-slot-mask*
    (logior *local-mask* *type-mask* *constant-mask* *is-update-slot-mask*))
  (defparameter *inherited-parent-mask*
    (logior *inherited-mask* *is-parent-mask*))
  (defparameter *not-inherited-mask* (lognot *inherited-mask*))
  (defparameter *not-parent-mask* (lognot *is-parent-mask*))
  (defparameter *not-parent-constant-mask*
    (lognot (logior *is-parent-mask* *constant-mask*)))
  (defparameter *all-bits-mask* (lognot *type-mask*)))

(defvar *check-constants* NIL)

(defvar *is-constant* T)

(defvar *accessed-slots* NIL)

(defvar *create-schema-schema* nil
  "Name of the current object being defined by Create-Instance.  Used for
   debugging only.")

(defmacro when-debug (&rest forms)
  (declare (ignore forms))
  nil)

(declaim (inline
	  formula-p deleted-p not-deleted-p is-inherited is-parent is-constant
	  is-update-slot set-is-update-slot is-local-only is-parameter
	  extract-type-code get-entry-type-code))

(defun formula-p (thing)
  (a-formula-p thing))

(defun deleted-p (schema)
  (declare #.*special-kr-optimization*)
  (null (schema-bins schema)))

(defun not-deleted-p (schema)
  (declare #.*special-kr-optimization*)
  (schema-bins schema))

(defun is-inherited (bits)
  (declare (fixnum bits))
  (logbitp *inherited-bit* bits))

(defun is-parent (bits)
  (declare (fixnum bits))
  (logbitp *is-parent-bit* bits))

(defun is-constant (bits)
  (declare (fixnum bits))
  (logbitp *is-constant-bit* bits))

(defun is-update-slot (bits)
  (declare (fixnum bits))
  (logbitp *is-update-slot-bit* bits))

(defun set-is-update-slot (bits)
  (declare (fixnum bits))
  (logior *is-update-slot-mask* bits))

(defun is-local-only (bits)
  (declare (fixnum bits))
  (logbitp *is-local-only-slot-bit* bits))

(defun is-parameter (bits)
  (declare (fixnum bits))
  (logbitp *is-parameter-slot-bit* bits))

(defun extract-type-code (bits)
  (declare (fixnum bits))
  (logand *type-mask* bits))

(defun get-entry-type-code (entry)
  (declare #.*special-kr-optimization*)
  (extract-type-code (sl-bits entry)))


(defmacro def-kr-type (typename-or-type &optional args body type-doc)
  "Defines a new type for KR's type-checking mechanism.  You must define
a type using def-kr-type before you can reference that type.  There
are 2 formats for def-kr-type, one named, one un-named, as the following
examples show:

     (def-kr-type my-named-type () '(or keyword null))
     (def-kr-type '(or keyword null))

Note that the first format is the same syntax as Lisp's 'deftype'.
With either definition, you could then specify some object's type to be
 (OR KEYWORD NULL).  With the first defn, you could also specify the type
to be \"MY-NAMED-TYPE\".

You can also provide a documentation string as the last parameter, as in:
     (def-kr-type my-named-type () '(or keyword null) \"Sample doc string\")"

  (cond ((listp typename-or-type)
	   (unless (eq (car typename-or-type) 'QUOTE)
	     (error "Illegal typename to def-kr-type: ~S" typename-or-type))
	   (unless (and (null args) (null body) (null type-doc))
	     (error "Illegal specification: (DEF-KR-TYPE ~S ~S ~S ~S)"
			typename-or-type args body type-doc))
	   (setq body typename-or-type)
	   (setq typename-or-type NIL))
        (args
	   (error "DEF-KR-TYPE only works with NULL args, not ~S~%" args))
        (T
	   (setq typename-or-type (symbol-name typename-or-type))))
  (setq body (eval body))
  `(add-new-type ,typename-or-type ',body ,(type-to-fn body) ,type-doc))

(defmacro memberq (item list)
  "Member, but with a test of EQ.  Interestingly, if 'item' is a keyword,
then it is faster to use the normal member fn!"
  (if (keywordp item)
  `(member ,item ,list)
  `(member ,item ,list :test #'eq)))


(defmacro assocq (item alist)
  "Assoc, but with a test of EQ."
  (if (keywordp item)
  `(assoc ,item ,alist)
  `(assoc ,item ,alist :test #'eq)))


(defmacro do-one-or-list ((var list &optional use-continue) &body body)
  "Execute the <body> on each element of the <list>, or only once if the
<list> is a single value."
  `(let* ((do-one-list ,list)
	  (,var (if (listp do-one-list) (car do-one-list) do-one-list)))
    (block nil
      (tagbody
       again
	 (if (null do-one-list)
	     (return-from nil nil))
	 ,@body
       ,@(if use-continue
	   '(endbody))
	 (if (not (listp do-one-list))
	     (return-from nil nil))
	 (setq do-one-list (cdr do-one-list)
	       ,var (car do-one-list))
	 (go again)))))


(defmacro push-one-or-list (item accessor-form &optional check-new-p)
  `(let ((current ,accessor-form))
    (if (null current)
      (setf ,accessor-form ,item)
      (if (listp current)
	,@(if check-new-p
	    `((if (not (member ,item current))
	      (setf ,accessor-form (cons ,item current))))
	    `((setf ,accessor-form (cons ,item current))))
	,@(if check-new-p
	    `((if (not (eq ,item current))
		(setf ,accessor-form (list ,item current))))
	    `((setf ,accessor-form (list ,item current))))))))


(defmacro delete-one-or-list (item accessor-form)
  `(let ((current ,accessor-form))
    (if (listp current)
      (setf ,accessor-form (delete ,item current))
      (if (eq ,item current)
	(setf ,accessor-form NIL)))))

(defmacro continue-out ()
  "Allow the current iteration of do-one-or-list to be terminated
prematurely."
  `(go endbody))


(declaim (inline get-dependent-formula))
(defun get-dependent-formula (dependency)
  (car dependency))


(declaim (inline slot-dependents))
(defun slot-dependents (slot-structure)
  (declare #.*special-kr-optimization*)
  ;; (let ((entry slot-structure))
  ;;   (when (full-sl-p entry)
  ;;     (full-sl-dependents entry)))
  )


(declaim (inline slot-accessor))
(defun slot-accessor (schema slot)
  "Returns a slot structure, or NIL."
  (values (gethash slot (schema-bins schema))))


(defmacro set-slot-accessor (schema slot value bits dependents)
  "Returns the slot structure it created or modified.
SIDE EFFECTS: if <dependents> is specified, the slot structure is
modified to be a full-slot structure."
  (let ((the-bins (gensym))
	(the-entry (gensym))
	(the-dependents (gensym)))
    `(let* ((,the-bins (schema-bins ,schema))
	    (,the-entry (gethash ,slot ,the-bins))
	    (,the-dependents ,dependents))
       (if ,the-entry
	   (progn
	     (when (and ,the-dependents (not (full-sl-p ,the-entry)))
	       ;; Need to use a full slot, only have a short one.
	       (setf (gethash ,slot ,the-bins) (setf ,the-entry (make-full-sl)))
	       (setf (sl-name ,the-entry) ,slot))
	     ;; Slot is present - update it.
	     (setf (sl-value ,the-entry) ,value)
	     (setf (sl-bits ,the-entry) ,bits)
	     (when ,the-dependents
	       (setf (full-sl-dependents ,the-entry) ,the-dependents))
	     ,the-entry)
	   ;; Slot is not present - create it.
	   (progn
	     (setf ,the-entry (if ,the-dependents (make-full-sl) (make-sl)))
	     (setf (sl-name ,the-entry) ,slot)
	     (setf (sl-value ,the-entry) ,value)
	     (setf (sl-bits ,the-entry) ,bits)
	     (when ,the-dependents
	       (setf (full-sl-dependents ,the-entry) ,the-dependents))
	     (setf (gethash ,slot ,the-bins) ,the-entry))))))

(defmacro a-formula-number (formula)
  `(the (or null fixnum) (a-formula-bins ,formula)))

(defmacro set-formula-number (formula value)
  `(setf (a-formula-number ,formula) ,value))

(defmacro on-schema (formula)
  `(a-formula-schema ,formula))

(defmacro on-slot (formula)
  `(a-formula-slot ,formula))

(defmacro cached-value (thing)
  `(a-formula-cached-value ,thing))

(defmacro cache-is-valid (thing)
  `(logbitp 0 (a-formula-number ,thing)))

(defmacro set-cache-is-valid (thing value)
  (if value
      `(set-formula-number ,thing (logior (a-formula-number ,thing) 1))
      `(set-formula-number ,thing
	(logand (a-formula-number ,thing) ,(lognot 1)))))

(defmacro cache-mark (thing)
  `(logand (a-formula-number ,thing) (lognot 1)))

(defparameter iterate-slot-value-entry nil
  "Ugly")

(defmacro iterate-slot-value ((a-schema inherited everything check-formula-p)
			      &body body)
"Iterate the <body> for all the slots in the <schema>, with the variable
<slot> bound to each slot in turn and the variable <value> bound to
the <slot>'s value.
If <everything> is T, even slots which contain *no-value* (but with same
bit set) are used."
  `(locally (declare ,*special-kr-optimization*)
     (,@(if check-formula-p `(if (not (formula-p ,a-schema))) '(progn))
	(maphash
	 #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	     (declare (ignore iterate-ignored-slot-name))
	     (let ((slot (sl-name iterate-slot-value-entry)) ; name for the slot
		   (value (sl-value iterate-slot-value-entry)))
	       ;; This slot exists
	       ,@(if inherited
		     ;; Either local or inherited will do.
		     (if everything
			 ;; Execute on a no-value, too.
			 body
			 ;; Only execute on real values.
			 `((unless (eq value *no-value*)
			     ,@body)))
		     ;; Make sure that the slot is not inherited.
		     `((unless (is-inherited (sl-bits iterate-slot-value-entry))
			 ,@(if everything
			       body
			       `((unless (eq value *no-value*)
				   ,@body))))))))
	 (schema-bins ,a-schema)))))

(defmacro doslots ((slot-var a-schema &optional inherited) &body body)
"Executes the <body> with <slot> bound in turn to each slot in the <schema>."
  `(iterate-slot-value (,a-schema ,inherited NIL NIL)
     (let ((,slot-var slot))
       ,@body)))

(declaim (inline get-local-value))
(defun get-local-value (schema slot)
  (locally (declare #.*special-kr-optimization*)
    (let ((entry (slot-accessor schema slot)))
      (if (if entry (not (is-inherited (sl-bits entry))))
	  (sl-value entry)))))

(defmacro expand-accessor (accessor-function schema &rest slots)
  (if slots
      (let ((kernel schema))
	(do ((slot slots (cdr slot)))
	    ((null slot))
	  (setf kernel
		`(,accessor-function ,kernel ,(car slot))))
	kernel)
      (error "expand-accessor: at least one slot is required")))

(defmacro with-constants-disabled (&body body)
"Execute the <body> with constant processing disabled."
  `(let ((*constants-disabled* t))
     ,@body))

(defmacro with-dependencies-disabled (&body body)
"Execute the <body> with dependencies processing disabled."
  `(let ((*setup-dependencies* nil))
     ,@body))

(declaim (inline relation-p))
(defun relation-p (slot)
  (assocq slot *relations*))

(defmacro g-value-body (schema slot)
  (let ((schema-form (if (symbolp schema) schema 'schema))
	(entry (gensym))
	(value (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let* (,@(unless (symbolp schema) `((schema ,schema)))
	      (,entry
	       (slot-accessor ,schema-form ,slot))
		(,value (if ,entry
			    ,@(progn
				  `((if (is-inherited (sl-bits ,entry))
					,@(progn
					      `((if (a-formula-p (sl-value ,entry))
						    (sl-value ,entry)))
					      )
					(sl-value ,entry))))
			    ,@(progn
				  `(*no-value*)))))
	 (if (eq ,value *no-value*)
	     ,@(cond
		     (t
		      `((if ,entry
			    (setf ,value NIL)
			    (if (not (formula-p (setf ,value
						      (g-value-inherit-values ,schema-form ,slot))))
				(setf ,value NIL)))))
		     ))
	 ,@(progn
	       `((if (a-formula-p ,value)
		     nil
		     ,value)))))))

(defmacro get-value (schema slot)
  `(g-value-body ,schema ,slot))

(defmacro g-value (schema &rest slots)
  (if slots
      nil
    `(progn ,schema)))

(defun s-value-chain (schema &rest slots)
  (locally (declare #.*special-kr-optimization*)
    (if (null schema)
	(error "S-VALUE on a null object:  (S-VALUE ~S~{ ~S~})" schema slots)
	(unless (schema-p schema)
	  (error "S-VALUE called with the non-object ~S :  (s-value ~S~{ ~S~})."
		 schema schema slots)))
    (do* ((s slots (cdr s))
	  (intermediate schema))
	 ((null (cddr s))
	  (s-value-fn intermediate (first s) (second s)))
      (let ((new-schema nil
			))
	(if (null new-schema)
	    (error
	     "An intermediate schema is null:  slot ~S of object ~S has value
  NIL in (S-VALUE ~S~{ ~S~})"
	     (car s) intermediate schema slots)
	    (unless (schema-p new-schema)
	      (error "An intermediate value is not a schema in (S-VALUE ~S~{ ~S~}),
at slot ~S  (non-schema value is ~S, last schema was ~S)"
		     schema slots (car s) new-schema intermediate)))
	(setf intermediate new-schema)))))

(defmacro s-value (schema &rest slots)
  (when slots
    (if (cddr slots)
	`(s-value-chain ,schema ,@slots)
	`(s-value-fn ,schema ,(first slots) ,(second slots)))))

(defmacro create-relation (relation inheritance-p &rest inverses)
"Defines a new relation with its inverses.  If <inheritance-p>
is non-nil, classifies the relation as one that performs inheritance.
Note that <relation> should be a slot name, not a schema."
  (let ((entry (gensym)))
    `(let ((inverses ',inverses))
      (when ,inheritance-p
	(pushnew ,relation *inheritance-relations*)
	(dolist (inverse inverses)
	  (pushnew inverse *inheritance-inverse-relations*)))
      (unless (assocq ,relation *relations*)
	(push (cons ,relation inverses) *relations*))
      (dolist (inv inverses)
	(let ((,entry (assocq inv *relations*)))
	  (if ,entry
	    (pushnew ,relation (cdr ,entry))
	    (progn
	      (push (list inv ,relation) *relations*))))))))


(declaim (inline has-slot-p))
(defun has-slot-p (schema slot)
  (locally (declare #.*special-kr-optimization*)
    (let ((entry (slot-accessor schema slot)))
      (and entry
	   (not (eq (sl-value entry) *no-value*))
	   (not (is-inherited (sl-bits entry)))))))

(defmacro kr-send (schema slot &rest args)
  (let ((the-schema (gensym))
	(the-function (gensym)))
    `(let* ((,the-schema ,schema)
	    (,the-function (g-value ,the-schema ,slot)))
       (when ,the-function
	 (funcall ,the-function ,@args)))))

(defmacro define-method (name class arg-list &rest body)
  (unless (keywordp name)
    (setf name (intern (symbol-name name) (find-package "KEYWORD")))
    (format t "DEFINE-METHOD takes a keyword as the method name - using ~S~%"
	    name))
  (let* ((function-name (intern (concatenate 'string
					     (symbol-name name)
					     "-METHOD-"
					     (symbol-name class)))))
    `(progn
       (defun ,function-name ,arg-list
	 ,@body)
       (s-value ,class ,name ',function-name))))

(defmacro create-schema (name &rest rest)
  (let ((prefix (memberq :NAME-PREFIX rest)))
    (when prefix
      (if name
	  (progn
	    (setf prefix nil))
	  (progn
	    (setf name (second prefix))
	    (setf prefix NIL))))
    (when (and (listp name) (eq (car name) 'QUOTE))
      (proclaim `(special ,(eval name))))
    (let* ((override (not (null (memberq :OVERRIDE rest))))
	   (destroy (and name (not override)))
	   (*create-schema-schema* name)
	   (slots (process-slots rest))
	   (generate-instance (not (null (memberq :generate-instance rest)))))
      `(do-schema-body
	   ,(if destroy
		`(make-a-new-schema ,name)
		(if (and (listp name)
			 (eq (car name) 'QUOTE)
			 (boundp (second name)))
		    (eval name)
		    `(make-a-new-schema ,name)))
	 ,(car slots)
	 ,generate-instance
		 ,override
	 ,@(cdr slots)))))

(defmacro create-prototype (name &rest slots)
  "Creates a prototype; really just another name for create-schema."
  `(create-schema ,name ,@slots))

(defmacro create-instance (name class &body body)
  "If CLASS is not nil, creates a schema with an IS-A slot set to that class.
   Otherwise, just creates a schema."
  (when (and (listp class)
	     (eq (car class) 'QUOTE))
    ;; Prevent a common mistake.
    (cerror
     "Remove the quote and use the resulting object."
     "  Quoted symbols cannot be used as prototypes: (create-instance ~S ~S)~%"
     name class)
    (setf class (eval (second class))))
  (dolist (element body)
    (when (and (listp element) (eq (car element) :IS-A))
      (format
       t
       "CREATE-INSTANCE ~S ~S: do not specify the :IS-A slot!  Ignored.~%"
       name class)
      (setf body (remove (assocq :IS-A body) body))))
  `(progn
     (create-schema ,name :GENERATE-INSTANCE
		    ;; class might be nil, which means no IS-A slot
		    ,@(if class `((:is-a ,class)))
		    ,@body)))



(defsetf g-value s-value)
