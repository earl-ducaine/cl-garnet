
;; (in-package :kr)

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
  schema
  slot
  cached-value
  is-a)

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

(defvar *inheritance-inverse-relations* '()
  "Inverses of all relations which perform inheritance.")

(defvar *relations* '()
  "An a-list of relations known to the system, with their inverse(s).
   Used for the creation of automatic reverse-links.")

(defvar *formula-pool* nil)

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
	  deleted-p not-deleted-p is-inherited is-parent is-constant
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

(declaim (inline slot-accessor))
(defun slot-accessor (schema slot)
  "Returns a slot structure, or NIL."
  (values (gethash slot (schema-bins schema))))

(defmacro set-slot-accessor (schema slot value bits dependents)
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

(declaim (inline get-local-value))
(defun get-local-value (schema slot)
  (locally (declare #.*special-kr-optimization*)
    (let ((entry (slot-accessor schema slot)))
      (if (if entry (not (is-inherited (sl-bits entry))))
	  (sl-value entry)))))

(declaim (inline relation-p))
(defun relation-p (slot)
  (assocq slot *relations*))

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
      (let ((new-schema nil))
	(setf intermediate new-schema)))))

(defmacro s-value (schema &rest slots)
  (when slots
    (if (cddr slots)
	`(s-value-chain ,schema ,@slots)
	`(s-value-fn ,schema ,(first slots) ,(second slots)))))

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

(defsetf g-value s-value)

(defun merge-declarations (declaration keyword output)
  (let ((old (find keyword output :key #'second)))
    (if old
	(setf (cadr (third old)) (union (cdr declaration)
					(cadr (third old))))
	(push `(cons ,keyword ',(cdr declaration)) output)))
  output)

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

(defun s-value-fn (schema slot value)
  (locally (declare #.*special-kr-optimization*)
    (unless (schema-p schema)
      (return-from s-value-fn (values value t)))
    (let* ((entry (slot-accessor schema slot)))
      (let ((is-formula nil)
	    (is-relation nil)
	    (was-formula (formula-p nil)))
	(when (formula-p value)
	  (setf is-formula T)
	  (setf (on-schema value) schema)
	  (setf (on-slot value) slot)
	  (unless (schema-name value)
	    (incf *schema-counter*)
	    (setf (schema-name value) *schema-counter*)))
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
					     slot value new-bits nil))))
	(when (and the-bits (is-parent the-bits))
	  (update-inherited-values schema slot value T))
	(when (and was-formula (not is-formula))
	  (set-cache-is-valid nil T))
	(values value nil)))))

(defun internal-s-value (schema slot value)
  (set-slot-accessor schema slot value *local-mask* nil)
  value)

(defun set-is-a (schema value)
  (set-slot-accessor schema :IS-A value *local-mask* NIL)
  (link-in-relation schema :IS-A value)
  value)

(defun find-parent (schema slot)
  (dolist (a-parent (get-local-value schema :is-a))
    (when a-parent
      (multiple-value-bind (value the-parent)
	  (find-parent a-parent slot)
	(when value
	  (return-from find-parent (values value the-parent)))))))

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
    (when do-types
      (dolist (parent parents)
	(locally
	    (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
	  (progn
	    (maphash
	     #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
		 (declare (ignore iterate-ignored-slot-name))
		 (let ((slot (sl-name iterate-slot-value-entry))
		       (bits (logand (sl-bits iterate-slot-value-entry)
				     *type-mask*)))
		   (unless (zerop bits)
		     (let ((the-entry (slot-accessor the-schema slot)))
		       (if the-entry
			   (sl-bits the-entry)
			   (set-slot-accessor the-schema slot *no-value* bits
					      nil))))))
	     (schema-bins parent))))))))

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
  (let ((parent (car (LOCALLY
 (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
 (LET* ((specific-slot-accessor (SLOT-ACCESSOR SCHEMA :IS-A))
        (slot-accessor
         (IF specific-slot-accessor
             (IF (IS-INHERITED (SL-BITS specific-slot-accessor))
                 (IF (A-FORMULA-P (SL-VALUE specific-slot-accessor))
                     (SL-VALUE specific-slot-accessor))
                 (SL-VALUE specific-slot-accessor))
             *NO-VALUE*)))
   (IF (EQ slot-accessor *NO-VALUE*)
       (IF specific-slot-accessor
           (SETF slot-accessor NIL)
           (IF (NOT
                (FORMULA-P
                 (SETF slot-accessor (G-VALUE-INHERIT-VALUES SCHEMA :IS-A))))
               (SETF slot-accessor NIL))))
   (IF (A-FORMULA-P slot-accessor)
       NIL
       slot-accessor))))))
    (locally
	(declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
      (progn
	(maphash
	 #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	     (declare (ignore iterate-ignored-slot-name))
	     (let ((slot (sl-name iterate-slot-value-entry))
		   (value (sl-value iterate-slot-value-entry)))
	       (unless (is-inherited (sl-bits iterate-slot-value-entry))
		 (unless (eq value *no-value*)
		   (let ((slot slot))
		     (let ((value (get-local-value parent slot)))
		       (s-value-fn-save-fn-symbol schema slot value)))))))
	 (schema-bins parent)))))
  (activate-new-instance-cns schema))

(defun activate-new-instance-cns (schema)
  (locally
      (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
    (progn
      (maphash
       #'(lambda (iterate-ignored-slot-name iterate-slot-value-entry)
	   (declare (ignore iterate-ignored-slot-name))
	   (let ((slot (sl-name iterate-slot-value-entry))
		 (value (sl-value iterate-slot-value-entry)))
	     (unless (is-inherited (sl-bits iterate-slot-value-entry))
	       (unless (eq value *no-value*)
		 (let ((slot slot))
		   (let ((value (get-local-value schema slot)))
		     (if (constraint-p value)
			 (add-constraint-to-slot schema slot value))))))))
       (schema-bins schema)))))

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
  (let* ((os-props (LOCALLY
		       (DECLARE (OPTIMIZE (SPEED 3) (SAFETY 0) (SPACE 0) (DEBUG 0)))
		     (LET* ((slot-accessor (SLOT-ACCESSOR OBJ :SB-OS-PROPS))
			    (specific-slot-accessor
			     (IF slot-accessor
				 (IF (IS-INHERITED (SL-BITS slot-accessor))
				     (IF (A-FORMULA-P (SL-VALUE slot-accessor))
					 (SL-VALUE slot-accessor))
				     (SL-VALUE slot-accessor))
				 *NO-VALUE*)))
		       (IF (EQ specific-slot-accessor *NO-VALUE*)
			   (IF slot-accessor
			       (SETF specific-slot-accessor NIL)
			       (IF (NOT
				    (FORMULA-P
				     (SETF specific-slot-accessor (G-VALUE-INHERIT-VALUES OBJ :SB-OS-PROPS))))
				   (SETF specific-slot-accessor NIL))))
		       (IF (A-FORMULA-P specific-slot-accessor)
			   NIL
			   specific-slot-accessor))))
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
