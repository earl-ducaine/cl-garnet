
;; (in-package :kr)


(defvar *mark-counter* 0)

(defstruct (sb-Constraint)
  variables
  strength
  methods
  selected-method
  mark
  other-slots
  set-slot-fn)

(defun get-sb-constraint-slot (obj slot)
  (case slot
    (:variables (sb-constraint-variables obj))
    (:strength (sb-constraint-strength obj))
    (:methods (sb-constraint-methods obj))
    (:selected-method (sb-constraint-selected-method obj))
    (:mark (sb-constraint-mark obj))
    (:set-slot-fn (sb-constraint-set-slot-fn obj))
    (t
     (getf (sb-constraint-other-slots obj) slot nil))))

(defmacro cn-variables (cn) `(sb-constraint-variables ,cn))
(defmacro cn-strength (cn) `(sb-constraint-strength ,cn))
(defmacro cn-methods (cn) `(sb-constraint-methods ,cn))
(defmacro cn-selected-method (cn) `(sb-constraint-selected-method ,cn))
(defmacro cn-mark (cn) `(sb-constraint-mark ,cn))

;; all slot-set forms call call-set-slot-fn

(defun set-sb-constraint-slot (cn slot val)
  (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn slot val)
  (case slot
    (:variables (setf (sb-constraint-variables cn) val))
    (:strength (setf (sb-constraint-strength cn) val))
    (:methods (setf (sb-constraint-methods cn) val))
    (:selected-method (setf (sb-constraint-selected-method cn) val))
    (:mark (setf (sb-constraint-mark cn) val))
    (:set-slot-fn (setf (sb-constraint-set-slot-fn cn) val))
    (t
     (setf (getf (sb-constraint-other-slots cn) slot nil) val)))
  val)

(defsetf cn-strength (cn) (val)
  `(let ((cn ,cn)(val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :strength val)
     (setf (sb-constraint-strength cn) val)))

(defsetf cn-variables (cn) (val)
  `(let ((cn ,cn)(val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :variables val)
     (setf (sb-constraint-variables cn) val)))

(defsetf cn-methods (cn) (val)
  `(let ((cn ,cn)(val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :methods val)
     (setf (sb-constraint-methods cn) val)))

(defsetf cn-selected-method (cn) (val)
  `(let ((cn ,cn)(val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :selected-method val)
     (setf (sb-constraint-selected-method cn) val)))

(defsetf cn-mark (cn) (val)
  `(let ((cn ,cn)(val ,val))
     (call-set-slot-fn (sb-constraint-set-slot-fn cn) cn :mark val)
     (setf (sb-constraint-mark cn) val)))

(defstruct (sb-Method)
  code
  outputs
  other-slots
  set-slot-fn)

(defun get-sb-method-slot (mt slot)
  (case slot
    (:code (sb-method-code mt))
    (:outputs (sb-method-outputs mt))
    (:set-slot-fn (sb-method-set-slot-fn mt))
    (t
     (getf (sb-method-other-slots mt) slot nil))))

(defmacro mt-code (mt) `(sb-method-code ,mt))
(defmacro mt-outputs (mt) `(sb-method-outputs ,mt))

(defsetf mt-code (mt) (val)
  `(let ((mt ,mt)(val ,val))
     (call-set-slot-fn (sb-method-set-slot-fn mt) mt :code val)
     (setf (sb-method-code mt) val)))

(defsetf mt-outputs (mt) (val)
  `(let ((mt ,mt)(val ,val))
     (call-set-slot-fn (sb-method-set-slot-fn mt) mt :outputs val)
     (setf (sb-method-outputs mt) val)))

(defun create-sb-method (&key (name nil)
			   (code #'(lambda (cn) cn))
			   (outputs nil))
  (make-sb-method :code code
		  :outputs outputs
		  :other-slots nil))

(defstruct (sb-variable)
  constraints
  determined-by
  walk-strength
  mark
  valid
  value
  other-slots
  set-slot-fn)

(defmacro var-value (var) `(sb-variable-value ,var))
(defmacro var-constraints (var) `(sb-variable-constraints ,var))
(defmacro var-determined-by (var) `(sb-variable-determined-by ,var))
(defmacro var-walk-strength (var) `(sb-variable-walk-strength ,var))
(defmacro var-mark (var) `(sb-variable-mark ,var))
(defmacro var-valid (var) `(sb-variable-valid ,var))

(defun set-sb-variable-slot (var slot val)
  (call-set-slot-fn (sb-variable-set-slot-fn var) var slot val)
  (case slot
    (:value (setf (sb-variable-value var) val))
    (:constraints (setf (sb-variable-constraints var) val))
    (:determined-by (setf (sb-variable-determined-by var) val))
    (:walk-strength (setf (sb-variable-walk-strength var) val))
    (:mark (setf (sb-variable-mark var) val))
    (:valid (setf (sb-variable-valid var) val))
    (:set-slot-fn (setf (sb-variable-set-slot-fn var) val))
    (t
     (setf (getf (sb-variable-other-slots var) slot nil) val))))

(defsetf var-value (var) (val) `(set-sb-variable-slot ,var :value ,val))
(defsetf var-constraints (var) (val) `(set-sb-variable-slot ,var :constraints ,val))
(defsetf var-determined-by (var) (val) `(set-sb-variable-slot ,var :determined-by ,val))
(defsetf var-walk-strength (var) (val) `(set-sb-variable-slot ,var :walk-strength ,val))
(defsetf var-mark (var) (val) `(set-sb-variable-slot ,var :mark ,val))
(defsetf var-valid (var) (val) `(set-sb-variable-slot ,var :valid ,val))

(defun create-sb-variable (&key (name nil)
				(value nil)
				(constraints nil)
				(determined-by nil)
				(walk-strength :min)
				(mark 0)
				(valid t)
				(set-slot-fn nil))
 (make-sb-variable :value value
			       :constraints constraints
			       :determined-by determined-by
			       :walk-strength :max
			       :mark mark
			       :valid valid
			       :set-slot-fn set-slot-fn
			       :other-slots nil))

(defun call-set-slot-fn (fns obj slot val)
  (cond ((null fns)
	 nil)
	((listp fns)
	 (loop for fn in fns do (call-set-slot-fn fn obj slot val)))
	(t
	 (funcall fns obj slot val))))

(defmacro enforced (c)
  `(cn-selected-method ,c))

(defun selected-method-input-vars (cn)
  (method-input-vars cn (cn-selected-method cn)))

(defun add-constraint (cn)
    (setf (CN-selected-method cn) nil)
    (setf (CN-mark cn) nil)
    (loop for v in (CN-variables cn) do
	  (push cn (VAR-constraints v)))
  cn)


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
(defsetf cn-variable-paths (c) (val) `(set-sb-constraint-slot ,c :mg-variable-paths ,val))


(defun create-mg-constraint (&key variable-paths variable-names)
  (let ((cn (make-sb-constraint)))
    (setf (cn-connection cn) :unconnected)
    (setf (cn-variable-paths cn) variable-paths)
    cn))

(defsetf var-os (v) (val) `(set-sb-variable-slot ,v :mg-os ,val))

(defun create-mg-variable (&key (name nil)
			     (os nil))
  (let* ((val (if (os-p os)
		  (g-value (os-object os) (os-slot os))
		  nil))
	 (var (create-sb-variable :name name
				  :value val)))
    (setf (VAR-os var) os)
    var))

(defun constraint-p (obj)
  (and (sb-constraint-p obj)
       (not (null (CN-connection obj)))))

(defun add-constraint-to-slot (obj slot cn)
  (setf (CN-os cn) (os obj slot))
  (connect-constraint cn))

(defun kr-init-method-hook (schema &optional the-function)
  (kr-init-method-save-fn-symbol schema the-function)
  (copy-down-and-activate-constraints schema))

(defun copy-down-and-activate-constraints (schema)
  (let ((parent (car (get-value schema :is-a))))
    (let* ((local-only-slots-val (g-value-no-copy parent :LOCAL-ONLY-SLOTS))
	   (local-only-slots (if (listp local-only-slots-val)
				 local-only-slots-val
				 (list local-only-slots-val))))
      (doslots (slot parent)
	(let ((value (get-local-value parent slot)))
	  (s-value-fn-save-fn-symbol schema slot value)))))
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
  (let ((var (create-mg-variable :os (os obj slot))))
    (set-object-slot-prop obj slot :sb-variable var)
    (s-value-fn-save-fn-symbol obj slot (g-value obj slot))
    var))

(eval-when (:load-toplevel :execute)
  (loop for (fn hook-fn) on '(s-value-fn s-value-fn-save-fn-symbol
			      kr-init-method kr-init-method-hook
			      ) by #'CDDR
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
