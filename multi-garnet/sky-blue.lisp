
(in-package :kr)


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

(defun create-sb-constraint (&key (name nil)
                               (variables nil)
                               (strength :max)
                               (methods nil)
                               (selected-method nil)
                               (mark nil)
			       (set-slot-fn nil)
                               )
  (let ((cn (make-sb-constraint :strength :max
                                :methods methods
                                :variables variables
                                :selected-method selected-method
                                :mark mark
				:set-slot-fn set-slot-fn
                                :other-slots nil)))
    (when name (set-sb-slot cn :name name))
    cn))

;; methods:
;;   a method represents one possible procedure for satisfying a constraint.
;;   fields of a method representation are initialized at the constraint
;;   creation time and never modified.
;;
;;   field        | type      | description
;;   -------------+-----------+--------------------------------------------
;;   code         | procedure | the procedure to be called to execute this
;;                |           | method (passed the constraint whose selected
;;                |           | method this is).
;;   outputs      | list of vars | list of output vars for this method.
;; other-slots    | alist     | association list of other slot/value pairs
;;   set-slot-fn  |  fn(s)    | fn or list of fns to call before setting any slot

(defstruct (sb-Method
	    (:print-function
	     (lambda (mt str lvl)
	       (declare (ignore lvl))
	       (cond ((get-sb-slot mt :name)
		      (format str "{mt-~A}" (get-sb-slot mt :name)))
		     (t (format str "{mt}")))))
	    )
  code
  outputs
  other-slots
  set-slot-fn
  )

(defun get-sb-method-slot (mt slot)
  (case slot
    (:code (sb-method-code mt))
    (:outputs (sb-method-outputs mt))
    (:set-slot-fn (sb-method-set-slot-fn mt))
    (t
     (getf (sb-method-other-slots mt) slot nil))))

(defmacro mt-code (mt) `(sb-method-code ,mt))
(defmacro mt-outputs (mt) `(sb-method-outputs ,mt))

(defun set-sb-method-slot (mt slot val)
  (call-set-slot-fn (sb-method-set-slot-fn mt) mt slot val)
  (case slot
    (:code (setf (sb-method-code mt) val))
    (:outputs (setf (sb-method-outputs mt) val))
    (:set-slot-fn (setf (sb-method-set-slot-fn mt) val))
    (t
     (setf (getf (sb-method-other-slots mt) slot nil) val))))

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
  (let ((mt (make-sb-method :code code
			    :outputs outputs
			    :other-slots nil)))
    (when name (set-sb-slot mt :name name))
    mt))

;; variables:
;;
;;   field        | type        | description
;;  --------------+-------------+--------------------------------------------
;;  value         | any         | value of the variable
;;  constraints   | set of      | all the constraints that reference this
;;                | constraints | variable.
;;  determined-by | constraint  | the constraint that determines this
;;                |             | variable's value.
;;  walk-strength | strength    | the walkabout strength of this variable.
;;  mark          | integer     | this variable's mark value.
;;  valid         | boolean     | true if this variable value is valid
;; other-slots    | alist       | association list of other slot/value pairs
;; set-slot-fn    | fn(s)       | fn or list of fns to call before setting any slot

(defstruct (sb-Variable
	    (:print-function
	     (lambda (var str lvl)
	       (declare (ignore lvl))
	       (cond ((get-sb-slot var :name)
		      (format str "{var-~A}" (get-sb-slot var :name)))
		     (t (format str "{var}")))))
	    )
  constraints
  determined-by
  walk-strength
  mark
  valid
  value
  other-slots
  set-slot-fn
  )

(defun get-sb-variable-slot (var slot)
  (case slot
    (:value (sb-variable-value var))
    (:constraints (sb-variable-constraints var))
    (:determined-by (sb-variable-determined-by var))
    (:walk-strength (sb-variable-walk-strength var))
    (:mark (sb-variable-mark var))
    (:valid (sb-variable-valid var))
    (:set-slot-fn (sb-variable-set-slot-fn var))
    (t
     (getf (sb-variable-other-slots var) slot nil))))

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
				(set-slot-fn nil)
				)
  (let ((var (make-sb-variable :value value
			       :constraints constraints
			       :determined-by determined-by
			       :walk-strength :max
			       :mark mark
			       :valid valid
			       :set-slot-fn set-slot-fn
			       :other-slots nil)))
    (when name (set-sb-slot var :name name))
    var))

;; ***** generic sb object fns *****

(defun get-sb-slot (obj slot)
  (etypecase obj
    (sb-constraint (get-sb-constraint-slot obj slot))
    (sb-variable (get-sb-variable-slot obj slot))
    (sb-method (get-sb-method-slot obj slot))))

(defun set-sb-slot (obj slot val)
  (etypecase obj
    (sb-constraint (set-sb-constraint-slot obj slot val))
    (sb-variable (set-sb-variable-slot obj slot val))
    (sb-method (set-sb-method-slot obj slot val))))

(defun sb-object-p (obj)
  (or (sb-constraint-p obj)
      (sb-variable-p obj)
      (sb-method-p obj)))

;; ***** set-slot-fn handling *****

(defun call-set-slot-fn (fns obj slot val)
  (cond ((null fns)
	 nil)
	((listp fns)
	 (loop for fn in fns do (call-set-slot-fn fn obj slot val)))
	(t
	 (funcall fns obj slot val))))

(defun add-set-slot-fn (obj fn)
  (let* ((curr (get-sb-slot obj :set-slot-fn))
	 (curr-list (if (listp curr) curr (list curr))))
    (unless (member fn curr-list)
      (set-sb-slot obj :set-slot-fn (cons fn curr)))
    ))

(defun remove-set-slot-fn (obj fn)
  (let* ((curr (get-sb-slot obj :set-slot-fn))
	 (curr-list (if (listp curr) curr (list curr))))
    (set-sb-slot obj :set-slot-fn (remove fn curr-list))
    ))

;; useful macros

(defmacro enforced (c)
  `(cn-selected-method ,c))

(defmacro do-method-output-vars ((var-var cn-form mt-form) . body)
  (declare (ignore cn-form))
  `(loop for ,var-var in (MT-outputs ,mt-form) do
	 (progn ,@body)))

(defmacro do-selected-method-output-vars ((var-var cn-form) . body)
  `(loop for ,var-var in (MT-outputs (cn-selected-method ,cn-form))
       do (progn ,@body)))

(defmacro do-method-input-vars ((var-var cn-form mt-form) . body)
  (let ((cn-var (gentemp))
	(mt-var (gentemp))
	(outputs-var (gentemp)))
    `(let* ((,cn-var ,cn-form)
	    (,mt-var ,mt-form)
	    (,outputs-var (mt-outputs ,mt-var)))
       (loop for ,var-var in (cn-variables ,cn-var)
	   do (when (not (member ,var-var ,outputs-var))
		,@body)))
    ))

(defmacro do-selected-method-input-vars ((var-var cn-form) . body)
  (let ((cn-var (gentemp))
	(mt-var (gentemp)))
    `(let* ((,cn-var ,cn-form)
	    (,mt-var (cn-selected-method ,cn-var)))
       (do-method-input-vars (,var-var ,cn-var ,mt-var) ,@body))
    ))

(defmacro do-consuming-constraints ((constraint-var var-form) . body)
  (let ((var-var (gentemp))
	(var-determined-by-var (gentemp)))
    `(let* ((,var-var ,var-form)
	    (,var-determined-by-var (var-determined-by ,var-var)))
       (loop for ,constraint-var in (var-constraints ,var-form)
	   do (when (and (not (eq ,constraint-var ,var-determined-by-var))
			 (enforced ,constraint-var))
		,@body)))
    ))

;; inefficient fns for consing up lists of input, output vars
;; use methods above for efficiency

(defun method-output-vars (cn mt)
  (declare (ignore cn))
  (mt-outputs mt))

(defun selected-method-output-vars (cn)
  (MT-outputs (cn-selected-method cn)))

(defun method-input-vars (cn mt)
  (loop for var in (cn-variables cn)
      when (not (member var (mt-outputs mt)))
      collect var))

(defun selected-method-input-vars (cn)
  (method-input-vars cn (cn-selected-method cn)))

(defun consuming-constraints (v)
  (loop for cn in (var-constraints v)
        when (and (enforced cn)
                  (not (eql cn (var-determined-by v))))
        collect cn))


;; ***** stack objects *****

(defstruct (sb-stack
	    (:conc-name "SB-STACK-")
	    (:print-function
	     (lambda (stack str lvl)
	       (declare (ignore lvl))
	       (sb-stack-print stack str)))
	    )
  size
  vector
  max-vector-size
  overflow)

(defun sb-stack-create (max-vector-size)
  (make-sb-stack :max-vector-size max-vector-size
		 :vector (make-array (list max-vector-size))
		 :size 0
		 :overflow nil))

(defun sb-stack-clear (stack)
  (setf (sb-stack-size stack) 0)
  stack)

(defun sb-stack-push (stack elt)
  (cond ((< (sb-stack-size stack) (sb-stack-max-vector-size stack))
	 (setf (aref (sb-stack-vector stack) (sb-stack-size stack)) elt))
	(t
	 (push elt (sb-stack-overflow stack))))
  (incf (sb-stack-size stack) 1)
  elt)

(defun sb-stack-pop (stack)
  (let* ((new-size (- (sb-stack-size stack) 1)))
    (setf (sb-stack-size stack) new-size)
    (cond ((< new-size 0)
	   (cerror "return nil" "sb-stack-pop: stack empty")
	   (setf (sb-stack-size stack) 0)
	   nil)
	  ((< new-size (sb-stack-max-vector-size stack))
	   (aref (sb-stack-vector stack) new-size))
	  (t
	   (pop (sb-stack-overflow stack))))
    ))

(defun sb-stack-top (stack)
  (let* ((new-size (- (sb-stack-size stack) 1)))
    (cond ((< new-size 0)
	   (cerror "return nil" "sb-stack-top: stack empty")
	   nil)
	  ((< new-size (sb-stack-max-vector-size stack))
	   (aref (sb-stack-vector stack) new-size))
	  (t
	   (car (sb-stack-overflow stack))))
    ))

(defun sb-stack-empty (stack)
  (zerop (sb-stack-size stack)))

;; loops over all elts in the stack, without removing any.
(defmacro do-sb-stack-elts ((var-var stack-form) . body)
  (let ((stack-var (gentemp))
	(vector-var (gentemp))
	(elt-var (gentemp))
	(overflow-var (gentemp))
	(vector-size-var (gentemp)))
    `(let* ((,stack-var ,stack-form)
	    (,vector-var (sb-stack-vector ,stack-var))
	    (,overflow-var (sb-stack-overflow ,stack-var))
	    (,vector-size-var (sb-stack-max-vector-size ,stack-var)))
       (loop for ,elt-var from (1- (sb-stack-size ,stack-var)) downto 0 do
	     (let ((,var-var (cond ((< ,elt-var ,vector-size-var)
				    (aref ,vector-var ,elt-var))
				   (t (pop ,overflow-var)))))
	       ,@body)))
    ))

(defun sb-stack-member (stack obj)
  (do-sb-stack-elts (elt stack)
    (when (eql elt obj)
      (return-from sb-stack-member t)))
  nil)

(defun sb-stack-print (stack str)
  (format str "{stack(~S):" (sb-stack-size stack))
  (do-sb-stack-elts (elt stack) (format str " ~S" elt))
  (format str "}"))

;; ***** stack of cns sorted by strength *****

(defstruct (sb-cns-set
	    (:conc-name "SB-CNS-SET-")
	    ;; (:print-function
	    ;;  (lambda (stack str lvl)
	    ;;    (declare (ignore lvl))
	    ;;    (sb-cns-set-print stack str)))
	    )
  size
  num-strengths
  cns-stacks)

(defun sb-cns-set-clear (stack)
  (let ((stacks (sb-cns-set-cns-stacks stack)))
    (loop for index from 0 to (1- (sb-cns-set-num-strengths stack))
	do (sb-stack-clear (aref stacks index)))
    (setf (sb-cns-set-size stack) 0)
    stack))

(defun sb-cns-set-add (stack cn)
  (let* ((stacks (sb-cns-set-cns-stacks stack))
	 (cn-stack (aref stacks (CN-strength cn))))
    (unless (sb-stack-member cn-stack cn)
      (sb-stack-push cn-stack cn)
      (incf (sb-cns-set-size stack) 1))
    cn))

(defun sb-cns-set-empty (stack)
  (zerop (sb-cns-set-size stack)))

;; (defun sb-cns-set-print (stack str)
;;   (format str "{cns-set(~S):" (sb-cns-set-size stack))
;;   (let ((stacks (sb-cns-set-cns-stacks stack)))
;;     (loop for index from 0 to (1- (sb-cns-set-num-strengths stack))
;; 	do (let ((cn-stack (aref stacks index)))
;; 	     (when (not (sb-stack-empty cn-stack))
;; 	       (format str " {")
;; 	       (do-sb-stack-elts (elt cn-stack) (format str "~S " elt))
;; 	       (format str "}")))))
;;   (format str "}"))

(defstruct (sb-plan
	    (:conc-name "SB-PLAN-")
	    (:print-function
	     (lambda (plan str lvl)
	       (declare (ignore lvl))
	       (sb-plan-print plan str)))
	    )
  list
  root-cns
  valid
  )

(defun add-constraint (cn)
    (setf (CN-selected-method cn) nil)
    (setf (CN-mark cn) nil)
    (loop for v in (CN-variables cn) do
	  (push cn (VAR-constraints v)))
  cn)
