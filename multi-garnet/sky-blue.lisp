;;;-*- Mode: COMMON-LISP; Package: SKY-BLUE -*-

(in-package "SKY-BLUE")

;; ********************************************************************************
;;                       SkyBlue data structures
;; ********************************************************************************

;; ***** strengths: represented as keywords *****

(defvar *strength-keyword-list*
  (list :>max :max :strong :medium :weak :s1 :s2 :s3 :s4 :s5 :s6 :s7 :min))

;; transfer between strength keywords and numbers.
;; For convenience, can give either form to either fn.

(defun get-strength (strength)
  (cond ((and (integerp strength)
	      (nth strength *strength-keyword-list*))
	 strength)
	((member strength *strength-keyword-list*)
	 (position strength *strength-keyword-list*))
	;; backwards compatibility: support required, wekaest
	((eql strength :required) (get-strength :max))
	((eql strength :weakest) (get-strength :min))
	(t
	 (error "get-strength: bad strength: ~S" strength))))

(defun get-strength-keyword (strength)
  (cond ((and (integerp strength)
	      (nth strength *strength-keyword-list*))
	 (nth strength *strength-keyword-list*))
	((member strength *strength-keyword-list*)
	 strength)
	(t
	 (error "get-strength-keyword: bad strength: ~S" strength))))

(defmacro weaker (s1 s2) `(> ,s1 ,s2))

(defvar *strength-list*
    (loop for key in *strength-keyword-list*
	collect (get-strength key)))


(defvar *max-strength* (get-strength :max))
(defvar *min-strength* (get-strength :min))

;; backwards compatibility: support required, wekaest
(defvar *required-strength* (get-strength :required))
(defvar *weakest-strength* (get-strength :weakest))

;; ***** marks *****

(defvar *mark-counter* 0)

;; new-mark returns a new, unique mark.  new-mark will never return nil,
;; so we can use nil as a mark to "unmark" objects.
(defun new-mark ()
  (incf *mark-counter* 1)
  *mark-counter*)

;; ***** sky-blue object definitions *****

;; constraint representation:
;;   
;;   field         | type      | description
;; ----------------+-----------+--------------------------------------------
;; variables       | set of    | the variables that this constraint references.
;;                 | variables |
;; strength        | strength  | this constraint's level in the constraint
;;                 |           |  hierarchy.
;; methods         | set of    | the potential methods for satisfying this
;;                 | methods   | constraint.
;; selected-method | method    | the method used to satisfy this constraint,
;;                 |           | nil if the constraint is not satisfied.
;;		   |	       | should only be manipulated by skyblue.
;; mark            | integer   | this constraint's mark value.
;; other-slots     | alist     | association list of other slot/value pairs
;; set-slot-fn     | fn(s)     | fn or list of fns to call before setting any slot

(defstruct (sb-Constraint
	    (:print-function
	     (lambda (cn str lvl)
	       (declare (ignore lvl))
	       (cond ((get-sb-slot cn :name)
		      (format str "{cn-~A}" (get-sb-slot cn :name)))
		     ((sb-constraint-strength cn)
		      (format str "{cn~A}"
			      (get-strength-keyword (sb-constraint-strength cn))))
		     (t (format str "{cn}")))))
	    )
  variables
  strength
  methods
  selected-method
  mark
  other-slots
  set-slot-fn
  )

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
  (let ((cn (make-sb-constraint :strength (get-strength strength)
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
			       :walk-strength (get-strength walk-strength)
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
	    (:print-function
	     (lambda (stack str lvl)
	       (declare (ignore lvl))
	       (sb-cns-set-print stack str)))
	    )
  size
  num-strengths
  cns-stacks)

(defun sb-cns-set-create (max-cns)
  (let* ((num-strengths (length *strength-list*))
	 (cns-per-strength (truncate max-cns num-strengths))
	 (strength-cns-stacks (make-array (list num-strengths))))
    (loop for index from 0 to (1- num-strengths) do
	  (setf (aref strength-cns-stacks index)
	    (sb-stack-create cns-per-strength)))
    (make-sb-cns-set :size 0
		     :cns-stacks strength-cns-stacks
		     :num-strengths num-strengths)
    ))

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

(defun sb-cns-set-pop-strongest (stack)
  (incf (sb-cns-set-size stack) -1)
  (cond ((< (sb-cns-set-size stack) 0)
	 (cerror "return nil" "sb-cns-set-pop: stack empty")
	 (sb-cns-set-clear stack)
	 nil)
	(t
	 (let ((stacks (sb-cns-set-cns-stacks stack)))
	   (loop for index from 0 to (1- (sb-cns-set-num-strengths stack))
	       do (let ((cn-stack (aref stacks index)))
		    (when (not (sb-stack-empty cn-stack))
		      (return (sb-stack-pop cn-stack))))
	       finally (cerror "return nil" "sb-cns-set-pop: all substacks empty")))
	 )))

(defun sb-cns-set-empty (stack)
  (zerop (sb-cns-set-size stack)))

(defun sb-cns-set-print (stack str)
  (format str "{cns-set(~S):" (sb-cns-set-size stack))
  (let ((stacks (sb-cns-set-cns-stacks stack)))
    (loop for index from 0 to (1- (sb-cns-set-num-strengths stack))
	do (let ((cn-stack (aref stacks index)))
	     (when (not (sb-stack-empty cn-stack))
	       (format str " {")
	       (do-sb-stack-elts (elt cn-stack) (format str "~S " elt))
	       (format str "}")))))
  (format str "}"))


;; loops over all elts in the set, from strongest-to-weakest,
;; without removing any.
(defmacro do-sb-cns-set-elts ((var-var set-form) . body)
  (let ((set-var (gentemp))
	(stacks-var (gentemp))
	(index-var (gentemp))
	(cn-stack-var (gentemp)))
    `(let* ((,set-var ,set-form)
	    (,stacks-var (sb-cns-set-cns-stacks ,set-var)))
       (loop for ,index-var from 0 to (1- (sb-cns-set-num-strengths ,set-var))
	   do (let ((,cn-stack-var (aref ,stacks-var ,index-var)))
		(when (not (sb-stack-empty ,cn-stack-var))
		  (do-sb-stack-elts (,var-var ,cn-stack-var) ,@body)))))
    ))

;; ***** plans *****

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

(defun sb-plan-print (plan str)
  (format str "{~Aplan:~Sroots,~Slist}"
	  (if (sb-plan-valid plan) "valid-" "invalid-")
	  (length (sb-plan-root-cns plan))
	  (length (sb-plan-list plan))
	  ))

;; ***** interlock to prevent accidental recursive calls to skyblue *****
;; add-constraint and remove-constraint should never be called recursively
;; (for example: from within a constraint method).  This signals a
;; continuable error if this is done.

(defvar *sky-blue-running* nil)

(defmacro with-sky-blue-recursion-check (&rest forms)
  `(cond (*sky-blue-running*
	  (cerror "add-constraint or remove-constraint ignored"
		  "sky-blue add-constraint or remove-constraint called recursively!")
	  )
	 (t
	  (unwind-protect
	      (progn
		(setq *sky-blue-running* t)
		(progn ,@forms))
	    (setq *sky-blue-running* nil))
	  )))

;; ********************************************************************************
;;                       SkyBlue algorithm
;; ********************************************************************************

;; Sky-Blue global stacks: rather than passing these stacks between the
;; different functions, these are saved as global variables.

;; stack of unenforced cns that we want to try enforcing, sorted by
;; strength from strongest to weakest
(defvar *unenforced-cns-stack* (sb-cns-set-create 200))

;; stack of newly-undetermined variables that we will trace downstream from to
;; find unenforced cns.
(defvar *undetermined-vars-stack* (sb-stack-create 100))

;; Stack of vars and cns.  To satisfy the enforced cns in the current
;; method graph, we will trace downstream from these vars and cns, evaling
;; methods.  Actually, things are a little more complicated: the functions
;; below should be used to add and remove things from this stack.
(defvar *exec-roots-stack* (sb-stack-create 100))

;; We only want to exec downstream of undetermined variables that are
;; invalid.
(defun exec-roots-add-undet-var (var)
  (when (not (VAR-valid var))
    (sb-stack-push *exec-roots-stack* var)
    ))

;; When a constraint is added to the stack, its original selected method is
;; also added, so that we can tell whether the constraint method has been
;; switched back (if the current selected method is the same as the
;; original method, then the constraint method doesn't need to be
;; executed).  When a constriant is added to this stack, its :in-exec-roots
;; property is also set, so that any given constraint is only added once to
;; this stack.
(defun exec-roots-add-cn (cn old-mt)
  (unless (get-sb-slot cn :in-exec-roots)
    (sb-stack-push *exec-roots-stack* old-mt)
    (sb-stack-push *exec-roots-stack* cn)
    (set-sb-slot cn :in-exec-roots t)
    ))

(defun exec-roots-empty ()
  (sb-stack-empty *exec-roots-stack*))

;; pop var or (cn and saved-mt), returning values (1) :var or :cn, (2) the
;; cn or var, (3) the save-mt, if there is one.
(defun exec-roots-pop ()
  (let* ((obj (sb-stack-top *exec-roots-stack*)))
    (typecase obj
      (sb-constraint
       ;; make sure that cn :in-exec-roots is cleared *before*
       ;; object is popped, in case interrupt occurs
       (set-sb-slot obj :in-exec-roots nil)
       (sb-stack-pop *exec-roots-stack*)
       ;; return cn and mt
       (values :cn obj (sb-stack-pop *exec-roots-stack*)))
      (sb-variable
       (sb-stack-pop *exec-roots-stack*)
       (values :var obj)))
    ))

(defun init-stacks ()
  (do-sb-stack-elts (cn *exec-roots-stack*)
    (when (sb-constraint-p cn)
      (set-sb-slot cn :in-exec-roots nil)
      ))
  (sb-stack-clear *exec-roots-stack*)
  (sb-stack-clear *undetermined-vars-stack*)
  (sb-cns-set-clear *unenforced-cns-stack*)
  )

;; ***** Sky-Blue Entry Points *****

(defun add-constraint (cn)
  (with-sky-blue-recursion-check
      ;; clear stacks
      (init-stacks)
    ;; initialize constraint fields, and register with variables
    (setf (CN-selected-method cn) nil)
    (setf (CN-mark cn) nil)
    (loop for v in (CN-variables cn) do
	  (push cn (VAR-constraints v)))
    ;; find the method graph to enforce the new cn (if possible),
    ;; collecting any exec-roots.
    (sb-cns-set-add *unenforced-cns-stack* cn)
    (update-method-graph)
    ;; enforce method graph cns by evaluating cn methods.
    (exec-from-roots)
    )
  cn)

(defun remove-constraint (cn)
  (with-sky-blue-recursion-check
      ;; unregister cn from variables
      (loop for v in (CN-variables cn) do
	    (setf (VAR-constraints v)
              (remove cn (VAR-constraints v))))
    (when (enforced cn)
      ;; clear stacks
      (init-stacks)
      ;; loop over current outputs of cn
      (do-selected-method-output-vars (var cn)
	;; un-determine current method's output vars
	(setf (VAR-determined-by var) nil)
	(sb-stack-push *undetermined-vars-stack* var)
	(exec-roots-add-undet-var var)
	)
      ;; un-enforce cn
      (setf (CN-selected-method cn) nil)
      ;; propagate walkstrength from newly undetermined vars
      (propagate-walkstrength *undetermined-vars-stack* nil)
      ;; collect all unenforced cns downstream of undetermined vars
      ;; that have the same or weaker strength than the removed cn
      (collect-unenforced *undetermined-vars-stack* nil (CN-strength cn) t)
      ;; find method graph to enforce the collected unenforced cns
      (update-method-graph)
      ;; enforce method graph cns by evaluating cn methods.
      (exec-from-roots)
      )
    )
    cn)

;; Changes the strength of a cn that is already within the graph, modifying
;; the graph and propagating walkabout strengths as needed.  If a
;; constraint is not in the graph, its strength can simply be changed by
;; calling (setf (CN-strength) cn), without calling this function.

(defun change-constraint-strength (cn strength)
  (with-sky-blue-recursion-check
      (let* ((new-strength (get-strength strength))
	     (old-strength (CN-strength cn))
	     (old-enforced (enforced cn)))
	;; change strength
	(setf (CN-strength cn) new-strength)
	;; clear stacks
	(init-stacks)
	;; propagate walkstrengths, and collect any unenforced cns
	;; that may now be enforcable
	(cond ((weaker new-strength old-strength)
	       ;; making the cn weaker
	       (cond (old-enforced
		      ;; weakening an enforced cn: it may be revoked
		      ;; propagate walkstrength downstream from cn
		      ;; with new strength
		      (propagate-walkstrength nil cn)
		      ;; collect all unenforced cns downstream of cn
		      ;; with the same or weaker strength than old-strength
		      (collect-unenforced nil cn old-strength t)
		      )
		     (t
		      ;; weakening an unenforced cn: simply change strength
		      nil))
	       )
	      ((weaker old-strength new-strength)
	       ;; strengthening the cn
	       (cond (old-enforced
		      ;; strengthening an enforced cn: propagate walkstrengths
		      (propagate-walkstrength nil cn)
		      )
		     (t
		      ;; strengthening an unenforced cn: it may now be enforcable
		      (sb-cns-set-add *unenforced-cns-stack* cn)
		      ))
	       )
	      (t
	       ;; strength the same: noop.
	       ))
	;; construct LGB mgraph
	(update-method-graph)
	;; enforce method graph cns by evaluating cn methods.
	(exec-from-roots)
	new-strength)))

;; this fn takes a list of cns in the graph, and executes their selected
;; methods, and any downstream methods.  This can be used to cause input
;; constraints to be executed.
(defun execute-constraints (cns)
  (with-sky-blue-recursion-check
      nil
    ;; push enforced constraints on exec-roots stack
    (init-stacks)
    (loop for cn in cns
	when (and (sb-constraint-p cn)
		  (enforced cn))
	do (exec-roots-add-cn cn (CN-selected-method cn)))
    ;; enforce method graph cns by evaluating cn methods.
    (exec-from-roots :execute-unchanged-cns t)
    )
  nil)


;; ***** update-method-graph *****

;; update-method-graph modifies the current method graph to include the
;; constraints in *unenforced-cns-stack*, maintaining it as a
;; locally-graph-better method graph.  Each time a cn is enforced, other
;; unenforced cns that might be enforcable may be found, so these are added
;; to the stack.  As a heuristic, the stack is kept in order of cn
;; strength, and we always try enforcing the strongest unenforced cn first.
;; This fn also updates *exec-roots-stack* by adding any cns were
;; sucessfully enforced, as well as any newly-undetermined vars that should
;; now be validated.  *undetermined-vars-stack* is a stack that is used for
;; temporary storage of a list of undetermined variables.
(defun update-method-graph ()
  (let* (ok cn)
    (loop until (sb-cns-set-empty *unenforced-cns-stack*) do
	  (setq cn (sb-cns-set-pop-strongest *unenforced-cns-stack*))
	  ;; try building mvine, collecting newly-undetermined vars
	  (sb-stack-clear *undetermined-vars-stack*)
	  (setq ok (build-mvine cn))
	  (when ok
	    ;; we found an mvine!
	    ;; prop walkstrengths down mvine, and from newly undetermined vars
	    (propagate-walkstrength *undetermined-vars-stack* cn)
	    ;; collect any cns strictly weaker than cn that may now be enforcible
	    (collect-unenforced *undetermined-vars-stack* cn (CN-strength cn) nil)
	    ))
    ))


;; ***** building method vines *****

;; build-mvine is one of a set of mutually-recursive fns that perform a
;; backtracking search for a mutually-consistant method assignments for the
;; specified constraint.  The "mvine" that is being built is a method vine
;; rooted at the cn originially used to start this process, which had
;; strength root-strength.  The leaves of the mvine are cns weaker than
;; this strength, that are left unenforced (with no method selected).  It
;; is also possible that a leaf may loop back to a lower part of the mvine:
;; this is acceptable.  These functions perform the search by marking cns
;; and vars with the specified mark: only when a complete mvine is found
;; are the selected methods changed to reflect this.  These functions all
;; return t iff a way was found to enforce the cn, nil otherwise.  All of
;; the functions add newly-undetermined vars to the stack
;; *undetermined-vars-stack*.

(defvar *mvine-cns-stack* (sb-stack-create 30))

(defun build-mvine (cn)
  (sb-stack-clear *mvine-cns-stack*)
  ;; try to build mvine starting with enforcing root cn as a branch
  (mvine-enforce-cn cn (cn-strength cn) (new-mark)))

(defun mvine-grow (root-strength done-mark)
  (if (sb-stack-empty *mvine-cns-stack*)
      ;; no more cns, we have found a complete prop path!
      t
    ;; process next cn
    (let* (cn ok)
      (setq cn (sb-stack-pop *mvine-cns-stack*))
      (cond ((eql done-mark (CN-mark cn))
	     ;; this cn has already been marked: process other cns
	     (setq ok (mvine-grow root-strength done-mark)))
	    ((weaker (CN-strength cn) root-strength)
	     ;; this cn is weaker than the root cn: revoke it
	     (setq ok (mvine-revoke-cn cn root-strength done-mark)))
	    (t
	     ;; try to find a method for this cn
	     (setq ok (mvine-enforce-cn cn root-strength done-mark)))
	    )
      ;; if we are backtracking, must restore *mvine-cns-stack*
      (when (not ok)
	(sb-stack-push *mvine-cns-stack* cn))
      ok)
    ))

(defun mvine-revoke-cn (cn root-strength done-mark)
  (let* ((old-mt (CN-selected-method cn))
	 ok)
    ;; mark this cn.  we will process it by revoking it.
    (setf (CN-mark cn) done-mark)
    ;; try building rest of mvine
    (setq ok (mvine-grow root-strength done-mark))
    (cond (ok
	   ;; we found entire mvine!
	   ;; undetermine unmarked old-mt outputs,
	   ;; and save on *undetermined-vars-stack*
	   (loop for var in (MT-outputs old-mt)
	       when (not (eql done-mark (VAR-mark var))) do
		 ;; unmarked vars must be newly undetermined.
		 (setf (VAR-determined-by var) nil)
		 (sb-stack-push *undetermined-vars-stack* var)
		 (exec-roots-add-undet-var var)
		 )
	   ;; set selected-method for this cn
	   (exec-roots-add-cn cn (CN-selected-method cn))
	   (setf (CN-selected-method cn) nil)
	   t)
	  (t
	   ;; no mvine found: we are backtracking.
	   ;; there is no other choice for this cn, so just un-mark cn
	   ;; and continue backtracking.
	   (setf (CN-mark cn) nil)
	   nil))
    ))

(defun mvine-enforce-cn (cn root-strength done-mark)
  (let* ((old-mt (CN-selected-method cn))
	 ;; note: old-mt=nil if cn is the unenforced mvine root
	 (old-outputs (if old-mt (MT-outputs old-mt) nil))
	 ok)
    ;; mark this constraint: we will try to make it into a branch
    (setf (CN-mark cn) done-mark)
    ;; try each possible method: returning if one is found that allows
    ;; mvine to be built
    (loop for mt in (CN-methods cn)
	when (possible-method mt root-strength done-mark old-outputs)
	do
	  (let* ((next-cns-cnt 0))
	    ;; add constraints determining this mt's outputs onto stack
	    ;; (and count them so we know how many to pop during backtracking)
	    (loop for var in (MT-outputs mt) do
		  (let ((next-cn (VAR-determined-by var)))
		    (when (and next-cn (not (eql cn next-cn)))
		      (sb-stack-push *mvine-cns-stack* next-cn)
		      (setq next-cns-cnt (+ next-cns-cnt 1)))))
	    ;; let's try to build the mvine with this cn/mt.
	    ;; mark the output vars of the method
	    (loop for var in (MT-outputs mt) do
		  (setf (VAR-mark var) done-mark))
	    ;; try building rest of mvine
	    (setq ok (mvine-grow root-strength done-mark))
	    (cond (ok
		   ;; we found entire mvine!
		   ;; undetermine unmarked output vars of old mt,
		   ;; and save on *undetermined-vars-stack*
		   (loop for var in old-outputs
		       when (not (eql done-mark (VAR-mark var))) do
			 ;; unmarked vars must be newly undetermined.
			 ;; reset determined-by of newly undetermined vars
			 (setf (VAR-determined-by var) nil)
			 (sb-stack-push *undetermined-vars-stack* var)
			 (exec-roots-add-undet-var var)
			 )
		   ;; set selected method for this cn, and ptrs in new outputs
		   (exec-roots-add-cn cn old-mt)
		   (setf (CN-selected-method cn) mt)
		   (loop for var in (MT-outputs mt) do
			 (setf (VAR-determined-by var) cn)
			 )
		   ;; return t without trying any more methods
		   (return-from mvine-enforce-cn t))
		  (t
		   ;; no mvine found: try next method
		   ;; "undo" current method choice: unmark method outputs
		   (loop for var in (MT-outputs mt) do
			 (setf (VAR-mark var) nil))
		   ;; pop constraints we added above
		   (loop for cnt from 1 to next-cns-cnt do
			 (sb-stack-pop *mvine-cns-stack*))
		   ))
	    ;; unless we found a soln: continue loop to try next method
	    ))
    ;; loop finished: no more methods to try: unmark cn and backtrack
    (setf (CN-mark cn) nil)
    ;; signal backtracking point (unless this is root cn)
    (unless (null old-mt)
      (signal-backtracking cn))
    nil))


(defvar *sky-blue-backtracking-warning* nil)

;; this is called to print a warning message when the mvine-enforce-cn fn
;; backtracks because cn cannot be extended into a complete mvine.
;; This message can be prevented by setting *sky-blue-backtracking-warning*
;; to nil.
(defun signal-backtracking (cn)
  (when *sky-blue-backtracking-warning*
	(format t "~&Sky-blue: backtracking at ~S~%" cn)))

;; a method is only possible if both: (1) all its outputs are unmarked
;; (i.e. they aren't being used yet in the mvine) (2) every output
;; walkstrength is weaker than the cn's strength (except for vars that are
;; current outputs)
(defun possible-method (mt root-strength mark current-outputs)
  (loop for var in (MT-outputs mt) never
	(or
	 ;; if an output var is marked, we can't use this method
	 (eql mark (VAR-mark var))
	 ;; if an output var is too strong, and is not currently
	 ;; determined by the cn, we can't use this method
	 (and (not (weaker (VAR-walk-strength var) root-strength))
	      (not (member var current-outputs)))
	 )
	))

;; ***** propagating walkabout strengths *****

;; propagate walkstrengths downstream from the variables in undet-var-stack
;; and the constraint root-cn.  Cycles are broken by inserting the most
;; conservative walkstrength (:min).

(defvar *propagate-walkstrength-stack* (sb-stack-create 100))

(defun propagate-walkstrength (undet-var-stack root-cn)
  (let* ((prop-mark (new-mark))
	 cn)
    ;; mark all cns we will be processing, and collect ordered pplan
    (sb-stack-clear *propagate-walkstrength-stack*)
    ;; make sure undetermined vars have walkstrength of :min,
    ;; and propagate walkstrength below them
    (when undet-var-stack
      (do-sb-stack-elts (var undet-var-stack)
	(setf (VAR-walk-strength var) *min-strength*)
	(pplan-add *propagate-walkstrength-stack* var prop-mark)
	))
    ;; propagate below root-cn, if any
    (when root-cn
      (pplan-add *propagate-walkstrength-stack* root-cn prop-mark)
      )
    ;; scan through pplan
    (loop until (sb-stack-empty *propagate-walkstrength-stack*) do
	  (setq cn (sb-stack-pop *propagate-walkstrength-stack*))
	  (when (eql (CN-mark cn) prop-mark)
	    (cond ((any-immediate-upstream-cns-marked cn prop-mark)
		   ;; Some of this cn's upstream cns have not been processed: there
		   ;; must be a cycle.  Handle it, possibly unmarking other cns in
		   ;; the cycle, and calculating their walkstrengths.
		   (propagate-walkstrength-cycle cn prop-mark)
		   )
		  (t
		   ;; cn is not in a cycle: compute walkstrengths and mark it done
		   (do-selected-method-output-vars (var cn)
		     (setf (VAR-walk-strength var) (compute-walkabout cn var)))
		   (setf (CN-mark cn) nil)
		   ))
	    ))
    ))

;; cn is in a cycle: break the cycle by setting any upstream vars
;; determined by the unprocessed cns to have :min walkabout strength (the
;; most conservative choice).
(defun propagate-walkstrength-cycle (cn prop-mark)
  (do-selected-method-input-vars (var cn)
    (let ((upstream-cn (VAR-determined-by var)))
      (when (and upstream-cn
		 (eql prop-mark (CN-mark upstream-cn)))
	(setf (VAR-walk-strength var) *min-strength*))))
  ;; compute walkstrengths for cn, and mark it done
  (do-selected-method-output-vars (var cn)
    (setf (VAR-walk-strength var) (compute-walkabout cn var)))
  (setf (CN-mark cn) nil)
  )

;; any-immediate-upstream-cns-unmarked returns t iff none of the cns
;; determining the inputs of cn are marked with the given mark
(defun any-immediate-upstream-cns-marked (cn mark)
  (do-selected-method-input-vars (var cn)
    (let ((upstream-cn (VAR-determined-by var)))
      (when (and upstream-cn
		 (eql mark (CN-mark upstream-cn)))
	(return-from any-immediate-upstream-cns-marked t))
      ))
  ;; none of the cns are marked: return nil
  nil)


;; compute-walkabout calculates the walkabout strength of the variable var
;; which is currently a selected output variable of the constraint cn.
;; This value is a lower bound on the strength a cn would need to have to
;; set this variable (causing this cn to change its selected mt).  Note:
;; different output vars may have different walkstrengths, since the cn may
;; have different sets of possible mts that don't set each var.  Note: have
;; to handle the case where some method's output vars is a subset of other
;; method's output vars.  Normally, one wouldn't define such a cn, but it
;; is possible to get this in Multi-Garnet with indirect var paths.
(defun compute-walkabout (cn var)
  (let* ((min-strength (CN-strength cn))
         (selected-method (CN-selected-method cn))
         (selected-out-vars (MT-outputs selected-method)))
    (loop for mt in (CN-methods cn)
          unless (eql mt selected-method)
          do
          (let ((out-vars (MT-outputs mt))
                (max-strength *min-strength*))
            (when (not (member var out-vars))
              ;; mt doesn't output to var, so this is a possible alternative
              ;; mt for this cn.  Find max output var walkstrength for this
              ;; mt, ignoring vars set by currently selected mt
              (loop for out-var in out-vars
		  when (and (weaker max-strength (VAR-walk-strength out-var))
			    (not (member out-var selected-out-vars)))
		  do (setf max-strength (VAR-walk-strength out-var)))
              ;; note: final max-strength will be :min if there is a mt
              ;; with outputs that are a subset of the selected method
              (when (weaker max-strength min-strength)
                (setf min-strength max-strength)))
            ))
    min-strength))

;; ***** collect unenforced constraints *****

;; collect-unenforced traces downstream from all of the vars in
;; undet-var-stack and the constraint root-cn, adding to
;; *unenforced-cns-stack* any unenforced cns that could possibly output to
;; these vars or downstream vars.  If collect-equal-p is nil, this only
;; collects cns strictly weaker than collection-strength.  If
;; collect-equal-p is non-nil, this collects cns weaker than or with the
;; same strength as collection-strength.
(defun collect-unenforced (undet-var-stack root-cn
			   collection-strength collect-equal-p)
  (let ((done-mark (new-mark)))
    (when undet-var-stack
      (do-sb-stack-elts (var undet-var-stack)
	(collect-unenforced-mark var collection-strength collect-equal-p done-mark)))
    (when (and root-cn (enforced root-cn))
      (do-selected-method-output-vars (var root-cn)
	(collect-unenforced-mark var collection-strength collect-equal-p done-mark)))
    ))

(defun collect-unenforced-mark (var collection-strength collect-equal-p done-mark)
  (loop for cn in (VAR-constraints var)
      unless (or
	      ;; if cn determines var, ignore
	      (eql cn (VAR-determined-by var))
	      ;; if we've already processed this cn, ignore it
	      (eql done-mark (CN-mark cn))
	      )
      do
	;; we will process this cn now: mark it
	(setf (CN-mark cn) done-mark)
	(cond ((enforced cn)
	       ;; cn is an enforced cn that consumes var:
	       ;; collect cns downstream of cn's outputs
	       (do-selected-method-output-vars (out-var cn)
		 (collect-unenforced-mark out-var collection-strength
					  collect-equal-p done-mark))
	       )
	      ((or (weaker (CN-strength cn) collection-strength)
		   (and collect-equal-p (eql (CN-strength cn) collection-strength)))
	       ;; cn is an unenforced cn that is weak enough to collect
	       (sb-cns-set-add *unenforced-cns-stack* cn)
	       ))
	))

;; ***** executing methods *****

(defvar *exec-pplan-stack* (sb-stack-create 100))

;; this fn executes methods downstream from the "exec-root" vars and cns on
;; *exec-roots-stack* to enforce the cns in the method graph.  A constraint
;; is used as an "exec-root" iff its method has changed, compared to the
;; original selected method also stored in *exec-roots-stack*.  A variable
;; is used as an "exec-root" if it is undetermined, and is not valid (in
;; which case it is marked as valid).
(defun exec-from-roots (&key (execute-unchanged-cns nil))
  (let* ((prop-mark (new-mark))
	 cn)
    ;; examine all vars and cns in *exec-roots-stack*,
    ;; and add to pplan
    (sb-stack-clear *exec-pplan-stack*)
    (loop until (exec-roots-empty) do
	  (multiple-value-bind (var-cn-keyword var-or-cn old-mt)
	      (exec-roots-pop)
	    (case var-cn-keyword
	      (:cn
	       (when (or execute-unchanged-cns
			 (not (eql old-mt (CN-selected-method var-or-cn))))
		 (pplan-add *exec-pplan-stack* var-or-cn prop-mark))
	       )
	      (:var
	       (when (and (not (VAR-determined-by var-or-cn))
			  (not (VAR-valid var-or-cn)))
		 (pplan-add *exec-pplan-stack* var-or-cn prop-mark)
		 (setf (VAR-valid var-or-cn) t)
		 )
	       ))
	    ))
    ;; scan through pplan
    (loop until (sb-stack-empty *exec-pplan-stack*) do
	  (setq cn (sb-stack-pop *exec-pplan-stack*))
	  (cond ((not (eql prop-mark (CN-mark cn)))
		 ;; this cn has already been processed: ignore
		 nil)
		((any-immediate-upstream-cns-marked cn prop-mark)
		 ;; Some of this cn's upstream cns have not been processed:
		 ;; there must be a cycle.  Call fn to handle cycle: this
		 ;; fn may mark other cns as done.
		 (exec-from-cycle cn prop-mark)
		 )
		(t
		 ;; All of this cn's upstream cns have been processed, so we
		 ;; can now process this one and mark it done
		 (execute-propagate-valid cn)
		 (setf (CN-mark cn) nil)
		 )
		))
    ))

;; executes the selected method of cn if all of its inputs are valid.  In
;; any case, it propagate the valid flag to the outputs.
(defun execute-propagate-valid (cn)
  (let ((inputs-valid (block valid-block
			(do-selected-method-input-vars (var cn)
			  (when (not (VAR-valid var))
			    (return-from valid-block nil)))
			t)))
    (when inputs-valid
      (execute-selected-method cn))
    (do-selected-method-output-vars (var cn)
      (setf (VAR-valid var) inputs-valid))
    ))

(defun execute-selected-method (cn)
  (funcall (MT-code (CN-selected-method cn)) cn))

(defvar *sky-blue-cycle-solver-fns* nil)

;; handle cycle by trying a series of cycle solvers.  If one of the solvers
;; can find a solution, install it in the cycle variables.  If none of the
;; solvers can find a solution (or one of the solvers returns :no-soln),
;; invalidate the output vars of all cns in the cycle (and downstream of
;; it).
(defun exec-from-cycle (cn prop-mark)
  (let* ((cycle-cns (collect-cns-in-cycle (list cn) nil prop-mark))
	 cycle-solvers-found-soln)
    ;; if any of the cycle input vars (input vars to any of
    ;; the cns in the cycle that are not set by other cycle cns)
    ;; are invalid, return without trying to solve the cycle.
    (loop for cn in cycle-cns do
	  (do-selected-method-input-vars (var cn)
	    (when (and (not (VAR-valid var))
		       (not (member (VAR-determined-by var) cycle-cns)))
	      ;; mark cn and downstream cns as done, and all outputs invalid
	      (unmark-invalidate-downstream cn prop-mark)
	      ;; and return without trying to solve cycle
	      (return-from exec-from-cycle nil))
	    ))
    ;; print warning
    (signal-cycle cycle-cns)
    ;; try cycle solvers
    (setq cycle-solvers-found-soln (call-cycle-solvers cycle-cns))
    (cond (cycle-solvers-found-soln
	   ;; some solver has succeeded.  Clear marks in cycle cns, so we
	   ;; won't process them, and set var-valid for cn outputs
	   (loop for cn in cycle-cns do
		 (setf (CN-mark cn) nil)
		 (do-selected-method-output-vars (var cn)
		   (setf (VAR-valid var) t)))
	   )
	  (t
	   ;; None of the cycle solvers were sucessful.  Mark cn and
	   ;; downstream cns as done, and all outputs invalid
	   (unmark-invalidate-downstream cn prop-mark)
	   ))
    ))

;; Call each cycle solver to try solving the cycle-cns, returning t if a
;; soln is found. If a solver can solve the cycle, it must set all of the
;; var values of vars set by cycle cns, and return t.  If it cannot solve
;; the cycle, it must not change anything, and return nil.  If it finds
;; that the cycle has no solution, it must not change anything, and return
;; :no-soln.
(defun call-cycle-solvers (cycle-cns)
  (loop for solver-fn in *sky-blue-cycle-solver-fns* do
	(let* ((solver-result (funcall solver-fn cycle-cns)))
	  (cond ((eql solver-result :no-soln)
		 ;; There is no solution to this cycle.
		 ;; Return nil without trying other cycle solvers.
		 (return nil))
		((eql solver-result t)
		 ;; This solver fn has succeeded.
		 ;; Return t without trying other cycle solvers.
		 (return t))))
      finally (return nil)))

;; return a list of all cns upstream of the cns in roots that have not been
;; processed (i.e., cn-mark is not equal to prop-mark).  When called with
;; the cycle cn found in exec-from-cycle, this returns all of the cns in
;; the cycle.
(defun collect-cns-in-cycle (roots collected prop-mark)
  (let* ((cn (car roots))
	 (next-roots (cdr roots)))
    (cond ((null roots) collected)
	  ((not (equal prop-mark (CN-mark cn)))
	   (collect-cns-in-cycle next-roots collected prop-mark))
	  ((member cn collected)
	   (collect-cns-in-cycle next-roots collected prop-mark))
	  (t
	   (do-selected-method-input-vars (var cn)
	     (when (VAR-determined-by var)
	       (push (VAR-determined-by var) next-roots)))
	   (collect-cns-in-cycle
	    next-roots (cons cn collected) prop-mark)
	   ))
    ))

(defvar *sky-blue-cycle-warning* nil)

(defun signal-cycle (cns)
  (when *sky-blue-cycle-warning*
    (let* ((*print-length* 5))
#-(and)      (declare (special *print-length*))
      (format t "~&Sky-blue cycle: ~S~%" cns))
    ))

;; linear-eqn-cycle-solver is an example of how one might write a cycle
;; solver to be added to the list *sky-blue-cycle-solver-fns*.  Note that
;; this is an incomplete example: the functions extract-cn-linear-eqn,
;; solve-linear-eqns, linear-eqn-soln-val, and linear-eqns-have-no-soln are
;; not defined.  In order to complete this example, you would need to
;; associate additional information with the constraints allowing
;; extract-cn-linear-eqn to extract the appropriate linear equation from a
;; constraint.  Note that the equation is extracted relative to a given set
;; of variables in a cycle: other variables in the constraint may be set
;; external to the cycle.  This would allow non-linear constraints to be
;; solved, if enough variables are determined to reduce them to linear
;; constraints.  For example, given the selected methods {5->A, A*B->C,
;; C+4->B}, the loop containing the second and third methods is a cycle of
;; linear equations, since A is determined outside of the cycle, reducing
;; the second method to 5*B->C.

;;; Define stubs to silence compiler.
(defun extract-cn-linear-eqn (cn cycle-vars)
  (declare (ignore cn cycle-vars))
  nil)
(defun solve-linear-eqns (eqns)
  (declare (ignore eqns))
  nil)
(defun linear-eqn-soln-val (soln var)
  (declare (ignore soln var))
  nil)
(defun linear-eqns-have-no-soln (eqns)
  (declare (ignore eqns))
  nil)

(defun linear-eqn-cycle-solver (cns)
  (let* ((cycle-vars (loop for cn in cns
			 append (selected-method-output-vars cn)))
	 (eqns (loop for cn in cns collect
		     (let* ((eqn (extract-cn-linear-eqn cn cycle-vars)))
		       ;; if we can't extract a linear equation from one of
		       ;; the cns, we can't solve the cycle with this cycle
		       ;; solver: return nil immediately
		       (when (null eqn)
			 (return-from linear-eqn-cycle-solver nil))
		       eqn)))
	 (soln (solve-linear-eqns eqns))
	 )
    (cond (soln
	   ;; we have a soln: install calculated values in vars and return t
	   (loop for var in cycle-vars do
		 (setf (VAR-value var)
		   (linear-eqn-soln-val soln var)))
	   t)
	  ((linear-eqns-have-no-soln eqns)
	   ;; no possible soln
	   :no-soln)
	  (t
	   ;; no solution found: return nil
	   nil))
    ))

;; takes a list of enforced cns, and unmarks all downstream enforced cns
;; that are marked with prop-mark (stopping the recursion when unmarked cns
;; are found).  All vars that are outputs of these cns are marked invalid.
(defun unmark-invalidate-downstream (cn prop-mark)
  (when (eql prop-mark (CN-mark cn))
    (setf (CN-mark cn) nil)
    (do-selected-method-output-vars (var cn)
      (setf (VAR-valid var) nil)
      (do-consuming-constraints (downstream-cn var)
	(unmark-invalidate-downstream downstream-cn prop-mark))
      )))

;; ***** constructing propagation plans *****

;; given a cn, pplan-add recursively travels downstream of the cn, marking
;; all cns with the given mark, and adds all marked cns to the specified
;; stack.  When this is done, the stack contains, in order, the cns that
;; will have to be examined to propagate walkstrengths or var values.  Each
;; cn will only appear at most once on the stack (since they are marked
;; when they are added).  Note that it will be necessary to check for
;; cycles while examining the cns.  Note: further cns can be added as roots
;; of the propagation, simply by calling pplan-add again.  pplan will also
;; take vars, or lists and stacks of cns and vars.

(defun pplan-add (stack obj done-mark)
  (cond ((sb-constraint-p obj)
	 (when (and (enforced obj)
		    (not (eql done-mark (CN-mark obj))))
	   ;; process unmarked, enforced constraint by marking it, collecting
	   ;; downstream constraints, and pushing it on top of the pplan stack.
	   (setf (CN-mark obj) done-mark)
	   (do-selected-method-output-vars (out-var obj)
	     (pplan-add stack out-var done-mark))
	   (sb-stack-push stack obj))
	 )
	((sb-variable-p obj)
	 ;; process variable by collecting downstream constraints rooted with
	 ;; constraints directly consuming the variable
	 (do-consuming-constraints (cn obj)
	   (pplan-add stack cn done-mark))
	 )
	;; accept list of cns and vars
	((null obj)
	 nil)
	((listp obj)
	 (pplan-add stack (car obj) done-mark)
	 (pplan-add stack (cdr obj) done-mark))
	;; also accept stack of cns and vars
	((sb-stack-p obj)
	 (do-sb-stack-elts (elt obj)
	   (pplan-add stack elt done-mark)))
	(t
	 (cerror "cont" "pplan-add: bad object ~S" obj))
	))

;; ***** extracting and executing plans *****

(defvar *extract-plan-stack* (sb-stack-create 100))

(defun extract-plan (root-cns)
  (let ((prop-mark (new-mark))
	(plan-list nil)
	cn)
    ;; mark all cns we will be processing, and collect ordered pplan
    (sb-stack-clear *extract-plan-stack*)
    (pplan-add *extract-plan-stack* root-cns prop-mark)
    ;; scan through pplan
    (loop until (sb-stack-empty *extract-plan-stack*) do
	  (setq cn (sb-stack-pop *extract-plan-stack*))
	  (cond ((not (eql prop-mark (CN-mark cn)))
		 ;; this cn has already been processed: ignore
		 nil)
		((any-immediate-upstream-cns-marked cn prop-mark) 
		 ;; Some of this cn's upstream cns have not been processed:
		 ;; there must be a cycle.  Add cycle to plan, unmarking
		 ;; cycle cns.
		 (push (extract-plan-cycle cn prop-mark) plan-list)
		 )
		(t
		 ;; all the upstream cns have been processed, so unmark this one
		 ;; and add it to the beginning of the plan
		 ;; (will reverse plan before returning)
		 (setf (CN-mark cn) nil)
		 (push cn plan-list)
		 )))
    (create-valid-plan root-cns (nreverse plan-list))
    ))

(defun extract-plan-cycle (cn prop-mark)
  (let* ((cycle-cns (collect-cns-in-cycle (list cn) nil prop-mark))
	 (cycle-outputs (loop for cn in cycle-cns append
			      (selected-method-output-vars cn)))
	 (cycle-inputs
	  (remove-duplicates
	   (loop for cn in cycle-cns append
		 (loop for var in (cn-variables cn)
		     unless (member var cycle-outputs)
		     collect var))))
	 )
    (loop for cn in cycle-cns do (setf (CN-mark cn) nil))	  
    (list cycle-cns cycle-inputs cycle-outputs)))

;; execute the cns and cycles in (sb-plan-list plan).
(defun execute-plan (plan)
  (when (not (sb-plan-valid plan))
    (cerror "noop" "trying to execute invalid plan ~S" plan)
    (return-from execute-plan nil))
  (loop for cn in (sb-plan-list plan) do
	(cond ((sb-constraint-p cn)
	       (execute-propagate-valid cn))
	      ((listp cn)
	       (execute-plan-cycle cn))))
  )

(defun execute-plan-cycle (cycle-info)
  (let* ((cycle-cns (first cycle-info))
	 (cycle-inputs (second cycle-info))
	 (cycle-outputs (third cycle-info))
	 (cycle-solvers-found-soln nil)
	 )
    ;; if any of the cycle input vars (input vars to any of
    ;; the cns in the cycle that are not set by other cycle cns)
    ;; are invalid, invalidate outputs and return without trying
    ;; to solve the cycle.
    (when (loop for var in cycle-inputs always (VAR-valid var))
      ;; input vars valid: try solving cycle
      ;; print warning
      (signal-cycle cycle-cns)
      ;; try cycle solvers
      (setq cycle-solvers-found-soln (call-cycle-solvers cycle-cns))
      )
    ;; set cycle outputs to valid if the cycle is solved, else invalid.
    (let* ((cycle-output-valid (not (null cycle-solvers-found-soln))))
      (loop for var in cycle-outputs do
	    (setf (VAR-valid var) cycle-output-valid)))
    ))

;; ** plan invalidation **

(defun create-valid-plan (root-cns plan-list)
  (let ((plan (make-sb-plan :list plan-list
			    :root-cns root-cns
			    :valid t)))
    (add-valid-plan-to-cns plan (sb-plan-list plan))
    (add-valid-plan-to-cns plan (sb-plan-root-cns plan))
    plan))

(defun add-valid-plan-to-cns (plan cns)
  (loop for cn in cns
      do (cond ((listp cn)
		;; cycle in list
		(add-valid-plan-to-cns plan (first cn)))
	       (t
		(set-sb-slot cn :valid-plans
			     (adjoin plan (get-sb-slot cn :valid-plans)))))
	 ))

(defun invalidate-plans-on-setting-method (cn new-mt)
  (invalidate-constraint-plans cn)
  (when new-mt
    (do-method-input-vars (var cn new-mt)
      (let ((input-cn (VAR-determined-by var)))
	(when input-cn
	  (invalidate-constraint-plans input-cn))
	)))
  )
  
(defun invalidate-constraint-plans (invalid-cn)
  (let* ()
    (loop for plan in (get-sb-slot invalid-cn :valid-plans) do
	  (setf (sb-plan-valid plan) nil)
	  (remove-invalid-plan-from-other-cns plan (sb-plan-root-cns plan) invalid-cn)
	  (remove-invalid-plan-from-other-cns plan (sb-plan-list plan) invalid-cn)
	  )
    (set-sb-slot invalid-cn :valid-plans nil)
    ))

(defun remove-invalid-plan-from-other-cns (plan cns invalid-cn)
  (loop for cn in cns do
	(cond ((eql cn invalid-cn)
	       nil)
	      ((listp cn)
	       (remove-invalid-plan-from-other-cns plan (first cn) invalid-cn))
	      (t
	       (set-sb-slot cn :valid-plans
			    (remove plan (get-sb-slot cn :valid-plans)))))
	))


