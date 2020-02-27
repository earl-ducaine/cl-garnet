;;;-*- Mode: COMMON-LISP; Package: MULTI-GARNET -*-

(in-package :multi-garnet)

(eval-when (:load-toplevel :execute)
  (when (fboundp 'disable-multi-garnet)
    (disable-multi-garnet)))

(defvar *multi-garnet-version* "2.2")

;; ***** "hook" (advise) facility *****

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun get-save-fn-symbol-name (sym)
    (cond ((symbolp sym)
	   (intern (concatenate 'string (symbol-name sym) "-SAVE-FN-SYMBOL")
		   (find-package :multi-garnet)))
	  ((and (consp sym)
		(eq :quote (first sym))
		(symbolp (second sym)))
	   (get-save-fn-symbol-name (second sym)))
	  (t (cerror "cont" "get-save-fn-symbol-name: bad symbol ~S" sym))
	  ))
  )

(defun install-hook (fn hook)
  (if (not (fboundp fn))
    (cerror "noop" "can't install hook on fn ~S -- no fn def" fn)
    (let ((save-fn (get-save-fn-symbol-name fn)))
      (unless (fboundp save-fn)
        (setf (symbol-function save-fn)
              (symbol-function fn)))
      (setf (symbol-function fn)
            (symbol-function hook)))))

(defun uninstall-hook (fn)
  (let ((save-fn (get-save-fn-symbol-name fn)))
    (if (fboundp save-fn)
	(setf (symbol-function fn) (symbol-function save-fn))
      (format t "~&warning: uninstall-hook: ~S fn never saved~%" fn))))

(defmacro call-hook-save-fn (fn &rest args)
  (let ((save-fn (get-save-fn-symbol-name fn)))
    `(,save-fn ,@args)))


;; ***** os (object slot) objects *****

(defmacro os (obj slot) `(cons ,obj ,slot))

(defmacro os-p (os)
  `(let ((os ,os))
     (and (consp os)
	  (schema-p (car os)))))

(defmacro os-object (os) `(car ,os))
(defmacro os-slot (os) `(cdr ,os))

(defvar *new-name-number* 0)

(defun create-new-name (name)
  (format nil "~A-~A" name (incf *new-name-number* 1)))

;; ***** access to extra cn, var slots used by multi-garnet *****

;;    constraint fields used by multi-garnet:
;; os              | os object | object&slot containing this constraint.
;; connection      | keyword   | connection status of this constraint.
;; variable-paths  | list of paths | paths for accessing constraint variables.
;; path-slot-list  | list of os's  | os's of path slots for this cn

;; Multi-Garnet fields go through sb:get-sb-slot (since these fields are
;; stored on the other-slots list).
(defmacro cn-os (v) `(sb:get-sb-constraint-slot ,v :mg-os))
(defmacro cn-connection (v) `(sb:get-sb-constraint-slot ,v :mg-connection))
(defmacro cn-variable-paths (c) `(sb:get-sb-constraint-slot ,c :mg-variable-paths))
(defmacro cn-variable-names (c) `(sb:get-sb-constraint-slot ,c :mg-variable-names))
(defmacro cn-path-slot-list (c) `(sb:get-sb-constraint-slot ,c :mg-path-slot-list))
(defsetf cn-os (v) (val) `(sb:set-sb-constraint-slot ,v :mg-os ,val))
(defsetf cn-connection (v) (val) `(sb:set-sb-constraint-slot ,v :mg-connection ,val))
(defsetf cn-variable-paths (c) (val) `(sb:set-sb-constraint-slot ,c :mg-variable-paths ,val))
(defsetf cn-variable-names (c) (val) `(sb:set-sb-constraint-slot ,c :mg-variable-names ,val))
(defsetf cn-path-slot-list (c) (val) `(sb:set-sb-constraint-slot ,c :mg-path-slot-list ,val))

(defmacro cn-connection-p (cn val)
  `(eq (cn-connection ,cn) ,val))

(defun invalidate-plan-fn (obj slot val)
  (when (and (eql slot :selected-method)
	     (constraint-p obj))
    ;; when selected method of constraint is set, may need to invalidate plans
    (sb:invalidate-plans-on-setting-method obj val)))

(defun create-mg-constraint (&key (strength :max)
				  (variables nil)
				  (methods nil)
				  (os nil)
				  (connection :unconnected)
				  (variable-paths nil)
				  (variable-names nil)
				  (path-slot-list nil)
				  (name nil)
				  )
  (let ((cn (sb:create-sb-constraint :strength strength
				     :variables variables
				     :methods methods
				     :name name)))
    (setf (CN-os cn) os)
    (setf (CN-connection cn) connection)
    (setf (CN-variable-paths cn) variable-paths)
    (setf (CN-variable-names cn) variable-names)
    (setf (CN-path-slot-list cn) path-slot-list)
    ;; want to invalidate plans when setting selected method
    (sb:add-set-slot-fn cn #'invalidate-plan-fn)
    cn))

(defun clone-constraint (cn)
  ;; copy ptrs to generally-immutable slots
  ;; (:strength :variable-paths)
  ;; and initialize others
  (create-mg-constraint :strength (sb:cn-strength cn)
			:methods (loop for mt in (sb:cn-methods cn)
				     collect (clone-mg-method mt))
			:variable-paths (cn-variable-paths cn)
			:variable-names (cn-variable-names cn)))

;;    variable fields used by multi-garnet:
;;  os            | os object   | object&slot containing the value of
;;                |             | this variable.
(defmacro var-os (v) `(sb:get-sb-variable-slot ,v :mg-os))
(defsetf var-os (v) (val) `(sb:set-sb-variable-slot ,v :mg-os ,val))

(defun create-mg-variable (&key (name nil)
				(os nil))
  (let* ((val (if (os-p os)
		  (g-value (os-object os) (os-slot os))
		nil))
	 (var (sb:create-sb-variable :name name
				     :value val)))
    (setf (VAR-os var) os)
    ;; update OS val when var value changed
    (sb:add-set-slot-fn var #'update-var-value-fn)
    var))

;; returns true if this is a *multi-garnet* constraint
(defun constraint-p (obj)
  (and (sb:sb-constraint-p obj)
       (not (null (CN-connection obj)))))

;; ***** gv-sb-slot and supporting fns *****

;; the following functions support using gv-sb-slot to access sb object
;; slots from within garnet formulas, so that the formulas will be
;; invalidated correctly when the slots are changed.  This is implemented
;; by saving a "copy" kr object on (sb:get-sb-slot obj
;; :gv-sb-slot-kr-object) Every time a slot on the list (sb:get-sb-slot obj
;; :gv-sb-slots) is changed, the corresponding slot in the kr object is
;; also set.  When gv-sb-slot is called, it establishes a formula to this
;; kr object.  Slot names are added to (sb:get-sb-slot obj :gv-sb-slots)
;; when gv-sb-slot is called, and removed when it appears that a slot no
;; longer has any dependencies.

(defun gv-sb-slot-check (obj slot val)
  (when (member slot (sb:get-sb-slot obj :gv-sb-slots))
    (update-sb-slot-formulas obj slot val)))

(defun update-sb-slot-formulas (obj slot val)
  (let ((slots (sb:get-sb-slot obj :gv-sb-slots)))
    (when (member slot slots)
      (let* ((kr-obj (get-gv-sb-slot-kr-object obj)))
	(cond ((kr-slot-referenced kr-obj slot)
	       (s-value kr-obj slot val))
	      (t
	       ;; no reference: we don't have to track this slot any more
	       (sb:set-sb-slot obj :gv-sb-slots (remove slot slots))
	       ;; if we have no more slots, can remove check-fn
	       (when (null (sb:get-sb-slot obj :gv-sb-slots))
		 (sb:remove-set-slot-fn obj #'gv-sb-slot-check))
	       ))
	))))

(defun kr-slot-referenced (schema slot)
  (kr::slot-dependents (kr::slot-accessor schema slot)))

(defun get-gv-sb-slot-kr-object (obj)
  (let ((kr-obj (sb:get-sb-slot obj :gv-sb-slot-kr-object)))
    (unless (schema-p kr-obj)
      (setq kr-obj (create-instance nil nil))
      (sb:set-sb-slot obj :gv-sb-slot-kr-object kr-obj))
    kr-obj))

;; fn to use to access sb object slots from within formulas
(defun gv-sb-slot (obj slot)
  (let* ((formula-slots (sb:get-sb-slot obj :gv-sb-slots))
	 (kr-obj (get-gv-sb-slot-kr-object obj)))
    (unless (member slot formula-slots)
      (sb:set-sb-slot obj :gv-sb-slots (cons slot (sb:get-sb-slot obj :gv-sb-slots)))
      ;; make sure we add check fn
      (sb:add-set-slot-fn obj #'gv-sb-slot-check)
      ;; make sure slot value copied to kr object
      (s-value kr-obj slot (sb:get-sb-slot obj slot)))
    (kr::gv-value-fn kr-obj slot)))

;; ***** m-constraint macro used to construct constraint given method forms *****

;; examples:
;;  (m-constraint :strong ((yy (gvl :a)) (xx (gvl :a :b))) (setf yy xx))

(defmacro m-constraint (strength-spec var-specs &rest method-specs)
  (let* ((strength
	  (cond ((find strength-spec sb:*strength-keyword-list*)
		 strength-spec)
		(t (error "bad strength in m-constraint: ~S" strength-spec))))
	 (var-names (loop for spec in var-specs collect
			  (if (symbolp spec) spec (first spec))))
	 (var-paths (loop for spec in var-specs collect
			  (cond ((symbolp spec)
				 (list
				  (intern (symbol-name spec)
					  (find-package :keyword))))
				((and (listp spec)
				      (listp (second spec))
				      (eql 'gvl (first (second spec))))
				 (cdr (second spec)))
				(t (error "bad var-specs in m-constraint")))))
         (method-output-var-name-lists
          (loop for spec in method-specs collect
                (let ((names (if (listp spec)
                               (if (listp (second spec))
                                 (second spec)
                                 (list (second spec))))))
                  (cond ((or (not (listp spec))
                             (not (eql 'setf (first spec))))
                         (error "bad method form in m-constraint: ~S" spec))
                        ((loop for varname in names
                               thereis (not (member varname var-names)))
                         (error "unknown var name in method form: ~S" spec)))
                  names)))
         (method-output-index-lists
	  (loop for var-name-list in method-output-var-name-lists collect
                (loop for varname in var-name-list collect
                      (position varname var-names))))
	 (method-forms
	  (loop for spec in method-specs
	      as output-var-names in method-output-var-name-lists
	      collect
		(if (null (cddr spec))
		    ;; if form is nil, this is a stay cn
		    nil
		  `(progn
		     ;; include output vars, to avoid "var never used" warnings
		     ,@output-var-names
		     ,@(cddr spec))
		  )))
	 (method-list-form
	  `(list ,@(loop for indices in method-output-index-lists
			 as form in method-forms
			 collect
			 `(create-mg-method
			   :output-indices (quote ,indices)
			   :code ,(if form
				     `(function (lambda (cn)
						  (execute-m-constraint-method
						   cn (function (lambda ,var-names ,form)))))
				    ;; if form is nil, this method is a stay.
				    '(function (lambda (cn) cn)))
			   ))))
	 (constraint-form
	  `(create-mg-constraint
	    :strength ,strength
	    :methods ,method-list-form
	    :variable-paths (quote ,var-paths)
	    :variable-names (quote ,var-names)))
	 )
    constraint-form))

(defun create-mg-method (&key (output-indices nil)
			      (code #'(lambda (cn) cn)))
  (let ((mt (sb:create-sb-method :outputs nil
				 :code code)))
    (sb:set-sb-slot mt :mg-output-indices output-indices)
    mt))

(defun clone-mg-method (mt)
  (create-mg-method :output-indices (sb:get-sb-slot mt :mg-output-indices)
		    :code (sb:mt-code mt)))

;; initialize method :outputs lists
;; (possible speed-up: save list, rplaca values)
(defun init-method-outputs (cn)
  (let ((vars (sb:cn-variables cn)))
    (loop for mt in (SB:cn-methods cn) do
	  (setf (sb:mt-outputs mt)
	    (loop for index in (sb:get-sb-slot mt :mg-output-indices)
		collect (nth index vars))))
    ))


;; handles stay specified as (m-stay-constraint :max box (gvl :a) (gvl :b :c))

(defmacro m-stay-constraint (strength-spec &rest var-specs)
  (let* ((var-names (loop for spec in var-specs collect
			  (if (symbolp spec) spec (gentemp))))
	 (full-var-specs (loop for name in var-names as spec in var-specs collect
			       (if (symbolp spec) spec (list name spec)))))
    `(let ((cn (m-constraint ,strength-spec ,full-var-specs (setf ,var-names))))
       (sb:set-sb-slot cn :stay-flag t)
       cn)))

(defun execute-m-constraint-method (cn method-code)
  (let* ((vars (SB:CN-variables cn))
	 (var-values (loop for v in vars
			 collect (get-variable-value v)))
	 (output-vars (sb:selected-method-output-vars cn)))
    (cond ((= 1 (length output-vars))
 	   (let* ((output-var (car output-vars))
 		  (val (apply method-code var-values)))
 	     (set-variable-value output-var val)))
 	  (t
 	   (let ((output-values (multiple-value-list (apply method-code var-values))))
 	     (when (< (length output-values) (length output-vars))
 	       (cerror "cont"
 		       "not enough output values for output variables in m-constraint ~S: ~S <- ~S"
 		       cn output-vars output-values))
 	     (loop for output-var in output-vars
 		 as val in output-values
 		 do (set-variable-value output-var val))
 	     )))
    ))


;; ***** entry for turning off collection/update of invalidated paths and formulas *****
;; warning: be very careful using these

(defvar *collect-invalid-paths-formulas* t)

(defmacro with-no-invalidation-collection (&rest forms)
  (let* ((old-val-var (gentemp)))
    `(let* ((,old-val-var *collect-invalid-paths-formulas*))
       (unwind-protect
	   (progn
	     (setq *collect-invalid-paths-formulas* nil)
	     (progn ,@forms))
	 (setq *collect-invalid-paths-formulas* ,old-val-var)))
    ))

(defvar *update-invalid-paths-formulas* t)

(defmacro with-no-invalidation-update (&rest forms)
  (let* ((old-val-var (gentemp)))
    `(let* ((,old-val-var *update-invalid-paths-formulas*))
       (unwind-protect
	   (progn
	     (setq *update-invalid-paths-formulas* nil)
	     (progn ,@forms))
	 (setq *update-invalid-paths-formulas* ,old-val-var)))
    ))

;; ***** set-slot-basic macro *****

(defvar *invalidated-path-constraints* nil)

(defun constraint-in-obj-slot (cn obj slot)
  (let* ((os (cn-os cn)))
    (and os
	 (eql (os-object os) obj)
	 (eql (os-slot os) slot))))

(defmacro set-slot-basic (obj slot value
			   &key
			   (prohibit-constraints nil)
			   (auto-activate-constraints t)
			   (invalidate-paths t)
			   )
  (unless (and (member auto-activate-constraints '(nil t))
	       (member invalidate-paths '(nil t))
	       (member prohibit-constraints '(nil t)))
    (cerror "cont" "set-slot-basic: key vals should be t or nil"))
  (when (and prohibit-constraints auto-activate-constraints)
    (cerror "cont" "set-slot-basic: prohibit-constraints and auto-activate-constraints are both t"))
  `(let ((obj ,obj)
	 (slot ,slot)
	 (value ,value)
	 )
     ;; before checks

     ;; if prohibit-constraints=t, check that new and old values of slot are not constraints
     ,@(if prohibit-constraints
	   '(
	     (if (or (constraint-p value)
		     (constraint-p (get-local-value obj slot)))
		 (cerror "cont"
			 "can't set <~S,~S> to constraint" obj slot))
	     )
	 )

     ;; remove old constraint previously stored in slot
     ,@(if auto-activate-constraints
	   '(
	     ;; get old value using get-local-value, so we won't eval formula if one is stored in slot
	     (let ((old-value (get-local-value obj slot)))
	       (when (and (constraint-p old-value)
		      ;; only de-activate if this cn was activated for _this_ obj,slot
		      (constraint-in-obj-slot old-value obj slot))
		 (remove-constraint-from-slot obj slot old-value)))
	     )
	 )

     ;; actually set object slot
     (call-hook-save-fn kr::s-value-fn obj slot value)

     ;; after checks

     ;; add new constraint in slot
     ,@(if auto-activate-constraints
	   '(
	     (when (and (constraint-p value)
			;; only auto-activate if os=nil
			(null (cn-os value)))
	       (add-constraint-to-slot obj slot value))
	     )
	 )
     ;; invalidate constraints whose paths use this slot
     ,@(if invalidate-paths
	   '(
	     (save-invalidated-path-constraints obj slot)
	     )
	 )

     value))

;; fn that sets slot only, by calling saved s-value-fn
(defun set-slot-no-checks (obj slot value)
  (set-slot-basic obj slot value
		  :prohibit-constraints nil
		  :auto-activate-constraints nil
		  :invalidate-paths nil
		  ))


(defun add-constraint-to-slot (obj slot cn)
  (cond ((null (CN-variable-paths cn))
         ;; constraint is not a multi-garnet constraint, so don't auto-connect
         nil)
        ((CN-os cn)
         (error "shouldn't happen: can't add constraint ~S to <~S,~S>, already stored in <~S,~S>"
		cn obj slot (os-object (CN-os cn)) (os-slot (CN-os cn))))
        (t
         ;; set up constraint fields
         (setf (CN-os cn) (os obj slot))
         ;; activate new constraint
         (connect-add-constraint cn))
        ))

(defun remove-constraint-from-slot (obj slot cn)
  (declare (ignore obj slot))
  (cond ((null (CN-variable-paths cn))
         ;; constraint is not a multi-garnet constraint, so don't auto-connect
         nil)
        (t
         ;; remove old constraint
         (remove-disconnect-constraint cn)
         ;; clear constraint fields
         (setf (CN-os cn) nil))
        ))

(defun save-invalidated-path-constraints (obj slot)
  (when mg::*collect-invalid-paths-formulas*
    (setf *invalidated-path-constraints*
      (append (get-object-slot-prop obj slot :sb-path-constraints)
	      *invalidated-path-constraints*))
    ))

;; ***** hook into kr:s-value-n *****

;; This hook is used to cause (s-value obj <slot-with-sb-var> val) to
;; add&remove an edit constraint.  This also auto-activates a
;; constraint when it is stored in a slot.

;; default strength used when setting object slots using s-value
(defvar *default-input-strength* :strong)

(defun s-value-fn-hook (schema slot value)
  (multi-garnet-s-value-fn schema slot value *default-input-strength*))

(defun s-value-strength (obj slot value strength)
  (multi-garnet-s-value-fn obj slot value strength))

(defvar *s-value-bad-schema-action* nil)

(defun multi-garnet-s-value-fn (schema slot value input-strength)
  (cond ((not (schema-p schema))
	 (case *s-value-bad-schema-action*
	   (:print (format t "~&S-VALUE called with bad schema ~S slot ~S, value ~S.~%"
			   schema slot value))
	   (:break (cerror "continue" "S-VALUE called with bad schema ~S slot ~S, value ~S."
			   schema slot value))
	   )
	 )
	((eq slot :is-a)
	 ;; don't trap KR inheritance manipulation.
	 ;; do simple set, without worrying about cns or paths
	 (set-slot-basic schema slot value
			 :auto-activate-constraints nil
			 :invalidate-paths nil)
	 )
	(t
	 (let ((slot-var (get-object-slot-prop schema slot :sb-variable)))
	   (cond ((and (constraint-p value)
		       ;; have to use inheritence to find if slot currently has formula
		       (formula-p (get-value schema slot)))
		  (cerror "noop" "can't put sb constraint ~S in slot <~S,~S> with formula"
			  value schema slot))
		 ((and (formula-p value) slot-var)
		  (cerror "noop" "can't put formula ~S in sb variable slot <~S,~S>"
                     value schema slot))
		 ((and (formula-p value)
		       ;; use get-local-value: cn must be copied down explicitly
		       (constraint-p (get-local-value schema slot)))
		  (cerror "noop" "can't put formula ~S in sb constraint slot <~S,~S>"
			  value schema slot))
		 ((and (formula-p value)
		       (get-object-slot-prop schema slot :sb-path-constraints))
		  (cerror "noop" "can't put formula ~S in sb constraint path slot <~S,~S>"
			  value schema slot))
		 (slot-var
		  ;; set the value of a sb-variable by adding and removing a strong constraint
		  ;; (note that this implies that the set may not happen if the var's walkabout
		  ;; strength is strong enough)
		  (set-input-variable slot-var value input-strength)
		  )
		 (t
		  ;; slot not an sb slot, so do normal set
		  (set-slot-basic schema slot value
				  :auto-activate-constraints t
				  :invalidate-paths t)
		  ))
	   (update-invalidated-paths-and-formulas)
	   )))
  value)

;; note: we purposely don't trap calls to set slots when objects are
;; created (in fns kr::internal-s-value and kr::internal-add-value) because
;; the constraints are activated when activate-new-instance-cns is called
;; after the object is created.  This also ensures that any slots that are
;; set in a call to create-instance are set before any of the cns for that
;; object are added.

;; ***** hook into inheritence mechanism to inherit constraints *****

;; want to copy down constraints after all other initialization is done.
;; (note: if CLOS-style :after methods were supported, I'd put this as an
;;        after method of the top object initialization method)

(defun kr-call-initialize-method-hook (schema slot)
  (call-hook-save-fn kr::kr-call-initialize-method schema slot)
  (when (eql slot :initialize)
    (copy-down-and-activate-constraints schema)))

(defun kr-init-method-hook (schema the-function)
  (call-hook-save-fn kr::kr-init-method schema the-function)
  (copy-down-and-activate-constraints schema))

(defun copy-down-and-activate-constraints (schema)
  (copy-down-mg-constraints schema)
  (activate-new-instance-cns schema)
  (update-invalidated-paths-and-formulas)
  )

;; copies down cns from parent, _without_ activating them
(defun copy-down-mg-constraints (schema)
  (let ((parent (car (get-value schema :is-a))))
    (when parent
      (let* ((local-only-slots-val (kr::g-value-no-copy parent :LOCAL-ONLY-SLOTS))
	     (local-only-slots (if (listp local-only-slots-val)
				   local-only-slots-val
				 (list local-only-slots-val))))
	(doslots (slot parent)
		 (when (and (not (eq slot :is-a))
			    (not (member slot local-only-slots))
			    (not (has-slot-p schema slot))
			    (constraint-p (get-local-value parent slot))
			    (constraint-in-obj-slot (get-local-value parent slot) parent slot))
		   (set-slot-basic schema slot
				   (clone-constraint (get-local-value parent slot))
				   :auto-activate-constraints nil
				   :invalidate-paths t)
		   ))
	))))

;; activates all cns in new instance (which all should be unconnected)
(defun activate-new-instance-cns (schema)
  (doslots
   (slot schema)
   (let ((value (get-local-value schema slot)))
     (cond ((not (constraint-p value))
	    nil)
	   ((not (null (CN-os value)))
	    ;; inherited cn that belongs to another os
	    nil)
	   ((cn-connection-p value :unconnected)
	    (add-constraint-to-slot schema slot value))
	   (t
	    (cerror "don't activate cn" "initializing <~S,~S>: found connected cn ~S with os ~S"
		    schema slot value (CN-os value)))
	   ))))

;; ***** hook into move-grow-interacter and two-point-interactor to set :box or :points slot *****

;; hook to force move-grow-interacter and two-point-interactor to set :box or :points slot
;; (hence calling constraint solver) rather than destructively changing the list.
(defun set-obj-list4-slot-no-db-hook (obj slot new-list4)
  (let* ((var (get-object-slot-prop obj slot :sb-variable))
	 (strength *default-input-strength*))
    (cond ((null var)
	   ;; no var for slot: just change value
	   (call-hook-save-fn inter::set-obj-list4-slot-no-db obj slot new-list4))
	  (t
	   ;; _may_ be able to set slot.  Try setting to copy of new list.
	   (set-input-variable var (copy-list new-list4) strength))
	  )
    new-list4))


;; ***** fns to destroy slots and schemas (experimental) *****

(defun destroy-slot-hook (schema slot)
  (let ((invalid-cns nil)
	(invalid-vars nil)
	(val (get-local-value schema slot))
	(var (get-object-slot-prop schema slot :sb-variable)))
      ;; remove constraint in slot
    (when (and (constraint-p val)
	       (constraint-in-obj-slot val schema slot))
      (remove-constraint-from-slot schema slot val))
    ;; find all constraints that use this slot
    (setq invalid-cns (get-object-slot-prop schema slot :sb-path-constraints))
    ;; find any var in this slot
    (when var
      (setq invalid-cns (append (sb:VAR-constraints var) invalid-cns))
      (push var invalid-vars))
    ;; remove invalid constraints
    (loop for cn in invalid-cns do (remove-disconnect-constraint cn))
    ;; flush invalid vars
    (loop for var in invalid-vars do
	  (setf (VAR-os var) nil))
    ;; actually destroy the slot
    (call-hook-save-fn kr::destroy-slot schema slot)
    ;; try reconnecting the invalid constraints
    (loop for cn in invalid-cns do (connect-add-constraint cn))
    ))


(defun destroy-schema-hook (schema &optional (send-destroy-message NIL) recursive-p)
  (let ((invalid-cns nil)
	(invalid-vars nil))
    (when (schema-p schema)
      ;; remove constraints in slots in this obj
      (LOCALLY
 (DECLARE (OPTIMIZE (SPEED 0) (SAFETY 3) (SPACE 0) (DEBUG 3)))
 (PROGN
  (MAPHASH
   #'(LAMBDA (KR::ITERATE-IGNORED-SLOT-NAME KR::ITERATE-SLOT-VALUE-ENTRY)
       (DECLARE (IGNORE KR::ITERATE-IGNORED-SLOT-NAME))
       (LET ((KR::SLOT (KR::SL-NAME KR::ITERATE-SLOT-VALUE-ENTRY))
             (KR::VALUE (KR::SL-VALUE KR::ITERATE-SLOT-VALUE-ENTRY)))
         (UNLESS (KR::IS-INHERITED (KR::SL-BITS KR::ITERATE-SLOT-VALUE-ENTRY))
           (UNLESS (EQ KR::VALUE KR::*NO-VALUE*)
             (LET ((SLOT KR::SLOT))
               (LET ((VAL (GET-LOCAL-VALUE SCHEMA SLOT)))
                 (WHEN
                     (AND (CONSTRAINT-P VAL)
                          (CONSTRAINT-IN-OBJ-SLOT VAL SCHEMA SLOT))
                   (REMOVE-BAD-INV-OBJECTS SCHEMA)
                   (REMOVE-CONSTRAINT-FROM-SLOT SCHEMA SLOT VAL))))))))
   (KR::SCHEMA-BINS SCHEMA))))
      ;; find all constraints that use slots in this object
      (LOCALLY
 (DECLARE (OPTIMIZE (SPEED 0) (SAFETY 3) (SPACE 0) (DEBUG 3)))
 (PROGN
  (MAPHASH
   #'(LAMBDA (KR::ITERATE-IGNORED-SLOT-NAME KR::ITERATE-SLOT-VALUE-ENTRY)
       (DECLARE (IGNORE KR::ITERATE-IGNORED-SLOT-NAME))
       (LET ((KR::SLOT (KR::SL-NAME KR::ITERATE-SLOT-VALUE-ENTRY))
             (KR::VALUE (KR::SL-VALUE KR::ITERATE-SLOT-VALUE-ENTRY)))
         (UNLESS (KR::IS-INHERITED (KR::SL-BITS KR::ITERATE-SLOT-VALUE-ENTRY))
           (UNLESS (EQ KR::VALUE KR::*NO-VALUE*)
             (LET ((SLOT KR::SLOT))
               (LET ((VAL (GET-LOCAL-VALUE SCHEMA SLOT)))
                 (WHEN
                     (AND (CONSTRAINT-P VAL)
                          (CONSTRAINT-IN-OBJ-SLOT VAL SCHEMA SLOT))
                   (REMOVE-BAD-INV-OBJECTS SCHEMA)
                   (REMOVE-CONSTRAINT-FROM-SLOT SCHEMA SLOT VAL))))))))
   (KR::SCHEMA-BINS SCHEMA))))
      ;; remove invalid constraints
      (loop for cn in invalid-cns do
	    ;; make sure that :is-a-inv is good before changing any slots
	   (remove-bad-inv-objects schema)
	   (remove-disconnect-constraint cn))
      ;; flush invalid vars
      (loop for var in invalid-vars do
	    (setf (VAR-os var) nil))
      ;; actually destroy the schema
      (call-hook-save-fn kr::destroy-schema schema send-destroy-message recursive-p)
      ;; try reconnecting the invalid constraints
      (loop for cn in invalid-cns do (connect-add-constraint cn))
      )))



(defun destroy-schema-hook-old (schema &optional (send-destroy-message NIL) recursive-p)
  (let ((invalid-cns nil)
	(invalid-vars nil))
    (when (schema-p schema)
      ;; remove constraints in slots in this obj
      (doslots
       (slot schema)
       (let ((val (get-local-value schema slot)))
	 (when (and (constraint-p val)
		    (constraint-in-obj-slot val schema slot))
	   ;; make sure that :is-a-inv is good before changing any slots
	   (remove-bad-inv-objects schema)
	   (remove-constraint-from-slot schema slot val))
	 ))
      ;; find all constraints that use slots in this object
      (doslots
       (slot schema)
       (let* ((var (get-object-slot-prop schema slot :sb-variable))
	      (cns (get-object-slot-prop schema slot :sb-path-constraints)))
	 (setq invalid-cns (append cns invalid-cns))
	 (when var
	   (setq invalid-cns (append (sb:VAR-constraints var) invalid-cns))
	   (push var invalid-vars))
	 ))
      ;; remove invalid constraints
      (loop for cn in invalid-cns do
	    ;; make sure that :is-a-inv is good before changing any slots
	   (remove-bad-inv-objects schema)
	   (remove-disconnect-constraint cn))
      ;; flush invalid vars
      (loop for var in invalid-vars do
	    (setf (VAR-os var) nil))
      ;; actually destroy the schema
      (call-hook-save-fn kr::destroy-schema schema send-destroy-message recursive-p)
      ;; try reconnecting the invalid constraints
      (loop for cn in invalid-cns do (connect-add-constraint cn))
      )))

(defun remove-bad-inv-objects (schema)
  (s-value schema :is-a-inv
	   (loop for obj in (g-value schema :is-a-inv)
	       when (schema-p obj)
	       collect obj)))

;; *** connecting and disconnecting constraints utilities *****

(defun connect-add-constraint (cn)
  (when (and (os-p (cn-os cn))
	     (cn-connection-p cn :unconnected))
    (connect-constraint cn))
  (when (cn-connection-p cn :connected)
    (mg-add-constraint cn))
  )

(defun remove-disconnect-constraint (cn)
  (when (cn-connection-p cn :graph)
    (mg-remove-constraint cn))
  (when (and (os-p (cn-os cn))
	     (or (cn-connection-p cn :connected)
		 (cn-connection-p cn :broken-path)))
    ;; if cn is not stored in an object slot
    ;; (like input cns, with-stays cns),
    ;; no need to disconnect.
    (disconnect-constraint cn))
  )

(defun connect-constraint (cn)
  (let* ((cn-os (CN-os cn))
	 (cn-var-paths (CN-variable-paths cn)))
    (cond ((not (cn-connection-p cn :unconnected))
	   (cerror "noop" "trying to connect constraint ~S with connection ~S"
		   cn (CN-connection cn))
	   nil)
	  (t
	   (let* ((root-obj (os-object cn-os))
		  (cn-path-links nil)
		  (paths-broken nil)
		  var-os-list)
	     ;; follow paths to get os-list for vars,
	     ;; while setting up dependency links
	     (setf var-os-list
	       (loop for path in cn-var-paths collect
		 (let ((obj root-obj))
		   (loop for (slot next-slot) on path do
		     ;; if at end of path, return os to final slot
		     (when (null next-slot)
		       (return (os obj slot)))
		     ;; at link slot: set up dependency and back ptrs
		     (set-object-slot-prop obj slot :sb-path-constraints
					   (adjoin cn (get-object-slot-prop
						       obj slot :sb-path-constraints)))
		     (push (os obj slot) cn-path-links)
		     ;; check that path slot doesn't contain local formula
		     ;; (doesn't matter if inherited slot contains formula,
		     ;; since we copy down value.)
		     ;;  << formula check removed >>
		     ;; make sure that value is copied down, so we detect changes
		     (copy-down-slot-value obj slot)
		     ;; ...and continue to next step on path
		     (setf obj (g-value obj slot))
		     ;; if next obj is not a schema, path is broken
		     (when (not (schema-p obj))
		       (setf paths-broken t)
		       (return nil))
		     ))))
	     ;; update ptrs from constraint to links
	     (setf (CN-path-slot-list cn) cn-path-links)
	     ;; iff no paths are broken, find/alloc vars
	     (cond (paths-broken
		    ;; some paths are broken.
		    ;; don't alloc vars.
		    (setf (SB:CN-variables cn) nil)
		    (setf (CN-connection cn) :broken-path))
		   (t
		    ;; all paths unbroken, find/alloc vars
		    (setf (SB:CN-variables cn)
		      (loop for var-os in var-os-list
		       collect (get-os-var var-os)))
		    ;; init output lists in cn methods
 		    (init-method-outputs cn)
		    ;; now, cn is connected
		    (setf (CN-connection cn) :connected)))
	     )))
    ))

(defun disconnect-constraint (cn)
  (let* ()
    (when (cn-connection-p cn :graph)
      (cerror "cont" "trying to disconnect constraint ~S with connection ~S"
              cn (CN-connection cn)))
    (loop for path-os in (CN-path-slot-list cn) do
      (set-os-prop path-os :sb-path-constraints
		   (remove cn (get-os-prop path-os :sb-path-constraints))))
    (setf (CN-path-slot-list cn) nil)
    (setf (SB:CN-variables cn) nil)
    (setf (CN-connection cn) :unconnected)
    ))

;; ***** marking invalid formulas *****

(defvar *invalidated-formulas* nil)

(in-package :kr)

;; this hook is essentially a complete copy of kr::propagate-change,
;; because we need to make changes in the middle, to stop propagation when
;; we reach a constrained variable.  We define it in package :kr, since it
;; calls so many kr-internal fns and macros.

(defun mg::propagate-change-hook (schema slot)
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
	  (cond ((and schema-ok
		      (or (mg::get-object-slot-prop new-schema new-slot :sb-variable)
			  (mg::get-object-slot-prop new-schema new-slot :sb-path-constraints)))
		 ;; we want to invalidate a formula that sets a constrained slot or a path slot.
		 ;; do not invalidate or propagate further: save ptr to formula to eval later
		 (when mg::*collect-invalid-paths-formulas*
		   (push formula mg::*invalidated-formulas*))
		 (continue-out))
		(schema-ok
		 (setf new-entry (slot-accessor new-schema new-slot))
		 (run-invalidate-demons new-schema new-slot new-entry)
		 )
		(t
		 #+GARNET-DEBUG
		 (progn
		   (format
		    t
		    "propagate-change: formula ~S on destroyed object ~S ~S~%    ~
	from change in schema ~S, slot ~S.~%"
		    formula new-schema new-slot schema slot))
		 ))
	  ;; The formula gets invalidated here.
	  (set-cache-is-valid formula nil)
	  ;; Notify all children who used to inherit the old value of the
	  ;; formula.
	  (if schema-ok
	    (if new-entry
		(let ((new-value  (sl-value new-entry))
                      (new-bits   (sl-bits new-entry))
		      (dependents (slot-dependents new-entry)))
		  (if (and (is-parent new-bits)
		           entry
                           (not (eq (sl-value entry) *no-value*)))
			(update-inherited-values
			 new-schema new-slot new-value T))
		  ;; Now recurse, following the slot in the schema on which
		  ;; the formula sits.
		  (if dependents
		      (propagate-change new-schema new-slot))))))))))

(in-package :multi-garnet)

;; ***** updating invalidated paths and formulas *****

(defvar *max-path-updates* 10)

(defvar *save-invalidated-path-constraints* nil)
(defvar *save-invalidated-formulas* nil)

(defvar *max-path-updates-warning* t)

(defvar *path-update-loop-warning* nil)

(defun update-invalidated-paths-and-formulas ()
  (when *update-invalid-paths-formulas*
    (with-no-invalidation-update
	nil
      ;;  Repeatedly update any constraints whose paths may have been
      ;; invalidated, and invalidated formulas on constrained variables.
      ;; This process may cause other paths or formulas to be invalidated, so
      ;; repeat until no other paths are invalidated.  If this is repeated
      ;; more than *max-path-updates* times, this indicates that there may
      ;; be a cycle: break, and optionally continue.
      (loop for invalidation-count from 1 to *max-path-updates*
	  while (or *invalidated-path-constraints*
		    *invalidated-formulas*)
	  do (when (or (eql *path-update-loop-warning* t)
		       (and (numberp *path-update-loop-warning*)
			    (>= invalidation-count *path-update-loop-warning*)))
	       (format t "~&update loop ~S: invalid paths= ~S, invalid formulas= ~S~%"
		       invalidation-count *invalidated-path-constraints* *invalidated-formulas*))
	     (update-invalidated-path-constraints)
	     (update-invalidated-formulas))
      (when (or *invalidated-path-constraints*
		*invalidated-formulas*)
	;; we have looped too many times: assume that there is an infinite loop.
	;; Print warning, and save remaining unresolved cns and formulas
	(cond ((eql *max-path-updates-warning* :error)
	       (cerror
		"clear lists of invalidated constraints and formulas"
		"updated paths and formulas ~S times without resolving invalidated constraints ~S and formulas ~S"
		*max-path-updates* *invalidated-path-constraints* *invalidated-formulas*)
	       )
	      (*max-path-updates-warning*
	       (format
		t
		"~&updated paths and formulas ~S times without resolving invalidated constraints ~S and formulas ~S:  clearing lists~%"
		*max-path-updates* *invalidated-path-constraints* *invalidated-formulas*)
	       )
	      )
	(setf *save-invalidated-path-constraints* *invalidated-path-constraints*)
	(setf *invalidated-path-constraints* nil)
	(setf *save-invalidated-formulas* *invalidated-formulas*)
	(setf *invalidated-formulas* nil))
      )))

(defun update-invalidated-path-constraints ()
  (let* ((path-constraints
	  (remove-duplicates *invalidated-path-constraints*)))
    ;; may invalidate other paths during this process -- record
    (setf *invalidated-path-constraints* nil)
    (when path-constraints
      (loop for cn in path-constraints do
	    (remove-disconnect-constraint cn)
	    (connect-add-constraint cn)
	    ))
    ))

;; The function update-invalidated-formulas updates all of the invalid
;; formulas in *invalidated-formulas*, as well as any formulas on
;; constrained slots that are invalidated when these formulas are updated,
;; etc.  This is done recursively here, rather than waiting for the loop in
;; update-invalidated-paths-and-formulas to call this repeatedly, because
;; of the possibility that there might be a ligitimate long chain of
;; formulas on constrained slots.  One difficulty is that we have to take
;; care of possible cycles of formulas.  We don't want to go into an
;; infinite loop.  This is handled by keeping a list of updated formulas,
;; and not updating a formula if it was already updated "upstream" of this
;; formula.  Since there are only a finite number of formulas, this will
;; terminate.  Another difficulty is that it is possible that we may update
;; particular formulas more than once.  For example, if formula A is a root
;; formula, and A->B and A->C and B->D and C->D, we may update formula D
;; twice, when going down each of the paths.  It isn't possible to do
;; clever planning to figure out the right order so that there is no
;; duplication, since we don't necessarily know whether each formula
;; updating will actually "take", updating the slot value, or whether other
;; constraints will override the formula input constraint.

(defun update-invalidated-formulas ()
  (let* ((root-formulas *invalidated-formulas*))
    (setf *invalidated-formulas* nil)
    (loop for (formula . more-roots) on root-formulas
	do (recursively-update-formulas formula more-roots))
    ))

(defun recursively-update-formulas (formula done)
  ;; break recursion if this formula is a member of done, which includes all
  ;; formulas we have updated (in which case there is a loop), as well as
  ;; formulas we know that we are going to update later.
  (unless (member formula done)
    (update-invalidated-formula formula)
    (let ((new-invalid-formulas *invalidated-formulas*)
	  (new-done (cons formula done)))
      (setf *invalidated-formulas* nil)
      (loop for child-formula in new-invalid-formulas
	  do (recursively-update-formulas child-formula new-done))
      )
    ))

(defun update-invalidated-formula (formula)
  (let* ((new-schema (kr::on-schema formula))
	 (new-slot (kr::on-slot formula))
	 (schema-ok (schema-p new-schema)))
    (when schema-ok
      (let* ((var (get-object-slot-prop new-schema new-slot :sb-variable)))
	(if var
	    (add-remove-formula-recomputing-constraint var)
	  (recompute-formula-saving-paths new-schema new-slot))))
    ))

(defun recompute-formula-saving-paths (schema slot)
  ;; increment sweep-mark, so formula doesn't erronously detect circularities
  (incf kr::*sweep-mark* 2)
  (kr::recompute-formula schema slot)
  (save-invalidated-path-constraints schema slot)
  )

(defvar *formula-set-strength* :strong)

(defun add-remove-formula-recomputing-constraint (var)
  (when (sb:weaker (sb:VAR-walk-strength var)
		   (sb:get-strength *formula-set-strength*))
    ;; we _may_ be able to set variable, because the the var walkstrength
    ;; is low.  however, still may not be able to set, because of interactions
    ;; of multi-output cns.  Just add&remove cn to set value.
    (let* ((cn (create-formula-recomputing-constraint var *formula-set-strength*)))
      (mg-add-constraint cn)
      (mg-remove-constraint cn)
      (dispose-formula-recomputing-constraint cn))
    ))

(defvar *formula-recomputing-constraint-reserve* nil)

;; dummy for input var os

(defun create-formula-recomputing-constraint (var strength)
  (let* ((cn (if *formula-recomputing-constraint-reserve*
		 (pop *formula-recomputing-constraint-reserve*)
	       (create-mg-constraint
		:methods (list (create-mg-method
				:code #'(lambda (cn)
					  (let* ((var (first (sb:cn-variables cn)))
						 (os (VAR-os var))
						 (obj (OS-object os))
						 (slot (OS-slot os)))
					    (recompute-formula-saving-paths obj slot)
					    (set-variable-value var (g-value obj slot))
					    ))
				:output-indices '(0))
			       )
		;; create dummy os, so we won't accidently activate
		;; this cn by storing it in an object slot.
		:os (os nil :formula-recomputing-cn)
		:name (create-new-name "formula-recomputing-cn")
		))))
    (setf (SB:CN-variables cn) (list var))
    ;; init output lists in cn methods
    (init-method-outputs cn)
    (setf (SB:CN-strength cn) (sb:get-strength strength))
    (setf (CN-connection cn) :connected)
    cn))

(defun dispose-formula-recomputing-constraint (cn)
  (push cn *formula-recomputing-constraint-reserve*))

(defun formula-recomputing-cn-p (cn)
  (let* ((os (cn-os cn)))
    (and (null (os-object os))
	 (eql :formula-recomputing-cn (os-slot os)))
    ))

;; ***** OS manipulation *****

(defun get-os-prop (os prop)
  (get-object-slot-prop
   (os-object os) (os-slot os) prop))

(defun get-object-slot-prop (obj slot prop)
  (getf (getf (g-local-value obj :sb-os-props)
	      slot nil)
	prop nil))

;; version of get-object-slot-prop for use in formulas,
;; that calls kr::gv-gn to tell kr that  it is
;; accessing :sb-os-props
(defun gv-object-slot-prop (obj slot prop)
  (when (schema-p obj)
    (kr::gv-fn obj :sb-os-props)
    (get-object-slot-prop obj slot prop)))

(defun set-os-prop (os prop val)
  (set-object-slot-prop
   (os-object os) (os-slot os) prop val))

(defun set-object-slot-prop (obj slot prop val)
  ;; let's catch constraints on the innards of sb-objects.
  ;; eventually, may want to allow such meta-constraints,
  ;; but not now
  (when (sb:sb-object-p obj)
    (cerror "cont" "trying to set os-prop of ~S, slot ~S, prop ~S"
            obj slot prop))
  (set-object-slot-prop-basic obj slot prop val)
  val)

(defun set-object-slot-prop-basic (obj slot prop val)
  (let* ((os-props (g-local-value obj :sb-os-props))
	 (slot-props (getf os-props slot nil)))
    (setf (getf slot-props prop) val)
    (setf (getf os-props slot) slot-props)
    (set-slot-no-checks obj :sb-os-props os-props)
    ;; mark props slot as changed, so debugger can
    ;; make (slow) displays depend on os props
    (mark-as-changed obj :sb-os-props)
    val))

(defun get-os-val (os)
  (g-value (os-object os) (os-slot os)))

(defun get-os-var (os)
  (get-object-slot-var (os-object os) (os-slot os)))

(defun get-object-slot-var (obj slot)
  (let ((var (get-object-slot-prop obj slot :sb-variable)))
    (when (null var)
      (setq var (create-object-slot-var obj slot)))
    var))

(defun create-object-slot-var (obj slot)
  (cond ;; << formula check removed >>
   ((sb:sb-object-p obj)
    (error "can't put variable on slot in sky-blue object: <~S,~S>"
	    obj slot))
   (t
    (let ((var (create-mg-variable :os (os obj slot))))
      (set-object-slot-prop obj slot :sb-variable var)
      (copy-down-slot-value obj slot)
      var))))

;; this fn is called when a sb-var is created, or a slot is used
;; on a constraint path, to copy down the slot value, if it is
;; inherited.  This allows us to detect when the value is changed.
(defun copy-down-slot-value (obj slot)
  (unless (has-slot-p obj slot)
    (with-constants-disabled
	(set-slot-basic
	 obj slot (g-value obj slot)
	 :prohibit-constraints t
	 :auto-activate-constraints nil
	 :invalidate-paths nil))))

(defun has-object-slot-var (obj slot)
  (get-object-slot-prop obj slot :sb-variable))

;; entries for accessing a variable value

(defun get-variable-value (var)
  (sb:var-value var))

(defun set-variable-value (var val)
  (setf (sb:var-value var) val))

;; ***** entries for setting a variable by adding/removing a stay constraint *****

;; flush test to avoid adding cn if walkstrengths of vars are too high:
;; for debugging, we want to save state when input var is added unsuccessfully.
;; (when (sb:weaker (sb:VAR-walk-strength v) (sb:get-strength strength)))

(defun set-input-variable (v val strength)
  ;; we _may_ be able to set variable.  Add&remove cn to set value.
  (let* ((cn (create-variable-input v val strength)))
    (mg-add-constraint cn)
    (mg-remove-constraint cn)
    (dispose-variable-input cn)
    (update-invalidated-paths-and-formulas)
    val))

;; ***** fns for managing reserves of simple input and stay constraints *****

(defvar *variable-input-reserve* nil)

(defun create-variable-input (v val strength)
  (let* ((cn (if *variable-input-reserve*
		 (pop *variable-input-reserve*)
	       (create-mg-constraint
		:methods (list (create-mg-method
				:code #'(lambda (cn)
					  (set-variable-value
					   (first (sb:cn-variables cn))
					   (sb:get-sb-slot cn :variable-input-value)
					   ))
				:output-indices '(0))
			       )
		;; create dummy os, so we won't accidently activate
		;; this cn by storing it in an object slot.
		:os (os nil :var-input-cn)
		:name (create-new-name "var-input-cn")
		))))
    (sb:set-sb-slot cn :variable-input-value val)
    (sb:set-sb-slot cn :input-flag t)
    (setf (SB:CN-variables cn) (list v))
    ;; init output lists in cn methods
    (init-method-outputs cn)
    (setf (SB:CN-strength cn) (sb:get-strength strength))
    (setf (CN-connection cn) :connected)
    cn))

(defun dispose-variable-input (cn)
  (push cn *variable-input-reserve*))

(defun variable-input-cn-p (cn)
  (let* ((os (cn-os cn)))
    (and (null (os-object os))
	 (eql :var-input-cn (os-slot os)))
    ))

(defvar *variable-stay-reserve* nil)

(defun create-variable-stay (v strength)
  (let* ((stay (if *variable-stay-reserve*
		   (pop *variable-stay-reserve*)
		 (create-mg-constraint
		  :methods (list (create-mg-method
				  :code #'(lambda (cn) cn)
				  :output-indices '(0))
				 )
		  ;; create dummy os, so we won't accidently activate
		  ;; this cn by storing it in an object slot.
		  :os (os nil :var-stay-cn)
		  :name (create-new-name "var-stay-cn")
		  ))))
    (sb:set-sb-slot stay :stay-flag t)
    (setf (SB:CN-variables stay) (list v))
    ;; init output lists in cn methods
    (init-method-outputs stay)
    (setf (SB:CN-strength stay) (sb:get-strength strength))
    (setf (CN-connection stay) :connected)
    stay))

(defun dispose-variable-stay (stay)
  (push stay *variable-stay-reserve*))

(defun variable-stay-cn-p (cn)
  (let* ((os (cn-os cn)))
    (and (null (os-object os))
	 (eql :var-stay-cn (os-slot os)))
    ))

;; ***** entry for enforcing stays during the executing of a form *****

(defmacro with-stays (obj-stay-list &rest forms)
  (let* ((cns-var (gentemp)))
    `(let* ((,cns-var ,`(stay-spec-to-cns (list ,@(loop for lst in obj-stay-list
						      collect `(list ,@lst))))))
       (unwind-protect
	   (progn
	     (add-variable-stays ,cns-var)
	     (progn ,@forms))
	 (remove-dispose-variable-stays ,cns-var)))
    ))

(defun add-variable-stays (cns)
  (loop for cn in cns do
    (mg-add-constraint cn)))

(defun remove-dispose-variable-stays (cns)
  (loop for cn in cns do
	(mg-remove-constraint cn)
	(dispose-variable-stay cn)))

(defun stay-spec-to-cns (stay-spec-list)
  (loop for (obj slot strength) in stay-spec-list
   collect (create-variable-stay
	    (get-object-slot-var obj slot)
	    (if strength strength *default-input-strength*))))

;; ***** entry for setting slots with specified strengths during the executing of a form *****

(defmacro with-slots-set (obj-set-list &rest forms)
  (let* ((cns-var (gentemp)))
    `(let* ((,cns-var ,`(with-set-spec-to-cns (list ,@(loop for lst in obj-set-list
							  collect `(list ,@lst))))))
       (unwind-protect
	   (progn
	     (add-with-set-cns ,cns-var)
	     (progn ,@forms))
	 (remove-dispose-with-set-cns ,cns-var)))
    ))

(defun add-with-set-cns (cns)
  (loop for cn in cns do
    (mg-add-constraint cn)))

(defun remove-dispose-with-set-cns (cns)
  (loop for cn in cns do
	(mg-remove-constraint cn)
	(dispose-variable-input cn)))

(defun with-set-spec-to-cns (spec-list)
  (loop for (obj slot value strength) in spec-list
   collect (create-variable-input
	    (get-object-slot-var obj slot)
	    value
	    (if strength strength *default-input-strength*))))

;; ***** interface to sky-blue add-constraint and remove-constraint *****

(defvar *constraint-hooks* nil)

(defvar *unsatisfied-max-constraint-warning* nil)

;; note: change cn-connection before adding or removing cn, so cn will be
;; marked correctly in snapshot

(defun mg-add-constraint (cn)
  (with-no-invalidation-update
      (when (not (cn-connection-p cn :connected))
	(cerror "cont" "trying to add constraint ~S with connection ~S"
		cn (CN-connection cn)))
    ;; note that cn is in graph, even if it is an unsat req cn
    (setf (CN-connection cn) :graph)
    (sb:add-constraint cn)
    (cond ((and (not (sb:enforced cn))
		(eq sb:*max-strength* (SB:CN-strength cn)))
	   ;; We couldn't enforce a newly-added max constraint.
	   ;; There must be a max-max conflict.  Print msg.
	   ;; note: we don't remove the cn, so it may become satisfied later
	   ;; when another max cn is removed.
	   (when *unsatisfied-max-constraint-warning*
	     (format t "~&Warning: Can't enforce max constraint ~S on object ~S, slot ~S~%"
		     cn (OS-object (CN-os cn)) (OS-slot (CN-os cn)))))))

  ;; update invalid paths/formulas only after adding cn,
  ;; to prevent recursive call to skyblue
  (update-invalidated-paths-and-formulas)

  (when *constraint-hooks*
    (dolist (hook *constraint-hooks*)
      (funcall hook cn :add)))

  cn)

(defun mg-remove-constraint (cn)
  (with-no-invalidation-update
      (when (not (cn-connection-p cn :graph))
	(cerror "cont" "trying to remove constraint ~S with connection ~S"
		cn (CN-connection cn)))
    (setf (CN-connection cn) :connected)
    (sb:remove-constraint cn)
    )
  ;; update invalid paths/formulas only after removing cn, to prevent recursive call to skyblue
  (update-invalidated-paths-and-formulas)
  cn)

;; ***** change-constraint-strength *****

(defun change-constraint-strength (cn strength)
  (let* ((strength-num (sb:get-strength strength)))
    (cond ((not (constraint-p cn))
	   (cerror "noop" "change-constraint-strength: ~S not a constraint" cn))
	  ((eql strength-num (sb:cn-strength cn))
	   ;; don't need to change strength
	   nil)
	  ((not (cn-connection-p cn :graph))
	   ;; cn not in graph -- just change field
	   (setf (sb:cn-strength cn) strength-num))
	  (t
	   (with-no-invalidation-update
	       (sb:change-constraint-strength cn strength-num))
	   ;; update invalid paths/formulas only after changing strength,
	   ;; to prevent recursive call to skyblue
	   (update-invalidated-paths-and-formulas)
	   ))
    strength))

;; ***** experimental fn to access plans *****

(defun create-plan (cns)
  (let* ((plan (sb:extract-plan cns)))
    plan))

(defun valid-plan-p (plan)
  (and (sb:sb-plan-p plan)
       (sb:sb-plan-valid plan)))

(defun run-plan (plan)
  (cond ((valid-plan-p plan)
	 (sb:execute-plan plan)
	 (update-invalidated-paths-and-formulas))
	(t
	 (cerror "noop" "can't run invalidated plan: ~S" plan))))

(defun propagate-plan-from-cn (cn)
  (let* ((plan (sb:get-sb-slot cn :cached-plan)))
    (unless (valid-plan-p plan)
      ;; we have to create the plan
      (setq plan (create-plan (list cn)))
      (sb:set-sb-slot cn :cached-plan plan))
    (run-plan plan)
    cn))

;; execute the selected method, and any downstream mts, for these cns
(defun propagate-from-cns (cns)
  (sb:execute-constraints
   (loop for cn in cns
       when (and (constraint-p cn)
		 (cn-connection-p cn :graph))
       collect cn))
  (update-invalidated-paths-and-formulas))

;; ***** fns to access state of constraints and variables *****

(defun constraint-state (cn)
  (if (constraint-p cn)
    (values (CN-connection cn)
	    (not (null (sb:enforced cn)))
	    (OS-object (CN-os cn))
	    (OS-slot (CN-os cn)))))

(defun variable-state (obj slot)
  (let* ((var (get-object-slot-prop obj slot :sb-variable))
	 (var-p (sb:sb-variable-p var))
	 (valid-p (if var-p (sb:VAR-valid var) nil))
	 (path-slot-p (not (null (get-object-slot-prop obj slot :sb-path-constraints)))))
    (values var-p valid-p path-slot-p)))

;; ***** fns to enable and disable multi-garnet hooks into garnet fns *****

(defvar *fn-to-hook-plist* '(kr::s-value-fn                   s-value-fn-hook
			     kr::kr-call-initialize-method    kr-call-initialize-method-hook
			     kr::kr-init-method               kr-init-method-hook
			     inter::set-obj-list4-slot-no-db  set-obj-list4-slot-no-db-hook
			     kr::destroy-schema               destroy-schema-hook
			     kr::destroy-slot                 destroy-slot-hook
			     kr::propagate-change             propagate-change-hook))

(defun enable-multi-garnet ()
  (loop for (fn hook-fn) on *fn-to-hook-plist* by #'cddr
	do (install-hook fn hook-fn)))

(defun disable-multi-garnet ()
  (loop for (fn hook-fn) on *fn-to-hook-plist* by #'CDDR do
	(uninstall-hook fn)))

(defun multi-garnet-enabled ()
  (loop for (fn hook-fn) on *fn-to-hook-plist* by #'CDDR
      always (eql (symbol-function fn) (symbol-function hook-fn))))


;; ***** fn to update mg slot when var value changed *****
;; defined here because it uses macro set-slot-basic

(defun update-var-value-fn (var slot val)
  (when (and (eql slot :value)
	     (sb:sb-variable-p var))
    (let ((os (VAR-os var)))
      (when (os-p os)
	(with-constants-disabled
	    (set-slot-basic
	     (os-object os) (os-slot os) val
	     :prohibit-constraints t
	     :auto-activate-constraints nil
	     :invalidate-paths t))
	))
    ))

;; (eval-when (:load-toplevel :execute)
;;   (enable-multi-garnet))
