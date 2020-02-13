(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(GVL-SIBLING AGGREGADGET)))

(defmacro gvl-sibling (name &rest slots)
  `(gvl :parent ,name ,@slots))

(defmacro get-name (def)
  `(car ,def))

(defmacro get-class-name (def)
  `(cadr ,def))

(defmacro get-body (def)
  `(cddr ,def))

(create-instance 'opal:aggregadget opal::aggregate
  (:local-only-slots '((:behaviors nil) (:window nil) (:parent nil))))

(defun call-create-instance (class slots agget &key name add-as)
  (unless (schema-p class)
    (error "~A ~A ~A ~A~%?"
	   "Is a comma missing before" class "in declaration of" agget))
  (let ((obj (kr::begin-create-instance NIL class :name-prefix name)))
    (dolist (pair slots)
      (if (consp pair)
	  (let ((val-list (cdr pair))
		(slot (car pair)))
	    (if (cdr val-list)
		;; Multiple values were supplied
		(s-value obj slot val-list)
		;; A single value was supplied.
		(s-value obj slot (car val-list))))))
    (when name (s-value obj :known-as name))
    (case add-as
      (:component (with-constants-disabled (add-local-component agget obj)))
      (:interactor
		   ))
    ;; The following instruction would have been done during the add, but
    ;; constants were disabled.
    (when (and add-as name) (declare-constant agget name))
    (kr::end-create-instance obj)
    obj
    ))

(defvar *inherit-formula* (o-formula (gv (car (gvl :parent :is-a))
					 (gvl :known-as) kr::*schema-slot*)))

(defun get-inherited-value (quasi-prototype inherited-slot)
  (let ((inherited-formula (get-value quasi-prototype inherited-slot)))
    (if (formula-p inherited-formula)
	(formula inherited-formula)
        (formula *inherit-formula*))))

(defun inherit-values (slots agg proto-component-name)
  (do ((slots-aux slots (cdr slots-aux)))
      ((null slots-aux))
    (let ((pair (car slots-aux)))
      (if (eq pair :inherit) ; the unusual case that :inherit was specified
	  (let ((inherited-slots (second slots-aux))
		(quasi-prototype (g-value (car (g-value agg :is-a))
					  proto-component-name)))
	    (setf slots (remove :inherit slots))
	    (setf slots (remove inherited-slots slots))
	    (dolist (inherited-slot inherited-slots)
	      (push (list inherited-slot (get-inherited-value quasi-prototype
							      inherited-slot))
		    slots))))))
  slots)

(defun is-first-comp-in-parts-list (components parts-list)
  (if components
      (let ((first-comp (g-value (first components) :known-as)))
	(member first-comp parts-list
		:test #'(lambda (fc part)
			  (let ((name (if (listp part)
					  (get-name part)
					  part)))
			    (if (numberp name)
				(eql 0 name)
				(eq fc name))))))))

(defun create-part (name class slots agget)
  (unless (schema-p class)
    (error "~A ~A ~A ~A~%   ~A ~A?"
	   "Is a comma missing before" class "in declaration of part" name
	   "of aggregadget" agget))
  ;; Take out :inherit keyword and install "inherited" formulas in slot list
  (setf slots (inherit-values slots agget name))
  ;; Add a few slots to the slots list
  (let (new-comp)
    (let ((kr::*redefine-ok* T))
      (setf new-comp (call-create-instance class slots agget
					   :name name
					   :add-as :component)))
    ;; Make :parent and :known-as constant slot
    (when (g-value agget :parts)
      (declare-constant new-comp :parent)
      (declare-constant new-comp :known-as))))

(defun get-part-from-function (part-name agget part-function)
  (let ((part (funcall part-function agget)))
    (with-constants-disabled
      (unless (numberp part-name) (s-value part :known-as part-name))
      (s-value part :*special-creator* part-function)
      (add-local-component agget part))
    (unless (numberp part-name) (declare-constant agget part-name))
    (declare-constant part :parent)
    (declare-constant part :known-as)))


(defun get-parts-from-function (agget part-function)
  (multiple-value-bind (components names)
		       (funcall part-function agget)
    (if names
	;; the function did return names for the parts
	(do ((components-list components (cdr components-list))
	     (names-list names (cdr names-list)))
	    ((or (null components-list) (null names-list)))
	  (let ((this-part (car components-list))
		(this-part-name (car names-list)))
	    (when this-part-name
	      (s-value this-part :known-as this-part-name))
	    (let ((kr::*constants-disabled* T))
	      (add-local-component agget this-part))))
	;; the function did not return names for the parts
	(dolist (new-component components)
	  (let ((kr::*constants-disabled* T))
	    (add-local-component agget new-component))))))

(defun get-name-and-protopart-from-rank (rank prototype agget
					 &optional NIL-name-OK)
  (let ((protopart (nth rank (g-local-value prototype :components))))
    (if protopart
	(values (g-value protopart :known-as) protopart)
	(unless NIL-name-OK
	  (error "Error while making parts of ~S:
Could not find component of rank ~S in prototype.~%" agget rank)))))

(defun get-inters-from-function (agget inter-function)
  (multiple-value-bind (inters names)
		       (funcall inter-function agget)
    (if names
	;; the function did returned names for the inters
	(do ((inters-list inters (cdr inters-list))
	     (names-list names (cdr names-list)))
	    ((or (null inters-list) (null names-list)))
	  (let ((this-inter (car inters-list))
		(this-inter-name (car names-list)))
	    (s-value agget :behaviors
                     (nconc (g-local-value agget :behaviors)
                            (list this-inter)))
	    (s-value this-inter :operates-on agget)
	    (when this-inter-name    ; the current inter has a name
	      (s-value agget this-inter-name this-inter)
	      (s-value this-inter :known-as this-inter-name))))
	;; the function did not return names for the inters
	(dolist (new-inter inters)
	  (s-value agget :behaviors
                   (nconc (g-local-value agget :behaviors) (list new-inter)))
	  (s-value new-inter :operates-on agget)))))

(define-method :initialize aggregadget (agget)
  (call-prototype-method agget)
  (let ((prototype (car (g-local-value agget :is-a)))
	(parts-list (g-local-value agget :parts))
	(inter-list (g-local-value agget :interactors)))
    (if (or (null parts-list)
	    (not (is-first-comp-in-parts-list
		  (g-local-value prototype :components) parts-list)))
	nil)
    (if (or (null inter-list)
	    (not (is-first-comp-in-parts-list  ; use same fn for inters
		  (g-local-value prototype :behaviors) inter-list)))
	nil
	)
    (if (or parts-list (g-value agget :parts))
	(declare-constant agget :components))))


(define-method :add-component aggregadget (agg element &rest args)
  (let (where locator known-as)
    (cond ((eq (first args) :where)
	   (setq where (second args))
	   (setq locator (third args)))
	  ((first args)
	   (setq where (first args))
	   (setq locator (second args)))
	  (t
	   (setq where :tail)))
    ;; first add to prototype
    (add-local-component agg element where locator)
    ;; now do instances
    (setf known-as (g-local-value element :known-as))
    (dolist (agg-instance (g-local-value agg :is-a-inv))
      (let ((element-instance (create-instance nil element))
	    (my-where where)
	    my-locator)
	(s-value element-instance :known-as known-as)
	(cond ((member where '(:front :tail :back :head)))
	      ((member where '(:behind :before :in-front :after))
	       )
	      (t (setf my-locator locator)))
	(add-component agg-instance element-instance my-where my-locator)))))

(define-method :add-local-component opal:aggregadget
  (agg gob &optional key where loc)
  (let ((name (g-local-value gob :known-as)))
    (when name
      (let ((kr::*constants-disabled* T))
	(s-value agg name gob))
      (if (g-value agg :parts)
	  (declare-constant agg name)))
    (kr-send opal::aggregate :add-component agg gob key where loc)))
