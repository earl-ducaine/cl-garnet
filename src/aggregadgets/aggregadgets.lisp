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
