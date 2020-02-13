(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(GVL-SIBLING AGGREGADGET)))

(defmacro get-name (def)
  `(car ,def))

(defmacro get-class-name (def)
  `(cadr ,def))

(defmacro get-body (def)
  `(cddr ,def))

(create-instance 'opal:aggregadget opal::aggregate
  (:local-only-slots '((:behaviors nil) (:window nil) (:parent nil))))

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
