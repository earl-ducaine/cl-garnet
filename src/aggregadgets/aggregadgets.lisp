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
