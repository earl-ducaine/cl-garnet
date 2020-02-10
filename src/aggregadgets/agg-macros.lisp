(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(add-local-component add-local-interactor remove-local-component
	    remove-local-interactor add-local-item
	    remove-local-item remove-nth-item remove-nth-component
	    notice-items-changed add-interactor remove-interactor
	    take-default-component
	    ;;replace-item-prototype-object
	    )))


(defmacro add-local-component (schema &rest args)
  `(kr-send ,schema :add-local-component ,schema ,@args))

(defmacro add-local-interactor (schema &rest args)
  `(kr-send ,schema :add-local-interactor ,schema ,@args))

(defmacro remove-local-component (schema &rest args)
  `(kr-send ,schema :remove-local-component ,schema ,@args))

(defmacro remove-local-interactor (schema &rest args)
  `(kr-send ,schema :remove-local-interactor ,schema ,@args))

(defmacro add-local-item (schema &rest args)
  `(kr-send ,schema :add-local-item ,schema ,@args))

(defmacro remove-local-item (schema &rest args)
  `(kr-send ,schema :remove-local-item ,schema ,@args))

(defmacro remove-nth-item (schema n)
  `(kr-send ,schema :remove-nth-item ,schema ,n))

(defmacro remove-nth-component (schema n)
  `(kr-send ,schema :remove-nth-component ,schema ,n))

(defmacro notice-items-changed (agg &optional no-propagation)
  `(kr-send ,agg :notice-items-changed ,agg ,no-propagation))

(defmacro add-interactor (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :add-interactor the-schema ,@args)))

(defmacro remove-interactor (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :remove-interactor the-schema ,@args)))

(defmacro take-default-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :take-default-component the-schema ,@args)))

;; (defmacro replace-item-prototype-object (schema &rest args)
;;   `(let ((the-schema ,schema))
;;     (kr-send the-schema :replace-item-prototype-object the-schema ,@args)))
