(in-package "OPAL")

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
