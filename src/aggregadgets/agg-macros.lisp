(in-package "OPAL")


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

;; (defmacro remove-interactor (schema &rest args)
;;   `(let ((the-schema ,schema))
;;     (kr-send the-schema :remove-interactor the-schema ,@args)))
