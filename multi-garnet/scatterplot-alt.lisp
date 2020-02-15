(in-package :kr)

(do-schema-body-alt
    (make-a-new-schema '*axis-rectangle*) opal::rectangle
    (cons :height-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:height))
	   :variable-names '(box height)))
    (cons :width-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:width))
	   :variable-names
	   '(box width)))
    (cons :top-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:top))
	   :variable-names '(box top)))
    (cons :left-cn
	  (create-mg-constraint
	   :variable-paths '((:box) (:left))
	   :variable-names '(box left))))
