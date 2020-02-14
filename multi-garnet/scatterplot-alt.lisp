(in-package :kr)

(do-schema-body-alt (make-a-new-schema '*axis-rectangle*) opal::rectangle
		    (cons :height-cn
			  (m-constraint :max (box height)
					(setf height (fourth box))))
		    (cons :width-cn
			  (m-constraint :max (box width) (setf width (third box))))
		    (cons :top-cn
			  (m-constraint :max (box top) (setf top (second box))))
		    (cons :left-cn
			  (m-constraint :max (box left) (setf left (first box)))))

(do-schema-body-alt (make-a-new-schema '*v-axis*) opal:aggregadget
		    (cons :parts `((:main ,*axis-rectangle*))))
