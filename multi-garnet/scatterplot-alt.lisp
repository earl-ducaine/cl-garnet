(in-package :multi-garnet)

(create-instance '*axis-rectangle* Opal:Rectangle
  (:left-cn (m-constraint :max (box left) (setf left (first box))))
  (:top-cn (m-constraint :max (box top) (setf top (second box))))
  (:width-cn (m-constraint :max (box width) (setf width (third box))))
  (:height-cn (m-constraint :max (box height) (setf height (fourth box)))))

(create-instance '*v-axis* opal:aggregadget
  (:parts `((:main ,*axis-rectangle*))))
