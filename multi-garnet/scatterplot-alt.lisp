(in-package :multi-garnet)

(create-instance '*axis-rectangle* Opal:Rectangle
  (:line-style Opal:thin-line)
  (:filling-style Opal:black-fill)
  (:box (list 100 100 100 100))
  (:left 100)
  (:top 100)
  (:width 100)
  (:height 100)
  ;; one-directional constraints unpacking :box
  (:left-cn (m-constraint :max (box left) (setf left (first box))))
  (:top-cn (m-constraint :max (box top) (setf top (second box))))
  (:width-cn (m-constraint :max (box width) (setf width (third box))))
  (:height-cn (m-constraint :max (box height) (setf height (fourth box)))))

(create-instance '*v-axis* opal:aggregadget
  (:parts `((:main ,*axis-rectangle*))))

(create-instance '*v-axis* opal:aggregadget
  (:parts `((:main ,*axis-rectangle*))))

;; (CREATE-SCHEMA '*V-AXIS* :GENERATE-INSTANCE (:IS-A OPAL:AGGREGADGET)
;;                 (:PARTS `((:MAIN ,*AXIS-RECTANGLE*))))

;; (KR::DO-SCHEMA-BODY (KR::MAKE-A-NEW-SCHEMA '*V-AXIS*) OPAL:AGGREGADGET T T NIL
;;                     NIL (CONS :PARTS `((:MAIN ,*AXIS-RECTANGLE*))))

;; (kr-init-method (KR::MAKE-A-NEW-SCHEMA '*V-AXIS*) nil)
