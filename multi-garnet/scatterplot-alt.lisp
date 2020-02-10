(in-package :kr)

;; (create-instance '*axis-rectangle* Opal::Rectangle
;;   (:left-cn (m-constraint :max (box left) (setf left (first box))))
;;   (:top-cn (m-constraint :max (box top) (setf top (second box))))
;;   (:width-cn (m-constraint :max (box width) (setf width (third box))))
;;   (:height-cn (m-constraint :max (box height) (setf height (fourth box)))))


(CREATE-SCHEMA '*AXIS-RECTANGLE* :GENERATE-INSTANCE (:IS-A OPAL::RECTANGLE)
                (:LEFT-CN
                 (M-CONSTRAINT :MAX (BOX LEFT) (SETF LEFT (FIRST BOX))))
                (:TOP-CN (M-CONSTRAINT :MAX (BOX TOP) (SETF TOP (SECOND BOX))))
                (:WIDTH-CN
                 (M-CONSTRAINT :MAX (BOX WIDTH) (SETF WIDTH (THIRD BOX))))
                (:HEIGHT-CN
                 (M-CONSTRAINT :MAX (BOX HEIGHT) (SETF HEIGHT (FOURTH BOX)))))

(DO-SCHEMA-BODY (MAKE-A-NEW-SCHEMA '*V-AXIS*) OPAL:AGGREGADGET T T NIL NIL
                (CONS :PARTS `((:MAIN ,*AXIS-RECTANGLE*))))
