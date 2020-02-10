(in-package :kr)

(DO-SCHEMA-BODY-alt (MAKE-A-NEW-SCHEMA '*AXIS-RECTANGLE*) OPAL::RECTANGLE T T NIL
                NIL
                (CONS :HEIGHT-CN
                      (M-CONSTRAINT :MAX (BOX HEIGHT)
                                    (SETF HEIGHT (FOURTH BOX))))
                (CONS :WIDTH-CN
                      (M-CONSTRAINT :MAX (BOX WIDTH) (SETF WIDTH (THIRD BOX))))
                (CONS :TOP-CN
                      (M-CONSTRAINT :MAX (BOX TOP) (SETF TOP (SECOND BOX))))
                (CONS :LEFT-CN
                      (M-CONSTRAINT :MAX (BOX LEFT) (SETF LEFT (FIRST BOX)))))

(DO-SCHEMA-BODY-alt (MAKE-A-NEW-SCHEMA '*V-AXIS*) OPAL:AGGREGADGET T T NIL NIL
                (CONS :PARTS `((:MAIN ,*AXIS-RECTANGLE*))))
