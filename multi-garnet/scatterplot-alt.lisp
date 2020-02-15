(in-package :kr)

(do-schema-body-alt (make-a-new-schema '*axis-rectangle*) opal::rectangle
		    (cons :height-cn
			  (CREATE-MG-CONSTRAINT :STRENGTH :MAX :METHODS
                      (LIST
                       (CREATE-MG-METHOD :OUTPUT-INDICES '(1) :CODE
                                         #'(LAMBDA (CN) NIL)))
                      :VARIABLE-PATHS '((:BOX) (:HEIGHT)) :VARIABLE-NAMES
                      '(BOX HEIGHT)))
		    (cons :width-cn
			  (CREATE-MG-CONSTRAINT :STRENGTH :MAX :METHODS
                      (LIST
                       (CREATE-MG-METHOD :OUTPUT-INDICES '(1) :CODE
                                         #'(LAMBDA (CN) NIL)))
                      :VARIABLE-PATHS '((:BOX) (:WIDTH)) :VARIABLE-NAMES
                      '(BOX WIDTH)))
		    (cons :top-cn
			  (CREATE-MG-CONSTRAINT :STRENGTH :MAX :METHODS
                      (LIST
                       (CREATE-MG-METHOD :OUTPUT-INDICES '(1) :CODE
                                         #'(LAMBDA (CN) NIL)))
                      :VARIABLE-PATHS '((:BOX) (:TOP)) :VARIABLE-NAMES
                      '(BOX TOP)))
		    (cons :left-cn
			  (CREATE-MG-CONSTRAINT :STRENGTH :MAX :METHODS
                      (LIST
                       (CREATE-MG-METHOD :OUTPUT-INDICES '(1) :CODE
                                         #'(LAMBDA (CN) NIL)))
                      :VARIABLE-PATHS '((:BOX) (:LEFT)) :VARIABLE-NAMES
                      '(BOX LEFT))))

(do-schema-body-alt (make-a-new-schema '*v-axis*) opal:aggregadget
		    (cons :parts `((:main ,*axis-rectangle*))))
