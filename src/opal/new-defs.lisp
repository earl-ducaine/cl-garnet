
(in-package "OPAL")

(defstruct (update-info (:print-function update-info-print-function))
	window
	old-bbox
	bits)

(create-instance 'view-object nil
  :declare ((:type
		   (fixnum :hit-threshold)
		   (known-as-type :known-as)
		   (kr-boolean :visible))
	    (:local-only-slots (:window nil) (:parent nil))))

(create-instance 'graphical-object view-object
  :declare ((:type
	     ((or (is-a-p line-style) null) :line-style)
	     ((or (is-a-p filling-style) null) :filling-style)
	     ((member  :copy :xor :no-op :or :clear :set :copy-inverted
		      :invert :and :equiv :nand :nor :and-inverted
		      :and-reverse :or-inverted :or-reverse)
	      :draw-function))))

(create-instance 'rectangle graphical-object
  :declare ((:update-slots :fast-redraw-p :top)))

(define-method :initialize view-object (gob)
    (s-value gob :update-info (make-update-info)))
