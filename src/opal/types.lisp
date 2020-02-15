
(in-package :kr)

(def-kr-type ITEMS-TYPE () 'list
  "[list of items: (\"Label2\"...)]")

(def-kr-type ACCELERATORS-TYPE () 'list
  "[list of lists: ((#\r \"Alt-r\" #\meta-r)...)]")

(def-kr-type KNOWN-AS-TYPE () '(or keyword null)
  "[keyword]")

(def-kr-type DRAW-FUNCTION ()
  '(member :copy :xor :no-op :or :clear :set
    :copy-inverted :invert :and :equiv :nand :nor
    :and-inverted :and-reverse :or-inverted :or-reverse)
  "[one of :COPY, :XOR, :NO-OP, :OR, :CLEAR, :SET,
                :COPY-INVERTED, :INVERT, :AND, :EQUIV, :NAND, :NOR,
                :AND-INVERTED, :AND-REVERSE, :OR-INVERTED, or :OR-REVERSE]")

(def-kr-type LINE-STYLE-OR-NIL ()
  '(or (is-a-p opal::line-style) null)
  "[either an instance of opal:LINE-STYLE or NIL]")

(def-kr-type FILLING-STYLE-OR-NIL ()
  '(or (is-a-p opal::filling-style) null)
  "[either an instance of opal:FILLING-STYLE or NIL]")

(defun fixnump (object) (typep object 'fixnum))

(def-kr-type fixnum ()
  '(satisfies sb-int:fixnump)
  "Potential efficiency hack.")

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
