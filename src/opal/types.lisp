
(in-package :kr)

(def-kr-type ITEMS-TYPE () 'list
  "[list of items: (\"Label2\"...)]")

(def-kr-type ACCELERATORS-TYPE () 'list
  "[list of lists: ((#\r \"Alt-r\" #\meta-r)...)]")

(def-kr-type KNOWN-AS-TYPE () '(or keyword null)
  "[keyword]")

(def-kr-type FILENAME-TYPE () '(or string pathname)
  "[file namestring]")

(def-kr-type PATHNAME () 'pathname
  "[pathname]")

(def-kr-type FONT-FAMILY ()
  '(MEMBER :FIXED :SERIF :SANS-SERIF)
  "[one of :FIXED, :SERIF, or :SANS-SERIF]")

(def-kr-type FONT-FACE ()
  '(or (member :roman :bold :italic :bold-italic) list)
  "[one of :ROMAN, :BOLD, :ITALIC, or :BOLD-ITALIC or a list]")

(def-kr-type FONT-SIZE ()
  '(member :small :medium :large :very-large)
  "[one of :SMALL, :MEDIUM, :LARGE, or :VERY-LARGE]")

(def-kr-type FILL-STYLE ()
  '(member :solid :stippled :opaque-stippled)
  "[one of :SOLID, :STIPPLED, or :OPAQUE-STIPPLED]")

(def-kr-type DRAW-FUNCTION ()
  '(member :copy :xor :no-op :or :clear :set
    :copy-inverted :invert :and :equiv :nand :nor
    :and-inverted :and-reverse :or-inverted :or-reverse)
  "[one of :COPY, :XOR, :NO-OP, :OR, :CLEAR, :SET,
                :COPY-INVERTED, :INVERT, :AND, :EQUIV, :NAND, :NOR,
                :AND-INVERTED, :AND-REVERSE, :OR-INVERTED, or :OR-REVERSE]")

(def-kr-type H-ALIGN ()
  '(member :left :center :right)
  "[one of :LEFT, :CENTER, or :RIGHT]")

(def-kr-type V-ALIGN ()
  '(member :top :center :bottom)
  "[one of :TOP, :CENTER, or :BOTTOM]")

(def-kr-type DIRECTION ()
  '(member :vertical :horizontal)
  "[either :VERTICAL or :HORIZONTAL]")

(def-kr-type DIRECTION-OR-NIL ()
  '(or (member :vertical :horizontal) null)
  "[either :VERTICAL, :HORIZONTAL, or NIL]")

(def-kr-type COLOR ()
  '(is-a-p opal::color)
  "[an instance of opal:COLOR]")

(def-kr-type COLOR-OR-NIL ()
  '(or null (is-a-p opal::color))
  "[either an instance of opal:COLOR or NIL]")

(def-kr-type BITMAP ()
  '(is-a-p opal::bitmap)
  "[an instance of opal:BITMAP]")

(def-kr-type BITMAP-OR-NIL ()
  '(or null (is-a-p opal::bitmap))
  "[either an instance of opal:BITMAP or NIL]")

(def-kr-type LINE-STYLE ()
  '(is-a-p opal::line-style)
  "[an instance of opal:LINE-STYLE]")

(def-kr-type LINE-STYLE-OR-NIL ()
  '(or (is-a-p opal::line-style) null)
  "[either an instance of opal:LINE-STYLE or NIL]")

(def-kr-type FILLING-STYLE ()
  '(is-a-p opal::filling-style)
  "[an instance of opal:FILLING-STYLE]")

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
