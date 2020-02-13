
(in-package :opal)

(create-instance 'line-style nil
  (:constant T))

(create-instance 'DEFAULT-LINE-STYLE line-style
  (:constant T))


(create-instance 'VIEW-OBJECT NIL
  :declare ((:type (fixnum :left :top)
		   (fixnum :width :height :hit-threshold)
		   (known-as-type :known-as)
		   (kr-boolean :visible))
	    (:update-slots :visible :fast-redraw-p)
	    (:local-only-slots (:window nil) (:parent nil))
	    (:sorted-slots :is-a :left :top :width :height :visible :line-style
			   :filling-style :draw-function :components :parent)
	    (:ignored-slots :depended-slots :update-slots :update-slots-values)
	    )
  (:left 0)
  (:top 0)
  (:width 0)
  (:height 0)
  (:hit-threshold 0)
  (:limit-values '((:is-a-inv 5)))
  (:global-limit-values 5))

(create-instance 'AGGREGATE view-object
  :declare (:type (list :components)
		  (fixnum :left :top :width :height))
  (:components)
  (:update-slots NIL)
  )

(create-instance 'GRAPHICAL-OBJECT view-object
  :declare ((:type (fixnum :top :left :width :height)
		   ((or (is-a-p line-style) null) :line-style)
		   ((or (is-a-p filling-style) null) :filling-style)
		   ((member :copy :xor :no-op :or :clear :set :copy-inverted
			    :invert :and :equiv :nand :nor :and-inverted
			    :and-reverse :or-inverted :or-reverse)
		    :draw-function))
	    (:update-slots :visible :fast-redraw-p :line-style :filling-style
			   :draw-function))
  (:top 0)
  (:left 0)
  (:width 20)
  (:height 20)
  (:draw-function :copy)
  (:line-style default-line-style)
  (:filling-style nil)
  (:select-outline-only nil))

(create-instance 'RECTANGLE graphical-object
  :declare ((:parameters :left :top :width :height :line-style :filling-style
			 :draw-function :visible)
	    (:type (fixnum :left :top :width :height))
	    (:maybe-constant :left :top :width :height :line-style
			     :filling-style :draw-function :visible)
	    (:update-slots :visible :fast-redraw-p :top :left :width :height
			   :line-style :filling-style :draw-function))
  )
