(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:left 300) (:top 300)
	(:height 150) (:width 250)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'propforobj garnet-gadgets:prop-sheet-for-obj
	(:obj (create-instance 'rect opal:rectangle
		(:top 10) (:left 175)
		(:height 50) (:width 50)
		(:filling-style opal:gray-fill)
		(:line-style opal:line-2)))
	(:slots
		`(:left :top :width :height
		  :line-style :filling-style)))

(opal:add-components agg rect propforobj)
(opal:update win t)
