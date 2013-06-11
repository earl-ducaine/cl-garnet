(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 100) (:width 200)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'label garnet-gadgets:labeled-box
	(:top 40) (:left 40)
	(:field-font (create-instance NIL opal:font
			(:size :large)))
	(:label-font (create-instance NIL opal:font
			(:size :large)
			(:face :bold))))

(opal:add-component agg label)
(opal:update win t)
