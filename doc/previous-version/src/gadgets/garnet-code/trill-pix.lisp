(create-instance 'win inter:interactor-window
	(:aggregate (create-instance 'agg opal:aggregate))
	(:left 100) (:top 50)
	(:width 240) (:height 100))

(create-instance 'trill garnet-gadgets:trill-device
	(:left 60) (:top 40))

(opal:add-component agg trill)
(opal:update win)
