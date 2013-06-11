(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:left 300) (:top 100)
	(:height 150) (:width 200)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'aro garnet-gadgets:arrow-line
	(:x1 30) (:y1 50)
	(:x2 170) (:y2 50)
	(:filling-style opal:black-fill))

(create-instance 'double-aro garnet-gadgets:double-arrow-line
	(:x1 30) (:y1 100)
	(:x2 170) (:y2 100)
	(:open-p NIL))

(opal:add-components agg aro double-aro)
(opal:update win T)
