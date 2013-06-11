(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 150) (:width 200)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'rect1 opal:rectangle
	(:top 30) (:left 20)
	(:height 25) (:width 40)
	(:filling-style opal:gray-fill))

(create-instance 'rect2 opal:rectangle
	(:top 30) (:left 100)
	(:height 55) (:width 50))

(create-instance 'line opal:line
	(:x1 50) (:y1 120)
	(:x2 115) (:y2 95)
	(:line-p T))

(create-instance 'g-sel garnet-gadgets:graphics-selection
	(:start-where (list :element-of agg)))


(opal:add-components agg rect1 rect2 line g-sel)
(opal:update win t)

