(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 150) (:width 180)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'white-rect opal:rectangle
	(:left 15) (:top 10)
	(:height 65) (:width 75))

(create-instance 'gray-rect opal:rectangle
	(:left 110) (:top 85)
	(:height 55) (:width 60)
	(:filling-style opal:gray-fill))
 
(create-instance 'line opal:line
	(:x1 25) (:y1 130)
	(:x2 160) (:y2 39)
	(:line-p t))

(create-instance 'mult-sel garnet-gadgets:multi-graphics-selection
	(:start-where (list :element-of agg)))

(opal:add-components agg white-rect gray-rect line mult-sel)
(opal:update win t)

