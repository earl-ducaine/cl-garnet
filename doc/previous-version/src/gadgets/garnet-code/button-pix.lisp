(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:width 500) (:height 240)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'text-butt garnet-gadgets:text-button-panel
	(:top 10) (:left 20)
	(:items '("Bach" "Beethoven" "Mozart" "Strauss")))

(create-instance 'x-butt garnet-gadgets:x-button-panel
	(:top 125) (:left 200)
	(:items '("Bold" "Italic" "Underline")))

(create-instance 'radio-butt garnet-gadgets:radio-button-panel
	(:top 15) (:left 150)
	(:direction :horizontal)
	(:items '("Helvetica" "Geneva" "Courier")))

(create-instance 'radio-butt-2 garnet-gadgets:radio-button-panel
	(:top 45) (:left 150)
	(:fixed-width-size 96)
	(:direction :horizontal)
	(:items '("Roman" "Times" "Symbol")))
 
(opal:add-component agg text-butt)
(opal:add-component agg x-butt)
(opal:add-component agg radio-butt)
(opal:add-component agg radio-butt-2)
(opal:update win T)
