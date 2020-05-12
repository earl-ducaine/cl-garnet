(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 220) (:width 500)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'pres-gauge garnet-gadgets:gauge
	(:left 30) (:top 30)
	(:title "Pressure"))

(create-instance 'temp-gauge garnet-gadgets:gauge
	(:left 275) (:top 55)
	(:title "Temperature")
	(:val-1 0) (:val-2 90)
	(:polygon-needle-p NIL)
	(:width 175))

(opal:add-component agg pres-gauge)
(opal:add-component agg temp-gauge)
(opal:update win)
