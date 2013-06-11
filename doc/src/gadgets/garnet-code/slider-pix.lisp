(create-instance 'v-slid garnet-gadgets:v-slider
	(:left 40) (:top 20)
	(:height 250))

(create-instance 'h-slid-top garnet-gadgets:h-slider
	(:left 120) (:top 80)
	(:width 200)
	(:num-marks 6)
	(:val-1 0) (:val-2 50)
	(:scr-trill-p NIL) (:page-trill-p NIL))

(create-instance 'h-slid-bottom garnet-gadgets:h-slider
	(:left 120) (:top 160)
	(:width 200)
	(:enumerate-p NIL)
	(:tic-marks-p NIL)
	(:value-feedback-p NIL))

(create-instance 'win inter:interactor-window 
	(:aggregate (create-instance 'agg opal:aggregate)))
(opal:add-component agg v-slid)
(opal:add-component agg h-slid-top)
(opal:add-component agg h-slid-bottom)
(opal:update win)
