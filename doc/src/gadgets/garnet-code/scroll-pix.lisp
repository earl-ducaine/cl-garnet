(create-instance 'v-scr garnet-gadgets:v-scroll-bar
	(:left 40) (:top 20)
	(:scr-incr 1)
	(:height 250)
	(:page-trill-p NIL)
	(:indicator-text-p NIL))

(create-instance 'h-scr-top garnet-gadgets:h-scroll-bar
	(:left 80) (:top 80)
	(:width 250))

(create-instance 'h-scr-bottom garnet-gadgets:h-scroll-bar
	(:left 80) (:top 160)
	(:width 250)
	(:scr-trill-p NIL)
	(:page-trill-p NIL))

(create-instance 'win inter:interactor-window 
	(:aggregate (create-instance 'agg opal:aggregate)))
(opal:add-component agg v-scr)
(opal:add-component agg h-scr-top)
(opal:add-component agg h-scr-bottom)
(opal:update win)
