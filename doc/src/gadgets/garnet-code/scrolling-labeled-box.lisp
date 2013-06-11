(when (boundp 'win) (opal:destroy win))

(garnet-load "gadgets:scrolling-labeled-box-loader")

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 55) (:width 220)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'scr-lab-box garnet-gadgets:scrolling-labeled-box
	(:top 20) (:left 40))

(opal:add-component agg scr-lab-box)
(opal:update win t)
