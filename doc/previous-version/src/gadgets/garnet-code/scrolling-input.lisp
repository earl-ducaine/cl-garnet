(when (boundp 'win) (opal:destroy win))

(garnet-load "gadgets:scrolling-input-string-loader")

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 50) (:width 160)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'scr-inp-str garnet-gadgets:scrolling-input-string
	(:top 17) (:left 40))

(opal:add-component agg scr-inp-str)
(opal:update win t)
