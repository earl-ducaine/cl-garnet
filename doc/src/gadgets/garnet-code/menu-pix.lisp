(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 150) (:width 250)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'my-menu garnet-gadgets:menu
	(:left 100) (:top 20)
	(:items '("Cut" "Copy" "Paste" "Undo"))
	(:title "Menu"))

(opal:add-component agg my-menu)
(opal:update win t)
