(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
	(:top 100) (:left 300)
	(:height 170) (:width 300)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'left-menu garnet-gadgets:scrolling-menu
	(:left 20) (:top 20)
	(:title "Menu")
	(:items '("Geneva" "Times" "Roman" "Courier" "Helvetica")))

(create-instance 'right-menu garnet-gadgets:scrolling-menu
	(:left 150) (:top 20)
	(:title "Scrolling Menu")
	(:title-font (create-instance NIL opal:font
			(:family :serif)
			(:face :italic)))
	(:v-spacing 7)
	(:page-trill-p NIL)
	(:scroll-on-left-p NIL)
	(:items '("Item 1" "Item 2" "Item 8" "Item 9" "Item 10"
			"Item 11" "Item 12" "Item 13")))

(opal:add-component agg left-menu)
(opal:add-component agg right-menu)
(opal:update win t)
