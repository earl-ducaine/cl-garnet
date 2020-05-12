(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 900) (:top 300)
  (:height 40) (:width 235)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'MY-AGG opal:aggrelist
   (:left 10) (:top 10)
   (:direction :horizontal)
   (:items '("This " "is " "an " "example"))
   (:item-prototype
    `(,opal:text
      (:string ,(formula '(nth (gvl :rank) (gvl :parent :items))))
      (:font ,(create-instance NIL opal:font
		 (:size :large))))))

(opal:add-components agg my-agg)
(opal:update win T)


