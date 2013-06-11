(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 50) (:top 600)
  (:height 75) (:width 175)
  (:aggregate (create-instance 'agg opal:aggregate)))

(opal:add-component agg (create-instance NIL opal:polyline
			  (:point-list '(10 50 50 10 90 10 130 50))
			  (:filling-style opal:light-gray-fill)
			  (:line-style opal:line-4)))
(opal:update win)
