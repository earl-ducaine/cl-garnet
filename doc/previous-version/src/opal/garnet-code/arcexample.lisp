(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'myrect opal:rectangle
  (:left 10) (:top 10)
  (:width 100) (:height 50))

(create-instance 'myarc opal:arc
  (:left 10) (:top 10)
  (:width 100) (:height 50)
  (:angle1 (/ PI 4))
  (:angle2 (/ PI 2))
  (:line-style opal:line-2)
  (:filling-style opal:light-gray-fill))

(opal:add-components agg myarc myrect)
(opal:update win T)
