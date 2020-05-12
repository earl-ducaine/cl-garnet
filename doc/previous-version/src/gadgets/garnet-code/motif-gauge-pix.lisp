(if (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 300) (:top 300)
  (:height 150) (:width 250)
  (:aggregate (create-instance 'agg opal:aggregate))
  (:background-color opal:motif-gray))

(create-instance 'gauge garnet-gadgets:motif-gauge
  (:left 15) (:top 0) 
  (:width 220) (:title "Temperature"))

(opal:add-components agg gauge)
(opal:update win T)
