(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 300) (:top 300)
  (:height 75) (:width 200)
  (:background-color opal:motif-gray)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'lab garnet-gadgets:motif-scrolling-labeled-box
  (:left 30) (:top 25))

(opal:add-components agg lab)
(opal:update win T)

  