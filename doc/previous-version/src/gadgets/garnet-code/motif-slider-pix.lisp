(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:top 300) (:left 300)
  (:width 120) (:height 200)
  (:aggregate (create-instance 'agg opal:aggregate))
  (:background-color opal:motif-gray))

(create-instance 'slide-1 garnet-gadgets:motif-slider
  (:top 15) (:left 5)
  (:height 170)
  (:val-1 0) (:val-2 10))

(create-instance 'slide-2 garnet-gadgets:motif-slider
  (:left 80) (:top 15)
  (:height 170)
  (:scr-trill-p T) (:indicator-text-p NIL))

(opal:add-components agg slide-1 slide-2)
(opal:update win T)