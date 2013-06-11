(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 600) (:top 300)
  (:width 325) (:height 250)
  (:aggregate (create-instance 'agg opal:aggregate))
  (:background-color opal:motif-gray))

(create-instance 'v-scr garnet-gadgets:motif-v-scroll-bar
  (:left 40) (:top 30)
  (:height 190))

(create-instance 'h-scr garnet-gadgets:motif-h-scroll-bar
  (:left 80) (:top 60)
  (:width 220) (:percent-visible 1))

(create-instance 'h-scr2 garnet-gadgets:motif-h-scroll-bar
  (:left 80) (:top 160)
  (:width 220) (:scr-trill-p NIL))

(opal:add-components agg v-scr h-scr h-scr2)
(opal:update win T)
