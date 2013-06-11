(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 300) (:top 300)
  (:height 220) (:width 300)
  (:background-color opal:motif-gray)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'text-butt garnet-gadgets:motif-text-button-panel
  (:left 75) (:top 20)
  (:items '("Red" "Green" "Blue")))

(create-instance 'check-butt garnet-gadgets:motif-check-button-panel
  (:left 175) (:top 30)
  (:items '("Bold" "Italic" "Underline")))

(create-instance 'rad-butt garnet-gadgets:motif-radio-button-panel
  (:left 75) (:top 130)
  (:items '("Helvetica" "Geneva" "Courier")))

(create-instance 'rad-butt-2 garnet-gadgets:motif-radio-button-panel
  (:left 175) (:top 130)
  (:items '("Roman" "Times" "Symbol")))

(opal:add-components agg text-butt check-butt rad-butt rad-butt-2)
(opal:update win T)
