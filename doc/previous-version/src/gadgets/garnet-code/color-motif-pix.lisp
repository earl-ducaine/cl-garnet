(when (boundp 'win) (opal:destroy win))

(let ((kr::*constants-disabled* T))
  (s-value opal:color :color-p NIL))

(garnet-load "gadgets:motif-v-scroll-loader")
(garnet-load "gadgets:motif-text-buttons-loader")
(garnet-load "gadgets:motif-radio-buttons-loader")
(garnet-load "gadgets:motif-gauge-loader")

(create-instance 'WIN inter:interactor-window
  (:top 300) (:left 300)
  (:height 200) (:width 250)
  (:aggregate (create-instance 'agg opal:aggregate))
;  (:background-color opal:motif-gray)
  )

(create-instance 'vert-scr garnet-gadgets:motif-v-scroll-bar
  (:top 15) (:left 15)
  (:height 175))

(create-instance 'rad-butt garnet-gadgets:motif-radio-button-panel
  (:left 70) (:top 125)
  (:items '("Radio 1" "Radio 2" "Radio 3")))

(create-instance 'text-butt garnet-gadgets:motif-text-button-panel
  (:left 175) (:top 125)
  (:items '("Text 1" "Text 2")))

(create-instance 'gauge garnet-gadgets:motif-gauge
  (:left 60) (:top 10)
  (:width 175) (:value-feedback-p NIL)
  (:num-marks 7))

(opal:add-components agg vert-scr rad-butt text-butt gauge)
(opal:update win T)