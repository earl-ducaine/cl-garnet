
(defvar db-init
  (progn
    (garnet-load "gadgets:error-gadget-loader")
    (garnet-load "gadgets:motif-error-gadget-loader")
    (garnet-load "gadgets:motif-prop-sheet-win-loader")
    (garnet-load "gadgets:motif-save-gadget-loader")))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
  (:left 500) (:top 10)
  (:width 500) (:height 500))
(opal:update WIN)

(create-instance 'EG gg:error-gadget
  (:window-left 0) (:window-top 0)
  (:string "There was an error")
  (:parent-window WIN))
(gg:display-error EG)

(create-instance 'MQG gg:motif-query-gadget
  (:window-left (+ 2 (g-value EG :window :width)))
  (:window-top 0)
  (:foreground-color opal:motif-gray)
  (:parent-window WIN))
(gg:display-query MQG)

(create-instance 'SGAD gg:motif-save-gadget
  (:parent-window WIN)
  (:window-left 0)
  (:window-top (+ 2 (g-value EG :window :height))))
(gg:display-save-gadget SGAD)

(create-instance 'PS gg:motif-prop-sheet-for-obj-with-ok
  (:left 5) (:top 5)
  (:obj opal:rectangle)
  (:slots `(:left :top :width :height
	    (:quality (:good :medium :bad))
	    (:line-style ,(create-instance NIL gg:pop-up-from-icon))
	    (:filling-style ,(create-instance NIL gg:pop-up-from-icon)))))
(create-instance 'PS-WIN inter:interactor-window
  (:background-color opal:motif-gray)
  (:left (+ 2 (g-value SGAD :window :width)))
  (:top (g-value SGAD :window :top))
  (:parent WIN)
  (:aggregate PS))
(opal:update PS-WIN)

