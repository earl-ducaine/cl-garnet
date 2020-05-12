(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:top 400) (:left 300)
  (:height 130) (:width 200)
  (:aggregate (create-instance 'agg opal:aggregate))
  (:background-color opal:motif-gray))

(create-instance 'rect opal:rectangle
  (:visible NIL))

(create-instance 'obj-prop garnet-gadgets:motif-prop-sheet-for-obj-with-OK
  (:left 5) (:top 5) (:obj RECT)
  (:slots `(:left :top :width :height
	    (:quality (:good :medium :bad))
	    (:line-style ,(create-instance NIL garnet-gadgets:pop-up-from-icon))
	    (:filling-style ,(create-instance NIL garnet-gadgets:pop-up-from-icon)))))

(opal:add-components agg rect obj-prop)
(opal:update win T)
