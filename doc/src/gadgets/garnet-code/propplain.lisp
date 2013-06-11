(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 300) (:top 300)
  (:height 100) (:width 200)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'prop garnet-gadgets:prop-sheet
  (:items `((:color "Red")
	    (:height "34")
	    (:status "Nervous")
	    (:direction ,(create-instance NIL garnet-gadgets:horiz-choice-list
			    (:items '("up" "down" "diagonal"))))
	    (:range "1" 1 NIL "(1..20)"))))

(opal:add-component agg prop)
(opal:update win T)



