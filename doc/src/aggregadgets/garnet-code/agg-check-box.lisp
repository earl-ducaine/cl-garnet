(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:width 200) (:height 100)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'check-mark opal:aggregadget
  (:parts
   `((:left-line ,opal:line
		 (:x1 80)
		 (:y1 50)
		 (:x2 95)
		 (:y2 70))
     (:right-line ,opal:line
		  (:x1 ,(o-formula (gvl :parent :left-line :x2)))
  		  (:y1 ,(o-formula (gvl :parent :left-line :y2)))
		  (:x2 120)
		  (:y2 30)))))

(create-instance 'check-box opal:aggregadget
  (:parts
   `((:box ,opal:rectangle
	   (:left 75) (:top 25) (:width 50) (:height 50))
     (:mark ,CHECK-MARK))))


(opal:add-components agg check-box)
(opal:update win T)
