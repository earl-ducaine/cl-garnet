(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 126) (:width 110)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'MY-AGG opal:aggrelist 
  (:left 10) (:top 10)
  (:line-style :line-3)
  (:parts
   `((:obj1 ,opal:rectangle (:width 90) (:height 30))
     (:obj2 ,opal:oval (:width 90) (:height 30))
     (:obj3 ,opal:roundtangle (:width 90) (:height 30)))))

(opal:add-components agg my-agg)
(opal:update win T)


