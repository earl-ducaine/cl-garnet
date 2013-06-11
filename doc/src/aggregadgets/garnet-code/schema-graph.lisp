(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 490) (:width 380)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'SCHEMA-GRAPH opal:aggregraph
   (:children-function #'(lambda (source-node depth)
			   (if (> depth 1)
			       NIL
			       (g-value source-node :is-a-inv))))
   (:info-function #'(lambda (source-node)
		       (string-capitalize
			(kr:name-for-schema source-node))))
   (:source-roots (list opal:view-object)))

(opal:add-components agg schema-graph)
(opal:update win T)


