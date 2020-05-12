(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 150) (:width 300)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'SCHEMA-GRAPH-2 opal:aggregraph
   (:children-function #'(lambda (source-node depth)
			   (when (< depth 1)
			     (g-value source-node :is-a-inv))))
   (:info-function #'(lambda (source-node)
		       (string-capitalize
			(kr:name-for-schema source-node))))
   (:source-roots (list opal:view-object))
   (:node-prototype
    (create-instance NIL opal:aggregraph-node-prototype
       (:interim-selected NIL)  
       (:parts
	`((:box :modify
	   (:filling-style ,(o-formula (if (gvl :parent :interim-selected)
					   opal:black-fill
					   opal:white-fill)))
	   (:draw-function :xor) (:fast-redraw-p T))
	  :text-al))))
   (:interactors
    `((:press ,inter:menu-interactor
       (:window ,(o-formula (gv-local :self :operates-on :window)))
       (:start-where ,(o-formula (list :element-of
                                       (gvl :operates-on :nodes))))
       (:final-function
	,#'(lambda (inter node)
	     (let* ((graph (g-value node :parent :parent))
		    (source-node (g-value node :source-node)))
	       (format T "~%~% ***** Clicked on ~S *****~%" source-node)
	       (kr:ps source-node))))))))

(opal:add-components agg schema-graph-2)
(opal:update win T)


