(when (boundp 'win) (opal:destroy win))

(defun sel-fn (gadget points)
  (let ((new-obj (create-instance NIL opal:polyline
                   (:point-list points)
		   (:filling-style opal:gray-fill))))
    (opal:add-component agg new-obj)))

(create-instance 'win inter:interactor-window
	(:top 300) (:left 300)
	(:height 250) (:width 350)
	(:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'poly garnet-gadgets:polyline-creator
	(:start-where (list :in win))
        (:selection-function 'sel-fn))

(opal:add-components agg poly)
(opal:update win T)
