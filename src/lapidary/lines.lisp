(in-package "LAPIDARY")

(defparameter *line-button-list* NIL)
(defparameter *arrow-button-list* NIL)

(defun line-do-go ()
(line-do-stop)

(create-instance 'LINE-WIN inter:interactor-window
   (:title "line styles")
   (:left (first *line-menu-dimensions*))
   (:top (second *line-menu-dimensions*))
   (:width (third *line-menu-dimensions*))
   (:height (fourth *line-menu-dimensions*)))

(s-value LINE-WIN :aggregate (create-instance 'LINE-TOP-AGG opal:aggregate))
(opal:update LINE-WIN)


(setf *line-button-list*
      (make-button-list `((opal:text :string "constraint" :value :constraint
				      :name :constraint)
			  (opal:text :string "none" :value nil
				      :name :borderless)
			  (opal:line :line-style opal:thin-line
				      :value opal:thin-line
				      :name :thin-line)
			  (opal:line :line-style opal:dashed-line
				      :value opal:dashed-line
				      :name :dashed-line)
			  (opal:line :line-style opal:dotted-line
				      :value opal:dotted-line
				      :name :dotted-line)
			  (opal:line :line-style opal:line-2
				      :value opal:line-2
				      :name :line-2)
			  (opal:line :line-style opal:line-4
				      :value opal:line-4
				      :name :line-4)
			  (opal:line :line-style opal:line-8
				      :value opal:line-8
				      :name :line-8))))



;;  LINE-MENU is the top panel of buttons controlling line-styles
;;
(create-instance 'LINE-MENU opal:aggrelist
   (:left 10)
   (:top 10)
   (:button-width 85)
   (:button-height 25)
   (:value (o-formula (gvl :selected :label :value)))
   (:name (o-formula (gvl :selected :label :name)))
   (:parts *line-button-list*)
   (:interactors
    `((:press ,LAPIDARY-MENU-BUTTON-INTER
	      (:final-function ,#'line-menu-handler)))))


;; set the initial selection to be a thin line
(let ((initial-selection (third (g-value line-menu :components))))
  (s-value line-menu :selected initial-selection)
  (s-value initial-selection :selected t))

(opal:add-component line-top-agg line-menu)
(opal:update LINE-WIN))

(defun line-do-stop ()
  (when (boundp 'LINE-WIN) (opal:destroy LINE-WIN)))
