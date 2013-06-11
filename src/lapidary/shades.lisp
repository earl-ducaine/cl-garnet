(in-package "LAPIDARY")

(defparameter *shade-button-list* NIL)

(defun MAKE-BUTTON-LIST (specifications-list)
  (mapcar #'(lambda (args)
	      (eval `(make-button-part ,@args)))
	  specifications-list))
(defun shade-do-go ()
  
  (shade-do-stop)
  
  (create-instance 'SHADE-WIN inter:interactor-window
   (:title "filling styles")
   (:left (first *shade-menu-dimensions*))
   (:top (second *shade-menu-dimensions*))
   (:width (third *shade-menu-dimensions*))
   (:height (fourth *shade-menu-dimensions*)))

  (s-value SHADE-WIN :aggregate (create-instance 'SHADE-TOP-AGG opal:aggregate))
  (opal:update SHADE-WIN)
  
  
  (setf *shade-button-list*
	(make-button-list `((opal:text :string "constraint" :value :constraint
					:name :constraint)
			    (opal:text :string "none" :value nil
					:name :transparent)
			    (opal:rectangle :filling-style opal:white-fill
					     :value opal:white-fill
					     :name :white)
			    (opal:rectangle :filling-style opal:light-gray-fill
					     :value opal:light-gray-fill
					     :name :light-gray)
			    (opal:rectangle :filling-style opal:gray-fill
					     :value opal:gray-fill
					     :name :gray)
			    (opal:rectangle :filling-style opal:dark-gray-fill
					     :value opal:dark-gray-fill
					     :name :dark-gray)
			    (opal:rectangle :filling-style opal:black-fill
					     :value opal:black-fill
					     :name :black))))
  
  
  ;;  SHADE-MENU is the top panel of buttons controlling line-styles
  ;;
  (create-instance 'SHADE-MENU opal:aggrelist
		   (:left 10)
		   (:top 10)
		   (:button-width 90)
		   (:button-height 35)
		   (:value (o-formula (gvl :selected :label :value)))
		   (:name (o-formula (gvl :selected :label :name)))
		   (:parts *shade-button-list*)
		   (:interactors
		    `((:press ,LAPIDARY-MENU-BUTTON-INTER
			      (:final-function ,#'shade-menu-handler)))))

;; set the initial selection to be a white filling-style
(let ((initial-selection (third (g-value shade-menu :components))))
  (s-value shade-menu :selected initial-selection)
  (s-value initial-selection :selected t))
  
  
  (opal:add-components SHADE-TOP-AGG SHADE-MENU)
  (opal:update SHADE-WIN))

(defun shade-do-stop ()
  (when (boundp 'SHADE-WIN) (opal:destroy SHADE-WIN)))
