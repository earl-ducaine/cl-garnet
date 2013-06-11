(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300) 
  (:height 60) (:width 170)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'FRAMED-TEXT opal:aggregadget
   (:left 20)     
   (:top 20)      
   (:parts
    `((:frame ,opal:rectangle
          (:left ,(o-formula (gvl :parent :left)))
          (:top ,(o-formula (gvl :parent :top)))
          (:width ,(o-formula (+ (gvl :parent :text :width) 4)))
          (:height ,(o-formula (+ (gvl :parent :text :height) 4))))
      (:text ,opal:cursor-text
          (:left ,(o-formula (+ (gvl :parent :left) 2)))
          (:top ,(o-formula (+ (gvl :parent :top) 2)))
          (:cursor-index NIL)
	  (:font ,(create-instance NIL opal:font
		   (:size :large)))
          (:string "Framed Text"))))
   (:interactors
    `((:text-inter ,inter:text-interactor
	  (:window ,(o-formula (gv-local :self :operates-on :window)))
          (:feedback-obj NIL)
          (:start-where ,(o-formula
                          (list :in (gvl :operates-on :text))))
          (:abort-event #\control-\g)
          (:stop-event (:leftdown #\RETURN))))))

(opal:add-components agg framed-text)
(opal:update win T)
