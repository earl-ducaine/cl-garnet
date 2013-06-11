(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 170) (:width 180)
  (:aggregate (create-instance 'agg opal:aggregate)))

(create-instance 'FRAMED-TEXT-LIST opal:aggrelist
   (:left 20) (:top 20)
   (:items '("An aggrelist" "using an" "aggregate" 
	     "as an" "item-prototype"))
   (:item-prototype
    `(,opal:aggregadget
      (:parts
       ((:frame ,opal:rectangle
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
	   (:string ,(o-formula
		      (nth (gvl :parent :rank)
		      (gvl :parent :parent :items)))))))
      (:interactors
       ((:text-inter ,inter:text-interactor
	   (:window ,(o-formula 
		      (gv-local :self :operates-on :window)))
	   (:feedback-obj NIL)
	   (:start-where ,(o-formula
	     (list :in (gvl :operates-on :text))))
	   (:abort-event #\control-\g)
	   (:stop-event (:leftdown #\RETURN))
	   (:final-function
	    ,#'(lambda (inter text event string x y)
		 (let ((elem (g-value inter :operates-on)))
		   (change-item (g-value elem :parent)
				string
				(g-value elem :rank))))) ))))))

(opal:add-components agg framed-text-list)
(opal:update win T)


