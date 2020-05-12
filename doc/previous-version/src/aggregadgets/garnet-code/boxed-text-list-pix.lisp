(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 210) (:width 180)
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

(create-instance 'BOXED-TEXT-LIST FRAMED-TEXT-LIST
   (:items '("An aggrelist" "using an" "aggregate" 
	     "as an" "item-prototype"))
   (:left 40)
   (:top 20)
   (:item-prototype
    `(:modify
      (:parts
       ((:frame :modify
	   ; make the frame gray
	   (:filling-style ,opal:gray-fill)
	   ; make the frame wider
	   (:width ,(o-formula (+ (opal:gvl-sibling :text :width) 16)))
	   ; make the frame taller
	   (:height ,(o-formula (+ (opal:gvl-sibling :text :height) 16))))
	(:white-box ,opal:rectangle
	   (:filling-style ,opal:white-fill)
	   (:left ,(o-formula (+ (gvl :parent :left) 4)))
	   (:top ,(o-formula (+ (gvl :parent :top) 4)))
	   (:width ,(o-formula (+ (opal:gvl-sibling :text :width) 8)))
	   (:height ,(o-formula (+ (opal:gvl-sibling :text :height) 8))))
	(:text :modify  ; move the text to allow for the border
	   (:left ,(o-formula (+ (gvl :parent :left) 8)))
	   (:top ,(o-formula (+ (gvl :parent :top) 8)))))))))

(opal:add-components agg boxed-text-list)
(opal:update win T)



