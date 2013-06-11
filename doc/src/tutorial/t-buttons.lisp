(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 750)(:top 80)(:width 200)(:height 400))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate))
(opal:update WIN)

(create-instance 'SHADOW-PROTO opal:rectangle
   (:left (o-formula (+ 20 (gvl :parent :left))))
   (:top (o-formula (+ 20 (gvl :parent :top))))
   (:width (o-formula (gvl :parent :shadow-width)))
   (:height (o-formula (gvl :parent :shadow-height)))
   (:filling-style opal:black-fill))

(create-instance 'GRAY-PROTO opal:rectangle
   (:left (o-formula (if (gvl :parent :interim-selected)
			 (+ 20 (gvl :parent :left))
			 (gvl :parent :left))))
   (:top (o-formula (if (gvl :parent :interim-selected)
			(+ 20 (gvl :parent :top))
			(gvl :parent :top))))
   (:width (o-formula (gvl :parent :shadow-width)))
   (:height (o-formula (gvl :parent :shadow-height)))
   (:filling-style opal:gray-fill))

(create-instance 'WHITE-PROTO opal:rectangle
   (:left (o-formula (+ 7 (gvl :parent :gray-rect :left))))
   (:top (o-formula (+ 7 (gvl :parent :gray-rect :top))))
   (:width (o-formula (- (gvl :parent :gray-rect :width) 14)))
   (:height (o-formula (- (gvl :parent :gray-rect :height) 14)))
   (:filling-style opal:white-fill))

(create-instance 'TEXT-PROTO opal:text
   (:left (o-formula (+ (gvl :parent :white-rect :left)
			(round (- (gvl :parent :white-rect :width)
				  (gvl :width))
			       2))))
   (:top (o-formula (+ (gvl :parent :white-rect :top)
		       (round (- (gvl :parent :white-rect :height)
				 (gvl :height))
			      2))))
   (:string (o-formula (gvl :parent :string))))

(create-instance 'BUTTON opal:aggregadget
   (:left 20) (:top 20)
   (:width 120) (:height 70)
   (:shadow-width 100) (:shadow-height 50)
   (:parts
    `((:shadow ,SHADOW-PROTO)
      (:gray-rect ,GRAY-PROTO)
      (:white-rect ,WHITE-PROTO)
      (:text ,TEXT-PROTO))))

(create-instance 'PRESS-PROTO inter:button-interactor
   (:window (o-formula (gvl :operates-on :window)))
   (:start-where (o-formula (list :element-of (gvl :operates-on))))
   (:start-event :leftdown))

(create-instance 'PANEL opal:aggrelist
   (:left 30) (:top 10)
   (:items '("Mozart" "Chopin" "Strauss" "Beethoven" "Vivaldi"))
   (:item-prototype
    `(,BUTTON
      (:string ,(o-formula (nth (gvl :rank) (gvl :parent :items))))))
   (:interactors
    `((:press ,PRESS-PROTO))))

(opal:add-components TOP-AGG PANEL)
(opal:update WIN)
