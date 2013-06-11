(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 700)(:top 10)(:width 210)(:height 100))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate
			  (:left 10) (:top 10)
			  (:width 200) (:height 200)))
(opal:update WIN)

(create-instance 'CHECK-BOX opal:aggregadget
   (:left 20)
   (:top 20)
   (:width 50)
   (:height 50)
   (:parts
    `((:box ,opal:rectangle
         (:left ,(o-formula (gvl :parent :left)))
         (:top ,(o-formula (gvl :parent :top)))
         (:width ,(o-formula (gvl :parent :width)))
         (:height ,(o-formula (gvl :parent :height))))
      (:mark ,opal:aggregadget
         (:parts
          ((:left-line ,opal:line
              (:x1 ,(o-formula (+ (gvl :parent :parent :left)
                     (floor (gvl :parent :parent :width) 10))))
              (:y1 ,(o-formula (+ (gvl :parent :parent :top)
                     (floor (gvl :parent :parent :height) 2))))
              (:x2 ,(o-formula (+ (gvl :parent :parent :left)
                     (floor (gvl :parent :parent :width) 2))))
              (:y2 ,(o-formula (+ (gvl :parent :parent :top)
                     (floor (* (gvl :parent :parent :height) 9)
			    10))))
              (:line-style ,opal:line-2))
          (:right-line ,opal:line
              (:x1 ,(o-formula 
		     (opal:gvl-sibling :left-line :x2)))
              (:y1 ,(o-formula
		     (opal:gvl-sibling :left-line :y2)))
              (:x2 ,(o-formula (+ (gvl :parent :parent :left)
                     (floor (* (gvl :parent :parent :width) 9)
			    10))))
              (:y2 ,(o-formula (+ (gvl :parent :parent :top)
                     (floor (gvl :parent :parent :height) 10))))
              (:line-style ,opal:line-2))))))))

(create-instance 'CB1 CHECK-BOX)

(create-instance 'CB2 CHECK-BOX (:left 90) (:width 100) (:height 60))

(opal:add-components TOP-AGG CB1 CB2)

(opal:update WIN)