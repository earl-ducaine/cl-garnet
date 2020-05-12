(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 700)(:top 10)(:width 200)(:height 100))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate
			  (:left 10) (:top 10)
			  (:width 200) (:height 200)))
(opal:update WIN)

(create-instance 'CHECK-MARK opal:aggregadget
   (:parts
    `((:left-line ,opal:line
                  (:x1 70)
                  (:y1 45)
                  (:x2 95)
                  (:y2 70))
      (:right-line ,opal:line
                   (:x1 95)
                   (:y1 70)
                   (:x2 120)
                   (:y2 30)))))

(opal:add-components TOP-AGG CHECK-MARK)

(opal:update WIN)