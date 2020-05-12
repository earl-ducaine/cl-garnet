(when (boundp 'w) (opal:destroy w))

(create-instance 'my-font opal:font
		 (:size :small))

(defmacro add-line-with-arrowheads (agg l str x-offset y-offset)
  `(opal:add-components ,agg
      ,l
      (create-instance nil opal:arrowhead
		 (:from-x (formula '(gv ,l :x1)))
		 (:from-y (formula '(gv ,l :y1)))
		 (:head-x (formula '(gv ,l :x2)))
		 (:head-y (formula '(gv ,l :y2)))
		 (:length 10) (:diameter 10))
      (create-instance nil opal:arrowhead
		 (:head-x (formula '(gv ,l :x1)))
		 (:head-y (formula '(gv ,l :y1)))
		 (:from-x (formula '(gv ,l :x2)))
		 (:from-y (formula '(gv ,l :y2)))
		 (:length 10) (:diameter 10))
      (create-instance nil opal:text (:string ,str)
		 (:left (formula '(+ ,x-offset (opal:gv-center-x ,l))))
		 (:top (formula '(+ ,y-offset (opal:gv-center-y ,l))))
		 (:font my-font))))


(create-instance 'w opal:window (:left 0) (:top 300)
		 (:width 400) (:height 400)
		 (:aggregate (create-instance 'a opal:aggregate)))

(create-instance 'r opal:roundtangle
		 (:top 50) (:left 50)
		 (:width 200) (:height 200)
		 (:filling-style opal:light-gray-fill)
		 (:line-style (create-instance nil opal:line-style
					       (:line-thickness 15)))
		 (:radius 50))

(create-instance 'left-line opal:line
		 (:x1 (formula '(gv r :left)))
		 (:y1 (formula '(opal:gv-center-y r)))
		 (:x2 (formula '(gv r :left)))
		 (:y2 (formula '(+ (opal:gv-bottom r) 90)))
		 (:line-style opal:dashed-line))

(create-instance 'top-line opal:line
		 (:x1 (formula '(opal:gv-center-x r)))
		 (:y1 (formula '(gv r :top)))
		 (:x2 (formula '(+ (opal:gv-right r) 90)))
		 (:y2 (formula '(gv r :top)))
		 (:line-style opal:dashed-line))

(create-instance 'right-line opal:line
		 (:x1 (formula '(opal:gv-right r)))
		 (:y1 (formula '(opal:gv-center-y r)))
		 (:x2 (formula '(opal:gv-right r)))
		 (:y2 (formula '(+ (opal:gv-bottom r) 90)))
		 (:line-style opal:dashed-line))

(create-instance 'bottom-line opal:line
		 (:x1 (formula '(opal:gv-center-x r)))
		 (:y1 (formula '(opal:gv-bottom r)))
		 (:x2 (formula '(+ (opal:gv-right r) 90)))
		 (:y2 (formula '(opal:gv-bottom r)))
		 (:line-style opal:dashed-line))

(create-instance 'arc1 opal:arc
  (:left (formula '(- (opal:gv-right r) (+ (gvl :width) 2))))
  (:top (formula '(- (opal:gv-bottom r) (+ (gvl :height) 2))))
  (:width (formula '(- (* 2 (gv r :radius)) 2)))
  (:height (formula '(- (* 2 (gv r :radius)) 2)))  
  (:angle1 (/ Pi 4))
  (:angle2 Pi)
  (:line-style opal:dashed-line))

(create-instance 'arc2 opal:arc
  (:left (formula '(- (opal:gv-right r) (+ (gvl :width) 2))))
  (:top (formula '(- (opal:gv-bottom r) (+ (gvl :height) 2))))
  (:width (formula '(- (* 2 (gv r :radius)) 2)))
  (:height (formula '(- (* 2 (gv r :radius)) 2)))
  (:angle1 (/ (* 5 Pi) 4))
  (:angle2 Pi)
  (:line-style (create-instance NIL opal:line-style
		 (:foreground-color opal:white)
		 (:background-color opal:black)
		 (:line-style :dash)
		 (:dash-pattern '(4 4)))))

(create-instance 'c-center-line opal:line
		 (:x1 (formula '(opal:gv-center-x arc1)))
		 (:y1 (formula '(opal:gv-center-y arc1)))
		 (:x2 (formula '(+ (opal:gv-right r) 90)))
		 (:y2 (formula '(opal:gv-center-y arc1)))
		 (:line-style opal:dashed-line))

(opal:add-components a r left-line top-line bottom-line right-line
		     c-center-line)

(create-instance 'radius-line opal:line
		 (:x1 (formula '(+ (opal:gv-right r) 30)))
		 (:y1 (formula '(gv c-center-line :y1)))
		 (:x2 (formula '(+ (opal:gv-right r) 30)))
		 (:y2 (formula '(gv bottom-line :y2))))

(create-instance 'width-line opal:line
		 (:x1 (formula '(gv r :left)))
		 (:y1 (formula '(+ (opal:gv-bottom r) 70)))
		 (:x2 (formula '(opal:gv-right r)))
		 (:y2 (formula '(+ (opal:gv-bottom r) 70))))

(create-instance 'height-line opal:line
		 (:x1 (formula '(+ (opal:gv-right r) 70)))
		 (:y1 (formula '(gv r :top)))
		 (:x2 (formula '(+ (opal:gv-right r) 70)))
		 (:y2 (formula '(opal:gv-right r))))

(add-line-with-arrowheads a radius-line ":radius" 3 -3)
(add-line-with-arrowheads a width-line ":width" -3 3)
(add-line-with-arrowheads a height-line ":height" 3 0)


(opal:add-components a
   (create-instance nil opal:text (:string ":left")
		    (:left (formula '(- (gv left-line :x2) (floor (gvl :width) 2))))
		    (:top (formula '(gv left-line :y2)))
		    (:font my-font))
   (create-instance nil opal:text (:string " :top")
		    (:left (formula '(gv top-line :x2)))
		    (:top (formula '(- (gv top-line :y2) (floor (gvl :height) 2))))
		    (:font my-font))
   arc1 arc2)

(opal:update w)

				
