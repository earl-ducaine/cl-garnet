(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 90) (:width 130)
  (:aggregate (create-instance 'agg opal:aggregate)))


;;; compute vertical position in :box according to a proportion
(defun vert-prop (frac) 
  (+ (gvl :parent :parent :box :top)
     (round (* (gvl :parent :parent :box :height) 
	       frac))))

;;; compute horizontal position in :box according to a proportion
(defun horiz-prop (frac)
  (+ (gvl :parent :parent :box :left)
     (round (* (gvl :parent :parent :box :width)
	       frac))))

(create-instance 'X-BOX opal:aggregadget
   (:left 40) 
   (:top 20)
   (:width 50)
   (:height 50)
   (:parts
    `((:box ,opal:rectangle
	    (:left ,(o-formula (gvl :parent :left)))
	    (:top  ,(o-formula (gvl :parent :top)))
	    (:width  ,(o-formula (gvl :parent :width)))
	    (:height ,(o-formula (gvl :parent :height))))
      (:mark ,opal:aggregadget
	     (:parts
	      ((:line1 ,opal:line
		       (:x1 ,(o-formula (horiz-prop 0.3)))
		       (:y1 ,(o-formula (vert-prop 0.3)))
		       (:x2 ,(o-formula (horiz-prop 0.7)))
		       (:y2 ,(o-formula (vert-prop 0.7))))
	       (:line2 ,opal:line
		       (:x1 ,(o-formula (horiz-prop 0.7)))
		       (:y1 ,(o-formula (vert-prop 0.3)))
		       (:x2 ,(o-formula (horiz-prop 0.3)))
		       (:y2 ,(o-formula (vert-prop 0.7))))))))))

(create-instance 'CIRCLE-X-BOX X-BOX
   (:left 40)
   (:top 20)
   (:parts 
    `((:box :modify (:filling-style ,opal:gray-fill))
      (:circle ,opal:circle
	       (:left ,(o-formula (+ (gvl :parent :left) 2)))
	       (:top ,(o-formula (+ (gvl :parent :top) 2)))
	       (:width ,(o-formula (- (gvl :parent :width) 4)))
	       (:height ,(o-formula (- (gvl :parent :height) 4)))
	       (:filling-style ,opal:white-fill))
      :mark)))

(defun circle-box-test ()
  (create-instance 'CIRCLE-BOX CIRCLE-X-BOX
   (:left 40)
   (:top 20)
   (:parts
    `(:box 
      :circle
      (:mark :omit)
      (:inner-circle ,opal:circle
	     (:left ,(o-formula (+ (gvl :parent :left) 10)))
	     (:top ,(o-formula (+ (gvl :parent :top) 10)))
	     (:width ,(o-formula (- (gvl :parent :width) 20)))
	     (:height ,(o-formula (- (gvl :parent :height) 20))))))))

(defun x-sq-box-test ()
  (create-instance 'X-SQ-BOX X-BOX
    (:left 40)
    (:top 20)
    (:parts
     `(:box		; inherit the box with no change
       (:mark :modify	; modify the mark
	(:parts		; since :line1 and :line2 are not mentioned,
			; they are inherited as is
	 ((:square ,opal:rectangle	; add a new part to the mark
	      (:left ,(o-formula (horiz-prop 0.2)))
	      (:width ,(o-formula (- (horiz-prop 0.8) 
				     (horiz-prop 0.2))))
	      (:top ,(o-formula (vert-prop 0.2)))
	      (:height ,(o-formula (- (vert-prop 0.8) 
				      (vert-prop 0.2))))))))))))

(opal:add-component agg (x-sq-box-test))
(opal:update win T)







