(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 800) (:top 300)
  (:height 120) (:width 180)
  (:aggregate (create-instance 'agg opal:aggregate)))

(defun my-cut () (format T "~%Function CUT called~%"))
(defun my-copy () (format T "~%Function COPY called~%"))
(defun my-paste () (format T "~%Function PASTE called~%"))
(defun my-undo () (format T "~%Function UNDO called~%"))

(create-instance 'MENU-ITEM opal:text
   (:string (o-formula (car (nth (gvl :rank) (gvl :parent :items)))))
   (:action (o-formula (cadr (nth (gvl :rank)
				  (gvl :parent :items))))))

(create-instance 'MENU opal:aggregadget
   (:left 20) (:top 20) 
   (:items '(("Cut" (my-cut)) ("Copy" (my-copy))
             ("Paste" (my-paste)) ("Undo" (my-undo))))
   (:parts 
    `((:shadow ,opal:rectangle
	(:filling-style ,opal:gray-fill)
	(:left ,(o-formula (+ (gvl :parent :frame :left) 8)))
	(:top ,(o-formula (+ (gvl :parent :frame :top) 8)))
	(:width ,(o-formula (gvl :parent :frame :width)))
	(:height ,(o-formula (gvl :parent :frame :height))))
      (:frame ,opal:rectangle
	(:filling-style ,opal:white-fill)
	(:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (gvl :parent :top)))
	(:width ,(o-formula (+ (gvl :parent :items-agg :width) 8)))
	(:height ,(o-formula (+ (gvl :parent :items-agg :height) 8))))
      (:feedback ,opal:rectangle
	(:left ,(o-formula (- (gvl :obj-over :left) 2)))
	(:top ,(o-formula (- (gvl :obj-over :top) 2)))
	(:width ,(o-formula (+ (gvl :obj-over :width) 4)))
	(:height ,(o-formula (+ (gvl :obj-over :height) 4)))
	(:visible ,(o-formula (gvl :obj-over)))
	(:draw-function :xor))
      (:items-agg ,opal:aggrelist
	(:fixed-width-p T)
	(:h-align :center)
	(:left ,(o-formula (+ (gvl :parent :left) 4)))
	(:top ,(o-formula (+ (gvl :parent :top) 4)))
	(:items ,(o-formula (gvl :parent :items)))
	(:item-prototype ,menu-item))))
   (:interactors
    `((:press ,inter:menu-interactor
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:start-where ,(o-formula (list :element-of
			(gvl :operates-on :items-agg))))
	(:feedback-obj ,(o-formula (gvl :operates-on :feedback)))
	(:final-function 
	  ,#'(lambda (interactor final-obj-over)
		(eval (g-value final-obj-over :action))))))))

(create-instance 'my-menu menu
  (:left 100) (:top 20)
  (:items '(("Read") ("Save") ("Cancel"))))

(opal:add-components agg MENU MY-MENU)
(opal:update win T)


