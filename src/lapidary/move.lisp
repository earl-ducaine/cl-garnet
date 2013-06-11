(in-package "COMMON-LISP-USER")

;;; ======================================================================
;;; this function creates an object that simulated the selection feedback
;;; objects. it replaces the 'grow' boxes with buttons so that the user
;;; can indicate which 'grow' box to select
;;; with eight 
;;; ======================================================================

(defun make-selection-buttons (button-agg)
  (let (obj obj-list
	(name-list '(:nw :n :ne :e :se :s :sw :w)))
    (do ((x '(l m r r r m l l) (cdr x))
	 (y '(t t t m b b b m) (cdr y))
	 (attach name-list (cdr attach)))
	((null x)) ; end test
      (setq obj
	    (kr:create-instance nil lapidary::button-with-square
	          (:left (case (car x)
			   (l (o-formula (gv button-agg :left)))
			   (m (center-x button-agg obj))
			   (r (o-formula (- (+ (gv button-agg :left)
					       (gv button-agg :width))
					    (gvl :width)
					    1)))))
	          (:top (case (car y)
			  ((t) (o-formula (gv button-agg :top)))
			  (m (center-y button-agg obj))
			  (b (o-formula (- (+ (gv button-agg :top)
					      (gv button-agg :height))
					      (gvl :height)
					      1)))))
		  (:where-attach (car attach))))
      (push obj obj-list))
    (values (reverse obj-list) name-list)))
	       
(defun make-move-dialog-window ()
  (let (agg)
  (create-instance 'move-dialog-window inter:interactor-window
       #+apple (:top 50))
    (create-instance 'agg opal:aggregate)
    (s-value move-dialog-window :aggregate agg)))

    ;; create the button object that allows the user to choose an
    ;; attach point

    

(create-instance 'selection-buttons opal:aggregadget
		 (:left 0)
		 (:top 0)
		 (:item-width-D2 (o-formula (floor (gvl :nw :background :width) 2)))
		 (:parts '(make-selection-buttons)))

(create-instance 'labeled-selection-buttons opal:aggregadget
     (:left 0)
     (:top 0)
     (:box-width 100)
     (:box-height 80)
     (:parts `((:text opal:text
		    (:left ,(o-formula (gvl :parent :left)))
		    (:top ,(o-formula (round (- (+ (gvl :parent :box :top) 
						   (/ (gvl :parent :box :height) 2))
						(/ (gvl :height) 2)))))
		    (:string ,(o-formula (gvl :parent	:string)))
		    (:font ,lapidary::fntb))
	       (:box opal:rectangle
		     (:left ,(o-formula (+ (gvl :parent :text :left)
					   (gvl :parent :text :width)
					   20)))
		     (:top ,(o-formula (+ (gvl :parent :top)
					  (gvl :parent :selection-buttons 
					       :item-width-D2))))
		     (:width ,(o-formula (gvl :parent :box-width)))
		     (:height ,(o-formula (gvl :parent :box-height))))
	       (:selection-buttons selection-buttons
		     (:left ,(o-formula (- (gvl :parent :box :left)
					   (gvl :item-width-D2))))
		     (:top ,(o-formula (gvl :parent :top)))
		     (:width ,(o-formula (+ (gvl :parent :box :width)
					    (gvl :nw :width))))
		     (:height ,(o-formula (+ (gvl :parent :box :height)
					     (gvl :nw :height)))))))
     (:interactors 
      `((:feel inter:button-interactor
	    (:window ,(o-formula (gvl :operates-on :window)))
	    (:how-set :set)
	    (:start-where ,(o-formula (list :element-of
					(gvl :operates-on :selection-buttons))))))))
 
