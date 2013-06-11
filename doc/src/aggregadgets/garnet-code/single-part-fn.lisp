(in-package "USER" :use '("LISP" "KR" "GARNET-DEBUG"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
   (:left 700)(:top 10)(:width 240)(:height 120))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate))
(opal:update WIN)

;; This function generates an object that can be used as a label for
;; an instance of AGG-PROTO.  The :item slot in the aggregadget may contain
;; any object or string.  Be careful not to choose a Garnet prototype
;; such as opal:circle for the item, since this function will use that
;; object as the label.
;;
(defun Get-Label (agg)
  (let* ((item (g-value agg :item))
	 ;; Item may be an object or a string
	 (new-label (if (schema-p item)
			(if (g-value item :parent)
			    ;; The item has been used already --
			    ;; Use it as a prototype
			    (create-instance NIL item)
			    ;; Use the item itself
			    item)
		        (create-instance NIL opal:text
			  (:string item)
			  (:font (opal:get-standard-font
				  :sans-serif :bold :very-large))))))
    (s-value new-label :left (o-formula (opal:gv-center-x-is-center-of
					 (gvl :parent))))
    (s-value new-label :top (o-formula (opal:gv-center-y-is-center-of
					(gvl :parent))))
    new-label))


(create-instance 'AGG-PROTO opal:aggregadget
  (:item "Text")
  (:top 20) (:width 60) (:height 80)
  (:parts
   `((:frame ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :left)))
	     (:top ,(o-formula (gvl :parent :top)))
	     (:width ,(o-formula (gvl :parent :width)))
	     (:height ,(o-formula (gvl :parent :height))))
     (:label ,#'Get-Label))))

(create-instance 'CIRCLE-LABEL opal:circle
  (:width 30) (:height 30)
  (:line-style NIL)
  (:filling-style opal:black-fill))

(create-instance 'SQUARE-LABEL opal:rectangle
  (:width 30) (:height 30)
  (:line-style NIL)
  (:filling-style opal:black-fill))

(create-instance 'PLUS-LABEL opal:aggregadget
  (:width 30) (:height 30)
  (:parts
   `((NIL ,opal:rectangle
      (:left ,(o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
      (:top ,(o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
      (:width ,(o-formula (round (gvl :parent :width) 2)))
      (:height ,(o-formula (gvl :parent :height)))
      (:line-style NIL) (:filling-style ,opal:black-fill))
     (NIL ,opal:rectangle
      (:left ,(o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
      (:top ,(o-formula (opal:gv-center-x-is-center-of (gvl :parent))))
      (:width ,(o-formula (gvl :parent :width)))
      (:height ,(o-formula (round (gvl :parent :height) 2)))
      (:line-style NIL) (:filling-style ,opal:black-fill)))))

(create-instance 'AGG1 AGG-PROTO
  (:left 10)
  (:item CIRCLE-LABEL))

(create-instance 'AGG2 AGG-PROTO
  (:left 90)
  (:item SQUARE-LABEL))

(create-instance 'AGG4 AGG-PROTO
  (:left 170)
  (:item "W"))

(opal:add-components TOP-AGG AGG1 AGG2 AGG4)
(opal:update WIN)

 