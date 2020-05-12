(in-package "user" :use '("kr" "kr-debug" "garnet-debug"))

(defvar init-gq
  (garnet-load "aggregadgets:aggregraphs-loader")
  (garnet-load "ps:ps-loader"))

(when (boundp 'win) (opal:destroy win))

(create-instance 'win inter:interactor-window
  (:left 600)(:top 30)(:width 400)(:height 455))
(s-value win :aggregate (create-instance 'top-agg opal:aggregate))
(opal:update win)

(defparameter *slots*
  (list (list opal:graphic-quality)
	(cons opal:filling-style ":fill-style :solid~%:fill-rule :even-odd~%:stipple nil~%:foreground-color opal:black~%:background-color opal:white")
	(cons opal:line-style ":line-thickness 0~%:line-style :solid~%:cap-style :butt~%:join-style :miter~%:dash-pattern nil~%:stipple nil~%:foreground-color opal:black~%:background-color opal:white")
	(cons opal:color ":red 1.0~%:green 1.0~%:blue 1.0~%:color-name nil")
	(cons opal:font ":family :fixed~%:face :roman~%:size :medium")
	(cons opal:font-from-file ":font-name \"\"~%:font-path nil")
	))

(create-instance 'node-proto opal:aggregadget
  (:left 0) (:top 0)
  (:box (list 0 0 0 0))
  (:info opal:graphic-quality)
  (:parts
   `((:rect ,opal:roundtangle
      (:filling-style ,opal:white-fill)
      (:top ,(kr:o-formula (kr:gvl :parent :top)))
      (:left ,(kr:o-formula (kr:gvl :parent :left)))
      (:width ,(kr:o-formula (+ (max (gvl :parent :text-al :width)
				     (if (gvl :parent :text-bob :visible)
					 (gvl :parent :text-bob :width) 0))
				10)))
      (:height ,(kr:o-formula (+ (kr:gvl :parent :text-al :height)
				 (if (gvl :parent :text-bob :visible)
				     (kr:gvl :parent :text-bob :height) 0)
				 8)))
      (:radius 5))
     (:text-al ,opal:text
      (:left ,(kr:o-formula (+ (kr:gvl :parent :left) 4)))
      (:top ,(kr:o-formula (+ (kr:gvl :parent :top) 4)))
      (:string ,(o-formula
		 (format nil "~s" (kr::schema-name (gvl :parent :info)))))
      (:font ,(opal:get-standard-font nil :bold nil)))
     (:text-bob ,opal:text
      (:left ,(o-formula (+ (gvl :parent :left) 6)))
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :text-al)) 2)))
      (:string ,(o-formula
		 (let ((str (cdr (assoc (gvl :parent :info) *slots*))))
		   (if str
		       (format nil str)
		       ""))))
      (:visible ,(o-formula (not (string= "" (gvl :string)))))))))

(create-instance 'graph opal:aggregraph
    (:left 10) (:top 10)
    (:h-spacing 30)
    (:node-prototype node-proto)
    (:children-function #'(lambda (obj depth)
			    (when (<= depth 1)
			      (remove nil
			        (mapcar #'(lambda (obj2)
					    (if (assoc obj2 *slots*) obj2))
					(g-value obj :is-a-inv))))))
    (:info-function #'identity)
    (:source-roots (list opal:graphic-quality)))

(opal:add-components top-agg graph)
(opal:update win)


#|
(create-instance 'm gg:multi-graphics-selection
  (:start-where (list :element-of (g-value graph :nodes))))
(opal:add-component top-agg m)

(create-instance 'i inter:move-grow-interactor
  (:start-where (list :element-of (g-value graph :nodes)))
  (:window win)
  (:slots-to-set (list t t nil nil)))

(let ((node (kr-send graph :source-to-graph-node graph inter:interactor-window)))
  (if (string= (g-value node :text-al :string) "interactors:interactor-window")
      (s-value node :text-al :string "inter:interactor-window")
      (error "Andy says -- wrong node!")))
(opal:update win)


(dolist (o (g-value node-proto :is-a-inv))
  (s-value o :box (list (g-value o :left) (g-value o :top) nil nil))
  (s-value o :left (o-formula (first (gvl :box))))
  (s-value o :top (o-formula (second (gvl :box)))))
|#
