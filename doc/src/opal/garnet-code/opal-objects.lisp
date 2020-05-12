(in-package "USER" :use '("KR" "KR-DEBUG" "GARNET-DEBUG"))

(defvar INIT
  (progn
    (garnet-load "aggregadgets:aggregraphs-loader")
    (garnet-load "opal:multifont-loader")
    (garnet-load "ps:ps-loader")
    (garnet-load "gadgets:multi-selection-loader")
    ))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
  (:left 50)(:top 30)(:width 912)(:height 776))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate))
(opal:update WIN)

(defparameter *slots*
  (list (cons opal:view-object ":left 0~%:top 0~%:width 0~%:height 0~%:visible #k<f16>")
	(cons opal::window ":left 0~%:top 0~%:width 355~%:height 277")
	(cons opal:graphical-object ":width 20~%:height 20~%:draw-function :copy~%:line-style opal:default-line-style~%:filling-style nil~%:select-outline-only nil~%:hit-threshold 0")
	(cons opal:aggregate ":left #k<f21>~%:top #k<f20>~%:width #k<f19>~%:height #k<f18>")
	(cons opal:bitmap ":width #k<f32>~%:height #k<f31>~%:filling-style opal:default-filling-style")
	(cons opal:text ":width #k<f51>~%:height #k<f52>~%:string \"\"~%:font opal:default-font~%:fill-background-p nil~%:justification :left")
	(cons opal:multipoint ":point-list (18 10 ...)~%:left #k<f30>~%:top #k<f29>~%:width #k<f28>~%:height #k<f27>")
	(cons opal:arc ":angle1 0~%:angle2 (/ pi 4)")
	(cons opal:line ":x1 0  :x2 0~%:y1 0  :y2 0~%:left #k<f25>~%:top #k<f24>~%:width #k<f23>~%:height #k<f24>~%:line-p t")
	(cons opal:roundtangle ":radius :small")
	(cons opal:arrowhead ":from-x 0~%:from-y 0~%:head-x 0~%:head-y 0~%:length 10~%:diameter 10~%:open-p t")
	(cons opal:multifont-text ":initial-text \"\"~%:word-wrap-p nil~%:text-width 300~%:fill-background-p t~%:draw-function :copy~%:show-marks nil")
	(list opal:rectangle)
	(list opal:polyline)
	(list opal:circle)
	(list opal:oval)
	(list opal:pixmap)
	(list opal:arrow-cursor)
	(list opal:hourglass-cursor)
	(list inter:interactor-window)
	(list opal:aggregadget)
	(list opal:aggrelist)
	(list opal:aggregraph)
	))

(create-instance 'node-proto opal:aggregadget
  (:left 0) (:top 0)
  (:box (list 0 0 0 0))
  (:info opal:view-object)
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
    (:left 10) (:top 40)
    (:node-prototype node-proto)
    (:children-function #'(lambda (obj depth)
			    (when (<= depth 3)
			      (remove nil
			        (mapcar #'(lambda (obj2)
					    (if (assoc obj2 *slots*) obj2))
					(g-value obj :is-a-inv))))))
    (:info-function #'identity)
    (:source-roots (list opal:view-object)))

(opal:add-components TOP-AGG GRAPH)
(opal:update WIN)

#|
(create-instance 'i inter:move-grow-interactor
  (:start-where (list :element-of (g-value graph :nodes)))
  (:window win)
  (:slots-to-set (list t t nil nil)))
|#

(create-instance 'M gg:multi-graphics-selection
  (:start-where (list :element-of (g-value GRAPH :nodes))))
(opal:add-component TOP-AGG M)

(let ((node (kr-send graph :source-to-graph-node graph inter:interactor-window)))
  (if (string= (g-value node :text-al :string) "INTERACTORS:INTERACTOR-WINDOW")
      (s-value node :text-al :string "INTER:INTERACTOR-WINDOW")
      (error "Andy says -- WRONG NODE!")))
(opal:update WIN)


(dolist (o (g-value node-proto :is-a-inv))
  (s-value o :box (list (g-value o :left) (g-value o :top) nil nil))
  (s-value o :left (o-formula (first (gvl :box))))
  (s-value o :top (o-formula (second (gvl :box)))))
