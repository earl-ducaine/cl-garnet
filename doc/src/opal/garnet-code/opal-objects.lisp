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
  (list (cons opal:view-object ":left 0~%:top 0~%:width 0~%:height 0~%:visible #k<F16>")
	(cons opal::window ":left 0~%:top 0~%:width 355~%:height 277")
	(cons opal:graphical-object ":width 20~%:height 20~%:draw-function :copy~%:line-style opal:default-line-style~%:filling-style NIL~%:select-outline-only NIL~%:hit-threshold 0")
	(cons opal:aggregate ":left #k<F21>~%:top #k<F20>~%:width #k<F19>~%:height #k<F18>")
	(cons opal:bitmap ":width #k<F32>~%:height #k<F31>~%:filling-style opal:default-filling-style")
	(cons opal:text ":width #k<F51>~%:height #k<F52>~%:string \"\"~%:font opal:default-font~%:fill-background-p NIL~%:justification :left")
	(cons opal:multipoint ":point-list (18 10 ...)~%:left #k<F30>~%:top #k<F29>~%:width #k<F28>~%:height #k<F27>")
	(cons opal:arc ":angle1 0~%:angle2 (/ PI 4)")
	(cons opal:line ":x1 0  :x2 0~%:y1 0  :y2 0~%:left #k<F25>~%:top #k<F24>~%:width #k<F23>~%:height #k<F24>~%:line-p T")
	(cons opal:roundtangle ":radius :small")
	(cons opal:arrowhead ":from-x 0~%:from-y 0~%:head-x 0~%:head-y 0~%:length 10~%:diameter 10~%:open-p T")
	(cons opal:multifont-text ":initial-text \"\"~%:word-wrap-p NIL~%:text-width 300~%:fill-background-p T~%:draw-function :copy~%:show-marks NIL")
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

(create-instance 'NODE-PROTO opal:aggregadget
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
		 (format NIL "~S" (kr::schema-name (gvl :parent :info)))))
      (:font ,(opal:get-standard-font NIL :bold NIL)))
     (:text-bob ,opal:text
      (:left ,(o-formula (+ (gvl :parent :left) 6)))
      (:top ,(o-formula (+ (opal:gv-bottom (gvl :parent :text-al)) 2)))
      (:string ,(o-formula
		 (let ((str (cdr (assoc (gvl :parent :info) *slots*))))
		   (if str
		       (format NIL str)
		       ""))))
      (:visible ,(o-formula (not (string= "" (gvl :string)))))))))

(create-instance 'GRAPH opal:aggregraph
    (:left 10) (:top 40)
    (:node-prototype NODE-PROTO)
    (:children-function #'(lambda (obj depth)
			    (when (<= depth 3)
			      (remove NIL
			        (mapcar #'(lambda (obj2)
					    (if (assoc obj2 *slots*) obj2))
					(g-value obj :is-a-inv))))))
    (:info-function #'identity)
    (:source-roots (list opal:view-object)))

(opal:add-components TOP-AGG GRAPH)
(opal:update WIN)

#|
(create-instance 'I inter:move-grow-interactor
  (:start-where (list :element-of (g-value GRAPH :nodes)))
  (:window WIN)
  (:slots-to-set (list T T NIL NIL)))
|#

(create-instance 'M gg:multi-graphics-selection
  (:start-where (list :element-of (g-value GRAPH :nodes))))
(opal:add-component TOP-AGG M)

(let ((node (kr-send graph :source-to-graph-node graph inter:interactor-window)))
  (if (string= (g-value node :text-al :string) "INTERACTORS:INTERACTOR-WINDOW")
      (s-value node :text-al :string "INTER:INTERACTOR-WINDOW")
      (error "Andy says -- WRONG NODE!")))
(opal:update WIN)


(dolist (o (g-value NODE-PROTO :is-a-inv))
  (s-value o :box (list (g-value o :left) (g-value o :top) NIL NIL))
  (s-value o :left (o-formula (first (gvl :box))))
  (s-value o :top (o-formula (second (gvl :box)))))