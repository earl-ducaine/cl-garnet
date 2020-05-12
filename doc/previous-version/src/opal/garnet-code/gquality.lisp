(in-package "USER" :use '("KR" "KR-DEBUG" "GARNET-DEBUG"))

(defvar INIT-GQ
  (garnet-load "aggregadgets:aggregraphs-loader")
  (garnet-load "ps:ps-loader"))

(when (boundp 'WIN) (opal:destroy WIN))

(create-instance 'WIN inter:interactor-window
  (:left 600)(:top 30)(:width 400)(:height 455))
(s-value WIN :aggregate (create-instance 'TOP-AGG opal:aggregate))
(opal:update WIN)

(defparameter *slots*
  (list (list opal:graphic-quality)
	(cons opal:filling-style ":fill-style :solid~%:fill-rule :even-odd~%:stipple NIL~%:foreground-color opal:black~%:background-color opal:white")
	(cons opal:line-style ":line-thickness 0~%:line-style :solid~%:cap-style :butt~%:join-style :miter~%:dash-pattern NIL~%:stipple NIL~%:foreground-color opal:black~%:background-color opal:white")
	(cons opal:color ":red 1.0~%:green 1.0~%:blue 1.0~%:color-name NIL")
	(cons opal:font ":family :fixed~%:face :roman~%:size :medium")
	(cons opal:font-from-file ":font-name \"\"~%:font-path NIL")
	))

(create-instance 'NODE-PROTO opal:aggregadget
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
    (:left 10) (:top 10)
    (:h-spacing 30)
    (:node-prototype NODE-PROTO)
    (:children-function #'(lambda (obj depth)
			    (when (<= depth 1)
			      (remove NIL
			        (mapcar #'(lambda (obj2)
					    (if (assoc obj2 *slots*) obj2))
					(g-value obj :is-a-inv))))))
    (:info-function #'identity)
    (:source-roots (list opal:graphic-quality)))

(opal:add-components TOP-AGG GRAPH)
(opal:update WIN)


#|
(create-instance 'M gg:multi-graphics-selection
  (:start-where (list :element-of (g-value GRAPH :nodes))))
(opal:add-component TOP-AGG M)

(create-instance 'I inter:move-grow-interactor
  (:start-where (list :element-of (g-value GRAPH :nodes)))
  (:window WIN)
  (:slots-to-set (list T T NIL NIL)))

(let ((node (kr-send graph :source-to-graph-node graph inter:interactor-window)))
  (if (string= (g-value node :text-al :string) "INTERACTORS:INTERACTOR-WINDOW")
      (s-value node :text-al :string "INTER:INTERACTOR-WINDOW")
      (error "Andy says -- WRONG NODE!")))
(opal:update WIN)


(dolist (o (g-value NODE-PROTO :is-a-inv))
  (s-value o :box (list (g-value o :left) (g-value o :top) NIL NIL))
  (s-value o :left (o-formula (first (gvl :box))))
  (s-value o :top (o-formula (second (gvl :box)))))
|#
