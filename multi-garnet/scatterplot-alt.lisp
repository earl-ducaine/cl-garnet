;;;-*- Mode: COMMON-LISP; Package: COMMON-LISP-USER -*-

(in-package :multi-garnet)



;; An axis line is actually a rectangle, so it will always be horizontal or
;; vertical (no arbitrary angles), and so it can be resized by
;; move-grow-interactor.  It is filled in so it looks like a line.
(create-instance
 '*axis-rectangle* Opal:Rectangle
 (:line-style Opal:thin-line)
 (:filling-style Opal:black-fill)
 (:box (list 100 100 100 100))
 (:left 100)
 (:top 100)
 (:width 100)
 (:height 100)
 ;; one-directional constraints unpacking :box
 (:left-cn (m-constraint :max (box left) (setf left (first box))))
 (:top-cn (m-constraint :max (box top) (setf top (second box))))
 (:width-cn (m-constraint :max (box width) (setf width (third box))))
 (:height-cn (m-constraint :max (box height) (setf height (fourth box))))
 )

(create-instance
    '*v-axis* opal:aggregadget
  (:parts `((:main ,*axis-rectangle*)))
  (:label "")
  (:screen-min 0)
  (:screen-max 100)
  (:world-min 0.0)
  (:world-max 1.0)
  (:offset 0.0)
  (:scale 1.0)
  (:axis-cn (m-constraint :max (screen-min screen-max world-min world-max offset scale)
			  (setf (offset scale)
				(let* ((scale (safe/ (- screen-max screen-min)
						     (- world-max world-min)))
				       (offset (- screen-min (* scale world-min))))
				  (values offset scale)))
			  (setf (world-min world-max)
				(let* ((world-min (safe/ (- screen-min offset) scale))
				       (world-max (+ (safe/ (- screen-max screen-min) scale)
						     world-min)))
				  (values world-min world-max)))
			  (setf (screen-min screen-max)
				(let* ((screen-min (+ (* world-min scale) offset))
				       (screen-max (+ (* (- world-max world-min) scale)
						      screen-min)))
				  (values (round screen-min) (round screen-max))))
			  ))
  ;; :box-cn relates the axis rectangle :box (left top width height) to
  ;; :screen-min, :screen-max, and :x.  Floats are rounded before
  ;; being stored in :box.
  (:x 20)
  (:box-cn (m-constraint :max ((box (gvl :main :box))
			       screen-min screen-max x)
			 (setf (screen-min screen-max x)
			       (let* ((left (first box))
				      (top (second box))
				      (height (fourth box))
				      (bottom (+ top height)))
				 (values bottom top left)))
			 (setf box (list (round x)
					 (round screen-max)
					 *axis-width*
					 (round (- screen-min screen-max))
					 )))))

(create-instance
    '*v-axis* opal:aggregadget
  (:parts `((:main ,*axis-rectangle*))))


;; (CREATE-SCHEMA '*V-AXIS* :GENERATE-INSTANCE (:IS-A OPAL:AGGREGADGET)
;;                 (:PARTS `((:MAIN ,*AXIS-RECTANGLE*))))

;; (KR::DO-SCHEMA-BODY (KR::MAKE-A-NEW-SCHEMA '*V-AXIS*) OPAL:AGGREGADGET T T NIL
;;                     NIL (CONS :PARTS `((:MAIN ,*AXIS-RECTANGLE*))))

;; (kr-init-method (KR::MAKE-A-NEW-SCHEMA '*V-AXIS*) nil)
