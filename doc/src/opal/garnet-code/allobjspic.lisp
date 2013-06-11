(use-package "KR")
(use-package "OPAL")
(when (boundp 'w) (opal:destroy w))
(create-instance 'w window (:aggregate (create-instance 'a aggregate)))

(create-instance 'l Line (:x1 50)(:y1 10)(:x2 10)(:y2 50))
(add-component a l)

(create-instance 'r Rectangle (:left 60)(:top 10)(:width 60)(:height 30)
		 (:filling-style Opal:light-gray-fill))
(add-component a r)

(create-instance 'r2 Roundtangle (:left 140)(:top 10)(:width 80)(:height 60)
		 (:filling-style opal:diamond-fill)
		 (:line-style opal:line-2))
(add-component a r2)

(create-instance 's1 text (:string "Hello World")(:left 250)(:top 20)
		 (:font (create-instance NIL opal:font
			  (:face :bold-italic)
			  (:size :large))))
			 
			  
(add-component a s1)

#|
(create-instance 'mp multipoint
		 (:point-list '(30 80 50 100 50 120 30 140 10 120 10 100)))
(add-component a mp)
|#

;; There is no method for postscripting multipoints, and besides, the points
;; would be too small to show up.  So we'll simulate a multipoint object with
;; a bunch of circles.
(do* ((point-list '(30 80 50 100 50 120 30 140 10 120 10 100)
		  (cddr point-list))
      (left (first point-list) (first point-list))
      (top (second point-list) (second point-list))
      (obj (create-instance NIL opal:circle
	     (:left left) (:top top)
	     (:width 3) (:height 3)
	     (:filling-style opal:black-fill))
	   (create-instance NIL opal:circle
	     (:left left) (:top top)
	     (:width 3) (:height 3)
	     (:filling-style opal:black-fill))))
     ((null point-list) T)
  (opal:add-component a obj))
     


(create-instance 'ml polyline
		 (:point-list '(90 80 110 100 110 120 90 140 70 120 70 100 90 80))
		 (:line-style NIL)
		 (:filling-style (halftone 10)))

(add-component a ml)

(create-instance 'ar arrowhead (:head-x 150)(:head-y 110)(:from-x 120)(:from-y 150)
		 (:line-style opal:line-2)(:length 20)(:diameter 20))

(add-component a ar)

(create-instance 'ov oval (:left 170)(:top 80)(:width 80)(:height 60)
		 (:line-style opal:dotted-line))
(add-component a ov)

(create-instance 'ci circle (:left 270)(:top 80)(:width 50)(:height 50)
		 (:filling-style black-fill)
		 (:line-style NIL))
(add-component a ci)

(create-instance 's2 multi-text (:string "Garnet supports
multi-line
text
strings!")(:left 350)(:top 80)(:justification :center))
(add-component a s2)

(create-instance 'ac arc (:left 10)(:top 160)(:width 100)(:height 50)
		 (:filling-style NIL)
		 (:angle1 0)
		 (:angle2 PI)
		 (:line-style dashed-line))
(add-component a ac)

(create-instance 'bm bitmap (:left 130)(:top 160)
	 (:image (read-image "/afs/cs/project/garnet/test/lib/gilt/giltbitmap.bitmap")))
(add-component a bm)
(update w)


