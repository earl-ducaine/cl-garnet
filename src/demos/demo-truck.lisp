;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-TRUCK; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Demo for the video
;;;
;;; ** Call (demo-truck:Do-Go) to start and (demo-truck:Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers

#|
==================================================================
Change log:
         5/30/94 Marty Geier - changed main window position in do-go
         9/9/90 Brad Myers - for the video
==================================================================
|#


(in-package :DEMO-TRUCK)

(declaim (special WIN1 WIN2 SLIDER SLIDER-AGG TOP-AGG TRUCK
		  MY-BLUE-FILL BIGFONT TIC))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter h1 40)
(defparameter h2 40)
(defparameter w1 110)
(defparameter w2 30)

(defparameter wheeloff 10)
(defparameter wheelsize 20)

(create-instance 'MY-BLUE-FILL opal:gray-fill
		 (:foreground-color opal:blue))

(create-instance 'BIGFONT opal:font (:face :bold))

(create-instance 'TIC opal:line
   (:x1 33)(:x2 47)(:y2 (o-formula (gvl :y1))))

(defparameter image
  (opal:read-image (merge-pathnames "indicator.bm" common-lisp-user::Garnet-Bitmap-PathName)))


(defun do-go (&key dont-enter-main-event-loop double-buffered-p)

  (create-instance 'WIN1 inter:interactor-window
     (:double-buffered-p double-buffered-p)
     (:left 10) (:top 132)
     (:width 640) (:height 253)(:title "Truck")
     (:aggregate (create-instance 'TOP-AGG opal:aggregate)))
  
  (create-instance 'TRUCK opal:aggregadget
     (:box '(10 150 NIL NIL))
     (:top 150)
     (:weight 30)
     (:tooheavy (o-formula (> (gvl :weight) 75)))
     (:parts
      `((:outline ,opal:polyline
	 (:point-list
	  ,(o-formula
	    (let* ((x1 (first (gvl :parent :box))) (x2 (+ x1 w1))
		   (x3 (+ x1 w1 w2)) (y1 (gvl :parent :top))
		   (y2 (+ y1 h1)) (y3 (+ y1 h1 h2)))
	      (list x1 y1 x2 y1 x2 y2 x3 y2 x3 y3 x1 y3 x1 y1))))
	 (:line-style ,opal:line-4)
	 (:filling-style ,(o-formula (if (gvl :parent :tooheavy)
					 opal:red-fill
					 MY-BLUE-FILL))))
	(:wheel1 ,opal:circle
	 (:left ,(o-formula (+ wheeloff
			       (first (gvl :parent :box)))))
	 (:top ,(o-formula (- (opal:gv-bottom (gvl :parent :outline)) 6)))
	 (:width ,wheelsize) (:height ,wheelsize)
	 (:line-style NIL)
	 (:filling-style ,opal:black-fill))
	(:wheel2 ,opal:circle
	 (:left ,(o-formula (- (opal:gv-right (gvl :parent :outline))
			       (gvl :width) 15)))
	 (:top ,(o-formula (- (opal:gv-bottom (gvl :parent :outline)) 6)))
	 (:width ,wheelsize) (:height ,wheelsize)
	 (:line-style NIL)
	 (:filling-style ,opal:black-fill))
	(:milelabel ,opal:text 
	 (:font ,BIGFONT)
	 (:string "Mileage:")
	 (:left ,(o-formula (+ (first (gvl :parent :box)) 5)))
	 (:top ,(o-formula (+ (gvl :parent :top) 8)))
	 (:fill-background-p T))
	(:mileage ,opal:text 
	 (:font ,BIGFONT)
	 (:string ,(o-formula (prin1-to-string (first (gvl :parent :box)))))
	 (:left ,(o-formula (+ 15 (opal:gv-right (gvl :parent :milelabel)))))
	 (:top ,(o-formula (gvl :parent :milelabel :top)))
	 (:fill-background-p T))
	(:weightlabel ,opal:text 
	 (:font ,BIGFONT)
	 (:string "Weight:")
	 (:left ,(o-formula (+ (first (gvl :parent :box)) 5)))
	 (:top ,(o-formula (+ (gvl :parent :top) 50)))
	 (:fill-background-p T))
	(:weightval ,opal:text 
	 (:font ,BIGFONT)
	 (:string ,(o-formula (prin1-to-string (gvl :parent :weight))))
	 (:left ,(o-formula (gvl :parent :mileage :left)))
	 (:top ,(o-formula (gvl :parent :weightlabel :top)))
	 (:fill-background-p T)))))
  (opal:add-component TOP-AGG TRUCK)
  (opal:update WIN1)

  (create-instance 'MOVER inter:move-grow-interactor
		   (:window WIN1)
		   (:start-where (list :in TRUCK))
		   (:running-where T))

  
  (create-instance 'WIN2 inter:interactor-window
     (:left (o-formula (- (gv WIN1 :width) 80)))
     (:top -2)
     (:width 80)
     (:height (o-formula (+ 4 (gv WIN1 :height))))
     (:parent WIN1)
     (:aggregate (create-instance 'SLIDER-AGG Opal:aggregate)))

  (create-instance 'SLIDER opal:aggregadget
     (:weight (o-formula (gvl :indicator :val)))
     (:parts
      `((:label ,opal:text
	 (:font ,BIGFONT)
	 (:left 5)(:top 10)(:string "Set Weight"))
	(:vert-line ,opal:line
	 (:x1 40) (:x2 40) (:y1 40) (:y2 240))
	(:tic1 ,tic (:y1 40))
	(:tic2 ,tic (:y1 90))
	(:tic3 ,tic (:y1 140))
	(:tic4 ,tic (:y1 190))
	(:tic4 ,tic (:y1 240))
	(:indicator ,opal:bitmap
	 (:box (41 140 NIL NIL))
	 (:image ,image)
	 (:left 41)(:draw-function :xor)
	 (:val ,(o-formula (inter:clip-and-map (second (gvl :box))
					       40 240 100 0)))
	 (:top ,(o-formula (- (inter:clip-and-map (gvl :val)
						  100 0 40 240) 7)))))))
		    
  (opal:add-component SLIDER-AGG SLIDER)
  (Opal:Update WIN2)

  (create-instance 'SLIDEIT inter:move-grow-interactor
     (:window WIN2)
     (:start-where (list :in SLIDER))
     (:obj-to-change (g-value SLIDER :indicator))
     (:attach-point :center)
     (:running-where T))
  (s-value TRUCK :weight (o-formula (gv SLIDER :weight)))
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )


(defun Do-stop ()
  (Opal:Destroy win1))

