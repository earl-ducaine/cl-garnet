;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COMMON-LISP-USER -*-

;;;
;;; A rough attempt to verify that the garnet "tour" works.
;;;

(in-package "COMMON-LISP-USER")



(eval-when (:compile-toplevel :load-toplevel :execute)
  (dolist (pair 
	    '((:radio-buttons "gg:radio-buttons-loader")
	      (:v-slider      "gg:v-slider-loader")))
    (unless (get :garnet-modules (first pair))
      (cl-user::garnet-load (second pair))))
)

(defun do-tour ()

  "An inelegant function to run us through the tour, to check
and see if things seem to be working..."

  (create-instance 'MYWINDOW inter:interactor-window)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (create-instance 'MYAGG opal:aggregate)
  (s-value MYWINDOW :aggregate MYAGG)
  (gv MYWINDOW :aggregate)
  (create-instance 'MYRECT MOVING-RECTANGLE) ; In the COMMON-LISP-USER package
  (opal:add-component MYAGG MYRECT)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (s-value MYRECT :filling-style opal:gray-fill)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (create-instance 'MYTEXT opal:text 
    (:left 200)(:top 80)
    (:string "Hello World"))
  (opal:add-component MYAGG MYTEXT)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (s-value MYTEXT :top 40)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (s-value MYTEXT :top (o-formula (gv MYRECT :top)))
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (s-value MYRECT :top 50)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (create-instance 'MYMOVER inter:move-grow-interactor
     (:start-where (list :in MYRECT))
     (:window MYWINDOW))
  #-(or cmu allegro lucid lispworks apple)  ;only do this if your Lisp is NOT a recent
  (inter:main-event-loop)                   ;version of CMU, Allegro, Lucid, or LispWorks
					;type F1 or \^C to exit when finished.

  (create-instance 'MYTYPER inter:text-interactor
     (:start-where (list :in MYTEXT))
     (:window MYWINDOW)
     (:start-event :rightdown)
     (:stop-event :rightdown))
  #-(or cmu allegro lucid lispworks apple)  ;only do this if your Lisp is NOT a recent
  (inter:main-event-loop)                   ;version of CMU, Allegro, Lucid, or LispWorks
					;type F1 or \^C to exit when finished.

  (s-value MYTEXT :justification :right)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (s-value MYTEXT :justification :center)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")
  
  (create-instance 'MYBUTTONS gg:radio-button-panel
     (:items '(:center :left :right))
     (:left 350)(:top 20)
     (:value :center)
     )
  (opal:add-component MYAGG MYBUTTONS)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")
  #-(or cmu allegro lucid lispworks apple)  ;only do this if your Lisp is NOT a recent
  (inter:main-event-loop)                   ;version of CMU, Allegro, Lucid, or LispWorks
					;type F1 or \^C to exit when finished.

  (s-value MYTEXT :justification (o-formula (gv MYBUTTONS :value)))
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")
  #-(or cmu allegro lucid lispworks apple)  ;only do this if your Lisp is NOT a recent
  (inter:main-event-loop)                   ;version of CMU, Allegro, Lucid, or LispWorks
					;type F1 or \^C to exit when finished.

  (s-value MYBUTTONS :direction :horizontal)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (s-value MYBUTTONS :direction :vertical)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")

  (create-instance 'MYSLIDER gg:v-slider
     (:left 10)(:top 20))
  (opal:add-component MYAGG MYSLIDER)
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")
  #-(or cmu allegro lucid lispworks apple)  ;only do this if your Lisp is NOT a recent
  (inter:main-event-loop)                   ;version of CMU, Allegro, Lucid, or LispWorks
					;type F1 or \^C to exit when finished.

  (s-value MYRECT :filling-style (o-formula
				  (opal:halftone (gv MYSLIDER :value))))
  (opal:update MYWINDOW)
  (y-or-n-p "Continue?")
  #-(or cmu allegro lucid lispworks apple)  ;only do this if your Lisp is NOT a recent
  (inter:main-event-loop)                   ;version of CMU, Allegro, Lucid, or LispWorks
					;type F1 or \^C to exit when finished.

  )
