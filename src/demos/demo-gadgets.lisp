;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MENU; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$	


;;;  DEMO-GADGETS
;;;
;;;  The function in this module creates a window showing several gadgets.
;;;
;;;  Some of the gadgets interact so that changing the value of one gadget
;;;  affects the appearance of another gadget.  These constraints are
;;;  implemented through the KR functions "o-formula" and "gv".
;;;
;;;  To run the demo, execute (demo-gadgets:do-go).  To stop, execute
;;;  (demo-gadgets:do-stop).
;;;
;;;  Written by Andrew Mickish

(in-package :DEMO-GADGETS)

(defvar DEMO-GADGETS-INIT
  (dolist (file '("h-scroll-loader" "v-slider-loader"
		  "trill-device-loader" "gauge-loader"
		  "menu-loader" "text-buttons-loader"
		  "x-buttons-loader" "radio-buttons-loader"
		  "labeled-box-loader" "scrolling-labeled-box-loader"))
          (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file))))
                                   

(declaim (special DEMO-GADGETS-WIN DEMO-GADGETS-TOP-AGG
		  DEMO-TEXT-BUTTONS-OBJ DEMO-H-SCROLL-OBJ DEMO-GAUGE-OBJ
		  DEMO-GAUGE2-OBJ DEMO-V-SLIDER-OBJ DEMO-X-BUTTONS-OBJ
		  DEMO-RADIO-BUTTONS-OBJ DEMO-MENU-OBJ DEMO-LAB-OBJ
		  DEMO-TRILL-OBJ DEMO-SCROLLING-STRING-OBJ))


;;  Global function executed when certain demo items are selected.
(defun IDENTIFY-ITEM (top-level-obj string)
  (format t "Object ~S now has value ~S.~%" top-level-obj string))
  

;;;  These functions are included to show that selection of one of the
;;;  menu items causes the associated local function to be called.
(defun my-cut (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function CUT called~%"))
(defun my-copy (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function COPY called~%"))
(defun my-paste (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function PASTE called~%"))
(defun my-undo (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function UNDO called~%"))
(defun my-cancel (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function CANCEL called~%"))


(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)

  (create-instance 'DEMO-GADGETS-WIN inter:interactor-window
     (:title "GARNET Gadgets")
     (:double-buffered-p double-buffered-p)
     (:left 10) (:top 50) (:width 600) (:height 600))

  (s-value DEMO-GADGETS-WIN
	   :aggregate
	   (create-instance 'DEMO-GADGETS-TOP-AGG opal:aggregate))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-GADGETS"))
     (g-value demo-gadgets-win :destroy-hooks)))

  
  (create-instance 'DEMO-TEXT-BUTTONS-OBJ garnet-gadgets:text-button-panel
     (:constant T)
     (:left 10) (:top 10)
     (:items '("Chopin" "Mozart" "Beethoven" "Bach" "Strauss"))
     (:selection-function #'identify-item))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-TEXT-BUTTONS-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-X-BUTTONS-OBJ garnet-gadgets:x-button-panel
     (:constant T)
     (:left 400) (:top 300)
     (:items '("scr-trill-p" "page-trill-p" "indicator-text-p"
	       "int-feedback-p")))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-X-BUTTONS-OBJ)


  (create-instance 'DEMO-H-SCROLL-OBJ garnet-gadgets:h-scroll-bar
     (:constant T :except :scr-trill-p :page-trill-p 
			  :indicator-text-p :int-feedback-p)
     (:left 10) (:top 350)
     (:scr-trill-p
      (o-formula (member "scr-trill-p" (gv demo-x-buttons-obj :value)
			 :test #'equal)))
     (:page-trill-p
      (o-formula (member "page-trill-p" (gv demo-x-buttons-obj :value)
			 :test #'equal)))
     (:indicator-text-p
      (o-formula (member "indicator-text-p" (gv demo-x-buttons-obj :value)
			 :test #'equal)))
     (:int-feedback-p
      (o-formula (member "int-feedback-p" (gv demo-x-buttons-obj :value)
			 :test #'equal))))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-H-SCROLL-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-GAUGE-OBJ garnet-gadgets:gauge
     (:constant T)
     (:left 30) (:top 430)
     (:title "Temperature")
     (:int-feedback-p NIL)
     )
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-GAUGE-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-GAUGE2-OBJ garnet-gadgets:gauge
     (:constant T)
     (:left 300) (:top 455)
     (:val-1 45) (:val-2 90)
     (:width 200) (:num-marks 12)
     (:title "Pressure")
     (:polygon-needle-p NIL))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-GAUGE2-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-V-SLIDER-OBJ garnet-gadgets:v-slider
     (:constant T)
     (:left 175) (:top 10)
     (:val-1 0) (:val-2 50)
     (:num-marks 6))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-V-SLIDER-OBJ)
  (opal:update DEMO-GADGETS-WIN)



  (create-instance 'DEMO-RADIO-BUTTONS-OBJ garnet-gadgets:radio-button-panel
     (:constant T)
     (:left 420) (:top 60)
     (:items '(:left :center :right)))
  (g-value DEMO-RADIO-BUTTONS-OBJ :value)
  (s-value DEMO-RADIO-BUTTONS-OBJ :value :left)
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-RADIO-BUTTONS-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-LAB-OBJ garnet-gadgets:labeled-box
     (:constant T)
     (:left 280) (:top 10)
     (:label-string "Title:")
     (:value "Menu"))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-LAB-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-MENU-OBJ garnet-gadgets:menu
     (:constant T :except :h-align :title)
     (:left 300) (:top 50)
     (:selection-function #'identify-item)
     (:h-align (o-formula (gv demo-radio-buttons-obj :value)))
     (:title (o-formula (gv demo-lab-obj :value)))
     (:items '(("Cut" my-cut) ("Copy" my-copy)
	       ("Paste" my-paste) ("Undo" my-undo))))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-MENU-OBJ)
  (opal:update DEMO-GADGETS-WIN)


  (create-instance 'DEMO-TRILL-OBJ garnet-gadgets:trill-device
     (:constant T)
     (:left 300) (:top 225)
     (:val-2 NIL))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-TRILL-OBJ)

  (create-instance 'DEMO-SCROLLING-STRING-OBJ garnet-gadgets:scrolling-labeled-box
     (:constant T)
     (:left 10) (:top 300)
     (:width 250)(:label-string "Scrolling String:")
     (:value "Long strings can be input and scrolled horizontally"))
  (opal:add-components DEMO-GADGETS-TOP-AGG DEMO-SCROLLING-STRING-OBJ)

  (opal:update DEMO-GADGETS-WIN)

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  )


(defun Do-Stop ()
  (opal:destroy DEMO-GADGETS-WIN))
