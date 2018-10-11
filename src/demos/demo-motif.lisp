;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-MOTIF; Base: 10 -*-
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


;;;  DEMO-MOTIF
;;;
;;;  The function in this module creates a window displaying the motif
;;;  gadgets.
;;;
;;;  Some of the gadgets interact so that changing the value of one gadget
;;;  affects the appearance of another gadget.  These constraints are
;;;  implemented through the KR functions "o-formula" and "gv".
;;;
;;;  To run the demo, execute (demo-motif:do-go).  To stop, execute
;;;  (demo-motif:do-stop).
;;;
;;;  Written by Andrew Mickish


(in-package :demo-motif)

;; (defvar demo-motif-init
;;   (dolist (file '("motif-v-scroll-loader"
;; 		  "motif-slider-loader"
;; 		  "motif-text-buttons-loader"
;; 		  "motif-check-buttons-loader"
;; 		  "motif-radio-buttons-loader"
;; 		  "motif-scrolling-labeled-box-loader"
;; 		  "motif-gauge-loader"
;;                   "motif-scrolling-window-loader"))
;;     (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file))))

;; (declaim (special combo-box red-box green-box blue-box red-bar green-bar
;; 		  blue-bar ground-buttons shade-slider shade-buttons
;; 		  demo-motif-win color-buttons color-menu gauge-1
;; 		  shade-box shade-box-border demo-motif-top-agg
;; 		  blue-box-border green-box-border red-box-border
;; 		  combo-box-border text-box-1 scroll-agg))


(defparameter *fill-to-swap* (create-instance nil opal:default-filling-style
				(:foreground-color
				 (create-instance nil opal:black))))


(defun member-string (string list)
  (member string list :test 'string=))

(defun change-color (obj red green blue)
  (let* ((old-fill (g-value obj :filling-style))
	 (new-fill *fill-to-swap*)
	 (color (g-value new-fill :foreground-color)))
    (setf *fill-to-swap* old-fill)
    (s-value color :red red)
    (s-value color :green green)
    (s-value color :blue blue)
    (s-value obj :filling-style new-fill)))

(defun new-color (red-255 green-255 blue-255)
  (let* ((red-100 (round red-255 2.55))
	 (green-100 (round green-255 2.55))
	 (blue-100 (round blue-255 2.55)))
    (cond
      ((g-value opal:color :color-p)
       (let ((red-1 (float (/ red-255 255)))
	     (green-1 (float (/ green-255 255)))
	     (blue-1 (float (/ blue-255 255))))
	 (change-color combo-box red-1 green-1 blue-1)
	 (change-color red-box red-1 0 0)
	 (change-color green-box 0 green-1 0)
	 (change-color blue-box 0 0 blue-1)))

      (t (s-value combo-box
		  :filling-style
		  (opal:halftone (round (+ red-255 green-255 blue-255) 7.65)))
	 (s-value red-box
		  :filling-style
		  (opal:halftone red-100))
	 (s-value green-box
		  :filling-style
		  (opal:halftone green-100))
	 (s-value blue-box
		  :filling-style
		  (opal:halftone blue-100))))

    (s-value red-bar :value red-100)
    (s-value green-bar :value green-100)
    (s-value blue-bar :value blue-100)))


(defun initial-color-fill (color)
  (if (g-value opal:color :color-p)
      (create-instance nil opal:default-filling-style
	 (:foreground-color (create-instance nil color)))
      (opal:halftone 100)))

(defun create-color-fill (color-list)
  (create-instance nil opal:default-filling-style
     (:foreground-color (create-instance nil opal:color
			   (:red (first color-list))
			   (:green (second color-list))
			   (:blue (third color-list))))))

(defun s-value-red-filling-style ()
  (let ((red-value (g-value red-bar :value)))
    (if (g-value opal:color :color-p)
        (change-color red-box (float (/ red-value 100)) 0 0)
        (s-value red-box
		  :filling-style
		  (opal:halftone red-value)))))

(defun s-value-green-filling-style ()
  (let ((green-value (g-value green-bar :value)))
    (if (g-value opal:color :color-p)
        (change-color green-box 0 (float (/ green-value 100)) 0)
        (s-value green-box
		  :filling-style
		  (opal:halftone green-value)))))

(defun s-value-blue-filling-style ()
  (let ((blue-value (g-value blue-bar :value)))
    (if (g-value opal:color :color-p)
        (change-color blue-box 0 0 (float (/ blue-value 100)))
        (s-value blue-box
		  :filling-style
		  (opal:halftone blue-value)))))

(defun s-value-combo-filling-style ()
  (let ((red-value (g-value red-bar :value))
	(green-value (g-value green-bar :value))
	(blue-value (g-value blue-bar :value)))
    (if (g-value opal:color :color-p)
        (change-color combo-box
		     (if (g-value red-bar :visible)
			 (float (/ red-value 100)) 0)
		     (if (g-value green-bar :visible)
			 (float (/ green-value 100)) 0)
		     (if (g-value blue-bar :visible)
			 (float (/ blue-value 100)) 0))
        (s-value combo-box
		 :filling-style
		 (opal:halftone (round (+ red-value green-value blue-value) 3))))))



(defun do-go (&key dont-enter-main-event-loop (double-buffered-p t))

  (let ((thin-gray-line-style (create-instance nil opal:line-style
				(:constant t)
				(:foreground-color opal:motif-gray)))
	(thin-orange-line-style (create-instance nil opal:line-style
				  (:constant t)
				  (:foreground-color opal:motif-orange)))
	(thin-green-line-style (create-instance nil opal:line-style
				 (:constant t)
				 (:foreground-color opal:motif-green)))
	(thin-blue-line-style (create-instance nil opal:line-style
				(:constant t)
				(:foreground-color opal:motif-blue))))

    (create-instance 'demo-motif-win inter:interactor-window
     (:double-buffered-p double-buffered-p)
     ;; filling-style is accessed by the :foreground-color of all the gadgets
     ;; in the window so that they change simultaneously
     (:filling-style
      (o-formula
       (if (gv opal:color :color-p)
	   (let ((value (gv ground-buttons :value)))
	     (cond
	       ((string= value "gray") opal:motif-gray-fill)
	       ((string= value "orange") opal:motif-orange-fill)
	       ((string= value "green") opal:motif-green-fill)
	       ((string= value "blue") opal:motif-blue-fill))))))
     ;; foreground-line-style is accessed by the color box borders
     (:foreground-line-style
      (o-formula
       (if (gv opal:color :color-p)
	   (let ((value (gv ground-buttons :value)))
	     (cond
	       ((string= value "gray") thin-gray-line-style)
	       ((string= value "orange") thin-orange-line-style)
	       ((string= value "green") thin-green-line-style)
	       ((string= value "blue") thin-blue-line-style))))))
     ;; the background color of the window
     (:background-color (o-formula (if (gvl :filling-style)
				       (gvl :filling-style :foreground-color)
				       opal:white)))
     (:title "demo-motif")
     (:left 650)(:top 45)(:width 480)(:height 450)))
  (s-value demo-motif-win
	   :aggregate
	   (create-instance 'demo-motif-top-agg opal:aggregate))

  ;; if we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::garnet-note-quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::garnet-note-quitted "demo-motif"))
     (g-value demo-motif-win :destroy-hooks)))

  (create-instance 'color-buttons garnet-gadgets:motif-check-button-panel
     (:constant t :except :foreground-color)
     (:left 130) (:top 150)
     (:items '("red" "green" "blue"))
     (:foreground-color (o-formula (gv demo-motif-win :background-color)))
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (s-value-combo-filling-style)))
     (:active-p t))


  ;; we bind color-box-proto as a variable instead of making a named schema
  ;; so that if we call do-go multiple times, we will not get destroy messages.
  (let ((color-box-proto
	 (create-instance nil opal:rectangle
	   (:width 25) (:height 25)
	   (:fast-redraw-p (if (g-value opal:color :color-p) :rectangle t))
	   (:draw-function (if (g-value opal:color :color-p) :copy :xor))
	   (:fast-redraw-line-style (o-formula (gv demo-motif-win
						   :foreground-line-style)))
	   (:fast-redraw-filling-style (o-formula
					(gv demo-motif-win :filling-style))))))

  (create-instance 'red-box-border color-box-proto
     (:left 10) (:top 10)
     (:visible (o-formula (member-string "red" (gv color-buttons :value)))))
  (create-instance 'red-box color-box-proto
     (:left 11) (:top 11) (:height 23) (:width 23)
     (:visible (o-formula (member-string "red" (gv color-buttons :value))))
     (:line-style nil)
     (:filling-style (initial-color-fill opal:red)))

  (create-instance 'green-box-border color-box-proto
     (:left 45) (:top 10)
     (:visible (o-formula (member-string "green" (gv color-buttons :value)))))
  (create-instance 'green-box color-box-proto
     (:left 46) (:top 11) (:height 23) (:width 23)
     (:visible (o-formula (member-string "green" (gv color-buttons :value))))
     (:line-style nil)
     (:filling-style (initial-color-fill opal:green)))

  (create-instance 'blue-box-border color-box-proto
     (:left 80) (:top 10)
     (:visible (o-formula (member-string "blue" (gv color-buttons :value)))))
  (create-instance 'blue-box color-box-proto
     (:left 81) (:top 11) (:height 23) (:width 23)
     (:visible (o-formula (member-string "blue" (gv color-buttons :value))))
     (:line-style nil)
     (:filling-style (initial-color-fill opal:blue)))

  (create-instance 'combo-box-border color-box-proto
     (:left 10) (:top 245) (:width 95) (:height 25)
     (:visible (o-formula (gv color-buttons :value))))
  (create-instance 'combo-box color-box-proto
     (:left 11) (:top 246) (:width 93) (:height 23)
     (:visible (o-formula (gv color-buttons :value)))
     (:line-style nil)
     (:filling-style (initial-color-fill opal:white)))

  (create-instance 'shade-box-border color-box-proto
     (:left 243) (:top 10)
     (:filling-style (initial-color-fill opal:white)))
  (create-instance 'shade-box color-box-proto
     (:left 244) (:top 11) (:width 23) (:height 23)
     (:line-style nil)
     (:filling-style (initial-color-fill opal:white)))
  ) ;; close binding of color-box-proto

  (create-instance 'ground-buttons garnet-gadgets:motif-radio-button-panel
     (:constant t :except :foreground-color)
     (:left 130) (:top 45)
     (:v-spacing 4)
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:items `("gray" "orange" "green" "blue"))
     (:active-p t)
     (:selection-function
      #'(lambda (g v)
	  (declare (ignore g v))
	  (kr-send shade-slider :selection-function
		   shade-slider (g-value shade-slider :value)))))


  (create-instance 'color-menu garnet-gadgets:motif-menu
     (:constant t :except :foreground-color)
     (:left 300) (:top 20)
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:final-feedback-p nil)
     (:items `(
	    ;; needs blue
	       ("navy blue" ,#'(lambda (g v)
				 (declare (ignore g v))
				 (new-color 0 0 128)))
	    ;; needs red and green
	       ("gold" ,#'(lambda (g v)
			    (declare (ignore g v))
			    (new-color 255 215 0)))
	    ;; needs red and blue
	       ("violet" ,#'(lambda (g v)
			      (declare (ignore g v))
			      (new-color 148 0 211)))
	    ;; needs green and blue
	       ("turquoise" ,#'(lambda (g v)
				 (declare (ignore g v))
				 (new-color 0 206 209)))
	    ;; needs everything
	       ("plum" ,#'(lambda (g v)
			    (declare (ignore g v))
			    (new-color 221 160 221)))
	       ("sienna" ,#'(lambda (g v)
			      (declare (ignore g v))
			      (new-color 160 82 45)))
	       ("motif-gray" ,#'(lambda (g v)
				  (declare (ignore g v))
				  (new-color 211 211 211)))
	       ("motif-green" ,#'(lambda (g v)
				   (declare (ignore g v))
				   (new-color 95 158 160)))
	       ("motif-blue" ,#'(lambda (g v)
				  (declare (ignore g v))
				  (new-color 114 159 255)))))
     (:accelerators '((#\n "f2" :f2) (#\o "f3" :f3) (#\v "f4" :f4)
		      (#\t "f5" :f5) (#\p "f6" :f6) (#\s "f7" :f7)
		      (#\g "f8" :f8) (#\r "f9" :f9) (#\b "f10" :f10)))
     (:inactive-items
      (o-formula
       (let* ((value (gv color-buttons :value))
	      (red-p (member-string "red" value))
	      (green-p (member-string "green" value))
	      (blue-p (member-string "blue" value)))
	 (append
	  (unless blue-p (list "navy blue"))
	  (unless (and red-p green-p) (list "gold"))
	  (unless (and red-p blue-p) (list "violet"))
	  (unless (and green-p blue-p) (list "turquoise"))
	  (unless (and red-p green-p blue-p)
	    (list "plum" "sienna" "motif-gray" "motif-green" "motif-blue"))))))

     (:bar-above-these-items '("motif-gray")))


  (create-instance 'shade-slider garnet-gadgets:motif-slider
     (:constant t :except :foreground-color)
     (:left 220) (:top 40)
     (:val-1 (o-formula (if (gv opal:color :color-p) 150 100)))
     (:val-2 0)
     (:scr-incr 5)
     (:foreground-color (o-formula (gv demo-motif-win :background-color)))
     (:active-p t)
     (:selection-function
      #'(lambda (gadget value)
	  (if (g-value opal:color :color-p)
	      (multiple-value-bind (r g b)
				   (garnet-gadgets::convert-aux
		                      (g-value gadget :foreground-color)
		                      (float (/ value 100)))
		 (change-color shade-box r g b))
	      (s-value shade-box
			 :filling-style
			 (opal:halftone value))))))

  (create-instance 'shade-buttons garnet-gadgets::motif-text-button-panel
     (:constant t :except :foreground-color)
     (:left 130) (:top 245)
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:final-feedback-p nil)
     (:direction :horizontal)
     (:fixed-width-p nil)
     (:items `(("highlight"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((hc (g-value shade-slider :highlight-fill
					    :foreground-color)))
			   (s-value shade-slider
				    :value
				    (round (* 100 gg::*highlight-value*)))
			   (change-color shade-box (g-value hc :red)
			     (g-value hc :green) (g-value hc :blue)))
			 (progn
			   (s-value shade-slider :value 50)
			   (s-value shade-box
				    :filling-style
				    (opal:halftone 50))))))
	       ("foreground"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((fc (g-value shade-slider :foreground-color)))
			   (s-value shade-slider :value 100)
			   (change-color shade-box (g-value fc :red)
			     (g-value fc :green) (g-value fc :blue)))
			 (progn
			   (s-value shade-slider :value 0)
			   (s-value shade-box
				    :filling-style
				    (opal:halftone 0))))))
	       ("background"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((bc (g-value shade-slider :background-fill
					    :foreground-color)))
			   (s-value shade-slider
				    :value
				    (round (* 100 gg::*background-value*)))
			   (change-color shade-box (g-value bc :red)
			     (g-value bc :green) (g-value bc :blue)))
			 (progn
			   (s-value shade-slider :value 0)
			   (s-value shade-box
				    :filling-style
				    (opal:halftone 0))))))

	       ("shadow"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((sc (g-value shade-slider :shadow-fill
					    :foreground-color)))
			   (s-value shade-slider
				    :value
				    (round (* 100 gg::*shadow-value*)))
			   (change-color shade-box (g-value sc :red)
			     (g-value sc :green) (g-value sc :blue)))
			 (progn
			   (s-value shade-slider :value 100)
			   (s-value shade-box
				    :filling-style
				    (opal:halftone 100)))))))))

  (create-instance 'text-box-1 garnet-gadgets::motif-scrolling-labeled-box
     (:constant t :except :foreground-color)
     (:left 280) (:top 300)
     (:width 170)
     (:label-string "title:")
     (:value "motif gauge")
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:field-offset 5))

  (create-instance 'gauge-1 garnet-gadgets:motif-gauge
     (:constant t :except :foreground-color :title)
     (:left 20) (:top 280)
     (:title (o-formula (gv text-box-1 :value)))
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:parts
      `(:base-line :semi-circ :tic-marks :needle1 :needle2
        (:gauge-title :modify
                      (:fast-redraw-p :rectangle)
                      (:fast-redraw-filling-style
                       ,(o-formula
                         (gv demo-motif::demo-motif-win :filling-style))))
        (:value-feedback :modify
                         (:fast-redraw-p :rectangle)
                         (:fast-redraw-filling-style
                          ,(o-formula
                            (gv demo-motif::demo-motif-win :filling-style))))
        :sel-box)))

  (create-instance 'red-bar garnet-gadgets:motif-v-scroll-bar
     (:constant t :except :visible :foreground-color)
     (:left 14) (:top 40)
     (:val-1 100) (:val-2 0)
     (:page-incr 20)
     (:visible (o-formula (member-string "red" (gv color-buttons :value))))
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:active-p t)
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (s-value-red-filling-style)
	  (s-value-combo-filling-style))))

  (create-instance 'green-bar garnet-gadgets:motif-v-scroll-bar
     (:constant t :except :visible :foreground-color)
     (:left 49) (:top 40)
     (:val-1 100) (:val-2 0)
     (:page-incr 20)
     (:visible (o-formula (member-string "green" (gv color-buttons :value))))
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:active-p t)
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (s-value-green-filling-style)
	  (s-value-combo-filling-style))))

  (create-instance 'blue-bar garnet-gadgets:motif-v-scroll-bar
     (:constant t :except :visible :foreground-color)
     (:left 84) (:top 40)
     (:val-1 100) (:val-2 0)
     (:page-incr 20)
     (:visible (o-formula (member-string "blue" (gv color-buttons :value))))
     (:foreground-color (o-formula (gv demo-motif-win
				       :background-color)))
     (:active-p t)
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (s-value-blue-filling-style)
	  (s-value-combo-filling-style))))

  (create-instance 'scroll-agg opal:aggregate)
  (opal:add-components scroll-agg red-bar green-bar blue-bar
		       red-box-border red-box green-box-border green-box
		       blue-box-border blue-box combo-box-border combo-box)

  (opal:add-components demo-motif-top-agg
		       scroll-agg color-buttons ground-buttons
		       shade-box-border shade-box shade-slider shade-buttons
		       color-menu text-box-1 gauge-1)

  (opal:update demo-motif-win)


  ;;; make sure :value formulas are initialized
  (g-value red-bar :value)
  (g-value green-bar :value)
  (g-value blue-bar :value)
  (g-value ground-buttons :value)


  ;;; initialize values in gadgets
  (s-value ground-buttons :value "gray")


  ;;; set up global tab interactor
  ;;;
  (s-value color-buttons :keyboard-selection-p t)

  (create-instance 'demo-motif-tab-inter garnet-gadgets:motif-tab-inter
     (:window demo-motif-win)
     (:objects (list color-buttons ground-buttons
		     red-bar green-bar blue-bar
		     shade-slider color-menu shade-buttons text-box-1 gauge-1)))

  ;;; set up global accelerator interactor for the menu
  ;;;
  (create-instance 'color-menu-inter garnet-gadgets:motif-menu-accelerator-inter
     (:window demo-motif-win)
     (:menus (list color-menu)))


  (opal:update demo-motif-win)

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  (format t "~%demo-motif:
      this demo shows how garnet is able to simulate the look and feel of the
   motif widgets.  in addition to operating the gadgets by mouse movements,
   you can select buttons and manipulate the scroll bars through keyboard
   interaction.  press the tab key (or control-tab) to move the keyboard
   selection around the window.  use the spacebar to select buttons, and the
   return key to select menu items.  the arrow keys will move the selection
   within a gadget (i.e., select another button in the same panel), and will
   also move the scroll bars and sliders.~%")

  )



(defun do-stop ()
  (opal:destroy demo-motif-win))
