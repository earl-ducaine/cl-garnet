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


;;;  CHANGE LOG:
;;;
;;;  07/02/93 Andrew Mickish - Excepted :keyboard-selection-p in constant lists
;;;  05/26/93 Andrew Mickish - Fixed constant declarations for new aggrelists
;;;  05/20/93 Andrew Mickish - Put scroll bars in their own aggregate;
;;;            made some feedback objects fast-redraw; made scroll bars
;;;            constant again.
;;;  07/03/92 Ed Pervin - Temporarily made red-bar, green-bar, blue-bar
;;;			be non-constant due to bug in KR 2.0.10.
;;;  04/08/92 Andrew Mickish - Changed :items functions of SHADE-BUTTONS to
;;;            set the SHADE-SLIDER with gg::*highlight-value*, etc.
;;;  03/10/92 Ed Pervin - Changed (create-instance NIL opal:black)
;;;			to just opal:black in many places.
;;;  02/28/92 Andrew Mickish - Modified to work with hash-table-oriented
;;;            implementation of gadget colors.
;;;  02/26/92 Andrew Mickish - Added border rectangles to color boxes
;;;  02/17/92 Andrew Mickish - Added constant slots
;;;  11/25/91 Ed Pervin      - Allowed background color for windows, so
;;;			       we don't have to use an actual rectangle.
;;;  10/08/91 Andrew Mickish - Added fast-redraw to color rectangles
;;;  03/21/91 Andrew Mickish - Added :selection-function of GROUND-BUTTONS
;;;  03/01/91 Andrew Mickish - Created
;;;


(in-package :DEMO-MOTIF)

(defvar DEMO-MOTIF-INIT
  (dolist (file '("motif-v-scroll-loader" "motif-slider-loader"
		  "motif-text-buttons-loader" "motif-check-buttons-loader"
		  "motif-radio-buttons-loader" "motif-menu-loader"
		  "motif-scrolling-labeled-box-loader" "motif-gauge-loader"
                  "motif-scrolling-window-loader"))
    (common-lisp-user::garnet-load (concatenate 'string "gadgets:" file))))

(declaim (special COMBO-BOX RED-BOX GREEN-BOX BLUE-BOX RED-BAR GREEN-BAR
		  BLUE-BAR GROUND-BUTTONS SHADE-SLIDER SHADE-BUTTONS
		  DEMO-MOTIF-WIN COLOR-BUTTONS COLOR-MENU GAUGE-1
		  SHADE-BOX SHADE-BOX-BORDER DEMO-MOTIF-TOP-AGG
		  BLUE-BOX-BORDER GREEN-BOX-BORDER RED-BOX-BORDER
		  COMBO-BOX-BORDER TEXT-BOX-1 SCROLL-AGG))


(defparameter *FILL-TO-SWAP* (create-instance NIL opal:default-filling-style
				(:foreground-color
				 (create-instance NIL opal:black))))


(defun MEMBER-STRING (string list)
  (member string list :test 'string=))

(defun CHANGE-COLOR (obj red green blue)
  (let* ((old-fill (g-value obj :filling-style))
	 (new-fill *FILL-TO-SWAP*)
	 (color (g-value new-fill :foreground-color)))
    (setf *FILL-TO-SWAP* old-fill)
    (s-value color :red red)
    (s-value color :green green)
    (s-value color :blue blue)
    (s-value obj :filling-style new-fill)))

(defun NEW-COLOR (red-255 green-255 blue-255)
  (let* ((red-100 (round red-255 2.55))
	 (green-100 (round green-255 2.55))
	 (blue-100 (round blue-255 2.55)))
    (cond
      ((g-value opal:color :color-p)
       (let ((red-1 (float (/ red-255 255)))
	     (green-1 (float (/ green-255 255)))
	     (blue-1 (float (/ blue-255 255))))
	 (change-color COMBO-BOX red-1 green-1 blue-1)
	 (change-color RED-BOX red-1 0 0)
	 (change-color GREEN-BOX 0 green-1 0)
	 (change-color BLUE-BOX 0 0 blue-1)))

      (t (s-value COMBO-BOX
		  :filling-style
		  (opal:halftone (round (+ red-255 green-255 blue-255) 7.65)))
	 (s-value RED-BOX
		  :filling-style
		  (opal:halftone red-100))
	 (s-value GREEN-BOX
		  :filling-style
		  (opal:halftone green-100))
	 (s-value BLUE-BOX
		  :filling-style
		  (opal:halftone blue-100))))

    (s-value RED-BAR :value red-100)
    (s-value GREEN-BAR :value green-100)
    (s-value BLUE-BAR :value blue-100)))


(defun INITIAL-COLOR-FILL (color)
  (if (g-value opal:color :color-p)
      (create-instance NIL opal:default-filling-style
	 (:foreground-color (create-instance NIL color)))
      (opal:halftone 100)))

(defun CREATE-COLOR-FILL (color-list)
  (create-instance NIL opal:default-filling-style
     (:foreground-color (create-instance NIL opal:color
			   (:red (first color-list))
			   (:green (second color-list))
			   (:blue (third color-list))))))

(defun S-VALUE-RED-FILLING-STYLE ()
  (let ((red-value (g-value RED-BAR :value)))
    (if (g-value opal:color :color-p)
        (change-color RED-BOX (float (/ red-value 100)) 0 0)
        (s-value RED-BOX
		  :filling-style
		  (opal:halftone red-value)))))

(defun S-VALUE-GREEN-FILLING-STYLE ()
  (let ((green-value (g-value GREEN-BAR :value)))
    (if (g-value opal:color :color-p)
        (change-color GREEN-BOX 0 (float (/ green-value 100)) 0)
        (s-value GREEN-BOX
		  :filling-style
		  (opal:halftone green-value)))))

(defun S-VALUE-BLUE-FILLING-STYLE ()
  (let ((blue-value (g-value BLUE-BAR :value)))
    (if (g-value opal:color :color-p)
        (change-color BLUE-BOX 0 0 (float (/ blue-value 100)))
        (s-value BLUE-BOX
		  :filling-style
		  (opal:halftone blue-value)))))

(defun S-VALUE-COMBO-FILLING-STYLE ()
  (let ((red-value (g-value RED-BAR :value))
	(green-value (g-value GREEN-BAR :value))
	(blue-value (g-value BLUE-BAR :value)))
    (if (g-value opal:color :color-p)
        (change-color COMBO-BOX
		     (if (g-value RED-BAR :visible)
			 (float (/ red-value 100)) 0)
		     (if (g-value GREEN-BAR :visible)
			 (float (/ green-value 100)) 0)
		     (if (g-value BLUE-BAR :visible)
			 (float (/ blue-value 100)) 0))
        (s-value COMBO-BOX
		 :filling-style
		 (opal:halftone (round (+ red-value green-value blue-value) 3))))))



(defun DO-GO (&key dont-enter-main-event-loop (double-buffered-p T))

  (let ((thin-gray-line-style (create-instance NIL opal:line-style
				(:constant T)
				(:foreground-color opal:MOTIF-GRAY)))
	(thin-orange-line-style (create-instance NIL opal:line-style
				  (:constant T)
				  (:foreground-color opal:MOTIF-ORANGE)))
	(thin-green-line-style (create-instance NIL opal:line-style
				 (:constant T)
				 (:foreground-color opal:MOTIF-GREEN)))
	(thin-blue-line-style (create-instance NIL opal:line-style
				(:constant T)
				(:foreground-color opal:MOTIF-BLUE))))
  
  (create-instance 'DEMO-MOTIF-WIN inter:interactor-window
     (:double-buffered-p double-buffered-p)
     ;; Filling-style is accessed by the :foreground-color of all the gadgets
     ;; in the window so that they change simultaneously
     (:filling-style
      (o-formula
       (if (gv opal:color :color-p)
	   (let ((value (gv GROUND-BUTTONS :value)))
	     (cond
	       ((string= value "Gray") opal:MOTIF-GRAY-FILL)
	       ((string= value "Orange") opal:MOTIF-ORANGE-FILL)
	       ((string= value "Green") opal:MOTIF-GREEN-FILL)
	       ((string= value "Blue") opal:MOTIF-BLUE-FILL))))))
     ;; Foreground-line-style is accessed by the color box borders
     (:foreground-line-style
      (o-formula
       (if (gv opal:color :color-p)
	   (let ((value (gv GROUND-BUTTONS :value)))
	     (cond
	       ((string= value "Gray") thin-gray-line-style)
	       ((string= value "Orange") thin-orange-line-style)
	       ((string= value "Green") thin-green-line-style)
	       ((string= value "Blue") thin-blue-line-style))))))
     ;; The background color of the window
     (:background-color (o-formula (if (gvl :filling-style)
				       (gvl :filling-style :foreground-color)
				       opal:WHITE)))
     (:title "Demo-Motif")
     (:left #-apple 650 #+apple 30)(:top 45)(:width 480)(:height 450)))
  (s-value DEMO-MOTIF-WIN
	   :aggregate
	   (create-instance 'DEMO-MOTIF-TOP-AGG opal:aggregate))


  (create-instance 'COLOR-BUTTONS garnet-gadgets:MOTIF-CHECK-BUTTON-PANEL
     (:constant T :except :foreground-color)
     (:left 130) (:top 150)
     (:items '("Red" "Green" "Blue"))
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN :background-color)))
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (S-VALUE-COMBO-FILLING-STYLE)))
     (:active-p T))


  ;; We bind COLOR-BOX-PROTO as a variable instead of making a named schema
  ;; so that if we call do-go multiple times, we will not get destroy messages.
  (let ((COLOR-BOX-PROTO
	 (create-instance NIL opal:rectangle
	   (:width 25) (:height 25)
	   (:fast-redraw-p (if (g-value opal:color :color-p) :rectangle T))
	   (:draw-function (if (g-value opal:color :color-p) :copy :xor))
	   (:fast-redraw-line-style (o-formula (gv DEMO-MOTIF-WIN
						   :foreground-line-style)))
	   (:fast-redraw-filling-style (o-formula
					(gv DEMO-MOTIF-WIN :filling-style))))))

  (create-instance 'RED-BOX-BORDER COLOR-BOX-PROTO
     (:left 10) (:top 10)
     (:visible (o-formula (MEMBER-STRING "Red" (gv COLOR-BUTTONS :value)))))
  (create-instance 'RED-BOX COLOR-BOX-PROTO
     (:left 11) (:top 11) (:height 23) (:width 23)
     (:visible (o-formula (MEMBER-STRING "Red" (gv COLOR-BUTTONS :value))))
     (:line-style NIL)
     (:filling-style (initial-color-fill opal:red)))

  (create-instance 'GREEN-BOX-BORDER COLOR-BOX-PROTO
     (:left 45) (:top 10)
     (:visible (o-formula (MEMBER-STRING "Green" (gv COLOR-BUTTONS :value)))))
  (create-instance 'GREEN-BOX COLOR-BOX-PROTO
     (:left 46) (:top 11) (:height 23) (:width 23)
     (:visible (o-formula (MEMBER-STRING "Green" (gv COLOR-BUTTONS :value))))
     (:line-style NIL)
     (:filling-style (initial-color-fill opal:green)))

  (create-instance 'BLUE-BOX-BORDER COLOR-BOX-PROTO
     (:left 80) (:top 10)
     (:visible (o-formula (MEMBER-STRING "Blue" (gv COLOR-BUTTONS :value)))))
  (create-instance 'BLUE-BOX COLOR-BOX-PROTO
     (:left 81) (:top 11) (:height 23) (:width 23)
     (:visible (o-formula (MEMBER-STRING "Blue" (gv COLOR-BUTTONS :value))))
     (:line-style NIL)
     (:filling-style (initial-color-fill opal:blue)))

  (create-instance 'COMBO-BOX-BORDER COLOR-BOX-PROTO
     (:left 10) (:top 245) (:width 95) (:height 25)
     (:visible (o-formula (gv COLOR-BUTTONS :value))))
  (create-instance 'COMBO-BOX COLOR-BOX-PROTO
     (:left 11) (:top 246) (:width 93) (:height 23)
     (:visible (o-formula (gv COLOR-BUTTONS :value)))
     (:line-style NIL)
     (:filling-style (initial-color-fill opal:white)))
  
  (create-instance 'SHADE-BOX-BORDER COLOR-BOX-PROTO
     (:left 243) (:top 10)
     (:filling-style (initial-color-fill opal:white)))
  (create-instance 'SHADE-BOX COLOR-BOX-PROTO
     (:left 244) (:top 11) (:width 23) (:height 23)
     (:line-style NIL)
     (:filling-style (initial-color-fill opal:white)))
  ) ;; Close binding of COLOR-BOX-PROTO

  (create-instance 'GROUND-BUTTONS garnet-gadgets:MOTIF-RADIO-BUTTON-PANEL
     (:constant T :except :foreground-color)
     (:left 130) (:top 45)
     (:v-spacing 4)
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:items `("Gray" "Orange" "Green" "Blue"))
     (:active-p T)
     (:selection-function
      #'(lambda (g v)
	  (declare (ignore g v))
	  (kr-send SHADE-SLIDER :selection-function
		   SHADE-SLIDER (g-value SHADE-SLIDER :value)))))

  
  (create-instance 'COLOR-MENU garnet-gadgets:MOTIF-MENU
     (:constant T :except :foreground-color)
     (:left 300) (:top 20)
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:final-feedback-p NIL)
     (:items `(
	    ;; Needs Blue
	       ("Navy Blue" ,#'(lambda (g v)
				 (declare (ignore g v))
				 (NEW-COLOR 0 0 128)))
	    ;; Needs Red and Green
	       ("Gold" ,#'(lambda (g v)
			    (declare (ignore g v))
			    (NEW-COLOR 255 215 0)))
	    ;; Needs Red and Blue
	       ("Violet" ,#'(lambda (g v)
			      (declare (ignore g v))
			      (NEW-COLOR 148 0 211)))
	    ;; Needs Green and Blue
	       ("Turquoise" ,#'(lambda (g v)
				 (declare (ignore g v))
				 (NEW-COLOR 0 206 209)))
	    ;; Needs Everything
	       ("Plum" ,#'(lambda (g v)
			    (declare (ignore g v))
			    (NEW-COLOR 221 160 221)))
	       ("Sienna" ,#'(lambda (g v)
			      (declare (ignore g v))
			      (NEW-COLOR 160 82 45)))
	       ("Motif-Gray" ,#'(lambda (g v)
				  (declare (ignore g v))
				  (NEW-COLOR 211 211 211)))
	       ("Motif-Green" ,#'(lambda (g v)
				   (declare (ignore g v))
				   (NEW-COLOR 95 158 160)))
	       ("Motif-Blue" ,#'(lambda (g v)
				  (declare (ignore g v))
				  (NEW-COLOR 114 159 255)))))
     (:accelerators '((#\N "F2" :F2) (#\o "F3" :F3) (#\V "F4" :F4)
		      (#\T "F5" :F5) (#\P "F6" :F6) (#\S "F7" :F7)
		      (#\G "F8" :F8) (#\r "F9" :F9) (#\B "F10" :F10)))
     (:inactive-items
      (o-formula
       (let* ((value (gv COLOR-BUTTONS :value))
	      (red-p (member-string "Red" value))
	      (green-p (member-string "Green" value))
	      (blue-p (member-string "Blue" value)))
	 (append
	  (unless blue-p (list "Navy Blue"))
	  (unless (and red-p green-p) (list "Gold"))
	  (unless (and red-p blue-p) (list "Violet"))
	  (unless (and green-p blue-p) (list "Turquoise"))
	  (unless (and red-p green-p blue-p)
	    (list "Plum" "Sienna" "Motif-Gray" "Motif-Green" "Motif-Blue"))))))

     (:bar-above-these-items '("Motif-Gray")))

	       
  (create-instance 'SHADE-SLIDER garnet-gadgets:MOTIF-SLIDER
     (:constant T :except :foreground-color)
     (:left 220) (:top 40)
     (:val-1 (o-formula (if (gv opal:color :color-p) 150 100)))
     (:val-2 0)
     (:scr-incr 5)
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN :background-color)))
     (:active-p T)
     (:selection-function
      #'(lambda (gadget value)
	  (if (g-value opal:color :color-p)
	      (multiple-value-bind (r g b)
				   (garnet-gadgets::convert-aux
		                      (g-value gadget :foreground-color)
		                      (float (/ value 100)))
		 (change-color SHADE-BOX r g b))
	      (s-value SHADE-BOX
			 :filling-style
			 (opal:halftone value))))))

  (create-instance 'SHADE-BUTTONS garnet-gadgets::MOTIF-TEXT-BUTTON-PANEL
     (:constant T :except :foreground-color)
     (:left 130) (:top 245)
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:final-feedback-p NIL)
     (:direction :horizontal)
     (:fixed-width-p NIL)
     (:items `(("Highlight"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((hc (g-value SHADE-SLIDER :highlight-fill
					    :foreground-color)))
			   (s-value SHADE-SLIDER
				    :value
				    (round (* 100 gg::*highlight-value*)))
			   (change-color SHADE-BOX (g-value hc :red)
			     (g-value hc :green) (g-value hc :blue)))
			 (progn
			   (s-value SHADE-SLIDER :value 50)
			   (s-value SHADE-BOX
				    :filling-style
				    (opal:halftone 50))))))
	       ("Foreground"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((fc (g-value SHADE-SLIDER :foreground-color)))
			   (s-value SHADE-SLIDER :value 100)
			   (change-color SHADE-BOX (g-value fc :red)
			     (g-value fc :green) (g-value fc :blue)))
			 (progn
			   (s-value SHADE-SLIDER :value 0)
			   (s-value SHADE-BOX
				    :filling-style
				    (opal:halftone 0))))))
	       ("Background"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((bc (g-value SHADE-SLIDER :background-fill
					    :foreground-color)))
			   (s-value SHADE-SLIDER
				    :value
				    (round (* 100 gg::*background-value*)))
			   (change-color SHADE-BOX (g-value bc :red)
			     (g-value bc :green) (g-value bc :blue)))
			 (progn
			   (s-value SHADE-SLIDER :value 0)
			   (s-value SHADE-BOX
				    :filling-style
				    (opal:halftone 0))))))

	       ("Shadow"
		,#'(lambda (g v)
		     (declare (ignore g v))
		     (if (g-value opal:color :color-p)
			 (let ((sc (g-value SHADE-SLIDER :shadow-fill
					    :foreground-color)))
			   (s-value SHADE-SLIDER
				    :value
				    (round (* 100 gg::*shadow-value*)))
			   (change-color SHADE-BOX (g-value sc :red)
			     (g-value sc :green) (g-value sc :blue)))
			 (progn
			   (s-value SHADE-SLIDER :value 100)
			   (s-value SHADE-BOX
				    :filling-style
				    (opal:halftone 100)))))))))

  (create-instance 'TEXT-BOX-1 garnet-gadgets::motif-scrolling-labeled-box
     (:constant T :except :foreground-color)
     (:left 280) (:top 300)
     (:width 170)
     (:label-string "Title:")
     (:value "Motif Gauge")
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:field-offset 5))

  (create-instance 'GAUGE-1 garnet-gadgets:MOTIF-GAUGE
     (:constant T :except :foreground-color :title)
     (:left 20) (:top 280)
     (:title (o-formula (gv TEXT-BOX-1 :value)))
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:parts
      `(:base-line :semi-circ :tic-marks :needle1 :needle2
        (:gauge-title :modify
                      (:fast-redraw-p :rectangle)
                      (:fast-redraw-filling-style
                       ,(o-formula
                         (gv demo-motif::DEMO-MOTIF-WIN :filling-style))))
        (:value-feedback :modify
                         (:fast-redraw-p :rectangle)
                         (:fast-redraw-filling-style
                          ,(o-formula
                            (gv demo-motif::DEMO-MOTIF-WIN :filling-style))))
        :sel-box)))

  (create-instance 'RED-BAR garnet-gadgets:MOTIF-V-SCROLL-BAR
     (:constant T :except :visible :foreground-color)
     (:left 14) (:top 40)
     (:val-1 100) (:val-2 0)
     (:page-incr 20)
     (:visible (o-formula (MEMBER-STRING "Red" (gv COLOR-BUTTONS :value))))
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:active-p T)
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (S-VALUE-RED-FILLING-STYLE)
	  (S-VALUE-COMBO-FILLING-STYLE))))

  (create-instance 'GREEN-BAR garnet-gadgets:MOTIF-V-SCROLL-BAR
     (:constant T :except :visible :foreground-color)
     (:left 49) (:top 40)
     (:val-1 100) (:val-2 0)
     (:page-incr 20)
     (:visible (o-formula (MEMBER-STRING "Green" (gv COLOR-BUTTONS :value))))
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:active-p T)
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (S-VALUE-GREEN-FILLING-STYLE)
	  (S-VALUE-COMBO-FILLING-STYLE))))

  (create-instance 'BLUE-BAR garnet-gadgets:MOTIF-V-SCROLL-BAR
     (:constant T :except :visible :foreground-color)
     (:left 84) (:top 40)
     (:val-1 100) (:val-2 0)
     (:page-incr 20)
     (:visible (o-formula (MEMBER-STRING "Blue" (gv COLOR-BUTTONS :value))))
     (:foreground-color (o-formula (gv DEMO-MOTIF-WIN
				       :background-color)))
     (:active-p T)
     (:selection-function
      #'(lambda (gadget value)
	  (declare (ignore gadget value))
	  (S-VALUE-BLUE-FILLING-STYLE)
	  (S-VALUE-COMBO-FILLING-STYLE))))
  
  (create-instance 'SCROLL-AGG opal:aggregate)
  (opal:add-components SCROLL-AGG RED-BAR GREEN-BAR BLUE-BAR
		       RED-BOX-BORDER RED-BOX GREEN-BOX-BORDER GREEN-BOX
		       BLUE-BOX-BORDER BLUE-BOX COMBO-BOX-BORDER COMBO-BOX)

  (opal:add-components DEMO-MOTIF-TOP-AGG
		       SCROLL-AGG COLOR-BUTTONS GROUND-BUTTONS
		       SHADE-BOX-BORDER SHADE-BOX SHADE-SLIDER SHADE-BUTTONS
		       COLOR-MENU TEXT-BOX-1 GAUGE-1)

  (opal:update DEMO-MOTIF-WIN)


  ;;; Make sure :value formulas are initialized
  (g-value RED-BAR :value)
  (g-value GREEN-BAR :value)
  (g-value BLUE-BAR :value)
  (g-value GROUND-BUTTONS :value)


  ;;; Initialize values in gadgets
  (s-value GROUND-BUTTONS :value "Gray")


  ;;; Set up global TAB interactor
  ;;;
  (s-value COLOR-BUTTONS :keyboard-selection-p T)

  (create-instance 'DEMO-MOTIF-TAB-INTER garnet-gadgets:MOTIF-TAB-INTER
     (:window DEMO-MOTIF-WIN)
     (:objects (list COLOR-BUTTONS GROUND-BUTTONS
		     RED-BAR GREEN-BAR BLUE-BAR
		     SHADE-SLIDER COLOR-MENU SHADE-BUTTONS TEXT-BOX-1 GAUGE-1)))

  ;;; Set up global accelerator interactor for the menu
  ;;;
  (create-instance 'COLOR-MENU-INTER garnet-gadgets:MOTIF-MENU-ACCELERATOR-INTER
     (:window DEMO-MOTIF-WIN)
     (:menus (list COLOR-MENU)))


  (opal:update DEMO-MOTIF-WIN)

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

  (format t "~%Demo-Motif:
      This demo shows how Garnet is able to simulate the look and feel of the
   Motif widgets.  In addition to operating the gadgets by mouse movements,
   you can select buttons and manipulate the scroll bars through keyboard
   interaction.  Press the tab key (or control-tab) to move the keyboard
   selection around the window.  Use the spacebar to select buttons, and the
   return key to select menu items.  The arrow keys will move the selection
   within a gadget (i.e., select another button in the same panel), and will
   also move the scroll bars and sliders.~%")
  
  )



(defun DO-STOP ()
  (opal:destroy DEMO-MOTIF-WIN))
