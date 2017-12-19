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


;;; This file contains demo code for showing menus in a window
;;; When loaded, it creates a window that contains five menus that can be
;;; operated with the left mouse button
;;;
;;; This is intended as a test and demonstration of several features of the
;;; Garnet project:  the menu interactor, aggregadgets, aggrelists,
;;; feedback objects, interim feedback objects, and polylines.
;;;
;;; ** Call (demo-menu:do-go) to start
;;;     and (demo-menu:do-stop) to stop **
;;;
;;; Designed by Brad A. Myers
;;; Aggregadget Version Written by Andrew Mickish
;;; Color option added by Ed Pervin


(in-package :DEMO-MENU)

;; (when (not (get :garnet-modules :text-buttons))
;;   (common-lisp-user::garnet-load "gadgets:text-buttons-loader"))

(declaim (special FRAME FEEDBACK DEMO-MENU-INTERACTOR MENU1-ITEM-PROTOTYPE
		  MENU1 MENU2 MENU3 MENU4 MENU5 MENU6 PLUS-FEEDBACK
		  ARROW-FEEDBACK MENU4-ITEM-PROTOTYPE MENU6-ITEM-PROTOTYPE
		  MENU-WIN MENU-TOP-AGG TEXT1 TEXT2 TEXT3 DASHED-ITEM
		  MENU1-OBJ MENU6-OBJ MENU3-OBJ MENU5-OBJ BIG-TEXT
		  MEDIUM-TEXT SMALL-TEXT SQUARE-ITEM CIRCLE-ITEM))


(defvar *color-p* (g-value opal:color :color-p))

;; These fonts are declared globally so that they can be created and displayed
;; before they are used in MENU4.  This will avoid creation delays while
;; menu items are being selected (see DO-GO function).
(defvar *interim-selected-font* (opal:get-standard-font NIL :bold NIL))
(defvar *selected-font* (opal:get-standard-font NIL :italic NIL))
(defvar *selected-interim-selected-font*
  (opal:get-standard-font NIL :bold-italic NIL))



;;; ********************************************************************
;;;   Objects used in several menus
;;; ********************************************************************


;;   This object is used for the outline of the menus.  The width and height
;; of the frame is calculated in the top-level menu object (e.g., MENU1) and
;; then referenced by the formulas defined here.
;;
(create-instance 'FRAME opal:rectangle
   (:constant T :parent)
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:width (o-formula (gvl :parent :frame-width)))
   (:height (o-formula (gvl :parent :frame-height)))
   (:filling-style opal:white-fill))


;;   The formulas in this object all depend on the value of the :obj-over slot,
;; which is set by the interactor of each menu.  When the user clicks on a
;; menu item, the interactor will set the :obj-over slot of the feedback
;; object to that menu item.  This will cause the :visible slot to become
;; non-NIL and will cause the dimensions of the feedback object to conform to
;; the dimensions of the selected item.
;;
(create-instance 'FEEDBACK opal:rectangle
   (:obj-over NIL)  ; Set by interactor to be the object covered by the feedback
   (:left (o-formula (gvl :obj-over :left)))
   (:top (o-formula (gvl :obj-over :top)))
   (:width (o-formula (gvl :obj-over :width)))
   (:height (o-formula (gvl :obj-over :height)))
   (:visible (o-formula (gvl :obj-over)))
   (:draw-function :xor)
   (:fast-redraw-p T))


;;   This menu interactor is defined so that instances of it can be added to
;; aggrelists of the menus in this demo.  The item-lists of MENU1, MENU2,
;; MENU4, and MENU5 all declare instances of this prototype.
;;   The :window slot is defined so that the interactor will be sensitive in
;; the window of the object that it operates on.  The formula in the
;; :start-where slot determines that the interactor will be invoked when the
;; user clicks over any component of the object that the interactor operates on.
;; The formula in the :feedback-obj slot will evaluate to the object that
;; should become visible whenever the mouse is held down over an element of
;; the aggrelist.
;;
(create-instance 'DEMO-MENU-INTERACTOR inter:menu-interactor
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula `(:element-of ,(gvl :operates-on))))
   (:feedback-obj (o-formula (gvl :operates-on :parent :int-feedback)))
   (:final-feedback-obj (o-formula (gvl :operates-on :parent :final-feedback))))



;;; ********************************************************************
;;; MENU1;  Simple single selection menu
;;;   The structure of this menu contains two frames -- one black "shadow"
;;; frame and one white frame.  An aggrelist of strings is positioned inside
;;; the white frame, and the dimensions of the white frame are determined
;;; by the dimensions of the aggrelist.
;;;   Two types of feedback are used:  1) Interim feedback - a black
;;; feedback rectangle is XOR'ed over an item when the left mouse button
;;; is held down over it, and  2) Final feedback - a rectangle appears around
;;; the last item selected.
;;; ********************************************************************


;;  One MENU1-ITEM-PROTOTYPE instance is created for each item in the :items
;;  list.  The string that appears in the object is determined by indexing
;;  through the :items list with the :rank that is assigned to each object by
;;  the aggrelist.
;;
(create-instance 'MENU1-ITEM-PROTOTYPE opal:text
   ;; The slots :rank, :left, and :top are installed automatically by the
   ;; aggrelist.
   (:constant '(:string :font :actual-heightp))
   (:string (o-formula (nth (gvl :rank) (gvl :parent :items))))
   (:line-style (o-formula (if (gvl :selected)
			       (if *color-p* opal:purple-line opal:default-line-style)
			       opal:default-line-style)))
   (:font opal:default-font))


(create-instance 'MENU1 opal:aggregadget
   (:constant :items :visible :offset :offset2 :left :top)
   (:left 10)
   (:top 10)
   (:obj-containing-howset NIL)
   (:items '("January" "February" "March" "April" "May" "June" "July" "August"
	     "September" "October" "November" "December"))
   (:offset 4)  ; The distance from the longest item string to the menu frame
   (:offset2 (o-formula (* 2 (gvl :offset))))
   (:frame-width (o-formula (+ (gvl :items-list :width) (gvl :offset2))))
   (:frame-height (o-formula (+ (gvl :items-list :height) (gvl :offset2))))
   (:parts
    `((:shadow ,FRAME
	   (:line-style NIL)  ; Avoid redundant outline of black box
	   (:filling-style ,opal:black-fill))
      (:frame ,FRAME
	   (:left ,(o-formula (+ (gvl :parent :left) 4)))
	   (:top ,(o-formula (+ (gvl :parent :top) 4))))
      ;; Interim feedback will appear over the object currently clicked on
      (:int-feedback ,FEEDBACK
	   (:left ,(o-formula (- (gvl :obj-over :left) 2)))
	   (:top ,(o-formula (+ (gvl :obj-over :top) 1)))
	   (:width ,(o-formula (+ 4 (gvl :obj-over :width))))
	   ;; When using black fill, set the :line-style to NIL to avoid
	   ;; the redundant drawing of an outline
	   (:line-style NIL)
	   (:filling-style ,(if *color-p* opal:yellow-fill opal:black-fill)))
      ;; Final feedback will appear over the selected object (via :obj-over)
      (:final-feedback ,FEEDBACK
	   (:line-style ,(if *color-p* opal:green-line opal:default-line-style))
	   (:left ,(o-formula (- (gvl :obj-over :left) 2)))
	   (:top ,(o-formula (+ (gvl :obj-over :top) 1)))
	   (:width ,(o-formula (+ 4 (gvl :obj-over :width)))))

      (:items-list ,opal:aggrelist
       (:constant (:fixed-width-p :fixed-height-p :fixed-width-size
		   :fixed-height-size :direction :h-spacing :v-spacing
		   :indent :rank-margin :pixel-margin :h-align :v-align))
	   (:left ,(o-formula (+ (gvl :parent :left) 8)))
	   (:top ,(o-formula (+ (gvl :parent :top) 8)))
	   (:v-spacing 0)
	   (:h-align :center)  ;; These slots together take care of centering
	   (:fixed-width-p T)  ;; the strings
	   (:items ,(o-formula (gvl :parent :items)))
	   (:item-prototype ,MENU1-ITEM-PROTOTYPE)
	   (:interactors
            ((:press ,DEMO-MENU-INTERACTOR
                     (:how-set
                      ,(o-formula
                        (gvl :operates-on :parent :obj-containing-howset :value)
                        :set))))))))) ; set is the initial value



;;; ********************************************************************
;;; MENU2;  Large multi-selection menu with plus signs.  The
;;;   The structure of this menu is similar to MENU1, except that there are
;;; multiple final feedback objects.  It is necessary to define the same number
;;; of final feedback objects as items, since more than one feedback object may
;;; be visible at one time.  By using an aggregadget as the prototype for the
;;; aggrelist, we can define one text string and one final feedback object per
;;; item, defining each as a "part" of the aggregadget.
;;; ********************************************************************


(create-instance 'plus-feedback opal:polyline
  (:obj-over NIL) ; set by the interactor
  (:visible (o-formula (gvl :obj-over)))
  (:constant :line-style)
  (:height (o-formula (- (gvl :obj-over :height) 2)))
  (:width (o-formula (gvl :height)))
  (:height/3 (o-formula (round (gvl :height) 3)))
  (:x1 (o-formula (+ (gvl :obj-over :left) (gvl :obj-over :width)
		      4)))
  (:x2 (o-formula (+ (gvl :x1) (gvl :height/3))))
  (:x3 (o-formula (+ (gvl :x2) (gvl :height/3))))
  (:x4 (o-formula (+ (gvl :x3) (gvl :height/3))))
  (:y1 (o-formula (+ 1 (gvl :obj-over :top))))
  (:y2 (o-formula (+ (gvl :y1) (gvl :height/3))))
  (:y3 (o-formula (+ (gvl :y2) (gvl :height/3))))
  (:y4 (o-formula (+ (gvl :y3) (gvl :height/3))))
  (:point-list (o-formula (list
			    (gvl :x2) (gvl :y1)  (gvl :x3) (gvl :y1)
			    (gvl :x3) (gvl :y2)  (gvl :x4) (gvl :y2)
			    (gvl :x4) (gvl :y3)  (gvl :x3) (gvl :y3)
			    (gvl :x3) (gvl :y4)  (gvl :x2) (gvl :y4)
			    (gvl :x2) (gvl :y3)  (gvl :x1) (gvl :y3)
			    (gvl :x1) (gvl :y2)  (gvl :x2) (gvl :y2)
			    (gvl :x2) (gvl :y1))))
  (:line-style NIL)  ; Avoid redundant outline of black box
  (:filling-style (if *color-p* opal:red-fill opal:black-fill)))

(create-instance 'arrow-feedback plus-feedback
  (:yhalf (o-formula (+ (gvl :y1) (round (gvl :height) 2) -1)))
  (:y2 (o-formula (+ 1 (gvl :y1) (gvl :height/3))))
  (:point-list (o-formula (list (gvl :x1)(gvl :yhalf)  (gvl :x2)(gvl :y1)
				(gvl :x2)(gvl :y2)     (gvl :x4)(gvl :y2)
				(gvl :x4)(gvl :y3)     (gvl :x2)(gvl :y3)
				(gvl :x2)(gvl :y4)     (gvl :x1)(gvl :yhalf)
				)))
  )


(create-instance 'MENU2 opal:aggregadget
   (:constant :items :visible :left :top)
   (:left 268)
   (:top 10)
   (:items '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
	     "Saturday"))
   (:inner-frame-width (o-formula (+ (gvl :items-list :width) 8 19)))
   (:outer-frame-width (o-formula (+ (gvl :inner-frame-width) 8)))
   (:inner-frame-height (o-formula (+ (gvl :items-list :height)
				      8)))
   (:outer-frame-height (o-formula (+ (gvl :inner-frame-height)
				      8)))
   (:parts
    `((:outer-frame ,FRAME
	   (:width ,(o-formula (gvl :parent :outer-frame-width)))
	   (:height ,(o-formula (gvl :parent :outer-frame-height)))
	   (:line-style ,opal:line-2))
      (:inner-frame ,FRAME
	   (:left ,(o-formula (+ (gvl :parent :left) 4)))
	   (:top ,(o-formula (+ (gvl :parent :top) 4)))
	   (:width ,(o-formula (gvl :parent :inner-frame-width)))
	   (:height ,(o-formula (gvl :parent :inner-frame-height))))
      (:int-feedback ,FEEDBACK
	   ;; Dimensions for this feedback box are larger than the dimensions
	   ;; of the object in the :obj-over slot because we are making the
           ;; lines of the feedback box thicker.
	   (:left ,(o-formula (+ 2 (gvl :parent :inner-frame :left))))
	   (:top ,(o-formula (- (gvl :obj-over :top) 1)))
	   (:width ,(o-formula (- (gvl :parent :inner-frame-width) 4)))
	   (:height ,(o-formula (+ 3 (gvl :obj-over :height))))
	   (:line-style ,(create-instance NIL opal:line-style
			   (:constant T)
			   (:line-thickness 2)
			   (:foreground-color
			    (if *color-p* opal:red opal:black)))))
      (:final-feedback1 ,arrow-feedback)
      (:final-feedback2 ,plus-feedback)
      (:items-list ,opal:aggrelist
       (:constant (:direction :h-spacing :indent :rank-margin :pixel-margin
		   :fixed-width-p :fixed-height-p :fixed-width-size
		   :fixed-height-size :h-align :v-align))
	   (:left ,(o-formula (+ (gvl :parent :left) 8)))
	   (:top ,(o-formula (+ (gvl :parent :top) 8)))
	   (:v-spacing ,(o-formula 4))
	   (:items ,(o-formula (gvl :parent :items)))
	   (:item-prototype ,opal:text
	    (:constant :font)
	    (:string ,(o-formula (nth (gvl :rank) (gvl :parent :items))))
	    (:font ,(opal:get-standard-font :serif :bold :large)))
	   (:interactors
	    ((:press ,DEMO-MENU-INTERACTOR
                     (:how-set :list-toggle)
		     (:final-feedback-obj
		      ,(o-formula
		       (let ((str (gvl :current-obj-over :string)))
			 (if (or (string= str "Sunday")
				 (string= str "Saturday"))
			     (gvl :operates-on :parent :final-feedback1)
			     (gvl :operates-on :parent :final-feedback2)))))
                     )))))))



;;; ********************************************************************
;;; MENU3;  Panel of buttons
;;;   This menu is just an instance of the TEXT-BUTTON-PANEL defined in the
;;; Garnet Gadget Set.
;;; ********************************************************************


(create-instance 'MENU3 garnet-gadgets:text-button-panel
   (:constant T)
   (:left 420)
   (:top 10)
   (:items '("Red" "Purple" "Green" "Blue" "Yellow"))
   (:item-colors
       (if *color-p*
         (list opal:red opal:purple opal:green opal:blue opal:yellow)
	 (list opal:black opal:black opal:black opal:black opal:black)))
   ;; (:final-feedback-p t)
   (:font (opal:get-standard-font :serif :roman :large)))

;;; ********************************************************************
;;; MENU4;  Italics and bold for selected items
;;;   In this menu, selection and interim selection of items is acknowledged
;;; by changing the font of the strings displayed in the menu.
;;;   The computation for the dimensions of the frame is more complicated in
;;; this menu than the others because we want to be able to change the fonts
;;; (and therefore the size) of the items without changing the size of the
;;; frame.  Thus at the top-level, the formulas for :max-item-width and
;;; :max-item-height iterate over the items and determine the dimensions of
;;; the largest string in the largest font.  The dimensions of the frame
;;; are then calculated only once, and the entire frame will never have to be
;;; redrawn.
;;; ********************************************************************


;; One MENU4-ITEM-PROTOTYPE object is created for each item in the :items list.
;; The string that appears in the object is determined by indexing through the
;; :items list with the :rank that is assigned to each object by the aggrelist.
;;
(create-instance 'MENU4-ITEM-PROTOTYPE opal:text
   ;; The slot :rank is installed automatically by aggrelists.  The slots
   ;; :selected and :interim-selected are set by the interactor.
   (:constant :line-style :string :visible)
   (:string (o-formula (nth (gvl :rank) (gvl :parent :items))))
   (:line-style (o-formula (if (or (gvl :selected) (gvl :interim-selected))
			       (if *color-p* opal:blue-line opal:default-line-style)
			       opal:default-line-style)))
   (:font (o-formula
	   (if (gvl :selected)
	       (if (gvl :interim-selected)
		   (gvl :parent :parent :selected-interim-selected-font)
		   (gvl :parent :parent :selected-font))
	       (if (gvl :interim-selected)
		   (gvl :parent :parent :interim-selected-font)
		   (gvl :parent :parent :normal-font))))))


(create-instance 'MENU4 opal:aggregadget
   (:constant :items :num-items :selected-interim-selected-font :left :top
	      :offset :offset2 :max-item-width :max-item-height)
   (:left 250)
   (:top 230)
   (:items '("History" "Math" "Chemistry" "Computer Science" "Biology"
	     "English" "Basket Weaving"))
   (:num-items (o-formula (length (gvl :items))))
   (:offset 4)
   (:offset2 (o-formula (* 2 (gvl :offset))))
   (:normal-font opal:default-font)
   (:interim-selected-font *interim-selected-font*)
   (:selected-font *selected-font*)
   (:selected-interim-selected-font *selected-interim-selected-font*)
   ;; Examine each string in the :items list as it appears in the largest font,
   ;; and choose the width of the widest string.  (We assume that the
   ;; :selected-interim-selected-font is the largest font.)
   (:max-item-width
    (o-formula
     (do* ((items (gvl :items))
	   (font (gvl :selected-interim-selected-font))  ; The largest font used
	   (num-items (gvl :num-items))
	   (i 0 (+ i 1))                              ; Look at the width of
	   (item (nth i items) (nth i items))         ; each string in the
	   (width (opal:string-width font item)       ; largest font used.
		  (opal:string-width font item))
	   (max-width width
		      (if (> width max-width)
			  width
			  max-width)))
	  ((= i num-items) max-width))
     0))
   ;; Analagous to :max-item-width
   (:max-item-height
    (o-formula
     (do* ((items (gvl :items))
	   (font (gvl :selected-interim-selected-font))  ; The largest font used
	   (num-items (gvl :num-items))
	   (i 0 (+ i 1))                              ; Look at the height of
	   (item (nth i items) (nth i items))         ; each string in the
	   (height (opal:string-height font item)     ; largest font used.
		   (opal:string-height font item))
	   (max-height height
		      (if (> height max-height)
			  height
			  max-height)))
	  ((= i num-items) max-height))
     0))
   (:frame-width (o-formula (+ (gvl :offset2) (gvl :max-item-width))))
   ;; For :frame-height, consider the height of all of the items and the spaces
   ;; between them.
   (:frame-height (o-formula (let ((num-items (gvl :num-items)))
			       (+ (* num-items (gvl :max-item-height))
				  (* (+ 1 num-items) (gvl :offset))))))
   (:parts
    `((:frame ,FRAME)
      (:items-list ,opal:aggrelist
       (:constant T)
	   (:left ,(o-formula (+ (gvl :parent :left) (gvl :parent :offset))))
	   (:top ,(o-formula (+ (gvl :parent :top) (gvl :parent :offset))))
           (:width ,(o-formula (gvl :parent :max-item-width)))
	   (:v-spacing ,(o-formula (gvl :parent :offset)))
	   (:items ,(o-formula (gvl :parent :items)))
	   (:item-prototype ,MENU4-ITEM-PROTOTYPE)
	   (:interactors
	    ((:press ,DEMO-MENU-INTERACTOR
		     (:feedback-obj NIL))))))))

  
;;; ********************************************************************
;;; MENU5; Graphical object menu: 2 columns
;;;   This menu is unique in this demo because it does not use an itemized
;;; aggrelist, but instead builds up an aggrelist by using the function
;;; ADD-COMPONENT.  There is no item-prototype because the objects in the
;;; menu have nothing in common -- there is text, a rectangle, a circle, and
;;; a line.
;;;   The menu is first created without any items, with formulas of the frame
;;; dependent on the dimensions of the aggrelist.  Items are then added to
;;; the aggrelist (inside the menu).  The aggrelist internally adjusts the
;;; positions of the items so that later, when the menu is added to the
;;; demo window, the items will be in place and the frame will be properly
;;; sized.
;;;
;;; ********************************************************************


(create-instance 'MENU5 opal:aggregadget
   (:constant :left :top :offset :visible)
   (:left 10)
   (:top 210)
   (:offset 4)
   (:offset2 (o-formula (* 2 (gvl :offset))))
   (:frame-width (o-formula (gvl :title-bar :width)))
   (:frame-height (o-formula (+ (gvl :items-list :height)
				(gvl :title-bar :height)
				(gvl :offset2))))
   (:parts
    `((:frame ,FRAME)
      (:title-bar ,opal:aggregadget
	      (:left ,(o-formula (+ 1 (gvl :parent :left))))
	      (:top ,(o-formula (+ 1 (gvl :parent :top))))
	      ;; We want the black rectangle to be either the width of the
	      ;; title string or the width of the widest menu item, whichever
	      ;; is larger.
	      (:width ,(o-formula (max (gvl :text :width)
				       (+ (gvl :parent :items-list :width)
					  (gvl :parent :offset2)))))
	      (:height ,(o-formula (gvl :text :height)))
	      (:parts
	       ((:text ,opal:text
		       (:constant T)
		       ;; This text will be centered over the menu
		       (:left ,(o-formula (- (+ (gvl :parent :left)
						(floor (gvl :parent :width) 2))
					     (floor (gvl :width) 2))))
		       (:top ,(o-formula (gvl :parent :top)))
		       (:string "Pick One:")
		       (:font ,opal:default-font))
		(:rect ,opal:rectangle
		       (:constant T)
		       (:left ,(o-formula (gvl :parent :left)))
		       (:top ,(o-formula (gvl :parent :top)))
		       (:width ,(o-formula (- (gvl :parent :width) 2)))
		       (:height ,(o-formula (gvl :parent :height)))
		       (:line-style NIL)  ; Avoid redundant border on black box
		       (:filling-style ,opal:black-fill)
		       (:draw-function :xor)))))  ;; Causes text to look inverse

      (:items-list ,opal:aggrelist
       (:constant (:fixed-width-p :fixed-height-p :fixed-width-size
		   :fixed-height-size :direction :indent :rank-margin
		   :pixel-margin))
	   (:left ,(o-formula (+ (gvl :parent :left) (gvl :parent :offset))))
	   (:top ,(o-formula (+ (gvl :parent :top)
				(gvl :parent :title-bar :height)
				(gvl :parent :offset))))
	   (:v-spacing ,(o-formula (gvl :parent :offset)))
	   (:h-spacing ,(o-formula (gvl :parent :offset2)))
	   (:rank-margin 3)  ; Three rows per column
	   (:interactors
	    ((:press ,DEMO-MENU-INTERACTOR))))
      (:int-feedback ,FEEDBACK
		     (:line-style NIL)  ; Avoid redundant border on black box
		     (:filling-style
			 ,(if *color-p* opal:yellow-fill opal:black-fill)))
      ;; The final feedback is a string below the menu that displays the name
      ;; (in the :mode-string slot) of the last selected object
      (:final-feedback ,opal:text
          (:fast-redraw-p :rectangle)
          (:fast-redraw-filling-style ,opal:white-fill)
	  (:constant (T :except :string))
	  (:left ,(o-formula (+ (gvl :parent :left)
				(gvl :parent :offset))))
	  (:top ,(o-formula (+ (gvl :parent :top)
			       (gvl :parent :frame-height)
			       (gvl :parent :offset))))
	  ;; Note: use GV instead of GVL in this formula because the object
	  ;; that is being accessed is named explicitly in the LET.
          (:obj-over NIL) ; set by interactor to object selected
	  (:string ,(o-formula
		     (let ((selected (gvl :obj-over)))
		       (concatenate 'string
			  "Mode is: " (if selected
					  (gv selected :mode-string)
					  "<NONE>")))))
	  (:font ,(opal:get-standard-font :serif NIL NIL))))))


;;; ********************************************************************
;;; MENU6;  Items go left and right
;;;   In this menu, selection and interim selection of items is acknowledged
;;; by moving the strings displayed in the menu.
;;;   The computation for the dimensions of the frame is more complicated in
;;; this menu since we need to have it be twice the size of the widest item.
;;; ********************************************************************


;; One MENU6-ITEM-PROTOTYPE object is created for each item in the :items list.
;; The string that appears in the object is determined by indexing through the
;; :items list with the :rank that is assigned to each object by the aggrelist.
;;
(create-instance 'MENU6-ITEM-PROTOTYPE opal:aggregadget
   ;; The slot :rank is installed automatically by aggrelists.  The slots
   ;; :selected and :interim-selected are set by the interactor.
   (:width (o-formula (* 2 (gvl :parent :parent :max-item-width))))
   (:height (o-formula (gvl :string :height)))
   (:value (o-formula (nth (gvl :rank) (gvl :parent :items))))
   (:parts
    `((:string ,opal:text
	   (:constant (T :height :except :left :line-style))
           (:font ,(o-formula (gvl :parent :parent :parent :font)))
	   (:line-style ,(o-formula
			  (if (gvl :parent :selected)
			      (if *color-p*
				  opal:orange-line
				  opal:default-line-style)
			      opal:default-line-style)))
           (:string ,(o-formula (string-capitalize (gvl :parent :value))))
           (:left ,(o-formula (+ (gvl :parent :left)
                                 (if (gvl :parent :selected)
                                     ; then make it right justified
                                     (- (gvl :parent :width)
                                        (gvl :width)
                                        2)
                                     ; else at parent left
                                     0))))
           (:top ,(o-formula (gvl :parent :top)))))))


(create-instance 'MENU6 opal:aggregadget
   (:constant :items :visible :font :left :top)
   (:left 100)
   (:top 10)
   (:font (opal:get-standard-font :sans-serif NIL NIL))
   ; initial value=:set
   (:value (o-formula (gvl :items-list :selected :value) :set))
   (:items '(:Set :Clear :toggle :List-add :list-remove :list-toggle))
   (:num-items (o-formula (length (gvl :items))))
   ;; Examine each string and choose the widest string.
   (:max-item-width
    (o-formula
     (let ((max-width 0)
           (font (gvl :font))
           this-width)
       (dolist (item (gvl :items))
         (setq this-width (opal:string-width font (string-capitalize item)))
         (when (> this-width max-width) (setq max-width (+ 5 this-width))))
       max-width)
     0))
   (:frame-width (o-formula (+ 3 (* 2 (gvl :max-item-width)))))
   (:text-height
    (o-formula
     (let ((sum 0)
           (font (gvl :font)))
       (dolist (item (gvl :items))
         (incf sum (opal:string-height font (string-capitalize item))))
       sum)))
   (:frame-height (o-formula (+ 2 (gvl :text-height)(gvl :title :height))))
   (:parts
    `((:frame ,FRAME)
      (:title ,opal:rectangle
	   (:constant T)
           (:left ,(o-formula (gvl :parent :left)))
           (:top  ,(o-formula (gvl :parent :top)))
           (:width ,(o-formula (gvl :parent :frame-width)))
           (:line-style NIL)
           (:filling-style ,opal:black-fill)
           (:height ,(o-formula (+ (gvl :parent :title-string :height) 4))))
      (:title-string ,opal:text
	   (:constant T)
           (:string "Choice:")
           (:left ,(o-formula (+ (gvl :parent :left)
                                 (gvl :parent :max-item-width)
                                 4)))
           (:font ,(opal:get-standard-font :sans-serif :bold NIL))
           (:top ,(o-formula (+ (gvl :parent :top) 2)))
           (:draw-function :xor))
      (:items-list ,opal:aggrelist
       (:constant (:fixed-width-p :fixed-height-p :fixed-width-size
		   :fixed-height-size :direction :h-spacing :v-spacing
		   :indent :rank-margin :pixel-margin))
           (:left ,(o-formula (+ (gvl :parent :left) 2)))
           (:top ,(o-formula (+ 1 (gvl :parent :top)
				(gvl :parent :title :height))))
           (:v-spacing 0)
           (:items ,(o-formula (gvl :parent :items)))
           (:item-prototype ,MENU6-ITEM-PROTOTYPE)
           (:interactors
            ((:press ,DEMO-MENU-INTERACTOR
                     (:continuous NIL)
                     (:feedback-obj NIL)))))
      (:vert-bar ,opal:line
		 (:constant T)
                 (:x1 ,(o-formula (+ (gvl :parent :left)
                                     (gvl :parent :max-item-width)
                                     1)))
                 (:x2 ,(o-formula (gvl :x1)))
                 (:y1 ,(o-formula (+ 1 (gvl :parent :top))))
                 (:y2 ,(o-formula (+ (gvl :parent :top)
                                     (gvl :parent :frame-height)
                                     -2)))
                 (:draw-function :xor)))))


;;;;;;;;;;;;;;;;;;;
;; DEMO FUNCTION ;;
;;     Do-Go     ;;
;;;;;;;;;;;;;;;;;;;

(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)

  ;;; create a window and an aggregate inside the window
  (create-instance 'MENU-WIN inter:interactor-window
	   (:double-buffered-p double-buffered-p)
	   (:left 50) (:top 45) (:width 530) (:height 400)
	   (:title "GARNET MENU") (:icon-title "Menu"))
  (s-value MENU-WIN :aggregate
	   (create-instance 'MENU-TOP-AGG opal:aggregate
			    (:overlapping NIL)))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-MENU"))
     (g-value menu-win :destroy-hooks)))

  ;; In order to avoid font creation delays during menu selections, create
  ;; these fonts before they are used in MENU4.  These objects display spaces
  ;; in the specified fonts at position (0,0).
  (create-instance 'TEXT1 opal:text
     (:constant T)
     (:string " ") (:font *interim-selected-font*))
  (create-instance 'TEXT2 opal:text
     (:constant T)
     (:string " ") (:font *selected-font*))
  (create-instance 'TEXT3 opal:text
     (:constant T)
     (:string " ") (:font *selected-interim-selected-font*))
  (opal:add-components MENU-TOP-AGG TEXT1 TEXT2 TEXT3)


  ;; Create instances of the menus and add them to the window (instances are
  ;; created so that the window can be destroyed in DO-STOP without destroying
  ;; the menu prototypes).
  (opal:add-components MENU-TOP-AGG
		       (create-instance 'MENU1-OBJ MENU1)
		       (create-instance 'MENU2-OBJ MENU2)
		       (create-instance 'MENU3-OBJ MENU3)
		       (create-instance 'MENU4-OBJ MENU4)
		       (create-instance 'MENU5-OBJ MENU5)
                       (create-instance 'MENU6-OBJ MENU6))
  (s-value menu1-obj :obj-containing-howset menu6-obj)

;;; Set all the colors in MENU3-OBJ
  (let ((button-list (g-value MENU3-OBJ :text-button-list))
        (button-color-list (g-value MENU3-OBJ :item-colors)))
    (dotimes (n (length (g-value button-list :items)))
      (let ((button (nth n (g-value button-list :components)))
	    (button-color (nth n button-color-list)))
        (s-value (g-value button :text) :line-style
	         (create-instance nil opal:line-style
		  		  (:foreground-color button-color)))
        (s-value (g-value button :shadow) :filling-style
	         (create-instance nil opal:black-fill
				  (:foreground-color button-color)
				  (:background-color button-color)))
        (s-value (g-value button :gray-outline) :filling-style
	         (create-instance nil opal:gray-fill
				  (:foreground-color button-color))))))

  (s-value (g-value MENU3-OBJ :final-feedback) :filling-style
	   (o-formula (gv MENU3-OBJ :text-button-list :selected :shadow :filling-style)))


  ;; Create and add objects to the aggrelist in MENU5-OBJ.  (Since MENU5
  ;; does not use an itemized aggrelist, these objects must be added this way.)
  ;; The :mode-string slots are used as identifiers for these objects when
  ;; one becomes selected.  The string is referenced in the FINAL-FEEDBACK
  ;; object of MENU5.
  (create-instance 'BIG-TEXT opal:text
     (:constant '(:string :font :actual-heightp :width :height :visible))
     (:string "Big")
     (:font (opal:get-standard-font :serif :bold :large))
     (:line-style (o-formula
		   (if (gvl :selected)
		       (if *color-p* opal:red-line opal:default-line-style)
		       opal:default-line-style)))
     (:mode-string "Big String"))
  (create-instance 'MEDIUM-TEXT opal:text
     (:constant '(:string :font :actual-heightp :width :height :visible))
     (:string "Medium")
     (:font (opal:get-standard-font NIL :italic NIL))
     (:line-style (o-formula
		   (if (gvl :selected)
		       (if *color-p* opal:red-line opal:default-line-style)
		       opal:default-line-style)))
     (:mode-string "Medium String"))
  (create-instance 'SMALL-TEXT opal:text
     (:constant '(:string :font :actual-heightp :width :height :visible))
     (:string "Small")
     (:font (opal:get-standard-font NIL NIL :small))
     (:line-style (o-formula
		   (if (gvl :selected)
		       (if *color-p* opal:red-line opal:default-line-style)
		       opal:default-line-style)))
     (:mode-string "Small String"))
  (create-instance 'SQUARE-ITEM opal:rectangle
     (:constant '(T :except :left :top :line-style))
     (:width 50) (:height 50)
     (:red-line-4 (create-instance NIL opal:line-style
		    (:constant T)
		    (:line-thickness 4)
		    (:foreground-color (if *color-p* opal:red opal:black))))
     (:line-style (o-formula (if (gvl :selected)
				 (gvl :red-line-4)
				 opal:line-4)))
     (:mode-string "Square"))
  (create-instance 'CIRCLE-ITEM opal:circle
     (:constant '(T :except :left :top :filling-style))
     (:width 50) (:height 50)
     (:red-gray-fill (create-instance NIL opal:gray-fill
		       (:foreground-color (if *color-p* opal:red opal:black))))
     (:filling-style (o-formula (if (gvl :selected)
				    (gvl :red-gray-fill)
				    opal:gray-fill)))
     (:mode-string "Circle"))

  (create-instance 'DASHED-ITEM opal:line
     (:constant '(T :width :height :except :line-style))
     (:x1 64)
     (:y1 336)
     (:x2 114)
     (:y2 366)
     (:red-dashed-line (create-instance NIL opal:line-style
			 (:constant T)
			 (:line-style :dash)
			 (:dash-pattern '(4 4))
			 (:foreground-color
			  (if *color-p* opal:red opal:black))))
     (:line-style (o-formula (if (gvl :selected)
				 (gvl :red-dashed-line)
				 opal:dashed-line)))
     (:mode-string "Dashed Line"))

  (opal:add-components (g-value MENU5-OBJ :items-list)
		       BIG-TEXT MEDIUM-TEXT SMALL-TEXT
		       SQUARE-ITEM CIRCLE-ITEM DASHED-ITEM)
  (declare-constant (g-value MENU5-OBJ :items-list) :components)

  (opal:update MENU-WIN)

  (format T "~%Demo-Menu:
  Press and drag over each menu to get feedback.
  (You must press directly on the dashed line to select it.)~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

)

(defun Do-Stop ()
  (opal:destroy MENU-WIN))
