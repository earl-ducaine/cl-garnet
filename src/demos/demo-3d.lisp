;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-3D; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Written by Brad Myers
;;; Upgraded to color by Ed Pervin (Mar 90)
;;; Changes:
;;; 15-Feb-94 Andrew Mickish - Made faster version for Mac
;;;             - Repositioned X's inside buttons, and made XOR fast redraw
;;;             - Removed X's color constraints, since XOR makes black anyway
;;;             - Removed color constraint of radio-button's feedback circle
;;; 09-Apr-92 Andrew Mickish - Changed create-instance of opal:line-2
;;;             to opal:line-style
;;; 25-Mar-92 Andrew Mickish - Get-Values ---> G-Value
;;; 13-Feb-92 Pervin - Merged demo-3d and color-demo-3d
;;; 13-Mar-91 Ed Pervin Changed package name back to demo-3d.

(in-package :DEMO-3D)

;;-----------------------------------

(defparameter shad-off 15)
(defparameter white-off 3)
(defparameter white-off2 (* 2 white-off))
(defparameter gray-size 6)
(defparameter gray-size2 (* 2 gray-size))
(defvar menu3 NIL)
(defvar *color-p* (g-value opal:color :color-p))

(declaim (special MENU-NAME))

(defun make-fixed-menu3 (top-agg pairs x y)
  (let (shadow outline outline2 this-item prev-item text-item inv-box fnt label color)
    (create-instance 'menu-name opal:aggregate
					  (:left x)(:top y)(:width 500)
					  (:height 500))
    (opal:add-components top-agg menu-name)

    ; create items
    (setq fnt (create-instance NIL opal:font
			   (:size :large)
			   (:family :serif)))
    (dolist (pair pairs)
      (setq label (car pair)
	    color (if *color-p* (cdr pair) opal:black))
      (setq this-item
	    (create-instance NIL opal:aggregate
	                (:name (concatenate 'string label "-agg"))
	                (:left x)
			(:prev-item prev-item)
			(:top (if prev-item (o-formula
					      (+ (gvl :prev-item :top)
						 (gvl :prev-item :height)
						 10)
					     0)
				  y))
			(:width (o-formula (+ shad-off gray-size2 white-off2
						 (gvl :parent :max-width)) 0))
			(:height (o-formula (+ shad-off gray-size2 white-off2
						  (gvl :my-string :height))))))
      (setf outline
	    (create-instance NIL opal:rectangle
	      (:name (concatenate 'string label "-outline"))
	      (:left (o-formula (+ (gvl :parent :left)
				      (if (gvl :parent :interim-selected)
					  shad-off 0)) 0))
	      (:top (o-formula (+ (gvl :parent :top)
				      (if (gvl :parent :interim-selected)
					  shad-off 0)) 0))
	      (:width (o-formula (- (gvl :parent :width) shad-off) 0))
	      (:height (o-formula (- (gvl :parent :height) shad-off) 0))
	      (:filling-style (create-instance nil opal:gray-fill
				(:foreground-color color)))))
      (setf outline2
	    (create-instance NIL opal:rectangle
	      (:name (concatenate 'string label "-outline2"))
	      (:outline outline)
	      (:left (o-formula (+ (gvl :outline :left) gray-size) 0))
	      (:top (o-formula (+ (gvl :outline :top) gray-size) 0))
	      (:width (o-formula (- (gvl :outline :width) gray-size2) 0))
	      (:height (o-formula (- (gvl :outline :height) gray-size2) 0))
	      (:filling-style opal:white-fill)))

      (setq text-item
	    (create-instance NIL opal:text
	      (:string label)
	      (:font fnt)
	      (:line-style (create-instance nil opal:line-style
			     (:constant T)
			     (:line-thickness 2)
			     (:foreground-color color)))
	      (:outline2 outline2)
	      (:left (o-formula (+ (gvl :outline2 :left) white-off) 0))
	      (:top (o-formula (+ (gvl :outline2 :top) white-off) 0))))
      (setq inv-box
	    (create-instance NIL opal:rectangle
	      (:text-item text-item)
	      (:left (o-formula (gvl :text-item :left) 0))
	      (:top (o-formula (gvl :text-item :top) 0))
	      (:width (o-formula (gvl :text-item :width)))
	      (:height (o-formula (gvl :text-item :height)))
	      (:visible (o-formula (eq (gvl :parent) (gv menu-name :selected))))
	      (:draw-function :xor)
	      (:filling-style (create-instance nil opal:black-fill
				(:foreground-color color)))
	      (:line-style opal:no-line)
	      (:fast-redraw-p T)))

      (s-value this-item :my-string text-item)

      (setf shadow
	    (create-instance NIL opal:rectangle
	      (:name (concatenate 'string label "-shadow"))
	      (:outline outline)
	      (:left (o-formula (+ (gvl :parent :left) shad-off) 0))
	      (:top (o-formula (+ (gvl :parent :top) shad-off) 0))
	      (:width (o-formula (gvl :outline :width) 0))
	      (:height (o-formula (gvl :outline :height) 0))
	      (:filling-style (g-value inv-box :filling-style))
	      (:visible (o-formula (not (gvl :parent :interim-selected))))))
      

      (opal:add-components this-item shadow outline outline2 text-item inv-box)
      (opal:add-components menu-name this-item)

      (setf prev-item this-item))

    (s-value menu-name :max-width
	     (o-formula 
	       (let ((maxw 0))
		 (dolist (item (g-value menu-name :components))
		   (setf maxw (MAX maxw (gv item :my-string :width))))
		 maxw) 0))

    menu-name))


;;-----------------------------------



(defun center-y (other-obj)
  (+ (gv other-obj :top)
     (floor (- (gv other-obj :height)(gvl :height)) 2)))

(defun right-x (obj)
  (+ (gv obj :left) (gv obj :width) 1))

(defun bottom-y (obj)
  (+ (gv obj :top) (gv obj :height) 1))

;;; radio buttons allow only one to be chosen
(defparameter circle-size 12)
(defparameter small-circle-size 12)

(defconstant off-default 3)

;;; shape can be :rectangle or :circle or :rountangle
;;; width and height are of the area for the *label*, the whole button will
;;; be larger.  left and top are for the whole button.
(defun make-gray-floating-object (name label-obj top &key (shape opal:rectangle))
  
  (let (object-agg outline outline2 shadow)
    
    (setq object-agg
	  (kr:create-instance NIL opal:aggregate
			      (:name name)
			      (:visible t)
			      (:top top)
			      (:label-obj label-obj)
			      (:left (o-formula (+ 5 (right-x (gvl :label-obj)))))
			      (:shad-off 3)
			      (:white-off off-default)
			      (:gray-size off-default)
			      (:white-off2 (* 2 off-default))
			      (:gray-size2 (* 2 off-default))
			      (:width
			       (o-formula (+ (gvl :shad-off)
					     (gvl :gray-size2)
					     (gvl :white-off2)
					     circle-size) 0))
			      (:height
			       (o-formula (+ (gvl :shad-off)
					     (gvl :gray-size2)
					     (gvl :white-off2)
					     circle-size)))))
    
    (setf outline
	  (kr:create-instance NIL shape
			      (:name (concatenate 'string name "-outline"))
			      (:left (o-formula
				       (+ (gvl :parent :left)
					  (if (gvl :parent :interim-selected)
					      (gvl :parent :shad-off) 0)) 0))
			      (:top (o-formula
				      (+ (gvl :parent :top)
					 (if (gvl :parent :interim-selected)
					     (gvl :parent :shad-off) 0)) 0))
			      (:width (o-formula
				        (- (gvl :parent :width) (gvl :parent :shad-off)) 0))
			      (:height (o-formula
					 (- (gvl :parent :height) (gvl :parent :shad-off)) 0))
			      (:filling-style opal:gray-fill)))
    (setf outline2
	  (kr:create-instance NIL shape
			      (:name (concatenate 'string name "-outline2"))
			      (:left (o-formula
				       (+ (gvl :parent :outline :left)
					  (gvl :parent :gray-size)) 0))
			      (:top (o-formula
				      (+ (gvl :parent :outline :top)
					 (gvl :parent :gray-size)) 0))
			      (:width (o-formula
				        (- (gvl :parent :outline :width)
					   (gvl :parent :gray-size2)) 0))
			      (:height (o-formula
					 (- (gvl :parent :outline :height)
					    (gvl :parent :gray-size2)) 0))
			      (:filling-style opal:white-fill)))
    
    (s-value object-agg :inner-left
		   (o-formula (+ (gvl :outline2 :left) (gvl :white-off)) 0))
    (s-value object-agg :inner-top
		   (o-formula (+ (gvl :outline2 :top) (gvl :white-off)) 0))
    
    (setf shadow
	  (kr:create-instance NIL shape
			      (:name (concatenate 'string name "-shadow"))
			      (:left (o-formula
				       (+ (gvl :parent :left) 
					  (gvl :parent :shad-off)) 0))
			      (:top (o-formula
				      (+ (gvl :parent :top)
					 (gvl :parent :shad-off)) 0))
			      (:width (o-formula (gvl :parent :outline :width) 0))
			      (:height (o-formula (gvl :parent :outline :height) 0))
			      (:filling-style opal:black-fill)
			      (:visible
			       (o-formula
				 (not (gvl :parent :interim-selected))))))
    
    (s-value object-agg :shadow shadow)
    (s-value object-agg :outline outline)
    (s-value object-agg :outline2 outline2)
    
    (opal:add-components object-agg shadow outline outline2)
    object-agg))

(defparameter vp NIL)

(defun make-Xs (button-agg)
  (let (agg l1 l2)
    (dolist (button (g-value button-agg :components))
      (setq agg (create-instance NIL opal:aggregate (:name "X-agg")))
      (setq l1 (create-instance NIL opal:line (:name "X line 1")
                  (:button button)
                  (:x1 (o-formula (gvl :button :inner-left) 1))
                  (:y1 (o-formula (gvl :button :inner-top) 1))
                  (:x2 (o-formula (+ (gvl :x1) circle-size)))
                  (:y2 (o-formula (+ (gvl :y1) circle-size)))
                  ;; On Mac, XOR'ed objects appear black anyway
                  (:line-style #+apple opal:line-2
                               #-apple (o-formula (if (gv menu3 :selected)
						      (gv menu3 :selected
                                                                :my-string
                                                                :line-style)
						      opal:line-2)))
                  (:fast-redraw-p T)
                  (:draw-function :xor)
                  (:visible (o-formula (gvl :button :selected)))))
      (setq l2 (create-instance NIL opal:line (:name "X line 2")
	          (:button button)
                  (:x1 (o-formula (+ (gvl :x2) circle-size)))
                  (:y1 (o-formula (gvl :button :inner-top) 1))
                  (:x2 (o-formula (gvl :button :inner-left) 1))
                  (:y2 (o-formula (+ (gvl :y1) circle-size)))
                  (:line-style #+apple opal:line-2
                               #-apple (o-formula (if (gv menu3 :selected)
						      (gv menu3 :selected
                                                                :my-string
                                                                :line-style)
						      opal:line-2)))
                  (:fast-redraw-p T)
                  (:draw-function :xor)
                  (:visible (o-formula (gvl :button :selected)))))
      (opal:add-components agg l1 l2)
      (opal:add-components button agg))
    agg))

(defun make-circle (button-agg)
  (create-instance NIL opal:circle (:name "circle marker")
     (:button-agg button-agg)
     (:left (o-formula (gvl :button-agg :selected :inner-left) 0))
     (:top (o-formula (gvl :button-agg :selected :inner-top) 0))
     (:width small-circle-size)
     (:height small-circle-size)
     (:draw-function :xor)
     (:fast-redraw-p T)
     ;; On Mac, XOR'ed objects appear black anyway
     (:filling-style #+apple opal:black-fill
                     #-apple (o-formula (if (kr:schema-p (gv menu3 :selected))
                                            (gv menu3 :selected :components
                                                      :filling-style)
                                            opal:black-fill)))
     (:line-style opal:no-line)
     (:visible (o-formula (gvl :button-agg :selected)))))

(defun make-X-buttons (name string-list fnt left top grey-p)
  (let (agg allbuttons label-obj circle button (prev-item NIL) greyobj)
    (setf allbuttons (create-instance nil opal:aggregate
			      (:name (concatenate 'string name "-allbuttons"))))
    (setf agg (create-instance nil opal:aggregate (:name name)))
    (opal:add-component agg allbuttons)
    (dolist (str string-list)
      (setf label-obj (create-instance nil opal:text
				 (:name (concatenate 'string name "-" str "-label"))
				 (:string str)
				 (:font fnt)
				 (:prev-item prev-item)
				 (:left (if prev-item
					 (o-formula (+ 10 (right-x (gvl :prev-item))))
					 left))
				 ;; :top filled in after button created
				 ))
      (setf button (make-gray-floating-object
		    (concatenate 'string  name "-" str "-button")
		    label-obj top))
      (setq prev-item button)
      (s-value label-obj :button button)
      (s-value label-obj :top (o-formula (center-y (gvl :button))))
      (opal:add-component agg label-obj)
      (opal:add-component allbuttons button))

    (setq circle (make-Xs allbuttons))
    (unless (g-value circle :parent) (opal:add-component agg circle))
    (when grey-p
      (setf greyobj (create-instance NIL opal:rectangle
				     (:line-style opal:no-line)
				     (:filling-style opal:gray-fill)
				     (:left left)(:top top)
				     (:width 320)
				     (:height 30)
				     (:draw-function :or))))
    (create-instance NIL inter:button-interactor
				(:start-where `(:element-of ,allbuttons))
				(:window vp)
				(:how-set :list-toggle))
    (list agg greyobj)))

(defun make-radio-buttons (name string-list fnt left top)
  (let (agg allbuttons label-obj circle button (prev-item NIL))
    (setf allbuttons (create-instance nil opal:aggregate
			      (:name (concatenate 'string name "-allbuttons"))))
    (setf agg (create-instance nil opal:aggregate (:name name)))
    (opal:add-component agg allbuttons)
    (dolist (str string-list)
      (setf label-obj (create-instance nil opal:text
				 (:name (concatenate 'string name "-" str "-label"))
				 (:string str)
				 (:font fnt)
				 (:prev-item prev-item)
				 (:left (if prev-item
					 (o-formula (+ 10 (right-x (gvl :prev-item))))
					 left))
				 ;; :top filled in after button created
				 ))
      (setf button (make-gray-floating-object
		    (concatenate 'string  name "-" str "-button")
		    label-obj top :shape opal:circle))
      (setq prev-item button)
      (s-value label-obj :button button)
      (s-value label-obj :top (o-formula (center-y (gvl :button))))
      (opal:add-component agg label-obj)
      (opal:add-component allbuttons button))

    (setq circle (make-circle allbuttons))
    (opal:add-component agg circle)
    (create-instance NIL inter:button-interactor
				(:start-where `(:element-of ,allbuttons))
				(:window vp)
				(:how-set :set))  ; only one can be set at a time
    agg))


(defparameter agg NIL)
(defvar inter3 NIL)
(defvar circles NIL)
(defvar circles2 NIL)

(defparameter fnt NIL)
(defparameter fnti NIL)

(defun do-stop ()
  (opal:destroy vp))

(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  (setq vp (kr:create-instance NIL inter:interactor-window
                               (:left #-apple 600 #+apple 225)
			       (:top 70)(:width 400)(:height 360)
                               (:double-buffered-p double-buffered-p)
			       (:title "GARNET 3D") (:icon-title "3D")))
  (setq agg (s-value vp :aggregate (create-instance NIL opal:aggregate)))

  (setq fnt (kr:create-instance NIL opal:font
			(:family :serif) (:face :roman) (:size :small)))
  (setq fnti (kr:create-instance NIL opal:font
			(:family :serif) (:face :italic) (:size :large)))


  (setq menu3 (make-fixed-menu3 agg
			    (list (cons "Purple" opal:purple)
				  (cons "Red" opal:red)
				  (cons "Blue" opal:blue)
				  (cons "Green" opal:green)
				  (cons "Yellow" opal:yellow)) 10 10))

  (setq inter3 (create-instance NIL
				  inter:menu-interactor 
				  (:start-where
				   `(:element-of ,menu3))
				  (:window vp)))

  (opal:update vp)

  (setf circles (make-radio-buttons "radio" '("Left" "Middle" "Right") fnt
				    150 10))

  (setf circles2 (car (make-X-buttons "radio3" '("Bold" "Italic" "Underline") fnt
				 150 50 NIL)))
  (opal:add-components agg circles circles2)

  (opal:update vp)

  (Format T "~%Demo-3D:
  Left button operates the 3 different menus.~%")
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

