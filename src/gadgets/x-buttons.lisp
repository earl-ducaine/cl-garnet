;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  X-Button-Panel
;;;
;;;  Features and operation of X-buttons:
;;;     1)  X-button-panel is a set of rectangular buttons with text on one
;;;         side.  Any number of buttons may be selected at one time.
;;;     2)  Click the left mouse button in a button to cause an X to be
;;;         superimposed on the button.
;;;     3)  The top level :value slot points to a list of the strings of the
;;;         currently selected buttons.
;;;     4)  The top level :value-obj slot points to the list of currently
;;;         selected buttons, and can be set directly with S-VALUE to select
;;;         a set of buttons.
;;;     5)  The :items slot may contain functions to be executed as each
;;;         button becomes selected, and :selection-function may contain a
;;;         function to be executed when any button becomes selected.
;;;
;;;  Customizable slots:
;;;     1)  All customizable slots of an aggrelist:
;;;            Direction -- :vertical or :horizontal
;;;            V-spacing -- distance between buttons, if vertical orientation
;;;            H-spacing -- same, if horizontal orientation
;;;            Fixed-width-p -- whether to put buttons in fields of constant
;;;                             width, specified in :fixed-width-size.
;;;            Fixed-height-p -- same, but with heights
;;;            Fixed-width-size -- width of all components (default is the
;;;                                width of the widest button)
;;;            Fixed-height-size -- same, but with heights 
;;;            H-align -- how to align buttons, if vertical orientation
;;;                       :left, :center, or :right
;;;            Rank-margin -- after this many components, a new row (or column)
;;;                           will be started
;;;            Pixel-margin -- absolute position in pixels after which a new
;;;                            row (or column) will be started
;;;            Indent -- amount to indent the new row (or column) in pixels
;;;     2)  Left, top, button-width, button-height
;;;     3)  Shadow-offset -- the amount of shadow that shows under the buttons
;;;     4)  Text-offset -- the distance from the text to the buttons
;;;     5)  Gray-width -- the width of the gray border on the buttons
;;;     6)  Font -- the font in which the button labels will appear
;;;     7)  Text-on-left-p -- whether text will appear on left side of buttons
;;;                           (NIL implies text will appear to the right)
;;;     8)  Items -- This can be: 
;;;                  A list of strings, as in '("Large" ...), or
;;;                  a list of atoms, as in '(:center ...), or
;;;                  a list of string/function pairs, '(("Cut" Cut-FN) ...), or
;;;                  a list of atom/function pairs, '((:center Center-FN) ...).
;;;                  Each function will be executed when the associated button
;;;                  becomes selected.  The parameters are the top-level
;;;                  GADGET and the ITEM-STRING.
;;;     9)  Selection-function -- Global function to be executed when any
;;;                               button is selected.  Parameters are the
;;;                               top-level GADGET and the ITEM-STRING.
;;;
;;;  NOTE:  This module requires several schemata defined in GAD-button-parts.
;;;         Thus, GAD-button-parts.fasl must be loaded before this module.
;;;
;;;  X-buttons demo:
;;;     This module contains a function which creates a window and a panel of
;;;     X-buttons.  To run it, enter (GARNET-GADGETS:x-buttons-go).  To stop,
;;;     enter (GARNET-GADGETS:x-buttons-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

;;; CHANGE LOG:
;;; 05/13/93  Andrew Mickish - :prev-visible ---> :prev
;;; 02/23/93  Andrew Mickish - Added :string-set-func
;;; 02/10/93  Andrew Mickish - Made :items of type items-type
;;; 01/25/93  Andrew Mickish - Fixed kr-path declaration (0 ==> 1) in :string
;;;              slot of :item-prototype
;;; 12/15/92  Andrew Mickish - Added type and parameter declarations
;;; 08/31/92  Andrew Mickish - Changed :fixed-height-size formula
;;; 06/16/92  Andrew Mickish - Added objects in :items list
;;; 04/17/92  Andrew Mickish - Now final-feedback objs are invisible when the
;;;              parent gadget is invisible
;;; 02/11/92 Andrew Mickish - Added :maybe-constant list
;;; 05/14/91 Andrew Mickish - Added "remove" to :x-button-list's :selected slot
;;; 04/15/91 Andrew Mickish - Added :notice-items-changed, :add-item, and
;;;              :remove-item methods
;;; 07/04/90 Pavan Reddy - changed the font default to opal:default-font
;;; 07/03/90 Pavan Reddy - altered X-BUTTON and X-BUTTON-LIST prototypes
;;;              so single button instances can be created
;;; 07/02/90 Andrew Mickish - Converted circularity between :value and
;;;              :selected slot;  Now the final-function sets :value-obj which
;;;              propagates to :value and :selected.
;;; 01/30/90 Andrew Mickish -  Added :selected slot to X-BUTTON-LIST
;;;              so that :value of panel can be set directly.
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(X-Button X-Button-Panel))
  #+garnet-test
  (export '(X-Buttons-Go X-Buttons-Stop
	    X-Buttons-Obj X-Button-Obj X-Buttons-Top-Agg X-Buttons-Win
	    X-Button-Obj2)))

(create-instance 'X-BUTTON opal:aggregadget
  :declare ((:parameters :left :top :button-width :button-height
			 :shadow-offset :text-offset :gray-width
			 :text-on-left-p :string :toggle-p :font :value
			 :selection-function :visible)
	    (:type ((integer 0) :button-width :button-height)
		   (integer :shadow-offset :text-offset :gray-width)
		   (kr-boolean :text-on-left-p :toggle-p)
		   ((or string keyword (satisfies schema-p)) :string)
		   ((or null string keyword (satisfies schema-p)) :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :button-width :button-height
			     :shadow-offset :text-offset :gray-width
			     :text-on-left-p :toggle-p :string :font :visible))
   (:left 0) (:top 0)
   (:button-width 20)
   (:button-height 20)
   (:shadow-offset 5) (:text-offset 5) (:gray-width 3)
   (:text-on-left-p T)
   (:toggle-p T)
   (:string "X Button")
   (:font opal:default-font)
   (:selection-function NIL)

   (:value (o-formula (if (gvl :selected) (gvl :string))))
   (:selected (o-formula (gvl :value)))

   (:floating-left (o-formula (+ (gvl :button-left)
				 (if (gvl :interim-selected)
				     (gvl :shadow-offset)
				     0))))
   (:floating-top (o-formula (+ (gvl :button-top)
				(if (gvl :interim-selected)
				    (gvl :shadow-offset)
				    0))))

   (:button-left (o-formula (if (gvl :text-on-left-p)
				(+ (gvl :left) (gvl :text-width)
				   (gvl :text-offset))
				(gvl :left))))
   (:button-top (o-formula (- (gvl :center-y)
			      (floor (gvl :button-unit-height) 2))))
   (:button-unit-width (o-formula (+ (gvl :button-width) (gvl :shadow-offset))))
   (:button-unit-height (o-formula (+ (gvl :button-height)
				      (gvl :shadow-offset))))
   (:text-width (o-formula (gvl :text :width)))
   (:width (o-formula (+ (gvl :text-width) (gvl :text-offset)
			 (gvl :button-unit-width))))
   (:height (o-formula (MAX (gvl :text :height)
			    (gvl :button-unit-height))))
   (:center-y (o-formula (+ (gvl :top)
			    (floor (gvl :height) 2))))
   (:text-label-prototype (create-instance NIL BESIDE-BUTTON-TEXT))
   (:parts
    `((:shadow ,BUTTON-SHADOW-RECT)
      (:gray-outline ,GRAY-RECT-OUTLINE)
      (:white-field ,WHITE-RECT-FIELD)
      (:text ,#'opal::Single-Button-Get-Label)
      (:feedback-obj ,opal:aggregadget
           (:left ,(o-formula (gv (kr-path 0 :parent :white-field) :left)))
	   (:top ,(o-formula (gv (kr-path 0 :parent :white-field) :top)))
	   (:width ,(o-formula (gv (kr-path 0 :parent :white-field) :width)))
	   (:height ,(o-formula (- (gv (kr-path 0 :parent :white-field) :height)
				   1)))
	   (:right ,(o-formula (+ (gvl :left) (gvl :width))))
	   (:bottom ,(o-formula (+ (gvl :top) (gvl :height))))
	   (:visible ,(o-formula (let ((p (kr-path 0 :parent)))
				   (and (gv p :visible)
					(gv p :selected)))))
	   (:parts
	    ((:neg-slope ,opal:line
		 (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
		 (:y1 ,(o-formula (gv (kr-path 0 :parent) :top)))
		 (:x2 ,(o-formula (gv (kr-path 0 :parent) :right)))
		 (:y2 ,(o-formula (gv (kr-path 0 :parent) :bottom)))
		 (:line-style ,opal:line-2))
	     (:pos-slope ,opal:line
		 (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
		 (:y1 ,(o-formula (gv (kr-path 0 :parent) :bottom)))
		 (:x2 ,(o-formula (gv (kr-path 0 :parent) :right)))
		 (:y2 ,(o-formula (gv (kr-path 0 :parent) :top)))
		 (:line-style ,opal:line-2)))))))

   (:interactors
    `((:x-button-press ,inter:button-interactor
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:start-where ,(o-formula (list :in-box (gvl :operates-on))))
	(:how-set ,(o-formula (if (gvl :operates-on :toggle-p) :toggle :set)))
	(:final-function
	 ,#'(lambda (interactor button)
	      (declare (ignore interactor))
	      (let ((string (when (g-value button :selected)
			      (g-value button :string))))
	      ; Execute selection function
	      (kr-send button :selection-function button string))))))))



(create-instance 'X-BUTTON-PANEL opal:aggregadget
  :declare ((:parameters :left :top :button-width :button-height :shadow-offset
			 :text-offset :gray-width :text-on-left-p :font :items
			 :value :direction :v-spacing :h-spacing :fixed-width-p
			 :fixed-height-p :fixed-width-size :fixed-height-size
			 :h-align :rank-margin :pixel-margin :indent
			 :selection-function :visible)
	    (:type (integer :v-spacing :h-spacing :indent :shadow-offset
			    :text-offset :gray-width)
		   ((or null (integer 0)) :fixed-width-size :fixed-height-size
		    :rank-margin :pixel-margin)
		   ((integer 0) :button-width :button-height)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   (kr-boolean :text-on-left-p :fixed-width-p
		    :fixed-height-p)
		   (items-type :items)
		   (list :value-obj :value)
		   ((member :vertical :horizontal) :direction)
		   ((member :left :center :right) :h-align)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :direction :v-spacing :h-spacing
			     :h-align :fixed-width-p :fixed-width-size
			     :fixed-height-p :fixed-height-size :indent
			     :rank-margin :pixel-margin
			     :button-width :button-height :shadow-offset
			     :text-offset :gray-width :text-on-left-p
			     :font :items :visible))
   
   ;; Customizable slots
   ;;
   (:left 0) (:top 0)
   (:direction :vertical)
   (:v-spacing 5) (:h-spacing 5)
   (:h-align (o-formula (if (gvl :text-on-left-p) :right :left)))
   (:fixed-width-p T)
   (:fixed-width-size (o-formula (+ (gvl :x-button-list :tail
					 :max-text-width-thus-far)
				    (gvl :button-unit-width)
				    (gvl :text-offset))))
   (:fixed-height-p NIL)
   (:fixed-height-size (o-formula (MAX (gvl :button-unit-width)
				       (gvl :radio-button-list :tail
					    :max-text-height-thus-far))))
   (:indent 0) (:rank-margin NIL) (:pixel-margin NIL)
   (:button-width 20)
   (:button-height 20)
   (:shadow-offset 5)
   (:text-offset 5)
   (:gray-width 3)
   (:text-on-left-p T)
   (:font opal:default-font)
   (:selection-function NIL)
   (:items '("X-label 1" "X-label 2" "X-label 3"))

   (:value-obj NIL)
   (:value (o-formula (let ((obj-list (gvl :value-obj)))
			(if obj-list
			    (mapcar #'(lambda (object)
					(g-value object :string))
				    obj-list)))))

   (:actions-p (o-formula (listp (first (gvl :items)))))
   (:button-unit-width (o-formula (+ (gvl :button-width) (gvl :shadow-offset))))
   (:button-unit-height (o-formula (+ (gvl :button-height)
				      (gvl :shadow-offset))))
   (:width (o-formula (gvl :x-button-list :width)))
   (:height (o-formula (gvl :x-button-list :height)))

   (:text-label-prototype
    (create-instance NIL BESIDE-BUTTON-TEXT
      (:left (o-formula
	      (let ((base-left (+ (gvl :parent :left)
				  (if (gvl :parent :text-on-left-p) 0
				      (+ (gvl :parent :button-unit-width)
					 (gvl :parent :text-offset))))))
		(case (gv (gvl :parent :parent :parent) :h-align)
		  (:left base-left)
		  (:center (+ base-left
			      (floor (- (gvl :parent :text-width)
					(gvl  :width)) 2)))
		  (:right (+ base-left (- (gvl :parent :text-width)
					  (gvl :width))))))))))
   (:parts
    `((:x-button-list ,ITEMS-AGGLIST
       (:selected
	,(o-formula
	  (let ((value-list (gv (kr-path 0 :parent) :value)))
	    (if value-list
		(let ((components (gvl :components))
		      ; Index-List is a list of numbers, one for each selected
		      ; button.  The numbers correspond to the rank of the
		      ; items in the :items slot.
		      ; The "remove" is needed if the :items slot ever changes
		      ; and the :value no longer corresponds to the :items.
		      (index-list (remove NIL
		       (if (gvl :parent :actions-p)
			   (mapcar #'(lambda (item)
				       (position item (gvl :items) :test
						 #'(lambda (val item)
						     (equal val (car item)))))
				   value-list)
			   (mapcar #'(lambda (item)
				       (position item (gvl :items)
						 :test #'equal))
				   value-list)))))
		  (mapcar #'(lambda (index) (nth index components))
			  index-list))))))
       (:item-prototype
	(,X-BUTTON
	 (:shadow-offset ,(o-formula (gv (kr-path 0 :parent :parent)
					 :shadow-offset)))
	 (:text-offset ,(o-formula (gv (kr-path 0 :parent :parent) :text-offset)))
	 (:gray-width ,(o-formula (gv (kr-path 0 :parent :parent) :gray-width)))
	 (:text-on-left-p ,(o-formula (gv (kr-path 0 :parent :parent)
					  :text-on-left-p)))
	 (:max-text-width-thus-far
	  ,(o-formula (if (gvl :prev)
			  (MAX (gvl :prev :max-text-width-thus-far)
			       (gvl :text :width))
			  (gvl :text :width))))
	 (:max-text-height-thus-far
	  ,(o-formula (if (gvl :prev)
			  (MAX (gvl :prev :max-text-height-thus-far)
			       (gvl :text :height))
			  (gvl :text :height))))
	 (:button-width ,(o-formula (gv (kr-path 0 :parent :parent)
					:button-width)))
	 (:button-height ,(o-formula (gv (kr-path 0 :parent :parent)
					 :button-height)))
	 (:button-unit-width ,(o-formula (gv (kr-path 0 :parent :parent)
					     :button-unit-width)))
	 (:button-unit-height ,(o-formula (gv (kr-path 0 :parent :parent)
					      :button-unit-height)))
	 (:text-width
	  ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-width-p)
			  (- (gv (kr-path 0 :parent :parent)
				 :fixed-width-size)
			     (gvl :button-unit-width) (gvl :text-offset))
			  (gvl :text :width))))
	 (:height ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-height-p)
				  (gv (kr-path 0 :parent :parent)
				      :fixed-height-size)
				  (MAX (gvl :text :height)
				       (gvl :button-unit-height)))))
      
	 ;; Conditional formulas are required to allow either a list of
	 ;; strings or a list of string/function pairs in the :items slot.
	 (:string ,(o-formula (if (gv (kr-path 1 :parent :parent) :actions-p)
				  (first (nth (gvl :rank)
					      (gv (kr-path 0 :parent) :items)))
				  (nth (gvl :rank)
				       (gv (kr-path 0 :parent) :items)))))
	 (:action ,(o-formula (when (gv (kr-path 0 :parent :parent) :actions-p)
				(second (nth (gvl :rank)
					     (gv (kr-path 0 :parent) :items))))))
      
	 (:font ,(o-formula (gv (kr-path 0 :parent :parent) :font)))
	 (:parts
	  (:shadow :gray-outline :white-field
	   (:text :modify ,#'opal::Panel-Get-Label)
	   (:feedback-obj :modify
	     (:visible ,(o-formula (let ((p (kr-path 0 :parent)))
				     (and (gv p :visible)
					  (member p (gv p :parent
							:selected)))))))))
      (:interactors
       ((:x-button-press :omit))))))))

   (:interactors
    `((:X-BUTTON-PRESS ,inter:button-interactor 
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :x-button-list))))
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:how-set :list-toggle)
	(:final-function
	 ,#'(lambda (interactor final-obj-over)
	      (let* ((action (g-value final-obj-over :action))
		     (gadget (g-value interactor :operates-on))
		     (string (g-value final-obj-over :string))
		     (value-obj (g-value gadget :x-button-list :selected)))

		;; Propagate change toward :value slot
		(s-value gadget :value-obj value-obj)

		;; Global function executed whenever selections change
		(kr-send gadget :selection-function gadget
			 (mapcar #'(lambda (object) (g-value object :string))
				 value-obj))

		; If the button is selected, then execute the local function
		(when (member final-obj-over value-obj)
		  (when action
		    (funcall action gadget string))))))))))


(define-method :add-local-item X-BUTTON-PANEL (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item :x-button-list args))
(define-method :add-item X-BUTTON-PANEL (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item :x-button-list args))
   
(define-method :remove-local-item X-BUTTON-PANEL
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item :x-button-list key))
(define-method :remove-item X-BUTTON-PANEL
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item :x-button-list key))

(s-value X-BUTTON-PANEL :change-item (g-value opal:aggrelist :change-item))
(s-value X-BUTTON-PANEL :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(define-method :string-set-func X-BUTTON-PANEL
    (gadget-obj str-obj final-event final-string)
  (let ((aggrel (g-value gadget-obj :X-BUTTON-LIST)))
    (opal::Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				      final-event final-string :rank)))


;;;
;;;  DEMO FUNCTION
;;;


  
#+garnet-test (defparameter X-Buttons-win NIL)
#+garnet-test (defparameter X-Buttons-top-agg NIL)
#+garnet-test (defparameter X-Buttons-Obj NIL)

#+garnet-test
(defun X-Buttons-Go ()
  (create-instance 'x-buttons-win inter:interactor-window
    #-apple(:left 650) #+apple (:left 300)
    #-apple(:top 5) #+apple(:top 45)
    (:height 360)(:width 350))
  (s-value X-Buttons-win
	   :aggregate
	   (create-instance 'x-buttons-top-agg opal:aggregate
	      (:overlapping NIL)))
  (create-instance 'x-buttons-obj X-Button-Panel
     (:left 30) (:top 20)
     (:items `("Ether" "Phlogiston" "Quintessence" "Alkahest"
	       ,(create-instance NIL opal:line
		  (:x1 (o-formula (gvl :left)))
		  (:y1 (o-formula (+ 4 (gvl :top))))  ;offset by 1/2 thickness
		  (:x2 (o-formula (+ 40 (gvl :x1))))
		  (:y2 (o-formula (gvl :y1)))
		  (:width 40) (:height 8)
		  (:line-style opal:line-8))
	       ,(create-instance NIL opal:oval
		  (:height 15) (:width 40)
		  (:filling-style opal:red-fill)
		  (:line-style opal:line-2)))))
  (create-instance 'x-button-obj X-Button
     (:left 170) (:top 50)
     (:text-on-left-p NIL))
  (create-instance 'x-button-obj2 X-Button
     (:left 170) (:top 150)
     (:string (create-instance NIL opal:oval
		(:width 40) (:height 20)
		(:filling-style opal:yellow-fill))))

  (opal:add-components X-Buttons-top-agg X-Buttons-Obj X-Button-Obj
		       X-Button-Obj2)
  (format t "Leftdown on an X-button causes an X to be superimposed on the~%")
  (format t "button, executes the function locally assigned to the button~%")
  (format t "(if there is one), and executes the function specified in~%")
  (format t ":selection-function (if there is one).~%")
  (opal:update X-Buttons-win))


#+garnet-test
(defun X-Buttons-Stop ()
  (opal:destroy X-Buttons-win))
