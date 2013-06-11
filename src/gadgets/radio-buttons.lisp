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
;;;  Radio-Button-Panel
;;;
;;;  Features and operation of Radio Buttons:
;;;     1)  Radio-button-panel is a set of circular buttons with text on one
;;;         side.  Only one button may be selected at a time.
;;;     2)  Click the left mouse button in a button to cause an inverse circle
;;;         to be superimposed on the button.
;;;     3)  The top level :value slot points to the string of the currently
;;;         selected button.
;;;     4)  The top level :value-obj slot points to the currently selected
;;;         button, and can be set directly with S-VALUE to select a button.  
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
;;;     2)  Left, top, button-diameter
;;;     3)  Shadow-offset -- the amount of shadow that shows under the buttons
;;;     4)  Text-offset -- the distance from the text to the buttons
;;;     5)  Gray-width -- the width of the gray border on the buttons
;;;     6)  Text-on-left-p -- whether text will appear on left side of buttons
;;;                           (NIL implies text will appear to the right)
;;;     7)  Font  --  The font in which the button labels will appear
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
;;;  Radio Buttons demo:
;;;     This module contains a function which creates a window and a panel of
;;;     Radio Buttons.  To run it, enter (GARNET-GADGETS:radio-buttons-go).
;;;     To stop, enter (GARNET-GADGETS:radio-buttons-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

;;; CHANGE LOG:
;;; 05/13/93  Andrew Mickish - :prev-visible ---> :prev
;;; 02/23/93  Andrew Mickish - Added :string-set-func
;;; 02/10/93  Andrew Mickish - Made :items of type items-type
;;; 12/15/92  Andrew Mickish - Added type and parameter declarations
;;; 08/31/92  Andrew Mickish - Changed :fixed-height-size formula
;;; 06/16/92  Andrew Mickish - Added objects in :items list
;;; 06/05/92  Andrew Mickish - Changed :visible in final-feedback to consider
;;;              the :parent of obj-over
;;; 04/17/92  Andrew Mickish - Now final-feedback objs are invisible when the
;;;              parent gadget is invisible
;;; 02/17/92  Andrew Mickish - Added :maybe-constant lists
;;; 07/26/91  Andrew Mickish - Added :toggle-p slot to button and panel
;;; 05/14/91  Andrew Mickish - Fixed :selected formula in :radio-button-list
;;; 04/15/91  Andrew Mickish - Added :notice-items-changed, :add-item, and
;;;              :remove-item methods
;;; 11/28/90  Pavan Reddy - added formula to :value-obj slot of RADIO-BUTTON-
;;;              PANEL so :value and :value-obj slots remain consistent.
;;; 07/03/90  Andrew Mickish - Moved objects from GAD-button-parts.lisp into
;;;              :parts slot of RADIO-BUTTON; reimplemented RADIO-BUTTON so
;;;              that single instances can be created.
;;; 07/02/90  Andrew Mickish - Converted circularity between :value and
;;;              :selected slot;  Now the final-function sets :value-obj which
;;;              propagates to :value and :selected.
;;; 06/25/90  Andrew Mickish - Added :FINAL-FEEDBACK part to RADIO-BUTTON-PANEL
;;;              to utilize fast-redraw-p technology
;;; 01/30/90  Andrew Mickish - Added :selected slot to RADIO-BUTTON-LIST
;;;              so that :value of panel can be set directly.
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Radio-Button Radio-Button-Panel))
  #+garnet-test
  (export '(Radio-Buttons-Go Radio-Buttons-Stop Radio-Button-Obj
	    Radio-Buttons-Obj Radio-Buttons-Top-Agg Radio-Buttons-Win
	    Radio-Button-Obj2)))

(create-instance 'RADIO-BUTTON opal:aggregadget
  :declare ((:parameters :left :top :button-diameter :shadow-offset
			 :text-offset :gray-width :string :text-on-left-p
			 :toggle-p :value :font :selection-function :visible)
	    (:type ((integer 0) :button-diameter)
		   (integer :shadow-offset :text-offset :gray-width)
		   ((or string keyword (satisfies schema-p)) :string)
		   ((or null string keyword (satisfies schema-p)) :value)
		   (kr-boolean :text-on-left-p :toggle-p)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :button-diameter :shadow-offset
			     :text-offset :gray-width :string :text-on-left-p
			     :toggle-p :font :visible))
   (:left 0) (:top 0)
   (:button-diameter 23)
   (:shadow-offset 5) (:text-offset 5) (:gray-width 3)
   (:string "Radio button")
   (:text-on-left-p T)
   (:toggle-p T)
   (:font opal:default-font)
   (:value (o-formula (if (gvl :selected) (gvl :string))))
   (:selection-function NIL)

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
   (:button-unit-width (o-formula (+ (gvl :button-diameter)
				     (gvl :shadow-offset))))
   (:button-unit-height (o-formula (gvl :button-unit-width)))
   (:text-left (o-formula (if (gvl :text-on-left-p)
			      (gvl :left)
			      (+ (gvl :left) (gvl :button-unit-width)
				 (gvl :text-offset)))))
   (:text-width (o-formula (gvl :text :width)))
   (:width (o-formula (+ (gvl :text-width) (gvl :text-offset)
			 (gvl :button-unit-width))))
   (:height (o-formula (MAX (gvl :button-unit-height)
			    (gvl :text :height))))
   (:center-y (o-formula (+ (gvl :top)
			    (floor (gvl :height) 2))))

   (:selected (o-formula (gvl :value)))  ; Set by interactor
   (:text-label-prototype (create-instance NIL BESIDE-BUTTON-TEXT))
   (:parts
    `((:shadow ,opal:circle
          (:left ,(o-formula (+ (gv (kr-path 0 :parent) :button-left)
				(gv (kr-path 0 :parent) :shadow-offset))))
	  (:top ,(o-formula (+ (gv (kr-path 0 :parent) :button-top)
			       (gv (kr-path 0 :parent) :shadow-offset))))
	  (:width ,(o-formula (gv (kr-path 0 :parent) :button-diameter)))
	  (:height ,(o-formula (gv (kr-path 0 :parent) :button-diameter)))
	  (:filling-style ,opal:black-fill))
      (:gray-outline ,opal:circle
          (:left ,(o-formula (gv (kr-path 0 :parent) :floating-left)))
	  (:top ,(o-formula (gv (kr-path 0 :parent) :floating-top)))
	  (:width ,(o-formula (gv (kr-path 0 :parent) :button-diameter)))
	  (:height ,(o-formula (gv (kr-path 0 :parent) :button-diameter)))
	  (:filling-style ,opal:gray-fill))
      (:white-field ,opal:circle
	  (:left ,(o-formula (+ (gv (kr-path 0 :parent) :floating-left)
				(gv (kr-path 0 :parent) :gray-width))))
	  (:top ,(o-formula (+ (gv (kr-path 0 :parent) :floating-top)
			       (gv (kr-path 0 :parent) :gray-width))))
	  (:width ,(o-formula (- (gv (kr-path 0 :parent) :button-diameter)
				 (* 2 (gv (kr-path 0 :parent) :gray-width)))))
	  (:height ,(o-formula (- (gv (kr-path 0 :parent) :button-diameter)
				  (* 2 (gv (kr-path 0 :parent) :gray-width)))))
	  (:filling-style ,opal:white-fill))
      (:text ,#'opal::Single-Button-Get-Label)
      (:feedback-obj ,opal:circle
	  (:left ,(o-formula (+ 3 (gv (kr-path 0 :parent) :floating-left)
				  (gv (kr-path 0 :parent) :gray-width))))
	  (:top ,(o-formula (+ 3 (gv (kr-path 0 :parent) :floating-top)
			         (gv (kr-path 0 :parent) :gray-width))))
	  (:width ,(o-formula (- (gv (kr-path 0 :parent) :button-diameter)
				 (* 2 (gv (kr-path 0 :parent) :gray-width)) 6)))
	  (:height ,(o-formula (- (gv (kr-path 0 :parent) :button-diameter)
				  (* 2 (gv (kr-path 0 :parent) :gray-width)) 6)))
	  (:visible ,(o-formula (let ((p (kr-path 0 :parent)))
				  (gv p :visible)
				  (gv p :selected))))
	  (:line-style NIL) (:filling-style ,opal:black-fill)
	  (:fast-redraw-p T)(:draw-function :xor))))
   (:interactors
    `((:radio-button-press ,inter:button-interactor
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:start-where ,(o-formula (list :in-box (gvl :operates-on))))
	(:how-set ,(o-formula (if (gvl :operates-on :toggle-p) :toggle :set)))
	(:final-function
	 ,#'(lambda (interactor button)
	      (declare (ignore interactor))
	      (let ((string (when (g-value button :selected)
			      (g-value button :string))))
		; Execute the selection-function
		(kr-send button :selection-function button string))))))))




(create-instance 'RADIO-BUTTON-PANEL opal:aggregadget
  :declare ((:parameters :left :top :button-diameter :shadow-offset
			 :text-offset :gray-width :text-on-left-p :toggle-p
			 :font :items :value
			 :direction :v-spacing :h-spacing :fixed-width-p
			 :fixed-height-p :fixed-width-size :fixed-height-size
			 :h-align :rank-margin :pixel-margin :indent
			 :selection-function :visible)
	    (:type ((integer 0) :button-diameter)
		   ((or null (integer 0)) :fixed-width-size :fixed-height-size
		    :rank-margin :pixel-margin)
		   (integer :v-spacing :h-spacing :shadow-offset :text-offset
			    :gray-width)
		   (kr-boolean :text-on-left-p :toggle-p :fixed-width-p
		    :fixed-height-p)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   (items-type :items)
		   ((or null string keyword (satisfies schema-p)) :value)
		   ((member :vertical :horizontal) :direction)
		   ((member :left :center :right) :h-align)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :direction :v-spacing :h-spacing
			     :h-align :fixed-width-p :fixed-width-size
			     :fixed-height-p :fixed-height-size :indent
			     :rank-margin :pixel-margin 
			     :button-diameter :shadow-offset :text-offset
			     :gray-width :text-on-left-p :toggle-p :font
			     :items :visible))
   ;; Customizable slots
   ;;
   (:left 0) (:top 0)
   (:direction :vertical)
   (:v-spacing 5) (:h-spacing 5)
   (:h-align (o-formula (if (gvl :text-on-left-p) :right :left)))
   (:fixed-width-p T)
   (:fixed-width-size (o-formula (+ (gvl :radio-button-list :tail
					 :max-text-width-thus-far)
				    (gvl :button-unit-width)
				    (gvl :text-offset))))
   (:fixed-height-p T)
   (:fixed-height-size (o-formula (MAX (gvl :button-unit-width)
				       (gvl :radio-button-list :tail
					    :max-text-height-thus-far))))
   (:indent 0) (:pixel-margin NIL) (:rank-margin NIL)
   (:button-diameter 23)
   (:shadow-offset 5) (:text-offset 5) (:gray-width 3)
   (:text-on-left-p T)
   (:toggle-p NIL)
   (:font opal:default-font)
   (:selection-function NIL)
   (:items '("Radio-text 1" "Radio-text 2" "Radio-text 3" "Radio-text 4"))

   (:value-obj (o-formula (gvl :radio-button-list :selected)))
   (:value (o-formula (let ((obj (gvl :value-obj)))
			(if obj (gv obj :string)))))

   (:actions-p (o-formula (listp (first (gvl :items)))))
   (:button-unit-width (o-formula (+ (gvl :button-diameter)
				     (gvl :shadow-offset))))
   (:width (o-formula (gvl :radio-button-list :width)))
   (:height (o-formula (gvl :radio-button-list :height)))
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
    `((:radio-button-list ,ITEMS-AGGLIST
       (:selected
	,(o-formula
	  (let ((value (gv (kr-path 0 :parent) :value)))
	    (if value
		; Index is the position of the selected item in the :items slot
		(let ((index (if (gv (kr-path 0 :parent) :actions-p)
				 (position value (gvl :items) :test
					   #'(lambda (val item)
					       (equal val (car item))))
				 (position value (gvl :items) :test #'equal))))
		  (when index
		    (nth index (gvl :components))))))))
	 (:item-prototype
	  (,RADIO-BUTTON
	   (:selected ,(o-formula (equal (gvl :string)
					 (gvl :parent :parent :value))))
	   (:shadow-offset ,(o-formula (gv (kr-path 0 :parent :parent)
					   :shadow-offset)))
	   (:text-offset ,(o-formula (gv (kr-path 0 :parent :parent)
					 :text-offset)))
	   (:gray-width ,(o-formula (gv (kr-path 0 :parent :parent) :gray-width)))
	   (:button-diameter ,(o-formula (gv (kr-path 0 :parent :parent)
					     :button-diameter)))
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
	   (:text-width
	    ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-width-p)
			    (- (gv (kr-path 0 :parent :parent) :fixed-width-size)
			       (gvl :button-unit-width) (gvl :text-offset))
			    (gvl :text :width))))
	   (:height
	    ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-height-p)
			    (gv (kr-path 0 :parent :parent) :fixed-height-size)
			    (MAX (gvl :text :height)
				 (gvl :button-unit-height)))))

	   ;; Conditional formulas are required to allow either a list of
	   ;; strings or a list of string/function pairs in the :items slot.
	   (:string
	    ,(o-formula (if (gv (kr-path 0 :parent :parent) :actions-p)
			    (first (nth (gvl :rank)
					(gv (kr-path 1 :parent) :items)))
			    (nth (gvl :rank) (gv (kr-path 1 :parent) :items)))))
	   (:action ,(o-formula (when (gvl :parent :parent :actions-p)
				  (second (nth (gvl :rank)
					       (gvl :parent :items))))))
	   (:font ,(o-formula (gv (kr-path 0 :parent :parent) :font)))
	   (:parts
	    (:shadow :gray-outline :white-field
	     (:text :modify ,#'opal::Panel-Get-Label)
	     (:feedback-obj :omit)))
	   (:interactors
	    ((:radio-button-press :omit))))))
      (:FINAL-FEEDBACK ,opal:circle
          (:obj-over ,(o-formula (gvl :parent :radio-button-list :selected)))
	  (:left ,(o-formula (+ 3 (gvl :obj-over :white-field :left))))
	  (:top ,(o-formula (+ 3 (gvl :obj-over :white-field :top))))
	  (:width ,(o-formula (- (gvl :obj-over :white-field :width) 6)))
	  (:height ,(o-formula (- (gvl :obj-over :white-field :height) 6)))
	  (:visible ,(o-formula (let ((obj-over (gvl :obj-over)))
				  (and obj-over
				       (gv obj-over :parent)
				       (gv (kr-path 0 :parent) :visible)))))
	  (:fast-redraw-p T) (:draw-function :xor)
	  (:line-style NIL)
	  (:filling-style ,opal:black-fill))))

   (:interactors
    `((:RADIO-BUTTON-PRESS ,inter:button-interactor 
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :radio-button-list))))
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:how-set ,(o-formula (if (gvl :operates-on :toggle-p) :toggle :set)))
	(:final-function
	 ,#'(lambda (interactor final-obj-over)
	      (let* ((action (g-value final-obj-over :action))
		     (gadget (g-value interactor :operates-on))
		     (selected (g-value final-obj-over :selected))
		     (string (when selected
			       (g-value final-obj-over :string))))

		;; Propagate new selection toward :value slot
		(s-value gadget :value-obj (when selected final-obj-over))

		;; Global function for all items
		(kr-send gadget :selection-function gadget string)

		;; Local function assigned to item
		(when action
		  (funcall action gadget string)))))))))


(define-method :add-local-item RADIO-BUTTON-PANEL (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item :radio-button-list args))
(define-method :add-item RADIO-BUTTON-PANEL (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item :radio-button-list args))
   
(define-method :remove-local-item RADIO-BUTTON-PANEL
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item :radio-button-list key))
(define-method :remove-item RADIO-BUTTON-PANEL
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item :radio-button-list key))

(s-value RADIO-BUTTON-PANEL
	 :change-item
	 (g-value opal:aggrelist :change-item))
(s-value RADIO-BUTTON-PANEL
	 :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(define-method :string-set-func RADIO-BUTTON-PANEL
    (gadget-obj str-obj final-event final-string)
  (let ((aggrel (g-value gadget-obj :RADIO-BUTTON-LIST)))
    (opal::Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				      final-event final-string :rank)))


;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test (defparameter Radio-Buttons-Win NIL)
#+garnet-test (defparameter Radio-Buttons-Top-Agg NIL)
#+garnet-test (defparameter Radio-Buttons-Obj NIL)

#+garnet-test
(defun Radio-Buttons-Go ()
  (create-instance 'Radio-Buttons-Win inter:interactor-window
     (:height 360)(:width 350)(:top 5)(:left 650))
  (s-value Radio-Buttons-win
	   :aggregate
	   (create-instance 'Radio-Buttons-Top-Agg opal:aggregate))
  (create-instance 'Radio-Buttons-Obj Radio-Button-Panel
     (:left 30) (:top 20)
     (:selection-function #'Report-Selection)
     (:items `("Einstein" "Fermi" "Lorentz" "Maxwell" "Planck"
	       ,(create-instance NIL opal:line
		  (:x1 (o-formula (gvl :left)))
		  (:y1 (o-formula (gvl :top)))
		  (:x2 (o-formula (+ (gvl :x1) 40)))
		  (:y2 (o-formula (gvl :y1)))
		  (:width 40) (:height 1))
	       ,(create-instance NIL opal:rectangle
		  (:width 40) (:height 6)
		  (:filling-style opal:yellow-fill)))))
  (create-instance 'Radio-Button-Obj Radio-Button
     (:left 160) (:top 100)
     (:text-on-left-p T)
     (:toggle-p T)
     (:string (create-instance NIL opal:roundtangle
		(:width 40) (:height 20)
		(:filling-style opal:blue-fill)
		(:line-style opal:line-2)))
     (:selection-function #'(lambda (gadget value)
			      (format t "Button ~S " gadget)
			      (if value
				  (format t "selected.~%")
				  (format t "de-selected.~%")))))
  (create-instance 'Radio-Button-Obj2 Radio-Button
     (:left 160) (:top 200)
     (:text-on-left-p NIL)
     (:toggle-p T)
     (:selection-function #'(lambda (gadget value)
			      (format t "Button ~S " gadget)
			      (if value
				  (format t "selected.~%")
				  (format t "de-selected.~%")))))
  (opal:add-components Radio-Buttons-Top-Agg
		       Radio-Buttons-Obj Radio-Button-Obj
		       Radio-Button-Obj2)
  (format t "Leftdown on a radio button causes an inverse circle to be~%")
  (format t "superimposed on the button, executes the function locally~%")
  (format t "assigned to the button (if there is one), and executes the~%")
  (format t "function specified in :selection-function (if there is one).~%")
  (opal:update Radio-Buttons-Win))

#+garnet-test 
(defun Radio-Buttons-Stop ()
  (opal:destroy Radio-Buttons-Win))
