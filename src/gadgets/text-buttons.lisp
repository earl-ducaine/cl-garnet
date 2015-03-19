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
;;;  Text-buttons
;;;
;;;  Features and operation of text-buttons:
;;;     1)  Click the left mouse button in a text button to select it.
;;;     2)  (Optional)  The text of the selected button will appear in inverse
;;;         video.
;;;     3)  The top level :value points to the string of the currently selected
;;;         button.
;;;     4)  The top level :value-obj slot points to the currently selected
;;;         button, and can be set directly with S-VALUE to select a button.
;;;     5)  The :items slot may contain functions to be executed as each
;;;         button is selected, and :selection-function may contain a
;;;         function to be executed when any button becomes selected.
;;;
;;;  Customizable slots:
;;;     1)  All customizable slots of an aggrelist:
;;;            Direction -- :vertical or :horizontal
;;;            V-spacing -- distance between buttons, if vertical orientation
;;;            H-spacing -- same, if horizontal orientation
;;;            Fixed-width-p -- whether all the buttons should be the width of
;;;                             :fixed-width-size (default is T)
;;;            Fixed-height-p -- same, but with heights
;;;            Fixed-width-size -- width of all components (default is the
;;;                                width of the widest button)
;;;            Fixed-height-size -- same, but with heights 
;;;            H-align -- how to align text within buttons horizontally
;;;                       :left, :center, or :right (default is :center)
;;;            V-align -- how to align text within buttons, vertically
;;;                       :top, :bottom, or :center (default is :center)
;;;            Rank-margin -- after this many components, a new row (or column)
;;;                           will be started
;;;            Pixel-margin -- absolute position in pixels after which a new
;;;                            row (or column) will be started
;;;            Indent -- amount to indent the new row (or column) in pixels
;;;     2)  Left, top
;;;     3)  Shadow-offset -- the amount of shadow that shows under the buttons
;;;     4)  Text-offset -- the distance from the edge of the longest text to
;;;                        the frame of the button
;;;     5)  Gray-width -- the width of the gray border on the buttons
;;;     6)  Font -- the font in which the text will appear
;;;     7)  Items -- This can be: 
;;;                  A list of strings, as in '("Large" ...), or
;;;                  a list of atoms, as in '(:center ...), or
;;;                  a list of string/function pairs, '(("Cut" Cut-FN) ...), or
;;;                  a list of atom/function pairs, '((:center Center-FN) ...).
;;;                  Each function will be executed when the associated button
;;;                  becomes selected.  The parameters are the top-level
;;;                  GADGET and the ITEM-STRING.
;;;     8)  Final-feedback-p -- whether to cause the text of the selected
;;;                             button to appear in inverse video
;;;     9)  Selection-function -- Global function to be executed when any
;;;                               button is selected.  Parameters are the
;;;                               top-level GADGET and the ITEM-STRING.
;;;
;;;  NOTE:  This module requires several schemata defined in GAD-button-parts.
;;;         Thus, GAD-button-parts.fasl must be loaded before this module.
;;;
;;;  Text-buttons demo:
;;;     This module contains a function which creates a window and a panel of
;;;     text-buttons.  To run it, enter (GARNET-GADGETS:text-buttons-go).
;;;     To stop, enter (GARNET-GADGETS:text-buttons-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Text-Button Text-Button-Panel))
  #+garnet-test
  (export '(Text-Buttons-Go Text-Buttons-Stop Text-Button-Obj
	    Text-Buttons-Obj Text-Buttons-Top-Agg Text-Buttons-Win
	    Text-Button-Obj1)))

;;;  IN-BUTTON-TEXT:  This text object is laid on top of the white rectangle
;;; 
(create-instance 'IN-BUTTON-TEXT opal:text
   (:constant '(:actual-heightp))
   (:left (o-formula (opal:gv-center-x-is-center-of
		      (gvl :parent :white-field))))
   (:top (o-formula (opal:gv-center-y-is-center-of
		     (gvl :parent :white-field))))
   (:string (o-formula (let ((s (gvl :parent :string)))
			 (if (stringp s)
			     s
			     (string-capitalize (string-trim ":" s))))))
   (:font (o-formula (gvl :parent :font))))


(create-instance 'TEXT-BUTTON opal:aggregadget
  :declare ((:parameters :left :top :shadow-offset :text-offset :gray-width
			 :string :toggle-p :font :final-feedback-p :value
			 :selection-function :visible)
	    (:type (fixnum :left :top :shadow-offset :text-offset :gray-width)
		   ((or string keyword (satisfies schema-p)) :string)
		   ((or null string keyword (satisfies schema-p)) :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   (kr-boolean :toggle-p :final-feedback-p)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :shadow-offset :text-offset :gray-width
			     :string :toggle-p :font :final-feedback-p
			     :visible))
   (:left 0) (:top 0)
   (:shadow-offset 10) (:text-offset 5) (:gray-width 5)
   (:string "Text Button")
   (:toggle-p T)
   (:font opal:default-font)
   (:final-feedback-p T)
   (:selection-function NIL)
   (:value (o-formula (if (gvl :selected) (gvl :string))))
   (:selected (o-formula (gvl :value)))
   (:floating-left (o-formula (+ (gvl-fixnum :button-left)
				 (if (gvl :interim-selected)
				     (gvl-fixnum :shadow-offset)
				     0))))
   (:floating-top (o-formula (+ (gvl-fixnum :button-top)
				(if (gvl :interim-selected)
				    (gvl-fixnum :shadow-offset)
				    0))))
   (:button-left (o-formula (gvl :left)))
   (:button-top (o-formula (gvl :top)))
   (:button-width (o-formula (+ (* 2 (gvl-fixnum :gray-width))
				(* 2 (gvl-fixnum :text-offset))
				(gvl-fixnum :text-width))))
   (:button-height (o-formula (+ (* 2 (gvl-fixnum :gray-width))
				 (* 2 (gvl-fixnum :text-offset))
				 (gvl-fixnum :text :height))))
   (:button-unit-width (o-formula (+ (gvl-fixnum :button-width)
				     (gvl-fixnum :shadow-offset))))
   (:button-unit-height (o-formula (+ (gvl-fixnum :button-height)
				      (gvl-fixnum :shadow-offset))))
   (:text-width (o-formula (gvl :text :width)))
   (:width (o-formula (gvl :button-unit-width)))
   (:height (o-formula (gvl :button-unit-height)))
   (:text-label-prototype IN-BUTTON-TEXT)
   (:parts
    `((:shadow ,BUTTON-SHADOW-RECT)
      (:gray-outline ,GRAY-RECT-OUTLINE)
      (:white-field ,WHITE-RECT-FIELD)
      (:text ,#'opal::Single-Button-Get-Label)
      (:feedback-obj ,opal:rectangle
          (:left ,(o-formula (gv (kr-path 0 :parent :white-field) :left)))
	  (:top ,(o-formula (gv (kr-path 0 :parent :white-field) :top)))
	  (:width ,(o-formula (gv (kr-path 0 :parent :white-field) :width)))
	  (:height ,(o-formula (gv (kr-path 0 :parent :white-field) :height)))
          (:filling-style ,opal:black-fill)
	  (:line-style NIL)
	  (:fast-redraw-p T) (:draw-function :xor)
	  (:visible ,(o-formula (let ((p (kr-path 0 :parent)))
				  (and (gv p :visible)
				       (gv p :final-feedback-p)
				       (gv p :selected))))))))
   (:interactors
    `((:text-button-press ,inter:button-interactor
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


;;;
;;;  TOP LEVEL AGGREGADGET
;;;

(create-instance 'TEXT-BUTTON-PANEL opal:aggregadget
  :declare ((:parameters :left :top :shadow-offset :text-offset :gray-width
			 :final-feedback-p :toggle-p :font :items :value
			 :direction :v-spacing :h-spacing :fixed-width-p
			 :fixed-height-p :fixed-width-size :fixed-height-size
			 :h-align :rank-margin :pixel-margin :indent
			 :selection-function :visible)
	    (:type (integer :v-spacing :h-spacing :indent :shadow-offset
			    :text-offset :gray-width)
		   ((or null (integer 0)) :fixed-width-size :fixed-height-size
		    :rank-margin :pixel-margin)
		   (kr-boolean :final-feedback-p :toggle-p
		    :fixed-width-p :fixed-height-p)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   (items-type :items)
		   ((or null (satisfies schema-p)) :value-obj)
		   ((or null string keyword (satisfies schema-p)) :value)
		   ((member :vertical :horizontal) :direction)
		   ((member :left :center :right) :h-align)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :direction :v-spacing :h-spacing
			     :h-align :fixed-width-p :fixed-width-size
			     :fixed-height-p :fixed-height-size :indent
			     :rank-margin :pixel-margin
			     :shadow-offset :text-offset :gray-width
			     :final-feedback-p :toggle-p :font :items
			     :visible))
   
   ;; Customizable slots
   ;;
   (:left 0) (:top 0)
   (:direction :vertical)
   (:v-spacing 5) (:h-spacing 5)
   (:h-align :center)
   (:fixed-width-p T)
   (:fixed-width-size (o-formula (+ (* 2 (gvl-fixnum :gray-width))
				    (* 2 (gvl-fixnum :text-offset))
				    (gvl :text-button-list :tail
					 :max-text-width-thus-far))))
   (:fixed-height-p T)
   (:fixed-height-size (o-formula (+ (* 2 (gvl-fixnum :gray-width))
				     (* 2 (gvl-fixnum :text-offset))
				     (gvl :text-button-list :tail
					  :max-text-height-thus-far))))
   (:indent 0) (:rank-margin NIL) (:pixel-margin NIL)
   (:shadow-offset 10) (:text-offset 5) (:gray-width 5)
   (:final-feedback-p T)
   (:toggle-p NIL)
   (:font opal:default-font)
   (:selection-function NIL)
   (:items '("Text 1" "Text 2" "Text 3" "Text 4"))
   (:value-obj (o-formula (gvl :text-button-list :selected)))
   (:value (o-formula (let ((obj (gvl :value-obj)))
			(if obj (gv obj :string)))))
   (:actions-p (o-formula (listp (first (gvl :items)))))
   (:width (o-formula (gvl :text-button-list :width)))
   (:height (o-formula (gvl :text-button-list :height)))
   (:text-label-prototype
    (create-instance NIL IN-BUTTON-TEXT
      (:left (o-formula
	      (let ((white-field (gvl :parent :white-field)))
		(case (gvl :parent :parent :parent :h-align)
		  (:left (+ (gv-fixnum white-field :left)
			    (gvl-fixnum :parent :text-offset)))
		  (:center (opal:gv-center-x-is-center-of white-field))
		  (:right (- (the fixnum (opal:gv-right white-field))
			     (gvl-fixnum :parent :text-offset)
			     (gvl-fixnum :width)))))))))
   (:parts
    `((:text-button-list ,ITEMS-AGGLIST
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
	(,TEXT-BUTTON
	  (:selected ,(o-formula (equal (gvl :string)
					(gvl :parent :parent :value))))
	  (:shadow-offset ,(o-formula (gv (kr-path 0 :parent :parent)
					  :shadow-offset)))
	  (:text-offset ,(o-formula (gv (kr-path 0 :parent :parent) :text-offset)))
	  (:gray-width ,(o-formula (gv (kr-path 0 :parent :parent) :gray-width)))
	  (:max-text-width-thus-far
	   ,(o-formula (if (gvl :prev)
			   (opal:q-MAX (gvl :prev :max-text-width-thus-far)
				 (gvl-fixnum :text :width))
			   (gvl-fixnum :text :width))))
	  (:max-text-height-thus-far
	   ,(o-formula (if (gvl :prev)
			   (opal:q-MAX (gvl :prev :max-text-height-thus-far)
				 (gvl-fixnum :text :height))
			   (gvl-fixnum :text :height))))
	  (:button-width ,(o-formula (let ((p (kr-path 0 :parent :parent)))
				       (if (gv p :fixed-width-p)
					   (gv-fixnum p :fixed-width-size)
					   (+ (the fixnum (* 2 (gvl :gray-width)))
					      (* 2 (gvl-fixnum :text-offset))
					      (gvl-fixnum :text :width))))))
	  (:button-height ,(o-formula (let ((p (kr-path 0 :parent :parent)))
					(if (gv p :fixed-height-p)
					    (gv-fixnum p :fixed-height-size)
					    (+ (* 2 (gvl-fixnum :gray-width))
					       (* 2 (gvl-fixnum :text-offset))
					       (gvl-fixnum :text :height))))))
	  (:button-unit-width ,(o-formula (+ (gvl-fixnum :button-width)
					     (gvl-fixnum :shadow-offset))))
	  (:button-unit-height ,(o-formula (+ (gvl-fixnum :button-height)
					      (gvl-fixnum :shadow-offset))))

	  ;; Conditional formulas are required to allow either a list of
	  ;; strings or a list of string/function pairs in the :items slot.
	  (:string ,(o-formula (if (gv (kr-path 0 :parent :parent) :actions-p)
				   (first (nth (gvl :rank)
					       (gv (kr-path 1 :parent) :items)))
				   (nth (gvl :rank)
					(gv (kr-path 1 :parent) :items)))))
	  (:action ,(o-formula (when (gv (kr-path 0 :parent :parent) :actions-p)
				 (second (nth (gvl :rank)
					      (gv (kr-path 1 :parent) :items))))))
	  (:font ,(o-formula (gv (kr-path 0 :parent :parent) :font)))
	  (:parts
	   (:shadow :gray-outline :white-field
	    (:text :modify ,#'opal::Panel-Get-Label)
	    (:feedback-obj :omit)))
	  (:interactors
	   ((:text-button-press :omit))))))

      (:FINAL-FEEDBACK ,opal:rectangle
          (:obj-over ,(o-formula (gvl :parent :text-button-list :selected)))
	  (:left ,(o-formula (gvl :obj-over :white-field :left)))
	  (:top ,(o-formula (gvl :obj-over :white-field :top)))
	  (:width ,(o-formula (gvl :obj-over :white-field :width)))
	  (:height ,(o-formula (gvl :obj-over :white-field :height)))
	  (:visible ,(o-formula (let ((p (kr-path 0 :parent))
				      (obj-over (gvl :obj-over)))
				  (and (gv p :visible)
				       (gv p :final-feedback-p)
				       obj-over
				       ;; The obj-over will not have a parent
				       ;; if it is being kept in storage
				       ;; (e.g., after a remove-item call)
				       (gv obj-over :parent)))))
	  (:fast-redraw-p T)
          (:draw-function :xor)
	  (:line-style NIL)
	  (:filling-style ,opal:black-fill))))

   (:interactors
    `((:TEXT-BUTTON-PRESS ,inter:menu-interactor 
	(:start-where ,(o-formula (list :element-of
					(gvl :operates-on :text-button-list))))
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


(define-method :add-local-item TEXT-BUTTON-PANEL (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item :text-button-list args))
(define-method :add-item TEXT-BUTTON-PANEL (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item :text-button-list args))
   
(define-method :remove-local-item TEXT-BUTTON-PANEL
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item :text-button-list key))
(define-method :remove-item TEXT-BUTTON-PANEL
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item :text-button-list key))

(s-value TEXT-BUTTON-PANEL :change-item
	 (g-value opal:aggrelist :change-item))
(s-value TEXT-BUTTON-PANEL :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(define-method :string-set-func TEXT-BUTTON-PANEL
    (gadget-obj str-obj final-event final-string)
  (let ((aggrel (g-value gadget-obj :TEXT-BUTTON-LIST)))
    (opal::Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				      final-event final-string :rank)))


;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test (defparameter Text-Buttons-win NIL)
#+garnet-test (defparameter Text-Buttons-top-agg NIL)
#+garnet-test (defparameter Text-Buttons-Obj NIL)

#+garnet-test
(defun Text-Buttons-Go ()
  (create-instance 'text-buttons-win inter:interactor-window
     (:height 380)(:width 360)(:top 5)(:left 650))
  (s-value Text-Buttons-win
	   :aggregate
	   (create-instance 'text-buttons-top-agg opal:aggregate))
  (opal:update Text-Buttons-Win)
  (create-instance 'text-buttons-obj Text-Button-Panel
     (:left 30) (:top 20)
     (:selection-function #'Report-Selection)
     (:items `("Mozart" "Bach" "Beethoven" "Ravel" "Strauss"
	       ,(create-instance NIL opal:oval
		  (:width 30) (:height 15)
		  (:filling-style opal:yellow-fill))
	       ,(create-instance NIL opal:line
		  (:x1 (o-formula (gvl :left)))
		  (:x2 (o-formula (+ 30 (gvl :x1))))
		  (:y1 (o-formula (+ 4 (gvl :top))))  ;offset by 1/2 thickness
		  (:y2 (o-formula (gvl :y1)))
		  (:height 8) (:width 30)
		  (:line-style (create-instance NIL opal:line-style
				 (:line-thickness 8)
				 (:stipple opal::gray-fill-bitmap)))))))
		  
  (create-instance 'text-button-obj Text-Button
     (:left 150) (:top 20)
     (:selection-function #'(lambda (gadget value)
			      (format t "Button ~S " gadget)
			      (if value
				  (format t "selected.~%")
				  (format t "de-selected.~%")))))
  (create-instance 'text-button-obj2 Text-Button
     (:left 150) (:top 100)
     (:string (create-instance NIL opal:roundtangle
		(:width 40) (:height 30)
		(:line-style opal:line-2)
		(:filling-style opal:light-gray-fill)))
     (:selection-function #'(lambda (gadget value)
			      (format t "Button ~S " gadget)
			      (if value
				  (format t "selected.~%")
				  (format t "de-selected.~%")))))
  (opal:add-components text-buttons-top-agg
		       text-buttons-obj
		       text-button-obj
		       text-button-obj2)
  (format t "Leftdown on a text-button causes an inverse box to be~%")
  (format t "superimposed on the button, executes the function locally~%")
  (format t "assigned to the button (if there is one), and executes the~%")
  (format t "function specified in :selection-function (if there is one).~%")
  (opal:update Text-Buttons-win))

#+garnet-test 
(defun Text-Buttons-Stop ()
  (opal:destroy Text-Buttons-win))
