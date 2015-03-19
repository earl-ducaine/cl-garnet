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
;;;  Motif-Text-buttons
;;;
;;;  Features and operation of text-buttons:
;;;     1)  Click the left mouse button in a text button to select it.
;;;     2)  Pressing the space-bar selects the currently outlined button,
;;;         and Tab toggles between gadgets in the demo.
;;;     3)  The top level :value slot points to the string of the currently
;;;         selected button.  This slot can be set with S-VALUE.
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
;;;     3)  Text-offset -- the distance from the edge of the longest text to
;;;                        the frame of the button
;;;     4)  Keyboard-selection-p -- Whether the keyboard interactor should
;;;            operate on the button (or button panel)
;;;     5)  Font -- the font in which the text will appear
;;;     6)  String (in single buttons)
;;;     7)  Active-p (in single buttons) -- Whether the single button can be
;;;                                         selected.
;;;     8)  Items (in button panels) -- This can be: 
;;;                  A list of strings, as in '("Large" ...), or
;;;                  a list of atoms, as in '(:center ...), or
;;;                  a list of string/function pairs, '(("Cut" Cut-FN) ...), or
;;;                  a list of atom/function pairs, '((:center Center-FN) ...).
;;;                  Each function will be executed when the associated button
;;;                  becomes selected.  The parameters are the top-level
;;;                  GADGET and the ITEM-STRING.
;;;     9)  Inactive-Items (in button panels) -- A list of strings designating
;;;                  items that are not selectable.
;;;    10)  Final-feedback-p -- whether to leave the currently selected button
;;;                             depressed.
;;;    11)  Foreground-Color
;;;    12)  Selection-function -- Global function to be executed when any
;;;                               button is selected.  Parameters are the
;;;                               top-level GADGET and the ITEM-STRING.
;;;
;;;  NOTE:  This module requires several schemata defined in motif-parts.
;;;         Thus, motif-parts.fasl must be loaded before this module.
;;;
;;;  Motif-Text-Buttons Demo:
;;;     The function below creates window containing a motif-text-button and
;;;     a motif-text-button-panel.
;;;     To run it, enter (GARNET-GADGETS:motif-text-buttons-go).
;;;     To stop, enter (GARNET-GADGETS:motif-text-buttons-stop).
;;;
;;;  Written by Andrew Mickish


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Text-Button Motif-Text-Button-Panel))
  #+garnet-test
  (export '(Motif-Text-Buttons-Go Motif-Text-Buttons-Stop
	    Demo-Motif-Text-Button Demo-Motif-Text-Button-Panel
	    Motif-Text-Buttons-Top-Agg Motif-Text-Buttons-Win)))

(create-instance 'MOTIF-TEXT-BUTTON-TEXT-LABEL-PROTOTYPE opal:text
  (:constant '(:actual-heightp))
  (:left (o-formula (opal:gv-center-x-is-center-of (gvl :parent :button))))
  (:top (o-formula (opal:gv-center-y-is-center-of (gvl :parent :button))))
  (:string (o-formula (gv (kr-path 0 :parent) :string)))
  (:font (o-formula (gv (kr-path 0 :parent) :font)))
  (:line-style (o-formula
		(let ((p (kr-path 0 :parent)))
		  (if (gv p :active-p)
		      opal:default-line-style
		      (gv p :stippled-line-style)))))
  (:fast-redraw-p :rectangle)
  (:fast-redraw-filling-style
   (o-formula (let ((p (kr-path 0 :parent)))
		(if (gv p :depressed-p)
		    (gv p :background-fill)
		    (gv p :foreground-fill))))))


(create-instance 'MOTIF-TEXT-BUTTON MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :text-offset :active-p :string :font
			 :final-feedback-p :toggle-p :keyboard-selection-p
			 :foreground-color :value :selection-function :visible)
	    (:type (integer :text-offset)
		   (kr-boolean :active-p :final-feedback-p :toggle-p
		    :keyboard-selection-p)
		   ((or string keyword (satisfies schema-p)) :string)
		   ((or null string keyword (satisfies schema-p)) :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :text-offset :active-p :string
			     :toggle-p :font :final-feedback-p
			     :foreground-color :visible))
   (:left 0)(:top 0)
   (:text-offset 5)
   (:active-p T)
   (:string "Motif Text Button")
   (:toggle-p T)
   (:font opal:default-font)
   (:final-feedback-p NIL)
   (:keyboard-selection-p NIL)
   (:foreground-color opal:MOTIF-GRAY)
   (:selection-function NIL)

   (:text-width (o-formula (gvl :text :width)))
   (:width (o-formula (+ 4 (gvl :text-width) (* 2 (gvl :text-offset)))))
   (:height (o-formula (+ 4 (gvl :text :height) (* 2 (gvl :text-offset)))))

   (:interim-selected NIL)
   (:depressed-p (o-formula (or (gvl :interim-selected)
				(if (gvl :final-feedback-p) (gvl :selected)))))
   (:value (o-formula (if (gvl :selected) (gvl :string))))
   (:selected (o-formula (gvl :value)))
   (:text-label-prototype MOTIF-TEXT-BUTTON-TEXT-LABEL-PROTOTYPE)
   (:parts
    `((:BUTTON ,MOTIF-BOX
	       (:left ,(o-formula
			(let ((p (kr-path 0 :parent)))
			  (+ 2 (gv p :left)))))
	       (:top ,(o-formula
			(let ((p (kr-path 0 :parent)))
			  (+ 2 (gv p :top)))))
	       (:width ,(o-formula
			(let ((p (kr-path 0 :parent)))
			  (- (gv p :width) 4))))
	       (:height ,(o-formula
			  (let ((p (kr-path 0 :parent)))
			    (- (gv p :height) 4))))
	       (:depressed-p ,(o-formula (gv (kr-path 0 :parent) :depressed-p))))
      (:TEXT ,#'opal::Single-Button-Get-Label)
      (:SEL-BOX ,MOTIF-SELECTION-BOX
                (:obj-over ,(o-formula (gvl :parent))))))
   (:interactors
    `((:press ,MOTIF-SINGLE-PRESS)
      (:key ,MOTIF-SINGLE-KEY))))


(create-instance 'MOTIF-TEXT-BUTTON-PANEL MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :text-offset :final-feedback-p :items
			 :inactive-items :toggle-p :keyboard-selection-p
			 :keyboard-selection :font
			 :foreground-color :value :active-p
			 :direction :v-spacing :h-spacing :fixed-width-p
			 :fixed-height-p :fixed-width-size :fixed-height-size
			 :h-align :rank-margin :pixel-margin :indent
			 :selection-function :visible)
	    (:type (integer :text-offset :v-spacing :h-spacing :indent)
		   ((or null (integer 0)) :fixed-width-size :fixed-height-size
		    :rank-margin :pixel-margin)
		   (kr-boolean :final-feedback-p :toggle-p :active-p
		    :keyboard-selection-p :fixed-width-p :fixed-height-p)
		   (items-type :items :inactive-items)
		   ((or null string keyword (satisfies schema-p))
		    :keyboard-selection :value)
		   ((or null (satisfies schema-p)) :keyboard-selection-obj
		    :value-obj)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((is-a-p opal:color) :foreground-color)
		   ((member :vertical :horizontal) :direction)
		   ((member :left :center :right) :h-align)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :text-offset :final-feedback-p
			     :toggle-p :items :font :foreground-color
			     :direction :v-spacing :h-spacing :v-align :h-align
			     :indent :fixed-width-p :fixed-width-size
			     :fixed-height-p :fixed-height-size :rank-margin
			     :pixel-margin :active-p :inactive-items :visible))
   (:left 0) (:top 0)
   (:text-offset 5)
   (:final-feedback-p NIL)
   (:toggle-p NIL)

   (:items '("Text 1" "Text 2" "Text 3" "Text 4"))
   (:inactive-items NIL)
   (:keyboard-selection-p NIL)
   (:keyboard-selection (o-formula
			 (progn
			   ;; Set up dependency, so won't become constant
			   (gvl :keyboard-selection-p)
			   (first
			    (last (set-exclusive-or (gvl :item-objs)
						    (gvl :inactive-items)
						    :test #'equal))))))
   (:keyboard-selection-obj (o-formula
			     (let ((rank (position (gvl :keyboard-selection)
						   (gvl :item-objs)
						   :test #'equal)))
			       (nth rank (gvl :button-list :components)))))
   (:font opal:default-font)
   (:foreground-color opal:MOTIF-GRAY)
   (:selection-function NIL)
   
   (:direction :vertical)
   (:v-spacing 5) (:h-spacing 5)
   (:v-align :top) (:h-align :left)
   (:indent 0) (:rank-margin NIL) (:pixel-margin NIL)
   (:fixed-width-p T)
   (:fixed-width-size (o-formula (+ (gvl :button-list :tail
					 :max-text-width-thus-far)
				    (* 2 (gvl :text-offset)))))
   (:fixed-height-p NIL)
   (:fixed-height-size (o-formula (+ (gvl :button-list :tail
					  :max-text-height-thus-far)
				     (* 2 (gvl :text-offset)))))

   (:width (o-formula (gvl :button-list :width)))
   (:height (o-formula (gvl :button-list :height)))

   (:value-obj NIL)
   (:value (o-formula (let ((obj (gv-local :self :value-obj)))
			(if obj
			    (nth (g-value obj :rank) (gvl :item-objs))))))

   (:item-objs (o-formula (let ((items (gvl :items)))
			    (if (listp (first items))
				(mapcar #'car items)
				items))))
   (:strings (o-formula (mapcar #'(lambda (item-obj)
				    (if (stringp item-obj)
					item-obj
					(if (schema-p item-obj)
					    item-obj
					    ;; Must be an atom
					    (string-capitalize
					     (string-trim ":" item-obj)))))
				(gvl :item-objs))))
   (:actions (o-formula (let ((items (gvl :items)))
			  (when (listp (first items))
			    (mapcar #'cadr items)))))
   (:active-p (o-formula (> (length (gvl :items))
			    (length (gvl :inactive-items)))))
   (:text-label-prototype
    (create-instance NIL MOTIF-TEXT-BUTTON-TEXT-LABEL-PROTOTYPE
      (:left (o-formula
	      (let ((button (kr-path 0 :parent :button)))
		(case (gv (kr-path 1 :parent :parent :parent) :h-align)
		  (:left (+ (gv button :left)
			    (gv (kr-path 2 :parent) :text-offset)))
		  (:center (opal:gv-center-x-is-center-of button))
		  (:right (- (opal:gv-right button)
			     (gv (kr-path 2 :parent) :text-offset)
			     (gvl :width)))))))))
   (:parts
    `((:BUTTON-LIST ,opal:aggrelist
       (:constant (:fixed-width-p :fixed-height-p :fixed-width-size
		   :fixed-height-size :h-align :v-align))
       (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:direction ,(o-formula (gvl :parent :direction)))
       (:v-spacing ,(o-formula (gv (kr-path 0 :parent) :v-spacing)))
       (:h-spacing ,(o-formula (gv (kr-path 0 :parent) :h-spacing)))
       (:fixed-width-p NIL) (:fixed-height-p NIL)
       (:rank-margin ,(o-formula (gv (kr-path 0 :parent) :rank-margin)))
       (:pixel-margin ,(o-formula (gv (kr-path 0 :parent) :pixel-margin)))
       (:indent ,(o-formula (gv (kr-path 0 :parent) :indent)))
       (:items ,(o-formula (gv (kr-path 0 :parent) :items)))
       (:selected ,(o-formula
		    (let* ((p (kr-path 0 :parent))
			   (value (gv p :value)))
		      (when value
			(let ((index (position value (gv p :item-objs)
					       :test #'equal)))
			  (when index
			    (nth index (gvl :components))))))))
       (:item-prototype
	(,MOTIF-TEXT-BUTTON
	 (:max-text-width-thus-far
	  ,(o-formula (if (gvl :prev)
			  (opal::q-max (gvl :prev :max-text-width-thus-far)
				       (gvl :text :width))
			  (gvl :text :width))))
	 (:max-text-height-thus-far
	  ,(o-formula (if (gvl :prev)
			  (opal::q-max (gvl :prev :max-text-height-thus-far)
			       (gvl :text :height))
			  (gvl :text :height))))
	 (:foreground-color ,(o-formula (gv (kr-path 0 :parent :parent)
					    :foreground-color)))
	 ;; Instead of recomputing filling-styles for each button, just
	 ;; compute them once at the top level and "inherit"
	 (:foreground-fill ,(o-formula (gv (kr-path 0 :parent :parent)
					   :foreground-fill)))
	 (:background-fill ,(o-formula (gv (kr-path 0 :parent :parent)
					   :background-fill)))
	 (:shadow-fill ,(o-formula (gv (kr-path 0 :parent :parent)
				       :shadow-fill)))
	 (:highlight-fill ,(o-formula (gv (kr-path 0 :parent :parent)
					  :highlight-fill)))
	 (:final-feedback-p ,(o-formula (gv (kr-path 0 :parent :parent)
					    :final-feedback-p)))
	 (:selected ,(o-formula (eq (gv (kr-path 0 :parent) :selected)
				    (gv :self))))
	 (:active-p ,(o-formula
		      (and (not (member (gvl :item-obj)
					(gv (kr-path 0 :parent :parent)
					    :inactive-items)
					:test #'equal))
			   (gv (kr-path 0 :parent :parent) :active-p))))
	 (:text-offset ,(o-formula (gv (kr-path 0 :parent :parent) :text-offset)))
	 (:text-width ,(o-formula (let ((p (kr-path 0 :parent :parent)))
				    (if (gv p :fixed-width-p)
					(- (gv p :fixed-width-size)
					   (* 2 (gvl :text-offset)))
					(gvl :text :width)))))
	 (:height ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-height-p)
				  (gv (kr-path 0 :parent :parent)
				      :fixed-height-size)
				  (+ (gvl :text :height)
				     (* 2 (gvl :text-offset))))))

	 ;; Conditional formulas are required to allow either a list of
	 ;; strings or a list of string/function pairs in the :items slot.
	 (:item-obj ,(o-formula (nth (gvl :rank)
				     (gv (kr-path 0 :parent :parent) :item-objs))))
	 (:string ,(o-formula
		    (progn
		      ;; Invoke invalidate-demon on aggrelist's :items slot
		      ;; so that :fix-update-slots method will be called
		      (gv (kr-path 0 :parent) :items)
		      (nth (gvl :rank)
			   (gv (kr-path 1 :parent :parent) :strings)))))
	 (:action ,(o-formula (nth (gvl :rank)
				   (gv (kr-path 0 :parent :parent ) :actions))))
      
	 (:font ,(o-formula (gv (kr-path 0 :parent :parent) :font)))
	 (:parts
	  (:button
	   (:text :modify ,#'opal::Panel-Get-Label)
	   (:sel-box :omit)))
	 (:interactors
	  ((:press :omit) (:key :omit))))))
      (:SEL-BOX ,MOTIF-SELECTION-BOX)))

   (:interactors
    `((:PRESS ,inter:button-interactor
        (:start-where ,(o-formula (list :custom 
				   (gv-local :self :operates-on :button-list)
				   #'Motif-Element-Of-Not-Illegal)))
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:how-set ,(o-formula (if (gvl :operates-on :toggle-p) :toggle :set)))
	(:final-function
	 ,#'(lambda (interactor final-obj-over)
	      (let* ((action (g-value final-obj-over :action))
		     (gadget (g-value interactor :operates-on))
		     (selected (g-value final-obj-over :selected))
		     (value (when selected
			      (g-value final-obj-over :item-obj))))

		;; Propagate new selection toward :value slot
		(s-value gadget :value-obj (when selected final-obj-over))

		;; Global function for all items
		(kr-send gadget :selection-function gadget value)

		;; Local function assigned to item
		(when action
		  (funcall action gadget value))))))

      (:KEY ,inter:button-interactor
       (:active ,(o-formula (and (gvl :window)
				 (gvl :operates-on :keyboard-selection-p))))
       (:window ,(o-formula (gv-local :self :operates-on :window)))
       (:continuous NIL)
       (:start-where T)
       (:start-event (list #\space
			   :uparrow :rightarrow :downarrow :leftarrow))
       (:final-function
	,#'(lambda (interactor obj)
	     (declare (ignore obj))
	     (let* ((char (inter:event-char inter:*Current-Event*))
		    (gadget (g-value interactor :operates-on))
		    (selection (g-value gadget :keyboard-selection-obj))
		    (prev-sel (g-value gadget :value-obj)))
	       (case char
		 (#\space
		  (let ((item-obj (g-value gadget :keyboard-selection))
			(action (g-value selection :action))
			(selected (if (g-value gadget :toggle-p)
				      (not (g-value selection :selected))
				      T)))
		    ;; Make interim feedback flash
		    (when prev-sel (s-value prev-sel :selected NIL))
		    (s-value selection :selected selected)
		    (unless (g-value gadget :final-feedback-p)
		      (s-value selection :interim-selected T)
		      (opal:update (g-value gadget :window))
		      (sleep .25)
		      (s-value selection :interim-selected NIL))

		    ;; Propagate new selection toward :value slot
		    (s-value gadget :value-obj (when selected selection))
		    
		    ;; Global function for all items
		    (kr-send gadget :selection-function
			     gadget (when selected item-obj))
		    
		    ;; Local function assigned to item
		    (when action
		      (funcall action gadget (when selected item-obj)))))
		 #|
		 (:leftdown
		  (let ((obj (motif-element-of-not-illegal
			      (g-value gadget :button-list)
			      interactor inter:*Current-Event*)))
		    (if (and obj (g-value obj :active-p))
			(s-value gadget
				 :keyboard-selection
				 (g-value obj :item-obj)))))
		 |#
		 ((:downarrow :rightarrow)
		  (let* ((prev-rank (g-value selection :rank))
			 (button-list (g-value gadget :button-list))
			 (buttons (g-value button-list :components))
			 (max-rank (g-value button-list :tail :rank)))
		    (do* ((rank (if (= prev-rank max-rank)
				    0 (+ 1 prev-rank))
				(if (= rank max-rank)
				    0 (+ 1 rank)))
			  (button (nth rank buttons) (nth rank buttons)))
			 ((g-value button :active-p)
			  (s-value gadget
				   :keyboard-selection
				   (g-value button :item-obj))))))

		 ((:uparrow :leftarrow)
		  (let* ((prev-rank (g-value selection :rank))
			 (button-list (g-value gadget :button-list))
			 (buttons (g-value button-list :components))
			 (max-rank (g-value button-list :tail :rank)))
		    (do* ((rank (if (> prev-rank 0)
				    (- prev-rank 1) max-rank)
				(if (> rank 0)
				    (- rank 1) max-rank))
			  (button (nth rank buttons) (nth rank buttons)))
			 ((g-value button :active-p)
			  (s-value gadget
				   :keyboard-selection
				   (g-value button :item-obj))))))))))))))


(s-value MOTIF-TEXT-BUTTON-PANEL :add-local-item
	 #'opal::Motif-Buttons-Add-Local-Item)
(s-value MOTIF-TEXT-BUTTON-PANEL :add-item
	 #'opal::Motif-Buttons-Add-Item)
(s-value MOTIF-TEXT-BUTTON-PANEL :remove-local-item
	 #'opal::Motif-Buttons-Remove-Local-Item)
(s-value MOTIF-TEXT-BUTTON-PANEL :remove-item
	 #'opal::Motif-Buttons-Remove-Item)
(s-value MOTIF-TEXT-BUTTON-PANEL :change-item
	 (g-value opal:aggrelist :change-item))
(s-value MOTIF-TEXT-BUTTON-PANEL :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))
(s-value MOTIF-TEXT-BUTTON-PANEL :string-set-func
	 #'opal::Motif-Button-String-Func)


;;;
;;;  DEMO FUNCTION
;;;

#+garnet-test 
(defun MOTIF-TEXT-BUTTONS-GO (&key dont-enter-main-event-loop
				   not-double-buffered-p)
  (create-instance 'MOTIF-TEXT-BUTTONS-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Text Buttons")
     (:left 650)(:top 10)(:width 200)(:height 200))
  (s-value MOTIF-TEXT-BUTTONS-WIN
	   :aggregate
	   (create-instance 'MOTIF-TEXT-BUTTONS-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-TEXT-BUTTON MOTIF-TEXT-BUTTON
     (:left 30) (:top 10))
  (create-instance 'DEMO-MOTIF-TEXT-BUTTON2 MOTIF-TEXT-BUTTON
     (:left 130) (:top 80)
     (:string (create-instance NIL opal:circle
		(:filling-style opal:gray-fill))))
  (create-instance 'DEMO-MOTIF-TEXT-BUTTON-PANEL MOTIF-TEXT-BUTTON-PANEL
     (:left 30) (:top 50))
  (opal:add-components MOTIF-TEXT-BUTTONS-TOP-AGG
		       (create-instance NIL MOTIF-BACKGROUND)
		       DEMO-MOTIF-TEXT-BUTTON DEMO-MOTIF-TEXT-BUTTON2
		       DEMO-MOTIF-TEXT-BUTTON-PANEL)
  ;;; Now set up global keyboard interactor
  (s-value DEMO-MOTIF-TEXT-BUTTON-PANEL :keyboard-selection-p T)
  (create-instance 'DEMO-MOTIF-TEXT-BUTTON-INTER MOTIF-TAB-INTER
     (:window MOTIF-TEXT-BUTTONS-WIN)
     (:objects (list DEMO-MOTIF-TEXT-BUTTON-PANEL DEMO-MOTIF-TEXT-BUTTON
		     DEMO-MOTIF-TEXT-BUTTON2)))
  (opal:update MOTIF-TEXT-BUTTONS-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

#+garnet-test 
(defun MOTIF-TEXT-BUTTONS-STOP ()
  (opal:destroy MOTIF-TEXT-BUTTONS-WIN))
