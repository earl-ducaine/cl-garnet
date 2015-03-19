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
;;;  Option button
;;;
;;;  Designed and written by Rajan Parthasarathy

;;;  CHANGE LOG:
;;;  12/06/94  Andrew Mickish - Added :modal-p formula;  changed :stop-action
;;;              for Robert Goldman so that it sets the :value slot first
;;;  03/22/94  Andrew Mickish - Fixed menu window positioning in
;;;              Set-Menu-Top-And-Left when option-button is in a subwindow
;;;  09/10/93  Rajan Parthasarathy - Modified Warp-To-Correct-Place and
;;;              Set-Menu-Top-And-Left to be more accurate and efficient.
;;;  07/02/93  Andrew Mickish - Removed :initial-item from :constant list
;;;  04/23/93  Andrew Mickish - Moved action functions from :initialize
;;;              method to Put-Menu-In-Window; Added :do-not-dump-slots
;;;  03/17/93  Brad Myers - add special-popdown functions for gilt, etc.
;;;  03/10/93  Andrew Mickish - Fixed bounding box
;;;  02/23/93  Andrew Mickish - Added :string-set-func
;;;  02/19/93  Andrew Mickish - Added Add/Remove-Item methods
;;;  02/10/93  Andrew Mickish - Made :items of type items-type
;;;  01/25/93  Szekely - Added :width and :height formulas to MOTIF-TEXT-BUTTON
;;;  12/14/92  Mickish - Added type and parameter declarations
;;;  12/10/92  Mickish - *drawable-to-window-mapping* ---> *garnet-windows*
;;;  09/28/92  Mickish - Formula --> o-formula in Put-Menu-In-Window
;;;  07/27/92  Rajan - Created

(in-package "GARNET-GADGETS")
(defparameter motif-ob NIL)
(defparameter demo-ob NIL)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Option-Button #+garnet-test Option-Button-Go
	    #+garnet-test Option-Button-Stop)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First functions display the submenu from gilt, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Put-Down-Garnet-Option-Popup (gadget)
  (when (schema-p gadget)
    (let ((g (g-value gadget :option-text-button))
	  (win (g-value gadget :option-button-menu :window))
	  (gadget-vis (g-value gadget :visible)))
      (when (schema-p g)
	(s-value g :visible gadget-vis))
      (when (schema-p win)
	(s-value win :visible NIL)
	(opal:update win))))) ;; doesn't seem to be seeing the s-value 


;; Returns the window if successful
(defun Garnet-Option-popup-item (gadget)
  (let ((g (g-value gadget :option-text-button))
	(win (g-value gadget :option-button-menu :window)))
    (when (opal:point-in-gob g (inter:event-x inter:*current-event*)
			     (inter:event-y inter:*current-event*))
      (Set-Menu-Top-And-Left g (g-value g :string))
      (opal:update win)
      (s-value g :visible NIL)
      (s-value win :visible T)
      (opal:raise-window win)
      win))) ;; return win if successful

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is responsible for saying if the menu is out
;; of the screen or not.  If the menu is out of screen at
;; the top, it returns 'TOP.  If menu's bottom is out of
;; screen, it returns 'BOTTOM.  Otherwise, it returns NIL.
;; NOTE: It also returns NIL for the case when BOTH the
;; top and bottom are out of screen.  In this situation,
;; the user is SCREEEEEEEWEEEED... (-:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Menu-Out-Of-Screen (button)
  (let ((menu (g-value button :parent :option-button-menu))
	(return-value NIL))
    (cond
      ((NULL (g-value menu :window :top)) NIL)
      ((AND (< (g-value menu :window :top) 0)
	    (> (+ (g-value menu :window :top)
		  (g-value menu :window :height))
	       gem:*screen-height*)) (setf return-value NIL))
      ((< (g-value menu :window :top) 0) (setf return-value 'top))
      ((> (+ (g-value menu :window :top)
	     (g-value menu :window :height))
	  gem:*screen-height*) (setf return-value 'bottom)))

    return-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is responsible for calculating and setting the
;; left and top of the menu's window.  It takes in the
;; button and its value.  The left of the window is
;; the left of the object window + the left of the button.
;;
;; The top is more complicated.  It initially sets the top
;; so that the item in the button is positioned where the
;; cursor is now.  Then, it checks to see if the menu is
;; out of the screen, by calling menu-out-of-screen.
;;
;; If the top of the menu is out of screen, it positions
;; the top at 0.  If the bottom is out, it positions the
;; bottom as far down as possible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Set-Menu-Top-And-Left (g v)
  (let* ((menu (g-value g :parent :option-button-menu))
	 (pos (or (position v (g-value g :parent :real-items) 
			    :test #'equalp)
		  0))
	 (top-win (g-value g :parent :window)))

    ;; Convert top-win's left and top to screen coordinates, since top-win
    ;; might be a subwindow.
    (multiple-value-bind (win-left win-top)
	(opal:convert-coordinates top-win 0 0)
      (s-value menu :win-left (- (+ win-left (g-value g :left))
				 (g-value top-win :left-border-width)))
      (s-value menu :win-top  (- (+ win-top
				    (- (g-value g :top)
				       (g-value (nth pos (g-value menu :menu-item-list :components)) :top)))
				 (g-value top-win :top-border-width))))

    (opal:update (g-value menu :window))

    (when (g-value g :parent :keep-menu-in-screen-p)
      (if (eq (menu-out-of-screen g) 'top)
	  (s-value menu :win-top
		   (- (g-value g :parent :window :top-border-width)))
	  (if (eq (menu-out-of-screen g) 'bottom)
	      (s-value menu :win-top
		       (- gem:*screen-height*
			  (g-value menu :window :height)
			  (g-value g :parent :window :top-border-width))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function puts the cursor in the correct place.
;; It first calculates left-offset and top-offset, which
;; is the distance between the cursor and the left of the
;; button, and the distance between the cursor and the top
;; of the button.  It positions the cursor in the correct
;; place in the menu window, wherever the menu window may
;; be.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun warp-to-correct-place (butt val)
  (let* ((menu (g-value butt :parent :option-button-menu))
	 (left-offset (- (inter:event-x inter:*current-event*)
			 (g-value butt :left)))
	 (top-offset (- (inter:event-y inter:*current-event*)
			(g-value butt :top)))
	 (pos (or (position val (g-value butt :parent :real-items)
			    :test #'equalp)
		  0))
	 (new-y (+
		 (g-value (nth pos (g-value menu :menu-item-list :components)) :top)
		 top-offset)))
    (inter:warp-pointer (g-value menu :window)
			left-offset
			new-y)
    (values left-offset new-y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function is called when the button is clicked on.
;; It positions the menu at the right place, and moves
;; the cursor to the correct place, and pops up the menu
;; and starts the interactor.
;;
;; HOW it gets the positions of the menu and the cursor
;; are discussed in the next couple of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Option-Text-Button-Selection-Function (g v)
  (declare (ignore v))
  (let ((menu (g-value g :parent :option-button-menu))
	(win (g-value g :parent :window))
	(ev-x NIL) (ev-y NIL)
	(ev inter:*current-event*))

    (Set-Menu-Top-And-Left g (g-value g :string))
    (opal:update (g-value menu :window))
    
    (setf (inter:event-window ev) (g-value menu :window))
    (multiple-value-setq (ev-x ev-y)
      (warp-to-correct-place g (g-value g :string)))

    (setf (inter:event-x ev) ev-x)
    (setf (inter:event-y ev) ev-y)
    
    (inter:start-interactor (g-value menu :selector) ev)
    (s-value g :visible NIL)
    (opal:update win)
    (s-value (g-value menu :window) :visible T)
    (opal:update (g-value menu :window))
    (opal:raise-window (g-value menu :window))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the gadget itself.  It is basically a button
;; that, when pressed, pops up a menu.  The customizable
;; slots are explained below:
;;
;; :top, :left -> Top and left of the gadget in the window
;; :text-offset -> The text offset of the button and menu
;; :label -> The string that appears before the button
;; :button-offset -> The distance between the button and :label
;; :button-shadow-offset -> The button's shadow offset.
;;                          Menu doesn't have shadow
;; :items -> The contents of the menu
;; :initial-item -> The initial item that appears in the
;;                  button (HAS TO BE NON-NIL)
;; :button-font -> The font for the string in the button
;; :label-font -> The font for the label
;; :selection-function -> When one of the items in the menu
;;                        is selected, this is called
;; :value -> Current value (just the string in the button)
;; :button-fixed-width-p -> Button width stays constant
;; :v-spacing -> v-spacing for button and menu
;; :keep-menu-in-screen-p -> Whether menu should stay inside
;;                           the screen or be allowed to go out
;; :menu-h-align -> aligning for menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(create-instance 'OPTION-BUTTON opal:aggregadget
  :declare ((:parameters :left :top :label :text-offset :button-offset
			 :button-shadow-offset :items :initial-item
			 :value :button-fixed-width-p :v-spacing :menu-h-align
			 :keep-menu-in-screen-p :button-font :label-font
			 :selection-function :visible)
	    (:type (string :label :initial-item :value)
		   (integer :text-offset :button-offset :button-shadow-offset
			    :v-spacing)
		   (items-type :items)
		   (kr-boolean :button-fixed-width-p :keep-menu-in-screen-p)
		   ((member :left :center :right) :menu-h-align)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :button-font :label-font)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :text-offset :label :button-offset
			     :button-shadow-offset :items
			     :button-font :label-font :button-fixed-width-p
			     :v-spacing :keep-menu-in-screen-p :menu-h-align))
;; Customizable slots

  (:top 40) (:left 40)
  (:text-offset 4)
  (:label "Option button:")
  (:button-offset 10)
  (:button-shadow-offset 5)
  (:items '("Item 1" "Item 2" "Item 3" "Item 4"))
  (:initial-item (o-formula (if (listp (first (gvl :items)))
				(first (first (gvl :items)))
				(first (gvl :items)))))
  (:button-font opal:default-font)
  (:label-font (opal:get-standard-font NIL :bold NIL))
  (:selection-function NIL)
  (:value (o-formula (gvl :option-text-button :string)))
  (:button-fixed-width-p T)
  (:v-spacing 0)
  (:keep-menu-in-screen-p T)
  (:menu-h-align :LEFT)
  
;; Non-customizable slots
  (:width (o-formula (+ (gvl :option-button-label :width)
			(gvl :option-text-button :width)
			(gvl :button-offset))))
  (:height (o-formula (max (gvl :option-button-label :height)
			   (gvl :option-text-button :height))))
  (:center-y (o-formula (+ (gvl :top) (round (gvl :height) 2))))
  (:real-items (o-formula (let ((l NIL))
			    (if (listp (first (gvl :items)))
				(progn
				  (dolist (i (gvl :items))
				    (setf l (cons (first i) l)))
				  (setf l (reverse l)))
				(gvl :items)))))

  (:special-popup-func 'Garnet-Option-popup-item)
  (:special-popdown-func 'Put-Down-Garnet-Option-Popup)
  (:update-slots (list :visible))

  (:parts
   `((:option-button-label ,opal:text
      (:left ,(o-formula (gvl :parent :left)))
      (:top ,(o-formula (- (gvl :parent :center-y) (floor (gvl :height) 2))))
      (:string ,(o-formula (gvl :parent :label)))
      (:font ,(o-formula (gvl :parent :label-font))))

     (:option-text-button ,gg:text-button
      (:maybe-constant
       :left :top :text-offset :active-p :toggle-p 
       :final-feedback-p :foreground-color :visible)
      (:left ,(o-formula (+ (gvl :parent :left)
			    (gvl :parent :option-button-label :width)
			    (gvl :parent :button-offset))))
      (:top ,(o-formula (- (gvl :parent :center-y) (floor (gvl :height) 2))))
      (:button-height ,(o-formula (+ (gvl :parent :option-button-menu
					  :menu-item-list :tail :height)
				     (gvl :parent :option-button-menu :v-spacing))))
      (:button-width ,(o-formula (if (gvl :parent :button-fixed-width-p)
				     (gvl :parent :option-button-menu
					  :menu-item-list :width)
				     (+ (* 2 (gvl :text-offset))
					(gvl :text-width)))))
      (:final-feedback-p NIL)
      (:text-offset ,(o-formula (gvl :parent :text-offset)))
      (:gray-width 0)
      (:shadow-offset ,(o-formula (gvl :parent :button-shadow-offset)))
      (:font ,(o-formula (gvl :parent :button-font)))
      (:string ,(o-formula (gvl :parent :initial-item)))
      (:selection-function ,#'Option-Text-Button-Selection-Function)
      ;; RGA modified this list because we don't want string to be constant.
      (:interactors
       ((:text-button-press :modify
			    (:continuous NIL))))))))

(define-method :fix-update-slots OPTION-BUTTON (ob)
  ;; will be called when :visible changes
  (unless (g-value ob :visible)
    (Put-Down-Garnet-Option-Popup ob))
  (call-prototype-method ob))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This function is called in the :initialize method.
;; It basically creates the menu, puts it in a window, and
;; puts the menu in a slot in the gadget called
;; :option-button-menu.  The hard part about this is trying
;; to understand the formulas.
;;
;; NOTE: For some wierd reason, the interactor crashes if
;; the last line is omitted.  I think this can be attributed
;; to the daemons being turned off during the initialize
;; method or something like that
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Put-Menu-In-Window (opt)     
  (let* ((menu
	  (create-instance NIL gg:menu
	    (:title NIL)
	    (:option-button-parent opt)
	    (:item-font (o-formula (gvl :option-button-parent :button-font)))
	    (:shadow-offset 0)
	    (:selection-function
	     #'(lambda (g v)
		 (when (g-value g :option-button-parent :selection-function)
		   (funcall
		    (g-value g :option-button-parent :selection-function)
		    (g-value g :option-button-parent) v))))
	    (:items (o-formula (gvl :option-button-parent :items)))
	    (:v-spacing (o-formula (gvl :option-button-parent :v-spacing)))
	    (:text-offset (o-formula (gvl :option-button-parent :text-offset)))
	    (:h-align (o-formula (gvl :option-button-parent :menu-h-align)))
	    (:win-left 0) (:win-top 0)
	    (:interactors
	     `((:selector :modify
		(:stop-event :leftup)
		(:window ,(o-formula
			   (let* ((the-opt (gvl :operates-on
						:option-button-parent))
				  (my-win (gv the-opt :window))
				  (button-win (gv the-opt :option-button-menu
						  :window)))
			     (if (and my-win button-win)
				 (list my-win button-win)
				 (or my-win button-win)))))
		(:stop-action
		 ,#'(lambda (int val)
		      (let* ((gad (g-value int :operates-on))
			     (parent (g-value gad :option-button-parent))
			     (button (g-value parent :option-text-button))
			     (win (g-value parent :window))
			     (menu-win (g-value gad :window))
			     (text (g-value val :text)))
			(s-value button :string (g-value text :string))
			(s-value button :font (g-value text :font))
			(s-value button :visible T)
			(opal:update win)
			(s-value menu-win :visible NIL)
			(opal:update menu-win))
		      (call-prototype-method int val)))
		(:running-action
		 ,#'(lambda (int cont prev)
		      (let* ((gad (g-value int :operates-on))
			     (parent (g-value gad :option-button-parent))
			     (button (g-value parent :option-text-button))
			     (win (g-value parent :window))
			     (menu-win (g-value gad :window)))
			(when (AND (NULL prev)    ;; outside
				   ;; if code, then not window exit or enter.
				   (inter:event-code inter:*current-event*)
				   (NOT (inter:event-downp inter:*current-event*)))
			  (s-value button :visible T)
			  (opal:update win)
			  (s-value menu-win :visible NIL)
			  (opal:update menu-win)))
		      (call-prototype-method int cont prev)))

		)))))
	 (win (create-instance NIL inter:interactor-window
		(:menu (o-formula (first (gvl :aggregate :components))))
		(:omit-title-bar-p T)
		(:double-buffered-p T)
		(:save-under T)
		(:border-width 0)
		(:left (o-formula
			(if (gvl :menu :option-button-parent :window)
			    (+ (gvl :menu :win-left)
			       (gvl :menu :option-button-parent :window
				   :left-border-width))
			    0)))
		(:top (o-formula
		       (if (gvl :menu :option-button-parent :window)
			   (+ (gvl :menu :win-top)
			      (gvl :menu :option-button-parent :window
				  :top-border-width))
			   0)))
		(:height (o-formula (gvl :menu :height)))
		(:width (o-formula (gvl :menu :width)))
		(:visible NIL)
		(:button opt)
		(:modal-p (o-formula
			   (gv-local :self :button :window :modal-p)))
		)))
    (s-value win :aggregate (create-instance NIL opal:aggregate))
    (s-value menu :window win)
    (opal:add-component (g-value win :aggregate) menu)
    (s-value opt :option-button-menu menu)
    (g-value opt :option-button-menu :selector :window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the initialize method for the gadget.
;; It calls the Put-Menu-In-Window function.  It also
;; sets the initial item of the menu to be the item
;; in :initial-item slot.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method :initialize OPTION-BUTTON (opt)
   (call-prototype-method opt)
   (Put-Menu-In-Window opt)
   (let ((menu (g-value opt :option-button-menu)))
     (g-value menu :value)
     (s-value menu :value (g-value opt :initial-item))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the destroy method for the option button.
;; It first destroys the window in which the menu is
;; sitting, THEN it destroys the gadget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method :destroy-me Option-Button (ob &optional erase)
  (let ((window (g-value ob :option-button-menu :window)))
    (Put-Down-Garnet-Option-Popup ob)
    (if (and window
	     (schema-p window)
; Why bother checking that the option-button's menu window has a drawable?
; We want to destroy the window object no matter what.
;	     (getf (xlib:drawable-plist (get-local-value window :drawable))
;		   :garnet)
	     )
	(opal:destroy window))
    (call-prototype-method ob erase)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add/Remove-Item methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method :add-local-item OPTION-BUTTON (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item
			       '(:option-button-menu :menu-item-list) args))
(define-method :add-item OPTION-BUTTON (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item 
			 '(:option-button-menu :menu-item-list) args))
   
(define-method :remove-local-item OPTION-BUTTON
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item 
				  '(:option-button-menu :menu-item-list) key))
(define-method :remove-item OPTION-BUTTON
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item 
			    '(:option-button-menu :menu-item-list) key))

(s-value OPTION-BUTTON :change-item
	 (g-value opal:aggrelist :change-item))
(s-value OPTION-BUTTON :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(s-value OPTION-BUTTON :string-set-func #'opal::Option-Button-String-Func)

(s-value OPTION-BUTTON :do-not-dump-slots
	 (append '(:option-button-menu)
		 (g-value OPTION-BUTTON :do-not-dump-slots)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is like the mini-demo function for this gadget.
;; It basically creates a window with an option button
;; and an opal:text box, that is the selected object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+garnet-test
(defun Option-Button-Go ()
  (create-instance 'demo-option-win inter:interactor-window
    (:title "Option Demo")
    (:left 800) (:top 100) (:height 90) (:width 180))
  (create-instance 'demo-option-agg opal:aggregate)
  (s-value demo-option-win :aggregate demo-option-agg)
  (opal:update demo-option-win T)

  (create-instance 'demo-ob-text-label opal:text
    (:top 50) (:left 15)
    (:string "Selected: "))

  (create-instance 'demo-ob-text opal:text
    (:top 50)
    (:left (o-formula (+ (g-value demo-ob-text-label :left)
			 (g-value demo-ob-text-label :width))))
    (:string (o-formula (g-value demo-ob :initial-item))))

  (create-instance 'demo-ob gg:option-button
	 (:left 15) (:top 15)
	 (:button-fixed-width-p NIL)
	 (:items '("Red" "Blue" "Green" "Yellow" "Aquamarine" "Cyan" "Fluorescent"))
	 (:initial-item "Cyan")
	 (:label "Color:")
	 (:button-font (opal:get-standard-font NIL :bold-italic NIL))
	 (:button-shadow-offset 2)
	 (:selection-function #'(lambda (g v)
				  (declare (ignore g))
				  (s-value demo-ob-text :string v)
				  (opal:update demo-option-win))))

  (create-instance 'ob-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in demo-ob))
    (:continuous NIL)
    (:window demo-option-win)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send demo-ob :special-popup-func
				  demo-ob))))

  (opal:add-components demo-option-agg demo-ob-text demo-ob-text-label demo-ob)
  (opal:update demo-option-win T)
  )

#+garnet-test
(defun Option-Button-Stop ()
  (opal:destroy demo-option-win))
    
