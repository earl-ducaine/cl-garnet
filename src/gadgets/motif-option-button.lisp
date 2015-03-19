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
;;;  03/22/94  Andrew Mickish - Fixed menu window positioning in Set-Motif-
;;;              Menu-Top-And-Left when option-button is in a subwindow
;;;  01/12/94  Andrew Mickish - xlib:drawable-plist ---> opal:drawable-to-window
;;;  08/23/93  Andrew Mickish - Because of item-prototype-object problems,
;;;              don't constrain :background-color of menu-win.
;;;  03/17/93  Brad Myers - add special-popdown functions for gilt, etc.
;;;  02/23/93  Andrew Mickish - Added :string-set-func
;;;  02/19/93  Andrew Mickish - Added Add/Remove-Item methods
;;;  02/10/93  Andrew Mickish - Made :items of type items-type
;;;  01/25/93  Szekely - Added :width and :height formulas to MOTIF-TEXT-BUTTON
;;;  12/15/92  Mickish - Added type and parameter declarations; added
;;;              :background-color to menu window
;;;  12/10/92  Mickish - *drawable-to-window-mapping* ---> *garnet-windows*
;;;  09/28/92  Mickish - Formula --> o-formula in Put-Motif-Menu-In-Window
;;;  08/14/92  Rajan - Added :foreground-color
;;;  07/29/92  Rajan - Created

(in-package "GARNET-GADGETS")
(defparameter demo-motif-ob NIL)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(motif-option-button
	    #+garnet-test motif-option-button-go
	    #+garnet-test motif-option-button-stop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First functions display the submenu from gilt, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Put-Down-Motif-Option-Popup (gadget)
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
(defun Motif-Option-popup-item (gadget)
  (let ((g (g-value gadget :option-text-button))
	(win (g-value gadget :option-button-menu :window)))
    (when (opal:point-in-gob g (inter:event-x inter:*current-event*)
			     (inter:event-y inter:*current-event*))
      (Set-Motif-Menu-Top-And-Left g (g-value g :string))
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

(defun Motif-Menu-Out-Of-Screen (button)
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
;; out of the screen, by calling motif-menu-out-of-screen.
;;
;; If the top of the menu is out of screen, it positions
;; the top at 0.  If the bottom is out, it positions the
;; bottom as far down as possible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Set-Motif-Menu-Top-And-Left (g v)
  (let ((menu (g-value g :parent :option-button-menu))
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
				       (* (g-value g :height) pos)))
				 (g-value top-win :top-border-width))))

    (opal:update (g-value menu :window))

    (when (g-value g :parent :keep-menu-in-screen-p)
      (if (eq (motif-menu-out-of-screen g) 'top)
	  (s-value menu :win-top
		   (- (g-value g :parent :window :top-border-width)))
	  (if (eq (motif-menu-out-of-screen g) 'bottom)
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

(defun warp-motif-to-correct-place (butt val)
  (let* ((menu (g-value butt :parent :option-button-menu))
	 (left-offset (- (inter:event-x inter:*current-event*)
			 (g-value butt :left)))
	 (top-offset (- (inter:event-y inter:*current-event*)
			(g-value butt :top)))
	 (pos (or (position val (g-value butt :parent :real-items)
				:test #'equalp)
		  0))
	 (new-y (+ (* (g-value butt :height)
		      pos)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Motif-Ob-Selection-Function (g v)
  (declare (ignore v))
  (let* ((menu (g-value g :parent :option-button-menu))
	 (menu-win (g-value menu :window))
	 (win (g-value g :parent :window))
	 (background-color (g-value g :foreground-color))
	 (ev-x NIL) (ev-y NIL)
	 (ev inter:*current-event*))

    (Set-Motif-Menu-Top-And-Left g (g-value g :string))

    ;; Set the background of the menu's window so that it doesn't flash
    ;; white as it becomes visible.  Check before setting, so we avoid extra
    ;; work in opal::Fix-Window-Properties.
    (unless (eq (g-value menu-win :background-color) background-color)
      (s-value menu-win :background-color background-color))

    (opal:update (g-value menu :window))
    
    (setf (inter:event-window ev) (g-value menu :window))
    (multiple-value-setq (ev-x ev-y)
      (warp-motif-to-correct-place g (g-value g :string)))

    (setf (inter:event-x ev) ev-x)
    (setf (inter:event-y ev) ev-y)
    
    (inter:start-interactor (g-value menu :press) ev)
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
;; :text-offset -> The text offset of the button
;; :label -> The string that appears before the button
;; :button-offset -> The distance between the button and :label
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

(create-instance 'MOTIF-OPTION-BUTTON motif-gadget-prototype
  :declare ((:parameters :left :top :text-offset :label :button-offset
			 :items :initial-item :button-font :label-font
			 :foreground-color :value :button-fixed-width-p
			 :v-spacing :keep-menu-in-screen-p :menu-h-align
			 :selection-function :visible)
	    (:type (string :label :initial-item :value)
		   (integer :text-offset :button-offset :v-spacing)
		   (items-type :items)
		   (kr-boolean :button-fixed-width-p :keep-menu-in-screen-p)
		   ((member :left :center :right) :menu-h-align)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :button-font :label-font)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :top :left :text-ofset :label :button-offset
			     :items :initial-item :button-font :label-font
			     :button-fixed-width-p :v-spacing :menu-h-align
			     :keep-menu-in-screen-p :foreground-color))
;; Customizable slots

  (:top 40) (:left 40)
  (:text-offset 6)
  (:label "Option button:")
  (:button-offset 2)
  (:items '("Item 1" "Item 2" "Item 3" "Item 4"))
  (:initial-item (o-formula (if (listp (first (gvl :items)))
				(first (first (gvl :items)))
				(first (gvl :items)))))
  (:button-font opal:default-font)
  (:label-font (opal:get-standard-font NIL :bold NIL))
  (:selection-function NIL)
  (:value (o-formula (gvl :option-text-button :string)))
  (:button-fixed-width-p T)
  (:v-spacing 8)
  (:keep-menu-in-screen-p T)
  (:menu-h-align :LEFT)
  (:foreground-color opal:motif-gray)
  
;; Non-customizable slots
  (:width (o-formula (+ (gvl :option-button-label :width)
			(gvl :option-text-button :width)
			(gvl :button-offset))))
  (:height (o-formula (max (gvl :option-button-label :height)
			   (gvl :option-text-button :height))))
  (:real-items (o-formula (let ((l NIL))
			    (if (listp (first (gvl :items)))
				(progn
				  (dolist (i (gvl :items))
				    (setf l (cons (first i) l)))
				  (setf l (reverse l)))
				(gvl :items)))))

  (:special-popup-func 'Motif-Option-popup-item)
  (:special-popdown-func 'Put-Down-Motif-Option-Popup)
  (:update-slots (list :visible))

  (:parts
   `((:option-button-label ,opal:text
      (:top ,(o-formula (gvl :parent :top)))
      (:left ,(o-formula (gvl :parent :left)))
      (:string ,(o-formula (gvl :parent :label)))
      (:font ,(o-formula (gvl :parent :label-font))))

     (:option-text-button ,gg:motif-text-button
      ;; RGA modified this list because we don't want string to be constant.
      (:maybe-constant
       :left :top :text-offset :active-p :toggle-p 
       :final-feedback-p :foreground-color :visible)
      (:foreground-color ,(o-formula (gvl :parent :foreground-color)))
      (:top ,(o-formula (- (gvl :parent :top)
			  (floor
			   (- (gvl :height)
			      (gvl :parent :option-button-label :height))
			   2))))
      (:left ,(o-formula (+ (gvl :parent :left)
			    (gvl :parent :option-button-label :width)
			    (gvl :parent :button-offset))))
      (:height ,(o-formula (gvl :parent :option-button-menu
				:menu-item-list :tail :height)))
      (:width ,(o-formula (if (gvl :parent :button-fixed-width-p)
				     (gvl :parent :option-button-menu
					  :menu-item-list :width)
				     (+ (* 2 (gvl :text-offset))
					(gvl :text-width)))))
      (:final-feedback-p NIL)
      (:text-offset ,(o-formula (gvl :parent :text-offset)))
      (:gray-width 0)
      (:font ,(o-formula (gvl :parent :button-font)))
      (:string ,(o-formula (gvl :parent :initial-item)))
      (:selection-function ,#'Motif-Ob-Selection-Function)
      (:interactors
       ((:press :modify
		(:continuous NIL))))))))

(define-method :fix-update-slots MOTIF-OPTION-BUTTON (ob)
  ;; will be called when :visible changes
  (unless (g-value ob :visible)
    (Put-Down-Motif-Option-Popup ob))
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

(defun Put-Motif-Menu-In-Window (motif-opt)     
  (let* ((motif-ob-menu
	  (create-instance NIL gg:motif-menu
	    (:title NIL)
	    (:option-button-parent motif-opt)
	    (:foreground-color (o-formula
				(gvl :option-button-parent :foreground-color)))
	    (:item-font (o-formula (gvl :option-button-parent :button-font)))
	    (:shadow-offset 0)
	    (:selection-function
	     #'(lambda (g v)
		 (let* ((parent (g-value g :option-button-parent))
			(sel-fn (g-value parent :selection-function)))
		   (if sel-fn
		       (funcall sel-fn parent (g-value v :string))))))
	    (:items (o-formula (gvl :option-button-parent :items)))
	    (:v-spacing (o-formula (gvl :option-button-parent :v-spacing)))
	    (:text-offset (o-formula (gvl :option-button-parent :text-offset)))
	    (:h-align (o-formula (gvl :option-button-parent :menu-h-align)))
	    (:win-left 0) (:win-top 0)
	    (:interactors
	     `((:press :modify
		(:stop-event :leftup)
		(:window ,(o-formula
			   (let* ((the-opt (gvl :operates-on
						:option-button-parent))
				  (my-win (gv the-opt :option-button-menu
					      :window))
				  (the-opt-win (gv the-opt :window)))
			     (if (and my-win the-opt-win)
				 (list my-win the-opt-win)
				 (or my-win the-opt-win)))))
		(:stop-action
		 ,#'(lambda (int val)
		      (let* ((gad (g-value int :operates-on))
			     (parent (g-value gad :option-button-parent))
			     (button (g-value parent :option-text-button))
			     (win (g-value parent :window))
			     (menu-win (g-value gad :window)))
			(s-value button :string (g-value val :string))
			(s-value button :font (g-value val :item-text :font))
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
	 (motif-win (create-instance NIL inter:interactor-window
		    (:motif-ob-menu (o-formula (first (gvl :aggregate :components))))
		    (:omit-title-bar-p T)
		    (:double-buffered-p T)
		    (:save-under T)
		    (:border-width 0)
		    (:left (o-formula
			    (if (gvl :motif-ob-menu :option-button-parent
				    :window)
				(+ (gvl :motif-ob-menu :win-left)
				   (gvl :motif-ob-menu :option-button-parent
				       :window
				       :left-border-width))
				0)))
		    (:top (o-formula
			   (if (gvl :motif-ob-menu :option-button-parent
					 :window)
			       (+ (gvl :motif-ob-menu :win-top)
				      (gvl :motif-ob-menu :option-button-parent
					  :window
					  :top-border-width))
			       0)))
		    (:height (o-formula (gvl :motif-ob-menu :height)))
		    (:width (o-formula (gvl :motif-ob-menu :width)))
		    (:visible NIL)
		    (:button motif-opt)
		    (:modal-p (o-formula
			       (gv-local :self :button :window :modal-p)))
		    )))
    (s-value motif-win :aggregate (create-instance NIL opal:aggregate))
    (s-value motif-ob-menu :window motif-win)
    (opal:add-component (g-value motif-win :aggregate) motif-ob-menu)
    (s-value motif-opt :option-button-menu motif-ob-menu)
;    (g-value motif-opt :option-button-menu :press :window)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the initialize method for the gadget.
;; It calls the Put-Motif-Menu-In-Window function.  It also
;; sets the initial item of the menu to be the item
;; in :initial-item slot.
;;
;; Then, it fixes up the interactors in the menu.  First,
;; the stop event is set to be leftup, which is releasing
;; the mouse button.  Then, it sets up the stop action of
;; the menu so that the button is made visible and the
;; menu, invisible, and also sets the button's string to
;; be the new value.
;;
;; Next, the running action is changed so that if the mouse
;; is released outside, nothing is selected.
;;
;; Then the continuous slot of the button is set to NIL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method :initialize MOTIF-OPTION-BUTTON (opt)
   (call-prototype-method opt)
   (Put-Motif-Menu-In-Window opt)

   (let ((menu (g-value opt :option-button-menu)))
     (g-value menu :value)
     (s-value menu :value (g-value opt :initial-item))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the destroy method for the option button.
;; It first destroys the window in which the menu is
;; sitting, THEN it destroys the gadget.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method :destroy-me  Motif-Option-Button (ob &optional erase)
  (let ((window (g-value ob :option-button-menu :window)))
    (Put-Down-Motif-Option-Popup ob)
    (if (and window
	     (schema-p window)
	     (opal:drawable-to-window (get-local-value window :drawable)))
	(opal:destroy window))
    (call-prototype-method ob erase)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add/Remove-Item methods
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method :add-local-item MOTIF-OPTION-BUTTON (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item
			       '(:option-button-menu :menu-item-list) args))
(define-method :add-item MOTIF-OPTION-BUTTON (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item 
			 '(:option-button-menu :menu-item-list) args))
   
(define-method :remove-local-item MOTIF-OPTION-BUTTON
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item 
				  '(:option-button-menu :menu-item-list) key))
(define-method :remove-item MOTIF-OPTION-BUTTON
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item 
			    '(:option-button-menu :menu-item-list) key))

(s-value MOTIF-OPTION-BUTTON :change-item
	 (g-value opal:aggrelist :change-item))
(s-value MOTIF-OPTION-BUTTON :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(s-value MOTIF-OPTION-BUTTON :string-set-func
	 #'opal::Option-Button-String-Func)

(s-value MOTIF-OPTION-BUTTON :do-not-dump-slots
	 (append '(:option-button-menu)
		 (g-value MOTIF-OPTION-BUTTON :do-not-dump-slots)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is like the mini-demo function for this gadget.
;; It basically creates a window with an option button
;; and an opal:text box, that is the selected object.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+garnet-test
(defun Motif-Option-Button-Go ()
  (create-instance 'demo-motif-option-win inter:interactor-window
    (:title "Option Demo")
    (:background-color opal:motif-gray)
    (:left 800) (:top 210) (:height 90) (:width 180))
  (create-instance 'demo-motif-option-agg opal:aggregate)
  (s-value demo-motif-option-win :aggregate demo-motif-option-agg)
  (opal:update demo-motif-option-win T)

  (create-instance 'demo-motif-ob-text-label opal:text
    (:top 50) (:left 15)
    (:string "Selected: "))

  (create-instance 'demo-motif-ob-text opal:text
    (:top 50)
    (:left (o-formula (+ (g-value demo-motif-ob-text-label :left)
			 (g-value demo-motif-ob-text-label :width))))
    (:string (o-formula (g-value demo-motif-ob :initial-item))))

  (create-instance 'demo-motif-ob gg:motif-option-button
	 (:left 15) (:top 15)
	 (:button-fixed-width-p NIL)
	 (:items '("Red" "Blue" "Green" "Yellow" "Aquamarine" "Cyan" "Fluorescent"))
	 (:initial-item "Cyan")
	 (:label "Color:")
	 (:text-offset 10)
	 (:button-font (opal:get-standard-font NIL :bold-italic NIL))
	 (:selection-function #'(lambda (g v)
				  (declare (ignore g))
				  (s-value demo-motif-ob-text :string v)
				  (opal:update demo-motif-option-win))))
  
  (create-instance 'motif-ob-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in demo-motif-ob))
    (:continuous NIL)
    (:window demo-motif-option-win)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send demo-motif-ob :special-popup-func
				  demo-motif-ob))))
  
  (opal:add-components demo-motif-option-agg
		       demo-motif-ob-text demo-motif-ob-text-label demo-motif-ob)
  (opal:update demo-motif-option-win T)
  )

#+garnet-test
(defun Motif-Option-Button-Stop ()
  (opal:destroy demo-motif-option-win))
