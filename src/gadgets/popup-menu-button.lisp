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
;;;  Popup menu from a button
;;;
;;;  Slots: (like those of menu and text-button)
;;;    :left - default 0
;;;    :top  - default 0
;;;    :string  - default GG:LINES-BITMAP; the string or object in the
;;;               button.  Default is a small bitmap showing a few lines
;;;            Another useful bitmap is GG:DOWNARROW-BITMAP
;;;     NOTE: do not use these bitmaps directly, but rather use something like
;;;        (:string (create-instance NIL GG:DOWNARROW-BITMAP))
;;;    :items  - Items in the pop-up menu
;;;    :item-to-string-function - Converts items to strings for the menu
;;;    :v-spacing  - default 0 ; of the menu
;;;    :h-align  - default :LEFT ; of the menu
;;;    :item-font  - default opal:default-font ; for the menu strings
;;;    :selection-function  - default NIL ; called with the menu selection
;;;    :keep-menu-in-screen-p  - default T ; if T, then menu will be
;;;                        adjusted if it would go offscreen
;;;    :position  - default :below; where menu pops up.  Choices are
;;;                 :below, :left, :right or a list of two numbers
;;;                 (can be computed with a formula)
;;;    :min-menu-width - default 0; the minimum width for the menu
;;;    :shadow-offset  - default 2 ; for the button
;;;    :text-offset  - default 3 ; for the button
;;;    :gray-width  - default 2 ; for the button
;;;
;;; Read-Only
;;;    :value  - The value selected in the menu
;;;
;;;
;;;  Designed and written by Richard McDaniel who stole most
;;;    of the code from Rajan Parthasarathy
;;;
;;;  CHANGE LOG:
;;;  05/23/94  Andrew Mickish - Fixed :window formula in :SELECTOR interactor
;;;              to check if menu's window is non-NIL before continuing gv's.
;;;  04/23/93  Andrew Mickish - Added :do-not-dump-slots
;;;  04/13/93  Andrew Mickish - Fixed typo: *screen-height* --> *screen-width*
;;;              in Fix-Menu-Out-Of-Screen
;;;  03/17/93  Brad Myers - add special-popdown functions for gilt, etc.
;;;                       - fixed :string-set-func so can edit label
;;;  03/10/93  Andrew Mickish - Fixed typo in :string-set-func (added opal
;;;              package prefix to Aggrelist-Edit-String-Func)
;;;  02/23/93  Andrew Mickish - Rewrote add/remove-item functions; added
;;;              :string-set-func
;;;  02/10/93  Andrew Mickish - Made :items of type items-type
;;;  01/28/93  Andrew Mickish - Fixed :width and :height formulas of menu
;;;              window so that NIL :items list won't crash;  added
;;;              :item-to-string-function
;;;  12/15/92  Andrew Mickish - Added type and parameter declarations
;;;  10/28/92  Andrew Mickish - Now :min-menu-width takes advantage of the
;;;              menu's new :min-menu-width slot
;;;  10/06/92  Brad Myers - support min-width and fix bugs
;;;  10/05/92  Andrew Mickish - Moved load of needed gadgets into loader file
;;;  10/04/92  Brad Myers - made work
;;;  09/17/92  Rich McDaniel - Created

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Popup-Menu-Button lines-bitmap downarrow-bitmap))

  #+garnet-test
  (export '(Popup-Menu-Button-Go Popup-Menu-Button-Stop)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First functions display the submenu from gilt, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Put-Down-Garnet-button-Popup (gadget)
  (let ((win (g-value gadget :menu-window)))
    (when (schema-p win)
      (s-value win :visible NIL)
      (opal:update win)))) ;; doesn't seem to be seeing the s-value 

;; return the popped up window
(defun Garnet-popup-button-item (gadget)
  (let ((menu (g-value gadget :menu))
	(menuwin (g-value gadget :menu-window)))
    (SetPopUpMenuPosition gadget menu menuwin)
    menuwin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is responsible for determining if the menu is out
;; of the screen or not.  If the menu is out of screen at
;; any of the sides, it will reset the :top and :left
;; attributes of the window to place it on the screen.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Fix-Menu-Out-Of-Screen (menuwin)
   (if (< (g-value menuwin :top) 0)
      (s-value menuwin :top 0)
      (if (> (+ (g-value menuwin :top) (g-value menuwin :height))
	     gem:*screen-height*)
         (s-value menuwin :top (- gem:*screen-height*
				(g-value menuwin :height)))
      )
   )
   (if (< (g-value menuwin :left) 0)
      (s-value menuwin :left 0)
      (if (> (+ (g-value menuwin :left) (g-value menuwin :width))
	     gem:*screen-width*)
         (s-value menuwin :left (- gem:*screen-width*
				 (g-value menuwin :width)))
      )
   )
)

(defun SetPopUpMenuPosition (popupbutton menu menuwin)
  (let* ((pos (g-value popupbutton :position))
	 (shadowoffset (g-value popupbutton :shadow-offset))
	 x y)
    (cond ((eq pos :below)
	   (setq x (+ (g-value popupbutton :left)
		      shadowoffset))
	   (setq y (+ (g-value popupbutton :top)
		      (g-value popupbutton :height))))
	  ((eq pos :right)
	   (setq x (opal:right popupbutton))
	   (setq y (- (g-value popupbutton :top)
		      (floor (- (g-value menu :height)
				(g-value popupbutton :height)) 2)
		      1)))
	  ((eq pos :left)
	   (setq x (+ (- (g-value popupbutton :left)
			 (g-value menu :width))
		      shadowoffset
		      1))
	   (setq y (- (g-value popupbutton :top)
		      (floor (- (g-value menu :height)
				(g-value popupbutton :height)) 2))))
	  ((listp pos)
	   (setq x (car pos))
	   (setq y (cadr pos)))
	  (T (error ":position argument for ~s must
 be :below :right :left or list of (x y), but it is ~s" popupbutton pos)))
    (multiple-value-bind (wx wy)
	(opal:convert-coordinates (g-value popupbutton :window) x y NIL)
      (s-value menuwin :left wx)
      (s-value menuwin :top wy)
      (when (g-value popupbutton :keep-menu-in-screen-p)
         (Fix-Menu-Out-Of-Screen menuwin))

      (s-value menuwin :visible T)
      (opal:raise-window menuwin))))  ;; raise calls update

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call this function to show the popup-menu.
;; It positions the menu at the given place, pops up the
;; menu, and starts the interactor.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun Pop-Up-Pop-Up (popupbutton)
  (let* ((event inter:*current-event*)
	 (menu (g-value popupbutton :menu))
	 (menuwin (g-value popupbutton :menu-window)))
    (SetPopUpMenuPosition popupbutton menu menuwin)
    (setf (inter:event-window event) menuwin)
    (setf (inter:event-x event) -10)
    (setf (inter:event-y event) -10) ; make sure is :outside
    (inter:start-interactor (g-value menu :selector) event)
    ;; make sure the menu interactor processes the event, so will be :outside
    (inter::general-go (g-value menu :selector) event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the popup menu gadget itself.  Really it's
;; just a window.  A menu gets put inside on
;; initialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(create-instance 'lines-bitmap opal:bitmap
    (:image (opal:read-image (merge-pathnames "pop-up-icon-no-border.bm"
					      common-lisp-user::Garnet-Bitmap-Pathname))))

(create-instance 'downarrow-bitmap opal:bitmap
    (:image (opal:read-image (merge-pathnames "downarrow.bm"
					      common-lisp-user::Garnet-Bitmap-Pathname))))

(create-instance 'Popup-Menu-Button gg:text-button
  :declare ((:parameters :left :top :string :items :v-spacing :h-align
			 :item-font :keep-menu-in-screen-p :position
			 :min-menu-width :shadow-offset :text-offset
			 :gray-width :selection-function :visible)
	    (:type ((or string (satisfies schema-p)) :string)
		   (items-type :items)
		   (integer :v-spacing :min-menu-width :shadow-offset
			    :text-offset :gray-width)
		   ((member :left :center :right) :h-align)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :item-font)
		   (kr-boolean :keep-menu-in-screen-p)
		   ((or (member :below :left :right) list) :position)
		   ((or null function symbol) :selection-function)))
  (:left 0)
  (:top 0)
  (:string #-apple lines-bitmap #+apple "Menu")
  (:items '("Item 1" "Item 2" "Item 3" "Item 4"))
  (:v-spacing 0)
  (:h-align :LEFT)
  (:title NIL)
  (:item-font opal:default-font)
  (:item-to-string-function
   #'(lambda (item)
       (if item
	   (if (or (stringp item) (schema-p item))
	       item
	       (string-capitalize (string-trim ":" item)))
	   "")))
  (:selection-function NIL)
  (:value-obj (o-formula (gvl :menu :value-obj)))
  (:value (o-formula (gvl :value-obj :string)))
  (:keep-menu-in-screen-p T)
  (:position :below)
  (:min-menu-width 0) 

  (:shadow-offset 2)
  (:text-offset 3)
  (:gray-width 2)

  ;; internal slots
  (:final-feedback-p NIL)
  
  (:menu NIL) ; filled in by initialize with the menu
  (:menu-window NIL)  ; filled in by initialize with the menu's window

  (:special-popup-func 'Garnet-popup-button-item)
  (:special-popdown-func 'Put-Down-Garnet-button-Popup)
  (:update-slots (cons :visible (g-value gg:text-button :update-slots)))

  (:interactors
      `((:text-button-press :modify
	 (:running-where T)
	 (:window ,(o-formula (let* ((oper (gvl :operates-on))
				     (my-win (gv oper :window))
				     (menu-win (gv oper :menu-window)))
				(if (and my-win menu-win)
				    (list my-win menu-win)
				    my-win))))
	 (:start-action
	      ,#'(lambda (inter button)
		    (call-prototype-method inter button)
		    (pop-up-pop-up button)))
	 (:final-function
	  ,#'(lambda (int val)
	      (declare (ignore val))
	      (let* ((oper (g-value int :operates-on))
		     (menu (g-value oper :menu))
		     (menuinter (g-value menu :selector))
		     (outside (g-value menuinter :current-state))
		     val)
		(inter:stop-interactor menuinter) ; in case still running
		;; when menuinter is outside, then will abort, so don't do
		;; selection function, etc.
		(unless (eq outside :outside)
		  (setq val (g-value menu :value)) ; do this after stop-inter
		  (s-value oper :value val)
		  (kr-send oper :selection-function oper val)))))
	 ))))

(s-value POPUP-MENU-BUTTON :do-not-dump-slots
	 (append '(:menu-window :menu)
		 (g-value POPUP-MENU-BUTTON :do-not-dump-slots)))

(define-method :fix-update-slots Popup-Menu-Button (ob)
  ;; will be called when :visible changes
  (unless (g-value ob :visible)
    (Put-Down-Garnet-button-Popup ob))
  (call-prototype-method ob))

(create-instance 'popupbuttonmenuproto gg:menu
  (:left 0) (:top 0)
  (:popup NIL)
  (:v-spacing (o-formula (gvl :popup :v-spacing)))
  (:h-align (o-formula (gvl :popup :h-align)))
  (:shadow-offset 0)
  (:text-offset (o-formula (gvl :popup :text-offset)))
  (:title NIL)
  (:items (o-formula (gvl :popup :items)))
  (:item-to-string-function (o-formula (gvl :popup :item-to-string-function)))
  (:item-font (o-formula (gvl :popup :item-font)))
  (:min-menu-width (o-formula (gvl :popup :min-menu-width)))
  (:interactors
   `((:selector :modify
      (:window ,(o-formula
		 (let* ((oper (gv-local (gv :self) :operates-on))
			(my-win (gv-local oper :window)))
		   (when my-win
		     (let ((button-win (gv oper :popup :window)))
		       (if button-win
			   (list my-win button-win)
			   my-win))))))
      (:stop-action ,#'(lambda (int val)
			 (call-prototype-method int val)
			 (let ((window (g-value int :operates-on :window)))
			   (s-value window :visible NIL))))
      (:abort-action ,#'(lambda (int val)
			  (call-prototype-method int val)
			  (let ((window (g-value int :operates-on :window)))
			    (s-value window :visible NIL))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the initialize method for the popup menu.
;; It automatically creates a menu gadget inside the
;; popup window.
;;
;; Then, it fixes up the interactors in the menu.  First,
;; the stop event is set to be leftup, which is releasing
;; the mouse button.  Then, it sets up the stop action of
;; the menu so that the window is made invisible.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-method :initialize Popup-Menu-Button (popup)
 (let (popmenu window)
   (call-prototype-method popup)
   (let ((kr::*demons-disabled* NIL)) ; turn on all demons
     (setq popmenu (create-instance NIL popupbuttonmenuproto
		     (:popup popup)))
     ;; change the formula so the menu's width will be bigger if necessary
     (setq window (create-instance NIL inter:interactor-window
		  (:omit-title-bar-p T)
		  (:double-buffered-p T)
		  (:save-under T)
		  (:border-width 0)
		  (:menu popmenu)
		  (:aggregate popmenu)
		  (:popup popup)
		  (:modal-p (o-formula (gv-local :self :popup :window :modal-p)))
		  ;; A drawable width or height of 0 causes CLX to crash
		  (:height (o-formula (max 1 (gvl :menu :height))))
		  (:width (o-formula (max 1 (gvl :menu :width))))
		  (:visible NIL)))
       (s-value popup :menu popmenu)
       (s-value popup :menu-window window)
       (opal:Update window))))

(define-method :destroy-me Popup-Menu-Button (popup &optional erase)
  (let ((menu-win (g-value popup :menu-window)))
    (Put-Down-Garnet-button-Popup popup)
    (when menu-win
      (opal:destroy menu-win))
    (call-prototype-method popup erase)))


(define-method :add-local-item POPUP-MENU-BUTTON (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item
			       '(:menu :menu-item-list) args))
(define-method :add-item POPUP-MENU-BUTTON (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item
			 '(:menu :menu-item-list) args))

(define-method :remove-local-item POPUP-MENU-BUTTON
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item
				  '(:menu :menu-item-list) key))
(define-method :remove-item POPUP-MENU-BUTTON
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item '(:menu :menu-item-list) key))

(s-value POPUP-MENU-BUTTON :change-item
	 (g-value opal:aggrelist :change-item))
(s-value POPUP-MENU-BUTTON :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(define-method :string-set-func POPUP-MENU-BUTTON
    (gadget-obj str-obj final-event final-string)
  (if (eq str-obj (g-value gadget-obj :text))
      (progn       ;; then is the label of the button
	(s-value gadget-obj :string final-string)
	T)
      ;; else must be in the menu
      (let ((aggrel (g-value gadget-obj :MENU :MENU-ITEM-LIST)))
    (opal::Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				      final-event final-string :rank))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is like the mini-demo function for this gadget.
;; It basically creates a window with a button.  Pressing
;; the button will cause a popup window to appear.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#+garnet-test
(defun Popup-Menu-Button-Go ()
   (create-instance 'demo-popup-win inter:interactor-window
         (:title "Popup Menu Button Demo")
         (:left 100) (:top 100) (:height 90) (:width 180))
   (create-instance 'demo-popup-agg opal:aggregate)
   (s-value demo-popup-win :aggregate demo-popup-agg)
   (opal:update demo-popup-win)

   (create-instance 'demo-popup-text-label opal:text
         (:top 50) (:left 15)
	 (:string "Selected: "))

   (create-instance 'demo-popup-button Popup-Menu-Button
	 (:left 15)
	 (:top 10)
	 (:string "foo")
	 (:items '("Red" "Blue" "Green" "Yellow" "Aquamarine"
		   "Cyan" "Fluorescent"))
	 (:selection-function #'(lambda (g v)
				  (declare (ignore g))
				  (format T "Selected is ~s~%" v))))

   (create-instance 'demo-popup-text opal:text
        (:top 50)
	(:left (o-formula (+ (g-value demo-popup-text-label :left)
			     (g-value demo-popup-text-label :width))))
	(:string (o-formula (format NIL "~s" (gv demo-popup-button :value)))))


  (create-instance 'popup-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in demo-popup-button))
    (:continuous NIL)
    (:window demo-popup-win)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send demo-popup-button :special-popup-func
				  demo-popup-button))))
  
   (opal:add-components demo-popup-agg demo-popup-button demo-popup-text
			demo-popup-text-label)

   (opal:update demo-popup-win)
   )

#+garnet-test
(defun Popup-Menu-Button-Stop ()
   (opal:destroy demo-popup-win)
   )
