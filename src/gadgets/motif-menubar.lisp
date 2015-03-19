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
;;;  Motif Menubar
;;;
;;;
;;;  Customizable slots:
;;;    1)  Left, top
;;;    3)  Title-font, item-font, accel-font
;;;    5)  Selection-function - a function to be executed when any item is
;;;           selected.  Takes the parameters (gadget menu-item submenu-item).
;;;    6)  Items - A list with the format
;;;           '(("m1" m1func ("m1,1"..."m1,N"))
;;;             ("m2" m2func (("m2,1" m2,1func)...("m2,N" m2,Nfunc)))
;;;             ...)
;;;           Where "mN" is a string or atom that is the title of a menu,
;;;        "mX,Y" is a string or atom in menu X, row Y,
;;;        mNfunc is executed when any item in menu N is selected,
;;;        mX,Yfunc is executed when item mX,Y is selected.
;;;           These two functions take the same parameters as the
;;;        selection-function.
;;;    7)  Accelerators - A list of accelerators, with the format
;;;             '((("s11" key11) .. ("s1N" key1N))
;;;               (("s21" key21) .. ("s2N" key2N))
;;;               ...)
;;;          where sXY is the accelarator string for the Yth submenu-item
;;;          in the Xth bar-item of the menubar; keyXY is the keyboard
;;;          event that starts the accelerator for the Yth submenu-item in
;;;          the Xth bar-item.
;;;    8)  Bar-Above-These-Items - A list of item-objs, with the format
;;;            '((ob11 ... ob1N)
;;;              (ob21 ... ob2N)
;;;              ...)
;;;           where obXY is the object in the Yth submenu-item of the Xth
;;;           bar-item above which a line should appear.
;;;    9)  Min-Menubar-Width - How big the menubar should be.  If it is too
;;;           small, or 0, this slot is ignored.
;;;
;;;  Programming interface (Garnet way):
;;;    1) Create an instance of motif-menubar with a value for the :items slot
;;;    2) Use opal:add-component to put the instance in a window
;;;
;;;  Caveats:
;;;     New motif-bar-items should be created with the :enable slot set to NIL
;;;  in order to keep their windows from being updated before they are added to
;;;  a menubar.

;;;  Pull Down Menus Demo:
;;;    The function below creates a window containing a pull down menu and
;;;    some text objects, as well as accelerators and bars.
;;;    Choosing an item from the pull-down menu will change the font of the
;;;    text objects.
;;;    To run it, enter (GARNET-GADGETS:motif-menubar-go).
;;;    To stop, enter (GARNET-GADGETS:motif-menubar-stop).
;;;
;;;  Designed and Implemented by Rajan Parthasarathy
;;;  Based heavily upon the GARNET menubar by Andrew Mickish & Pavan Reddy
;;;
;;;  CHANGE LOG:
;;;


;;;  01/14/97  Russell Almond (Thien Ngyen) - Code to make character
;;;  accelerators case insensitive.  Added Right justified help item.
;;;  12/06/94  Andrew Mickish - Added :modal-p formula to pop-up windows
;;;  06/22/93  Andrew Mickish - Set :rank slot in :add-item methods
;;;  06/15/93  Andrew Mickish - Removed references to :number-of-comps;
;;;             Added :do-not-dump-slots for BAR-ITEM; Called Check-Bar-Item
;;;             in Make-Bar-Item;  In :abort-action, check prev-baritem
;;;             is non-NIL before s-valuing it; Added :fix-update-slots method
;;;             for MOTIF-BAR-ITEM
;;;  06/10/93  Andrew Mickish - Moved MOTIF-MENU-ITEM modifications (like the
;;;             :active-p formula) into the MOTIF-SUBMENU definition
;;;  05/16/93  Andrew Mickish - Put :width and :height formulas in BAR-ITEM
;;;  04/23/93  Andrew Mickish - Added :do-not-dump-slots
;;;  04/22/93  Andrew Mickish - Added :active-p
;;;  04/18/93  Andrew Mickish - Removed ,@ from MOTIF-BAR-ITEM's :add-item
;;;             method
;;;  04/13/93  Andrew Mickish - Fixed bugs in change of 03/08/93
;;;  03/17/93  Brad Myers - add special-popdown functions for gilt, etc.
;;;  03/08/93  Andrew Mickish - Fixed :left and :top formulas of submenu-window
;;;             so that offset is correct when menubar is in a subwindow;
;;;             Type check :items with Check-Menubar-Items;  Added some slots
;;;             to type declarations.
;;;  03/03/93  Andrew Mickish - Fixed :string-set-func to work with bar-items
;;;  02/24/93  Andrew Mickish - Added :string-set-func
;;;  02/23/93  Andrew Mickish - Eliminated need to call notice-items-changed
;;;             on the menubar (but apparently must be called internally on
;;;             new submenus... Argh!)
;;;  02/10/93  Andrew Mickish - Fixed typo (:font ---> :item-font) in
;;;              MOTIF-SUBMENU, added :accel-font
;;;  02/08/93  Andrew Mickish - :notice-items-changed ---> :fix-update-slots
;;;  01/07/93  Rajan Parthasarathy - Created

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(MOTIF-MENUBAR MOTIF-BAR-ITEM MAKE-MOTIF-SUBMENU-WIN
	    ; Creation Functions
	    Make-Motif-Menubar Make-Motif-Bar-Item Make-Motif-Submenu-Item))

  ; Demo things
  #+garnet-test
  (export '(MOTIF-MENUBAR-GO MOTIF-MENUBAR-STOP DEMO-MOTIF-MENUBAR
	    MOTIF-MENUBAR-WIN MOTIF-MENUBAR-TOP-AGG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First function is to display the submenu from gilt, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the new window if and only if a different bar-item is to be
;; displayed.  Gadget parameter is the top level menubar.
(defun Motif-Menubar-popup-item (gadget)
  (let (bar-item)
    ;; first, find the MOTIF-BAR-ITEM that is under the mouse
    (setq bar-item (opal:point-to-component (g-value gadget :menubar-items)
				      (inter:event-x inter:*current-event*)
				      (inter:event-y inter:*current-event*)))
    (when bar-item
      (DoSpecialPopUpMenubar gadget bar-item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a window that an instance of MOTIF-SUBMENU can be put into

(defun MAKE-MOTIF-SUBMENU-WIN (a-bar-item a-submenu)
  (let ((win
	 (create-instance NIL inter:interactor-window
	   (:bar-item a-bar-item)
	   (:aggregate a-submenu)
	   (:background-color (o-formula (gvl :bar-item :parent
					      :parent :foreground-color)))
	   (:border-width 0)
	   (:win-submenu a-submenu)
	   (:omit-title-bar-p T)
	   (:save-under T)         
	   (:double-buffered-p T)  
	   (:visible NIL)
	   (:modal-p (o-formula (gv-local :self :bar-item :window :modal-p)))
	   (:height (o-formula
		     (gvl :win-submenu :height)))
	   (:width (o-formula
		    (gvl :win-submenu :width)))
	   (:left (o-formula
		   (let* ((bar-item (gvl :bar-item))
			  (win (gv bar-item :window))
			  (left-offset 0))
		     (cond
		       (win
			(do ((parent (gv win :parent) (gv parent :parent)))
			    ((null parent) left-offset)
			  (setf left-offset
				(+ left-offset (gv parent :left)
				   (or (gv parent :left-border-width) 0))))
			(+ (gv bar-item :left) (gv win :left)
			   (gv win :left-border-width) left-offset -5))
		       (t 0)))))
	   (:top (o-formula
		  (let* ((bar-item (gvl :bar-item))
			 (win (gv bar-item :window))
			 (top-offset 0))
		    (cond
		      (win
		       (do ((parent (gv win :parent) (gv parent :parent)))
			   ((null parent) top-offset)
			 (setf top-offset
			       (+ top-offset (gv parent :top)
				  (or (gv parent :top-border-width) 0))))
		       (+ (gv bar-item :top) (gv bar-item :height)
			  (gv win :top) (gv win :top-border-width) top-offset))
		      (t 0)))))
	   )))
    (s-value a-submenu :window win)
    win))




;; This object is a text field in the menu bar.  
;; An aggrelist of these items makes up the menu bar.
;;
(create-instance 'MOTIF-BAR-ITEM motif-gadget-prototype
   (:width (o-formula (gvl :text :width)))
   (:height (o-formula (gvl :text :height)))
   ; The mnemonic ":desc" is a description of a submenu.  The top-level :items
   ; slot is a list of desc's.
   (:desc (o-formula (nth (gvl :rank) (gvl :parent :parent :items))))
   (:accelerators (o-formula
		   (nth (gvl :rank) (gvl :parent :parent :accelerators))))
   (:bar-above-these-items
    (o-formula (nth (gvl :rank) (gvl :parent :parent :bar-above-these-items))))
   (:menu-obj (o-formula (first (gvl :desc))))
   (:action (o-formula (second (gvl :desc))))
   (:items (o-formula (third (gvl :desc))))
   (:string (o-formula (let ((menu-obj (gvl :menu-obj)))
			 (if (stringp menu-obj)
			     menu-obj
			     (string-capitalize
			      (string-trim ":" menu-obj))))))
   (:font (o-formula (gvl :parent :parent :title-font)))
   (:spacing 5)
   (:enabled (o-formula (when (gvl :parent :parent :active-p)
			  (if (= 4 (length (gvl :desc)))
			      (fourth (gvl :desc))
			      T))))
   (:disabled-line-style (o-formula (gvl :parent :parent :disabled-line-style)))
   (:foreground-color (o-formula (gvl :parent :parent :foreground-color)))
   ;; slot :submenu filled by :fix-update-slots with a SUBMENU
   ;; slot :submenu-window set with the window of the submenu
   (:parts
    `(
      (:text ,opal:text
       (:left ,(o-formula (gvl :parent :left)))
       (:top ,(o-formula (gvl :parent :top)))
       (:font ,(o-formula (gvl :parent :font)))
       (:string ,(o-formula (string (gvl :parent :string))))
       (:line-style ,(o-formula 
		      (if (gvl :parent :enabled)
			  opal:default-line-style
			  (gvl :parent :disabled-line-style))))
       ))))

;; This is the top level MOTIF-MENUBAR gadget -- the big mutha.
;; Basically, it contains 3 parts -> a motif-box that represents
;; the menubar, an aggrelist of bar-items, and a feedback object.
;; It also has two interactors -- menubar-select: used for selecting
;; stuff from the menubar, and menubar-accel, which does the 
;; accelerator thang.

(create-instance 'MOTIF-MENUBAR motif-gadget-prototype
   :declare ((:parameters :left :top :items :title-font :item-font :accel-font
			  :foreground-color :min-menubar-width :accelerators
			  :bar-above-these-items :accelerator-windows
			  :active-p :selection-function :help-style)
	     (:type ((satisfies Check-Menubar-Items) :items)
		    (list :accelerators :bar-above-these-items)
		    ((or list (satisfies schema-p)) :accelerator-windows)
		    (integer :min-menubar-width)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
			 :title-font :item-font :accel-font)
		    ((is-a-p opal:color) :foreground-color)
		    (kr-boolean :active-p)
		    (kr-boolean :help-style)
		    ((or null function symbol) :selection-function)))
   ; Customizable slots
   (:left 0)(:top 0)
   (:left-offset 0)
   (:items NIL)
   (:title-font (opal:get-standard-font NIL :bold NIL))
   (:item-font (opal:get-standard-font NIL :bold NIL))
   (:accel-font (opal:get-standard-font NIL :bold NIL))
   (:min-menubar-width 0)
   (:accelerators NIL)
   (:bar-above-these-items NIL)
   (:accelerator-windows (o-formula (gvl :window)))
   (:active-p T)
   ;; RGA --- forces final item to be right justified.
   (:help-style NIL)  ;; T or Nil

   ; Internal slots
   (:disabled-line-style (o-formula
			  (create-instance NIL opal:line-style
			    (:stipple opal::gray-fill-bitmap)
			    (:background-color (gvl :foreground-color)))))
   (:enabled-items
    (o-formula (when (gvl :active-p)
		 (let ((enabled-items NIL))
		   (dovalues (component (gvl :menubar-items) :components
					:in-formula T
					:result enabled-items)
		       (when (gv component :enabled)
			 (push component enabled-items)))))))
   (:list-of-all-enabled-objects
    (o-formula (let ((l (copy-list (gvl :enabled-items))))
		 (dolist (baritems (gvl :enabled-items))
		   (setf l (append l (copy-list
				     (gv-local baritems :submenu :enabled-items)))))
		 l)))
   (:running-where-list (o-formula (append (gvl :list-of-all-enabled-objects)
					   (gvl :submenu-window-list))))
   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:destroy-me 'MENUBAR-DESTROY)
   (:special-popup-func 'Motif-Menubar-popup-item)
   (:special-popdown-func 'Put-Down-MenuBar-Popups)
   (:update-slots (cons :visible (g-value opal:aggrelist :update-slots)))
   (:old-visible T)
   
   ;; slot :submenu-window-list set with list of all windows being used.

   (:parts
    ;; This is the box that appears behind the text and
    ;; represents the menubar
    `((:menubar-box ,motif-box
       (:left ,(o-formula (gvl :parent :left)))
       (:top ,(o-formula (gvl :parent :top)))
       (:height ,(o-formula (+ (gvl :parent :menubar-items :height) 14)))
       (:width ,(o-formula (max (+ (gvl :parent :menubar-items :width) 18)
				(gvl :parent :min-menubar-width)))))

      ;; These are the menubar items themselves
      (:menubar-items ,opal:aggrelist
       (:left ,(o-formula (+ (gvl :parent :left)
			     (gvl :parent :left-offset) 9)))
       (:top ,(o-formula (+ (gvl :parent :top) 7)))
       (:items ,(o-formula (gvl :parent :items)))
       ;; RGA -- transmit help style info.
       (:right-justify-last ,(o-formula (gvl :parent :help-style)))
       (:justify-width ,(o-formula (- (gvl :parent :width) 18)))
       (:h-spacing 8)
       (:direction :horizontal)
       (:item-prototype ,MOTIF-BAR-ITEM)
       (:selection-function NIL)
       (:h-align :center))

      ;; feedback-obj-topline and bottom-line represent the
      ;; feedback object that appears on the menubar.
      (:feedback-obj-topline ,opal:polyline
       (:point-list ,(o-formula
		      (if (gvl :prev-baritem)
			  (let* ((ob (gvl :prev-baritem))
				 (left (gv ob :left))
				 (top (gv ob :top))
				 (ht (gv ob :height))
				 (wt (gv ob :width)))
			    (list (- left 4) (+ top ht 3)
				  (- left 4) (- top 2)
				  (+ left wt 4) (- top 2))))))
       (:prev-baritem ,(o-formula
			(gvl :parent :menubar-select :prev-baritem)))
       (:visible ,(o-formula
		   (AND (gvl :prev-baritem)
			(gvl :prev-baritem :enabled))))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :highlight-line-style)))
       (:fast-redraw-p :redraw)
       (:fast-redraw-line-style ,(o-formula (gv (kr-path 0 :parent)
						:foreground-line-style))))

      (:feedback-obj-bottomline ,opal:polyline
       (:point-list ,(o-formula
		      (if (gvl :prev-baritem)
			(let* ((ob (gvl :prev-baritem))
			       (left (gv ob :left))
			       (top (gv ob :top))
			       (ht (gv ob :height))
			       (wt (gv ob :width)))
			  (list (- left 5) (+ top ht 2)
				(+ left wt 3) (+ top ht 2)
				(+ left wt 3) (- top 1))))))
       
       (:prev-baritem ,(o-formula
			(gvl :parent :feedback-obj-topline :prev-baritem)))
       (:visible ,(o-formula (gvl :parent :feedback-obj-topline :visible)))
       (:line-style ,(o-formula (gv (kr-path 0 :parent) :shadow-line-style)))
       (:fast-redraw-p :redraw)
       (:fast-redraw-line-style ,(o-formula (gv (kr-path 0 :parent)
						:foreground-line-style))))
      ))
       
   (:interactors
    ;; The accelerator interactor.  Its menus are set to be all the
    ;; menus in the menubar.
    `((:menubar-accel ,motif-menu-accelerator-inter
       (:active ,(o-formula (let ((o (gv-local :self :operates-on)))
			      (and (gv o :active-p)
				   (gv o :window)))))
       (:window ,(o-formula (gvl :operates-on :accelerator-windows)))
       (:menus ,(o-formula
		 (let ((l NIL))
		   (dolist (a-baritem (gvl :operates-on :menubar-items :components))
		     (push (gv a-baritem :submenu) l))
		   l))))
      ;; This is for clicking on a bar item, moving out of the submenu's window,
      ;; etc. etc.
      (:menubar-select ,inter:menu-interactor
       (:active ,(o-formula (gv-local :self :operates-on :window)))
       (:window ,(o-formula
		  (let ((main-win (gv-local :self :operates-on :window))
			(submenu-wins (gv-local :self :operates-on
						:submenu-window-list)))
		    (if main-win
			(if submenu-wins
			    (cons main-win (copy-list submenu-wins))
			    main-win)))
		  ))
       (:start-where ,(o-formula (list :list-element-of
				       (gvl :operates-on)
				       :list-of-all-enabled-objects)))
       (:running-where ,(o-formula (list :list-element-of
					 (gvl :operates-on)
					 :running-where-list)))
       (:feedback-obj ,(o-formula (gvl :operates-on :feedback-obj-topline)))
       (:outside NIL)
       (:start-action
	,#'(lambda (inter obj)
	     (call-prototype-method inter obj)
	     (s-value (g-value inter :operates-on) :*bar-item-popped-up NIL)))
       (:outside-action
	,#'(lambda (inter outside prev)
	     (call-prototype-method inter outside prev)
	     
	     ;; make sure the top level bar items stay selected since
	     ;; sub-menus are showing
	     
	     (when (is-a-p prev MOTIF-BAR-ITEM)
	       (s-value prev :interim-selected T))))
       (:running-action
	,#'(lambda (inter prev new)
	     (let* ((prev-baritem (g-value inter :prev-baritem))
		    (new-is-bar (is-a-p new MOTIF-BAR-ITEM))
		    (new-baritem (when new
				   (if new-is-bar
				     new
				     ;; else is a sub-item, get its bar-item
				     (g-local-value new :bar-item)))))
	       (call-prototype-method inter prev new)

	       ;; Now, when you get into a submenu window, you want to start
	       ;; that submenu's interactor.
	       
	       (let ((curr-win (inter:event-window inter::*current-event*))
		     (obj-wins (g-local-value inter :operates-on :submenu-window-list)))
		 (if  (member curr-win obj-wins :test #'equal)
		     (let* ((submenu (g-local-value prev-baritem :submenu))
			    (first-obj-over
			     (OR (g-value submenu :press :remembered-last-object)
				 (first (g-value submenu :menu-item-list :components))))
			    (ce inter::*current-event*)
			    (ev-x (inter:event-x ce))
			    (ev-y (inter:event-y ce))
			    (ev (inter:make-event :window curr-win
						  :code (inter:event-code ce)
						  :char :leftdown
						  :mousep T :downp T
						  :x ev-x
						  :y ev-y 
						  :timestamp (inter:event-timestamp ce))))
		       (when (eq (g-local-value prev-baritem :submenu :press :current-state)
				 :start)
			 (inter:start-interactor (g-value submenu :press) ev))

		       ;; For some weird reason, the first item that's been selected
		       ;; in the newly popped up window don't have a feedback object
		       ;; around them.  So here, we EXPLICITLY tell it to put a
		       ;; feedback object around the FIRST item it selects, unless
		       ;; that item is disabled.
		       
		       (when (AND
			      (opal:point-in-gob first-obj-over ev-x ev-y)
			      (g-value first-obj-over :enabled))
			 (s-value submenu :value (g-value first-obj-over :item-obj))
			 (s-value submenu :value-obj first-obj-over)
			 (s-value first-obj-over :interim-selected T)))

		     (unless (NULL prev-baritem)
		       (s-value (g-local-value prev-baritem :submenu) :value-obj NIL)))
		     )

	       ;; keep the interim selected of the bar item so it
	       ;; shows up as highlighted when items in sub-menu selected.
	       (if new-baritem
		   ;; this makes the subwindow NOT go away when move
		   ;; outside subwindow
		   (unless (eq new-baritem prev-baritem)
		     (s-value inter :prev-baritem new-baritem)
		     (if prev-baritem
			 (let ((win (g-local-value prev-baritem :submenu-window))
			       (prev-selected (g-local-value prev-baritem :submenu :press
						       :remembered-last-object)))
			   (s-value prev-baritem :interim-selected NIL)
			   (when prev-selected
			     (s-value prev-selected :interim-selected NIL))
			   (s-value win :visible NIL)
			   (opal:update win)))
		     (if new-baritem
			 (let ((win (g-value new-baritem :submenu-window)))
			   (s-value (g-value new-baritem :submenu)
				    :value-obj NIL)
			   (s-value win :visible T)
			   (opal:raise-window win)))))  ;; Call update
	       ;;; this needs to be after the call-prototype-method,
	       ;;; since that is where the :interim-selected will be
	       ;;; turned off.
	       (unless new-is-bar
		 (when new-baritem
		   (s-value new-baritem :interim-selected T))))
	     ))
       (:abort-action
	,#'(lambda (inter last)
	     (declare (ignore last))
	     (let ((prev-baritem (g-value inter :prev-baritem)))
	       (when prev-baritem
		 (let ((prev-selected (g-value prev-baritem :submenu :press
					       :remembered-last-object)))

		   (s-value prev-baritem :interim-selected NIL)
		   (when prev-selected
		     (s-value prev-selected :interim-selected NIL)))
		 (let ((win (g-value prev-baritem :submenu-window)))
		   (s-value inter :prev-baritem NIL)
		   (s-value win :visible NIL)
		   (opal:update win))))))

       ;; This hides the submenu's window after an item has been selected
       (:final-function
	,#'(lambda (inter last)
	     (declare (ignore last))
	     (when (g-value inter :prev-baritem)
	       (let ((win (g-value inter :prev-baritem :submenu :window)))
		 (s-value win :visible NIL)
		 (opal:update win))
	       (s-value inter :prev-baritem NIL))))
       ))))

(s-value MOTIF-MENUBAR :do-not-dump-slots
	 (append '(:submenu-window-list)
		 (g-value MOTIF-MENUBAR :do-not-dump-slots)))
(s-value MOTIF-BAR-ITEM :do-not-dump-slots
	 (append '(:submenu :submenu-window)
		 (g-value MOTIF-BAR-ITEM :do-not-dump-slots)))
(let ((menubar-select (g-value MOTIF-MENUBAR :menubar-select)))
  (s-value menubar-select :do-not-dump-slots
	   (append '(:prev-baritem)
		   (g-value menubar-select :do-not-dump-slots))))
	 


;; This function gets called when an item has been selected from
;; the submenu.  Basically, it hides the submenu's window and
;; calls the appropriate :selection-function and the item's
;; function

(defun SUBMENU-SELECTION-FUNCTION (submenu submenu-item)
  (let* ((baritem (g-value submenu :bar-item))
	 (bar-action (g-value baritem :action))
	 (bar-obj (g-value baritem :menu-obj))
	 (a-menubar (g-value baritem :parent :parent)))
    (s-value (g-value submenu :window) :visible NIL)
    (s-value (g-value a-menubar :menubar-select) :prev-baritem NIL)
    (opal:update (g-value submenu :window))
    (if bar-action
	(funcall bar-action a-menubar bar-obj
		 (g-value submenu-item :item-obj)))
    (if (schema-p a-menubar)
	(kr-send a-menubar :selection-function
		 a-menubar bar-obj (g-value submenu-item :item-obj)))
    ))
				   
;; This is a submenu for the motif-menubar (surprise surprise!)
;; Its a motif-menu that inherits some slots from the top level
;; menubar.  Also, the accelerators for the menubar are given
;; as lists of two items - the string and the key.  BUT!  The
;; motif-menu wants its accelerators to be lists of THREE items -
;; the underline, the string, and the key.  So, we have to insert
;; NILs for the underlines.  That's what the o-formula in the
;; :accelerators slot does.
;;
;; A submenu-item is just an instance of MOTIF-MENU-ITEM.  Which
;; means any MOTIF-MENU-ITEM can be added to the menubar.  

(create-instance 'MOTIF-SUBMENU motif-menu
  (:bar-item NIL)
  (:menubar (o-formula (gvl :bar-item :parent :parent)))
  (:foreground-color
   (o-formula (gvl :menubar :foreground-color)))
  (:final-feedback-p NIL)
  (:item-font (o-formula (gvl :menubar :item-font)))
  (:accel-font (o-formula (gvl :menubar :accel-font)))
  (:items (o-formula (gvl :bar-item :items)))
  (:keyboard-selection-p NIL)
  (:selection-function #'SUBMENU-SELECTION-FUNCTION)
  (:accelerators (o-formula
		  (let ((bar-acc (gvl :bar-item :accelerators))
			menu-acc temp)
		    (when bar-acc
		      (dolist (a-list bar-acc)
			(setf temp (push NIL a-list))
			(setf menu-acc
			      (append menu-acc (list temp)))))
		    menu-acc)))
  (:bar-above-these-items (o-formula (gvl :bar-item :bar-above-these-items)))
  (:text-offset (o-formula (if (gvl :accelerators) 4 0)))
  (:parts
   `(:frame
     (:menu-item-list :modify
      (:item-prototype
       (:modify
	(:enabled T)
	(:active-p ,(o-formula (gvl :enabled)))
	(:get-title-fn menubar-get-title-motif)
	(:set-title-fn menubar-set-title-motif)
	)))
     :bar-list :feedback-obj-topline :feedback-obj-bottomline :sel-box)))
  
;;; Auxiliary function for MOTIF-MENUBAR's :fix-update-slots method.
;;; This function is used to establish the links between a bar-item and its
;;; submenu.  A new submenu is created and put in a new window.  This new
;;; window is added to the :submenu-window-list slot of the interactor in the
;;; top-level menubar.

(defun ATTACH-MOTIF-SUBMENU (menubar-items a-bar-item)
  ; a-bar-item might have been taken from storage, so check to see whether
  ; it has a :submenu and :submenu-window before creating these
  (let* ((new-submenu (or (g-local-value a-bar-item :submenu)
			  (create-instance NIL MOTIF-SUBMENU
			    (:bar-item a-bar-item))))
	 (win (or (g-local-value a-bar-item :submenu-window)
		  (Make-Motif-Submenu-Win a-bar-item new-submenu)))
	 (a-menubar (g-value menubar-items :parent))
	 (submenu-window-list (g-local-value a-menubar :submenu-window-list)))

    ; if a-bar-item was taken from storage, then its :submenu-window is
    ; already on the :submenu-window-list
    (unless (member win submenu-window-list)
      (s-value a-menubar :submenu-window-list
	       (cons win (copy-list submenu-window-list))))
		     
    ; bookkeeping in case a-bar-item was not taken from storage
    (s-value a-bar-item :submenu new-submenu)
    (s-value a-bar-item :submenu-window win))
  a-bar-item)


;;;    This should be called when the number of items in the menu is changed
;;; manually (without calling add-item or remove-item).  This is basically an
;;; enhanced version of the default fix-update-slots method which reuses old
;;; components.
;;;    When the menubar instance is created, aggrelists creates components
;;; for it, but this function has to be called to create submenus for the
;;; components.  If the number of :items changes, then this function should be
;;; called to both create (or destroy) new components for the aggrelist and
;;; create (or destroy) corresponding submenus.
;;;
(define-method :fix-update-slots MOTIF-MENUBAR (a-menubar)
  ;; will be called when :items or :visible change
  (let ((old-visible (g-value a-menubar :old-visible))
	(new-visible (g-value a-menubar :visible)))
    (if (eq old-visible new-visible)
	;; then items must have changed -- Generate new bar-items
	(call-prototype-method a-menubar)
	(progn ;; else visible changed
	  (s-value a-menubar :old-visible new-visible)
	  (unless new-visible
	    (Put-Down-MenuBar-Popups a-menubar))))))

(define-method :fix-update-slots MOTIF-BAR-ITEM (a-bar-item)
  (let ((a-menubar (g-value a-bar-item :parent)))
    (unless (member (g-local-value a-bar-item :submenu-window)
		    (g-local-value a-menubar :submenu-window-list))
      ;; Generate new submenu window for the bar-item
      (ATTACH-MOTIF-SUBMENU a-menubar a-bar-item))))


;;;
;;;  DEMO FUNCTIONS
;;;


#+garnet-test (defparameter *MOTIF-FONT-TO-SWAP* (create-instance NIL opal:font))
#+garnet-test (defvar motif-family-text NIL)
#+garnet-test (defvar motif-face-text NIL)
#+garnet-test (defvar motif-size-text NIL)
#+garnet-test (defvar motif-combo-text NIL)


;;; When we want to change an object's font, set the slots of *MOTIF-FONT-TO-SWAP*,
;;; then set the object to have that font.  (Opal does not notice when you
;;; just change the slots of a font.)
;;;
#+garnet-test
(defun Motif-Change-Font (text-obj &key family face size)
  (let ((old-font (g-value text-obj :font))
	(new-font *MOTIF-FONT-TO-SWAP*))
    (setf *MOTIF-FONT-TO-SWAP* old-font)
    (if family
	(s-value new-font :family family)
	(s-value new-font :family :fixed))
    (if face
	(s-value new-font :face face)
	(s-value new-font :face :roman))
    (if size
	(s-value new-font :size size)
	(s-value new-font :size :medium))
    ;; The :max-char-as/descent formulas do not depend on the slots just
    ;; modified, so you have to explicitly recompute them when reusing a font
    (recompute-formula new-font :max-char-ascent)
    (recompute-formula new-font :max-char-descent)
    (s-value text-obj :font new-font)))


;;; Some functions to call when items are selected
;;;
#+garnet-test
(defun Motif-Family-Fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-family-text slot value)
  (s-value motif-family-text :string (string-downcase value)))
#+garnet-test
(defun Motif-Face-Fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-face-text slot value)
  (s-value motif-face-text :string (string-downcase value)))
#+garnet-test
(defun Motif-Size-Fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-size-text slot value)
  (s-value motif-size-text :string (string-downcase value)))

#+garnet-test
(defun Motif-Fixed-Fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "Setting :family slot to :fixed.~%"))
#+garnet-test
(defun Motif-Serif-Fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "Setting :family slot to :serif.~%"))
#+garnet-test
(defun Motif-Sans-Serif-Fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "Setting :family slot to :sans-serif.~%"))


#+garnet-test
(defun Motif-Menubar-Go (&key dont-enter-main-event-loop)

  (create-instance 'MOTIF-MENUBAR-WIN inter:interactor-window
    (:background-color opal:motif-green)
    #-apple (:left 700)  #+apple (:left 300)
    (:top 45)(:height 360)(:width 600)
    
    (:aggregate (create-instance 'MOTIF-MENUBAR-TOP-AGG opal:aggregate)))
  (opal:update MOTIF-MENUBAR-WIN)

  (create-instance 'DEMO-MOTIF-MENUBAR MOTIF-MENUBAR
    (:foreground-color opal:motif-green)
    (:help-style t)
    (:width (kr:o-formula (kr:gvl :window :width) 600))
    (:items
     '((:family motif-family-fn
	((:fixed motif-fixed-fn)
	 (:serif motif-serif-fn)
	 (:sans-serif motif-sans-serif-fn)))
       (:face motif-face-fn
	((:roman)(:bold)(:italic)(:bold-italic)))
       (:size motif-size-fn
	((:small)(:medium)(:large)(:very-large)))
       (:help NIL
	((:help) (:ballons)(:news)(:index)))
       ))
    (:accelerators
     '((("!f" :|META-f|) ("!e" :|META-e|) ("!a" :|META-a|))
       (("!r" :|META-r|) ("!b" :|META-b|) ("!i" :|META-i|) ("!B" :META-B))
       (("!s" :|META-s|) ("!m" :|META-m|) ("!l" :|META-l|) ("!v" :|META-v|))))
    (:bar-above-these-items
     '(NIL
       NIL
       (:large)
       (:news :index)))
    (:selection-function
     #'(lambda (gadget slot value)
	 (declare (ignore gadget slot value))
	 (let ((family (g-value motif-family-text :font :family))
	       (face (g-value motif-face-text :font :face))
	       (size (g-value motif-size-text :font :size)))
	   (motif-change-font motif-combo-text
			:family family :face face :size size)))))

  (create-instance 'motif-menubar-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in DEMO-MOTIF-MENUBAR))
    (:continuous NIL)
    (:window MOTIF-MENUBAR-WIN)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send DEMO-MOTIF-MENUBAR :special-popup-func
				  DEMO-MOTIF-MENUBAR))))

  (opal:add-component MOTIF-MENUBAR-TOP-AGG DEMO-MOTIF-MENUBAR)
  (opal:update MOTIF-MENUBAR-WIN)

  (create-instance 'motif-family-text opal:text
     (:left 10)
     (:top 200)
     (:string "fixed")
     (:font (create-instance NIL opal:font)))
  (create-instance 'motif-face-text opal:text
     (:left 75)
     (:top 200)
     (:string "roman")
     (:font (create-instance NIL opal:font)))
  (create-instance 'motif-size-text opal:text
     (:left 160)
     (:top 200)
     (:string "medium")
     (:font (create-instance NIL opal:font)))
  (create-instance 'motif-combo-text opal:text
     (:left 75)
     (:top 230)
     (:string "combo")
     (:font (create-instance NIL opal:font)))

  (opal:add-components MOTIF-MENUBAR-TOP-AGG
		       motif-family-text motif-face-text motif-size-text motif-combo-text)
  
  (opal:update MOTIF-MENUBAR-WIN)
 (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
 )



;;;
;;;  MOTIF-MENUBAR-STOP
;;;

#+garnet-test
(defun Motif-Menubar-Stop ()
  (opal:destroy MOTIF-MENUBAR-WIN))


;;;
;;;  UTILITY FUNCTIONS USED BY EXPORTED FUNCTIONS
;;;

(defun Confirm-Motif-Menubar (a-menubar)
  (unless (is-a-p a-menubar MOTIF-MENUBAR)
    (error "~S is not an instance of ~S.~%" a-menubar MOTIF-MENUBAR))
  T)

(defun Confirm-Motif-Bar-Item (a-bar-item)
  (unless (is-a-p a-bar-item MOTIF-BAR-ITEM)
    (error "~S is not an instance of ~S.~%" a-bar-item MOTIF-BAR-ITEM))
  T)


(defun confirm-motif-bar-or-submenu-item (menubar-component)
  (unless (or (is-a-p menubar-component MOTIF-BAR-ITEM)
	      (is-a-p menubar-component MOTIF-MENU-ITEM))
    (error "~S is not an instance of ~S or ~S.~%" menubar-component
	   MOTIF-BAR-ITEM MOTIF-MENU-ITEM))
  T)

(defun Insert-Item-At-N (a-list item n)
  (let ((new-list (copy-list a-list))
	(len (1- (length a-list))))

    (when (< len n)
      (dotimes (i (- n len 1))
	(setf new-list
	      (append new-list (list NIL)))))
    
    (append (subseq new-list 0 n) (list item)
	  (subseq new-list n))))

(defun Remove-Item-At-N (a-list n)
  (if (< n (length a-list))
      (append (subseq a-list 0 n) (subseq a-list (1+ n)))
      a-list))
	  
;;;
;;;  EXPORTED FUNCTIONS
;;;

; Menubar Functions


;
; The parameter item may be either 
; 1) An instance of MOTIF-BAR-ITEM, or
; 2) A sublist of an :items list
;
; NOTE NOTE NOTE NOTE:  THE :ACCELERATOR KEYWORD *MUST* APPEAR BEFORE THE
; :WHEREs IF YOU WANT TO ADD ANY ACCELERATORS
;
; Locator should be a sublist of an :items list or an installed bar-item.
; Or, you can use the key feature of add-item and make locator the title of
; an installed bar-item:
;    (add-item demo-motif-menubar new-bar :after "Bar2" :key #'car)
;
; Implementation note:  The reason that we do not just set the :items list
; and let the fix-update-slots method deal with the components is that the
; item parameter can be an actual component to be added, so you should not
; generate a new component via fix-update-slots.
;
(define-method :add-item MOTIF-MENUBAR (a-menubar item &rest args)
  (let* ((a-bar-item (if (is-a-p item MOTIF-BAR-ITEM)
			 item
			 (or (let ((old-bi (pop (g-value a-menubar :storage))))
			       (when old-bi
				 (g-value old-bi :desc)  ; initialize formula
				 (s-value old-bi :desc item)
				 old-bi))
			     (Make-Motif-Bar-Item :desc item))))
	 (a-list (g-value a-bar-item :parent)) ; parent should be NIL
	 where locator key accel)

    ;; with the motif-menubar, you can specify accelerators
    ;; to be added as well.
    
    (when (eq :accelerators (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))
    
    (when a-list
      (error "~S is already installed in ~S.~%" a-bar-item a-list))
    
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    
    ; Add the bar-item as a component of the menubar
    (let* ((locator-comp (if (is-a-p locator MOTIF-BAR-ITEM)
			    locator
			    (get-bar-component a-menubar locator)))
	   (locator-desc (when locator-comp
			   (g-value locator-comp :desc)))
	   (items (copy-list (g-value a-menubar :items)))
	   (desc (g-value a-bar-item :desc))
	   (menubar-items (g-value a-menubar :menubar-items))
	   rank)

      (opal:add-local-component menubar-items
				a-bar-item where locator-comp)
      (setf rank
	    (s-value a-bar-item :rank
		     (position a-bar-item (g-value menubar-items :components))))
      (s-value (g-value a-menubar :menubar-items) :old-items
         (s-value a-menubar
		  :items
		  (opal::insert-item desc items where locator-desc key)))

    ;; Adjust the accelerators and bar-above-these-items lists to
    ;; recognize that a new bar-item has been added

    (s-value a-menubar :accelerators
	     (insert-item-at-n (g-value a-menubar :accelerators) accel rank))

    (s-value a-menubar :bar-above-these-items
	     (insert-item-at-n (g-value a-menubar :bar-above-these-items)
			       NIL rank))
    )
    
    ; Do additional bookkeeping that attaches the bar-item to the menubar
    (let* ((win (g-value a-bar-item :submenu-window))
	   (top-inter (g-value a-menubar :menubar-select))
	   (cur-wins (g-value top-inter :window)))
      ; Make sure win is destroyed along with a-menubar
      (unless (member win (g-local-value a-menubar :submenu-window-list))
	(s-value a-menubar :submenu-window-list
	       (append (list win) (g-local-value a-menubar :submenu-window-list))))
      ; Add win to the top-level interactor's :window slot
      (if (listp cur-wins)
	  (append (list win) (g-value top-inter :window))
	  (s-value top-inter :window (list win cur-wins)))
      (mark-as-changed top-inter :window))

    a-bar-item))


;
;
; The a-bar-item parameter can either be
; 1) An instance of MOTIF-BAR-ITEM, or
; 2) A sublist of the :items list
;
(define-method :remove-item MOTIF-MENUBAR (a-menubar item)
  (let ((a-bar-item (if (is-a-p item MOTIF-BAR-ITEM)
			item
			(get-bar-component a-menubar item)))
	(rank NIL))
    (unless (and a-bar-item
		 (eq a-menubar (and
				(g-value a-bar-item :parent)  ;;Check if it has a parent
				(g-value a-bar-item :parent :parent)))) ;;Get the menubar
      (error "~S does not have ~S as its menubar.~%"
	     a-bar-item a-menubar))
    (setf rank (position a-bar-item (g-value a-menubar :menubar-items :components)))

    ; Remove the appropriate accelerators
    (s-value a-menubar :accelerators
	     (remove-item-at-n (g-value a-menubar :accelerators) rank))
    ; Remove the appropriate bars
    (s-value a-menubar :bar-above-these-items
	     (remove-item-at-n (g-value a-menubar :bar-above-these-items) rank))

    ; Remove the bar-item from the menubar
    (s-value a-menubar :submenu-window-list
	     (remove (g-local-value a-bar-item :submenu-window)
		     (g-local-value a-menubar :submenu-window-list)))
    (opal:remove-local-component (g-value a-menubar :menubar-items) a-bar-item)
    (unless (eq a-bar-item item)
      (push a-bar-item (g-value a-menubar :storage)))
      
    ; Change the top-level :items list
    (let ((old-desc (if (is-a-p a-bar-item MOTIF-BAR-ITEM)
			(g-value a-bar-item :desc)
			a-bar-item)))
      (s-value (g-value a-menubar :menubar-items) :old-items
	 (s-value a-menubar :items (remove old-desc (g-value a-menubar :items)
					   :test #'equal))))))

(defun Make-Motif-Menubar ()
  (create-instance NIL MOTIF-MENUBAR))

(define-method :Menubar-Components MOTIF-MENUBAR (a-menubar)
  (g-value a-menubar :menubar-items :components))

(define-method :Set-Menubar MOTIF-MENUBAR (a-menubar new-menus)
  ; Can't use dovalues and destructive operation on :components together
  (let ((components (copy-list (Menubar-Components a-menubar))))
    (dolist (old-comp components)
      (opal:remove-item (g-value a-menubar :menubar-items) old-comp))
    (dolist (new-menu new-menus)
      (opal:add-item (g-value a-menubar :menubar-items) new-menu))))



; Bar-Item functions
;
; The item parameter can either be
; 1) An instance of MOTIF-MENU-ITEM, or
; 2) A description of a submenu-item: (list string action) where "string" is
;    a string or atom and "action" is a function
;
; NOTE NOTE NOTE NOTE:  THE :ACCELERATOR KEYWORD *MUST* APPEAR BEFORE THE
; :WHEREs IF ANY ACCELERATORS ARE TO BE ADDED

(define-method :add-item MOTIF-BAR-ITEM (a-bar-item item &rest args)
  (let ((a-menubar
	 (and
	  (g-value a-bar-item :parent)
	  (g-value a-bar-item :parent :parent)))
	(submenu (g-value a-bar-item :submenu))
	(old-desc (or (g-local-value a-bar-item :desc)
		      (copy-list (g-value a-bar-item :desc))))
	where locator key accel rank)

    ;; when adding a submenu item to a bar item, you can also
    ;; provide it with an accelerator
    (when (eq :accelerator (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))

    (multiple-value-setq (where locator key) (opal::get-wheres args))
    
    ;; item can be either a submenu-item or a list
    (if (schema-p item)
	(let* ((new-item `(,(g-value item :item-obj)
			   ,(g-value item :action)))
	       (new-sub-desc (opal::insert-item new-item (third old-desc)
						where locator key))
	       (new-desc (list (first old-desc) (second old-desc) new-sub-desc)))

	  (setf rank
		(s-value item :rank
			 (position new-item new-sub-desc :test #'equal)))
	  (if a-menubar
	      (s-value a-menubar :items
		       (substitute new-desc old-desc
				   (or (g-local-value a-menubar :items)
				       (copy-list (g-value a-menubar :items)))
				   :test #'equal))
	      (s-value a-bar-item :desc new-desc))
	  (s-value (g-value submenu :menu-item-list) :old-items new-sub-desc)
	  
	  (kr:with-constants-disabled
	      (opal:add-local-component (g-value submenu :menu-item-list)
					item :at rank))
	  )

	(let* ((new-sub-desc (opal::insert-item item (third old-desc)
						where locator key))
	       (new-desc (list (first old-desc) (second old-desc)
			       new-sub-desc)))
	  
	  (setf rank (position item new-sub-desc
			       :test #'(lambda (x y)
					 (equal x (funcall key y)))))
	  (if a-menubar
	      (s-value a-menubar
		       :items
		       (substitute new-desc old-desc
				   (or (g-local-value a-menubar :items)
				       (copy-list (g-value a-menubar :items)))
				   :test #'equal))
	      (s-value a-bar-item :desc new-desc))
	  (s-value (g-value submenu :menu-item-list) :items new-sub-desc)
	  ))

    ;; Now you have to set the :accelerators list of the top-level menubar
    ;; to understand that a new item has been added to the submenu, and if
    ;; necessary, insert the new accelerator into the list.  You do this
    ;; ONLY IF A MENUBAR EXISTS

    (when a-menubar
      (let* ((a-list (g-value a-menubar :accelerators))
	     (old-acc-list (nth (g-value submenu :bar-item :rank) a-list)))
	(when old-acc-list 
	  (let ((new-acc-list (Insert-Item-At-N old-acc-list accel rank)))
	    (s-value a-menubar :accelerators
		     (substitute new-acc-list old-acc-list a-list))
	  ))))
    ))


;
;
; The item parameter can be either
; 1) An instance of MOTIF-MENU-ITEM, or
; 2) A string or atom
;
(define-method :remove-item MOTIF-BAR-ITEM (a-bar-item &optional item
						 &key (key #'opal:no-func))
  (let* ((a-submenu-item (if (is-a-p item MOTIF-MENU-ITEM)
			     (get-submenu-component a-bar-item
							  (g-value item :item-obj))
			     (get-submenu-component a-bar-item item)))
	 (submenu (g-value a-bar-item :submenu))
	 (Submenu-Components (g-value submenu :menu-item-list :components))
	 (rank (if item
		   (position a-submenu-item Submenu-Components
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (length Submenu-Components)))))
    
    (unless rank
      (error "~S does not have ~S as its bar-item.~%"
	     a-submenu-item a-bar-item))
    ; If the user did not supply an item, then just remove the last component
    (unless a-submenu-item
      (setf a-submenu-item (nth rank Submenu-Components)))

    (opal:remove-local-component (g-value a-submenu-item :parent) a-submenu-item)

    ; Update the :items or :desc list
    (let* ((a-menubar (and
		       (g-value a-bar-item :parent)
		       (g-value a-bar-item :parent :parent)))
	   (old-desc (g-value a-bar-item :desc))
	   (old-sub-desc (third old-desc))
	   (item-obj (g-value a-submenu-item :item-obj))
	   (action (g-value a-submenu-item :action))
	   (new-sub-desc (remove (if action
				     (list item-obj action)
				     (list item-obj))
				 old-sub-desc :test #'equal))
	   (new-desc (substitute new-sub-desc old-sub-desc old-desc
				 :test #'equal)))

      (if a-menubar
	  (s-value (g-value submenu :menu-item-list) :old-items
	     (s-value a-menubar :items
		      (substitute new-desc old-desc (g-value a-menubar :items)
				  :test #'equal)))
	  (s-value a-bar-item :desc new-desc))
      (s-value (g-value submenu :menu-item-list) :old-items new-sub-desc)
      
      ;; Now you have to remove the accelerator from the menubar
      (when a-menubar
	(let* ((accels (g-value a-menubar :accelerators))
	       (a-list (nth (g-value a-bar-item :rank) accels)))
	  (when a-list
	    (let ((new-a-list (remove-item-at-n a-list rank)))
	      (s-value a-menubar :accelerators
		       (substitute new-a-list a-list accels))))))
      )))

(s-value MOTIF-MENUBAR :change-item (g-value opal:aggrelist :change-item))
(s-value MOTIF-MENUBAR :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))
(s-value MOTIF-BAR-ITEM :change-item #'menubar-change-item)
(s-value MOTIF-BAR-ITEM :remove-nth-item (g-value opal:aggrelist :remove-nth-item))
(s-value MOTIF-BAR-ITEM :set-submenu-fn #'set-submenu-fn)
(s-value MOTIF-MENUBAR :get-bar-component-fn #'get-bar-component-fn)


;; Returns an instance of a motif-bar-item

(defun Make-Motif-Bar-Item (&key desc font title)
  ;; Type-check the argument before creating an object for it.
  (when desc (Check-Bar-Item desc))
  (let* ((new-bar-item (create-instance NIL MOTIF-BAR-ITEM))
	 (new-submenu (create-instance NIL MOTIF-SUBMENU
			 (:bar-item new-bar-item)
			 (:font (or font opal:default-font))))
	 (win (Make-Motif-Submenu-Win new-bar-item new-submenu)))
    (s-value new-bar-item :submenu new-submenu)
    (s-value new-bar-item :submenu-window win)
    ; Put in initial value of :desc slot
    (g-value new-bar-item :desc) ; to initialize the default formula
    (if desc
	(s-value new-bar-item :desc desc)
	(s-value new-bar-item :desc (list title NIL NIL)))
    new-bar-item))

;; Returns the title of a menubar-component, which could
;; be a bar-item or a submenu-item

(defun Menubar-Get-Title-Motif (menubar-component)
  (if (is-a-p menubar-component MOTIF-BAR-ITEM)
      (g-value menubar-component :menu-obj)
      (g-value menubar-component :item-obj)))

(s-value MOTIF-BAR-ITEM :get-title-fn #'menubar-get-title-motif)

; If the menubar-component is installed in a menubar, then the :items list is
; changed.  Otherwise, the object's :desc slot is set.
;
(defun Menubar-Set-Title-Motif (menubar-component string)
  (cond

    ;; The parameter is a MOTIF-BAR-ITEM
    ((is-a-p menubar-component MOTIF-BAR-ITEM)
     (let* ((a-bar-item menubar-component)
	    (a-menubar (and
			(g-value a-bar-item :parent)
			(g-value a-bar-item :parent :parent))))
       (cond
	 (a-menubar
	  ; a-bar-item is installed, so set the :items list
	  (rplaca (find (g-value a-bar-item :desc)
			(g-value a-menubar :items) :test #'equal)
		  string)
	  (mark-as-changed a-menubar :items))
	 
	 (t
	  ; not installed, so just set local :desc list
	  (rplaca (g-value a-bar-item :desc) string)
	  (mark-as-changed a-bar-item :desc)))))

    ;; The parameter is a MOTIF-MENU-ITEM
    ((or
      (is-a-p menubar-component MOTIF-MENU-ITEM))
     (let* ((a-submenu-item menubar-component)
	    (a-submenu-agg (g-value menubar-component :parent :parent)))
       (cond
	 (a-submenu-agg
	  ; a-submenu-item is installed in a bar-item
	  (let* ((a-bar-item (g-value a-submenu-agg :bar-item))
		 (a-menubar (when a-bar-item
			      (g-value a-bar-item :parent :parent))))
	    (cond

	      ; a-submenu-item is installed in a menubar
	      (a-menubar
	       (let* ((old-desc (g-value a-bar-item :desc))
		      (old-items-desc
		       (find old-desc (g-value a-menubar :items))))
		 (dolist (desc (third old-items-desc))
		   (when (and (equal (first desc)
				     (g-value a-submenu-item :item-obj))
			      (equal (second desc)
				     (g-value a-submenu-item :action)))
		     (rplaca desc string)
		     (mark-as-changed a-menubar :items)))))

	      ; a-submenu-item is not installed in a menubar
	      (t
	       (let* ((old-desc (g-value a-bar-item :desc)))
		 (dolist (desc (third old-desc))
		   (when (and (equal (first desc)
				     (g-value a-submenu-item :item-obj))
			      (equal (second desc)
				     (g-value a-submenu-item :action)))
		     (rplaca desc string)
		     (mark-as-changed a-bar-item :items))))))))

	 ; a-submenu-item is not installed in a bar-item
	 (t
	  (g-value a-submenu-item :desc)
	  (s-value a-submenu-item :desc (list string))))))
	  
    ; Else, print error message
    (t (error "~S is not an instance of ~S or ~S.~%" menubar-component
	   MOTIF-BAR-ITEM MOTIF-MENU-ITEM)))
  string)

(s-value MOTIF-BAR-ITEM :set-title-fn #'menubar-set-title-motif)

(define-method :Submenu-Components MOTIF-BAR-ITEM (a-bar-item)
  (g-value a-bar-item :submenu :menu-item-list :components))

;
; The args parameter will (optionally) include the where, locator, and key
; parameters that are sent to add-item.  The where refers to the placement
; of the new submenu item among the current submenu items within b-item.
;
(define-method :Add-Submenu-Item MOTIF-MENUBAR (a-menubar b-item s-item &rest args)
  (let* ((a-bar-item (if (is-a-p b-item MOTIF-BAR-ITEM)
			 b-item
			 (get-bar-component a-menubar b-item)))
	 where locator key accel)
    (when (eq :accelerator (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))

    (multiple-value-setq (where locator key) (opal::get-wheres args))
    (opal:add-item a-bar-item s-item :accelerator accel where locator key)))

;
; After looking up the b-item to get a bar-item object, call opal:remove-item.
;
(define-method :Remove-Submenu-Item MOTIF-MENUBAR (a-menubar b-item s-item)
  (let* ((a-bar-item (if (is-a-p b-item MOTIF-BAR-ITEM)
			 b-item
			 (get-bar-component a-menubar b-item))))
    (opal:remove-item a-bar-item s-item)))

; new-desc can have two forms:
;   1) A list of MOTIF-MENU-ITEM instances, or
;   2) A list of MOTIF-MENU-ITEM descriptions, such as
;      ((item1 action1) (item2) (item3 action3)))
;

;; Gets the submenu-component corresponding to a given title in a
;; bar-item

(define-method :Get-Submenu-Component MOTIF-BAR-ITEM (a-bar-item item)
  (find-if #'(lambda (a-submenu-item)
	       (if (listp item)
		   (equal item (list (g-value a-submenu-item :item-obj)))
		   (equal item (g-value a-submenu-item :item-obj))
		   ))
	   (Submenu-Components a-bar-item)))

;; Finds a submenu component, given a menubar, the title of the bar-item,
;; and the title of the submenu-item

(define-method :Find-Submenu-Component MOTIF-MENUBAR (a-menubar submenu-title submenu-item)
  (let ((a-bar-item (if (is-a-p submenu-title MOTIF-BAR-ITEM)
			submenu-title
			(get-bar-component a-menubar submenu-title))))
    (get-submenu-component a-bar-item submenu-item)))


; Submenu-item functions

;
; This function returns an instance of MOTIF-MENU-ITEM.  If the :desc key is
; supplied, then the accompanying parameter should be the string/function or
; atom/function pair that describes a submenu-item.
;

(defun Make-Motif-Submenu-Item (&key desc (enabled T))
  (let ((new-submenu-item (create-instance NIL MOTIF-MENU-ITEM
			    (:desc NIL)
			    (:item-obj (o-formula (first (gvl :desc))))
			    (:action (o-formula (second (gvl :desc))))
			    (:enabled T)
			    (:active-p (o-formula (gvl :enabled)))
			    (:get-title-fn #'menubar-get-title-motif)
			    (:set-title-fn #'menubar-set-title-motif)
			    (:parts
			     `((:item-text ,MOTIF-MENU-TEXT-LABEL-PROTOTYPE)
			       :accel-text
			       :underline)))))
    (unless (listp desc)
      (error "Expected a list description of a submenu-item, but got ~S.~%"
	     desc))
    (when desc
      (s-value new-submenu-item :desc desc))
    (s-value new-submenu-item :enabled enabled)
    new-submenu-item))



;;;
;;;  Methods for editing the strings of the menubar and submenus
;;;

(define-method :new-item-label gg::MOTIF-MENUBAR (obj)
  (let ((val (1+ (or (g-value obj :last-menubar-label-used)
		     (length (g-value obj :items))))))
    (s-value obj :last-menubar-label-used val)
    (list (format NIL "Label~a" val) NIL '(("sub-item1")))))

(define-method :new-item-label gg::MOTIF-BAR-ITEM (obj)
  (let ((val (1+ (or (g-value obj :last-bar-item-label-used)
		     (length (g-value obj :items))))))
    (s-value obj :last-bar-item-label-used val)
    (list (format NIL "Label~a" val))))

(define-method :string-set-func gg::MOTIF-MENUBAR
  (gadget-obj str-obj final-event final-string)
  ;; First test if we are editing the string of a bar-item
  (let ((a-bar-item (g-value str-obj :parent)))
    (if (is-a-p a-bar-item gg::MOTIF-BAR-ITEM)
	(let* ((agglist (g-value gadget-obj :menubar-items))
	       (old-desc (g-value a-bar-item :desc))
	       (new-desc (push final-string (cdr (copy-tree old-desc)))))
	  (Put-Down-MenuBar-Popups gadget-obj)
	  (opal::Aggrelist-Edit-String-Func gadget-obj agglist str-obj
					    final-event new-desc :rank))
	;; It wasn't the string of a bar-item.  It was a submenu item.
	(let ((agglist (g-value str-obj :parent :parent)))
	  (setf a-bar-item (g-value agglist :parent :bar-item))
	  (opal::Aggrelist-Edit-String-Func a-bar-item agglist str-obj
					    final-event (list final-string)
					    :rank))
	)))

;;; RGA (Thien)
;;; allow specifying the char accel
(kr:s-value gg::MOTIF-SUBMENU :accelerators
  (kr:o-formula
   (let ((bar-acc (kr:gvl :bar-item :accelerators))
	 menu-acc temp )
     (when bar-acc
       (dolist (a-list bar-acc)
	 (setf temp nil)
	 (when (second a-list)
	   (setf temp (cons (second a-list) temp)))
	 (when (first a-list)
	   (setf temp (cons (first a-list) temp)))
	 (setf temp (cons (third a-list) temp))
	 (setf menu-acc
	   (append menu-acc (list temp)))))
     menu-acc)))

(defun gw-char-equal (c1 c2)
  (and (characterp c1)
       (characterp c2)
       (char-equal c1 c2)))

;;; RGA (Thien)
;;; accel char should be case insensitive
(kr:s-value MOTIF-MENU-ACCEL-INTER :final-function
  #'(lambda (interactor obj)
      (declare (ignore obj))
      (let* ((gadget (g-value interactor :operates-on))
	     (char (inter:event-char inter:*Current-Event*))
	     (accel-chars (g-value gadget :local-accel-chars))
	     (rank (position char accel-chars :test #'gw-char-equal)))
	(when (and rank (g-value gadget :keyboard-selection-p))
	  (let* ((selection (nth rank
				 (g-value gadget :menu-item-list :components)))
		 (action (g-value selection :action))
		 (prev-sel (g-value gadget :value-obj)))
		   
	    (when (g-value (nth rank (g-value gadget :menu-item-list :components))
			   :active-p)

	      ;; Propagate new selection toward :value slot
	      (s-value gadget :value-obj selection)
	      (s-value selection :selected T)
	      (if (and prev-sel (not (eq prev-sel selection)))
		  (s-value prev-sel :selected NIL))

	      ;; Make interim feedback flash if no final-feedback
	      (unless (g-value gadget :final-feedback-p)
		(s-value selection :interim-selected T)
		(opal:update (g-value gadget :window))
		(sleep .25)
		(s-value selection :interim-selected NIL))
		     
	      ;; Global function for all items
	      (kr-send gadget :selection-function gadget selection)
		    
	      ;; Local function assigned to item.
	      ;; If this is in a menubar, you have to call the item
	      ;; function with THREE arguments. Otherwise, with 2 args
		
	      (when action
		(if (AND (g-value gadget :bar-item)
			 (boundp 'MOTIF-BAR-ITEM)
			 (is-a-p (g-value gadget :bar-item) MOTIF-BAR-ITEM))
		    (funcall action
			     (g-value gadget :bar-item :parent :parent)  ;; The menubar
			     (g-value gadget :bar-item :menu-obj)  ;; The bar-item
			     (g-value selection :item-obj))        ;; The item
		  (funcall action gadget (g-value selection :item-obj))))))))))




