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
;;;  Pull Down Menus
;;;
;;;
;;;  Customizable slots:
;;;    1)  Left, top
;;;    2)  Title-font, item-font
;;;    4)  Selection-function - a function to be executed when any item is
;;;           selected.  Takes the parameters (gadget menu-item submenu-item).
;;;    5)  Items - A list with the format
;;;           '(("m1" m1func ("m1,1"..."m1,N"))
;;;             ("m2" m2func (("m2,1" m2,1func)...("m2,N" m2,Nfunc)))
;;;             ...)
;;;           Where "mN" is a string or atom that is the title of a menu,
;;;        "mX,Y" is a string or atom in menu X, row Y,
;;;        mNfunc is executed when any item in menu N is selected,
;;;        mX,Yfunc is executed when item mX,Y is selected.
;;;           These two functions take the same parameters as the
;;;        selection-function.
;;;
;;;  Programming interface (Garnet way):
;;;    1) Create an instance of menubar with a value for the :items slot
;;;    2) Use opal:add-component to put the instance in a window
;;;
;;;  Caveats:
;;;     New bar-items should be created with the :enable slot set to NIL in
;;;  order to keep their windows from being updated before they are added to
;;;  a menubar.

;;;  Pull Down Menus Demo:
;;;    The function below creates a window containing a pull down menu and
;;;    some text objects.  Choosing an item from the pull-down menu will
;;;    change the font of the text objects.
;;;    To run it, enter (GARNET-GADGETS:menubar-go).
;;;    To stop, enter (GARNET-GADGETS:menubar-stop).
;;;
;;;  Written by Pavan Reddy and Andrew Mickish

;;;
;;; CHANGE LOG:
;;;
;;; 12/06/94  Andrew Mickish - Added :modal-p formula to pop-up windows
;;; 06/22/93  Andrew Mickish - Set :rank slot in :add-item methods
;;; 06/15/93  Andrew Mickish - Called Check-Bar-Item in Make-Bar-Item;
;;;             Added :do-not-dump-slots for BAR-ITEM; Used g-local-value
;;;             instead of g-value when referencing :submenu-window slots
;;;             Added :fix-update-slots method for BAR-ITEM
;;; 06/11/93  Andrew Mickish - Removed references to :number-of-comps
;;; 05/13/93  Andrew Mickish - :prev-visible ---> :prev
;;; 04/23/93  Andrew Mickish - Added :do-not-dump-slots
;;; 04/18/93  Andrew Mickish - Removed ,@ from BAR-ITEM's :add-item method
;;; 04/13/93  Andrew Mickish - Fixed bugs in change of 03/08/93
;;; 03/17/93  Brad Myers - add special-popdown functions for gilt, etc.
;;; 03/08/93  Andrew Mickish - Fixed :left and :top formulas of submenu-windows
;;;             so that offset is correct when menubar is in a subwindow;
;;;             Type check :items with Check-Menubar-Items
;;; 03/04/93  Andrew Mickish - Made :string-set-func work with BAR-ITEMs
;;; 02/24/93  Andrew Mickish - Added :string-set-func
;;; 02/23/93  Andrew Mickish - Eliminated need to call notice-items-changed
;;;             on the menubar (but apparently must be called internally on
;;;             new submenus... Argh!)
;;; 02/08/93  Andrew Mickish - :notice-items-changed ---> :fix-update-slots
;;; 01/07/93  Rajan Parthasarathy - Converted the exported functions
;;;             to methods.  This way, you can call the same "function"
;;;             on both this menubar and the motif-menubar.  Also fixed
;;;             two bugs.
;;; 12/14/92  Andrew Mickish - Added type and parameter declarations
;;; 09/25/92  Andrew Mickish - Removed *** HACK ***'s because fixed update
;;; 09/21/92  Andrew Mickish - Now all submenu window bookkeeping is done in
;;;             MENUBAR's :submenu-window-list instead of interactor's :window.
;;; 08/24/92  Andrew Mickish - Added update calls to get around update bug
;;;             (see *** HACK *** comments)
;;; 07/22/92  Brad Vander Zanden - Changed two nconc's to append's
;;; 06/26/92  Andrew Mickish - Rewrote :notice-items-changed, :add-item, and
;;;             :remove-item methods so objects can appear in the :items list
;;; 06/22/92  Ed Pervin - It is necessary to call notice-items-changed on
;;;		menubars during the execution of opal:reconnect-garnet.
;;; 05/22/92  Brad Myers - new way to use menu-interactor that uses multiple
;;;                        windows.
;;; 04/15/92  Andrew Mickish - Gave BAR-ITEM text a white line style and put
;;;             it on top of the "cover"
;;; 04/14/92  Andrew Mickish - Added schema-p check in final-function so you
;;;             can destroy the menubar in an item function
;;; 02/19/92  Andrew Mickish - Fixed reuse of old bar-items by adding a push
;;;             in the remove-item method and a pop in the add-item method
;;; 06/10/91  Ed Pervin - Call opal:raise-window on subwindows so
;;;		they won't be covered by main window in twm.
;;; 05/15/91  Andrew Mickish - Added defvar's
;;; 05/01/91  Andrew Mickish - Put in Garnet-Gadgets package



(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(MENUBAR BAR-ITEM SUBMENU SUBMENU-ITEM MAKE-SUBMENU-WIN
	    ;; Creation Functions
	    Make-Menubar Make-Bar-Item Make-Submenu-Item))
  ;; Demo things
  #+garnet-test
  (export '(MENUBAR-GO MENUBAR-STOP DEMO-MENUBAR MENUBAR-WIN MENUBAR-TOP-AGG)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; First function is to display the submenu from gilt, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns the new window if and only if a different bar-item is to be
;; displayed.  Gadget parameter is the top level menubar.
(defun Garnet-Menubar-popup-item (gadget)
  (let (bar-item)
    ;; first, find the MOTIF-BAR-ITEM that is under the mouse
    (setq bar-item (opal:point-to-component gadget
				      (inter:event-x inter:*current-event*)
				      (inter:event-y inter:*current-event*)))
    (when bar-item
      (DoSpecialPopUpMenubar gadget bar-item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(create-instance 'MENUBAR-TEXT-LABEL-PROTOTYPE opal:text
  (:left (o-formula
	  (case (gvl :parent :h-align)
	    (:left (+ (gvl :parent :left) (gvl :parent :text-offset)))
	    (:center (opal:gv-center-x-is-center-of (gvl :parent)))
	    (:right (- (+ (gvl :parent :left) (gvl :parent :width))
		       (gvl :width) (gvl :parent :text-offset))))))
  (:top (o-formula (+ (gvl :parent :top) (gvl :parent :text-offset))))
  (:string (o-formula (gvl :parent :string)))
  (:font (o-formula (gvl :parent :font))))


(defun Menubar-Get-Label (agg)
  (let ((alist (g-value agg :parent))
	(item-guess (g-value agg :item-guess)))
    (if (or alist item-guess)
	(let ((item (if alist
			(nth (g-value agg :rank)
			     (g-value alist :items))
			;; Hack to create stand-alone submenu-items
			item-guess
			))
	      ;; Have to have a globally defined text-label-prototype (i.e.,
	      ;; not a slot in the SUBMENU) because submenu components can be
	      ;; created stand-alone (i.e., unable to reference the SUBMENU).
	      (text-label-prototype MENUBAR-TEXT-LABEL-PROTOTYPE))
	  ;; Don't forget that item functions are allowed!
	  (if (consp item) (setq item (first item)))
	  ;; Don't forget that menus have item conversion functions!
	  (if (and alist (g-value alist :item-to-string-function))
	      (setf item (kr-send alist :item-to-string-function item)))
	  (cond
	    ((schema-p item)
	     (let ((new-label (if (g-value item :parent)
				  ;; The item has been used already --
				  ;; Use it as a prototype
				  (create-instance NIL item)
				  ;; Use the item itself
				  item))
		   (leftform (get-value text-label-prototype :left))
		   (topform (get-value text-label-prototype :top)))
	       ;; Automatically set the :left and :top of the label
	       (unless (is-a-p (get-local-value item :left) leftform)
		 (s-value new-label :left (formula leftform)))
	       (unless (is-a-p (get-local-value item :top) topform)
		 (s-value new-label :top (formula topform)))
	       new-label))
	    (t (create-instance NIL text-label-prototype))))
	;; Give the item-prototype a bogus part
	(create-instance NIL opal:null-object))))


;; This is an item in the vertical menus that appear below the menu bar.
;;
(create-instance 'SUBMENU-ITEM opal:aggregadget
   ; The mnemonic ":desc" is a description of a submenu.  The top-level :items
   ; slot is a list of desc's.
   (:desc (o-formula (nth (gvl :rank) (gvl :parent :items))))
   (:item-obj (o-formula (first (gvl :desc))))
   (:action (o-formula (second (gvl :desc))))
   (:string (o-formula (let ((item-obj (gvl :item-obj)))
			 (if (stringp item-obj)
			     item-obj
			     (string-capitalize
			      (string-trim ":" item-obj))))))
   (:text-offset (o-formula (gvl :parent :text-offset)))
   (:enabled (o-formula (if (= 4 (length (gvl :desc)))
			    (fourth (gvl :desc)) T)))
   (:font (o-formula (gvl :parent :font)))
   (:text-offset2 (o-formula (gvl :parent :text-offset2)))
   (:h-align (o-formula (gvl :parent :h-align)))
   (:max-text-width-thus-far
    (o-formula (if (gvl :prev)
		      (MAX (gvl :prev :max-text-width-thus-far)
			   (gvl :text :width))
		      (gvl :text :width))))
   (:height (o-formula (+ (gvl :text :height) (gvl :text-offset2))))
   (:width (o-formula (+ (gvl :parent :tail :max-text-width-thus-far)
			      (gvl :text-offset2))))
   ;; parent is a submenu
   (:bar-item (o-formula (gvl :parent :bar-item)))
   (:parts
    `((:text ,#'Menubar-Get-Label)
      (:cover ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :text :left)))
	     (:top ,(o-formula (gvl :parent :text :top)))
	     (:height ,(o-formula (- (gvl :parent :height)
				     (gvl :parent :text-offset2))))
	     (:width ,(o-formula (- (gvl :parent :width)
				    (gvl :parent :text-offset2))))
	     (:visible ,(o-formula (or (not (gvl :parent :enabled))
				       (gvl :parent :interim-selected))))
	     (:draw-function ,(o-formula (if (gvl :parent :enabled)
					    :xor :and)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula (if (gvl :parent :enabled)
					    opal:black-fill
					    opal:gray-fill)))))))


;; Menu that pops up when you click on the menubar
;; A window for this object is created in Attach-Submenu
;;
(create-instance 'SUBMENU opal:aggrelist
   (:left 0)
   (:top 0)
   (:height (o-formula (let ((total-height 0))
			 (dolist (c (gvl :components))
			   (incf total-height (g-value c :height)))
			 total-height)))
   (:h-align :left)
   (:text-offset 5)
   (:text-offset2 6)
   (:v-spacing 0)
   (:font NIL)  ; Set during fix-update-slots or add-item
   (:my-baritem NIL)
   (:enabled-items
    (o-formula (let ((enabled-items NIL))
		 (dovalues (component :self :components
				      :in-formula T
				      :result enabled-items)
		    (when (gv component :enabled)
		      (push component enabled-items))))))
   (:items (o-formula (gvl :bar-item :items)))
   (:item-prototype submenu-item)
   )

;; Returns a window that an instance of SUBMENU can be put into
;;
(defun MAKE-SUBMENU-WIN (a-bar-item a-submenu)
  (create-instance NIL inter:interactor-window
     (:bar-item a-bar-item)
     (:aggregate a-submenu)
     (:omit-title-bar-p T)
     (:save-under T)
     (:double-buffered-p T)  
     (:visible NIL)
     (:modal-p (o-formula (gv-local :self :bar-item :window :modal-p)))
     (:height (o-formula (max 1 (gvl :aggregate :height))))
     (:width (o-formula
	      (max 1
		   (+ (gvl :aggregate :text-offset) (gvl :aggregate :width)))))
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
		     (gv win :left-border-width) left-offset))
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
		  (+ (gv bar-item :top) (gv bar-item :height) (gv win :top)
		     (gv win :top-border-width) top-offset))
		 (t 0)))))
     ))


;; This object is a text field in the menu bar covered by a black rectangle.
;; An aggrelist of these items makes up the menu bar.
;;
(create-instance 'BAR-ITEM opal:aggregadget
   (:width (o-formula (+ (* 2 (gvl :spacing)) (gvl :text :width))))
   (:height (o-formula (gvl :text :height)))
   ; The mnemonic ":desc" is a description of a submenu.  The top-level :items
   ; slot is a list of desc's.
  
   (:desc (o-formula (nth (gvl :rank) (gvl :parent :items))))
   (:menu-obj (o-formula (first (gvl :desc))))
   (:action (o-formula (second (gvl :desc))))
   (:items (o-formula (third (gvl :desc))))
   (:string (o-formula (let ((menu-obj (gvl :menu-obj)))
			 (if (stringp menu-obj)
			     menu-obj
			     (string-capitalize
			      (string-trim ":" menu-obj))))))
   (:font (o-formula (gvl :parent :title-font)))
   (:spacing 5)
   (:enabled (o-formula (if (= 4 (length (gvl :desc)))
			    (fourth (gvl :desc))
			    T)))
   ;; slot :submenu filled by :fix-update-slots with a SUBMENU
   ;; slot :submenu-window set with the window of the submenu

   (:parts
    `((:cover ,opal:rectangle
	     (:left ,(o-formula (gvl :parent :left)))
	     (:top ,(o-formula (gvl :parent :top)))
             (:width ,(o-formula (gvl :parent :width)))
             (:height ,(o-formula (gvl :parent :height)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (if (gvl :parent :enabled)
				   (if (gvl :parent :interim-selected)
				       opal:white-fill
				       opal:black-fill)
				   opal:gray-fill))))
      (:text ,opal:text
       (:left ,(o-formula (+ (gvl :parent :left) (gvl :parent :spacing))))
       (:top ,(o-formula (gvl :parent :top)))
       (:font ,(o-formula (gvl :parent :font)))
       (:string ,(o-formula (string (gvl :parent :string))))
       (:line-style ,(o-formula (if (gvl :parent :interim-selected)
				    opal:default-line-style
				    opal:white-line)))))))


(create-instance 'MENUBAR opal:aggrelist
   :declare ((:parameters :left :top :items :title-font :item-font
			  :selection-function)
	     (:type ((satisfies Check-Menubar-Items) :items)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
			 :title-font :item-font)
		    ((or null function symbol) :selection-function)))
   ; Customizable slots
   (:left 0)(:top 0)
   (:items NIL)
   (:title-font opal:default-font)
   (:item-font opal:default-font)

   ; Internal slots
   (:h-spacing 0)
   (:direction :horizontal)
   (:item-prototype BAR-ITEM)
   (:selection-function NIL)
   (:enabled-items
    (o-formula (let ((enabled-items NIL))
		 (dovalues (component :self :components
				      :in-formula T
				      :result enabled-items)
		    (when (gv component :enabled)
		      (push component enabled-items))))))
   (:list-of-all-enabled-objects
    (o-formula (let ((l (copy-list (gvl :enabled-items))))
		 (dolist (baritems (gvl :enabled-items))
		   (setf l (append l (copy-list
				     (gv-local baritems :submenu :enabled-items)))))
		 l)))
   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:destroy-me 'MENUBAR-DESTROY)
   (:special-popup-func 'Garnet-Menubar-popup-item)
   (:special-popdown-func 'Put-Down-MenuBar-Popups)

   (:old-visible T)

   ;; slot :submenu-window-list set with list of all windows being used.
   (:interactors
    `((:menubar-select ,inter:menu-interactor
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
	     (when (is-a-p prev BAR-ITEM)
	       (s-value prev :interim-selected T))))
       (:running-action
	,#'(lambda (inter prev new)
	     (let* ((prev-baritem (g-value inter :prev-baritem))
		    (new-is-bar (is-a-p new BAR-ITEM))
		    (new-baritem (when new
				   (if new-is-bar
				     new
				     ;; else is a sub-item, get its bar-item
				     (g-local-value new :bar-item)))))
	       (call-prototype-method inter prev new)
	       ;; keep the interim selected of the bar item so it
	       ;; shows up as highlighted when items in sub-menu selected.
	       (if new-baritem
		   ;; this makes the subwindow NOT go away when move
		   ;; outside subwindow
		   (unless (eq new-baritem prev-baritem)
		     (s-value inter :prev-baritem new-baritem)
		     (if prev-baritem
			 (let ((win (g-local-value prev-baritem :submenu-window)))
			   (s-value prev-baritem :interim-selected NIL)
			   (s-value win :visible NIL)
			   (opal:update win)))
		     (if new-baritem
			 (let ((win (g-local-value new-baritem :submenu-window)))
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
		 (s-value prev-baritem :interim-selected NIL)
		 (let ((win (g-local-value prev-baritem :submenu-window)))
		   (s-value inter :prev-baritem NIL)
		   (s-value win :visible NIL)
		   (opal:update win))))))
       (:final-function
	,#'(lambda (inter obj)
	     (let* ((is-bar (is-a-p obj BAR-ITEM))
		    (baritem (if is-bar obj
				 ;; else is a sub-item, get its bar-item
				 (g-value obj :bar-item)))
		    (bar-action (g-value baritem :action))
		    (bar-obj (g-value baritem :menu-obj))
		    (item-action (unless is-bar (g-value obj :action)))
		    (item-obj (unless is-bar (g-value obj :item-obj)))
		    (gadget (g-value inter :operates-on))
		    (prev-baritem (g-value inter :prev-baritem))
		    )
	       (when baritem
		 (s-value baritem :interim-selected NIL)
		 (let ((win (g-local-value baritem :submenu-window)))
		   (when win
		     (s-value win :visible NIL)
		     (opal:update win))))
	       (when prev-baritem
		 (s-value inter :prev-baritem NIL))
	       (if bar-action
		   (funcall bar-action gadget bar-obj item-obj))
	       ;; The schema-p check allows you to destroy the menubar gadget
	       ;; in the menu-action or item-action and avoid passing the
	       ;; destroyed object to KR-SEND
	       (if (and item-action
			(schema-p gadget))
		   (funcall item-action gadget bar-obj item-obj))
	       (if (schema-p gadget)
		   (kr-send gadget :selection-function
			    gadget bar-obj item-obj)))))
       ))))

(s-value MENUBAR :do-not-dump-slots
	 (append '(:submenu-window-list)
		 (g-value MENUBAR :do-not-dump-slots)))
(s-value BAR-ITEM :do-not-dump-slots
	 (append '(:submenu :submenu-window)
		 (g-value BAR-ITEM :do-not-dump-slots)))
(s-value SUBMENU :do-not-dump-slots
	 (append '(:bar-item)
		 (g-value BAR-ITEM :do-not-dump-slots)))
(let ((menubar-select (g-value MENUBAR :menubar-select)))
  (s-value menubar-select :do-not-dump-slots
	   (append '(:prev-baritem)
		   (g-value menubar-select :do-not-dump-slots))))


;;; Auxiliary function for MENUBAR's :fix-update-slots method
;;;    This function is used to establish the links between a bar-item and its
;;; submenu.  A new submenu is created and put in a new window.  This new
;;; window is added to the :submenu-window-list slot of the interactor in the
;;  top-level menubar.
(defun ATTACH-SUBMENU (a-menubar a-bar-item)
  ; a-bar-item might have been taken from storage, so check to see whether
  ; it has a :submenu and :submenu-window before creating these
  (let* ((new-submenu (or (g-local-value a-bar-item :submenu)
			  (create-instance NIL SUBMENU
			     (:bar-item a-bar-item)
			     (:font (g-value a-menubar :item-font)))))
	 (win (or (g-local-value a-bar-item :submenu-window)
		  (Make-Submenu-Win a-bar-item new-submenu))))
      ; if a-bar-item was taken from storage, then its :submenu-window is
      ; already on the :submenu-window-list
      (s-value a-menubar :submenu-window-list
	       (append (list win)
		       (g-local-value a-menubar :submenu-window-list)))
      ; bookkeeping in case a-bar-item was not taken from storage
      (s-value a-bar-item :submenu new-submenu)
      (s-value a-bar-item :submenu-window win))
  a-bar-item)


;;;    This should be called when the number of items in the menu is changed
;;; manually (without calling add-item or remove-item).  This is basically an
;;; enhanced version of the default fix-update-slots which reuses old
;;; components.
;;;    When the menubar instance is created, aggrelists creates components
;;; for it, but this function has to be called to create submenus for the
;;; components.  If the number of :items changes, then this function should be
;;; called to both create (or destroy) new components for the aggrelist and
;;; create (or destroy) corresponding submenus.
;;;
(define-method :fix-update-slots MENUBAR (a-menubar)
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

(define-method :fix-update-slots BAR-ITEM (a-bar-item)
  (let ((a-menubar (g-value a-bar-item :parent)))
    (unless (member (g-local-value a-bar-item :submenu-window)
		    (g-local-value a-menubar :submenu-window-list))
      ;; Generate new submenu window for the bar-item
      (ATTACH-SUBMENU a-menubar a-bar-item))))



;;;
;;;  DEMO FUNCTIONS
;;;


#+garnet-test (defparameter *FONT-TO-SWAP* (create-instance NIL opal:font))
#+garnet-test (defvar family-text NIL)
#+garnet-test (defvar face-text NIL)
#+garnet-test (defvar size-text NIL)
#+garnet-test (defvar combo-text NIL)


;;; When we want to change an object's font, set the slots of *FONT-TO-SWAP*,
;;; then set the object to have that font.  (Opal does not notice when you
;;; just change the slots of a font.)
;;;
#+garnet-test
(defun Change-Font (text-obj &key family face size)
  (let ((old-font (g-value text-obj :font))
	(new-font *FONT-TO-SWAP*))
    (setf *FONT-TO-SWAP* old-font)
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
(defun Family-Fn (gadget slot value)
  (declare (ignore gadget))
  (change-font family-text slot value)
  (s-value family-text :string (string-downcase value)))
#+garnet-test
(defun Face-Fn (gadget slot value)
  (declare (ignore gadget))
  (change-font face-text slot value)
  (s-value face-text :string (string-downcase value)))
#+garnet-test
(defun Size-Fn (gadget slot value)
  (declare (ignore gadget))
  (change-font size-text slot value)
  (s-value size-text :string (string-downcase value)))

#+garnet-test
(defun Fixed-Fn (gadget slot value)
  (declare (ignore gadget slot value))
  (format t "Setting :family slot to :fixed.~%"))
#+garnet-test
(defun Serif-Fn (gadget slot value)
  (declare (ignore gadget slot value))
  (format t "Setting :family slot to :serif.~%"))
#+garnet-test
(defun Sans-Serif-Fn (gadget slot value)
  (declare (ignore gadget slot value))
  (format t "Setting :family slot to :sans-serif.~%"))


#+garnet-test
(defun Menubar-Go (&key dont-enter-main-event-loop)

  (create-instance 'MENUBAR-WIN inter:interactor-window
     (:top 5)(:left 700)(:height 360)(:width 300)
     (:aggregate (create-instance 'MENUBAR-TOP-AGG opal:aggregate)))

  (create-instance 'DEMO-MENUBAR MENUBAR
     (:items
      '((:family family-fn
	 ((:fixed fixed-fn)(:serif serif-fn)(:sans-serif sans-serif-fn)))
	(:face face-fn
	 ((:roman)(:bold)(:italic)(:bold-italic)))
	(:size size-fn
	 ((:small)(:medium)(:large)(:very-large)))))
     (:selection-function
      #'(lambda (gadget slot value)
	  (declare (ignore gadget slot value))
	  (let ((family (g-value family-text :font :family))
		(face (g-value face-text :font :face))
		(size (g-value size-text :font :size)))
	    (change-font combo-text
			 :family family :face face :size size)))))

  (create-instance 'menubar-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in DEMO-MENUBAR))
    (:continuous NIL)
    (:window MENUBAR-WIN)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send DEMO-MENUBAR :special-popup-func
				  DEMO-MENUBAR))))
  
  (opal:add-component MENUBAR-TOP-AGG DEMO-MENUBAR)

  (create-instance 'family-text opal:text
     (:left 10)
     (:top 200)
     (:string "fixed")
     (:font (create-instance NIL opal:font)))
  (create-instance 'face-text opal:text
     (:left 75)
     (:top 200)
     (:string "roman")
     (:font (create-instance NIL opal:font)))
  (create-instance 'size-text opal:text
     (:left 160)
     (:top 200)
     (:string "medium")
     (:font (create-instance NIL opal:font)))
  (create-instance 'combo-text opal:text
     (:left 75)
     (:top 230)
     (:string "combo")
     (:font (create-instance NIL opal:font)))

  (opal:add-components MENUBAR-TOP-AGG
		       family-text face-text size-text combo-text)
  
  (opal:update MENUBAR-WIN)
 (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
 )



;;;
;;;  MENUBAR-STOP
;;;

#+garnet-test
(defun Menubar-Stop ()
  (opal:destroy MENUBAR-WIN))


;;;
;;;  UTILITY FUNCTIONS USED BY EXPORTED FUNCTIONS
;;;

(defun Confirm-Menubar (a-menubar)
  (unless (is-a-p a-menubar MENUBAR)
    (error "~S is not an instance of ~S.~%" a-menubar MENUBAR))
  T)

(defun Confirm-Bar-Item (a-bar-item)
  (unless (is-a-p a-bar-item BAR-ITEM)
    (error "~S is not an instance of ~S.~%" a-bar-item BAR-ITEM))
  T)


(defun Confirm-Submenu-Item (a-submenu-item)
  (unless (is-a-p a-submenu-item SUBMENU-ITEM)
    (error "~S is not an instance of ~S.~%" a-submenu-item SUBMENU-ITEM))
  T)

(defun Confirm-Bar-or-Submenu-Item (menubar-component)
  (unless (or (is-a-p menubar-component BAR-ITEM)
	      (is-a-p menubar-component SUBMENU-ITEM))
    (error "~S is not an instance of ~S or ~S.~%" menubar-component
	   BAR-ITEM SUBMENU-ITEM))
  T)


;;;
;;;  EXPORTED FUNCTIONS
;;;

; Menubar Functions


;
; The parameter item may be either 
; 1) An instance of BAR-ITEM, or
; 2) A sublist of an :items list
;
; Locator should be a sublist of an :items list or an installed bar-item.
; Or, you can use the key feature of add-item and make locator the title of
; an installed bar-item:
;    (add-item demo-menubar new-bar :after "Bar2" :key #'car)
;
; Implementation note:  The reason that we do not just set the :items list
; and let :fix-update-slots deal with it is that the item parameter can be an
; actual component to be added, so you should not generate a new component via
; :fix-update-slots.
;
(define-method :add-item MENUBAR (a-menubar item &rest args)
  (let* ((a-bar-item (if (is-a-p item BAR-ITEM)
			 item
			 (or (let ((old-bi (pop (g-value a-menubar :storage))))
			       (when old-bi
				 (g-value old-bi :desc)  ; initialize formula
				 (s-value old-bi :desc item)
				 old-bi))
			     (make-bar-item :desc item))))
	 (parent (g-value a-bar-item :parent)) ; parent should be NIL
	 where locator key)

    (when parent
      (error "~S is already installed in ~S.~%" a-bar-item parent))
    
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    
    ; Add the bar-item as a component of the menubar
    (let* ((locator-comp (if (is-a-p locator BAR-ITEM)
			     locator
			     (get-bar-component a-menubar locator)))
	   (items (copy-list (g-value a-menubar :items)))
	   (desc (g-value a-bar-item :desc)))
      
      (opal:add-local-component a-menubar a-bar-item where locator-comp)
      (s-value a-bar-item :rank
	       (position a-bar-item (g-value a-menubar :components)))
      (s-value a-menubar :old-items
	       (s-value a-menubar
			:items
			(opal::insert-item desc items where locator key)))
      )

    ; Do additional bookkeeping that attaches the bar-item to the menubar
    (let* ((win (g-local-value a-bar-item :submenu-window))
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
; 1) An instance of BAR-ITEM, or
; 2) A sublist of the :items list
;
(define-method :remove-item MENUBAR (a-menubar item)
  (let ((a-bar-item (if (is-a-p item BAR-ITEM)
			item
			(get-bar-component a-menubar item))))
    (unless (and a-bar-item
		 (eq a-menubar (g-value a-bar-item :parent)))
      (error "~S does not have ~S as its menubar.~%"
	     a-bar-item a-menubar))
      
    ; Remove the bar-item from the menubar
    (s-value a-menubar :submenu-window-list
	     (remove (g-local-value a-bar-item :submenu-window)
		     (g-local-value a-menubar :submenu-window-list)))
    
    (opal:remove-local-component a-menubar a-bar-item)
    (unless (eq a-bar-item item)
      (push a-bar-item (g-value a-menubar :storage)))
      
    ; Change the top-level :items list
    (let ((old-desc (if (is-a-p a-bar-item BAR-ITEM)
			(g-value a-bar-item :desc)
			a-bar-item)))
      (s-value a-menubar :old-items
	 (s-value a-menubar :items (remove old-desc (g-value a-menubar :items)
					   :test #'equal))))))

(defun Make-Menubar ()
  (create-instance NIL menubar))

(define-method :Menubar-Components MENUBAR (a-menubar)
   (g-value a-menubar :components))

(define-method :Set-Menubar MENUBAR (a-menubar new-menus)
  ; Can't use dovalues and destructive operation on :components together
  (let ((components (copy-list (Menubar-Components a-menubar))))
    (dolist (old-comp components)
      (opal:remove-item a-menubar old-comp))
    (dolist (new-menu new-menus)
      (opal:add-item a-menubar new-menu))))



; Bar-Item functions

;
; The item parameter can either be
; 1) An instance of SUBMENU-ITEM, or
; 2) A description of a submenu-item: (list string action) where "string" is
;    a string or atom and "action" is a function
;
(define-method :add-item BAR-ITEM (a-bar-item item &rest args)
  (let ((a-menubar (g-value a-bar-item :parent))
	(submenu (g-value a-bar-item :submenu))
	(old-desc (or (g-local-value a-bar-item :desc)
		      (copy-list (g-value a-bar-item :desc))))
	where locator key)
    (multiple-value-setq (where locator key) (opal::get-wheres args))

    ;; item can be either a submenu-item or a list
    (if (schema-p item)
	(let* ((new-item `(,(g-value item :item-obj)
			   ,(g-value item :action)))
	       (new-sub-desc (opal::insert-item new-item (third old-desc)
						where locator key))
	       (new-desc (list (first old-desc) (second old-desc) new-sub-desc))
	       (rank (s-value item :rank
			      (position new-item new-sub-desc))))
	    (if a-menubar
		(s-value a-menubar :items
			 (substitute new-desc old-desc
			     (or (g-local-value a-menubar :items)
				 (copy-list (g-value a-menubar :items)))
			     :test #'equal))
		(s-value a-bar-item :desc new-desc))
	    (s-value submenu :old-items new-sub-desc)
    	    (opal:add-local-component submenu item :at rank))

	(let* ((new-sub-desc (opal::insert-item item (third old-desc)
						where locator key))
	       (new-desc (list (first old-desc) (second old-desc)
			       new-sub-desc))
	       (rank (position item new-sub-desc
			       :test #'(lambda (x y)
					 (equal x (funcall key y))))))
	    (if a-menubar
		(s-value a-menubar
		   :items
		   (substitute new-desc old-desc
			       (or (g-local-value a-menubar :items)
				   (copy-list (g-value a-menubar :items)))
			       :test #'equal))
		(s-value a-bar-item :desc new-desc))
	    (s-value submenu :old-items new-sub-desc)
	    (opal::Add-The-Component submenu rank)))))



;
;
; The item parameter can be either
; 1) An instance of SUBMENU-ITEM, or
; 2) A string or atom
;
(define-method :remove-item BAR-ITEM (a-bar-item &optional item
						 &key (key #'opal:no-func))
  (let* ((a-submenu-item (if (is-a-p item SUBMENU-ITEM)
			     item
			     (get-submenu-component a-bar-item item)))
	 (submenu (g-value a-bar-item :submenu))
	 (submenu-components (g-value submenu :components))
	 (rank (if item
		   (position a-submenu-item submenu-components
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (length submenu-components)))))

    (unless rank
      (error "~S does not have ~S as its bar-item.~%"
	     a-submenu-item a-bar-item))
    ; If the user did not supply an item, then just remove the last component
    (unless a-submenu-item
      (setf a-submenu-item (nth rank submenu-components)))
    ; Remove the submenu-item from the bar-item
    (opal:remove-local-component submenu a-submenu-item)
    
    ; Update the :items or :desc list
    (let* ((a-menubar (g-value a-bar-item :parent))
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
	  (s-value a-menubar
		   :items
		   (substitute new-desc old-desc (g-value a-menubar :items)
			       :test #'equal))
	  (s-value a-bar-item :desc new-desc))
      (s-value submenu :old-items new-sub-desc))))

(s-value BAR-ITEM :change-item #'menubar-change-item)
(s-value BAR-ITEM :remove-nth-item (g-value opal:aggrelist :remove-nth-item))
(s-value BAR-ITEM :set-submenu-fn #'set-submenu-fn)
(s-value MENUBAR :get-bar-component-fn #'get-bar-component-fn)

(defun Make-Bar-Item (&key desc font title)
  ;; Type-check the argument before creating an object for it.
  (when desc (Check-Bar-Item desc))
  (let* ((new-bar-item (create-instance NIL BAR-ITEM))
	 (new-submenu (create-instance NIL SUBMENU
			 (:bar-item new-bar-item)
			 (:font (or font opal:default-font))))
	 (win (Make-Submenu-Win new-bar-item new-submenu)))
    (s-value new-bar-item :submenu new-submenu)
    (s-value new-bar-item :submenu-window win)
    ; Put in initial value of :desc slot
    (g-value new-bar-item :desc) ; to initialize the default formula
    (if desc
	(s-value new-bar-item :desc desc)
	(s-value new-bar-item :desc (list title NIL NIL)))
    new-bar-item))


(defun Menubar-Get-Title-Fn (menubar-component)
  (cond ((is-a-p menubar-component BAR-ITEM)
	 (g-value menubar-component :menu-obj))
	((is-a-p menubar-component SUBMENU-ITEM)
	 (g-value menubar-component :item-obj))))

(s-value BAR-ITEM :get-title-fn #'menubar-get-title-fn)
(s-value SUBMENU-ITEM :get-title-fn #'menubar-get-title-fn)

; If the menubar-component is installed in a menubar, then the :items list is
; changed.  Otherwise, the object's :desc slot is set.
;
(defun Menubar-Set-Title-Fn (menubar-component string)
  (cond

    ;; The parameter is a BAR-ITEM
    ((is-a-p menubar-component BAR-ITEM)
     (let* ((a-bar-item menubar-component)
	    (a-menubar (g-value a-bar-item :parent)))
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

    ;; The parameter is a SUBMENU-ITEM
    ((is-a-p menubar-component SUBMENU-ITEM)
     (let* ((a-submenu-item menubar-component)
	    (a-submenu-agg (g-value menubar-component :parent)))
       (cond
	 (a-submenu-agg
	  ; a-submenu-item is installed in a bar-item
	  (let* ((a-bar-item (g-value a-submenu-agg :bar-item))
		 (a-menubar (when a-bar-item
			      (g-value a-bar-item :parent))))
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
	   BAR-ITEM SUBMENU-ITEM)))
  
  string)

(s-value BAR-ITEM :set-title-fn #'menubar-set-title-fn)
(s-value SUBMENU-ITEM :set-title-fn #'menubar-set-title-fn)

(define-method :Submenu-Components BAR-ITEM (a-bar-item)
  (g-value a-bar-item :submenu :components))

;
; The args parameter will (optionally) include the where, locator, and key
; parameters that are sent to add-item.  The where refers to the placement
; of the new submenu item among the current submenu items within b-item.
;
(define-method :Add-Submenu-Item MENUBAR (a-menubar b-item s-item &rest args)
  (let* ((a-bar-item (if (is-a-p b-item BAR-ITEM)
			 b-item
			 (get-bar-component a-menubar b-item)))
	 where locator key)
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    (opal:add-item a-bar-item s-item where locator key)))

;
; After looking up the b-item to get a bar-item object, call opal:remove-item.
;
(define-method :Remove-Submenu-Item MENUBAR (a-menubar b-item s-item)
  (let* ((a-bar-item (if (is-a-p b-item BAR-ITEM)
			 b-item
			 (get-bar-component a-menubar b-item))))
    (opal:remove-item a-bar-item s-item)))

; new-desc can have two forms:
;   1) A list of SUBMENU-ITEM instances, or
;   2) A list of submenu-item descriptions, such as
;      ((item1 action1) (item2) (item3 action3)))
;

(define-method :Get-Submenu-Component BAR-ITEM (a-bar-item item)
  (find-if #'(lambda (a-submenu-item)
	       (if (listp item)
		   (equal item (g-value a-submenu-item :desc))
		   (equal item (g-value a-submenu-item :item-obj))))
	   (Submenu-Components a-bar-item)))

(define-method :Find-Submenu-Component MENUBAR (a-menubar submenu-title submenu-item)
  (let ((a-bar-item (if (is-a-p submenu-title BAR-ITEM)
			submenu-title
			(get-bar-component a-menubar submenu-title))))
    (get-submenu-component a-bar-item submenu-item)))


; Submenu-item functions

;
; This function returns an instance of SUBMENU-ITEM.  If the :desc key is
; supplied, then the accompanying parameter should be the string/function or
; atom/function pair that describes a submenu-item.
;
(defun Make-Submenu-Item (&key desc (enabled T))
  (unless (listp desc)
    (error "Expected a list description of a submenu-item, but got ~S.~%"
	   desc))
  (let ((new-submenu-item (create-instance NIL SUBMENU-ITEM
			    (:item-guess (car desc)))))
    (when desc
      (g-value new-submenu-item :desc)
      (s-value new-submenu-item :desc desc))
    (g-value new-submenu-item :enabled)
    (s-value new-submenu-item :enabled enabled)
    new-submenu-item))


;;;
;;;  Methods for editing the strings of the menubar and submenus
;;;

(define-method :new-item-label gg::MENUBAR (obj)
  (let ((val (1+ (or (g-value obj :last-menubar-label-used)
		     (length (g-value obj :items))))))
    (s-value obj :last-menubar-label-used val)
    (list (format NIL "Label~a" val) NIL '(("sub-item1")))))

(define-method :new-item-label gg::BAR-ITEM (obj)
  (let ((val (1+ (or (g-value obj :last-bar-item-label-used)
		     (length (g-value obj :items))))))
    (s-value obj :last-bar-item-label-used val)
    (list (format NIL "Label~a" val))))

(define-method :string-set-func gg::MENUBAR
  (gadget-obj str-obj final-event final-string)
  ;; First test if we are editing the string of a bar-item
  (let ((a-bar-item (g-value str-obj :parent)))
    (if (is-a-p a-bar-item gg::BAR-ITEM)
	(let* ((old-desc (g-value a-bar-item :desc))
	       (new-desc (push final-string (cdr (copy-tree old-desc)))))
	  (Put-Down-MenuBar-Popups gadget-obj)
	  (opal::Aggrelist-Edit-String-Func gadget-obj gadget-obj str-obj
					    final-event new-desc :rank))
	;; It wasn't the string of a bar-item.  It was a submenu item.
	(let ((a-submenu (g-value str-obj :parent :parent)))
	  (setf a-bar-item (g-value a-submenu :bar-item))
	  (opal::Aggrelist-Edit-String-Func a-bar-item a-submenu str-obj
					    final-event (list final-string)
					    :rank))
	)))




#|
***** PROBLEMS: Asynchronous window errors
***** disabled items not going grey
|#
