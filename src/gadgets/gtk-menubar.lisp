;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written byt Earl Ducaine and is made available under
;;; an MIT style license.
;;;
;;;  GTK Menubar (Menubar patterned on the old, gtk style menubar)
;;;
;;;  Customizable slots: (Menubar inherits most of its slots from the
;;;  classes it's derived fram)
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

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(gtk-menubar gtk-bar-item make-gtk-submenu-win
	    ;; creation functions
	    gtk-motif-menubar gtk-motif-bar-item gtk-motif-submenu-item))
  ;; Demo things
  (export '(gtk-menubar-go gtk-menubar-stop gtk-motif-menubar
	    gtk-menubar-win gtk-menubar-top-agg)))


;; First function is to display the submenu from gilt, etc.

;; Returns the new window if and only if a different bar-item is to be
;; displayed.  Gadget parameter is the top level menubar.
(defun gtk-menubar-popup-item (gadget)
  (motif-menubar-popup-item gadget))

;; Returns a window that an instance of motif-submenu can be put into
(defun make-gtk-submenu-win (a-bar-item a-submenu)
  (make-motif-submenu-win a-bar-item a-submenu))

;; This object is a text field in the menu bar.
;; An aggrelist of these items makes up the menu bar.
(create-instance 'gtk-bar-item motif-bar-item)

;; This is the top level motif-menubar gadget -- the big mutha.
;; Basically, it contains 3 parts -> a motif-box that represents
;; the menubar, an aggrelist of bar-items, and a feedback object.
;; It also has two interactors -- menubar-select: used for selecting
;; stuff from the menubar, and menubar-accel, which does the
;; accelerator thang.
(create-instance 'gtk-menubar motif-menubar)

(s-value gtk-menubar :do-not-dump-slots
	 (append '(:submenu-window-list)
		 (g-value gtk-menubar :do-not-dump-slots)))

(s-value gtk-bar-item :do-not-dump-slots
	 (append '(:submenu :submenu-window)
		 (g-value gtkbar-item :do-not-dump-slots)))

(let ((menubar-select (g-value gtk-MENUBAR :menubar-select)))
  (s-value menubar-select :do-not-dump-slots
	   (append '(:prev-baritem)
		   (g-value menubar-select :do-not-dump-slots))))

;; This is a submenu for the gtk-menubar (surprise surprise!)  Its a
;; motif-menu that inherits some slots from the top level menubar.
;; Also, the accelerators for the menubar are given as lists of two
;; items - the string and the key.  BUT!  The motif-menu wants its
;; accelerators to be lists of THREE items - the underline, the
;; string, and the key.  So, we have to insert NILs for the
;; underlines.  That's what the o-formula in the :accelerators slot
;; does.  A submenu-item is just an instance of MOTIF-MENU-ITEM.
;; Which means any MOTIF-MENU-ITEM can be added to the menubar.
(create-instance 'gtk-submenu motif-submenu)

;;; Auxiliary function for motif-menubar's :fix-update-slots method.
;;; this function is used to establish the links between a bar-item and its
;;; submenu.  A new submenu is created and put in a new window.  This new
;;; window is added to the :submenu-window-list slot of the interactor in the
;;; top-level menubar.
(defun attach-gtk-submenu (menubar-items a-bar-item)
  (attach-motif-submenu menubar-items a-bar-item))

;;; This should be called when the number of items in the menu is changed
;;; manually (without calling add-item or remove-item).  This is basically an
;;; enhanced version of the default fix-update-slots method which reuses old
;;; components.
;;;
;;; When the menubar instance is created, aggrelists creates components
;;; for it, but this function has to be called to create submenus for the
;;; components.  If the number of :items changes, then this function should be
;;; called to both create (or destroy) new components for the aggrelist and
;;; create (or destroy) corresponding submenus.
(define-method :fix-update-slots gtk-menubar (a-menubar)
	       (call-prototype-method a-menubar))

;;;  demo functions
(defparameter *gtk-font-to-swap* (create-instance NIL opal:font))
(defvar gtk-family-text NIL)
(defvar gtk-face-text NIL)
(defvar gtk-size-text NIL)
(defvar gtk-combo-text NIL)

;;; When we want to change an object's font, set the slots of
;;; *motif-font-to-swap*, then set the object to have that font.
;;; (Opal does not notice when you just change the slots of a font.)
(defun gtk-change-font (text-obj &key family face size)
  (let ((old-font (g-value text-obj :font))
	(new-font *gtk-font-to-swap*))
    (setf *gtk-font-to-swap* old-font)
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
(defun gtk-family-fn (gadget slot value)
  (declare (ignore gadget))
  (gtk-change-font gtk-family-text slot value)
  (s-value gtk-family-text :string (string-downcase value)))

(defun gtk-face-fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-face-text slot value)
  (s-value motif-face-text :string (string-downcase value)))

(defun gtk-size-fn (gadget slot value)
  (declare (ignore gadget))
  (motif-change-font motif-size-text slot value)
  (s-value motif-size-text :string (string-downcase value)))

(defun gtk-fixed-fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "Setting :family slot to :fixed.~%"))

(defun gtk-serif-fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "Setting :family slot to :serif.~%"))

(defun gtk-sans-serif-fn (gadget bar-item submenu-item)
  (declare (ignore gadget bar-item submenu-item))
  (format t "Setting :family slot to :sans-serif.~%"))

(defun gtk-menubar-go (&key dont-enter-main-event-loop)
  (create-instance 'gtk-menubar-win inter:interactor-window
    (:background-color opal:motif-green)
    (:left 700)
    (:top 45)
    (:height 360)
    (:width 600)
    (:aggregate (create-instance 'gtk-menubar-top-agg opal:aggregate)))
  (opal:update gtk-menubar-win)

  (create-instance 'demo-gtk-menubar gtk-menubar
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

  (create-instance 'gtk-menubar-special-popup-inter inter:button-interactor
    (:start-event #\p)
    (:start-where (list :in demo-gtk-menubar))
    (:continuous nil)
    (:window gtk-menubar-win)
    (:final-function #'(lambda (inter obj)
			 (declare (ignore inter obj))
			 (kr-send demo-gtk-menubar :special-popup-func
				  demo-gtk-menubar))))

  (opal:add-component gtk-MENUBAR-TOP-AGG DEMO-gtk-MENUBAR)
  (opal:update gtk-MENUBAR-WIN)

  (create-instance 'gtk-family-text opal:text
     (:left 10)
     (:top 200)
     (:string "fixed")
     (:font (create-instance NIL opal:font)))
  (create-instance 'gtk-face-text opal:text
     (:left 75)
     (:top 200)
     (:string "roman")
     (:font (create-instance NIL opal:font)))
  (create-instance 'gtk-size-text opal:text
     (:left 160)
     (:top 200)
     (:string "medium")
     (:font (create-instance NIL opal:font)))
  (create-instance 'gtk-combo-text opal:text
     (:left 75)
     (:top 230)
     (:string "combo")
     (:font (create-instance nil opal:font)))
  (opal:add-components gtk-menubar-top-agg
		       gtk-family-text
		       gtk-face-text
		       gtk-size-text
		       gtk-combo-text)

  (opal:update gtk-menubar-win)
  (unless dont-enter-main-event-loop
    (inter:main-event-loop)))

;;;  motif-menubar-stop
(defun gtk-menubar-stop ()
  (opal:destroy gtk-menubar-win))

;;;  utility functions used by exported functions
(defun confirm-gtk-menubar (a-menubar)
  (unless (is-a-p a-menubar gtk-MENUBAR)
    (error "~S is not an instance of ~S.~%" a-menubar gtk-menubar))
  t)

(defun Confirm-Motif-Bar-Item (a-bar-item)
  (unless (is-a-p a-bar-item MOTIF-BAR-ITEM)
    (error "~S is not an instance of ~S.~%" a-bar-item MOTIF-BAR-ITEM))
  t)

(defun confirm-gtk-bar-or-submenu-item (menubar-component)
  (unless (or (is-a-p menubar-component gtk-bar-item)
	      (is-a-p menubar-component gtk-menu-item))
    (error "~S is not an instance of ~S or ~S.~%" menubar-component
	   motif-bar-item motif-menu-item))
  t)

(defun insert-item-at-n (a-list item n)
  (let ((new-list (copy-list a-list))
	(len (1- (length a-list))))
    (when (< len n)
      (dotimes (i (- n len 1))
	(setf new-list
	      (append new-list (list NIL)))))

    (append (subseq new-list 0 n) (list item)
	  (subseq new-list n))))

(defun remove-item-at-n (a-list n)
  (if (< n (length a-list))
      (append (subseq a-list 0 n) (subseq a-list (1+ n)))
      a-list))


;;;  EXPORTED FUNCTIONS

;; Menubar Functions
;;
;; The parameter item may be either
;; 1) An instance of MOTIF-BAR-ITEM, or
;; 2) A sublist of an :items list
;;
;; NOTE NOTE NOTE NOTE:  THE :ACCELERATOR KEYWORD *MUST* APPEAR BEFORE THE
;; :WHEREs IF YOU WANT TO ADD ANY ACCELERATORS
;;
;; Locator should be a sublist of an :items list or an installed bar-item.
;; Or, you can use the key feature of add-item and make locator the title of
;; an installed bar-item:
;;    (add-item demo-motif-menubar new-bar :after "Bar2" :key #'car)
;;
;; Implementation note:  The reason that we do not just set the :items list
;; and let the fix-update-slots method deal with the components is that the
;; item parameter can be an actual component to be added, so you should not
;; generate a new component via fix-update-slots.
(define-method :add-item motif-menubar (a-menubar item &rest args)
	       (apply-prototype-method (concatenate 'list (list a-menubar item)
						    args)))


;; The a-bar-item parameter can either be
;; 1) An instance of MOTIF-BAR-ITEM, or
;; 2) A sublist of the :items list
(define-method :remove-item MOTIF-MENUBAR (a-menubar item)
	       (apply-prototype-method (concatenate 'list (list a-menubar item)
						    args)))

(defun make-gtk-menubar ()
  (create-instance nil gtk-menubar))

(define-method :menubar-components gtk-menubar (a-menubar)
  (g-value a-menubar :menubar-items :components))

(define-method :set-menubar gtk-menubar (a-menubar new-menus)
	       (call-prototype-method a-menubar new-menus))

;; Bar-Item functions
;;
;; The item parameter can either be
;; 1) An instance of MOTIF-MENU-ITEM, or
;; 2) A description of a submenu-item: (list string action) where "string" is
;;    a string or atom and "action" is a function
;;
;; NOTE NOTE NOTE NOTE:  THE :ACCELERATOR KEYWORD *MUST* APPEAR BEFORE THE
;; :WHEREs IF ANY ACCELERATORS ARE TO BE ADDED
(define-method :add-item motif-bar-item (a-bar-item item &rest args)
	       (apply-prototype-method
		(concatenate 'list (list a-bar-item item) args)))

;; The item parameter can be either
;; 1) An instance of MOTIF-MENU-ITEM, or
;; 2) A string or atom
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
