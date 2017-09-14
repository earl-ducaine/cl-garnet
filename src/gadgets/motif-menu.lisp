;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;          The Garnet User Interface Development Environment.       ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;;; $Id$
;;


;;;  Motif Menu
;;
;;  Features and operation of Motif-Menu:
;;     1)  Click the left mouse button on a menu item to select the item,
;;         or if keyboard-selection is enabled, use the arrow keys to
;;         move the keyboard-selection box.
;;     2)  If the final-feedback is enabled, a selection box will appear
;;         around the selected item.
;;     3)  The top level :value slot contains a list of strings of the
;;         currently selected items.
;;     5)  The :items slot may contain functions to be executed as each
;;         item is selected, and :menu-selection-function may contain a
;;         function to be executed when there is a change in the currently
;;         selected items.
;;     6)  The :accelerators slot may contain a list of character/string
;;         pairs that determine which character in the item string to
;;         underline and a description of the key sequence that will
;;         execute a menu selection directly.
;;
;;  Customizable slots:
;;     1)  Left, top
;;     2)  Min-frame-width
;;     3)  Text-offset -- The distance from the longest string to the frame.
;;     4)  V-spacing -- The distance between items.
;;     5)  H-align -- How to align items horizontally (:left, :center, or
;;                   :right).
;;     6)  Items -- This can be: 
;;                  A list of strings, as in '("Large" ...), or
;;                  a list of atoms, as in '(:center ...), or
;;                  a list of string/function pairs, '(("Cut" Cut-FN)...)), or
;;                  a list of atom/function pairs, '((:center Center-FN)...)).
;;                  Each function will be executed when the associated button
;;                  becomes selected.  The parameters are the top-level
;;                  GADGET and the ITEM-STRING.
;;     7)  Inactive-items -- A list of strings that determines which items
;;                           should be gray and non-selectable.
;;     8)  Accelerators -- A list of character/string/character triples, equal
;;            in length to the :items list (or NIL).  Each triple consists of
;;            the character in the corresponding item string to be underlined,
;;            a string which describes the key sequence to select the menu
;;            item directly, and the corresponding key sequence in character
;;            form.
;;     9)  Bar-above-these-items -- A bar will apear before each item in
;;            this list.
;;    10)  Item-To-String-Function -- a function which takes an ITEM and
;;            returns a string to be displayed in the menu corresponding to
;;            that item.  For example, if the :items slot contains a list of
;;            Garnet schemas, then the function would return the name of a
;;            schema.  If each item is a string/fn or atom/fn pair, only the
;;            CAR of the pair is sent to the :item-to-string-function.
;;            The default function assumes that :items contains a list of
;;            strings.
;;    11)  Final-feedback-p -- whether to border the selected item with a
;;                             selection box.
;;    12)  Item-font, accel-font
;;    13)  Keyboard-selection-p -- Whether to allow selections by the keyboard
;;    14)  Value -- The currently selected string
;;         Foreground-Color
;;    15)  Selection-function -- Global function to be executed whenever
;;            there is a change in the list of currently selected items.
;;            Parameters are the top-level MOTIF-MENU gadget and the
;;            ITEM-OBJ that was just selected.  (The name of the item that
;;            was just selected can be obtained by g-value'ing the :item slot
;;            of the ITEM-OBJ.)
;;
;;  Motif-Menu demo:
;;     This module contains a function which creates a window and a 
;;     menu.  To run it, enter (GARNET-GADGETS:motif-menu-go).
;;     To stop, enter (GARNET-GADGETS:motif-menu-stop).
;;
;;  NOTE:  This module requires several schemata defined in motif-parts.
;;         Thus, motif-parts.fasl must be loaded before this module.
;;
;;  Written by Andrew Mickish


;;;  Change Log:
;;  08/19/93 Rajan Parthasarathy - Modified :underline's :x1 & :width
;;           slots (as suggested by Poelman) in MOTIF-MENU-ITEM to consider
;;           variable width fonts.
;;  07/26/93 Andrew Mickish - Added special proclamation to eliminate warning
;;  06/30/93 Andrew Mickish - Fixed :fast-redraw-filling-style formula
;;  06/22/93 Andrew Mickish - Added checks for NIL :items list in formulas
;;           for :max-item-width and :items-height.
;;  06/16/93 Andrew Mickish - Fixed more :visible formulas to look at parent's
;;           visibility; set :old-items of aggrelist in *-local-item methods
;;  02/23/93 Andrew Mickish - Added :string-set-func
;;  02/22/93 Andrew Mickish - Made MOTIF-MENU-TEXT-LABEL-PROTOTYPE a stand-
;;           alone object for use in the MOTIF-MENUBAR
;;  02/10/93 Andrew Mickish - Added items-type and accelerators-type
;;  02/09/93 Andrew Mickish - Added dependency on :items list so that
;;           the aggrelist's :fix-update-slots method will be invoked;
;;           Removed kr-path calls from MOTIF-MENU-ITEM
;;  01/05/93 Rajan Parthasarathy - Created MOTIF-MENU-ITEM and set it
;;           to be the prototype of MOTIF-MENU-ITEM-LIST.  Also, the
;;           way to check to see if an item is active is to look at
;;           the :active-p slot of the item.  Previously, it was to
;;           look up the item in the :inactive-items slot of the menu
;;  12/15/92 Andrew Mickish - Added type and parameter declarations
;;  06/16/92 Andrew Mickish - Added objects in :items list
;;  04/30/92 Andrew Micksih - Called get-standard-font for fonts
;;  02/27/92 Andrew Mickish - Removed :leftdown from MOTIF-MENU-KEY-INTER
;;  02/11/92 Andrew Mickish - Added :maybe-constant list
;;  11/06/91 Andrew Mickish - Changed the feedback object from a MOTIF-BOX
;;           to two fast redraw polylines
;;  10/13/91 Andrew Mickish - Added :leftdown case in MOTIF-MENU-KEY-INTER
;;           so the keyboard selection will follow the mouse
;;  10/08/91 Andrew Mickish - Turned off fast-redraw in selection box
;;  05/09/91 Andrew Mickish - In MOTIF-MENU, changed :global-accel-chars,
;;           :global-accel-strings, and :local-accel-chars to contiain NIL
;;           elements when some accelerator characters are not supplied; and
;;           changed :start-event in MOTIF-MENU-ACCELERATOR-INTER to delete
;;           these NIL elements from its list
;;  03/01/91 Andrew Mickish - Created


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Menu Motif-Menu-Accelerator-Inter))
  #+garnet-test
  (export '(Motif-Menu-Go Motif-Menu-Stop
	    Demo-Motif-Menu Motif-Menu-Win Motif-Menu-Top-Agg))

  (proclaim '(special MOTIF-BAR-ITEM)))

(create-instance 'MOTIF-MENU-TEXT-LABEL-PROTOTYPE opal:text
  (:constant '(:actual-heightp))
  (:left (o-formula
	  (let* ((p0 (gvl :parent))
		 (p1 (gvl :parent :parent :parent))
		 (text-offset (+ (gv p1 :text-offset)
				 (if (gv p1 :accelerators) 0 4))))
	    (case (gv p1 :h-align)
	      (:left (+ (gv p0 :left) text-offset))
	      (:center (- (+ (gv p0 :left)
			     (floor (gv p0 :width) 2))
			  (floor (gvl :width) 2)))
	      (:right (- (+ (gv p0 :left) (gv p0 :width))
			 (gvl :width) text-offset))))))
  (:top (o-formula (opal:gv-center-y-is-center-of (gvl :parent))))
  (:string (o-formula (gv (kr-path 0 :parent) :string)))
  (:font (o-formula (gv (kr-path 0 :parent :parent :parent) :item-font)))
  (:line-style (o-formula (gv (kr-path 0 :parent) :line-style)))
  (:fast-redraw-p :rectangle)
  (:fast-redraw-filling-style
   (o-formula (gv (kr-path 0 :parent :parent :parent) :foreground-fill))))



;;  Can't use kr-path in ouside references from MOTIF-MENU-ITEM because
;;  instances can be manipulated as independent components in the menubar.
;;
(create-instance 'MOTIF-MENU-ITEM opal:aggregadget
  (:left (o-formula (gvl :parent :left)))
  (:top (o-formula (+ (gvl :parent :top)
		      (* (gvl :rank) (gvl :parent :parent :item-height)))))
  (:width (o-formula (gvl :parent :width)))
  (:height (o-formula (gvl :parent :parent :item-height)))
  (:char (o-formula (let ((chars (gvl :parent :parent :local-accel-chars)))
		      (if chars (nth (gvl :rank) chars)))))
  (:accel-string (o-formula
		  (let ((strings (gvl :parent :parent :global-accel-strings)))
		    (if strings (nth (gvl :rank) strings)))))
  ;; Conditional formulas are required to allow either a list of
  ;; strings or a list of string/function pairs in the :items slot.
  (:item-obj (o-formula (nth (gvl :rank) (gvl :parent :parent :item-objs))))
  (:string (o-formula
	    (progn
	      ;; Invoke invalidate-demon on aggrelist's :items slot
	      ;; so that :fix-update-slots method will be called
	      (gvl :parent :items)
	      (nth (gvl :rank) (gvl :parent :parent :strings)))))
  (:action (o-formula (nth (gvl :rank) (gvl :parent :parent :actions))))
  (:active-p (o-formula (not (member (gvl :item-obj)
				      (gvl :parent :parent :inactive-items)
				      :test #'equal))))
  (:line-style (o-formula (if (gvl :active-p)
			       opal:default-line-style
			       (gvl :parent :parent :stippled-line-style))))
  (:visible (o-formula (let ((parent (gvl :parent)))
			 (and (or (null parent) (gv parent :visible))
			      (not (equal (gvl :string) ""))))))
  (:parts
   `((:item-text ,#'opal::Panel-Get-Label)
     (:accel-text ,opal:text
      (:constant (:actual-heightp))
      (:left ,(o-formula
	       (let ((p (gvl :parent :parent :parent)))
		 (- (gv p :right) (gv p :text-offset) (gvl :width) 4))))
      (:top ,(o-formula (gv (kr-path 0 :parent :item-text) :top)))
      (:string ,(o-formula (or (gv (kr-path 0 :parent) :accel-string)
			       "")))
      (:font ,(o-formula (gvl :parent :parent :parent :accel-font)))
      (:visible ,(o-formula (and (gv (kr-path 0 :parent) :visible)
				 (not (string= "" (gvl :string))))))
      (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style
       ,(o-formula (gvl :parent :parent :foreground-fill))))
     (:underline ,opal:line
      (:width ,(o-formula (opal:string-width
                     (gv (kr-path 0 :parent :item-text) :font)
                     (string (gv (kr-path 1 :parent) :char)))))
      (:x1 ,(o-formula (+ (gv (kr-path 0 :parent :item-text) :left)
                       (opal:string-width
                        (gv (kr-path 0 :parent :item-text) :font)
                        (gv (kr-path 0 :parent :item-text) :string)
                        :end (position
                              (gv (kr-path 1 :parent) :char)
                              (gv (kr-path 0 :parent :item-text) :string))))))
      (:y1 ,(o-formula (- (+ (gv (kr-path 0 :parent :item-text) :top)
			     (gv (kr-path 0 :parent :item-text) :height)) 1)))
      (:x2 ,(o-formula (+ (gvl :x1) (gvl :width))))
      (:y2 ,(o-formula (gvl :y1)))
      (:visible ,(o-formula (and (gv (kr-path 0 :parent) :visible)
				 (gv (kr-path 0 :parent) :char))))
      (:line-style ,(o-formula (gv (kr-path 0 :parent) :line-style)))
      (:fast-redraw-p :rectangle)
      (:fast-redraw-filling-style
       ,(o-formula (gvl :parent :parent :foreground-fill))))
     )))

(create-instance 'MOTIF-MENU-ITEM-LIST opal:aggrelist
   (:constant '(:rank-margin :pixel-margin :fixed-width-p :fixed-height-p
		:direction))
   (:left (o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
   (:top (o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
   (:width (o-formula (- (gv (kr-path 0 :parent) :frame-width) 4)))
   (:height (o-formula (gv (kr-path 0 :parent) :items-height)))
   (:direction NIL)
   (:items (o-formula (gv (kr-path 0 :parent) :items)))
   (:item-to-string-function (o-formula (gv (kr-path 0 :parent)
					    :item-to-string-function)))
   (:item-prototype MOTIF-MENU-ITEM))

(create-instance 'MOTIF-MENU-BAR-LIST opal:aggrelist
   (:constant '(:rank-margin :pixel-margin :fixed-width-p :fixed-height-p
		:direction))
   (:left (o-formula (+ 4 (gv (kr-path 0 :parent) :left))))
   (:top (o-formula (+ 4 (gv (kr-path 0 :parent) :top))))
   (:width (o-formula (- (gv (kr-path 0 :parent) :frame-width) 8)))
   (:height (o-formula (gv (kr-path 0 :parent) :items-height)))
   (:direction NIL)
   (:items (o-formula (gv (kr-path 0 :parent) :bar-above-these-items)))
   (:item-prototype
    `(,opal:aggregadget
      (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
      (:top ,(o-formula (gvl :obj-after :top)))
      (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
      (:height ,(o-formula (gv (kr-path 0 :parent) :height)))  
      (:item-after ,(o-formula (nth (gvl :rank) (gv (kr-path 0 :parent) :items))))
      (:obj-after
       ,(o-formula
	 (let* ((menu (kr-path 0 :parent :parent))
		(components (gv menu :menu-item-list :components))
		(item (gvl :item-after))
		(item-rank (position item (gv menu :item-objs) :test #'equal)))
	   (nth item-rank components))))
      (:parts
       ((:top-line ,opal:line
	 (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
	 (:y1 ,(o-formula (gv (kr-path 0 :parent) :top)))
	 (:x2 ,(o-formula (+ (gvl :x1) (gv (kr-path 0 :parent) :width))))
	 (:y2 ,(o-formula (gvl :y1)))
	 (:line-style ,(o-formula
			(gv (kr-path 0 :parent :parent :parent)
			    :thin-shadow-line-style))))
	(:bot-line ,opal:line
	 (:x1 ,(o-formula (gv (kr-path 0 :parent) :left)))
	 (:y1 ,(o-formula (+ 1 (gv (kr-path 0 :parent) :top))))
	 (:x2 ,(o-formula (+ (gvl :x1) (gv (kr-path 0 :parent) :width))))
	 (:y2 ,(o-formula (gvl :y1)))
	 (:line-style ,(o-formula
			(gv (kr-path 0 :parent :parent :parent)
			    :thin-highlight-line-style)))))))))


(create-instance 'MOTIF-MENU-KEY-INTER inter:button-interactor
  (:active (o-formula (and (gvl :window)
			   (gvl :operates-on :keyboard-selection-p))))
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:continuous NIL)
  (:start-where T)
  (:start-event (list #\return
		      :uparrow :rightarrow :downarrow :leftarrow))
  (:final-function
   #'(lambda (interactor obj)
       (declare (ignore obj))
       (let* ((char (inter:event-char inter:*Current-Event*))
	      (gadget (g-value interactor :operates-on))
	      (selection (g-value gadget :keyboard-selection-obj))
	      ;; Multiple selection not implemented
	      (prev-sel (g-value gadget :value-obj)))
	 (case char
	   (#\return
	    (let ((item-obj (g-value gadget :keyboard-selection))
		  (action (g-value selection :action)))
	       
	      ;; Propagate new selection toward :value slot
	      (s-value gadget :value-obj selection)
	       
	      ;; Make interim feedback flash
	      (s-value selection :interim-selected T)
	      (s-value selection :selected T)
	      (if (and prev-sel (not (eq prev-sel selection)))
		  (s-value prev-sel :selected NIL))
	      (opal:update (g-value gadget :window))
	      (sleep .25)
	      (s-value selection :interim-selected NIL)

	      ;; Global function for all items
	      (kr-send gadget :selection-function gadget selection)
		    
	      ;; RGA --- Needed to fix this function.
	      ;; Local function assigned to item.
	      ;; If this is in a menubar, you have to call the item
	      ;; function with THREE arguments. Otherwise, with 2 args
		
	      (when action
		(if (AND (g-value gadget :bar-item)
			 (boundp 'MOTIF-BAR-ITEM)
			 (is-a-p (g-value gadget :bar-item) MOTIF-BAR-ITEM))
		    (funcall action
			     (g-value gadget :bar-item :parent :parent)	;; The menubar
			     (g-value gadget :bar-item :menu-obj)	;; The bar-item
			     item-obj)					;; The item
		    (funcall action gadget item-obj)))

	      ))
;;;	   (:leftdown
;;;	   (let ((obj (motif-element-of-not-illegal
;;;	   (g-value gadget :menu-item-list)
;;;	   interactor inter:*Current-Event*)))
;;;	   (when (and obj (g-value obj :active-p))
;;;	   (s-value gadget
;;;	   :keyboard-selection
;;;	   (g-value obj :item-obj)))))
	   ((:downarrow :rightarrow)
	    (let* ((prev-rank (g-value selection :rank))
		   (menu-item-list (g-value gadget :menu-item-list))
		   (items (g-value menu-item-list :components))
		   (max-rank (g-value menu-item-list :tail :rank)))
	      (do* ((rank (if (= prev-rank max-rank)
			      0 (+ 1 prev-rank))
			  (if (= rank max-rank)
			      0 (+ 1 rank)))
		    (item (nth rank items) (nth rank items)))
		   ((g-value item :active-p)
		    (s-value gadget
			     :keyboard-selection
			     (g-value item :item-obj))))))

	   ((:uparrow :leftarrow)
	    (let* ((prev-rank (g-value selection :rank))
		   (menu-item-list (g-value gadget :menu-item-list))
		   (items (g-value menu-item-list :components))
		   (max-rank (g-value menu-item-list :tail :rank)))
	      (do* ((rank (if (> prev-rank 0)
			      (- prev-rank 1) max-rank)
			  (if (> rank 0)
			      (- rank 1) max-rank))
		    (item (nth rank items) (nth rank items)))
		   ((g-value item :active-p)
		    (s-value gadget
			     :keyboard-selection
			     (g-value item :item-obj)))))))))))


(create-instance 'MOTIF-MENU-ACCEL-INTER inter:button-interactor
  (:active (o-formula (and (gvl :window)
			   (gvl :operates-on :keyboard-selection-p))))
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:continuous NIL)
  (:start-where T)
  (:start-event (list :any-keyboard :except #\return :uparrow :rightarrow
		      :downarrow :leftarrow))
  (:final-function
   #'(lambda (interactor obj)
       (declare (ignore obj))
       (let* ((gadget (g-value interactor :operates-on))
	      (char (inter:event-char inter:*Current-Event*))
	      (accel-chars (g-value gadget :local-accel-chars))
	      (rank (position char accel-chars)))
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
			      (g-value gadget :bar-item :parent :parent) ;  The menubar
			      (g-value gadget :bar-item :menu-obj)	 ;  The bar-item
			      (g-value selection :item-obj))		 ;  The item
		     (funcall action gadget (g-value selection :item-obj)))))))))))


(create-instance 'MOTIF-MENU-PRESS-INTER inter:menu-interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:start-where (o-formula (list :custom
				 (gvl :operates-on :menu-item-list)
				 #'Motif-Element-Of-Not-Illegal)))
  (:feedback-obj (o-formula (gvl :operates-on :feedback-obj-topline)))
  (:how-set :set)
  ;; I don't know why this was here
;;;   (:start-action
;;;    #'(lambda (interactor first-obj)
;;;	(let* ((gadget (g-value interactor :operates-on)))
;;;	  (s-value gadget :value-obj NIL))
;;;	(call-prototype-method interactor first-obj)))
  (:stop-action
   #'(lambda (interactor obj-under-mouse)
       (let* ((gadget (g-value interactor :operates-on))
	      (action (g-value obj-under-mouse :action)))

	 (s-value obj-under-mouse :interim-selected NIL)
	 (s-value gadget :value-obj obj-under-mouse)
	   
	 ;; Global function executed whenever selections change
	 (kr-send gadget :selection-function gadget obj-under-mouse)

	 ;; Local function assigned to item
	 ;; If this is in a menubar, you have to call the item
	 ;; function with THREE arguments.  Otherwise, with 2 args

	 (when action
	   (if (AND (g-value gadget :bar-item)
		    (boundp 'MOTIF-BAR-ITEM)
		    (is-a-p (g-value gadget :bar-item) MOTIF-BAR-ITEM))
	       (funcall action
			(g-value gadget :bar-item :parent :parent) ;; The menubar
			(g-value gadget :bar-item :menu-obj)	   ;; The bar-item
			(g-value obj-under-mouse :item-obj))	   ;; The item	    
	       (funcall action
			gadget
			(g-value obj-under-mouse :item-obj))))))))


(create-instance 'MOTIF-MENU MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :min-frame-width :text-offset :v-spacing
			 :h-align :items :accelerators :bar-above-these-items
			 :inactive-items
			 :item-font :accel-font :item-to-string-function
			 :final-feedback-p :foreground-color :value
			 :selection-function :visible)
	    (:type ((integer 0) :min-frame-width)
		   (integer :text-offset :v-spacing)
		   ((member :left :center :right) :h-align)
		   (accelerators-type :accelerators)
		   (items-type :items :inactive-items :bar-above-these-items)
		   ((or function symbol) :item-to-string-function)
		   (kr-boolean :keyboard-selection-p)
		   ((or null string keyword (satisfies schema-p))
		    :keyboard-selection :value)
		   ((or null (satisfies schema-p)) :keyboard-selection-obj
		    :value-obj)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :item-font :accel-font)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :min-frame-width :text-offset :h-align
			     :v-spacing :items :accelerators
			     :bar-above-these-items :item-to-string-function
			     :final-feedback-p :foreground-color :item-font
			     :accel-font :visible))
  (:left 0) (:top 0)
  (:min-frame-width 0)
  (:text-offset 6)
  (:v-spacing 8)
  (:h-align :left)
  (:items '("Menu 1" "Menu 2" "Menu 3" "Menu 4" "Menu 5"))
  (:inactive-items NIL)
  (:accelerators NIL)
  (:bar-above-these-items NIL)
  (:item-to-string-function
   #'(lambda (item)
       (if item
	   (if (or (stringp item) (schema-p item))
	       item
	       (string-capitalize (string-trim ":" item)))
	   "")))
  (:final-feedback-p T)

  (:foreground-color opal:MOTIF-GRAY)
  (:item-font opal:default-font)
  (:accel-font opal:default-font)

  (:keyboard-selection-p NIL)
  (:keyboard-selection (o-formula
			(first
			 (last (set-exclusive-or (gvl :item-objs)
						 (gvl :inactive-items)
						 :test #'equal)))))
  (:keyboard-selection-obj (o-formula
			    (let ((rank (position (gvl :keyboard-selection)
						  (gvl :item-objs)
						  :test #'equal)))
			      (nth rank (g-value (gvl :menu-item-list)
						 :components)))))
  (:selection-function NIL)
  (:value-obj NIL)
  (:value (o-formula (let ((obj (gvl :value-obj)))
		       (if obj
			   (g-value obj :item-obj)))))

  (:item-objs (o-formula (let ((items (gvl :items)))
			   (if (listp (first items))
			       (mapcar #'car items)
			       items))))
  (:strings (o-formula
	     (let ((fn (gvl :item-to-string-function)))
	       (declare (ignore fn))
	       (mapcar #'(lambda (item-obj)
			   (kr-send (gv :self) :item-to-string-function
				    item-obj))
		       (gvl :item-objs)))))
  (:actions (o-formula (let ((items (gvl :items)))
			 (when (listp (first items))
			   (mapcar #'cadr items)))))
  (:local-accel-chars (o-formula
		       (let ((accels (gvl :accelerators)))
			 (if accels
			     (mapcar #'first accels)))))
  (:global-accel-strings (o-formula
			  (let ((accels (gvl :accelerators)))
			    (if accels
				(mapcar #'second accels)))))
  (:global-accel-chars (o-formula
			(let ((accels (gvl :accelerators)))
			  (if accels
			      (mapcar #'third accels)))))
  (:max-item-width
   (o-formula
    (do* ((item-strings (gvl :strings))
	  (number-of-items (length item-strings))
	  (accels (if (gvl :accelerators)
		      (gvl :global-accel-strings)
		      (make-list number-of-items :initial-element "")))
	  (item-font (gvl :item-font))
	  (accel-font (gvl :accel-font))
	  (i 0 (+ i 1))
	  (item-str (nth i item-strings) (nth i item-strings))
	  (accel-str (nth i accels) (nth i accels))
	  (str-width (if (schema-p item-str)
			 (g-value item-str :width)
			 (if item-str
			     (opal:string-width item-font item-str)
			     ;; The case for a NULL :items list
			     0))
		     (if (schema-p item-str)
			 (g-value item-str :width)
			 (if item-str
			     (opal:string-width item-font item-str)
			     0)))
	  (accel-width (if accel-str
			   (opal:string-width accel-font accel-str)
			   0)
		       (if accel-str
			   (opal:string-width accel-font accel-str)
			   0))
	  (max-width (+ str-width accel-width)
		     (let ((width (+ str-width accel-width)))
		       (if (> width max-width) width max-width))))
	 ((= i number-of-items) (+ max-width (* 2 (gvl :text-offset))
				   (if (gvl :accelerators)
				       (gvl :text-offset) 0))))))

  (:frame-width (o-formula (+ 12 (MAX (gvl :max-item-width)
				      (gvl :min-frame-width)))))
  (:frame-height (o-formula (+ 4 (gvl :items-height))))
  (:item-height (o-formula (+ (gvl :v-spacing)
			      (MAX (let ((max-schema-height 0))
				     (dolist (s (gvl :strings))
				       (if (schema-p s)
					   (setq max-schema-height
						 (MAX max-schema-height
						      (g-value s :height)))))
				     max-schema-height)
				   (opal:string-height (gvl :item-font) "X")
				   (if (gvl :accelerators)
				       (opal:string-height (gvl :accel-font)
							   "X") 0)))))
  (:items-height (o-formula (let ((items (gvl :items)))
			      (if items
				  (* (length items) (gvl :item-height))
				  0))))
  (:width (o-formula (gvl :frame-width)))
  (:height (o-formula (gvl :frame-height)))
  (:right (o-formula (+ (gvl :left) (gvl :width))))

  (:active-p (o-formula (> (length (gvl :items))
			   (length (gvl :inactive-items)))))
  (:text-label-prototype MOTIF-MENU-TEXT-LABEL-PROTOTYPE)
  (:parts
   `((:FRAME ,MOTIF-BOX
	     (:constant (:depressed-p))
	     (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
	     (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
	     (:width ,(o-formula (gv (kr-path 0 :parent) :frame-width)))
	     (:height ,(o-formula (gv (kr-path 0 :parent) :frame-height)))
	     (:depressed-p NIL))
     (:MENU-ITEM-LIST ,MOTIF-MENU-ITEM-LIST)
     (:BAR-LIST ,MOTIF-MENU-BAR-LIST)
     (:FEEDBACK-OBJ-TOPLINE ,opal:polyline
			    (:feedback-width ,(o-formula (- (gv (kr-path 0 :parent) :frame-width) 10)))
			    (:feedback-height ,(o-formula (- (gv (kr-path 0 :parent) :item-height) 6)))
			    (:point-list ,(o-formula
					   (let* ((p (kr-path 0 :parent))
						  (left (+ 5 (gv p :left)))
						  (top (+ 3 (gvl :obj-over :top))))
					     (list left (+ top (gvl :feedback-height))
						   left top
						   (+ left (gvl :feedback-width)) top))))
			    ;; Set by interactors -- guarantee NIL if no menu-items are active
			    (:obj-over ,(o-formula (let* ((p (kr-path 0 :parent))
							  (value (gv p :value)))
						     (unless (member value
								     (gv p :inactive-items)
								     :test #'equal)
						       (gv p :value-obj)))))
			    (:visible ,(o-formula
					(let ((obj-over (gvl :obj-over)))
					  (and obj-over
					       (or (gv obj-over :interim-selected)
						   (gv (kr-path 0 :parent) :final-feedback-p))
					       (gv (kr-path 0 :parent) :visible)))))
			    (:line-style ,(o-formula (gv (kr-path 0 :parent) :highlight-line-style)))
			    (:fast-redraw-p :redraw)
			    (:fast-redraw-line-style ,(o-formula (gv (kr-path 0 :parent)
								     :foreground-line-style))))
     (:FEEDBACK-OBJ-BOTTOMLINE ,opal:polyline
			       (:point-list ,(o-formula
					      (let* ((left (+ 5 (gv (kr-path 0 :parent) :left)))
						     (top (+ 3 (gvl :obj-over :top)))
						     (p1 (kr-path 1 :parent :feedback-obj-topline))
						     (bottom (+ top (gv p1 :feedback-height)))
						     (right (+ left (gv p1 :feedback-width))))
						(list right top right bottom left bottom))))
			       (:obj-over ,(o-formula (gv (kr-path 0 :parent :feedback-obj-topline)
							  :obj-over)))
			       (:visible ,(o-formula (gv (kr-path 0 :parent :feedback-obj-topline)
							 :visible)))
			       (:line-style ,(o-formula (gv (kr-path 0 :parent) :shadow-line-style)))
			       (:fast-redraw-p :redraw)
			       (:fast-redraw-line-style ,(o-formula (gv (kr-path 0 :parent)
									:foreground-line-style))))
     (:SEL-BOX ,MOTIF-SELECTION-BOX
	       (:fast-redraw-p NIL) (:draw-function :copy)
	       (:fast-redraw-line-style NIL))))
  (:interactors
   `((:PRESS ,motif-menu-press-inter)
     (:ACCEL ,motif-menu-accel-inter)
     (:KEY ,motif-menu-key-inter))))
      
       

;; This interactor is used in conjunction with a set of motif-menu gadgets
;; to implement the global accelerator feature.  An instance of this
;; interactor is supplied with a list of motif-menu instances and the window
;; in which those menus appear.  Then, whenever a global accelerator key
;; is pressed in the window, the corresponding item functions will be
;; executed.
(create-instance 'MOTIF-MENU-ACCELERATOR-INTER inter:button-interactor
  :declare ((:parameters :window :menus :start-where :start-event
			 :accel-chars :waiting-priority :running-priority
			 :stop-action :final-function)
	    (:type ((or list (is-a-p inter:interactor-window)) :window)
		   (list :menus :accel-chars)
		   ((or list (member T)) :start-where)
		   ((or keyword character list) :start-event)
		   ((is-a-p inter:priority-level) :waiting-priority
		    :running-priority)
		   ((or null function symbol) :stop-action :final-function)))
  (:window NIL)
  (:menus NIL)
  (:continuous NIL)
  (:start-where T)
  (:start-event (o-formula (remove NIL (multiple-value-call #'append
					 (values-list (gvl :accel-chars))))))
  (:accel-chars (o-formula (mapcar #'(lambda (menu)
				       (gv menu :global-accel-chars))
				   (gvl :menus))))
  (:waiting-priority MOTIF-TAB-PRIORITY-LEVEL)
  (:running-priority MOTIF-TAB-PRIORITY-LEVEL)
  (:stop-action
   #'(lambda (interactor obj-over)
       ;; First get the menu and item ranks corresponding to the accelerator
       (let ((menu-rank NIL) (char-rank NIL))
	 (do* ((char (g-value interactor :start-char))
	       (accel-chars (g-value interactor :accel-chars))
	       (local-menu-rank 0 (+ 1 local-menu-rank))
	       (menu-chars (nth local-menu-rank accel-chars)
			   (nth local-menu-rank accel-chars))
	       (local-char-rank (position char menu-chars)
				(position char menu-chars)))
	      (local-char-rank
	       (progn
		 (setf menu-rank local-menu-rank)
		 (setf char-rank local-char-rank))))

	 (let* ((menu (nth menu-rank (g-value interactor :menus)))
		(selection (nth char-rank (g-value menu :menu-item-list
						   :components)))
		(action (g-value selection :action))
		(prev-sel (g-value menu :value-obj)))

	      
	   ;; Check to see whether corresponding item is inactive
	   (when (g-value selection :active-p)
	     ;; Propagate new selection toward :value slot
	     (s-value menu :value-obj selection)
	     (s-value selection :selected T)
	     (if (and prev-sel (not (eq prev-sel selection)))
		 (s-value prev-sel :selected NIL))
		
	     ;; Make interim feedback flash if no final-feedback
	     (unless (g-value menu :final-feedback-p)
	       (s-value selection :interim-selected T)
	       (opal:update (g-value menu :window))
	       (sleep .25)
	       (s-value selection :interim-selected NIL))
		
	     ;; Execute associated functions
	     (kr-send menu :selection-function menu selection)
		
	     ;; Local item function.
	     ;; If this is in a menubar, you have to call the item
	     ;; function with THREE arguments, otherwise, with 2 args.
		
	     (when action
	       (if (AND (g-value menu :bar-item)
			(boundp 'MOTIF-BAR-ITEM)
			(is-a-p (g-value menu :bar-item) MOTIF-BAR-ITEM))
		   (funcall action
			    (g-value menu :bar-item :parent :parent) ;; The menubar
			    (g-value menu :bar-item :menu-obj)	     ;; The bar-item
			    (g-value selection :item-obj))	     ;; The item		
		   (funcall action menu (g-value selection :item-obj))))))


	 ;; Gotta check this to see that the interactor wasn't already
	 ;; destroyed.
	 (when (schema-p interactor)
	   ;; Now execute the user's :final-function
	   (kr-send interactor :final-function interactor obj-over))))))

  
(define-method :add-local-item MOTIF-MENU (gadget item &rest args)
  (let ((accel NIL) where locator key)
    ;; Strip off and bind the accelerator (if there is one)
    (when (eq :accelerator (first args))
      ;; accel will be the user's new accelerator character
      (setf accel (second args))
      (setf args (cddr args)))
    ;; Get where as usual
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    (let* ((old-items (or (g-local-value gadget :items)
			  (copy-list (g-value gadget :items))))
	   (items (opal::insert-item item old-items where locator key))
	   (rank (position item items
			   :test #'(lambda (x y)
				     (equal x (funcall key y)))))
	   (alist (g-value gadget :menu-item-list)))
      (s-value alist :old-items (s-value gadget :items items))
      (if accel
	  (s-value gadget :accelerators
		   (opal::insert-item accel (g-value gadget :accelerators)
				      :at rank #'opal:no-func)))
      (opal::Add-The-Component (g-value gadget :menu-item-list) rank))))

(define-method :add-item MOTIF-MENU (gadget item &rest args)
  "This function takes an :accelerator key, and also takes the
standard 'where' keywords.  The :accelerator key must appear
before the 'where' keys, since all keyword parameters are
handled manually within the function."
  (let ((accel NIL) where locator key)
    ;; Strip off and bind the accelerator (if there is one)
    (when (eq :accelerator (first args))
      ;; accel will be the list of the :accelerator keyword and the user's
      ;; new accelerator character
      (setf accel args)
      (setf args (cddr args))
      (setf (cddr accel) NIL))
    ;; Get where as usual
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    ;; Insert the item in the items list as usual
    (eval `(opal:add-local-item ,gadget ,item ,@accel ,where ,locator ,key))
    (let ((rank (position item (g-value gadget :items)
			  :test #'(lambda (x y)
				    (equal x (funcall key y))))))
      (dolist (inst (g-value gadget :is-a-inv))
	(unless (has-slot-p inst :items)
	  (opal::Recursive-Add-Component
	   (g-value inst :menu-item-list) rank))))))


(define-method :remove-local-item MOTIF-MENU (gadget &optional item
						     &key (key #'opal:no-func))
  (let* ((items (or (g-local-value gadget :items)
		    (copy-list (g-value gadget :items))))
	 (accels (or (g-local-value gadget :accelerators)
		     (copy-list (g-value gadget :accelerators))))
	 (rank (if item
		   (position item items
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (length items))))
	 (alist (g-value gadget :menu-item-list)))
    ;; Gadgets always have lists for their :items values, so don't consider
    ;; the case where the value of :items is a number.
    (s-value alist :old-items
	     (s-value gadget :items
		      (opal::delete-elt (or item (nth rank items)) items key)))
    (if accels
	(s-value gadget :accels
		 (opal::delete-elt (nth rank accels) items key)))
    ;; Before destroying the component, remove the item from it
    (let* ((comp-to-destroy (nth rank (g-value alist :components))))
      (if (and (schema-p item)
	       (g-value item :parent)
	       (opal::Is-In-Hierarchy comp-to-destroy item))
	  (let ((kr::*constants-disabled* T))
	    (opal:remove-local-component (g-value item :parent) item)))
      (if (g-value comp-to-destroy :parent)
	  (opal:remove-local-component alist comp-to-destroy))
      (opal:destroy comp-to-destroy))))


(define-method :remove-item MOTIF-MENU (gadget &optional item
					       &key (key #'opal::no-func))
    (opal::Gadget-Remove-Item gadget item :menu-item-list key))

(s-value MOTIF-MENU :change-item (g-value opal:aggrelist :change-item))
(s-value MOTIF-MENU :remove-nth-item (g-value opal:aggrelist :remove-nth-item))

(define-method :string-set-func MOTIF-MENU
    (gadget-obj str-obj final-event final-string)
  (let ((aggrel (g-value gadget-obj :MENU-ITEM-LIST)))
    (opal::Aggrelist-Edit-String-Func gadget-obj aggrel str-obj
				      final-event final-string :rank)))




;;;  DEMO FUNCTION
;;

#+garnet-test
(defvar BOLD-FONT (opal:get-standard-font NIL :bold NIL))

#+garnet-test
(defun MOTIF-MENU-GO (&key dont-enter-main-event-loop not-double-buffered-p)
  (create-instance 'MOTIF-MENU-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Menu")
     (:left 750)(:top 10)(:width 210)(:height 200)
     (:background-color opal:motif-blue))
  (s-value MOTIF-MENU-WIN
	   :aggregate
	   (create-instance 'MOTIF-MENU-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-MENU MOTIF-MENU
     (:left 34) (:top 20)
     (:foreground-color opal:MOTIF-BLUE)
     (:items `(("Restore size" Restore-Size-Fn)
	       ("Move window" Move-Window-Fn)
	       ("Resize window" Resize-Window-Fn)
	       ("Iconify window" Iconify-Window-Fn)
	       ("Full-zoom" Full-Zoom-Fn)
	       ("Lower window" Lower-Window-Fn)
	       ("Kill window" Kill-Window-Fn)))
     (:inactive-items '("Restore size"))
     (:accelerators '((#\R "F2" :F2) (#\M "F3" :F3) (#\s "F4" :F4)
		      (#\I "F5" :F5) (#\F "F6" :F6) (#\L "F7" :F7)
		      (#\K "F8" :F8)))
     (:bar-above-these-items '("Kill window"))
     (:item-font BOLD-FONT)
     (:accel-font BOLD-FONT)
     (:keyboard-selection-p T)
     (:selection-function #'(lambda (gadget item-obj)
			      (declare (ignore gadget))
			      (format t "Selected ~S.~%"
				      (g-value item-obj :item-obj)))))
  (create-instance 'DEMO-MOTIF-MENU-ACCELERATOR-INTER
		   MOTIF-MENU-ACCELERATOR-INTER
     (:window MOTIF-MENU-WIN)
     (:menus (list DEMO-MOTIF-MENU)))
  (opal:add-components MOTIF-MENU-TOP-AGG
		       DEMO-MOTIF-MENU)
  (opal:update MOTIF-MENU-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )


#+garnet-test 
(defun MOTIF-MENU-STOP ()
  (opal:destroy MOTIF-MENU-WIN))

#+garnet-test
(defun Restore-Size-Fn (g v)
  (declare (ignore g v))
  (format t "Restore-Size-Fn called~%~%"))
#+garnet-test
(defun Move-Window-Fn (g v)
  (declare (ignore g v))
  (format t "Move-Window-Fn called~%~%"))
#+garnet-test
(defun Resize-Window-Fn (g v)
  (declare (ignore g v))
  (format t "Resize-Window-Fn called~%~%"))
#+garnet-test
(defun Iconify-Window-Fn (g v)
  (declare (ignore g v))
  (format t "Iconify-Window-Fn called~%~%"))
#+garnet-test
(defun Full-Zoom-Fn (g v)
  (declare (ignore g v))
  (format t "Full-Zoom-Fn called~%~%"))
#+garnet-test
(defun Lower-Window-Fn (g v)
  (declare (ignore g v))
  (format t "Lower-Window-Fn called~%~%"))
#+garnet-test
(defun Kill-Window-Fn (g v)
  (declare (ignore g v))
  (format t "Kill-Window-Fn called~%~%"))
