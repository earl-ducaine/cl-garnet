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
;;;  Scrolling Menu
;;;
;;;  Features and operation of Scrolling-Menu:
;;;     1)  The Scrolling-Menu object is a vertical list of strings framed by
;;;         a white box with a scroll bar on one side.  Moving the indicator
;;;         of the scroll bar causes different subsets of the full list of
;;;         items to appear in the menu frame.  An optional title may
;;;         appear over the menu frame.
;;;     2)  Click the left mouse button on a menu item to select the item,
;;;         or if multiple-selection is enabled, click the left mouse button
;;;         while holding down the shift key to select multiple items.
;;;     3)  If the interim menu feedback is enabled, a box will be drawn
;;;         around the selected item momentarily.
;;;     4)  If the final menu feedback is enabled, the selected item(s) will
;;;         appear in inverse-video.
;;;     5)  The top level :value slot contains a list of strings of the
;;;         currently selected items (or a list of one string, if multiple
;;;         selection is not enabled).
;;;     6)  Items may be manually selected by setting the :selected-ranks slot
;;;         to be a list of the ranks of the desired items as they appear in
;;;         the :items list (the rankings start at 0).
;;;     6)  The :items slot may contain functions to be executed as each
;;;         item is selected, and :menu-selection-function may contain a
;;;         function to be executed when there is a change in the currently
;;;         selected items.
;;;
;;;  Customizable slots:
;;;     1)  Left, top
;;;     2)  Scroll-on-left-p -- Whether to put the scroll bar to the left of
;;;                             the menu frame.  Default = T.
;;;     3)  Min-Scroll-Bar-Width -- The minimum width of the scroll bar.  This
;;;            width will be overridden if it is too narrow to accomodate the
;;;            text in the indicator.  Default = 20.
;;;     4)  Scr-trill-p  --  Whether to have single arrow trill boxes that
;;;                          increment by :scr-incr.  Default = T.
;;;     5)  Page-trill-p --  Whether to have double arrow trill boxes that
;;;                          increment by :page-incr.  Default = T.
;;;     6)  Scr-Incr  --  Value to increment position by in single arrow box.
;;;                       Defalut = 1.
;;;     7)  Page-incr  --  Value to increment postion by in double arrow box.
;;;                        Default = 5.
;;;     8)  Indicator-text-p -- Whether to report indicator position
;;;                             numerically in the indicator.  Default = NIL.
;;;     9)  Int-scroll-feedback-p  --  Whether a thick-lined box should follow
;;;            the mouse when moving the indicator.  A value of NIL causes the
;;;            indicator to follow the mouse directly.  Default = NIL.
;;;    10)  Indicator-font -- Font to report indicator position with.
;;;                           Default = small, fixed, roman font.
;;;    11)  Scroll-selection-function -- A function to be executed whenever
;;;            the scroll bar is moved.  Parameters are the SCROLL-BAR of
;;;            the scrolling menu and the VALUE of the scroll bar.
;;;    12)  Min-frame-width -- The minimum width of the frame around the menu.
;;;                            Default = 0.
;;;    13)  V-spacing -- Distance between menu items.  Default = 6.
;;;    14)  H-align -- How to justify the items (:left, :center, or :right).
;;;                    Default = :left.
;;;    15)  Multiple-p -- whether to allow multiple selections from the menu
;;;                       (using shift-leftdown).  Default = T.
;;;    16)  Items -- This can be: 
;;;                  A list of strings, as in '("Large" ...), or
;;;                  a list of atoms, as in '(:center ...), or
;;;                  a list of string/function pairs, '(("Cut" Cut-FN) ...), or
;;;                  a list of atom/function pairs, '((:center Center-FN) ...).
;;;                  Each function will be executed when the associated button
;;;                  becomes selected.  The parameters are the top-level
;;;                  GADGET and the ITEM-STRING.
;;;    17)  Item-To-String-Function -- a function which takes an ITEM and
;;;            returns a string to be displayed in the menu corresponding to
;;;            that item.  For example, if the :items slot contains a list of
;;;            Garnet schemas, then the function would return the name of a
;;;            schema.  If each item is a string/fn or atom/fn pair, only the
;;;            CAR of the pair is sent to the :item-to-string-function.
;;;            The default function assumes that :items contains a list of
;;;            strings.
;;;    18)  Num-visible -- Number of items to show in the menu.  Default = 5.
;;;            Note:  When s-valued, the following function call must be made
;;;            afterwards:  (opal:notice-items-changed your-sm-obj)
;;;    19)  Int-menu-feedback-p -- Whether to cause a box outline to appear
;;;            around an item during selection.  Default = T.
;;;    20)  Final-feedback-p -- Whether to cause the selected item(s) to
;;;                             appear in inverse-video.  Default = T.
;;;    21)  Text-offset -- Distance from the longest text to the menu frame.
;;;                        Default = 4.
;;;    22)  Title -- a string to appear in inverse at the top of the menu
;;;                  (a value of NIL implies no title will appear).
;;;                  Default = NIL.
;;;    23)  Title-Font and Item-Font
;;;    24)  Selected-Ranks -- A list of indices corresponding to the ranks of
;;;            the currently selected items in the :items list (the first item
;;;            in the :items list has rank 0).  This slot may be set to select
;;;            items and formulas may depend on it.
;;;    25)  Menu-selection-function -- Global function to be executed whenever
;;;            there is a change in the list of currently selected items.
;;;            Parameters are the top-level SCROLLING-MENU gadget and the
;;;            ITEM-OBJ that was just selected.  (The name of the item that
;;;            was just selected can be obtained by g-value'ing the :item slot
;;;            of the ITEM-OBJ.)
;;;
;;;  Scrolling-Menu demo:
;;;     This module contains a function which creates a window and a scrolling
;;;     menu.  To run it, enter (GARNET-GADGETS:scrolling-menu-go).
;;;     To stop, enter (GARNET-GADGETS:scrolling-menu-stop).
;;;
;;;  NOTE:  This module requires the scroll bar module and all of its parts
;;;         modules.  Thus, GAD-scroll-parts, GAD-v-arrows, GAD-v-boxes,
;;;         and v-scroll-bar must be loaded before this module.
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Scrolling-Menu))
  #+garnet-test
  (export '(Scrolling-Menu-Go Scrolling-Menu-Stop
	    Scrolling-Menu-Obj Scrolling-Menu-Win Scrolling-Menu-Top-Agg)))

(create-instance 'SCROLLING-MENU-FRAME opal:rectangle
   (:filling-style opal:white-fill)
   (:left (o-formula (gv (kr-path 0 :parent) :frame-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:width (o-formula (gv (kr-path 0 :parent) :frame-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :frame-height))))

(create-instance 'SCROLLING-MENU-INTERIM-FEEDBACK opal:rectangle
   (:left (o-formula (+ 1 (gv (kr-path 0 :parent) :frame-left))))
   (:top (o-formula (if (gvl :obj-over :prev)
			(gvl :obj-over :top)
			(+ (gvl :obj-over :top) 1))))
   (:width (o-formula (- (gv (kr-path 0 :parent) :frame-width) 2)))
   (:height (o-formula (if (and (gvl :obj-over :prev)
				(gvl :obj-over :next))
			   (gvl :obj-over :height)
			   (- (gvl :obj-over :height) 1))))
   (:draw-function :xor)
   (:fast-redraw-p T)
   (:visible (o-formula (and (gv (kr-path 0 :parent) :int-menu-feedback-p)
			     (gvl :obj-over)))))

(create-instance 'SCROLLING-MENU-TITLE opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :frame-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:title (o-formula (gv (kr-path 0 :parent) :title)))
   (:font (o-formula (gv (kr-path 0 :parent) :title-font)))
   (:string-width (o-formula (if (gvl :title)
				 (+ (gvl :text :width)
				    (* 2 (gv (kr-path 0 :parent) :text-offset)))
				 0)))
   (:width (o-formula (gv (kr-path 0 :parent) :frame-width)))
   (:height (o-formula (if (gvl :title) (gvl :parent :title-height) 0)))
   (:visible (o-formula (gvl :title)))
   (:parts
    `((:text ,opal:text
             (:constant (:actual-heightp))
	     (:left ,(o-formula (- (+ (gv (kr-path 0 :parent) :left)
				      (floor (gv (kr-path 0 :parent) :width) 2))
				   (floor (gvl :width) 2))))
	     (:top ,(o-formula (+ (gv (kr-path 0 :parent) :top)
				  (floor (gv (kr-path 1 :parent :parent)
					     :v-spacing) 2))))
	     (:string ,(o-formula (gv (kr-path 0 :parent) :title)))
	     (:font ,(o-formula (gv (kr-path 0 :parent) :font))))
      (:rect ,opal:rectangle
	     (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
	     (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
	     (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
	     (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
	     (:line-style ,opal:line-2)))))

(create-instance 'SCROLLING-MENU-TEXT-LABEL-PROTOTYPE opal:text
  (:constant '(:actual-heightp))
  (:known-as :text)
  (:left (o-formula
	  (let* ((p0 (kr-path 0 :parent))
		 (p1 (kr-path 1 :parent :parent :parent))
		 (text-offset (gv p1 :text-offset)))
	    (case (gv p1 :h-align)
	      (:left (+ (gv p0 :left) text-offset))
	      (:center (- (+ (gv p0 :left) (floor (gv p0 :width) 2))
			  (floor (gvl :width) 2)))
	      (:right (- (+ (gv p0 :left) (gv p0 :width))
			 (gvl :width) text-offset))))))
  (:top (o-formula (opal:gv-center-y-is-center-of (gvl :parent))))
  (:string (o-formula (let* ((p (kr-path 0 :parent))
			     (fn (gv p :item-to-string-function)))
			(declare (ignore fn))
			(kr-send p :item-to-string-function (gv p :item)))))
  (:font (o-formula (gv (kr-path 0 :parent :parent :parent) :item-font))))


(create-instance 'SCROLLING-MENU-ITEM opal:aggregadget
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (+ (gv (kr-path 0 :parent) :top)
		       (* (gvl :rank)
			  (gv (kr-path 1 :parent :parent) :item-height)))))
   (:width (o-formula (gv (kr-path 0 :parent :parent) :frame-width)))
   (:height (o-formula (gv (kr-path 0 :parent :parent) :item-height)))
   (:item (o-formula (let ((element (nth (gvl :rank)
					 (gv (kr-path 0 :parent) :items))))
		       (if (gv (kr-path 1 :parent :parent) :actions-p)
			   (first element)
			   element))))
   (:real-rank (o-formula (+ (gvl :rank) (gv (kr-path 0 :parent :parent) :start))))
   (:highlighted (o-formula (and (gv (kr-path 0 :parent :parent) :final-feedback-p)
				 (member (gvl :real-rank)
					 (gv (kr-path 0 :parent :parent)
					     :selected-ranks)))))
   (:visible (o-formula (not (equal (gvl :text :string) ""))))
   (:item-to-string-function (o-formula (gv (kr-path 0 :parent :parent)
					     :item-to-string-function)))
   (:parts
    `((:text ,#'opal::Panel-Get-Label)
      (:final-feedback ,opal:rectangle
	     (:left ,(o-formula (+ 1 (gv (kr-path 0 :parent) :left))))
	     (:top ,(o-formula (let ((p (kr-path 0 :parent)))
				 (if (gv p :prev)
				     (gv p :top)
				     (+ (gv p :top) 1)))))
	     (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 2)))
	     (:height ,(o-formula (let ((p (kr-path 0 :parent)))
				    (if (and (gv p :prev) (gv p :next))
					(gv p :height)
					(- (gv p :height) 1)))))
	     (:draw-function :xor)
	     (:fast-redraw-p T)
	     (:filling-style ,opal:black-fill)
	     (:visible ,(o-formula (and (gv (kr-path 0 :parent) :visible)
					(gv (kr-path 0 :parent) :highlighted))))))))

(create-instance 'SCROLLING-MENU-ITEM-LIST opal:aggrelist
   (:left (o-formula (gv (kr-path 0 :parent) :frame-left)))
   (:top (o-formula (+ (gv (kr-path 0 :parent) :top)
			   (gv (kr-path 1 :parent :menu-title) :height))))
   (:width (o-formula (gv (kr-path 0 :parent) :frame-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :items-height)))
   (:direction NIL)
   (:items (o-formula (gv (kr-path 0 :parent) :visible-items)))
   (:item-to-string-function (o-formula (gv (kr-path 0 :parent)
					    :item-to-string-function)))
   (:item-prototype scrolling-menu-item))

(create-instance 'SCROLLING-MENU-SCROLL-BAR garnet-gadgets:v-scroll-bar
   (:left (o-formula (gv (kr-path 0 :parent) :scroll-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:height (o-formula (gv (kr-path 0 :parent) :frame-height)))
   (:val-1 0)
   (:val-2 (o-formula (gv (kr-path 0 :parent) :val-2)))
   (:min-width (o-formula (gv (kr-path 0 :parent) :min-scroll-bar-width)))
   (:scr-trill-p (o-formula (gv (kr-path 0 :parent) :scr-trill-p)))
   (:page-trill-p (o-formula (gv (kr-path 0 :parent) :page-trill-p)))
   (:scr-incr (o-formula (gv (kr-path 0 :parent) :scr-incr)))
   (:page-incr (o-formula (gv (kr-path 0 :parent) :page-incr)))
   (:int-feedback-p (o-formula (gv (kr-path 0 :parent) :int-scroll-feedback-p)))
   (:indicator-text-p (o-formula (gv (kr-path 0 :parent) :indicator-text-p)))
   (:scroll-p (o-formula (gv (kr-path 0 :parent) :scroll-p)))
   (:indicator-font (o-formula (gv (kr-path 0 :parent) :indicator-font)))
   (:selection-function (o-formula (gv (kr-path 0 :parent)
				       :scroll-selection-function))))

(create-instance 'SCROLLING-MENU-SELECTOR inter:menu-interactor
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula (list :element-of
				   (gvl :operates-on :menu-item-list))))
   (:feedback-obj (o-formula (gvl :operates-on :feedback-obj)))
   (:start-event (o-formula (if (gvl :operates-on :multiple-p)
				 '(:leftdown :shift-leftdown)
				 :leftdown)))
   (:how-set :toggle)
   (:stop-action
    #'(lambda (interactor obj-under-mouse)
	(let* ((rank (g-value obj-under-mouse :real-rank))
	       (gadget (g-value interactor :operates-on))
	       (action (when (g-value gadget :actions-p)
			 (second (nth rank (g-value gadget :items)))))
	       (selected-ranks (g-value gadget :selected-ranks))
	       (feedback-obj (g-value interactor :feedback-obj))
	       (new-selected (case (g-value interactor :start-char)
			       (:shift-leftdown
				(set-exclusive-or (list rank) selected-ranks))
			       (:leftdown
				(if (g-value gadget :toggle-p)
				    (set-difference (list rank) selected-ranks)
				    (list rank))))))

	  (s-value feedback-obj :obj-over NIL)
	  (s-value gadget :selected-ranks new-selected)
	
	  ;; Global function executed whenever selections change
	  (kr-send gadget :menu-selection-function
		   gadget
		   obj-under-mouse) 

	  ;; Local function assigned to item
	  (when action
	    (when (member rank new-selected)
	      (funcall action
		       gadget
		       (g-value obj-under-mouse :text :string))))
	  ))))

(create-instance 'SCROLLING-MENU opal:aggregadget
  :declare ((:parameters :left :top :scroll-on-left-p :min-scroll-bar-width
			 :scr-trill-p :page-trill-p :indicator-text-p :scr-incr
			 :page-incr :int-scroll-feedback-p :indicator-font
			 :scroll-selection-function :min-frame-width :v-spacing
			 :h-align :multiple-p :toggle-p :items
			 :item-to-string-function :item-font :num-visible
			 :int-menu-feedback-p :final-feedback-p :text-offset
			 :title :title-font :menu-selection-function
			 :selected-ranks :value :visible)
	    (:type (kr-boolean :scroll-on-left-p :scr-trill-p :page-trill-p
		    :indicator-text-p :int-scroll-feedback-p :multiple-p
		    :toggle-p :int-menu-feedback-p :final-feedback-p)
		   ((integer 0) :min-scroll-bar-width :min-frame-width
		    :num-visible)
		   (integer :scr-incr :page-incr :v-spacing :text-offset)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :indicator-font :item-font :title-font)
		   ((or null function symbol) :scroll-selection-function
		    :menu-selection-function)
		   ((member :left :center :right) :h-align)
		   (items-type :items)
		   (list :selected-ranks :value)
		   ((or function symbol) :item-to-string-function)
		   ((or null string) :title))
	    (:maybe-constant :left :top :scroll-on-left-p :min-scroll-bar-width
			     :scr-trill-p :page-trill-p :indicator-text-p
			     :scr-incr :page-incr :int-scroll-feedback-p
			     :indicator-font :min-frame-width :v-spacing
			     :h-align :multiple-p :items
			     :item-to-string-function :int-menu-feedback-p
			     :item-font :num-visible :final-feedback-p
			     :text-offset :title :title-font :visible))
   ;;; Customizable slots
   ;;;
   (:left 0) (:top 0)
   (:toggle-p T)

   ;; Scroll bar slots
   (:scroll-on-left-p T)
   (:min-scroll-bar-width 20)
   (:scr-trill-p T)
   (:page-trill-p T)
   (:indicator-text-p NIL)
   (:scr-incr 1)
   (:page-incr 5)
   (:int-scroll-feedback-p NIL)
   (:indicator-font (opal:get-standard-font NIL NIL :small))
   (:scroll-selection-function NIL)

   ;; Menu slots
   (:min-frame-width 0)
   (:v-spacing 6)
   (:h-align :left)     ; Implemented in MENU-ITEM code, not through aggrelists
   (:multiple-p T)
   (:items '("Item 1" "Item 2" "Item 3" "Item 4" "Item 5" "Item 6" "Item 7"
	     "Item 8" "Item 9" "Item 10" "Item 11" "Item 12" "Item 13"
	     "Item 14" "Item 15" "Item 16" "Item 17" "Item 18" "Item 19"
	     "Item 20"))
   (:item-to-string-function
    #'(lambda (item)
	(if item
	    (if (or (stringp item) (schema-p item))
		item
		(string-capitalize (string-trim ":" item)))
	    "")))
   (:item-font opal:default-font)
   (:num-visible 5)
   (:int-menu-feedback-p T)
   (:final-feedback-p T)
   (:text-offset 4)
   (:title NIL)
   (:title-font (opal:get-standard-font :serif :roman :large))
   (:menu-selection-function NIL)
   (:selected-ranks NIL)

   ;; Generally non-customizable slots
   ;;
   (:value (o-formula (mapcar #'(lambda (rank)
				  (if (gvl :actions-p)
				      (first (nth rank (gvl :items)))
				      (nth rank (gvl :items))))
			      (gvl :selected-ranks))))
   (:scroll-p (o-formula (> (length (gvl :items)) (gvl :num-visible))))
   (:actions-p (o-formula (and (car (gvl :items)) (listp (car (gvl :items))))))
   (:start (o-formula (if (gvl :scroll-p)
			  (gvl :scroll-bar :value)
			  0)))
   (:end (o-formula (+ (gvl :start) (MIN (length (gvl :items))
					 (gvl :num-visible)))))
   (:visible-items (o-formula (subseq (gvl :items) (gvl :start)
				      (min (length (gvl :items))
					   (gvl :end)))))
   (:max-item-width
    (o-formula
     (do* ((fn (gvl :item-to-string-function))
	   (items (if (gvl :actions-p)
		      (mapcar #'car (gvl :items))
		      (gvl :items)))
	   (font (gvl :item-font))
	   (items-len (length items))
	   (i 0 (+ i 1))
	   (item (nth i items) (nth i items))
	   (str (kr-send (gv :self) :item-to-string-function item)
		(kr-send (gv :self) :item-to-string-function item))
	   (str-width (if (schema-p str)
			  (g-value str :width)
			  (opal:string-width font str))
		      (if (schema-p str)
			  (g-value str :width)
			  (opal:string-width font str)))
	   (max-width str-width
		      (if (> str-width max-width) str-width max-width)))
	  ((= i items-len) (+ max-width (* 2 (gvl :text-offset))))
       (declare (ignore fn)))))
   (:scroll-left-on-right (o-formula (+ (gvl :left) (gvl :frame-width))))
   (:scroll-left (o-formula (if (gvl :scroll-on-left-p)
				(gvl :left)
				(gvl :scroll-left-on-right))))
   (:val-2 (o-formula (if (gvl :scroll-p)
			  (- (length (gvl :items)) (gvl :num-visible))
			  1)))
   (:frame-left-on-right (o-formula (+ (gvl :left)
				       (gvl :scroll-bar :width))))
   (:frame-left (o-formula (if (gvl :scroll-on-left-p)
			       (gvl :frame-left-on-right)
			       (gvl :left))))
   (:frame-width (o-formula (MAX (gvl :menu-title :string-width)
				 (gvl :max-item-width)
				 (gvl :min-frame-width))))
   (:frame-height (o-formula (+ (gvl :title-height) (gvl :items-height))))
   (:title-height (o-formula (if (gvl :title)
				 (+ (gvl :v-spacing)
				    (opal:string-height (gvl :title-font) "X"))
				 0)))
   (:item-height (o-formula
		  (+ (gvl :v-spacing)
		     (MAX (let ((max-schema-height 0))
			    (dolist (s (gvl :strings))
			      (if (schema-p s)
				  (setq max-schema-height
					(MAX max-schema-height
					     (g-value s :height)))))
			    max-schema-height)
			  (opal:string-height (gvl :item-font) "X")))))
   (:items-height (o-formula (* (gvl :num-visible) (gvl :item-height))))
   (:width (o-formula (+ (gvl :frame-width) (gvl :scroll-bar :width))))
   (:height (o-formula (+ (gvl :items-height) (gvl :title-height))))
   (:text-label-prototype SCROLLING-MENU-TEXT-LABEL-PROTOTYPE)
   (:parts
    `((:scroll-bar ,scrolling-menu-scroll-bar)
      (:frame ,scrolling-menu-frame)
      (:feedback-obj ,scrolling-menu-interim-feedback)
      (:menu-title ,scrolling-menu-title)
      (:menu-item-list ,scrolling-menu-item-list)))
   (:interactors
    `((:selector ,scrolling-menu-selector))))


(define-method :add-local-item SCROLLING-MENU (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item :menu-item-list args))
(define-method :add-item SCROLLING-MENU (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item :menu-item-list args))
   
(define-method :remove-local-item SCROLLING-MENU
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item :menu-item-list key))
(define-method :remove-item SCROLLING-MENU
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item :menu-item-list key))

(s-value SCROLLING-MENU :change-item (g-value opal:aggrelist :change-item))
(s-value SCROLLING-MENU :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

(define-method :string-set-func SCROLLING-MENU
    (gadget-obj str-obj final-event final-string)
  (if (eq str-obj (g-value gadget-obj :menu-title :text))
      ; then is title
      (opal::set-one-value gadget-obj :title final-string)
      ; else is a menu item
      (let ((aggrel (g-value gadget-obj :MENU-ITEM-LIST)))
	(opal::Aggrelist-Edit-String-Func gadget-obj aggrel str-obj final-event
					  final-string :real-rank))))

;;;
;;;  FUNCTION TO DISPLAY SCROLLING MENU IN A WINDOW
;;;

#+garnet-test 
(defun Report-Change (gadget item-object)
  (let ((item-string (g-value item-object :item)))
    (format t "~%Clicked on string ~S in gadget ~S.~%" item-string gadget)))

#+garnet-test (defparameter scrolling-menu-win NIL)
#+garnet-test (defparameter scrolling-menu-top-agg NIL)
#+garnet-test (defparameter scrolling-menu-obj NIL)

#+garnet-test
(defun Scrolling-Menu-Go ()
  (create-instance 'scrolling-menu-win inter:interactor-window
     (:left 700)(:top 5)(:height 360)(:width 300))
  (s-value scrolling-menu-WIN
	   :aggregate
	   (create-instance 'scrolling-menu-top-agg opal:aggregate))
  (create-instance 'scrolling-menu-obj scrolling-menu
     (:left 30) (:top 30)
     (:title "Menu")
     (:menu-selection-function #'Report-Change)
     (:items '(("Geneva" geneva-fn) ("Times" times-fn)
	       ("Roman" roman-fn) ("Courier" courier-fn)
	       ("Helvetica" helvetica-fn) ("Chicago" chicago-fn)
	       ("Symbol" symbol-fn) ("Monaco" monaco-fn)
	       ("Venice" venice-fn) ("Gothic" gothic-fn)
	       ("Celtic" celtic-fn))))
  (opal:add-components scrolling-menu-top-agg scrolling-menu-Obj)
  (opal:update scrolling-menu-WIN))

#+garnet-test 
(defun Scrolling-Menu-Stop ()
  (opal:destroy scrolling-menu-win))


;;;  These functions are included to show that selection of one of the
;;;  menu items causes the associated local function to be called.

#+garnet-test
(defun geneva-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function GENEVA-FN called~%"))
#+garnet-test
(defun times-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function TIMES-FN called~%"))
#+garnet-test
(defun roman-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function ROMAN-FN called~%"))
#+garnet-test
(defun courier-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function COURIER-FN called~%"))
#+garnet-test
(defun helvetica-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function HELVETICA-FN called~%"))
#+garnet-test
(defun chicago-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function CHICAGO-FN called~%"))
#+garnet-test
(defun symbol-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function SYMBOL-FN called~%"))
#+garnet-test
(defun monaco-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function MONACO-FN called~%"))
#+garnet-test
(defun venice-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function VENICE-FN called~%"))
#+garnet-test
(defun gothic-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function GOTHIC-FN called~%"))
#+garnet-test
(defun celtic-fn (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function CELTIC-FN called~%"))

