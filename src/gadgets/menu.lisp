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
;;;  Menu
;;;
;;;  Features and operation of Menu:
;;;     1)  The Menu object is a vertical list of strings framed by a white
;;;         box.  An optional title may appear at the top of the menu in
;;;         inverse video.
;;;     2)  Click the left mouse button on a menu item to select the item.
;;;     3)  A box will be drawn around the selected item momentarily.
;;;     4)  The top level :value slot points to the string of the currently
;;;         selected item.
;;;     5)  The top level :value-obj slot points to the currently selected
;;;         item object, and can be set directly with S-VALUE to select an
;;;         item.
;;;     6)  The :items slot may contain functions to be executed as each
;;;         item is selected, and :selection-function may contain a function
;;;         to be executed when any item selected.
;;;
;;;  Customizable slots:
;;;     1)  Left, top
;;;     2)  V-spacing -- distance between menu items
;;;     3)  H-align -- how to justify the items (:left, :center, or :right)
;;;     3)  Shadow-offset -- the amount of shadow that shows under the menu
;;;     4)  Text-offset -- the distance from the longest text to the menu frame
;;;     4)  Title -- a string to appear in inverse at the top of the menu
;;;                  (a value of NIL implies no title will appear)
;;;     5)  Title-Font and Item-Font
;;;     6)  Items -- This can be: 
;;;                  A list of strings, as in '("Large" ...), or
;;;                  a list of atoms, as in '(:center ...), or
;;;                  a list of string/function pairs, '(("Cut" Cut-FN) ...), or
;;;                  a list of atom/function pairs, '((:center Center-FN) ...).
;;;                  Each function will be executed when the associated button
;;;                  becomes selected.  The parameters are the top-level
;;;                  GADGET and the ITEM-STRING.
;;;     7)  Selection-function -- Global function to be executed when any
;;;                               button is selected.  Parameters are the
;;;                               top-level GADGET and the ITEM-STRING.
;;;
;;;     NOTE:  When the menu object is exported (as in the demo function
;;;            below), slots can be changed on the fly as well when the objects
;;;            are created.
;;;
;;;  Menu demo:
;;;     This module contains a function which creates a window and a menu.
;;;     To run it, enter (GARNET-GADGETS:menu-go).
;;;     To stop, enter (GARNET-GADGETS:menu-stop).
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

;;; CHANGE LOG
;;; 05/20/93 Andrew Mickish - Set direction of aggrelist to NIL; adjusted
;;;             constant declarations
;;; 05/13/93 Andrew Mickish - :prev-visible ---> :prev
;;; 12/14/92 Andrew Mickish - Added type and parameter declarations
;;; 11/30/92 Andrew Mickish - Added :item-to-string-function
;;; 10/28/92 Andrew Mickish - Added :min-menu-width
;;; 06/16/92 Andrew Mickish - Added objects in :items list
;;; 04/17/92 Andrew Mickish - Now final-feedback objs are invisible when the
;;;             parent gadget is invisible
;;; 02/11/92 Andrew Mickish - Added :maybe-constant list
;;; 01/28/92 Ed Pervin - Had to wrap (function ...) around lambda for CMUCL.
;;; 05/06/91 Andrew Mickish - Put new :width formula in MENU-ITEM-LIST
;;; 04/17/91 Andrew Mickish - Changed MENU's :frame-width formula to look at
;;;            the :width of the aggrelist instead of the :max-width
;;;

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Menu))
  #+garnet-test
  (export '(Menu-Go Menu-Stop Menu-Obj Menu-Top-Agg Menu-Win)))

(create-instance 'MENU-SHADOW-RECT opal:rectangle
   (:left (o-formula (+ (gv (kr-path 0 :parent) :left)
			(gv (kr-path 0 :parent) :shadow-offset))))
   (:top (o-formula (+ (gv (kr-path 0 :parent) :top)
		       (gv (kr-path 0 :parent) :shadow-offset))))
   (:width (o-formula (gv (kr-path 0 :parent :frame) :width)))
   (:height (o-formula (gv (kr-path 0 :parent :frame) :height)))
   (:filling-style opal:black-fill))

(create-instance 'MENU-FRAME opal:rectangle
   (:filling-style opal:white-fill)
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:width (o-formula (gv (kr-path 0 :parent) :frame-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :frame-height))))

(create-instance 'MENU-FEEDBACK-RECT opal:rectangle
   (:left (o-formula (+ 1 (gvl :obj-over :left))))
   (:top (o-formula (+ 1 (gvl :obj-over :top))))
   (:width (o-formula (- (gvl :obj-over :width) 2)))
   (:height (o-formula (- (gvl :obj-over :height) 2)))
   (:visible (o-formula (and (gvl :parent :visible) (gvl :obj-over)))))

(create-instance 'MENU-TITLE opal:aggregadget
   (:text-offset (o-formula (gv (kr-path 0 :parent) :text-offset)))
   (:text-offset2 (o-formula (gv (kr-path 0 :parent) :text-offset2)))
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :top)))
   (:title (o-formula (gv (kr-path 0 :parent) :title)))
   (:font (o-formula (gv (kr-path 0 :parent) :title-font)))
   (:width (o-formula (gv (kr-path 0 :parent) :frame-width)))
   (:height (o-formula (if (gvl :title)
			   (+ (gvl :text :height) (gvl :text-offset2))
			   0)))
   (:visible (o-formula (and (gvl :parent :visible) (gvl :title))))
   (:parts
    `((:rect ,opal:rectangle
	     (:left ,(o-formula (+ 1 (gv (kr-path 0 :parent) :left))))
	     (:top ,(o-formula (+ 1 (gv (kr-path 0 :parent) :top))))
	     (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 2)))
	     (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 2)))
	     (:filling-style ,opal:black-fill)
	     (:visible ,(o-formula (gv (kr-path 0 :parent) :visible))))
      (:text ,opal:text
             (:constant (:actual-heightp))
	     (:left ,(o-formula (- (+ (gv (kr-path 0 :parent) :left)
				      (floor (gv (kr-path 0 :parent) :width) 2))
				   (floor (gvl :width) 2))))
	     (:top ,(o-formula (+ (gv (kr-path 0 :parent) :top)
				  (gv (kr-path 0 :parent) :text-offset))))
	     (:string ,(o-formula (gv (kr-path 0 :parent) :title)))
	     (:font ,(o-formula (gv (kr-path 0 :parent) :font)))
	     (:line-style ,opal:white-line)
	     (:visible ,(o-formula (gv (kr-path 0 :parent) :visible)))))))


(create-instance 'MENU-ITEM opal:aggregadget
   (:left (o-formula (gvl :parent :parent :left)))
   (:top (o-formula (if (gvl :prev)
			(+ (gvl :prev :top) (gvl :prev :height))
			(gvl :parent :top))))
   ;; Conditional formulas are required to allow either a list of strings or
   ;; a list of string/function pairs in the :items slot.
   (:item (o-formula (let ((element (nth (gvl :rank)
					 (gv (kr-path 0 :parent) :items))))
		       (if (gv (kr-path 1 :parent :parent) :actions-p)
			   (first element)
			   element))))
   (:action (o-formula (if (gv (kr-path 0 :parent :parent) :actions-p)
			   (second (nth (gvl :rank)
					(gv (kr-path 1 :parent) :items))))))
   
   (:item-to-string-function (o-formula (gv (kr-path 0 :parent)
					     :item-to-string-function)))
   (:font (o-formula (gv (kr-path 0 :parent :parent) :item-font)))
   (:text-offset (o-formula (gv (kr-path 0 :parent :parent) :text-offset)))
   (:h-align (o-formula (gv (kr-path 0 :parent :parent) :h-align)))
   (:max-text-width-thus-far
       (o-formula (if (gvl :prev)
		      (MAX (gvl :prev :max-text-width-thus-far)
			   (gvl :text :width))
		      (gvl :text :width))))
   (:max-text-height-thus-far
       (o-formula (if (gvl :prev)
		      (MAX (gvl :prev :max-text-height-thus-far)
			   (gvl :text :height))
		      (gvl :text :height))))
   ;; These slots are used by the parent aggrelist in calculating its own
   ;; :max-width and :height slots.
   (:width (o-formula (let ((p (kr-path 0 :parent :parent)))
			(MAX (gv p :title-width)
			     (gv p :min-menu-width)
			     (+ (gvl :parent :tail :max-text-width-thus-far)
				(* 2 (gv p :text-offset)))))))
   (:height (o-formula (+ (gvl :parent :tail :max-text-height-thus-far)
			  (floor (gvl :parent :v-spacing) 2))))
   ;; An aggregadget is implemented for each text item so that there is no
   ;; "dead" space between menu items.
   (:parts
    `((:text ,#'opal::Panel-Get-Label))))

(create-instance 'MENU-ITEM-LIST opal:aggrelist
   (:constant '(:fixed-width-p :fixed-height-p :rank-margin :pixel-margin
		:direction :h-spacing :indent))
   (:left (o-formula (gv (kr-path 0 :parent) :left)))
   (:top (o-formula (gv (kr-path 0 :parent) :items-top)))
   (:width (o-formula (gvl :tail :width)))
   (:direction NIL)
   (:v-spacing (o-formula (gv (kr-path 0 :parent) :v-spacing)))
   (:item-to-string-function (o-formula (gv (kr-path 0 :parent)
					     :item-to-string-function)))
   (:items (o-formula (gv (kr-path 0 :parent) :items)))
   (:item-prototype menu-item))

(create-instance 'MENU opal:aggregadget
   :declare ((:parameters :left :top :v-spacing :h-align :shadow-offset
			  :text-offset :title :title-font :items :item-font
			  :value-obj :value :item-to-string-function
			  :selection-function :visible)
	     (:type (integer :v-spacing :shadow-offset :text-offset)
		    ((integer 0) :min-menu-width)
		    ((member :left :center :right) :h-align)
		    ((or null string) :title)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :title-font :item-font)
		    (list :items)
		    ((or null string keyword (satisfies schema-p)) :value)
		    ((or null (satisfies schema-p)) :value-obj)
		    ((or function symbol) :item-to-string-function)
		    ((or null function symbol) :selection-function))
	     (:maybe-constant :left :top :v-spacing :h-align :shadow-offset
			      :text-offset :title :title-font :items :item-font
			      :item-to-string-function :min-menu-width
			      :visible))
   ;; Customizable slots
   ;;
   (:left 0) (:top 0) 
   (:v-spacing 10)
   (:h-align :left)     ; Implemented in MENU-ITEM code, not through aggrelists
   (:shadow-offset 5)
   (:text-offset 4)
   (:title NIL)
   (:title-font (opal:get-standard-font :serif :roman :large))
   (:min-menu-width 0)
   (:items '("Item 1" "Item 2" "Item 3" "Item 4"))
   (:item-font opal:default-font)
   (:item-to-string-function
    #'(lambda (item)
	(if item
	    (if (or (stringp item) (schema-p item))
		item
		(string-capitalize (string-trim ":" item)))
	    "")))
   (:selection-function NIL)
   (:value-obj NIL)
   (:value (o-formula (gvl :value-obj :item)))

   ;; Generally non-customizable slots
   ;;
   (:actions-p (o-formula (consp (first (gvl :items)))))
   (:items-top (o-formula (+ (gvl :top) (gvl :menu-title :height))))
   (:text-offset2 (o-formula (* 2 (gvl :text-offset))))
   (:title-width (o-formula (if (gvl :title)
				(+ (gvl :text-offset2)
				   (opal:string-width (gvl :title-font)
						      (gvl :title)))
				0)))
   (:frame-width (o-formula (MAX (gvl :menu-item-list :width)
				 (gvl :title-width))))
   (:frame-height (o-formula (+ (gvl :menu-title :height)
				(gvl :menu-item-list :height))))
   (:width (o-formula (+ (gvl :frame-width) (gvl :shadow-offset))))
   (:height (o-formula (+ (gvl :frame-height) (gvl :shadow-offset))))
   (:text-label-prototype 
    (create-instance NIL opal:text
      (:constant '(:actual-heightp))
      (:left (o-formula (case (gvl :parent :h-align)
			  (:left (+ (gvl :parent :left)
				    (gvl :parent :text-offset)))
			  (:center (opal:gv-center-x-is-center-of
				    (gvl :parent)))
			  (:right (- (+ (gvl :parent :left)
					(gvl :parent :width))
				     (gvl :width)
				     (gvl :parent :text-offset))))))
      (:top (o-formula (opal:gv-center-y-is-center-of (gvl :parent))))
      (:string (o-formula (let* ((p (kr-path 0 :parent))
				 (fn (gv p :item-to-string-function)))
			    (declare (ignore fn))
			    (kr-send p :item-to-string-function
				     (gv p :item)))))
      (:font (o-formula (gv (kr-path 0 :parent) :font)))))
   (:parts
    `((:shadow ,menu-shadow-rect)
      (:frame ,menu-frame)
      (:feedback ,menu-feedback-rect)
      (:menu-title ,menu-title)
      (:menu-item-list ,menu-item-list)))
   (:interactors
    `((:selector ,inter:menu-interactor
		 (:window ,(o-formula (gv-local :self :operates-on :window)))
		 (:start-where ,(o-formula (list :element-of
						 (gvl :operates-on
						      :menu-item-list))))
		 (:running-where ,(o-formula (list :element-of
						   (gvl :operates-on
							:menu-item-list))))
		 (:how-set NIL)
		 (:feedback-obj ,(o-formula (gvl :operates-on :feedback)))
		 (:stop-action
		  ,#'(lambda (interactor obj-under-mouse)
		       (let ((action (g-value obj-under-mouse :action))
			     (gadget (g-value interactor :operates-on))
			     (value (g-value obj-under-mouse :item)))
			 
			 (s-value (g-value gadget :feedback) :obj-over NIL)
			 (s-value gadget :value-obj obj-under-mouse)
			 
			 ;; Global function for all items
			 (kr-send gadget :selection-function gadget value)
			 
			 ;; Local function assigned to item
			 (if action
			     (funcall action gadget value)))))))))

(define-method :add-local-item MENU (gadget item &rest args)
  (opal::Gadget-Add-Local-Item gadget item :menu-item-list args))
(define-method :add-item MENU (gadget item &rest args)
  (opal::Gadget-Add-Item gadget item :menu-item-list args))
   
(define-method :remove-local-item MENU
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Local-Item gadget item :menu-item-list key))
(define-method :remove-item MENU
               (gadget &optional item &key (key #'opal:no-func))
  (opal::Gadget-Remove-Item gadget item :menu-item-list key))

(s-value MENU :change-item (g-value opal:aggrelist :change-item))
(s-value MENU :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))

#|
(define-method :notice-items-changed MENU
               (gadget &optional no-propagation)
  (opal:notice-items-changed (g-value gadget :menu-item-list)
			     no-propagation))
|#

;;;
;;;  MENU-GO
;;;

#+garnet-test (defparameter Menu-win NIL)
#+garnet-test (defparameter Menu-top-agg NIL)
#+garnet-test (defparameter Menu-Obj NIL)

#+garnet-test
(defun Report-Item (top-level-obj value)
  (let ((value-obj (g-value top-level-obj :value-obj)))
    (format t "Menu-item object ~S selected with value ~S.~%"
	    value-obj value)))

#+garnet-test
(defun Menu-Go ()
  (create-instance 'menu-win inter:interactor-window
     (:top 5)(:left 700)(:height 360)(:width 300))
  (s-value Menu-win
	   :aggregate
	   (create-instance 'menu-top-agg opal:aggregate))
  (create-instance 'menu-obj Menu
     (:left 20) (:top 20)
     (:selection-function #'Report-Item)
     (:title "Menu")
     (:h-align :center)
     (:items `(("Cut" my-cut) ("Copy" my-copy)
	       ("Paste" my-paste) ("Undo" my-undo) ("Cancel" my-cancel)
	       (,(create-instance NIL opal:circle
		   (:constant :line-style :width :height)
		   (:width 15) (:height 15)
		   (:filling-style opal:red-fill))
		schema-fn)
	       (,(create-instance NIL opal:line
		   (:constant :line-style :width :height)
		   (:x1 (o-formula (gvl :left)))
		   (:x2 (o-formula (+ 30 (gvl :x1))))
		   (:y1 (o-formula (+ 2 (gvl :top))))  ;offset by 1/2 thickness
		   (:y2 (o-formula (gvl :y1)))
		   (:width 30) (:height 4)
		   (:line-style (create-instance NIL opal:line-style
				  (:line-thickness 4)
				  (:foreground-color opal:blue))))
		schema-fn))))
		
  (opal:add-components Menu-top-agg Menu-Obj)
  (format t "Leftdown on a menu item causes a box to be drawn around the~%")
  (format t "button, executes the function locally assigned to the item~%")
  (format t "(if there is one), and executes the function specified in~%")
  (format t ":selection-function (if there is one).~%")
  (opal:update Menu-win)
  NIL)

#+garnet-test
(defun Menu-Stop ()
  (opal:destroy Menu-win))
  

;;;  These functions are included to show that selection of one of the
;;;  menu items causes the associated local function to be called.

#+garnet-test
(defun my-cut (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function CUT called~%~%"))
#+garnet-test
(defun my-copy (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function COPY called~%~%"))
#+garnet-test
(defun my-paste (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function PASTE called~%~%"))
#+garnet-test
(defun my-undo (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function UNDO called~%~%"))
#+garnet-test
(defun my-cancel (gadget item-string)
  (declare (ignore gadget item-string))
  (format t "Function CANCEL called~%~%"))
#+garnet-test
(defun schema-fn (gadget schema)
  (declare (ignore gadget))
  (format t "Selected schema ~S~%~%" schema))
