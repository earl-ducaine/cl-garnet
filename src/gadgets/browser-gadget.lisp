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
;;;  BROWSER-GADGET
;;;
;;;    The browser-gadget is useful for examining hierarchical structures,
;;; such as Garnet object networks and directory trees.  The gadget is composed
;;; of a set of scrolling menus, where each title corresponds to an item
;;; specified in the top-level :items list, and each list of menu selections
;;; corresponds to the children of the item appearing in the title.  Clicking
;;; the left mouse button on one of the menu selections causes that selection
;;; to appear as the title of the next scrolling menu, with all of its children
;;; appearing as choices in the new menu.  Additionally, middledown over a menu
;;; selection causes a gray feedback object to appear over the selection.
;;;
;;;   1) :num-menus - The number of visible scrolling menus.  If this slot is
;;;          set at run time, opal:notice-items-changed must be called on the
;;;          :menu-list of the browser-gadget instance.
;;;   2) :num-rows - The number of items in each scrolling menu.
;;;   3) :menu-items-generating-function - The function used to generate menu
;;;          selections from the items appearing in the :items list.
;;;   4) :item-to-string-function - The function used to get a string from a
;;;          member of the :items list.
;;;   5) :additional-selection-coordinate - A two element list of the form
;;;          '(menu-rank choice-rank), where the ranks are zero-based according
;;;          according to the full length :items list and scrolling menu
;;;          choices.
;;;   6) :additional-selection-p - Whether to allow middledown gray selection
;;;   7) :additional-selection-function - Function executed when the additional
;;;          selection is selected.
;;;   8) :additional-selection - The item named by the additional selection
;;;          (do not set this slot; instead set the
;;;          :additional-selection-coordinate).
;;;   9) :selection-function - Function to be executed when the user selects
;;;          one of the menu choices.
;;;  10) :item-font - The font that the menu items will appear in.
;;;  11) :title-font - The font that the titles of the menus will appear in.
;;;
;;;  NOTE: This module requires schemata defined for the h-scroll-bar and
;;;  scrolling-menu gadgets.  These modules are automatically loaded when the
;;;  loader file "browser-gadget-loader" is used.
;;;
;;;  Browser-gadget demos:
;;;     Two demos of the browser-gadget appear in seperate garnet modules:
;;;     the DEMO-SCHEMA-BROWSER and the DEMO-FILE-BROWSER.
;;;
;;;  Designed by Brad Myers
;;;  Written by Andrew Mickish

;;;  Maintenance note:
;;;    In order to avoid a circularity which arises from the dependence of the
;;;  horizontal scroll bar width on the scrolling menu aggrelist width, a
;;;  complicated formula has been installed in the :width slot of the top-level
;;;  aggrelist.  When this formula is evaluated, it sets the :width of the
;;;  horizontal scroll bar and the proper pixel position of the indicator box
;;;  according to this new width.  In order to ensure that this formula is
;;;  evaluated as necessary, a dummy rectangle is included in the
;;;  browser-gadget's :parts list whose :left formula is computed whenever
;;;  the visible items change.

;;;
;;; CHANGE LOG:
;;;
;;; 12/14/92  Andrew Mickish - Added type and parameter declarations
;;; 09/15/92  Andrew Mickish - Fixed bug in computation of new :starts list.
;;; 04/30/92  Andrew Mickish - Called get-standard-font for italic font,
;;;             added (:constant T) to line-style definition
;;; 03/27/92  Andrew Mickish - Changed DOVALUES to DO in :notice-items-changed
;;;             method to eliminate CMUCL compiler warnings.
;;; 02/11/92  Andrew Mickish - Added :maybe-constant list
;;;

(in-package "GARNET-GADGETS")

(eval-when (eval load compile)
  (export '(BROWSER-GADGET
	    BROWSER-MENU-FN PUSH-FIRST-ITEM PROMOTE-ITEM
	    SET-FIRST-ITEM BROWSER-MENU-SCROLL-FN BROWSER-SCROLL-FN)))


;;  BROWSER-MENU-FN:  Called when an item is selected or deselected.
;;  Updates information lists in the top level browser object.
;;
(defun BROWSER-MENU-FN (menu sm-item)
  (let* ((sm-item-rank (g-value sm-item :real-rank))
	 (browser (g-value menu :parent :parent))
	 (starts (g-value browser :starts))
	 (items (g-value browser :items))
	 (all-items (g-value browser :all-items))
	 (item (g-value sm-item :item))
	 (menu-rank (g-value menu :rank))
	 (real-menu-rank (+ menu-rank (g-value browser :start)))
	 (new-menu-rank (+ 1 real-menu-rank))
	 (selected (g-value browser :selected-ranks)))

    (if (g-value sm-item :highlighted)
	; if item has just become selected, update the info lists in
	; browser to include a new menu

	(let ((generated-items (kr-send browser
					:menu-items-generating-function item)))
	  (if generated-items
	      (let*  ; ADD A MENU
		((new-items (append (subseq items 0 new-menu-rank) (list item)))
		 (new-all-items (append (subseq all-items 0 new-menu-rank)
					(list generated-items)))
		 (new-selected (append (subseq selected 0 real-menu-rank)
				       (list (list sm-item-rank))))
		 (saved-starts (subseq starts 0 new-menu-rank))
		 (free-menus (- (g-value browser :num-menus)
				(length saved-starts)))
		 (new-starts (append saved-starts
				     (if (< 0 free-menus)
					 (make-list free-menus
						    :initial-element 0)
					 (list 0)))))
				     
		; discard items that were beyond the rank of the current menu
		; and add the new item to the end of the :items list
		(s-value browser :items new-items)
		(s-value browser :all-items new-all-items)

		; discard selected-indices beyond and including the current
		; menu, then add the new selection to the end of the list
		(s-value browser :selected-ranks new-selected)
			 
		; discard starts that were beyond the rank of the current menu,
		; and add a new start for the new menu
		(s-value browser :starts new-starts))

	      ;else, make sure there are no menus to the right
	      (let ((new-items (subseq items 0 new-menu-rank)))
		(unless (equal items new-items)
		  (let ((new-all-items (subseq all-items 0 new-menu-rank))
			(new-selected (append (subseq selected 0 real-menu-rank)
					      (list (list sm-item-rank))))
			(new-starts (subseq starts 0 new-menu-rank)))
		  (s-value browser :items new-items)
		  (s-value browser :all-items new-all-items)
		  (s-value browser :selected-ranks new-selected)
		  (s-value browser :starts new-starts)))))
	  ; execute selection-function
	  (kr-send browser :selection-function browser item))

	;else, item has just become deselected
	(progn  ; DELETE MENUS
	  (let ((new-items (subseq items 0 new-menu-rank)))
	    (unless (equal items new-items)
	      (let* ((new-all-items (subseq all-items 0 new-menu-rank))
		     (new-selected (subseq selected 0 real-menu-rank))
		     (saved-starts (subseq starts 0 new-menu-rank))
		     (new-starts (append saved-starts
				   (make-list (- (length starts)
						 (length saved-starts))
					      :initial-element 0))))
		(s-value browser :items new-items)
		(s-value browser :all-items new-all-items)
		(s-value browser :selected-ranks new-selected)
		(s-value browser :starts new-starts))))))
		      

    ; Remove the gray feedback if it is to the right of the item just selected
    (let* ((gray-menu-rank (car (g-value browser
					 :additional-selection-coordinate))))
      (when (and gray-menu-rank (> gray-menu-rank real-menu-rank))
	(s-value browser :additional-selection-coordinate NIL)))

    ; Make sure the browser is scrolled all the way to the right
    (let* ((scroll-bar (g-value browser :scroll-bar))
	   (val-2 (g-value scroll-bar :val-2)))
      (unless (eq val-2 (g-value scroll-bar :value))
	(s-value scroll-bar :value val-2)))

    ; Set all the menus with the appropriate values for their :items lists
    ; and scroll-bar :value's
    (update-menus browser)))


(defun PUSH-FIRST-ITEM (browser new-item)
  (let* ((old-browser-items (g-value browser :items))
	 (old-all-items (g-value browser :all-items))
	 (generated-items (kr-send browser :menu-items-generating-function
				   new-item)))
    (s-value browser :items (cons new-item old-browser-items))
    (s-value browser :all-items (cons generated-items old-all-items))
    (let* (;; Determine which menu item in the new menu should be highlighted
	   (item-rank (position (car old-browser-items) generated-items
				:test #'equal))
	   (additional-selection-coordinate
	    (g-value browser :additional-selection-coordinate))
	   ;; If there are more than :num-menus items already, then just add
	   ;; a 0 to the front of the :starts list.  Otherwise, add a 0 to the
	   ;; front and strip the 0 off the end
	   (new-starts
	    (if (>= (length old-browser-items) (g-value browser :num-menus))
		(cons 0 (g-value browser :starts))
		(cons 0 (butlast (g-value browser :starts))))))

      (when old-browser-items
	(s-value browser :selected-ranks (cons (list item-rank)
					 (g-value browser :selected-ranks))))
      (s-value browser :starts new-starts)
      (if additional-selection-coordinate
	  (s-value browser :additional-selection-coordinate
		   (list (+ 1 (car additional-selection-coordinate))
			 (second additional-selection-coordinate))))
      (s-value (g-value browser :scroll-bar) :value 0)
      (update-menus browser))))


(defun PROMOTE-ITEM (browser coordinate)
  (let* ((real-menu-rank (car coordinate))
	 (real-obj-rank (cadr coordinate))
	 ;; KEEP-P is T if the item is both gray-selected and highlighted;
	 ;;   Used to decide whether to retain all of the items in the
	 ;;   scrolling-menus to the right of ITEM
	 (keep-p (equal (list real-obj-rank)
			(nth real-menu-rank (g-value browser :selected-ranks))))
	 (item (nth real-obj-rank
		    (nth real-menu-rank (g-value browser :all-items)))))
    (s-value (g-value browser :scroll-bar) :value 0)
    (if keep-p
	; Retain items in scrolling menus to the right: chop off the
	; left side of the :items, :starts, and :selected-ranks lists and
	; add fillers if necessary
	(let* ((rank+1 (+ 1 real-menu-rank))
	       (new-items (if item (nthcdr rank+1 (g-value browser :items))))
	       (new-all-items
		(if item (nthcdr rank+1 (g-value browser :all-items)))))
	  (s-value browser :items new-items)
	  (s-value browser :all-items new-all-items)
	  (let* ((num-menus (g-value browser :num-menus))
		 (new-items-length (length new-items))
		 (scroll-p (< new-items-length num-menus))
		 (shifted-starts (nthcdr rank+1 (g-value browser :starts)))
		 (extra-starts (if scroll-p
				   (make-list (- num-menus
						 (length shifted-starts))
					      :initial-element 0)))
		 (new-starts (append shifted-starts extra-starts)))
	    (s-value browser :starts new-starts))
	  (s-value browser
		   :selected-ranks
		   (nthcdr rank+1 (g-value browser :selected-ranks))))

	; Don't keep any of the current items -- zero out the lists
	(progn
	  (s-value browser :items (if item (list item)))
	  (s-value browser
		   :all-items
		   (if item
		       (list (kr-send browser :menu-items-generating-function
				      item))))
	  (s-value browser :selected-ranks NIL)
	  (s-value browser :starts (make-list (g-value browser :num-menus)
					      :initial-element 0))))
    (update-menus browser)))


(defun SET-FIRST-ITEM (browser item)
  (let* ((menu-list (g-value browser :menu-list))
	 (all-items (kr-send browser :menu-items-generating-function item)))

    ; Validate several slots containing formulas before they are destroyed
    ; by s-value's
    (g-value browser :scroll-bar :value)
    (opal:do-components menu-list
       #'(lambda (menu)
	   (g-value menu :scroll-bar :value)))

    ; Discard the current :items list and install the new item in it
    (s-value browser :items (if item (list item)))
    (s-value browser :all-items (if all-items (list all-items)))
    ; Reinitialize information lists
    (s-value browser :starts (make-list (g-value browser :num-menus)
					:initial-element 0))
    (s-value browser :selected-ranks NIL)
    (s-value browser :additional-selection-coordinate NIL)
    (update-menus browser)))


;;    This function sets the menus to have the correct :items list and correct
;; :value for the scroll bar.
;;
(defun UPDATE-MENUS (browser)
  (let ((start (g-value browser :start))
	(starts (g-value browser :starts))
	(items (g-value browser :items))
	(all-items (g-value browser :all-items)))
    (opal:do-components (g-value browser :menu-list)
      #'(lambda (menu)
	  (let* ((real-rank (+ start (g-value menu :rank)))
		 (title-item (nth real-rank items))
		 (items (nth real-rank all-items))
		 (value (nth real-rank starts))
		 (scroll-bar (g-value menu :scroll-bar)))
	    (unless (equal title-item (g-value menu :title-item))
	      (s-value menu :title-item title-item))
	    (unless (equal items (g-value menu :items))
	      (s-value menu :items items))
	    (unless (equal value (g-value scroll-bar :value))
	      ; Ensure that no current invalidations will override the new value
	      (g-value scroll-bar :value)
	      (s-value scroll-bar :value (if value value 0))))))))


;; This function is called when the user moves a scroll bar on one of the
;; scrolling menus.  The function changes the :starts index list so that the
;; :start of the scrolling menu will be recorded in the list.
;;
(defun BROWSER-MENU-SCROLL-FN (scroll-bar value)
  (let* ((menu (g-value scroll-bar :parent))
	 (browser (g-value menu :parent :parent))
	 (rank (+ (g-value menu :rank) (g-value browser :start))))
    (setf (nth rank (g-value browser :starts)) value)))


;; This function is called when the user moves the horizontal scroll bar
;; in order to scroll the items in the menus.  The function iterates over
;; the scrolling menus and sets the :start of each one according to the value
;; of its element in the :starts list.
;;
(defun BROWSER-SCROLL-FN (scroll-bar value)
  (declare (ignore value))
  (let* ((browser (g-value scroll-bar :parent)))
    (update-menus browser)))



(create-instance 'BROWSER-GADGET opal:aggregadget
   :declare ((:parameters :left :top :num-menus :num-rows :item-font
			  :title-font :menu-items-generating-function
			  :item-to-string-function :selection-function
			  :additional-selection-p
			  :additional-selection-coordinate
			  :additional-selection-function)
	     (:type ((integer 0) :num-menus :num-rows)
		    ((or function symbol) :menu-items-generating-function
		     :item-to-string-function :menu-function)
		    ((or null function symbol)
		     :additional-selection-function :selection-function)
		    (list :additional-selection-coordinate)
		    ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		     :font :title-font))
	     (:maybe-constant :left :top :num-menus :num-rows
			      :menu-items-generating-function :menu-function
			      :item-to-string-function :additional-selection-p
			      :item-font :title-font :visible))

   ;; Browser parameters
   (:left 0)
   (:top 0)
   (:num-menus 3)
   (:num-rows 5)
   (:menu-items-generating-function
    #'(lambda (arg)
	(declare (ignore arg))
	(error "You must supply a function in the :menu-items-generating-funciton
slot of your instance of the BROWSER-GADGET")))
   (:item-to-string-function #'identity)
   (:menu-function #'BROWSER-MENU-FN)

   ;; Additional selection parameters
   (:additional-selection-coordinate NIL)
   (:additional-selection-p T)
   (:additional-selection-function NIL)
   (:additional-selection
    (o-formula
     ; Returns the item appearing in the gray-selected scrolling-menu-item
     (let ((coordinate (gvl :additional-selection-coordinate)))
       (when coordinate
	 (let* ((real-menu-rank (car coordinate))
		(real-obj-rank (cadr coordinate)))
	   (nth real-obj-rank (nth real-menu-rank (gvl :all-items))))))))
   (:item-font opal:default-font)
   (:title-font (opal:get-standard-font NIL :italic NIL))
   (:selection-function NIL)

   ; Non-customizable slots
   (:items NIL)
   (:starts (o-formula (make-list (gvl :num-menus) :initial-element 0)))
   (:start (o-formula (if (gvl :scroll-p) (gvl :scroll-bar :value) 0)))
   (:scroll-p (o-formula (> (length (gvl :items)) (gvl :num-menus))))
   (:end (o-formula (+ (gvl :start) (gvl :num-menus))))
   (:visible-items (o-formula (subseq (gvl :items) (gvl :start) (gvl :end))))
   (:parts
    `((:menu-list ,opal:aggrelist
        (:constant (:direction :h-spacing :pixel-margin :rank-margin
		    :fixed-width-p :fixed-height-p))
        (:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (gvl :parent :top)))
	(:direction :horizontal)
	(:h-spacing 10)
	;; This complicated :width formula not only computes the width of the
	;; aggrelist but also sets the correct pixel position for the indicator
	;; box, according to the new width.
	(:width ,(o-formula
		  (let ((max-x 0))
		    (gv (first (gvl :components)) :items)
		    (opal:do-components KR::*schema-self*
		      #'(lambda (menu)
			  (setf max-x (MAX max-x (opal:gv-right menu)))))
		    (let* ((left (gvl :left))
			   (new-width (MAX 0 (- max-x left)))
			   (bound-left (+ left 40)) ; width of trill boxes = 40
			   (bound-right (- max-x 40))
			   (scroll-bar (g-value KR::*schema-self* :parent
						:scroll-bar))
			   (value (g-value scroll-bar :value))
			   (val-1 (g-value scroll-bar :val-1))
			   (val-2 (g-value scroll-bar :val-2))
			   (indicator (g-value scroll-bar :indicator))
			   (ind-width (g-value indicator :width))
			   (new-box (list (inter:clip-and-map value val-1 val-2
					         bound-left
						 (- bound-right ind-width))
					  0 0 0)))
		      (s-value scroll-bar :width new-width)
		      (s-value indicator :box new-box)
		      new-width))))
	(:items ,(o-formula (gvl :parent :num-menus)))
	(:item-prototype
	 (,SCROLLING-MENU
	  (:constant (:indicator-text-p :int-scroll-feedback-p :multiple-p
		      :int-scroll-feedback-p :scroll-on-left-p :scr-incr
		      :min-scroll-bar-width :scr-trill-p :page-trill-p
		      :indicator-font :min-frame-width :v-spacing :h-align
		      :int-menu-feedback-p :final-feedback-p :text-offset))
	  (:height ,(o-formula
		     (let ((new-height (+ (* (gvl :num-visible)
					     (gvl :item-height))
					  (gvl :title-height))))
		       (let* ((top (g-value KR::*schema-self* :top))
			      (bound-top (+ top 40)) ; height of boxes = 40
			      (bound-bottom (- (+ top new-height) 40))
			      (scroll-bar (g-value kr::*schema-self*
						   :scroll-bar))
			      (value (g-value scroll-bar :value))
			      (val-1 (g-value scroll-bar :val-1))
			      (val-2 (g-value scroll-bar :val-2))
			      (indicator (g-value scroll-bar :indicator))
			      (ind-height (g-value indicator :height))
			      (new-box 
			       (list 0 (inter:clip-and-map value val-1 val-2
					      bound-top
					      (- bound-bottom ind-height))
				     0 0)))
			 (s-value scroll-bar :height new-height)
			 (s-value indicator :box new-box)
			 new-height))))
	  (:num-visible ,(o-formula (gvl :parent :parent :num-rows) 5))
	  (:page-incr ,(o-formula (gvl :parent :parent :num-rows)))
	  (:indicator-text-p NIL)
	  (:int-scroll-feedback-p NIL)
	  (:scroll-selection-function BROWSER-MENU-SCROLL-FN)
	  (:multiple-p NIL)
	  (:min-frame-width 100)
	  (:item-to-string-function ,(o-formula (gvl :parent :parent
						     :item-to-string-function)))
	  (:item-font ,(o-formula (gvl :parent :parent :item-font)
				  opal:default-font))
	  (:title-font ,(o-formula (gvl :parent :parent :title-font)))
	  (:items ,(make-list 5))
	  (:title-item NIL)
	  (:title ,(o-formula
		    (let* ((item (gvl :title-item)))
		      (if item
			  (let* ((p (gvl :parent :parent))
				 (fn (gv p :item-to-string-function)))
			    (declare (ignore fn))
			    (kr-send p :item-to-string-function item))
			  ""))))
	  (:selected-ranks ,(o-formula (let ((p (gvl :parent :parent)))
					 (nth (+ (gvl :rank) (gv p :start))
					      (gv p :selected-ranks)))))
	  (:menu-selection-function ,(o-formula (gvl :parent :parent
						     :menu-function)))
	  (:parts
	   (:frame :feedback-obj :menu-title :menu-item-list
	     (:scroll-bar :modify
		(:height 120)))))))  ; the height of a menu with 5 items

      (:scroll-bar ,H-SCROLL-BAR
        (:constant (:min-height :val-1 :scr-trill-p :page-trill-p
		    :indicator-text-p :scr-incr :int-feedback-p
		    :format-string :indicator-font))
	(:left ,(o-formula (gvl :parent :left)))
	(:top ,(o-formula (+ (gvl :parent :top)
			     (gvl :parent :menu-list :height) 10)))
	(:width 380)
	(:val-2 ,(o-formula (MAX (- (length (gvl :parent :items))
				    (gvl :parent :num-menus))
				 1)))
	(:scroll-p ,(o-formula (gvl :parent :scroll-p)))
	(:indicator-text-p NIL)
	(:page-incr ,(o-formula (gvl :parent :num-menus)))
	(:int-scroll-feedback-p T)
	(:selection-function BROWSER-SCROLL-FN))

      (:gray-feedback ,opal:rectangle
       (:left ,(o-formula (+ 1 (gvl :obj-over :left))))
       (:top ,(o-formula (if (gvl :obj-over :prev)
			     (gvl :obj-over :top)
			     (+ 1 (gvl :obj-over :top)))))
       (:width ,(o-formula (- (gvl :obj-over :width) 2)))
       (:height ,(o-formula (if (and (gvl :obj-over :prev)
				     (gvl :obj-over :next))
				(gvl :obj-over :height)
				(- (gvl :obj-over :height) 1))))
       (:draw-function :xor)
       (:fast-redraw-p T)
       (:filling-style NIL)
       (:line-style ,(create-instance NIL opal:line-style
		        (:constant T)
		        (:line-thickness 4)
			(:stipple opal::gray-fill-bitmap)))
       (:visible ,(o-formula (gvl :obj-over)))
       (:obj-over
	,(o-formula
	  (let ((coordinate (gvl :parent :additional-selection-coordinate)))
	    (when coordinate
	      (let ((real-menu-rank (car coordinate))
		    (real-obj-rank (cadr coordinate))
		    (browser-start (gvl :parent :start)))
		;; When the coordinate indicates a visible position
		(when (and (>= real-menu-rank browser-start)
			   (< real-menu-rank (+ browser-start
						(gvl :parent :num-menus))))
		  (let* ((menu-rank (- real-menu-rank browser-start))
			 (menu (nth menu-rank
				    (gvl :parent :menu-list :components)))
			 (menu-start (gv menu :start)))
		    (when (and (>= real-obj-rank menu-start)
			       (< (+ menu-start (gv menu :num-visible))))
		      (nth (- real-obj-rank (gv menu :start))
			   (gv menu :menu-item-list :components))
		      )))))))))

      (:dummy-rect ,opal:rectangle
		   (:left ,(o-formula (progn (gvl :parent :menu-list :width)
					-1)))
		   (:top -1) (:width 0) (:height 0))))
   (:interactors
    `((:gray-inter ,inter:menu-interactor
       (:window ,(o-formula (gv-local :self :operates-on :window)))
       (:active ,(o-formula (gvl :operates-on :additional-selection-p)))
       (:start-where
	,(o-formula (list :leaf-element-of (gvl :operates-on)
			  :type SCROLLING-MENU-ITEM)))
       (:start-event :middledown)
       (:feedback-obj ,(o-formula (gvl :operates-on :gray-feedback)))
       (:start-action
	,#'(lambda (interactor first-obj-over)
	     (let ((browser (g-value interactor :operates-on))
		   (feedback-obj (g-value interactor :feedback-obj)))
	       (if (equal (g-value feedback-obj :obj-over) first-obj-over)
		   (s-value browser :additional-selection-coordinate NIL)
		   (let ((obj-rank (g-value first-obj-over :real-rank))
			 (menu-rank (+ (g-value first-obj-over :parent
						:parent :rank)
				       (g-value browser :start))))
		     (s-value browser :additional-selection-coordinate
			      (list menu-rank obj-rank)))))))
       (:running-action
	,#'(lambda (interactor prev-obj-over new-obj-over)
	     (declare (ignore prev-obj-over))
	     (let ((browser (g-value interactor :operates-on)))
	       (if new-obj-over
		   (let ((obj-rank (g-value new-obj-over :real-rank))
			 (menu-rank (+ (g-value new-obj-over :parent :parent
						:rank)
				       (g-value browser :start))))
		     (s-value browser :additional-selection-coordinate
			      (list menu-rank obj-rank)))))))
       (:stop-action
	; Compute the :additional-selection through the formula in case it has
	; just become deselected (send NIL instead of the item)
	,#'(lambda (interactor final-obj-over)
	     (declare (ignore final-obj-over))
	     (let ((browser (g-value interactor :operates-on)))
	       (kr-send (g-value interactor :operates-on)
			:additional-selection-function
			browser (g-value browser :additional-selection)))))))))


#|
(define-method :notice-items-changed BROWSER-GADGET
               (gadget &optional no-propagation)
  (declare (ignore no-propagation))
  (opal:notice-items-changed (g-value gadget :menu-list))
  (do ((components (g-value gadget :menu-list :components)
		   (cdr components)))
      ((null components) T)
    (opal:notice-items-changed (car components))))
|#