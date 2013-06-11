;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lapidary:Defs.Lisp
;;;
;;; This file contains many of the schemas, defconstants, defvars, and 
;;;  defstructs which are used by Lapidary.  This does not contain any 
;;;  defmacros, however.
;;;

(in-package "LAPIDARY")


;;;=========================================================
;;;
;;; set up a global selection schema that keeps track of the 
;;; selected objects in all windows and the selection-type 
;;; of objects in all windows (e.g., one-one--one primary 
;;; and one secondary selection, zero-one--no primary and 
;;; one secondary selections)
;;;
;;;=========================================================

(create-schema '*selection-info*
     (:p-selected nil)
     (:s-selected nil)
     (:selected (o-formula (union (gvl :p-selected) (gvl :s-selected))))
     (:feedback nil) ; list of selection feedback objects currently displayed
     (:selection-type (o-formula (classify-selections))))

;;;====================================================================
;;;
;;; set up the schema that keeps track of event information
;;; for the start, stop, and abort events in interactor dialog
;;; boxes. event-cards contains a card for each event, event-type
;;; indicates whether the event is a start-, stop, or abort-event
;;; and queue indicates on which interactor's queue the information
;;; should be stored
;;;
;;;====================================================================

(create-schema '*event-info*
     (:event-cards nil)
     (:event-type nil)
     (:queue nil))

;;; put names into opal objects, filling styles, and line styles so
;;; that meaningful names can be constructed for objects

;;; opal objects

(s-value opal:rectangle :name "rectangle")
(s-value opal:line :name "line")
(s-value opal:circle :name "circle")
(s-value opal:text :name "text")
(s-value opal:roundtangle :name "roundtangle")

;;; opal filling styles

(s-value opal:white-fill :name "white")
(s-value opal:light-gray-fill :name "light-gray")
(s-value opal:gray-fill :name "gray")
(s-value opal:dark-gray-fill :name "dark-gray")
(s-value opal:black-fill :name "black")

;;; opal line styles

(s-value opal:thin-line :name "thin-line")
(s-value opal:dotted-line :name "dotted-line")
(s-value opal:dashed-line :name "dashed-line")
(s-value opal:line-2 :name "line2")
(s-value opal:line-4 :name "line4")
(s-value opal:line-8 :name "line8")

;;;=========================================================
;;;
;;; map names of menu selections to positions in a menu 
;;; array
;;;
;;;=========================================================

;;; indicates if dialog boxes should be loaded

(defparameter *load-db* nil)

;;; font used for slot names in constraint menus
(defvar *slot-font* (create-instance NIL opal:font
		       (:size :large) (:family :serif) (:face :bold-italic)))

;;; used so that objects will appear with a white border if they are 
;;; inverted

(defvar *white-line-style* (create-instance NIL opal:line-style
			      (:stipple opal::white-fill-bitmap)))

(defvar *large-bold-italic-serif-font* (create-instance NIL opal:font
				         (:size :large) (:face :bold-italic)
					 (:family :serif)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Size of the selection boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant sel-box-size 7)
(defconstant sel-box-sizeD2 3)
(defconstant *agg-sel-circle-size* 17)
(defconstant *min-agg-size* (+ (* 2 *agg-sel-circle-size*) 4))
(defconstant *min-leaf-size* (+ (* 2 sel-box-size) 4))

;; error gadget to display error messages
(create-instance '*lapidary-error-window* garnet-gadgets:error-gadget)

;; create a general purpose query gadget
(create-instance 'lapidary-query-gadget GARNET-GADGETS:query-gadget
      (:SELECTION-FUNCTION 'by-demo-OKCANCEL-FUNCTION)
      (:modal-p nil)
      (:function-for-ok 'by-demo-ok-function))

;; definitions for lists that contain free feedback objects
(defvar *undersized-feedback-list* nil)
(defvar *leaf-feedback-list* nil)
(defvar *agg-feedback-list* nil)
(defvar *line-feedback-list* nil)

;; definitions for variables that contain cursor information
(defvar copy-cursor-pair nil)
(defvar instance-cursor-pair nil)
(defvar load-cursor-pair nil)
(defvar move-cursor-pair nil)
(defvar delete-cursor-pair nil)

(defvar *CREATED-INSTANCES* NIL
  "list of objects created by loading a file")

(defvar *window-count* 0
  "number of windows created")

(defvar *prop-sheet*
  (create-instance NIL garnet-gadgets:prop-sheet-for-obj-with-OK
		   (:OK-Function 'Aggrelist-Prop-Sheet-Finish)
		   (:Apply-Function 'Aggrelist-Prop-Sheet-Finish)))

;; return T if error (not number)
(defun num-only (val)
  (unless (numberp val)
    (lapidary-error "Value must be a number.")
    T))

;; return T if error (not number or nil)
(defun nil-or-num (val)
  (unless (or (null val)
	      (numberp val))
    (lapidary-error "Value must be a number or NIL.")
    T))

(defparameter *aggrelist-slots* (List '(:direction (:Vertical :Horizontal))
				      (list :v-spacing 'num-only)
				      (list :h-spacing 'num-only)
				      '(:fixed-width-p (T NIL))
				      '(:fixed-height-p (T NIL))
				      '(:h-align (:left :center :right))
				      '(:v-align (:top :center :bottom))
				      (list :rank-margin 'nil-or-num)
				      (list :pixel-margin 'nil-or-num)
				      (list :indent 'num-only)))

;;; used for creating feedback when resizing an aggrelist
(defvar *aggrelist-feedback-slots* 
  '(:left :top :direction :v-spacing :h-spacing 
		:fixed-width-size :fixed-height-size
		:fixed-width-p :fixed-height-p
		:h-align :v-align
		:rank-margin :pixel-margin
		:indent :items))

;; some slots should not be altered by the user. These are stored in
;; an object's :do-not-alter-slots

(s-value opal:text :do-not-alter-slots '(:width :height))
(s-value opal:bitmap :do-not-alter-slots '(:width :height))

;; when a constraint is destroyed in a slot, some slots, such as
;; :filling-style and :line-style, should inherit from their prototype;
;; others, such as :left and :top, should assume their current values.
;; the following list records those slots which should not inherit

(defvar *do-not-inherit-list* '(:left :top :active))


;; add slots to list of slots that c32 should show for lapidary interactors
(s-value lapidary:directional-move-grow-interactor :slots-to-show
	 (append (g-value lapidary:directional-move-grow-interactor 
			  :slots-to-show)
		 '(:grow-box-parms :move-box-parms)))

;;;=========================================================
;;;
;;; queue that a custom formula should be placed on 
;;;
;;;=========================================================

(defvar *inter-queue* nil)
