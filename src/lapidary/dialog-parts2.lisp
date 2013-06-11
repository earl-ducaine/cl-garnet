;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;; This file provides the common functions and aggregadgets in the
;;; interactor menus and font menus
;;;

;;; CHANGE LOG
;;;
;;; 08/24/92 amickish - Added defvars and proclaim

(in-package "LAPIDARY")

(defvar *bold-font* (create-instance NIL opal:font (:face :bold)))
(defvar *very-large-bold-italic-serif-font*
  (create-instance NIL opal:font
     (:size :very-large) (:face :bold-italic) (:family :serif)))

(declaim (special text-properties-win name-box titled-frame text-box
		  select-box act-buttons font-parameter-panel))

(when (boundp 'name-box) (opal:destroy name-box))
(when (boundp 'titled-frame) (opal:destroy titled-frame))
(when (boundp 'text-box) (opal:destroy text-box))
(when (boundp 'select-box) (opal:destroy select-box))
(when (boundp 'act-buttons) (opal:destroy act-buttons))
(when (boundp 'font-parameter-panel) (opal:destroy font-parameter-panel))


;;; =========================================================================
;;;
;;;    Functions used in several dialog boxes
;;;
;;; =========================================================================

;;;******************
;;;  DIALOG-ENQUEUE
;;;******************
;    Called by dialog box selection-functions;  given a slot name, a value
;  to be inserted into that slot, an a queue (a global variable) that the
;  slot/value pair should be inserted into, this macro conses the two
;  parameters together and shoves them into the queue to be traversed by 
;  Brad's code. If the entry already exists, it removes the entry and 
;  reinserts it at the front. The queue is reversed in order to create the
;  interactor, so the things most recently added will be processed last.
;  This is necessary to ensure that formulas added by demonstration are
;  processed from oldest to newest (some of the older ones may be superceded
;  by some of the newer ones)
;

(defmacro DIALOG-ENQUEUE (slot value queue)
  `(progn
     (setf ,queue (delete ,slot ,queue :key #'car))
     (push (cons ,slot ,value) ,queue)))

;;;******************
;;;  UNCONSTRAIN-FN
;;;******************

(defun UNCONSTRAIN-FN (slot &key (type NIL))
  (let ((selections (g-value *SELECTION-INFO* :selected)))
    (mapcar #'(lambda (obj)
		(if (is-a-p obj type)
		    (destroy-constraint obj slot)))
	    selections)))


;;; ==========================================================================
;;;
;;;   Schemas used by several dialog boxes
;;;
;;; ==========================================================================

;;;*************
;;;  NAME-BOX
;;;*************

(create-instance 'NAME-BOX garnet-gadgets:labeled-box
   (:left (o-formula (+ 10 (gvl :parent :left))))
   (:top (o-formula (+ 10 (opal:gv-bottom (gvl :parent :title)))))
   (:label-string "Interactor Name:")
   (:value "")
   (:min-frame-width 150))


;;;*****************
;;;   TITLED-FRAME
;;;*****************

(create-instance 'TITLED-FRAME opal:aggregadget
   (:maybe-constant :font :string :left :top :width :height)
   (:left (o-formula (gvl :parent :left)))
   (:top (o-formula (gvl :parent :top)))
   (:width (o-formula (+ 30 (gvl :parent :contents :width))))
   (:height (o-formula (+ (floor (gvl :text :height) 2)
			  (gvl :frame :height))))
   (:string ":slot")
   (:font *bold-font*)
   (:parts
    `((:frame ,opal:rectangle
		(:left ,(o-formula (gvl :parent :left)))
		(:top ,(o-formula (opal:gv-center-y (gvl :parent :text))))
		(:width ,(o-formula (gvl :parent :width)))
		(:height ,(o-formula (+ 20 (gvl :parent :parent :contents
						:height)))))
      (:text-frame ,opal:rectangle
		   (:left ,(o-formula (+ (gvl :parent :left) 10)))
		   (:top ,(o-formula (gvl :parent :text :top)))
		   (:width ,(o-formula (+ (gvl :parent :text :width) 10)))
		   (:height ,(o-formula (gvl :parent :text :height)))
		   (:line-style NIL)
		   (:filling-style ,opal:white-fill))
      (:text ,opal:text
	     (:left ,(o-formula (+ (gvl :parent :text-frame :left) 5)))
	     (:top ,(o-formula (gvl :parent :top)))
	     (:string ,(o-formula (gvl :parent :string)))
	     (:font ,(o-formula (gvl :parent :font)))))))



;;;************
;;;  TEXT-BOX
;;;************

(create-instance 'TEXT-BOX opal:aggregadget
   (:constant '(:width :min-frame-width :height))
   (:left 0) (:top 0)
;  (:width (o-formula (MAX (gvl :min-frame-width) (+ 6 (gvl :text :width)))))
   (:width 125)
   (:height 20)
   (:min-frame-width 125)
   (:string "")
   (:parts
    `((:frame ,opal:rectangle
	      (:left ,(o-formula (gvl :parent :left)))
	      (:top ,(o-formula (gvl :parent :top)))
	      (:width ,(o-formula (gvl :parent :width)))
	      (:height ,(o-formula (gvl :parent :height))))
      (:text ,opal:text
	     (:left ,(o-formula (+ 3 (gvl :parent :left))))
	     (:top ,(o-formula (+ 2 (gvl :parent :top))))
	     (:string ,(o-formula (let ((string (gvl :parent :string)))
				    (if string string ""))))
	     (:draw-function :xor) (:fast-redraw-p T)))))


;;;************************************************************
;;;      SELECT-BOX   (a labeled radio-button with framed-text)
;;;  and SELECT-BOX-PANEL
;;;
;;;  note: Text-interactor is disabled for Lapidary demo!
;;;************************************************************


(create-instance 'BUTTON-BOUND-BOX opal:rectangle
   (:left (o-formula (gv (kr-path 0 :parent) :button-left)))
   (:top (o-formula (gv (kr-path 0 :parent) :button-top)))
   (:width (o-formula (gv (kr-path 0 :parent) :button-unit-width)))
   (:height (o-formula (gv (kr-path 0 :parent) :button-unit-height)))
   (:line-style NIL) (:hit-threshold 0))


(create-instance 'SELECT-BOX garnet-gadgets:RADIO-BUTTON
   (:left 0) (:top 0)
   (:width (o-formula (+ (gvl :text-width) (gvl :text-offset)
			 10 (gvl :button-unit-width)
			 (gvl :text-box :width))))
   (:height (o-formula (MAX (gvl :button-unit-height)
			    (gvl :text-box :height)
			    (gvl :text :height))))
   (:floating-left (o-formula (+ (gvl :button-left)
				 (if (gvl :button-bound-box :interim-selected)
				     (gvl :shadow-offset)
				     0))))
   (:floating-top (o-formula (+ (gvl :button-top)
				 (if (gvl :button-bound-box :interim-selected)
				     (gvl :shadow-offset)
				     0))))
   (:text-visible (o-formula (not (string= (gvl :string)
					    "Start Anywhere in Window"))))
   (:string "string")
   (:field-string (o-formula (if (gvl :selected)
				  (gvl :parent :field-string))))
   (:selected (o-formula (string= (gvl :string) (gvl :parent :value))))
   (:min-frame-width 125)
   (:parts
    `(:shadow :gray-outline :white-field :text :feedback-obj
      (:button-bound-box ,BUTTON-BOUND-BOX)
      (:text-box ,TEXT-BOX
	  (:visible ,(o-formula (gvl :parent :text-visible)))
          (:left ,(o-formula (+ 10 (opal:gv-right (gvl :parent :shadow)))))
	  (:top ,(o-formula (- (gvl :parent :center-y)
			       (floor (gvl :height) 2))))
	  (:string ,(o-formula (gvl :parent :field-string)))
	  (:min-frame-width ,(o-formula (gvl :parent :min-frame-width))))))
   (:interactors
    `((:radio-button-press :omit)
      (:select-box-press ,inter:button-interactor
        (:window ,(o-formula (gv-local :self :operates-on :window)))
	(:start-where ,(o-formula (list :in-box
					(gvl :operates-on :button-bound-box))))
	(:final-function
	 ,#'(lambda (interactor bound-box)
	      (declare (ignore bound-box))
	      (let* ((button (g-value interactor :operates-on)))
		; Toggle selection of button
		(s-value button :selected T)
		; Execute selection function
		(kr-send button :selection-function
			 button (g-value button :string)))))))))


(create-instance 'SELECT-BOX-PANEL opal:aggrelist
   (:left (o-formula (+ 15 (gvl :parent :parent :left))))
   (:top (o-formula (+ 20 (gvl :parent :parent :top))))
   (:items (o-formula (gvl :parent :parent :items)))
   (:v-spacing (o-formula (gvl :parent :or-1 :height)))
   (:type (o-formula (gvl :parent :parent :type)))
   (:type-restriction (o-formula (gvl :parent :parent :type-restriction)))
   (:value (o-formula (gvl :parent :value)))
   (:field-string (o-formula (gvl :parent :field-string))) 
   (:min-frame-width 125)
   (:selected NIL)  ; Set by interactor
   (:item-prototype
    `(,SELECT-BOX
      (:floating-left ,(o-formula (+ (gvl :button-left)
				     (if (gvl :interim-selected)
					 (gvl :shadow-offset)
					 0))))
      (:floating-top ,(o-formula (+ (gvl :button-top)
				    (if (gvl :interim-selected)
					(gvl :shadow-offset)
					0))))
      (:min-frame-width ,(o-formula (gvl :parent :min-frame-width)))
      (:string ,(o-formula (let* ((p (kr-path 0 :parent))
				  (item (nth (gvl :rank) (gv p :items))))
			     (if (listp item) (first item) item))))
      (:action ,(o-formula (let* ((p (kr-path 0 :parent))
				  (item (nth (gvl :rank) (gv p :items))))
			     (if (listp item) (second item)))))
      (:interactors
       ((:select-box-press :omit)))))
   (:interactors
    `((:button-press ,inter:menu-interactor
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:start-where ,(o-formula
			(list :check-leaf-but-return-element
			      (gvl :operates-on)
			      :type BUTTON-BOUND-BOX)))
	(:how-set :set)
	(:final-function ,#'(lambda (interactor button)
			      (let ((panel (g-value interactor :operates-on))
				    (value (g-value button :string)))
				; set the :value field of the parent
				(s-value panel :value value)
				; Global function
				(kr-send panel :selection-function
					 panel value)
				; Item function
				(kr-send button :action
					 button value))))))))


;;;***************
;;;  ACT BUTTONS
;;;***************

(create-instance 'ACT-BUTTONS garnet-gadgets:text-button-panel
   (:top (o-formula (gvl :parent :top)))
   (:width (o-formula (+ (gvl :fixed-width-size) (gvl :shadow-offset))))
   (:height (o-formula (+ (* 4 (gvl :v-spacing))
			  (* 5 (+ (gvl :fixed-height-size)
				  (gvl :shadow-offset))))))
   (:text-offset 3) (:shadow-offset 5) (:gray-width 4)
   (:final-feedback-p NIL)
   (:inter (o-formula (gvl :window :inter)))
   (:items '(("CREATE INSTANCE" create-interactor)
	     ("MODIFY" modify-interactor)
	     ("DESTROY" destroy-interactor)
	     ("SAVE" write-interactor)
	     ("C32" display-interactor-properties)
	     ("CANCEL" cancel-interactor-changes))))


;;;************************
;;;  FONT-PARAMETER-PANEL
;;;************************

(create-instance 'FONT-PARAMETER-PANEL opal:aggregadget
   (:left (o-formula (+ 20 (gvl :parent :left))))
   (:width (o-formula (+ (gvl :text :width) 15 (gvl :panel :width))))
   (:parts
    `((:text ,opal:text
	     (:left ,(o-formula (gvl :parent :left)))
	     (:top ,(o-formula (opal:gv-center-y-is-center-of
				(gvl :parent :panel))))
	     (:string ,(o-formula (gvl :parent :title)))
	     (:font ,*bold-font*))
      (:panel ,garnet-gadgets:radio-button-panel
;	      (:constant ,(o-formula (gvl :parent :constant)))
	      (:constant (t :except :left :top :width :height :items))
              (:left ,(o-formula (+ 15 (opal:gv-right (gvl :parent :text)))))
              (:top ,(o-formula (gvl :parent :top)))
	      (:width ,(o-formula
                        (let ((width 0)
                              (h-spacing (gvl :h-spacing)))
                          (gvl :radio-button-list :components)
                          (opal:do-components (gvl :radio-button-list)
                             #'(lambda (button)
                                 (setf width (+ width h-spacing
                                                (g-value button :width)))))
                          width)))
              (:direction :horizontal)
              (:fixed-width-p NIL)
              (:font ,opal:default-font)
              (:value ,(o-formula (gvl :parent :value)))

	      (:items ,(o-formula (gvl :parent :items)))
	      (:selection-function
	       ,#'(lambda (gadget value)
		 (let* ((standard-font-panel (g-value gadget :parent :parent))
			font)

		   ;; White-out other panels
		   (wipe-file-panel) (wipe-formula-button)

		   ;; Record selection in :font-spec slot
		   (setf (nth (g-value gadget :parent :rank) 
			      (g-value standard-font-panel :font-spec))
			      value)

		   ;; Ensure that change is propagated to other panels
		   (mark-as-changed standard-font-panel :font-spec)

		   ;; get the new font and store it in the :value slot
		   ;; of the text properties window
		   (setf font 
			 (get-font-from-spec
			  (g-value standard-font-panel :font-spec)))
		   (s-value text-properties-win :value font)

		   ;; Apply resulting font to all primary text selections
		   (apply-font font))))))))

;;;*******************
;;;  START-WHERE
;;;*******************

(create-instance 'START-WHERE opal:aggregadget
   (:maybe-constant '(:left :top :width :height))
   (:left (o-formula (+ 10 (gvl :parent :left))))
   (:top (o-formula (+ (opal:gv-bottom (gvl :parent :known-as)) 10)))
   (:parts
    `((:titled-frame ,TITLED-FRAME
	  (:string ":start-where"))
      (:contents ,opal:aggregadget
	  (:width ,(o-formula (- (opal:gv-right (gvl :type-restrict))	
			 (gvl :other-button :left))))
	  (:height ,(o-formula (- (opal:gv-bottom (gvl :other-button))
				  (gvl :select-box-panel :top))))
	  (:value ,(o-formula (gvl :parent :value)))
	  (:field-string ,(o-formula (gvl :parent :field-string)))
	  (:parts
	   ((:select-box-panel ,SELECT-BOX-PANEL)
	    (:or-1 ,opal:text
;	        (:constant ,(o-formula (gvl :parent :parent :constant)))
		(:constant (t))
		(:left ,(o-formula (+ 30 (gvl :parent :parent :left))))
		(:top ,(o-formula (opal:gv-bottom
				   (car (gvl :parent :select-box-panel
					     :components)))))
		(:string "or"))
	    (:or-2 ,opal:text
;		(:constant ,(o-formula (gvl :parent :parent :constant)))
		(:constant (t))
		(:left ,(o-formula (+ 30 (gvl :parent :parent :left))))
		(:top ,(o-formula (opal:gv-bottom
				   (gvl :parent :select-box-panel))))
		(:string "or"))


	    (:other-button ,garnet-gadgets:text-button
;		(:constant ,(o-formula (when (gvl :parent :parent :constant)
;					     '(t :queue))))
		(:constant (t))
		(:queue ,(o-formula (gvl :window :queue)))
		(:inter ,(o-formula (gvl :window :inter)))
		(:left ,(o-formula (+ 15 (gvl :parent :parent :left))))
		(:top ,(o-formula (+ 5 (opal:gv-bottom (gvl :parent :or-2)))))
		(:string "Other")
		(:selected ,(o-formula (string= (gvl :string) (gvl :parent :select-box-panel :value))))
		(:gray-width 3) (:shadow-offset 5) (:text-offset 3)
		(:selection-function show-start-where-win)
		(:final-feedback-p t))
	    (:other-box ,text-box
		(:left ,(o-formula (+ 10 (opal:gv-right
					  (gvl :parent :other-button)))))
		(:top ,(o-formula (opal:gv-center-y-is-center-of
				   (gvl :parent :other-button))))
		(:string ,(o-formula (if (gvl :parent :other-button :selected)
					       (gvl :parent :select-box-panel :field-string)))))
	    (:type-restrict ,garnet-gadgets:x-button
;		(:constant ,(o-formula (when (gvl :parent :parent :constant)
;					     '(t :queue))))
		(:constant (t))
		(:value ,(o-formula (gvl :parent :select-box-panel :type)))
	        (:left ,(o-formula (+ 10 (opal:gv-right
					  (gvl :parent :other-box)))))
		(:top ,(o-formula (gv-center-my-top
				   (gvl :parent :other-button))))
		(:selection-function prompt-for-type-restrict)
		(:queue ,(o-formula (gvl :window :queue)))
		(:inter ,(o-formula (gvl :window :inter)))
		(:string "Type restriction:"))))))))


(create-instance 'lap-radio-button-feedback opal:circle
  (:constant '(:width :height))
  (:draw-function :xor)
  (:filling-style opal:black-fill)
  (:line-style nil)
  (:fast-redraw-p t)
  (:white-frame (o-formula (gvl :obj-over :white-frame)))
  (:left (o-formula (opal:gv-center-x-is-center-of (gvl :white-frame))))
  (:top (o-formula (opal:gv-center-y-is-center-of (gvl :white-frame))))
  (:width (o-formula (- (gvl :white-frame :width) 6)))
  (:height (o-formula (- (gvl :white-frame :height) 6)))
  (:visible (o-formula (gvl :obj-over)))
  (:obj-over nil))

(create-instance 'LAP-RADIO-BUTTON OPAL:AGGREGADGET
  (:maybe-constant '(:left :top :width :height))
  (:LEFT 265)
  (:TOP 49)
  (:WIDTH 27)
  (:HEIGHT 27)
  (:pretend-to-be-leaf t)
  (:parts `(
    (:SHADOW ,OPAL:CIRCLE
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:BLACK-FILL)
      (:LINE-STYLE ,OPAL:LINE-0)
      (:DIAMETER 36)
      (:RADIUS ,(o-formula (/ (GVL :DIAMETER) 2) 18))
      (:LEFT ,(formula `(+ (GVL :PARENT :LEFT ) 5 ) 270))
      (:TOP ,(formula `(+ (GVL :PARENT :TOP ) 5 ) 54))
      (:WIDTH 23)
      (:HEIGHT 23))
    (:GRAY-FRAME ,OPAL:CIRCLE
      (:LINE-STYLE ,OPAL:LINE-0)
      (:FILLING-STYLE ,OPAL:GRAY-FILL)
      (:DRAW-FUNCTION :COPY)
      (:LEFT ,(formula `(if (gvl :parent :interim-selected)
			    (gvl :parent :shadow :left)
			    (+ (GVL :PARENT :SHADOW :LEFT ) -5 )) 265))
      (:TOP ,(formula `(if (gvl :parent :interim-selected)
			   (gvl :parent :shadow :top)
			   (+ (GVL :PARENT :SHADOW :TOP ) -5 )) 49))
      (:WIDTH 23)
      (:HEIGHT 23))
    (:WHITE-FRAME ,OPAL:CIRCLE
      (:DRAW-FUNCTION :COPY)
      (:FILLING-STYLE ,OPAL:WHITE-FILL)
      (:LINE-STYLE ,OPAL:LINE-0)
      (:DIAMETER 21)
      (:RADIUS ,(o-formula (/ (GVL :DIAMETER) 2) 21/2))
      (:LEFT ,(formula `(+ (GVL :PARENT :GRAY-FRAME :LEFT ) 3 ) 268))
      (:TOP ,(formula `(+ (GVL :PARENT :GRAY-FRAME :TOP ) 3 ) 52))
      (:WIDTH 17)
      (:HEIGHT 17)))))

(create-instance 'labeled-lap-radio-button lap-radio-button
    (:maybe-constant '(:string))
    (:width (o-formula (+ (gvl :shadow :width) (gvl :label :width) 15)))
    (:string "")
    (:parts `(:shadow :gray-frame :white-frame
	      (:label ,opal:text
		     (:constant (t))
		     (:left ,(o-formula (+ (opal:gv-right 
					    (gvl :parent :shadow)) 10)))
		     (:top ,(o-formula (opal:gv-center-y-is-center-of 
					(gvl :parent :shadow))))
		     (:string ,(o-formula (gvl :parent :string)))))))



