;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Designed and implemented by Brad Myers

#|
============================================================
Change log:
    7/09/93 Andrew Mickish - Added :initial-text to :type-gadgets list
    7/01/93 Andrew Mickish - Moved Gilt-Error and Sel-Obj-Value here from
              gilt.lisp; Moved Show-Save/Read-Dialog to motif-gilt-
              save/read.lisp
    2/17/93 Brad Myers - moved more functions here so motif- and gilt-gadgets
                         are just the actual gadget windows
                       - support for pop-up gadgets, like menubars
   11/20/92 Andrew Mickish - Added priority levels for multi-selection;
              moved RunGadgetActiveForm, etc., here from gilt-gadgets
    3/26/92 Andrew Mickish - Added Invalid-Pathname-p
     2/9/92 Brad Myers - broke off from gilt-gadgets and motif-gilt-gadgets.
============================================================
|#


(in-package "GILT")

(declaim (special *Selection-Obj* *Run-Build-Obj* *Objs-Agg*
		  *Top-Gadget-Name* *Last-Filename*
		  *Main-Win* *IB-Win* *garnet-Ib-Win* *motif-Ib-Win*
		  Save-File Read-File text-edit))

;;; An association list which the save function uses to get the correct
;;; loader file names
(defparameter *load-file*
 '(("V-SCROLL-BAR" "V-SCROLL")("H-SCROLL-BAR" "H-SCROLL")
   ("TEXT-BUTTON-PANEL" "TEXT-BUTTONS")("X-BUTTON-PANEL" "X-BUTTONS")
   ("RADIO-BUTTON-PANEL" "RADIO-BUTTONS")

   ("MOTIF-V-SCROLL-BAR" "MOTIF-V-SCROLL")
   ("MOTIF-H-SCROLL-BAR" "MOTIF-H-SCROLL")
   ("MOTIF-TEXT-BUTTON-PANEL" "MOTIF-TEXT-BUTTONS")
   ("MOTIF-CHECK-BUTTON-PANEL" "MOTIF-CHECK-BUTTONS")
   ("MOTIF-RADIO-BUTTON-PANEL" "MOTIF-RADIO-BUTTONS")))


(defparameter *work-win* NIL) ; work window
(defparameter *Error-Gadget* NIL)  ; A gadget for reporting errors


(create-instance 'wheat opal:color
  (:red 0.9607843)
  (:green 0.87058824)
  (:blue 0.7019608))

(create-instance 'wheat-fill opal:filling-style
  (:foreground-color wheat))


;;;  The RunGadgetActiveForm, etc., formulas were made mostly obsolete
;;;  by putting the multi-selection gadget at a higher priority level

;; never run the objects in the gadget selection window
(defparameter RunGadgetActiveForm
  (o-formula (and (not (eq (gvl :window) *Ib-Win*))
		  (eq :run (gv *Run-Build-Obj* :value)))))
(defparameter BuildGadgetActiveForm
  (o-formula (eq :build (gv *Run-Build-Obj* :value))))

;;; Move takes precedence over text editing, popping up sub-objects
;;; takes precedence text editing. 

(defvar *Selection-Priority-Level*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))
(defvar *Higher-than-Selection-Priority-Level*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))
(defvar *Higher-Than-Text-Edit-Level*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))
(defvar *Selection-Move-Grow-Priority-Level*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))
(defvar *Selection-Grow-Multiple-Priority-Level*
  (create-instance NIL inter:priority-level
    (:stop-when :if-any)))


(pushnew *Selection-Priority-Level* inter:priority-level-list)
(pushnew *Higher-than-Selection-Priority-Level* inter:priority-level-list)
(pushnew *Higher-Than-Text-Edit-Level* inter:priority-level-list)
(pushnew *Selection-Move-Grow-Priority-Level* inter:priority-level-list)
(pushnew *Selection-Grow-Multiple-Priority-Level* inter:priority-level-list)

(defparameter LinepForm
  (o-formula (let ((objs (gv *selection-obj* :value)) obj)
	       (cond ((cdr objs) NIL) ;not line if multiple
		     ((setq obj (car objs))
		      (g-value obj :line-p))
		     (T NIL)))))


(defparameter leftform (o-formula (first (gvl :box))))
(defparameter topform (o-formula (second (gvl :box))))
(defparameter widthform (o-formula (third (gvl :box))))
(defparameter heightform (o-formula (fourth (gvl :box))))


;;; Set the value slot, but first make sure that formulas are set up by
;;; calling g-value
(defun Init-Value (obj new-val)
  (g-value obj :value) ; need to do this to set up the dependencies
  (s-value obj :value new-val))


;; This function pops up a font, line-style or filling-style dialog window
;; when the user presses on an icon.
(defun Pop-Up-Prop-Dialog (icon-gadget)
  (let ((func (g-value icon-gadget :creator-function))
	(slot (car (gg:Get-Val-For-PropSheet-Value
		    (g-value icon-gadget :parent)))))
    (multiple-value-bind (left top)
      (opal:convert-coordinates (g-value icon-gadget :window)
				(g-value icon-gadget :left)
				(opal:bottom icon-gadget) NIL)
      (setq top (+ 10 top))
      (funcall func left top (g-value *selection-obj* :value) slot))))

;;This function creates a pop-up-from-icon gadget for a font
(defun Font-for ()
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Font-Dialog)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))

;;This function creates a pop-up-from-icon gadget for a line-style
(defun Line-style-for ()
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Line-Props-Dialog)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))

;;This function creates a pop-up-from-icon gadget for a filling-style
(defun Fill-style-for ()
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Fill-Props-Dialog)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))


(defun Color-DB-for ()
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Color-Dialog-For)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))


(defun Gilt-Error (str)
  (opal:update *work-win*)
  (garnet-gadgets:display-error *error-gadget* str))


;; return T if error (not number or nil)
(defun nil-or-num (val)
  (unless (or (null val)
	      (numberp val))
    (gilt-error "Value must be a number or NIL.")
    T))

;; return T if error (not number)
(defun num-only (val)
  (unless (numberp val)
    (gilt-error "Value must be a number.")
    T))

;; return T if error (file not found)
(defun invalid-pathname-p (p)
  (unless (probe-file p)
    (gilt-error (format NIL "File not found:~%~S" p))
    T))

;;; Need a point to leaf in the text object, so can find the string (since
;;; it is in the string itself and not in a leaf).
(Defun Fake-Point-to-Leaf (agg x y &key type)
  (let ((ret (if (and (or (null type) (is-a-p agg type))
		      (opal:point-in-gob agg x y))
		 agg
		 NIL)))
    ret))


;;; Used to make title of gadget window

(defvar TITLE-FONT (opal:get-standard-font :serif :italic :large))

;;; This function loads the bitmap specified from the Gilt directory
(defun Get-Gilt-Bitmap (bitmapname)
  (opal:read-image (merge-pathnames bitmapname
				    common-lisp-user::Garnet-Gilt-Bitmap-PathName)))

;;; This function loads the pixmap specified from the Gilt directory
(defun Get-Gilt-Pixmap (pixmapname)
  (opal:read-xpm-file (merge-pathnames pixmapname
				       common-lisp-user::Garnet-Gilt-Bitmap-PathName)))


(create-instance 'gray-out opal:rectangle
   (:obj-over NIL)
   (:line-style NIL)
   (:filling-style (create-instance NIL opal:filling-style
		      (:fill-style :stippled)
		      (:stipple opal::light-gray-fill-bitmap)))
   (:left (o-formula (gvl :obj-over :left)))
   (:top (o-formula (gvl :obj-over :top)))
   (:width (o-formula (gvl :obj-over :width)))
   (:height (o-formula (gvl :obj-over :height)))
   (:visible NIL)) ; usually replace this with a formula



(create-instance 'mygrayline opal:line-style
  (:constant T)
  (:line-thickness 2)
  (:foreground-color opal:MOTIF-GRAY))
(create-instance 'mywhiteline opal:line-style
  (:constant T)
  (:line-thickness 2)
  (:foreground-color opal:white))

;;; Prototype for the Gilt palette windows.
(create-instance 'IB-OBJS opal:aggregadget
  (:string "Gadgets")
  (:widget-set :garnet)
  (:parts
     `((:title-line ,opal:line
	(:x1 2)	
	(:constant (:x1 :line-style))
	(:y1 ,(o-formula (opal:gv-center-y (gvl :parent :title-string))))
	(:x2 ,(o-formula (let ((win (gvl :window)))
			   (- (gv win :width)(gv win :RIGHT-BORDER-WIDTH)
			      (gv win :LEFT-BORDER-WIDTH) 2))))
	(:y2 ,(o-formula (gvl :y1))))
       (:title-string ,opal:text
	(:string ,(o-formula (gvl :parent :string)))
	(:font ,title-font)
	(:constant (T :except :left))
	(:fill-background-p T)
	(:line-style ,(o-formula
		       (if (and (eq (gvl :parent :widget-set) :motif)
				(gv opal:color :color-p))
			   (create-instance NIL opal:line-style
			      (:constant T)
			      (:background-color opal:MOTIF-GRAY))
			   opal:default-line-style)))
	(:left ,(o-formula (floor (- (gvl :window  :width)
				     (gvl :width)) 2)))
	(:top 0))
       (:selectable-objs ,opal:aggregate)  ; filled explicitly below
       (:feedback ,opal:rectangle
	(:obj-over NIL)
	(:line-style ,opal:line-2)
	;; visible if obj-over and in build mode
	(:visible ,(o-formula (and (gvl :obj-over)
				   (eq :build (gv *Run-Build-Obj* :value)))))
	(:left ,(o-formula (- (gvl :obj-over :left) 5)))
	(:top ,(o-formula (- (gvl :obj-over :top) 5)))
	(:width ,(o-formula (+ (gvl :obj-over :width) 10)))
	(:height ,(o-formula (+ (gvl :obj-over :height) 10)))
	(:fast-redraw-p :redraw)
	(:fast-redraw-line-style ,(o-formula
				   (if (and (eq (gvl :parent :widget-set)
						:motif)
					    (gv opal:color :color-p))
				       mygrayline
				       mywhiteline))))
       (:cover-up ,gray-out
	(:obj-over ,(o-formula (gvl :window)))
	(:left 0) ; override left and top, so will be zero
	(:top 0)
	;; visible if in run mode
	(:visible ,(o-formula (eq :run (gv *Run-Build-Obj* :value)))))))
    (:interactors
     `((:select ,inter:button-interactor
	(:window ,(o-formula  (gv-local :self :operates-on :window)))
	(:how-set :set)
	(:continuous NIL)
	(:final-feedback-obj ,(o-formula (gvl :operates-on :feedback)))
	(:active ,(formula BuildGadgetActiveForm))
	(:start-event :any-mousedown)
	(:start-where
	 ,(o-formula (list :element-of
			   (gvl :operates-on :selectable-objs))))))))


(defparameter *prop-sheet* NIL)

(defparameter pop-up-color
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Color-Dialog-For)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))

(defparameter pop-up-font
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Font-Dialog)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))

(defparameter pop-up-line-style
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Line-Props-Dialog)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))

(defparameter pop-up-fill-style
  (create-instance NIL Garnet-gadgets:Pop-Up-From-Icon
    (:constant :icon-image :pop-up-function)
    (:creator-function 'Show-Fill-Props-Dialog)
    (:pop-up-function 'Pop-Up-Prop-Dialog)))

(defun Make-Prop-Sheet (error-gadget)
  (setq *prop-sheet*
	(create-instance NIL garnet-gadgets:motif-prop-sheet-for-obj-with-done
	  (:constant '(T :except :obj :slots))
	  (:done-Function 'Prop-Sheet-Finish)
	  (:error-gadget error-gadget)
	  (:union? T)
	  (:type-gadgets
	   `(;; don't access the positions here, since will be in the :box slot
	     (:left NIL) (:top NIL) (:width NIL) (:height NIL)
	     (:x1 NIL) (:x2 NIL) (:y1 NIL) (:y2 NIL) (:initial-text NIL)
	     (:selection-function NIL) ; omit selection-function since using
					; select-function instead.
	     (:image NIL) ; omit image (for bitmaps) since is a formula
	     (:KEYBOARD-SELECTION-P NIL) ; don't bother with keyboard stuff
	     (:KEYBOARD-SELECTION NIL)
	     ;; special hack for Popup-Menu-Button
	     ((or (member :below :left :right) list)
	      (member :below :left :right))
	     ;; get the types out of slots, to make sure have them right
  	     (,(g-type opal:line-style :foreground-color) ,pop-up-color)
	     (,(g-type opal:text :font) ,pop-up-font)
	     (,(g-type opal:line :line-style) ,pop-up-line-style)
	     (,(g-type opal:rectangle :filling-style) ,pop-up-fill-style)
	     )))))
	  
	     
(defun Make-Error-Gadget (work-win)
  (create-instance NIL garnet-gadgets:motif-error-gadget
    (:parent-window work-win)))

(defvar italic-font (opal:get-standard-font NIL :italic NIL))

;; ** This seems to make the system too slow.  ?Too many dependencies??
;; (defun diff-values (obj-list slot)
;;  (let ((val (gv (first obj-list) slot)))
;;    (dolist (obj (cdr obj-list))
;;      (unless (= val (gv obj slot))
;;	(return T)))))

;; If slot is left or top, finds the minimum of all objects.  If width or
;; height, finds maximum.  Uses the fake rectangle inside the
;; multi-graphics-selection object!
(defun Get-Multi-Value (objs slot)
  (declare (ignore objs))
  (let ((fake-rect (gv *selection-obj* :move-grow-it :obj-to-change)))
    (prin1-to-string (gv fake-rect slot))))

(defun Sel-Obj-Value (slot)
  (let* ((objs (gv *selection-obj* :value))
	 (obj (car objs))
	 (multi (cdr objs)))
    (cond ((null obj) "0")
	  (multi (Get-Multi-Value objs slot))
	  (T (prin1-to-string (gv obj slot))))))

(create-instance 'number-enter-proto garnet-gadgets:motif-scrolling-labeled-box
	     (:left 5) (:top 50) (:width 90)
	     (:slot NIL) ;set with a formula like (if (gvl :line-p) :x1 :left)
	     (:value (o-formula (Sel-Obj-Value (gvl :slot))))
	     (:field-offset 2)
	     (:field-font (o-formula
			   (let ((value (gv *Selection-obj* :value)))
			     ;; italic if multiple selections
			     (if (and value
				      (cdr value)
				      ;; (diff-values value (gvl :slot))
				      )
				 italic-font
				 opal:default-font))))
	     (:active-p (o-formula
			 (let ((value (gv *Selection-obj* :value)))
			   (and value
				(eq :build (gv *Run-Build-Obj* :value))))))
	     (:min-frame-width 40)
	     (:text-inter (o-formula (gvl :field-text :text-edit)))
	     (:parts
	      `((:label-text :modify
		 (:fast-redraw-p :rectangle)
		 (:fast-redraw-filling-style ,wheat-fill))
		:frame :field-text :sel-box)))

(defparameter *objects-with-popups* NIL)

(defun Clean-up-popups ()
  (let ((local-copy *objects-with-popups*))
    (setq *objects-with-popups* NIL)
    (s-value text-edit :extra-window NIL)
    (dolist (obj local-copy)
      (when (schema-p obj)
	(kr-send obj :special-popdown-func obj)))))

;;; Returns T if did pop up a sub-object, else returns NIL if either
;;; can't or already popped up.
(defun Pop-Up-Sub-Objs-If-Should (obj)
  (let (extra-win)
    (when (and (g-value obj :special-popup-func)
	       (setq extra-win (kr-send obj :special-popup-func obj)))
      (pushnew obj *objects-with-popups*)
      (s-value text-edit :extra-window extra-win)
      (s-value extra-win :*pop-up-window-from* obj)
      T)))
