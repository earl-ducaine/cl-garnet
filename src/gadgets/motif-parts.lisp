;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;


;;;  MOTIF-PARTS
;;
;;   This module is a collection of schema definitions required by the motif
;;   gadgets.
;; 
;;   Written by Andrew Mickish


;;;  CHANGE LOG
;;   04/19/93  Andrew Mickish - Added :field-stippled-line-style
;;   02/22/93  Brad Myers - Added MOTIF-RECT
;;   12/15/92  Andrew Mickish - Added type and parameter declarations;
;;               removed references to ERROR-PRIORITY-LEVEL
;;   06/24/92  Andrew Mickish - Added auto-repeating MOTIF-JUMP interactor
;;   04/09/92  Ed Pervin - Changed defvar of motif-tab-priority-level
;; 		 and error-priority-level to proclaim (to eliminate warning).
;;   04/07/92  Andrew Mickish - Changed default-fs's and default-ls's to
;;                              regular filling-styles and line-styles
;;   04/06/92  Andrew Mickish - Increased contrast in *shadow-value* and
;;                              *highlight-value*
;;   03/02/92  Andrew Mickish - Added field-stippled-ls to style-array
;;   02/28/92  Andrew Mickish - Implemented hash-table-oriented generation of
;;               motif line styles and filling styles.
;;   02/27/92  Andrew Mickish - Removed :leftdown case from stop-action of
;;               MOTIF-TAB-INTER
;;   02/26/92  Ed Pervin - Changed convert-aux to merely return three values,
;; 		 rather than creating an new (unneeded) opal:color object.
;;   01/28/92  Ed Pervin - Provide is not defined in CMUCL.
;;   12/05/91  Andrew Mickish - Changed :active formula of MOTIF-TRILL.
;;   10/13/91  Andrew Mickish - Changed :stop-when from :if-any to NIL.
;;               Added :leftdown case to stop-action of MOTIF-TAB-INTER.
;;   10/08/91  Andrew Mickish - Added fast-redraw to motif-selection-box
;;   05/13/91  Edward Pervin - In case statement, changed (NIL NIL)
;; 		 to ((NIL) NIL)
;;   04/17/91  Andrew Mickish - Fixed MOTIF-TAB-INTER to consider single
;;               object case
;;   04/17/91  Andrew Mickish - Changed occurrences of "motif-scrolling-text-
;;               box" to "motif-scrolling-labeled-box"
;;   04/08/91  Edward Pervin - #\control-tab not defined in KCL.
;;   03/01/91  Andrew Mickish - Created


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(MOTIF-TAB-PRIORITY-LEVEL MOTIF-TAB-INTER MOTIF-MENU-INTER
	    MOTIF-BACKGROUND MOTIF-RECT))
  (proclaim '(special MOTIF-TAB-PRIORITY-LEVEL MOTIF-SCROLLING-LABELED-BOX)))


(defvar MOTIF-GADGETS-INIT
  (dolist (pair '((:GAD-scroll-parts "GAD-scroll-parts")))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) CL-USER::Garnet-Gadgets-PathName)))))


;;  Values for changing the brightness of the foreground-color
(defvar *BACKGROUND-VALUE* .85)
(defvar *SHADOW-VALUE* .24)
(defvar *HIGHLIGHT-VALUE* 1.45)


;;; Color Utility Functions
;;

;; This function is used to compute the shades of the Motif colors in the
;; Motif gadgets.  Given a color and a brightness adjustment, the function
;; returns a color which is "brighter" or "dimmer" based on the adjustment
;; factor.
;;        | Y |   | .3   .59  .11 | | R |
;;        | I | = | .6  -.28 -.32 | | G |       ( Y = Brightness )
;;        | G |   | .21 -.52  .31 | | B |
;; Given the RGB values, multiply by the above matrix to get YIG values.
;; Multiply Y by the adjustment to get the new brightness, then multiply
;; the new YIG matrix by the inverse of the original 3x3 matrix.
(defun CONVERT-AUX (color adjustment)
  (if color
      ;; Return three values
      (let* ((red (g-value color :red))
	     (green (g-value color :green))
	     (blue (g-value color :blue))

	     (y (+ (* .3 red) (* .59 green) (* .11 blue)))
	     (i (+ (* .6 red) (* -.28 green) (* -.32 blue)))
	     (q (+ (* .21 red) (* -.52 green) (* .31 blue)))

	     (new-bright (* y adjustment))

	     (new-red (MAX 0 (MIN 1 (+ new-bright (* .95 i) (* .62 q)))))
	     (new-green (MAX 0 (MIN 1 (+ new-bright (* -.28 i) (* -.64 q)))))
	     (new-blue (MAX 0 (MIN 1 (+ new-bright (* -1.1 i) (* 1.7 q))))))

 	(values new-red new-green new-blue))
      ;; otherwise return NIL
  ))


(defun CONVERT-COLOR (adjustment)
  (multiple-value-bind (red green blue)
      (convert-aux (gvl :foreground-color) adjustment)
    (if red
      (create-instance NIL opal:color
        (:red red) (:green green) (:blue blue)))))


(defun MAKE-COLOR (r g b)
  (create-instance NIL opal:color (:red r) (:green g) (:blue b)))


(defun MAKE-MOTIF-FILLING (color-slot)
  (let ((color (gvl color-slot)))
     (if color
	 (create-instance NIL opal:filling-style
            (:foreground-color color)))))


(create-instance 'MOTIF-THICK-STIPPLED-LINE-STYLE opal:line-style
   (:constant T)
   (:line-thickness 2)
   (:stipple opal::gray-fill-bitmap))

(create-instance 'MOTIF-THIN-STIPPLED-LINE-STYLE opal:line-style
   (:constant T)
   (:stipple opal::gray-fill-bitmap))



;;; Predefined motif line-styles and filling-styles to improve performance
;;  in the usual case that everything is motif-gray.
;;
(defvar *foreground-fill-index* 0)
(defvar *background-fill-index* 1)
(defvar *shadow-fill-index* 2)
(defvar *highlight-fill-index* 3)
(defvar *foreground-ls-index* 4)
(defvar *shadow-ls-index* 5)
(defvar *highlight-ls-index* 6)
(defvar *stippled-ls-index* 7)
(defvar *thin-shadow-ls-index* 8)
(defvar *thin-highlight-ls-index* 9)
(defvar *field-stippled-ls-index* 10)
(defvar *style-array-hash-table* (make-hash-table))

(defun add-motif-style-array (foreground-color style-array)
  (setf (gethash foreground-color *style-array-hash-table*) style-array))
(defun get-motif-style-array (foreground-color)
  (gethash foreground-color *style-array-hash-table*))

(defun make-motif-style-array (foreground-color)
  (let* ((style-array (make-array '(11)))
	 
	 (background-color (multiple-value-call #'make-color
			    (convert-aux foreground-color *BACKGROUND-VALUE*)))
	 (shadow-color (multiple-value-call #'make-color
			 (convert-aux foreground-color *SHADOW-VALUE*)))
	 (highlight-color (multiple-value-call #'make-color
			    (convert-aux foreground-color *HIGHLIGHT-VALUE*)))
	 
	 (foreground-fill (cond
			    ((eq foreground-color opal:MOTIF-GRAY)
			     opal:MOTIF-GRAY-FILL)
			    ((eq foreground-color opal:MOTIF-BLUE)
			     opal:MOTIF-BLUE-FILL)
			    ((eq foreground-color opal:MOTIF-GREEN)
			     opal:MOTIF-GREEN-FILL)
			    ((eq foreground-color opal:MOTIF-ORANGE)
			     opal:MOTIF-ORANGE-FILL)
			    (t (create-instance NIL opal:filling-style
				 (:foreground-color foreground-color)))))
	 (background-fill (create-instance NIL opal:filling-style
			    (:foreground-color background-color)))
	 (shadow-fill (create-instance NIL opal:filling-style
			(:foreground-color shadow-color)))
	 (highlight-fill (create-instance NIL opal:filling-style
			   (:foreground-color highlight-color)))
	 
	 (foreground-ls (create-instance NIL opal:line-style
			  (:constant T)
			  (:line-thickness 2)
			  (:foreground-color foreground-color)))
	 (shadow-ls (create-instance NIL opal:line-style
		      (:constant T)
		      (:line-thickness 2)
		      (:foreground-color shadow-color)))
	 (highlight-ls (create-instance NIL opal:line-style
			 (:constant T)
			 (:line-thickness 2)
			 (:foreground-color highlight-color)))
	 (stippled-ls (create-instance NIL opal:line-style
                        (:constant T)
			(:background-color foreground-color)
			(:stipple opal::gray-fill-bitmap)))
	 (thin-shadow-ls (create-instance NIL opal:line-style
                           (:constant T)
			   (:line-thickness 1)
			   (:foreground-color shadow-color)))
	 (thin-highlight-ls (create-instance NIL opal:line-style
                              (:constant T)
			      (:line-thickness 1)
			      (:foreground-color highlight-color)))
	 (field-stippled-ls (create-instance NIL opal:line-style
                              (:constant T)
			      (:background-color background-color)
			      (:stipple opal::gray-fill-bitmap))))
    (setf (aref style-array *foreground-fill-index*) foreground-fill)
    (setf (aref style-array *background-fill-index*) background-fill)
    (setf (aref style-array *shadow-fill-index*) shadow-fill)
    (setf (aref style-array *highlight-fill-index*) highlight-fill)
    (setf (aref style-array *foreground-ls-index*) foreground-ls)
    (setf (aref style-array *shadow-ls-index*) shadow-ls)
    (setf (aref style-array *highlight-ls-index*) highlight-ls)
    (setf (aref style-array *stippled-ls-index*) stippled-ls)
    (setf (aref style-array *thin-shadow-ls-index*) thin-shadow-ls)
    (setf (aref style-array *thin-highlight-ls-index*) thin-highlight-ls)
    (setf (aref style-array *field-stippled-ls-index*) field-stippled-ls)
    (add-motif-style-array foreground-color style-array)
    style-array))
    


;; This function is used as the :start-where of the button panel interactors
;; to make sure that "invalid" items are not selected.
;;
(defun MOTIF-ELEMENT-OF-NOT-ILLEGAL (obj inter event)
  (declare (ignore inter))
  (when (eq (g-value obj :window) (inter:event-window event)) ;--bam
    (let ((el (opal:point-to-component obj (Inter:event-x event)
				           (Inter:event-y event))))
      (if (and el (g-value el :active-p)) el NIL))))


;; This function is called when the mouse is clicked in the trough of the
;; scroll bars or the slider.  It causes the :value to be inc/decremented
;; by :page-incr.
;;
(defun MOTIF-JUMP-FN (interactor mouse-coordinate indicator-coordinate)
  (let* ((bar (g-value interactor :operates-on))
	 (value (g-value bar :value))
	 (val-1 (g-value bar :val-1))
	 (val-2 (g-value bar :val-2))
	 (page-incr (g-value bar :page-incr)))

    (if (< mouse-coordinate indicator-coordinate)

	;; Mouse clicked above (or to the left of) the indicator
	(if (< val-1 val-2)
	    (let ((thresh-val (+ val-1 page-incr)))
	      (if (> value thresh-val)
		  (s-value bar :value (- value page-incr))
		  (s-value bar :value val-1)))
	    (let ((thresh-val (- val-1 page-incr)))
	      (if (< value thresh-val)
		  (s-value bar :value (+ value page-incr))
		  (s-value bar :value val-1))))

	;; Mouse clicked below (or to the right of) the indicator
	(if (< val-1 val-2)
	    (let ((thresh-val (- val-2 page-incr)))
	      (if (< value thresh-val)
		  (s-value bar :value (+ value page-incr))
		  (s-value bar :value val-2)))
	    (let ((thresh-val (+ val-2 page-incr)))
	      (if (> value thresh-val)
		  (s-value bar :value (- value page-incr))
		  (s-value bar :value val-2)))))))


;; This function is used by the keyboard interactors in the scroll bars and
;; slider to change the :value with the arrow keys.
;; FMG It is also used by the scroll wheel support (the scroll wheel is treated
;; as if it were clicking on the trill button).
;;
(defun MOTIF-KEY-TRILL-FN (interactor obj)
  (declare (ignore obj))
  (let* ((bar (g-value interactor :operates-on))
	 (value (g-value bar :value))
	 (val-1 (g-value bar :val-1))
	 (val-2 (g-value bar :val-2))
	 (inc-by (g-value bar :scr-incr))
	 (up-or-left (first (g-value interactor :start-event))))
    (if (eq (inter:event-char inter:*current-event*) up-or-left)
	(if (< val-1 val-2)
	    (let ((thresh-val (+ val-1 inc-by)))
	      (if (> value thresh-val)
		  (s-value bar :value (- value inc-by))
		  (s-value bar :value val-1)))
	    (let ((thresh-val (- val-1 inc-by)))
	      (if (< value thresh-val)
		  (s-value bar :value (+ value inc-by))
		  (s-value bar :value val-1))))
	(if (< val-1 val-2)
	    (let ((thresh-val (- val-2 inc-by)))
	      (if (< value thresh-val)
		  (s-value bar :value (+ value inc-by))
		  (s-value bar :value val-2)))
	    (let ((thresh-val (+ val-2 inc-by)))
	      (if (> value thresh-val)
		  (s-value bar :value (- value inc-by))
		  (s-value bar :value val-2)))))
    (kr-send bar :selection-function bar (g-value bar :value))))


;; This object is a prototype for all of the Motif gadgets.  It defines all
;; the color slots for the Motif gadgets.
(create-instance 'MOTIF-GADGET-PROTOTYPE opal:aggregadget
   (:foreground-color opal:MOTIF-GRAY)
   (:style-array (o-formula (let ((foreground-color (gvl :foreground-color)))
			      (or (get-motif-style-array foreground-color)
				  (make-motif-style-array foreground-color)))))
   (:foreground-fill
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *foreground-fill-index*)
		   opal:white-fill)))
   (:background-fill
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *background-fill-index*)
		   opal:white-fill)))
   (:shadow-fill
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *shadow-fill-index*)
		   opal:black-fill)))
   (:highlight-fill
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *highlight-fill-index*)
		   opal:gray-fill)))
   (:foreground-line-style (o-formula
			    (aref (gvl :style-array) *foreground-ls-index*)))
   (:shadow-line-style
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *shadow-ls-index*)
		   opal:line-2)))
   (:highlight-line-style
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *highlight-ls-index*)
		   MOTIF-THICK-STIPPLED-LINE-STYLE)))
   (:stippled-line-style
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *stippled-ls-index*)
		   MOTIF-THIN-STIPPLED-LINE-STYLE)))
   (:thin-shadow-line-style
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *thin-shadow-ls-index*)
		   opal:default-line-style)))
   (:thin-highlight-line-style
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *thin-highlight-ls-index*)
		   MOTIF-THIN-STIPPLED-LINE-STYLE)))
   (:field-stippled-line-style
    (o-formula (if (gv opal:color :color-p)
		   (aref (gvl :style-array) *field-stippled-ls-index*)
		   MOTIF-THIN-STIPPLED-LINE-STYLE))))


;; This object is the "raised" or "depressed" box that is ubiquitous in Motif.
;;
(create-instance 'MOTIF-BOX opal:aggregadget
   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:bottom (o-formula (+ (gvl :top) (gvl :height))))
   (:depressed-p NIL)
   (:parts
    `((:top-box ,opal:rectangle
	     (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
	     (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
	     (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
	     (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (let ((p (kr-path 0 :parent :parent)))
				 (if (gv (kr-path 1 :parent) :depressed-p)
				     (gv p :shadow-fill)
				     (gv p :highlight-fill))))))
      (:bottom-box ,opal:rectangle
	     (:left ,(o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
	     (:top ,(o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
	     (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 2)))
	     (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 2)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (let ((p (kr-path 0 :parent :parent)))
				 (if (gv (kr-path 1 :parent) :depressed-p)
				     (gv p :highlight-fill)
				     (gv p :shadow-fill))))))
      (:gray-box ,opal:rectangle
	     (:left ,(o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
	     (:top ,(o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
	     (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 4)))
	     (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 4)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (let* ((p (kr-path 0 :parent :parent)))
				 (if (gv (kr-path 1 :parent) :depressed-p)
				     (gv p :background-fill)
				     (gv p :foreground-fill)))))))))


(create-instance 'MOTIF-RECT gg::MOTIF-GADGET-PROTOTYPE
   :declare ((:parameters :left :top :width :height :foreground-color :visible
			  :depressed-p)
	    (:type (kr-boolean :depressed-p :visible)
		   ((is-a-p opal:color) :foreground-color)
		   )
	    (:maybe-constant :left :top :width :height :depressed-p
			     :foreground-color :visible))
   (:left 0)(:top 0)(:width 50)(:height 50)
   (:right (o-formula (+ (gvl :left) (gvl :width))))
   (:bottom (o-formula (+ (gvl :top) (gvl :height))))
   (:depressed-p NIL)
   (:parts
    `((:top-box ,opal:rectangle
	     (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
	     (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
	     (:width ,(o-formula (gv (kr-path 0 :parent) :width)))
	     (:height ,(o-formula (gv (kr-path 0 :parent) :height)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (let ((p (kr-path 0 :parent)))
				 (if (gv p :depressed-p)
				     (gv p :shadow-fill)
				     (gv p :highlight-fill))))))
      (:bottom-box ,opal:rectangle
	     (:left ,(o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
	     (:top ,(o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
	     (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 2)))
	     (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 2)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (let ((p (kr-path 0 :parent)))
				 (if (gv p :depressed-p)
				     (gv p :highlight-fill)
				     (gv p :shadow-fill))))))
      (:gray-box ,opal:rectangle
	     (:left ,(o-formula (+ 2 (gv (kr-path 0 :parent) :left))))
	     (:top ,(o-formula (+ 2 (gv (kr-path 0 :parent) :top))))
	     (:width ,(o-formula (- (gv (kr-path 0 :parent) :width) 4)))
	     (:height ,(o-formula (- (gv (kr-path 0 :parent) :height) 4)))
	     (:line-style NIL)
	     (:filling-style ,(o-formula
			       (let* ((p (kr-path 0 :parent)))
				 (if (gv p :depressed-p)
				     (gv p :background-fill)
				     (gv p :foreground-fill)))))))))


;; This is the outline box associated with keyboard operations on the
;; gadgets.
;; 
(create-instance 'MOTIF-SELECTION-BOX opal:rectangle
   (:left (o-formula (gvl :obj-over :left)))
   (:top (o-formula (gvl :obj-over :top)))
   (:width (o-formula (gvl :obj-over :width)))
   (:height (o-formula (gvl :obj-over :height)))
   (:obj-over (o-formula (gv (kr-path 0 :parent) :keyboard-selection-obj)))
   (:visible (o-formula (if (gv (kr-path 0 :parent) :visible)
			    (and (gv (kr-path 0 :parent) :keyboard-selection-p)
				 (gvl :obj-over) (gv (kr-path 0 :parent) :active-p)))))
   (:line-style opal:line-2)
   (:fast-redraw-p (if (g-value opal:color :color-p) :redraw T))
   (:draw-function (if (g-value opal:color :color-p) :copy :xor))
   (:fast-redraw-filling-style NIL)
   (:fast-redraw-line-style (o-formula (gv (kr-path 0 :parent)
					   :foreground-line-style))))
				 

;; Given a window, this rectangle will expand to the window's dimensions and
;; will have a :filling-style corresponding to the :foreground-color.  On a
;; black-and-white screen, the :filling-style will be NIL.
(create-instance 'MOTIF-BACKGROUND opal:rectangle
   :declare ((:parameters :foreground-color)
	     (:type ((is-a-p opal:color) :foreground-color)))
   (:window NIL)
   (:left 0)
   (:top 0)
   (:width (o-formula (gvl :window :width)))
   (:height (o-formula (gvl :window :height)))
   (:foreground-color opal:MOTIF-GRAY)
   (:line-style NIL)
   (:filling-style (o-formula
		    (if (gv opal:color :color-p)
			(let ((fg (gvl :foreground-color)))
			  (cond
			    ((eq fg opal:MOTIF-GRAY) opal:MOTIF-GRAY-FILL)
			    ((eq fg opal:MOTIF-BLUE) opal:MOTIF-BLUE-FILL)
			    ((eq fg opal:MOTIF-ORANGE) opal:MOTIF-ORANGE-FILL)
			    ((eq fg opal:MOTIF-GREEN) opal:MOTIF-GREEN-FILL)
			    (t (create-instance NIL opal:filling-style
				 (:foreground-color fg)))))
			NIL))))

;; This interactor is for the arrows on the scroll bars and slider.
;;
(create-instance 'MOTIF-TRILL inter:button-interactor
   (:active (o-formula (let ((p (gvl :operates-on :parent)))
			 (and (gvl :operates-on :visible)
			      (gvl :window)
			      (gv p :scroll-p) (gv p :active-p)))))
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula (list :in-box (gvl :operates-on))))
   (:timer-repeat-p T)
   (:extra-function #'val-1-fn)
   (:final-function #'(lambda (interactor obj)
			(kr-send interactor :extra-function
				 interactor obj)
			(kr-send (g-value interactor :operates-on :parent)
				 :selection-function
				 (g-value interactor :operates-on :parent)
				 (g-value interactor :operates-on :parent
					  :value)))))


;; This interactor is used when the mouse is clicked in the trough of the
;; scroll bars or slider
(create-instance 'MOTIF-JUMP inter:Button-Interactor
  (:window (o-formula (gv-local :self :operates-on :window)))
  (:active (o-formula (let ((p (gvl :operates-on)))
			(and (gvl :window)
			     (gv p :scroll-p) (gv p :active-p)))))
  (:start-where (o-formula (list :in-box (gvl :operates-on :bounding-area))))
  (:running-where (o-formula (list :in-box (gvl :operates-on
						:bounding-area))))
  (:timer-repeat-p T)
  (:final-function
   #'(lambda (interactor obj)
       (MOTIF-JUMP-FN interactor (inter:event-x inter:*current-event*)
		      (g-value interactor :operates-on :indicator :left))
       (SLIDE-FINAL-FN interactor obj))))


;; This interactor is used when the mouse is clicked on a single button
;; (i.e., not part of a panel).
;;
(create-instance 'MOTIF-SINGLE-PRESS inter:Button-Interactor
   (:active (o-formula (and (gvl :window) (gvl :operates-on :active-p))))
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:start-where (o-formula (list :in-box (gvl :operates-on))))
   (:how-set (o-formula (if (gvl :operates-on :toggle-p) :toggle :set)))
   (:final-function
    #'(lambda (interactor button)
	(declare (ignore interactor))
	(let ((selected (g-value button :selected)))
	  ;; Execute selection function
	  (kr-send button :selection-function button selected)))))


;; This interactor is used when the keyboard is used on a single button
;; (i.e., not part of a panel).
;;
(create-instance 'MOTIF-SINGLE-KEY inter:Button-Interactor
   (:active (o-formula (let ((p (kr-path 0 :operates-on)))
			 (and (gvl :window) (gv p :active-p)
			      (gv p :keyboard-selection-p)))))
   (:window (o-formula (gv-local :self :operates-on :window)))
   (:continuous NIL)
   (:start-where T)
   (:start-event '#\space)
   (:final-function
    #'(lambda (interactor obj)
	(declare (ignore obj))
	(let* ((button (g-value interactor :operates-on))
	       (selected (if (g-value button :toggle-p)
			     (not (g-value button :selected))
			     T)))
	  (unless (g-value button :final-feedback-p)
	    (s-value button :interim-selected T)
	    (opal:update (g-value interactor :window))
	    (sleep .25)
	    (s-value button :interim-selected NIL))
	  (s-value button :selected selected)
	  (kr-send button :selection-function button selected)))))


;; This function creates a new priority level and adds it to the front
;; of the interactorts priority level list.  Thus, this level has higher
;; priority than the default inter-levels, and it will allow the
;; MOTIF-TAB-INTER to get a #\tab command before any other interactor in
;; the interface.
;;
(defun ADD-MOTIF-TAB-PRIORITY-LEVEL ()
  (unless (and (boundp 'MOTIF-TAB-PRIORITY-LEVEL)
	       (member MOTIF-TAB-PRIORITY-LEVEL inter:priority-level-list))
    (push (create-instance 'MOTIF-TAB-PRIORITY-LEVEL inter:priority-level
	    (:stop-when NIL))
	  inter:priority-level-list)))

(ADD-MOTIF-TAB-PRIORITY-LEVEL)


;; This interactor can be used to toggle the keyboard selection around a set
;; of motif gadgets.  When the :objects slot is set to a list of motif
;; gadgets, hitting the TAB key will rotate the keyboard selection through
;; the list.
;;
(create-instance 'MOTIF-TAB-INTER inter:button-interactor
   :declare ((:parameters :window :objects :rank :start-where
			  :start-event :waiting-priority :running-priority
			  :stop-action :final-function)
	     (:type ((or list (is-a-p inter:interactor-window)) :window)
		    (list :objects)
		    ((integer 0) :rank)
		    ((or list (member T)) :start-where)
		    ((or keyword character list) :start-event)
		    ((is-a-p inter:priority-level) :waiting-priority
		     :running-priority)
		    ((or null function symbol) :stop-action :final-function)))
   (:window NIL)
   (:continuous NIL)
   (:start-where T)
   (:start-event '(#\tab :control-tab))
   (:objects NIL)
   (:rank 0)
   (:waiting-priority MOTIF-TAB-PRIORITY-LEVEL)
   (:running-priority MOTIF-TAB-PRIORITY-LEVEL)
   (:stop-action
    #'(lambda (interactor final-obj-over)
	(declare (ignore final-obj-over))
	(let* ((prev-rank (g-value interactor :rank))
	       (objects (g-value interactor :objects))
	       (max-rank (- (length objects) 1))
	       (new-rank (case (inter:event-char inter:*Current-Event*)
			   #|
			   (:leftdown
			    ;; Leftdown, so activate the keyboard selection
			    ;; in the object just selected.
			    (do* ((x (inter:event-x inter:*Current-Event*))
				  (y (inter:event-y inter:*Current-Event*))
				  (gobs objects (cdr gobs))
				  (gob (car gobs) (car gobs)))
				 ((or (null gob)
				      (opal:point-in-gob gob x y))
				  (when (and gob
					     (not (g-value gob
						   :keyboard-selection-p)))
				    (position gob objects)))))
                           |#
			   (#\tab
			    ;; Tab, so increment selection rank
			    (do* ((rank (if (= prev-rank max-rank)
					    0 (+ 1 prev-rank))
					(if (= rank max-rank)
					    0 (+ 1 rank)))
				  (obj (nth rank objects) (nth rank objects)))
				 ((and (g-value obj :visible)
				       (g-value obj :active-p)) rank)))
			   (:CONTROL-TAB
			    ;; Control-tab, so decrement selection rank
			    (do* ((rank (if (> prev-rank 0)
					    (- prev-rank 1) max-rank)
					(if (> rank 0)
					    (- rank 1) max-rank))
				  (obj (nth rank objects) (nth rank objects)))
				 ((and (g-value obj :visible)
				       (g-value obj :active-p)) rank)))))
	       (prev-object (nth prev-rank objects))
	       new-object)
	  
	  (if (eq prev-rank new-rank)

	      ; There is only one tab-selectable object, so toggle its activity
	      (let ((new-p (not (g-value prev-object :keyboard-selection-p))))
		(setq new-object prev-object)
		(s-value prev-object :keyboard-selection-p new-p)
		; Check if it is a MOTIF-SCROLLING-LABELED-BOX
		(when (and (boundp 'MOTIF-SCROLLING-LABELED-BOX)
			   (is-a-p prev-object MOTIF-SCROLLING-LABELED-BOX))
		  (let ((prev-object-inter (g-value prev-object :field-text
						    :text-edit)))
		    (if new-p
			(inter:start-interactor prev-object-inter)
			(inter:stop-interactor prev-object-inter)))))

	      ; There are multiple selectable objects
	      (when new-rank
		(setq new-object (nth new-rank objects))
		(s-value prev-object :keyboard-selection-p NIL)
		(s-value interactor :rank new-rank)
		(s-value new-object :keyboard-selection-p T)

	        ;; Now take care of MOTIF-SCROLLING-LABELED-BOX objects
		(when (boundp 'MOTIF-SCROLLING-LABELED-BOX)
		  (if (is-a-p prev-object MOTIF-SCROLLING-LABELED-BOX)
		      (inter:stop-interactor (g-value prev-object
						      :field-text :text-edit)))
		  (if (is-a-p new-object MOTIF-SCROLLING-LABELED-BOX)
		      (inter:start-interactor (g-value new-object
						       :field-text :text-edit))))))

	        ;; Now execute the user's final-function
	  (kr-send interactor :final-function interactor new-object)
	  ))))

(setf (get :garnet-modules :motif-parts) t)

