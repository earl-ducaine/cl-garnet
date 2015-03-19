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
;;;  Motif Trill Device
;;;
;;;  Basically, the motif-trill-device is just a motif-h-scroll-bar
;;;  with some feedback-text sitting in its center, as well as an
;;;  interactor for that text.  Unlike the GARNET trill-device, this
;;;  one has no page-trill.
;;;
;;;  Features and operation of the motif-trill-device:
;;;
;;;    1)  Click the left mouse button on the arrows to change
;;;        the value by :scroll-incr increments.
;;;    2)  Text inside the scrollbar changes to reflect the current value.
;;;    3)  Left click on the value to edit it.
;;;    4)  The top level :value slot is the current value, and can be set
;;;        directly.
;;;    
;;;  Customizable slots:
;;;
;;;    1) :left, :top, :value, :val-1, :val-2, :min-frame-width,
;;;                        :min-height -> as usual
;;;    2) :format-string - The format with which to convert the value to
;;;                        a string to be used in the feedback-text
;;;    3) :value-feedback-font - Font for the feedback-object
;;;    4) :value-feedback-p - Whether you want a value feedback
;;;    5) :scroll-incr - how much to increment the scrolling by
;;;    6) :selection-function - #'(lambda (gadget value)...)

;;;  Motif Trill Device Demo:
;;;    To run the demo, enter (GARNET-GADGETS:motif-trill-go)
;;;    To stop, use (GARNET-GADGETS:motif-trill-stop)
;;;
;;;  Designed & Implemented by Rajan Parthasarathy

;;;  CHANGE LOG:
;;;  08/12/93 Rajan Parthasarathy - Fixed bounding box, checked for
;;;             "" in (read-from-string ...)
;;;  07/15/93 Rajan Parthasarathy - Allowed NIL values for :val-1
;;;             and :val-2, made gadget grow and shrink if either
;;;             :val-1 or :val-2 is NIL.
;;;  05/26/93 Andrew Mickish - Added constant declarations
;;;  02/23/93 Andrew Mickish - Added type and parameter declarations
;;;  01/21/93 Rajan Parthasarathy - Created

(in-package "GARNET-GADGETS")
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Motif-Trill-Device))
  #+garnet-test
  (export '(Motif-Trill-Win Motif-Trill-Agg Motif-Trill-Obj
	    Motif-Trill-Go Motif-Trill-Stop)))
	  
(defun motif-trill-stop-action (inter obj stop-event)
  (declare (ignore inter stop-event))
  (s-value obj :cursor-index NIL)

  ;; mhsb stands for motif-h-scroll-bar 
  
  (let* ((gad (g-value obj :parent))
	 (mhsb (g-value gad :h-bar))
	 (a-string (g-value obj :string))
	 (value (unless (equal a-string "")
		  (read-from-string a-string)))
	 (val-1 (g-value mhsb :val-1))
	 (val-2 (g-value mhsb :val-2))
	 (num (when (numberp value) value)))

    (if num
	(progn
	  (cond
	    ;; if val-1 & val-2 are both non-nil
	    ;; check to see if it's between them,
	    ;; smaller than the min, or larger than
	    ;; the max
	    ((AND val-1 val-2)
	     (cond 
	       ((OR
		 (AND (>= val-1 val-2) (>= num val-2) (<= num val-1))
		 (AND (< val-1 val-2) (> num val-1) (< num val-2)))
		(s-value gad :value num))
	       ((<= num (min val-1 val-2))
		(s-value gad :value (min val-1 val-2)))
	       ((>= num (max val-2 val-1)
		   (s-value gad :value (max val-2 val-1))))))

	    ;; if val-1 is NIL, then we only check
	    ;; to see if it's bigger than val-2
	    ((NOT val-1)
	     (if (>= num val-2)
		 (s-value gad :value val-2)
		 (s-value gad :value num)))

	     ;; if means val-2 is NIL, we only check to see
	     ;; if the number is smaller than val-1

	     ((NOT val-2)
	      (if (<= num val-1)
		  (s-value gad :value val-1)
		  (s-value gad :value num)))

	     ;; if you got here, that means both val-1 &
	     ;; val-2 are NIL.  So no limits!  Just assign
	     ;; the value to be num.

	     (T
	      (s-value gad :value num)))
	       
	  (kr-send gad :selection-function gad (g-value gad :value)))
	(progn
	  (inter::beep)
	  (s-value obj :string (format NIL (g-value obj :format-string)
				       (g-value mhsb :value)))))
    (mark-as-changed mhsb :value)))

(defun motif-trill-h-bar-sel-fun (h-bar val)
  (s-value (g-value h-bar :parent) :value val)
  (kr-send (g-value h-bar :parent) :selection-function
	   (g-value h-bar :parent) val))
				  
(create-instance 'motif-trill-device motif-gadget-prototype
  :declare ((:parameters :left :top :width :height :value :val-1 :val-2
			 :foreground-color :format-string :value-feedback-font
			 :value-feedback-p :scroll-incr :selection-function
			 :visible)
	    (:type ((or number null) :val-1 :val-2)
		   (number :scroll-incr :value)
		   ((is-a-p opal:color) :foreground-color)
		   (string :format-string)
		   (kr-boolean :value-feedback-p)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file))
		    :value-feedback-font)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :width :height :val-1 :val-2
			 :foreground-color :format-string :value-feedback-font
			 :value-feedback-p :scroll-incr :visible))
  ;; Customizable slots

  (:value 20)
  (:left 0) (:top 0)
  (:val-1 0) (:val-2 100)
  (:min-frame-width 20)
  (:min-height 20)
  (:format-string "~a")
  (:value-feedback-font opal:default-font)
  (:value-feedback-p T)
  (:scroll-incr 1)
  (:selection-function NIL)
  (:foreground-color opal:motif-gray)
  
  ;; Non-customizable slots

  (:widest-value-width
   (o-formula (if (and (gvl :val-1) (gvl :val-2))
		  (max (opal:string-width (gvl :value-feedback-font)
					  (format NIL (gvl :format-string)
						  (gvl :val-1)))
		       (opal:string-width (gvl :value-feedback-font)
					  (format NIL (gvl :format-string)
						  (gvl :val-2))))
		  (max (gvl :min-frame-width)
		       (gvl :feedback-text :width)))))

  (:value-feedback-width (o-formula (if (gvl :value-feedback-p)
					(+ 5 (gvl :widest-value-width))
					0)))

  (:highest-value-height (o-formula (opal:string-height
				     (gvl :value-feedback-font) "X")))

  (:value-feedback-height
   (o-formula (if (gvl :value-feedback-p)
		  (max (gvl :min-height)
		       (+ 2 (gvl :highest-value-height)))
		  (gvl :min-height))))

  (:parts
   `((:h-bar ,motif-h-scroll-bar
      (:constant (:scr-trill-p :percent-visible))
      (:foreground-color ,(o-formula (gvl :parent :foreground-color)))
      (:selection-function ,#'motif-trill-h-bar-sel-fun)
      (:scr-incr ,(o-formula (gvl :parent :scroll-incr)))
      (:value ,(o-formula (gvl :parent :value)))
      (:val-1 ,(o-formula (if (gvl :parent :val-1)
			      (gvl :parent :val-1)
			      most-negative-double-float)))
      (:val-2 ,(o-formula (if (gvl :parent :val-2)
			      (gvl :parent :val-2)
			      most-positive-double-float)))
      (:left ,(o-formula (gvl :parent :left)))
      (:top ,(o-formula (gvl :parent :top)))
      (:width ,(o-formula (+ 20
			     (* 2 (gvl :left-arrow :width))
			     (gvl :parent :value-feedback-width))))
      (:height ,(o-formula (+ 10
			      (gvl :parent :value-feedback-height))))
      (:page-incr 0) (:percent-visible 1.0))
     (:feedback-text ,opal:cursor-text
      (:the-box ,(o-formula (gvl :parent :h-bar :indicator)))
      (:visible ,(o-formula (gvl :parent :value-feedback-p)))
      (:font ,(o-formula (gvl :parent :value-feedback-font)))
      (:left ,(o-formula (opal:gv-center-x-is-center-of (gvl :the-box))))
      (:top ,(o-formula (opal:gv-center-y-is-center-of (gvl :the-box))))
      (:h-bar ,(o-formula (gvl :parent :h-bar)))
      (:format-string ,(o-formula (gvl :parent :format-string)))
      (:string ,(o-formula (format NIL
				   (gvl :format-string)
				   (gvl :h-bar :value)))))))
  (:interactors
   `((:feedback-inter ,inter:text-interactor
      (:text ,(o-formula (gvl :operates-on :feedback-text)))
      (:active ,(o-formula (gvl :text :visible)))
      (:window ,(o-formula (gvl :text :window)))
      (:start-where ,(o-formula (list :in-box (gvl :text))))
      (:start-event :leftdown)
      (:stop-event #\RETURN)
      (:feedback-obj ,(o-formula (gvl :text)))
      (:stop-action ,#'MOTIF-TRILL-STOP-ACTION))
     )))


;; slide is high priority and eats up feedback-inter's events.
;; since we don't need it, we just disable it here.

(s-value (g-value motif-trill-device :h-bar :slide) :active NIL)
(s-value (g-value motif-trill-device :h-bar :jump) :active NIL)

#+garnet-test (defparameter Motif-Trill-Win NIL)
#+garnet-test (defparameter Motif-Trill-Agg NIL)
#+garnet-test (defparameter Motif-Trill-Obj NIL)

#+garnet-test
(defun motif-trill-go (&optional constant)
  (create-instance 'motif-trill-win inter:interactor-window
    (:left 700) (:top 20) (:width 200) (:height 125)
    (:aggregate (create-instance 'motif-trill-agg opal:aggregate))
    (:background-color opal:motif-gray))

  (create-instance 'motif-trill-obj motif-trill-device
    (:constant constant)
    (:left 50) (:top 50)
    (:value-feedback-font (create-instance NIL opal:font
			    (:size :large))))
  
  (opal:add-component motif-trill-agg motif-trill-obj)
  (opal:update motif-trill-win)
  )

#+garnet-test
(defun motif-trill-stop ()
  (opal:destroy motif-trill-win))

  
