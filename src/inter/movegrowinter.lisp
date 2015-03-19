;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; $Id$
;;;
;;; This file contains the mouse and keyboard interactors to select objects
;;; and move them around or grow them.  It should be loaded after
;;; Interactors.lisp
;;;
;;; Designed and implemented by Brad A. Myers


(in-package "INTERACTORS")


;;;;===========================================================
;;; Filtering
;;;============================================================

(defun Get-Filtered-Input (inter event)
  (let ((filter (g-value inter :input-filter))
	(x (event-x event))
	(y (event-y event)))
    (when filter
      (if-debug inter (format T "Filtering input, old ~s,~s~%" x y)))
    (cond ((null filter)(values x y))
	  ((numberp filter) (values (* filter (round x filter))
				    (* filter (round y filter))))
	  ((listp filter)
	   (let ((xmod (first filter))
		 (xorigin (second filter))
		 (ymod (third filter))
		 (yorigin (fourth filter)))
	     (values (+ xorigin (* xmod (round (- x xorigin) xmod)))
		     (+ yorigin (* ymod (round (- y yorigin) ymod))))))
	  ((garnet-utils:safe-functionp filter) (funcall filter inter x y))
	  (t (error ":input-filter of ~s should be NIL, number, list of 4 numbers or functionp" inter)))))
		   


;;;;===========================================================
;;; Clip-and-Map
;;;============================================================

;;; The Clip-and-Map procedure works as follows:
;;;    (Clip-and-Map (val val-1 val-2 target-val-1 target-val-2) takes val,
;;;    clips it to be in the range val-1 .. val-2, and if target-val-1 and
;;;    target-val-2 are provided, then scales and
;;;    translates the value (using linear-interpolation) to be between
;;;    target-val-1 and target-val-2.  Unless target-val-1 and target-val-2
;;;    are both integers, the mapping will be to a float.
;;; Val-1 is allowed to be less than or greater than Val-2.
;;;
(defun Clip-and-Map (val val-1 val-2 &optional target-val-1 target-val-2)
  (if (and target-val-1 target-val-2)
      ;; then do clip and map
      (if (eq val-1 val-2)
	  (cond ((< val val-1) target-val-1)
		(t target-val-2))
	  (cond ((< val val-1 val-2) target-val-1)
		((< val-1 val-2 val) target-val-2)
		((< val val-2 val-1) target-val-2)
		((< val-2 val-1 val) target-val-1)
		(t (+ target-val-1
		      (if (and (integerp target-val-1) (integerp target-val-2))
			  ; integer targets
			  (round (* (- val val-1)
				    (- target-val-2 target-val-1))
				 (- val-2 val-1))
		          ; float targets
			  (/ (* (- val val-1) (- target-val-2 target-val-1))
			     (- val-2 val-1)))))))

      ;; else, just do clip (no map)
      (cond ((< val val-1 val-2) val-1)
	    ((< val-1 val-2 val) val-2)
	    ((< val val-2 val-1) val-2)
	    ((< val-2 val-1 val) val-1)
	    ; now make sure that return value is integer if val-1 and val-2
	    ; are both integers (this comes in real handy sometimes)
	    (t (if (and (integerp val-1) (integerp val-2))
		   (round val) val)))))



;;;;===========================================================
;;; Move-Grow-Interactor
;;;============================================================

;;;============================================================
;;; Helper procedures for the default procedures to go into the slots
;;;============================================================

			     ;left top width height
(defparameter *glo-points* (list 0 0 0 0))  ; use this to avoid cons-ing


;;; Calculates an object's position.
(defun CalcPosition (an-interactor obj x y)
  (let ((attach (g-value an-interactor :attach-point)))
    (if-debug an-interactor (format T "   CalcPosition attach=~s, x,y=(~s,~s)~%"
				    attach x y))
        ;; RGA --- added call to get-obj-slots-for-movegrow
    (multiple-value-bind (left top width height)
	(get-obj-slots-for-movegrow obj nil an-interactor)
      (declare (ignore top left))
        ;; use a global to avoid cons-ing
      (setf (first *glo-points*)
	(case attach
	  ((:nw :sw :w) x)
	  ((:ne :se :e) (1+ (- x width)))
	  ((:n :s) (- x (floor width 2)))
	  (:center (- x (floor width 2))) ; use integer divide
	  (:where-hit (- x (g-value an-interactor :x-off)))
	  (t (error "bad attach ~s on interactor ~s" attach an-interactor))))
      (setf (second *glo-points*)
	(case attach
	  ((:nw :ne :n) y)
	  ((:sw :se :s) (1+ (- y height)))
	  ((:e :w) (- y (floor height 2)))
	  (:center (- y (floor height 2)))
	  (:where-hit (- y (g-value an-interactor :y-off)))))
      (setf (third *glo-points*) width)
      (setf (fourth *glo-points*) height)
      *glo-points*)))

;; Deals with changing an objects size, not position, returns a new
;; left top w h.  x and y in are the new mouse point
(defun CalcSizeAndPosition (an-interactor obj x y)
  (let* ((attach (g-value an-interactor :attach-point))
	 (minwidth (g-value an-interactor :Min-width))
	 (minheight (g-value an-interactor :Min-height))
	 #|| ; RGA replaced with call to get-obj-slots-for-movegrow
	 (left (g-value obj :left))
	 (top (g-value obj :top))
	 (width (g-value obj :width))
	 (height (g-value obj :height))
	 ||#
	 rightp1 bottomp1)
    (if-debug an-interactor
	      (format T "   CalcSizeAndPosition attach=~s, obj=~s, x,y=(~s,~s)~%"
		      attach obj x y))
    ;; RGA --- added call to get-obj-slots-for-movegrow
    (multiple-value-bind (left top width height)
	(get-obj-slots-for-movegrow obj nil an-interactor)

      (when minwidth (setq rightp1 (+ 1 left width)))
      (when minheight (setq bottomp1 (+ 1 top height)))
      (when (eq attach :where-hit)
	(setq attach (g-value an-interactor :where-hit-attach))
	(setq x (+ x (g-value an-interactor :x-off))) ;these are + or - as needed
	(setq y (+ y (g-value an-interactor :y-off)))) ;set by CalcWhereHitAttach
      ;; use a global to avoid cons-ing
      ;; first do left and width
      (case attach
	((:nw :sw :w) 
	 (when (and minwidth
		    (< (- rightp1 x) minwidth))
	   (setq x (- rightp1 minwidth)))
	 (setf (first *glo-points*) x)
	 (setf (third *glo-points*) (+ width (- left x))))
	((:s :n)			; no changes for these
	 (setf (first *glo-points*) left)
	 (setf (third *glo-points*) width))
	((:ne :se :e)
	 (setf (first *glo-points*) left)
	 (setf (third *glo-points*)
	   (if (and minwidth
		    (< (- x left) minwidth))
	       minwidth			; use minwidth if too small
	     (1+ (- x left)))))		; otherwise, get new width
	(t (error "bad attach ~s on interactor ~s" attach an-interactor)))

      ;; now do top and height
      (case attach
	((:nw :ne :n)
	 (when (and minheight
		    (< (- bottomp1 y) minheight))
	   (setq y (- bottomp1 minheight)))
	 (setf (second *glo-points*) y)
	 (setf (fourth *glo-points*) (+ height (- top y))))
	((:e :w)			; no changes for these
	 (setf (second *glo-points*) top)
	 (setf (fourth *glo-points*) height))
	((:se :sw :s)
	 (setf (second *glo-points*) top)
	 (setf (fourth *glo-points*)
	   (if (and minheight
		    (< (- y top) minheight))
	       minheight		; use minheight if too small
	     (1+ (- y top)))))		; otherwise, get new height
	(t (error "bad attach ~s on interactor ~s" attach an-interactor)))
      *glo-points*)))

(defconstant sqrt2 (sqrt 2))

;; Deals with changing a line object's end point.  The point to change is
;; determined by the value of :attach-point, which should be 1, 2 or :where-hit
;; returns a new points lines, for both sets of end points: (x1 y1 x2 y2)
;; but only one of the points will have changed.
(defun CalcLineEndPoint (an-interactor x y)
  (let ((attach (g-value an-interactor :attach-point))
	(minlength (g-value an-interactor :Min-length))
	(origpoints (g-value an-interactor :saved-original-points)))
    ;; RGA--Note don't need get-obj-slots-... call here, uses
    ;; :saved-original-points slot, fixed elsewhere.
    (if-debug an-interactor
	      (format T "   CalcLineEndPoint attach=~s, x,y=(~s,~s)~%"
		      attach x y))
    (when (eq attach :where-hit)
      (setq attach (g-value an-interactor :where-hit-attach))) ;set by
    						       ;CalcLineWhereHitAttach
    (if minlength
	; time for expensive math
	(let (firstx firsty movingx movingy xdist ydist denom)
	  (case attach
	    (1 (setf firstx (third origpoints))
	       (setf firsty (fourth origpoints)))
	    (2 (setf firstx (first origpoints))
	       (setf firsty (second origpoints)))
	    (t (error "bad attach for line ~s, should be 1, 2, or :where-hit" attach)))
	  (setf xdist (- x firstx))
	  (setf ydist (- y firsty))
	  (setf denom (sqrt (+ (* xdist xdist)(* ydist ydist))))
	  (if (< denom minlength)
	      (progn
		(if (zerop denom) ; don't devide by zero
		    (progn
		      (setf movingx (+ x (ceiling minlength sqrt2)))
		      (setf movingy (+ y (ceiling minlength sqrt2))))
		    ; not zero, use calculated points
		    (progn
		      (setf movingx (+ firstx (ceiling (* xdist minlength) denom)))
		      (setf movingy (+ firsty (ceiling (* ydist minlength) denom))))))
	      ; else not less than minimum length
	      (progn
		(setf movingx x)
		(setf movingy y)))
	  ; now set point-list
	  (case attach
	    (1 (setf (first *glo-points*) movingx)
	       (setf (second *glo-points*) movingy)
	       (setf (third *glo-points*) firstx)
	       (setf (fourth *glo-points*) firsty))

	    (2 (setf (first *glo-points*) firstx)
	       (setf (second *glo-points*) firsty)
	       (setf (third *glo-points*) movingx)
	       (setf (fourth *glo-points*) movingy))))

	; else don't worry about minimum length because no minimum length
	(case attach
	  (1 (setf (first *glo-points*) x)
	     (setf (second *glo-points*) y)
	     (setf (third *glo-points*) (third origpoints))
	     (setf (fourth *glo-points*) (fourth origpoints)))
	  
	  (2 (setf (first *glo-points*) (first origpoints))
	     (setf (second *glo-points*) (second origpoints))
	     (setf (third *glo-points*) x)
	     (setf (fourth *glo-points*) y))
	  (t (error "bad attach for line ~s, should be 1, 2, or :where-hit" attach))))
    *glo-points*))



;;; Calculates an line's position as it is moved without changing length or slope
(defun CalcLineMove (an-interactor x y)
  (let ((attach (g-value an-interactor :attach-point))
	(origxdist (g-value an-interactor :orig-x-dist))
	(origydist (g-value an-interactor :orig-y-dist))
	xoff yoff)
    ;; RGA don't need call to get-obj-slots-... here, uses x and y
    ;; from event.
    (if-debug an-interactor (format T "   CalcLineMove attach=~s, x,y=(~s,~s)~%"
				    attach x y))
    (when (eq attach :where-hit)
      (setq xoff (g-value an-interactor :x-off))  ;these are + or - as needed
      (setq yoff (g-value an-interactor :y-off)))

;; use a global to avoid cons-ing
    (setf (first *glo-points*)
	  (case attach
	    (1 x)
	    (2 (- x origxdist))
	    (:center (- x origxdist))
	    (:where-hit (- x xoff))
	    (t (error "bad attach ~s on interactor ~s" attach an-interactor))))
    (setf (second *glo-points*)
	  (case attach
	    (1 y)
	    (2 (- y origydist))
	    (:center (- y origydist))
	    (:where-hit (- y yoff))))
    (setf (third *glo-points*)
	  (case attach
	    (1 (+ x origxdist))
	    (2 x)
	    (:center (+ x origxdist))
	    (:where-hit (+ (- x xoff) origxdist))))
    (setf (fourth *glo-points*)
	  (case attach
	    (1 (+ y origydist))
	    (2 y)
	    (:center (+ y origydist))
	    (:where-hit (+ (- y yoff) origydist))))
    *glo-points*))
    


;;; ----------------------------------------------------------------------
;;; functions to deal with :where-hit and initialize the interactor
;;; ----------------------------------------------------------------------
;;; Orig-?-dist is the distance from x2 to x1, unless centered in which case it
;;; is half the distance.
(defun SetLineInitialSlots (an-interactor obj x y)
  ;; RGA --- added call to get-obj-slots-for-movegrow
  (multiple-value-bind (x1 y1 x2 y2)
      (get-obj-slots-for-movegrow obj t an-interactor)
    (let ((dx (- x2 x1))
	  (dy (- y2 y1))
	  (attach (g-value an-interactor :attach-point)))
      (s-value an-interactor :orig-x-dist (if (eq attach :center)
					      (floor dx 2)
					    dx))
      (s-value an-interactor :orig-y-dist (if (eq attach :center)
					      (floor dy 2)
					    dy))
      (when (eq :where-hit attach)
        ;; then also set up where to grow from or offsets
	(if (g-value an-interactor :grow-p)
	    (CalcLineWhereHitAttach an-interactor x y) ; for growing lines
	  (progn
	    (s-value an-interactor :x-off (- x x1))
	    (s-value an-interactor :y-off (- y y1))))))))

;;; Call this when press and attach-point is :where-hit and moving an
;;; end-point of a line to set the
;;; interactor's :where-hit-attach slot based on hit position.
;;; Returns :where-hit-attach
(defun CalcLineWhereHitAttach (an-interactor x y)
  (let* ((origpoints (g-value an-interactor :saved-original-points))
	 (x1 (first origpoints))
	 (y1 (second origpoints))
	 (x2 (third origpoints))
	 (y2 (fourth origpoints))
	 d1 d2 attach)
    ;; RGA don't need call to get-obj-slots-... here, uses x and y
    ;; from event.
    (unless (and x1 y1 x2 y2)
      (error
       "Move-Grow a line (:line-p is T), but object has no X1,Y1,X2,Y2"))
    (setq d1 (+ (* (- x1 x)(- x1 x)) (* (- y1 y)(- y1 y))))
    (setq d2 (+ (* (- x2 x)(- x2 x)) (* (- y2 y)(- y2 y))))
    (setq attach (if (< d1 d2) 1 2))
    (s-value an-interactor :where-hit-attach attach)
    (if-debug an-interactor
	      (format T "Calculated attach point for line is endpoint ~s~%" attach))
    attach))

;;; Call this when press and attach-point is :where-hit and growing a
;;; rectangle to set the
;;; interactor's :where-hit-attach slot based on hit position.  Also sets
;;; x-off and y-off.  Returns :where-hit-attach
(defun CalcWhereHitAttach (an-interactor x y)
  (let* ((origbox (g-value an-interactor :saved-original-points))
	 (x-off (- (first origbox) x))  ; should be negative numbers
	 (y-off (- (second origbox) y))   ; if point is inside the box
	 (w3 (floor (third origbox) 3))
	 (h3 (floor (fourth origbox) 3))
	 (xcontrol (cond ((< x (+ (first origbox) w3))
			  (s-value an-interactor :x-off x-off)
			  :w)
			 ((> x (+ (first origbox) w3 w3))
			  (s-value an-interactor :x-off
				   (+ -1 (third origbox) x-off))
			  :e)
			 (T (s-value an-interactor :x-off 0)
			    :c)))
	 (control (cond ((< y (+ (second origbox) h3))
			 (s-value an-interactor :y-off y-off)
			 (case xcontrol 
			   (:w :nw)
			   (:e :ne)
			   (:c :n)))
			((> y (+ (second origbox) h3 h3)) 
			 (s-value an-interactor :y-off
				   (+ -1 (fourth origbox) y-off))
			 (case xcontrol 
			   (:w :sw)
			   (:e :se)
			   (:c :s)))
			(T
			 (s-value an-interactor :y-off 0)
			 (case xcontrol 
			      (:w :w)
			      (:e :e)
			      (:c     ;; *Hack* for center, use :nw
			       (s-value an-interactor :x-off x-off)
			       (s-value an-interactor :y-off y-off)
			       :nw))))))
    ;; RGA don't need call to get-obj-slots-... here, uses x and y
    ;; from event.
    (if-debug an-interactor
	      (format T "Calculated attach point is  ~s~%" control))
    (s-value an-interactor :where-hit-attach control)
    control))

;;; makes the feedback for interactor be visible if vis = T or invisible if
;;; vis = NIL
(defun sel-change-feedback-visible (an-interactor feedback
						  object-being-changed vis)
  #-garnet-debug (declare (ignore an-interactor))
  (when feedback
    (let ((val (if vis object-being-changed NIL)))
      (dbprint-feed :obj-over feedback val an-interactor)
      (s-value feedback :obj-over val))))
  
;; old-list4 and new-list4 should both be lists of length four.  Copies the values
;; from the old one into the new one without consing.  Useful for box slots
;; and x1 x2 slots
(defun Copy-List4 (old-list4 new-list4)
  (setf (first old-list4) (first new-list4))
  (setf (second old-list4) (second new-list4))
  (setf (third old-list4) (third new-list4))
  (setf (fourth old-list4) (fourth new-list4)))

;; Copies the 4 values into an existing list if there, otherwise creates one
(defun set-obj-list4-slot (obj slot new-list4 inter feedbackp)
  #-garnet-debug (declare (ignore inter feedbackp))
  (dbprint-either slot obj new-list4 inter feedbackp)
  (set-obj-list4-slot-no-db obj slot new-list4))

(defun set-obj-list4-slot-no-db (obj slot new-list4)
  (when obj
    (let ((oldval (get-local-value obj slot)))
      (if (and oldval (listp oldval) (eq 4 (length oldval)))
	  ; then re-use old slots so no cons-ing
	  (progn (Copy-List4 oldval new-list4)
	    (Mark-As-Changed obj slot)) ; do this to get constraints to go
	  ; else create a new one
	  (s-value obj slot (copy-list new-list4))))))

;; Top level call to set the appropriate object slots
(defun obj-or-feedback-change (feedback object-being-changed new-points line-p
					inter)
  (if feedback
      (set-obj-slots-for-movegrow feedback line-p new-points inter T)
      (set-obj-slots-for-movegrow object-being-changed
				  line-p new-points inter NIL)))

(defun set-obj-slots-for-movegrow (obj line-p new-list4 inter feedbackp)
  (when obj 
    (let ((slots-to-set (g-value inter :slots-to-set)))
      (cond ((eq slots-to-set :box)
	     (set-obj-list4-slot obj (if line-p :points :box) new-list4
				 inter NIL))
	    ((eq slots-to-set :points)
	     (set-obj-list4-slot obj (if line-p :points :box) new-list4
				 inter NIL))
	    ((null slots-to-set) (error "slots-to-set nil in ~s" inter))
	    ((listp slots-to-set)
	     (Slot-set obj (first slots-to-set) (if line-p :x1 :left)
		       (first new-list4) inter feedbackp)
	     (Slot-set obj (second slots-to-set) (if line-p :y1 :top)
		       (second new-list4) inter feedbackp)
	     (Slot-set obj (third slots-to-set) (if line-p :x2 :width)
		       (third new-list4) inter feedbackp)
	     (Slot-set obj (fourth slots-to-set) (if line-p :y2 :height)
		       (fourth new-list4) inter feedbackp))
	    (t (error "bad :slots-to-set in ~s.  Should be :box :points or list of slots" inter))))))
	     
(defun slot-set (obj slot default-slot new-val inter feedbackp)
  #-garnet-debug (declare (ignore feedbackp))
  (cond ((null slot)) ; don't set
	((eq slot T)
	 (dbprint-either default-slot obj new-val inter feedbackp)
	 (s-value obj default-slot new-val))
	((keywordp slot)
	 (dbprint-either slot obj new-val inter feedbackp)
	 (s-value obj slot new-val))
	(t (error "slots-to-set value bad ~s in inter ~s" slot inter))))

;;; RGA Added Get versions to retriving the values; returns four values.

(defun get-obj-slots-for-movegrow (obj line-p inter)
  (when obj 
    (let ((slots-to-set (g-value inter :slots-to-set))
;;	  (old-points (kr:g-value obj (if line-p :points :box)))
	  )
      (cond ((member slots-to-set '(:box :points))
	     ;; RGA using old-points gives some odd effects
	     ;; (if old-points (values-list old-points))
	     (if line-p
		 (values (kr:g-value obj :x1)
			 (kr:g-value obj :y1)
			 (kr:g-value obj :x2)
			 (kr:g-value obj :y2))
	       (values (kr:g-value obj :left)
		       (kr:g-value obj :top)
		       (kr:g-value obj :width)
		       (kr:g-value obj :height))))
	    ((null slots-to-set) (error "slots-to-set nil in ~s" inter))
	    ((listp slots-to-set)
	     (values 
	      (Slot-get obj (first slots-to-set) (if line-p :x1 :left)
			inter)
	      (Slot-get obj (second slots-to-set) (if line-p :y1 :top)
			inter)
	      (Slot-get obj (third slots-to-set) (if line-p :x2 :width)
			inter)
	      (Slot-get obj (fourth slots-to-set) (if line-p :y2 :height)
			inter)))
	    (t (error "bad :slots-to-get in ~s.  Should be :box, :points or list of slots"
		      inter))))))

(defun slot-get (obj slot default-slot inter)
  (cond ((null slot) (g-value obj default-slot))
	((eq slot T) (g-value obj default-slot))
	((keywordp slot) (g-value obj slot))
	(t (error "slots-to-get value bad ~s in inter ~s" slot inter))))



;;;;===========================================================
;;; Default Procedures to go into the slots
;;;============================================================

(declaim (special Move-Grow-Interactor))

(defun Move-Grow-Interactor-Initialize (new-Move-Grow-schema)
  (if-debug new-Move-Grow-schema (format T "Select change initialize ~s~%"
					 new-Move-Grow-schema))
  (Check-Interactor-Type new-Move-Grow-schema inter:Move-Grow-Interactor)
  (Check-Required-Slots new-Move-Grow-schema)
  (Set-Up-Defaults new-Move-Grow-schema)
  )

(defun Move-Grow-Int-Start-Action (an-interactor object-being-changed
						  first-points)
  (if-debug an-interactor (format T "Move-Grow int-start moving ~s firstpoints=~s~%"
				  object-being-changed first-points))
  ;;change feedback or object first so no flicker when turned visible
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (obj-or-feedback-change feedback object-being-changed
			    first-points
			    (g-value an-interactor :line-p)
			    an-interactor)
    (when feedback (sel-change-feedback-visible 
		    an-interactor feedback object-being-changed T)
      )))

(defun Move-Grow-Int-Running-Action (an-interactor object-being-changed
						    new-points)
  (if-debug an-interactor
	    (format T "Move-Grow int-running, obj = ~s, points=~s~%"
				  object-being-changed new-points))
  (obj-or-feedback-change (g-value an-interactor :feedback-obj)
			object-being-changed new-points
			(g-value an-interactor :line-p)
			an-interactor))

(defun Move-Grow-Int-Outside-Action (an-interactor outside-control
						  object-being-changed) 
  (if-debug an-interactor (format T "Move-Grow int-outside, mov = ~s~%"
				  object-being-changed))
  (unless (eq :last outside-control)
    (let ((feedback (g-value an-interactor :feedback-obj)))
      (if feedback
	  (sel-change-feedback-visible
	   an-interactor feedback object-being-changed NIL)
	  (set-obj-slots-for-movegrow object-being-changed
				 (g-value an-interactor :line-p)
			         (g-value an-interactor :saved-original-points)
				 an-interactor
				 NIL)))))

(defun Move-Grow-Int-Back-Inside-Action (an-interactor outside-control
					       object-being-changed
					       new-inside-points) 
  (if-debug an-interactor 
	    (format T "Move-Grow int-back-in, obj = ~s, new points=~s~%"
		    object-being-changed new-inside-points))
  ;;first change the feedback or object to the new position, and then make it
  ;; visible, if necessary
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (obj-or-feedback-change feedback object-being-changed
			  new-inside-points
			  (g-value an-interactor :line-p)
			  an-interactor)
    (when (and feedback
	       (null outside-control))
      (sel-change-feedback-visible an-interactor feedback object-being-changed T))))

(defun Move-Grow-Int-Stop-Action (an-interactor object-being-changed
						 final-points)
  (if-debug an-interactor (format T "Move-Grow int-stop obj ~s final-points=~s~%"
				  object-being-changed final-points))
  ;;turn off feedback
  (sel-change-feedback-visible an-interactor (g-value an-interactor :feedback-obj)
			       object-being-changed NIL)

  ;;set object to final position
  (set-obj-slots-for-movegrow object-being-changed
			      (g-value an-interactor :line-p)
			      final-points an-interactor NIL)

  (KR-Send an-interactor :final-function an-interactor object-being-changed
	   final-points))

(defun Move-Grow-Int-Abort-Action (an-interactor object-being-changed)
  (if-debug an-interactor (format T "Move-Grow int-abort moving ~s~%"
				  object-being-changed))
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (if feedback
	(sel-change-feedback-visible an-interactor feedback
				     object-being-changed NIL)
	(set-obj-slots-for-movegrow object-being-changed
				 (g-value an-interactor :line-p)
			         (g-value an-interactor :saved-original-points)
				 an-interactor
				 NIL))))
  
;;;;===========================================================
;;; Go procedure utilities
;;;============================================================

;;; Want a non-standard default running-where so call this instead of calling
;;; Fix-Up-Running-where.    Default here is T (anywhere).
;;; probably it doesn't really make much sence to use '* with movegrow, but it
;;; is supported anyway
(defun Move-Grow-Fix-Running-where (an-interactor new-obj-over)
  (if (g-value an-interactor :running-where)
      ;; fix it up normally in case have '(:xxx *)
      (Fix-Running-Where an-interactor new-obj-over)
      ;; otherwise use T
      (s-value an-interactor :generated-running-where T)))

(defun CalcChangeBoxOrLine (an-interactor obj x y)
  (if (g-value an-interactor :line-p)
      (if (g-value an-interactor :grow-p)
	  (CalcLineEndPoint an-interactor x y)
	  (CalcLineMove an-interactor x y))
      (if (g-value an-interactor :grow-p)
	  (CalcSizeAndPosition an-interactor obj x y)
	  (CalcPosition an-interactor obj x y))))

;;; if continuous: (remove from start level, add to stop and abort
;;; 		    level, change state to running)
;;; save object over, call start procedure.
(defun Move-Grow-do-start (an-interactor new-obj-over event)
  (if-debug an-interactor (format T "Move-Grow starting over ~s~%" new-obj-over))
        ;; if obj-to-change supplied, then use that, otherwise use whatever was
	;; under the mouse when started
  (let ((obj (or (g-value an-interactor :obj-to-change) new-obj-over))
	points line-p)
    (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
      (if-debug an-interactor (format T "   Move-Grow moving ~s~%" obj))
      (s-value an-interactor :obj-being-changed obj)
      ;; don't check line-p until the previous slots have been set, in case
      ;; there are formulas
      (setq line-p (g-value an-interactor :line-p))
      (s-value an-interactor :saved-original-points
	;; RGA replaced with get-obj-slots-for-movegrow
	(multiple-value-list
	    (get-obj-slots-for-movegrow obj line-p an-interactor))
	#|| ;; RGA -- was
	(if line-p
	    (list (g-value obj :x1) (g-value obj :y1)
		  (g-value obj :x2) (g-value obj :y2))
	  (list (g-value obj :left) (g-value obj :top)
		(g-value obj :width) (g-value obj :height)))
	||#
	)
      (if (and obj (not (eq obj T)))
	(progn
	  (if (g-value an-interactor :line-p)
	      (SetLineInitialSlots an-interactor obj x y)
	      ;; otherwise, left,top,width,height
	      (when (eq :where-hit (g-value an-interactor :attach-point))
		(if (g-value an-interactor :grow-p)
		    (CalcWhereHitAttach an-interactor x y) ; for growing
		    (progn 		               ; for moving
		      (s-value an-interactor :x-off (- x (g-value obj :left)))
		      (s-value an-interactor :y-off (- y (g-value obj :top)))))))
	  (setf points (CalcChangeBoxOrLine an-interactor obj x y)))
	;else no object, just return x y
	(setf points (list x y 10 10)))  ; what use here for w h?
      (if (g-value an-interactor :continuous) ;then will go to running state
	(progn
	  (Move-Grow-Fix-Running-where an-interactor new-obj-over)
	  (when (g-value an-interactor :outside) ;needed if stop while outside
	    (set-obj-list4-slot-no-db an-interactor :saved-last-points points))
	  (GoToRunningState an-interactor T)
	  (kr-send an-interactor :start-action an-interactor obj points))
	;; else call stop-action
	(progn
	  (GoToStartState an-interactor NIL)
	  (kr-send an-interactor :stop-action an-interactor obj points))))))

#| ----------------------------------------------------------------------
---- Pedro's proposal for :where-hit when filter doesn't work because
---- object jumps to off-grid even if it was on grid to start with
(defun Move-Grow-do-start (an-interactor new-obj-over event)
  (if-debug an-interactor (format T "Move-Grow starting over ~s~%" new-obj-over))
        ;; if obj-to-change supplied, then use that, otherwise use whatever was
	;; under the mouse when started
  (let ((obj (or (g-value an-interactor :obj-to-change) new-obj-over))
	(eventx (event-x event))
	(eventy (event-y event))
	points line-p)
    (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
      (if-debug an-interactor (format T "   Move-Grow moving ~s~%" obj))
      (s-value an-interactor :obj-being-changed obj)
      ;; don't check line-p until the previous slots have been set, in case
      ;; there are formulas
      (setq line-p (g-value an-interactor :line-p))
      (s-value an-interactor :saved-original-points
	;; RGA replaced with get-obj-slots-for-movegrow
	(multiple-value-list
	    (get-obj-slots-for-movegrow obj line-p an-interactor))
	;; RGA -- was
	;;(if line-p
	;;   (list (g-value obj :x1) (g-value obj :y1)
	;;         (g-value obj :x2) (g-value obj :y2))
	;;  (list (g-value obj :left) (g-value obj :top)
	;;        (g-value obj :width) (g-value obj :height)))
	)
      (if (and obj (not (eq obj T)))
	(progn
	  ;; use the orig, unfiltered x,y to compute for :where-hit -Szekely
	  (if (g-value an-interactor :line-p)
	      (SetLineInitialSlots an-interactor obj eventx eventy)
	      ;; otherwise, left,top,width,height
	      (when (eq :where-hit (g-value an-interactor :attach-point))
		(if (g-value an-interactor :grow-p)
		    (CalcWhereHitAttach an-interactor  ; for growing
					eventx eventy)
		    (progn 		               ; for moving
		      (s-value an-interactor :x-off
			       (- eventx (g-value obj :left)))
		      (s-value an-interactor :y-off
			       (- eventy (g-value obj :top)))))))
	  (setf points (CalcChangeBoxOrLine an-interactor obj x y)))
	;else no object, just return x y
	(setf points (list x y 10 10)))  ; what use here for w h?
      (if (g-value an-interactor :continuous) ;then will go to running state
	(progn
	  (Move-Grow-Fix-Running-where an-interactor new-obj-over)
	  (when (g-value an-interactor :outside) ;needed if stop while outside
	    (set-obj-list4-slot-no-db an-interactor :saved-last-points points))
	  (GoToRunningState an-interactor T)
	  (kr-send an-interactor :start-action an-interactor obj points))
	;; else call stop-action
	(progn
	  (GoToStartState an-interactor NIL)
	  (kr-send an-interactor :stop-action an-interactor obj points))))))
----------------------------------------------------------------------
|#

(defun Move-Grow-do-outside (an-interactor)
  (if-debug an-interactor (format T "Move-Grow outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :outside)
	       (g-value an-interactor :obj-being-changed)))
;;;filtering based on :last is handled by the :outside-action procedure
;;;  (unless (eq :last (g-value an-interactor :outside))
;;;    (s-value an-interactor :remembered-last-object NIL)))

(defun Move-Grow-do-back-inside (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (if-debug an-interactor (format T "Move-Grow back-inside over ~s at:~s~%"
				  obj event))
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    ;;  (s-value an-interactor :x x)
    ;;  (s-value an-interactor :y y)
    (s-value an-interactor :current-state :running)
    (let* ((moving-obj (g-value an-interactor :obj-being-changed))
	   (points (CalcChangeBoxOrLine an-interactor moving-obj x y)))
      (when (g-value an-interactor :outside) ;needed if stop while outside
	(set-obj-list4-slot-no-db an-interactor :saved-last-points points))
      (kr-send an-interactor :back-inside-action an-interactor
		   (g-value an-interactor :outside)
		   moving-obj points))))

(defun Move-Grow-do-running (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (if-debug an-interactor (format T "Move-Grow running over ~s at:~s~%" obj event))
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    ;;  (s-value an-interactor :x x)
    ;;  (s-value an-interactor :y y)
    (let* ((moving-obj (g-value an-interactor :obj-being-changed))
	   (points (CalcChangeBoxOrLine an-interactor moving-obj x y)))
      (when (g-value an-interactor :outside) ;needed if stop while outside
	(set-obj-list4-slot-no-db an-interactor :saved-last-points points))
      (kr-send an-interactor :running-action an-interactor
		   moving-obj points))))

;;; points is the final value calculated
(defun Move-Grow-do-stop-helper (an-interactor points)
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Stop-Action an-interactor
	       (g-value an-interactor :obj-being-changed) points))

(defun Move-Grow-do-stop (an-interactor obj event)
  #-garnet-debug (declare (ignore obj))
  (if-debug an-interactor (format T "Move-Grow stop over ~s at:~s~%"
				  obj event))
  (multiple-value-bind (x y) (Get-Filtered-Input an-interactor event)
    (s-value an-interactor :prev-x x) ; used in case explicit stop
    (s-value an-interactor :prev-y y)
    (Move-Grow-do-stop-helper an-interactor
			      (CalcChangeBoxOrLine
			       an-interactor (g-value an-interactor
						      :obj-being-changed) x y))))
(defun Move-Grow-Explicit-stop (an-interactor)
  (if-debug an-interactor (format T "Move-Grow explicit stop~%"))
  (let ((x (g-value an-interactor :prev-x))
	(y (g-value an-interactor :prev-y)))
    (Move-Grow-do-stop-helper an-interactor
			      (CalcChangeBoxOrLine
			       an-interactor (g-value an-interactor
						      :obj-being-changed) x y))))

(defun Move-Grow-do-abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format T "Move-Grow aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor
	       (g-value an-interactor :obj-being-changed)))

;;;check to see if need to stop or abort based on whether :outside = :last
(defun Move-Grow-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format T "Move-Grow stop outside~%"))
  (if (eq :last (g-value an-interactor :outside))
      (Move-Grow-do-stop-helper an-interactor
			   (g-value an-interactor :saved-last-points))
      (Move-Grow-do-abort an-interactor NIL event)))


;;;;===========================================================
;;; Move-Grow schema
;;;============================================================

(Create-Schema 'inter:Move-Grow-Interactor
		     (:is-a inter:interactor)
		     (:name :First-Move-Grow-interactor)
		     (:start-action 'Move-Grow-Int-Start-Action)
		     (:running-action 'Move-Grow-Int-Running-Action)
		     (:stop-action 'Move-Grow-Int-Stop-Action)
		     (:abort-action 'Move-Grow-Int-Abort-Action)
		     (:outside-action 'Move-Grow-Int-Outside-Action)
		     (:back-inside-action 'Move-Grow-Int-Back-Inside-Action)
		     (:obj-to-change NIL)  ;supplied by application program
		     (:Min-width 0); minimum allowed width and height
		     (:Min-height 0)
		     (:attach-point :where-hit) ; where attach to object
		     (:grow-p NIL) ; if T then grow, else move
		     (:line-p NIL) ; if T, then move an end of the line,
			        ; else move left,top,width,height of rectangle
		     (:x-off 0) ; needed for :where-hit.  Offset from where
		     (:y-off 0)    ;    hit to top left of object
		     (:slots-to-set :box)
		     (:input-filter NIL)
		     (:saved-original-points NIL) ; used for ABORT or outside
		     (:saved-last-points NIL) ; used if stop and outside and
						; outside control is :last
		     (:obj-being-changed NIL) ; saved object under the mouse
		     (:Go 'General-Go)  ; proc executed when events happen
		     (:Do-Start 'Move-Grow-Do-Start)     ; these are
		     (:Do-Running 'Move-Grow-Do-Running) ;   called by GO
		     (:Do-Stop 'Move-Grow-Do-Stop)       ;   to do
		     (:Do-Explicit-Stop 'Move-Grow-Explicit-Stop);for stop-interactor
		     (:Do-Abort 'Move-Grow-Do-Abort)     ;   the real work.
		     (:Do-Outside 'Move-Grow-Do-Outside) ;   They call the
		     (:Do-Back-Inside 'Move-Grow-Do-Back-Inside)  ; appropriate
		     (:Do-Outside-Stop 'Move-Grow-Do-Outside-Stop); -action
								     ; procedures
		     (:initialize 'Move-Grow-Interactor-Initialize))


