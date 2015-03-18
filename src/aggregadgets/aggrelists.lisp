;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; AGGRELISTS. This subclass of aggregates allow the easy creation of
;;; list-type objects, such as menus. Aggrelists features automatic
;;; and customizable horizontal or vertical layout, generation of
;;; items according to a prototype, and can be used with aggregadgets
;;; in order to create complex objects.

;;; May, 1993:  A new version of Aggrelists by David S. Kosbie and
;;;             Andrew Mickish.
;;; The fundamental improvement in the new implementation is that it does not
;;; place ANY formulas into its components!  All the computation is performed
;;; inside of the object's :Fix-Update-Slots method.  This should be far more
;;; economical in both space and time.
;;;
;;; $Id::                                                             $


(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(aggrelist null-object)))


;;;--------------------------------------------------------------------------
;;; Compute the width and height of an aggrelist, without referencing the left
;;; or top of the aggrelist.  Note however that if the item objects are not of
;;; fixed width and height, their width and height will need to be calculated,
;;; which might (depending on their type) depend eventually on the left or top
;;; of the aggrelist.
;;;
;;; You may ask, "Why do we have a formula in the :width (or :height) slots
;;; when the appropriate value could be computed by the :fix-update-slots
;;; method?"  Here are two good reasons.  First of all, you might want to
;;; supply a value for the :width of your aggrelist, and have all the
;;; components depend on that value.  If the :fix-update-slots method set the
;;; slot, then your value would be overwritten.  Secondly, having a formula
;;; in the :width slot helps us to call the :fix-update-slots method whenever
;;; something changes that would require a relayout.  For example, if I change
;;; the width of a component, the formula in the aggrelist's :width slot will
;;; be invalidated, and since :width is an update-slot, the :fix-update-slots
;;; method will be invoked.
;;;--------------------------------------------------------------------------

(defun dimensions-fn ()
  (let ((currleft 0)
	(currtop 0)
	(width 0)
	(height 0)
	(h-spacing (gvl :h-spacing))
	(v-spacing (gvl :v-spacing))
	(indent (gvl :indent))
	(fixed-width (gvl :fixed-width))
	(fixed-height (gvl :fixed-height))
	(rank-margin (gvl :rank-margin))
	(pixel-margin (gvl :pixel-margin)))
    (case (gvl :direction)
      (:horizontal
       (let ((left-offset 0)
	     (line-count 0)
	     (first-pass? T)
	     comp-width
	     )
        (dolist (child (gvl :components))
	  (when (gv child :visible)
	    (setq line-count (1+ line-count))
	    (if first-pass?
		(setq first-pass? NIL)
		(setq left-offset (+ h-spacing left-offset
				     (or fixed-width comp-width))))
	    (setq comp-width  (g-value child :width))

	    (when (or (and rank-margin (> line-count rank-margin))
		      (and pixel-margin
			   (> (+ left-offset (or fixed-width comp-width))
			      pixel-margin)))
	      (setq left-offset (gvl :indent))
	      (setq currtop (+ height v-spacing))
	      (setq currleft indent)
	      (setq line-count 1))
	    (setq currleft (+ currleft
			      (or fixed-width (gv child :width))
			      h-spacing))
	    (setq width (max width (- currleft h-spacing)))
	    (setq height (max height (+ currtop (or fixed-height
						    (gv child :height)))))
	    ;; RGA leave this til last to force recomputation.
	    (if (gvl :right-justify-last)
		(setq width
		  (max width (gvl :justify-width))))
	    ))))
      (:vertical
       (let ((top-offset 0)
	     (line-count 0)
	     (first-pass? T)
	     comp-height)
	(dolist (child (gvl :components))
	  (when (gv child :visible)
	    (setq line-count (1+ line-count))
	    (if first-pass?
		(setq first-pass? NIL)
		(setq top-offset (+ v-spacing top-offset
				     (or fixed-height comp-height))))
	    (setq comp-height (g-value child :height))

	    (when (or (and rank-margin (> line-count rank-margin))
		      (and pixel-margin
			   (> (+ top-offset (or fixed-height comp-height))
			      pixel-margin)))
	      (setq top-offset 0)
	      (setq currtop indent)
	      (setq currleft (+ width h-spacing))
	      (setq line-count 1))
	    (setq currtop (+ currtop
			     (or fixed-height (gv child :height))
			     v-spacing))
	    (setq width (max width
			     (+ currleft (or fixed-width (gv child :width)))))
	    (setq height (max height (- currtop v-spacing)))
	    ;; RGA leave this til last to force recomputation.
	    (if (gvl :bottom-justify-last)
		(setq height
		  (max height (gvl :justify-height))))
	    ))))
      (t (let ((max-x -999999) (min-x 999999)
	       (max-y -999999) (min-y 999999))
	   (dolist (child (gvl :components))
	     (when (gv child :visible)
	       (setf max-x (max max-x (+ (or (gv child :left) 0)
					 (or (gv child :width) 0))))
	       (setf min-x (min min-x (or (gv child :left) min-x)))
	       (setf max-y (max max-y (+ (or (gv child :top) 0)
					 (or (gv child :height) 0))))
	       (setf min-y (min min-y (or (gv child :top) min-y)))))
	   (setf width (max 0 (- max-x min-x)))
	   (setf height (max 0 (- max-y min-y))))))
    (list width height)))



;;;--------------------------------------------------------------------------
;;;
;;;    Object definitions
;;;
;;;--------------------------------------------------------------------------

(create-instance 'null-object opal:view-object
  (:visible T))

(create-instance 'opal:aggrelist opal:aggregate
 :declare ((:type ((or (member :vertical :horizontal) null) :direction)
		 (integer :v-spacing :h-spacing :indent)
		 ((member :top :center :bottom) :v-align)
		 ((member :left :center :right) :h-align)
		 (kr-boolean :right-justify-last :bottom-justify-last)
		 ((or null (integer 0)) :fixed-width-size
					:fixed-height-size
					:justify-width
					:justify-height
		  :rank-margin :pixel-margin))
	   (:update-slots :max-width :max-height :left :top :direction
			  :h-spacing :v-spacing	:indent :h-align :v-align
			  :fixed-width-p :fixed-height-p
			  :fixed-width-size :fixed-height-size
			  :rank-margin :pixel-margin :items
	      ;; :width and :height should be in this list to cause the
	      ;; :fix-update-slots method to be invoked when the :width or
	      ;; :height of a component changes (the formulas in these slots
	      ;; depend on the widths and heights of the components).
			  :width :height
	      ;; :visible-components is responsible for relayout of the
	      ;; aggrelist when a component becomes invisible or is removed
			  :visible-components
	      ;; RGA --- These want to be here as well
			  :right-justify-last
			  :bottom-justify-last
			  :justify-width :justify-height
			  )
	   (:maybe-constant :left :top :width :height :direction :h-spacing
			    :v-spacing :indent :h-align :v-align :max-width
			    :max-height :fixed-width-p :fixed-height-p
			    :fixed-width-size :fixed-height-size
			    :rank-margin :pixel-margin :items :visible
			    :right-justify-last :bottom-justify-last
			    :justify-width :justify-height))


               ; User-settable Slots ;
 (:left 0)
 (:top 0)
 (:direction :vertical) ;; :horizontal or :vertical
 (:h-spacing 5)         ;;; Pixels between horizontal elements
 (:v-spacing 5)         ;;; Pixels between vertical elements
 (:indent 0)            ;;; How much to indent on wraparound
 (:h-align :left)       ;;; Can be :left, :center, or :right
 (:v-align :top)        ;;; Can be :top, :center, or :bottom
 (:fixed-width-p NIL)           ;;; Width Fields of fixed-size?
 (:fixed-height-p NIL)          ;;; Height Fields of fixed-size?
 (:fixed-width-size NIL)
 (:fixed-height-size NIL)
 (:rank-margin NIL)     ;;; If non-NIL, then after this many components, a
                        ;;; new row/column will be started for
                        ;;; horizontal/vertical lists
 (:pixel-margin NIL)    ;;; Same as rank-margin, but in pixels not objects
 (:right-justify-last NIL)  ;; Used for producing Help-Style in motif menubar
 (:bottom-justify-last NIL)
 ;; Width (or height) accros which list should span
 (:justify-width (o-formula (if (gvl :right-justify-last) (gvl :width))))
 (:justify-height (o-formula (if (gvl :bottom-justify-last) (gvl :height))))
                 ; Internal Slots ;

 (:force-computation? NIL)  ;; used by add-, move-, and remove-component

 (:max-width  (o-formula (let ((width 0))
                           (dolist (c (gvl :components))
                              (when (gv c :visible)
                                (setq width (max width (gv c :width)))))
                           width)))
 (:max-height (o-formula (let ((height 0))
                           (dolist (c (gvl :components))
                              (when (gv c :visible)
                                (setq height (max height (gv c :height)))))
                           height)))
 (:fixed-width (o-formula (if (gvl :fixed-width-p)
			      (or (gvl :fixed-width-size)
				  (gvl :max-width)))))
 (:fixed-height (o-formula (if (gvl :fixed-height-p)
			       (or (gvl :fixed-height-size)
				   (gvl :max-height)))))
 (:update-slots-values NIL)
 (:head nil)
 (:tail nil)

 (:dimensions (o-formula (dimensions-fn)))
 (:width (o-formula (first (gvl :dimensions))))    ;; See comments for dim-fn
 (:height (o-formula (second (gvl :dimensions))))  ;; See comments for dim-fn

 (:items NIL)           ;;; List of the items (when itemized)
 (:item-prototype NIL)  ;;; Specification of prototype of the items
                        ;;; (when itemized)
 (:item-prototype-object NIL) ;;; the actual object
 ;; This formula will become constant if all of the layout parameters
 ;; are constant.  This slot will be checked by the :fix-update-slots
 ;; method and, if it is constant, the :left and :top of each component
 ;; will be declared constant
 (:layout-fixed? (o-formula
		   (progn
		    (gvl :left) (gvl :top) (gvl :items) (gvl :direction) 
		    (gvl :v-spacing) (gvl :h-spacing) (gvl :indent)
		    (gvl :v-align) (gvl :h-align)
		    (gvl :fixed-width-p) (gvl :fixed-height-p)
		    (gvl :fixed-width-size) (gvl :fixed-height-size)
		    (gvl :rank-margin) (gvl :pixel-margin))))
 ;; :visible-components is responsible for relayout of the
 ;; aggrelist when a component becomes invisible or is removed
 (:visible-components (o-formula
		       (progn
			 (dolist (c (gvl :components))
			   (gv c :visible))
			 (s-value (gv :self) :force-computation? T))))
 )

(defconstant +lister-max-width+           0)
(defconstant +lister-max-height+          1)
(defconstant +lister-left+                2)
(defconstant +lister-top+                 3)
(defconstant +lister-direction+           4)
(defconstant +lister-h-spacing+           5)
(defconstant +lister-v-spacing+           6)
(defconstant +lister-indent+              7)
(defconstant +lister-h-align+             8)
(defconstant +lister-v-align+             9)
(defconstant +lister-fixed-width-p+      10)
(defconstant +lister-fixed-height-p+     11)
(defconstant +lister-fixed-width-size+   12)
(defconstant +lister-fixed-height-size+  13)
(defconstant +lister-rank-margin+        14)
(defconstant +lister-pixel-margin+       15)
(defconstant +lister-items+              16)
(defconstant +lister-height+             17)
(defconstant +lister-width+              18)
(defconstant +lister-visible-components+ 19)
(defconstant +lister-right-justify-last+ 20)
(defconstant +lister-bottom-justify-last+ 21)
(defconstant +lister-justify-width+ 22)
(defconstant +lister-justify-heigh+ 23)


;;;--------------------------------------------------------------------------
;;;
;;;    Methods for the aggrelist
;;;
;;;--------------------------------------------------------------------------

(s-value AGGRELIST :add-interactor
	 (g-value AGGREGADGET :add-interactor))
(s-value AGGRELIST :add-local-interactor
	 (g-value AGGREGADGET :add-local-interactor))
(s-value AGGRELIST :remove-interactor
	 (g-value AGGREGADGET :remove-interactor))
(s-value AGGRELIST :remove-local-interactor
	 (g-value AGGREGADGET :remove-local-interactor))

(s-value AGGRELIST :take-default-component
	 (g-value AGGREGADGET :take-default-component))

(define-method :add-component opal:aggrelist (my-agg-list &rest other-args)
  (apply (g-value opal:aggregadget :add-component) my-agg-list other-args)
  (s-value my-agg-list :force-computation? T)
  (funcall (g-value my-agg-list :invalidate-demon) my-agg-list NIL NIL))

(define-method :remove-component opal:aggrelist (my-agg-list &rest other-args)
  (apply (g-value opal:aggregadget :remove-component) my-agg-list other-args)
  (s-value my-agg-list :force-computation? T)
  (funcall (g-value my-agg-list :invalidate-demon) my-agg-list NIL NIL))

(define-method :move-component opal:aggrelist (my-agg-list &rest other-args)
  (apply (g-value opal:aggregadget :move-component) my-agg-list other-args)
  (s-value my-agg-list :force-computation? T)
  (funcall (g-value my-agg-list :invalidate-demon) my-agg-list NIL NIL))

(define-method :destroy-me opal:aggrelist (agg &optional (top-level-p t))
  (dolist (behavior (copy-list (g-local-value agg :behaviors)))
    (destroy behavior))
; BVZ says that item-prototype-objects might still be used even after the
; aggrelist is destroyed.
;  (let ((item-prototype-object (g-local-value agg :item-prototype-object)))
;    (when item-prototype-object
;      (destroy item-prototype-object)))
  (call-prototype-method agg top-level-p))


;;;----------------------------------------------------------------------
;;;
;;;   Initialization method for aggrelists
;;;
;;;----------------------------------------------------------------------

;;; ITEMIZED AGGRELISTS: When all the components of an aggrelist are of a
;;; same type, they can be automatically created according to a prototype
;;; (specified in the :item-prototype slot, the actual instance is in the
;;; :item-prototype-object slot) and a list of items (given in the
;;; :items slot). Add-Local-Item and Remove-Local-Item allow to modify
;;; the items of an itemized aggrelist after it has been created.


(defun Generate-Aggrelist-Components (agg item-prototype number-of-comps)
  ;; The :constant list of each component must be conditioned on
  ;; whether the :parent's :items slot is constant
  (if (kr::slot-constant-p agg :items)
      (let ((prev-component NIL))
	(dotimes (count number-of-comps)
	  (let* ((kr::*redefine-ok* T)
		 (new-component (kr::begin-create-instance NIL item-prototype
				  (:rank count)
				  (:prev prev-component))))
	    (with-constants-disabled
	      (cond (prev-component
		     (s-value agg :tail new-component)
		     (s-value prev-component :next new-component))
		    (t (s-value agg :tail
				(s-value agg :head new-component))))
	      ;; Skip the aggrelist add-component method since we've
	      ;; already done all the aggrelist bookkeeping.
	      (kr-send opal:aggregate :add-component agg new-component))
	    (declare-constant new-component :rank)
	    (declare-constant new-component :parent)
	    (declare-constant new-component :prev)
	    (kr::end-create-instance new-component)
	    (setf prev-component new-component)
	    ))
	(kr:declare-constant agg :head)
	(kr:declare-constant agg :tail)
	(kr:declare-constant agg :components))
      (let ((prev-component NIL))
	(dotimes (count number-of-comps)
	  (let ((new-component (kr::begin-create-instance NIL item-prototype
				 (:rank count)
				 (:prev prev-component))))
	    (add-local-component agg new-component)
	    (kr::end-create-instance new-component)
	    (setf prev-component new-component))))))

;;;
;;; Create-Items uses the value of :items if it's a number, or its
;;; cardinality if it's a list, to create that very number of instances
;;; of :item-prototype-object.  Each of these instances is added as a component
;;; of the aggrelist.
;;;
(defun create-items (agg)
  (let* ((item-prototype-description (g-local-value agg :item-prototype))
	 (item-prototype
	  (cond
	   ;; The :item-prototype slot is empty -- This aggrelist is an
	   ;; instance of a prototype aggrelist, so inherit the item-prototype
	   ((null item-prototype-description)
	    (g-value agg :item-prototype-object))
	   ;; The :item-prototype slot contains a description of an object --
	   ;; Create the item-prototype according to this description
	   ((listp item-prototype-description)
	    (let* ((car-of-desc (car item-prototype-description)))
	      (if (eq car-of-desc :modify)
		  ;; get prototype from agg's prototype
		  (call-create-instance (g-value agg :item-prototype-object)
					(cdr item-prototype-description)
					agg)
		  ;; the car is a prototype schema
		  (call-create-instance car-of-desc
					(cdr item-prototype-description)
	                                agg))))
	   ;; The :item-prototype slot contains a schema
	   (t (call-create-instance item-prototype-description NIL agg)))))

    ;; If there is an item-prototype, this is an itemized aggrelist
    (if item-prototype
	(let* ((source-value (g-value agg :items))
	       (number-of-comps (if (numberp source-value)
				    source-value
				    (length source-value))))
	  (when item-prototype-description
	    (s-value agg :item-prototype-object item-prototype)
	    (when (g-value agg :direction)
	      (s-value item-prototype :left 0)
	      (s-value item-prototype :top 0)))
	  (Generate-Aggrelist-Components agg item-prototype number-of-comps)))))


(define-method :initialize aggrelist (my-agg-list)
  (call-prototype-method my-agg-list)
  (s-value my-agg-list :update-slots-values
           (make-array (length (g-value my-agg-list :update-slots))))
  (let ((prototype (car (g-value my-agg-list :is-a)))
	(parts-list (g-local-value my-agg-list :parts))
	(inter-list (g-local-value my-agg-list :interactors)))
    (cond
      (parts-list
        ;; After making parts from the :parts list, go through each component
        ;; and mark the :prev slot constant (:prev was set during the
        ;; add-local-component of aggrelists).
	(make-parts my-agg-list parts-list prototype)
	(declare-constant my-agg-list :components)
	(declare-constant my-agg-list :head)
	(declare-constant my-agg-list :tail)
	(dolist (part (g-value my-agg-list :components))
	  (declare-constant part :parent)
	  (declare-constant part :prev)))
      (t (create-items my-agg-list)
	 (s-value my-agg-list :old-items (g-value my-agg-list :items))
	 ))
    (if (or (null inter-list)
	    (not (is-first-comp-in-parts-list
		  (g-value prototype :behaviors) inter-list)))
	(make-inters-from my-agg-list prototype))
    (make-interactors my-agg-list inter-list prototype))
;  (when (g-local-value my-agg-list :components)
;    (fix-update-slots my-agg-list))
  )

(define-method :do-items aggrelist (a-aggregate a-function 
						&key (type t))
   (block do-items-method
     (when (numberp (g-local-value a-aggregate :items))
       (warn "Can't iterate over the items of a non-itemized aggrelist.")
       (return-from do-items-method (values)))
     (let ((items (g-local-value a-aggregate :items)))
       (loop for child in items
	     ;; there may be "holes" in the virtual aggregate's
	     ;; :item-array [2003/09/16:rpg]
	     unless (null child)
	       when (or (eq type t)
			(is-a-p child type))
		 do (funcall a-function child)))))
