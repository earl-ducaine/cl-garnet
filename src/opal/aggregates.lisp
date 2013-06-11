;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Changes:
;;; 07/06/93 Amickish - Adjusted scoping of gob-update-info in :add-component
;;;                     method to avoid UPDATE-INFO type error in CMUCL
;;; 03/10/93 Amickish - Now obj-in-rectangle, leaf-objects-in-rectangle, and
;;;                     components-in-rectangle only return visible objects
;;; 02/22/93 Koz/Amickish - Called do-all-components in :add-component method
;;;                     to invalidate all the children of the new component
;;; 12/31/92 Amickish - Called do-all-components in :remove-component method
;;;                     to remove components from the invalid objects list
;;; 04/29/92 Pervin   - Moved function to remove object from invalid objects
;;;			list to update-basics.lisp
;;; 04/28/92 Amickish - Wrapped destroy-slot call in with-constants-disabled
;;; 04/10/92 Amickish - Renamed FUNCTION variable in :do-components and
;;;                     :do-all-comonents methods to A-FUNCTION to avoid
;;;                     conflicts, added type checking when :do-components
;;;                     and :do-all-components call A-FUNCTION on SELF.
;;; 03/30/92 Amickish - Changed point-in-gob-method-view-object function calls
;;;                     to point-in-gob macro calls.
;;; 03/25/92 Amickish - Get-Values ---> G-Value;
;;;                     (setf (get-local-values ...)) ---> (s-value ...)
;;; 03/23/92 Pervin - In remove-component, make sure window exists before
;;;                   doing a get-local-value on it.
;;; 03/18/92 Amickish - Removed CMUCL compiler warnings: set-values--->s-value,
;;;                     bound a-window-update-info in :remove-component and
;;;                     :move-component methods.
;;; 03/16/92 Amickish - Adjusted scoping in add-component method
;;; 03/11/92 Pervin - Remove component, make sure window exists before
;;;		      doing a g-value on it.
;;; 03/05/92 Pervin - In remove-component, make sure last-inv-obj is
;;;                     still correct after ex-component is removed from
;;;			invalid objects list.
;;; 03/03/92 Amickish - s-value of :internally-parented ---> destroy-slot
;;; 02/28/92 Amickish - Removed kr::schema-slots check because schema-p works
;;;                     correctly now (returns NIL on destroyed objects)
;;; 02/27/92 Szekely - Changed :destroy-me method of aggregate to not do
;;;                    copy-list.
;;; 02/19/92 Amickish - Bound kr::*constants-disabled* in initialize method
;;;                     for aggregates.
;;; 02/06/92 Pervin - Added leaf-objects-in-rectangle, components-in-rectangle,
;;;		      and obj-in-rectangle.
;;; 02/01/92 Amickish - Added :internally-parented checks to opal:aggregate's
;;;                     add-component method.
;;; 11/23/91 Amickish&Koz - Rewrote move-component so now it does not call
;;;                         add-component.  Instead, both methods call
;;;                         install-component, a function whose code used
;;;                         to be part of add-component.
;;; 11/6/91 Pervin - Made move-component a method.
;;; 9/25/91 Pervin - Add-component takes a new optional argument :move
;;;		     which is only invoked if it is called from move-component
;;; 8/29/91 Pervin - In remove-component, make sure that object being
;;;                  removed from aggregate belongs to that aggregate!
;;; 3/25/91 Pervin - Added :local t to dovalues loop.
;;; 3/4/91  D'Souza - Removed nickname "MO" of package Opal.
;;; 7/11/90 Ed Pervin - new :destroy-me method
;;; 6/1/90   RBD Fixed add-component to accept :head :tail :after :before
;;; 5/20/90  BVZ Changed g-value to g-local-value in add-component
;;;              to fix window bug.
;;; 5/8/90   Mike Sannella
;;;              Do-Components and Do-All-Components were ignoring
;;;              their :type argument.
;;; 3/6/90   ECP Allowed point-to-leaf, point-to-component,
;;;              do-components, and do-all-components to accept
;;;              a list of types as the argument to :type.
;;; 1/2/90   RBD Corrected a misplaced parenthesis in add-component.
;;;              Also, in add-component, replaced calls to append-value
;;;		 by calls to nconc.
;;; 12/11/89 ECP Point-to-leaf was returning T when :type was aggregate.

(in-package "OPAL")

;;; Aggregate objects

;;; Aggregates allow for a group of graphical-objects to be associated
;;; together to form a new, more complex object.
;;; 
;;; An implementation detail:
;;; The children of a gob are stored in a list from bottom most to top
;;; most, since we want to redraw fastest and redraws occur from bottom to
;;; top.

;;; Methods on aggregates:

;;; Initialize
;;; 
;;; The :aggregate-p slot is used by the update algorithm for efficiency
(define-method :initialize opal:aggregate (a-aggregate)
  (call-prototype-method a-aggregate)
  (let ((components (g-local-value a-aggregate :components)))
    (let ((kr::*constants-disabled* T))
      (s-value a-aggregate :components NIL))
    (dolist (child components)
      (unless (g-local-value child :parent)
	(add-component a-aggregate child :where :front)))
    (setf (update-info-aggregate-p
	   (the UPDATE-INFO (g-local-value a-aggregate :update-info)))
	  T)))

;;; Destroy method
;;; 
;;; If top-level-p is true, then you must erase the aggregate (carefully!).
;;; Otherwise, we can presume that the calling party has already gone
;;; through the trouble of erasing the aggregate.
;;; If, while trying to erase the aggregate, we hit illegal values, then the
;;; window will be updated fully (after the destroy).

(define-method :destroy-me opal:aggregate (a-aggregate &optional (top-level-p T))
  (if a-aggregate
      (let* ((the-window (g-local-value a-aggregate :window))
             (erase-p (and top-level-p the-window))
             (parent  (g-local-value a-aggregate :parent))
             total-update-p)
        (if erase-p;; If at top-level, then erase...
            (if (null parent)
                (if (eq a-aggregate (g-value the-window :aggregate))
                    (s-value the-window :aggregate NIL)
                    (progn
                      (format t "~%Warning in Destroy: aggregate '~A' has no parent,~%" a-aggregate)
                      (format t   "        is in window '~A', but is not that window's:aggregate.~%"
                              the-window)
                      (setq erase-p NIL)))
                (setq total-update-p (not (carefully-erase a-aggregate the-window)))))
        (when (and top-level-p parent)
          (s-value parent :components
		   (delete a-aggregate (g-local-value parent :components)))
          (mark-as-changed parent :components))
        (let ((components (g-local-value a-aggregate :components)))
          (with-constants-disabled (destroy-slot a-aggregate :components))
          (dolist (component components)
            (when (schema-p component)
              (destroy component NIL))))
        (destroy-schema a-aggregate)
        (if erase-p
            (update the-window total-update-p)))))



;;; Install-Component is called from Add-Component and Move-Component.  It
;;; does all the weird placement stuff for getting the gob into the right
;;; location of the :components list of the aggregate.  It does nothing else.
(defun install-component (a-aggregate gob args)
  (let (locator where)
    (cond ((eq (first args) :where)
                (setq where (second args))
                (setq locator (third args)))
          ((first args)
                (setq where (first args))
                (setq locator (second args)))
          (t (setq where :front)))
    (case where
      ((:front :tail)
       (s-value a-aggregate :components
		(nconc (g-local-value a-aggregate :components)
		       (list gob))))
      ((:behind :before)
       (let ((components (g-local-value a-aggregate :components)))
	 (do ((smash-slot components (cdr smash-slot))
	      (pre-splice nil smash-slot))
	     ((or (eq (car smash-slot) locator)
		  (null smash-slot))
	      (cond ((null smash-slot)
		     (error "Locator for :where :behind ~S is not in aggregate ~S."
			    locator a-aggregate))
		    (pre-splice
		     (setf (cdr pre-splice) (cons gob smash-slot))
		     (s-value a-aggregate :components components))
		    (t (s-value a-aggregate
				:components (cons gob smash-slot))))))))
      ((:in-front :after)
       (let* ((components (g-local-value a-aggregate :components))
	      (remainder (member locator components))
	      (splice (cons gob (cdr remainder))))
	 (cond (remainder
		(setf (cdr remainder) splice)
		(s-value a-aggregate :components components))
	       (t (error "Locator for :where :in-front ~S is not in aggregate ~S"
			 locator a-aggregate)))))
      (:at
       (let* ((components (g-local-value a-aggregate :components))
	      (remainder (unless (zerop locator)
			   (nthcdr (1- locator) components))))
	 (s-value a-aggregate :components
		  (if (zerop locator)
		      (cons gob components)
		      (progn
			(setf (cdr remainder) (cons gob (cdr remainder)))
			components)))))
      ((:back :head)
       (s-value a-aggregate :components
		(cons gob (g-local-value a-aggregate :components))))
      (otherwise
       (s-value a-aggregate :components
		(nconc (g-local-value a-aggregate :components)
		       (list gob)))
       (warn (format nil "Bad where option in add-component: ~S." where))))))

;;; Add-Component adds gob to aggregate covering according to the arguments of
;;; the where keyword argument.  Note that the :where keyword is now OPTIONAL.
;;; 
;;; Options for the :where keyword argument:
;;;    :front, :back, :behind, :in-front, or :at
;;;    :tail,  :head, :before, :after
;;; Interpretation of arg:
;;;  - if (member '(:behind :in-front :before :after) where) 
;;;    gob is positioned relative to (third arg)
;;;  - if (eq where :at) gob is positioned at (third arg) positions from
;;; the front of the children
;;;  - otherwise it is ignored.
;;; 
;;; This is really a lot less complicated than it seems, you can say things
;;; like:
;;; (add-component foo :where :front)       == (add-component foo :front)
;;; (add-component foo :where :behind bar)  == (add-component foo :behind :bar)
;;; (add-component foo :where :at 2)        == (add-component foo :at 2)

(define-method :add-component opal:aggregate (a-aggregate gob &rest args)
  (if (eq gob (g-local-value gob :window))		;; Is this a window?
      (error "*** ~A is a WINDOW, and was not added to ~A~%"
	     gob a-aggregate))
  (let ((parent (g-local-value gob :parent))
	(internally-parented (g-local-value gob :internally-parented)))
    (if (and parent (not internally-parented))
	;; The object already has a parent which wasn't assigned by
	;; aggregadgets
	(error "Graphical-object ~S has :parent ~S already." gob parent))
    (install-component a-aggregate gob args)
    ;; Set up the reverse pointer from child to aggregate
    (if internally-parented
	(destroy-slot gob :internally-parented)
	(s-value gob :parent a-aggregate)))

  ;; Propagate window and dirty bit to children
  (let ((a-window (g-local-value a-aggregate :window)))
    (when a-window
      (let ((gob-update-info (the UPDATE-INFO (g-local-value gob :update-info))))
	(set-display-slots gob a-window t)
	
	;; Place the object on its window's invalid-objects list
	(make-object-invalid gob gob-update-info a-window)
   
	;; Invalidate all of the aggregate's children (recursively)
	(if (update-info-aggregate-p gob-update-info)
	    (do-all-components gob
	      #'(lambda (c)
		  (let ((c-update-info (g-value c :update-info)))
		    (make-object-invalid c c-update-info a-window)))))

	;; Indicate that the old-bbox is not valid now since gob was
	;; not visible in this window previously...
	(setf (bbox-valid-p (update-info-old-bbox gob-update-info)) NIL))))

  ;; Signal we have changed components list
  (mark-as-changed a-aggregate :components)

  ;; Return gob
  gob)

;;; Add multiple components at the same time
;;; 
(defun add-components (agg &rest components)
  (dolist (component components)
    (add-component agg component))
  (car (last components)))

;;; Remove-component deletes the topmost occurance of gob in aggregate
;;; 
(define-method :remove-component opal:aggregate (a-aggregate gob)

 (if (not (eq (g-local-value gob :parent) a-aggregate))
  (warn (format nil "Cannot remove-component ~A from ~A" gob a-aggregate))
  ;; add the gob's bounding box to the clipping region of the topmost
  ;; overlapping aggregate that contains it (or to its parent if no
  ;; such aggregate is found), and clear the gob's drawable, display,
  ;; and dirty slots, and recursively clear the same slots in its children
  ;; as well

  (let* ((gob-update-info (the UPDATE-INFO (g-local-value gob :update-info)))
         (a-window (update-info-window gob-update-info))
	 (a-window-update-info 
	    (when a-window (g-local-value a-window :update-info))))
    (when a-window-update-info
      (let* ((win-update-info (g-value a-window :win-update-info))
	     (window-bbox (update-info-old-bbox a-window-update-info))
	     (bbox (update-info-old-bbox gob-update-info)))

	; since the object is no longer in the window, it should be
	; removed from the window's invalid objects list.
	; (this is defined in update-basics.lisp).
	(if (update-info-aggregate-p gob-update-info)
	    (do-all-components gob #'(lambda (c)
				       (remove-from-invalid-objects-list
					c win-update-info))
			       :self T)
	    (remove-from-invalid-objects-list gob win-update-info))

	(merge-bbox window-bbox bbox)
        (set-display-slots gob nil nil)))

    (setf (update-info-invalid-p gob-update-info) NIL)
    (s-value a-aggregate :components
	     (delete gob (g-local-value a-aggregate :components)
		     :from-end t :count 1))
    (s-value gob :parent NIL)

    ;; signal we have changed components list
    (mark-as-changed a-aggregate :components))))

;;; Similar to a call to remove-component followed by a call to
;;; add-component, but faster because it does not call set-display-slots
;;; or perform any of the other overhead.  It merely sets the windows old-bbox,
;;; manually removes the object from the :components list, then reinstalls it

(define-method :move-component opal:aggregate (a-aggregate gob &rest args)
  (let* ((gob-update-info (g-local-value gob :update-info))
         (a-window (update-info-window gob-update-info))
	 (a-window-update-info (g-local-value a-window :update-info)))
    (and a-window a-window-update-info
      (let ((window-bbox (update-info-old-bbox a-window-update-info))
            (bbox (update-info-old-bbox gob-update-info)))
	(merge-bbox window-bbox bbox))))
  (s-value a-aggregate :components
	   (delete gob (g-local-value a-aggregate :components)))
  (install-component a-aggregate gob args))
  
;;; Remove multiple components at the same time
(defun remove-components (agg &rest components)
  (dolist (component components)
    (remove-component agg component)))

;;; Remove all components
(defun remove-all-components (agg)
  (dolist (component (copy-list (g-local-value agg :components)))
    (remove-component agg component)))

;;; Like kr:is-a-p, but types can be a list.
(defun my-is-a-p (child types)
  (when types
    (if (listp types)
	(or (is-a-p child (car types))
	    (my-is-a-p child (cdr types)))
	(is-a-p child types))))

;;; Do-Components applies function to all children of aggregate

(define-method :do-components opal:aggregate (a-aggregate a-function 
					     &key (type t) (self nil))
  (let ((children (g-local-value a-aggregate :components)))
    (dolist (child children)
      (when (or (eq type t)
		(is-a-p child type))
        (funcall a-function child)))
    (when (and self
	       (or (eq type t)
		   (is-a-p a-aggregate type)))
      (funcall a-function a-aggregate))))


;;; Do-All-Components is like do-components, except it continued through all
;;; aggregates to their children

(define-method :do-all-components opal:aggregate (a-aggregate a-function
						 &key (type t) (self nil))
  (let ((children (g-local-value a-aggregate :components)))
    (dolist (child children)
      (if (is-a-p child opal:aggregate)
	  (do-all-components child a-function :type type :self t)
	  (when (or (eq type t)
		    (is-a-p child type))
	    (funcall a-function child))))
    (when (and self
	       (or (eq type t)
		   (is-a-p a-aggregate type)))
      (funcall a-function a-aggregate))))


;;; Point-To-Component queries the aggregate for first generation children
;;; at point (x,y). If :type is specified only children of the specified
;;; type will be returned.

(defun point-to-component-recur (component-list x y type)
  (and component-list
       (or (point-to-component-recur (cdr component-list) x y type)
	   (let ((child (car component-list)))
	     (when (and (or (eq type t)
			    (my-is-a-p child type))
			(point-in-gob child x y))
	       child)))))
  
(define-method :point-to-component opal:aggregate
	       (a-aggregate x y &key (type t))
  (when (point-in-gob a-aggregate x y)
    (point-to-component-recur (g-local-value a-aggregate :components) x y type)))


;;; Point-To-Leaf is similar to Point-To-Component except that the query
;;; continues to the deepest children.

(defun point-to-leaf-recur (component-list x y type)
  (and component-list
       (or (point-to-leaf-recur (cdr component-list) x y type)
	   (let ((child (car component-list)))
	     (cond ((and (is-a-p child opal:aggregate)
			 (not (g-value child :pretend-to-be-leaf)))
		    (point-to-leaf child x y :type type))
		   ((or (eq type t)
			(my-is-a-p child type))
		    (when (point-in-gob child x y) child)))))))

(define-method :point-to-leaf opal:aggregate
	       (a-aggregate x y &key (type t))
  (when (point-in-gob a-aggregate x y)
    (or (and (not (eq type t))
	     (my-is-a-p a-aggregate type)
             a-aggregate)
	(point-to-leaf-recur (g-local-value a-aggregate :components)
			     x y type))))



;; Returns T if two rectangles intersect
(defun rectintersect (left1 top1 right1 bottom1
		      left2 top2 right2 bottom2)
  (and left1 top1 right1 bottom1
      (<= left1 right2)
       (<= left2 right1)
       (<= top1 bottom2)
       (<= top2 bottom1)))

;; Returns T if rectangle1 is inside rectangle2
(defun rectsubset (left1 top1 right1 bottom1
		   left2 top2 right2 bottom2)
  (and left1 top1 right1 bottom1
       (<= left2 left1)
       (<= right1 right2)
       (<= top2 top1)
       (<= bottom1 bottom2)))

;;; Returns a list of leafs of obj that intersect the rectangle
;;; bounded by top, left, width, height  (unless :intersect in nil,
;;; in which case the leafs must be completely inside the rectangle).
;;;
(defun leaf-objects-in-rectangle (obj top left bottom right
				 &key (type t) (intersect t))
  (if (g-value obj :visible)
      (let* ((obj-left (g-value obj :left))
	     (obj-top (g-value obj :top))
	     (obj-right (+ obj-left -1 (g-value obj :width)))
	     (obj-bottom (+ obj-top -1 (g-value obj :height))))
	(cond ((and (is-a-p obj aggregate)
		    (not (g-value obj :pretend-to-be-leaf)))
	       (when (rectintersect obj-left obj-top obj-right obj-bottom
				    left top right bottom)
		 (let (leafs)
		   (dolist (c (g-value obj :components))
		     (setq leafs (nconc leafs
					(leaf-objects-in-rectangle
					 c top left bottom right
					 :type type
					 :intersect intersect))))
		   leafs)))
	      ((or (eq type t) (my-is-a-p obj type))
	       (when (funcall (if intersect #'rectintersect #'rectsubset)
			      obj-left obj-top obj-right obj-bottom
			      left top right bottom)
		 (list obj)))))))


;;; Returns T or NIL depending if "obj" intersects rectangle
;;; bounded by "top", "left", "width", "height".
;;; (If intersect = NIL, then "obj" must be completely inside the rectangle.
;;; (If type <> T, then "obj" must be of type "type".)
(defun obj-in-rectangle (obj top left bottom right &key (type t) (intersect t))
  (if (g-value obj :visible)
      (let* ((obj-left (g-value obj :left))
	     (obj-top (g-value obj :top))
	     (obj-right (+ obj-left -1 (g-value obj :width)))
	     (obj-bottom (+ obj-top -1 (g-value obj :height))))
	(and (or (eq type t) (my-is-a-p obj type))
	     (funcall (if intersect #'rectintersect #'rectsubset)
		      obj-left obj-top obj-right obj-bottom
		      left top right bottom)))))

  
;;; Like leaf-objects-in-rectangle, but only returns top-level components
;;; instead of leafs.
(defun components-in-rectangle (agg top left bottom right
				&key (type t) (intersect t))
  (if (g-value agg :visible)
      (let (leafs)
	(dolist (obj (g-value agg :components))
	  (when (obj-in-rectangle obj top left bottom right
				  :type type :intersect intersect)
	    (setq leafs (nconc leafs (list obj)))))
	leafs)))



;;; This routine sets the :hit-threshold slot of the aggregate
;;; agg to be the maximum of the hit-thresholds of its components.
(defun set-aggregate-hit-threshold (agg)
  (when (is-a-p agg opal:aggregate)
    (let ((max-hit 0))
      (dovalues (c agg :components :local t)
		(set-aggregate-hit-threshold c)
		(setq max-hit (max max-hit (g-value c :hit-threshold))))
      (s-value agg :hit-threshold max-hit))))
