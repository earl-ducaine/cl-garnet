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
;;; Add a component to an aggregadget and its instances.
;;; 
;;; Roger B. Dannenberg, 1990
;;;
;;; $Id::                                                             $


#|
======================================================================
Change log:
  07/14/93 Andrew Mickish - Fixed :replace-item-prototype-object method to work
             with new aggrelists;  New Remove-The-Component;
             Eliminated recursive maintenance of components in add-item and
             remove-item, since it will be taken care of by fix-update-slots
  05/28/93 Dario Giuse    - Replaced bindings of kr::*constants-disabled* with
	     calls to with-constants-disabled.
  05/20/93 Andrew Mickish - Removed calls to opal::destroy-constraints
  04/19/93 Andrew Mickish - Fixed Fix-Update-Slots method to handle change in
             :items value from a list to a number and vice versa.
  03/03/93 Andrew Mickish - Fixed Option-Button-String-Func to consider label;
             Aggrelist-Edit-String-Func now calls :new-item-label method
             (used by menubars)
  03/01/93 Andrew Mickish - In add/remove-item methods, check whether items
             is a number, not whether it is a cons (same effect without
             crashing on NIL :items list)
  02/24/93 Andrew Mickish - Added Aggrelist-Edit-String-Func
  02/19/93 Andrew Mickish - Gadget-Add/Remove-Item slot parameter can be a list
             for gadgets with aggrelists not at the top-level (option-button)
  01/18/93 Andrew Mickish - Notice-items-changed --> :fix-update-slots
  12/03/92 Andrew Mickish - Made item parameter optional for :add-item method;
             Fixed :remove-nth-item method to remove nth component; Fixed
             aggrelist methods to work when :items is a number
  06/16/92 Andrew Mickish - New aggrelist and gadget methods (comments below)
  04/13/92 Brad VanderZanden - In :remove-component, do not remove an instance
	     from an aggregadget if in the same aggregadget as the prototype.
  04/07/92 Andrew Mickish - Get-Local-Value ---> G-Local-Value
  03/25/92 Andrew Mickish - Get-Values ---> G-Value
  03/18/92 Andrew Mickish - Added condition to recursion in :remove-item method
  11/06/91 Edward Pervin - Made move-component a method.
  05/13/91 Edward Pervin - Add-item and remove-item had been defined
	     in aggrelists.lisp.
  04/22/91 Andrew Mickish - Added Gadget-Add-Item and Gadget-Remove-Item
======================================================================
|#

#| Implementation details:

ADD-COMPONENT
Call add-local-component to add a new component at the prototype level.
Then go to each instance and recursively call add-component.
The insert point will be determined as follows:
  The default position is :front.
  If the position is :front/:tail, always insert at the :front.
  If the position is :back/:head, always insert at the :back.
  If the position is :behind/:before x, then
    if the instance aggregadget has a component that is an instance of x, then
      insert :behind the instance of x,
    otherwise if the instance aggregadget has a component y named xn, where
         x is :known-as xn, then
      insert :behind y
    otherwise, print a warning and insert at the :front  (the philosophy
      here is to err toward the front, making errors visible).
  If the position is :in-front/:after x, then the situation is analogous to
    :behind/:before.
  If the position is :at, then use :at and the same locator on each
    instance.

ADD-ITEM
Works just like add-component, but acts on :items slots.  If an
instance inherits its :item slot, then no local changes are made.
After the changes are made, notice-items-changed is called on each 
affected aggrelist.

|#

(in-package "OPAL")

(eval-when (eval load compile)
  (export '(gadget-add-item gadget-remove-item)))


;; Supports Add-Component's terminology (screen related):
;;	:front :back :behind :in-front :at
;; And also the corresponding names (list related):
;;      :tail  :head :before :after    :at
;;
;; Also, you can call it as (add-element list-agg element :where :head)
;; or you can simply omit the ':where' field (again, to be like Add-Component)
;;
;; The default is for :where is :tail
;;
;; The big inefficiency right now is that after it finds the element in
;; the :components slot, it eventually calls "call-prototype-method", which
;; will do the same thing all over again!

; element - the new component to be added
; elements - the old components of the aggregate
;
(define-method :add-local-component aggrelist (my-agg-list element &rest args)
 (let ((elements (g-local-value my-agg-list :components)) 
       where locator
       (name (g-local-value element :known-as)))
   (when (member element elements)
     (remove-local-component my-agg-list element))

   ; If the component is named, then set a slot in the aggregate so that it
   ; can be accessed through that slot
   (if name (s-value my-agg-list name element))

   (multiple-value-setq (where locator) (get-wheres args))

   ;Call Add-Component
   (do ((successful nil successful))	;;; This is better done with a GO
       (successful t)
     (setq successful t)                ;;; So we must set to NIL for repeat
     (setq elements (g-local-value my-agg-list :components))
     (cond ((null (g-local-value my-agg-list :tail))	;;;; NEW LIST ???
	    (s-value element :prev nil)
	    (s-value element :next nil)
	    (s-value my-agg-list :tail element)
	    (s-value my-agg-list :head element)
	    (kr-send aggregate :add-component my-agg-list element))
	   ((or (eq where :front)
		(eq where :tail))
	    (let ((old-tail (g-local-value my-agg-list :tail)))
	      (s-value old-tail :next element)
	      (s-value element :next nil)
	      (s-value element :prev old-tail)
	      (s-value my-agg-list :tail element)
	      (kr-send aggregate :add-component 
		       my-agg-list element :where :front)))
	   ((or (eq where :back)
		(eq where :head))
	    (let ((old-head (g-local-value my-agg-list :head)))
	      (s-value old-head :prev element)
	      (s-value element :prev nil)
	      (s-value element :next old-head)
	      (s-value my-agg-list :head element)
	      (kr-send aggregate :add-component 
		       my-agg-list element :where :back)))
	   ((or (eq where :behind)		;;; Goes after 'behind-element'
		(eq where :before))
	    (let ((mem-sublist (member locator elements)))
	      (if mem-sublist
		  (let ((behind-element (g-value locator :prev)))
		    (setq mem-sublist (cdr mem-sublist))
		    (s-value element :prev behind-element)
		    (s-value element :next locator )
		    (s-value locator :prev element)
		    (if behind-element
			(s-value behind-element :next element)
			(s-value my-agg-list :head element))
		    (kr-send aggregate :add-component 
			     my-agg-list element :where :behind locator))
		  (progn
		    (warn "New element being placed at back of aggrelist.")
		    (setq where :back)		;;; Just put it at the back
		    (setq successful nil)))))	;;; by looping again
	   ((eq where :at)
	    (let ((count (length elements)))
	      (setq locator (if (numberp locator) (max locator 0) 0))
	      (setq locator (min locator count))
	      (cond ((eq locator 0) (setq where :back))
		    ((eq locator count) (setq where :front))
		    (t (setq locator (nth locator elements))
		       (setq where :before)))
	      (setq successful nil)))	;;; by looping again
	   ((or (eq where :in-front)	;;; Search list backwards!
		(eq where :after))
	    (let ((mem-sublist (member locator elements)))
	      (if mem-sublist
		  (let ((infront-element (g-value locator :next)))
		    (s-value element :prev locator)
		    (s-value element :next infront-element )
		    (s-value locator :next element)
		    (if infront-element
			(s-value infront-element :prev element)
			(s-value my-agg-list :tail element))
		    (kr-send aggregate :add-component
			     my-agg-list element :where :in-front locator))
		  (progn
		    (warn "New element being placed at front of aggrelist.")
		    (setq where :front)		;;; Just put it at the front
		    (setq successful nil)))))	;;; by looping again
	   (t (format t "***Illegal :where ('~S') in 'list-add'~%" where)
	      (format t "***  Defaulting, settting :where to :front~%")
	      (setq where :front)
	      (setq successful nil))))
   ))

(define-method :remove-local-component aggrelist (my-agg-list element)
  (let ((next (g-value element :next))
	(prev (g-value element :prev))
	(name (g-local-value element :known-as)))
    (cond (name
	   (destroy-slot my-agg-list name)))
    (when (eq element (g-value my-agg-list :tail))
      (s-value my-agg-list :tail prev))
    (when (eq element (g-value my-agg-list :head))
      (s-value my-agg-list :head next))
    (if prev (s-value prev :next next))
    (if next (s-value next :prev prev))
    (kr-send aggregate :remove-component my-agg-list element)
    ))


;;; Replace the item-prototype slot and propagate the change to instances.
;;;
(define-method :replace-item-prototype-object aggrelist (agg item-proto)
  (let ((old-proto (g-value agg :item-prototype-object)))
    (dolist (agg-instance (g-local-value agg :is-a-inv))
      (when (is-a-p (g-value agg-instance :item-prototype-object) old-proto)
	(replace-item-prototype-object agg-instance
				       (create-instance nil item-proto)))))
  (s-value agg :item-prototype-object item-proto)
  (remove-all-components agg)
  (s-value agg :old-items NIL))


;;; New Improved Aggrelist Methods
;;; Andrew Mickish  6/16/92
;;; 
;;; There are several differences between the implementation of these
;;; methods and the old aggrelist methods.
;;; 1)  Unlike the old methods, these make changes to instances in a
;;;     manner consistent with the rest of Garnet.  The old methods
;;;     s-valued a local :items list in all of an aggrelist's instances,
;;;     thereby destroying the natural inheritance scheme of the hierarchy.
;;;     In the new methods, the :items list is changed once in the prototype,
;;;     and then for each instance that inherits that :items list,
;;;     corresponding components are added or removed from the aggrelist.
;;; 2)  These methods handle component maintenance simultaneously with the
;;;     :items list, rather than just changing the :items list and calling
;;;     Notice-Items-Changed.

(defun Add-The-Component (alist rank)
  (let* ((components (g-value alist :components))
	 (prev-component (if (> rank 0)
			     (nth (1- rank) components)))
	 (prototype (g-value alist :item-prototype-object))
	 ;; I tried using begin/end-create-instance here, but had problems
	 ;; with the query gadget
	 (new-component (create-instance NIL prototype
			  (:parent alist)
			  (:internally-parented T)
			  (:rank rank)
			  (:prev prev-component))))
    (add-local-component alist new-component :at rank)))

(defun Recursive-Add-Component (alist rank)
  (Add-The-Component alist rank)
  (dolist (inst (g-value alist :is-a-inv))
    (unless (has-slot-p inst :items)
      (Recursive-Add-Component inst rank))))


(define-method :add-local-item opal:aggrelist (alist item &rest args)
  (multiple-value-bind (where locator key) (get-wheres args)
    (let* ((old-items (or (g-local-value alist :items)
			  (let ((i (g-value alist :items)))
			    (if (numberp i) i (copy-list i)))))
	   (items (if (numberp old-items)
		      (1+ old-items)
		      (opal::insert-item item old-items where locator key)))
	   (rank (if (numberp old-items)
		     old-items
		     (position item items
			       :test #'(lambda (x y)
					 (equal x (funcall key y)))))))
      (s-value alist :old-items (s-value alist :items items))
      (Add-The-Component alist rank))))


(define-method :add-item opal:aggrelist (alist &optional item &rest args)
  (multiple-value-bind (where locator key) (get-wheres args)

    ;; first add to the prototype
    (add-local-item alist item where locator key)

    ;; now do instances
    #+comment
    (let* ((items (g-value alist :items))
	   (rank (if (numberp items)
		     items
		     (position item items
			       :test #'(lambda (x y)
					 (equal x (funcall key y)))))))

      (dolist (inst (g-value alist :is-a-inv))
	(unless (has-slot-p inst :items)
	  ;; The instances have already gotten changes in the :items list
	  ;; through inheritance, so just add corresponding components
	  (Recursive-Add-Component inst rank)
	  ;; otherwise, :items is not inherited, so don't inherit changes
	  )))))

(defun Is-In-Hierarchy (agg obj)
  (if obj
      (or (eq agg obj)
	  (Is-In-Hierarchy agg (g-value obj :parent)))))

(defun Remove-The-Component (alist rank &optional old-item)
  ;; Before destroying the component, remove the item from it
  (let* ((comp-to-destroy (nth rank (g-value alist :components))))
    ;; Hack for item/function pairs in gadgets
    (if (and (consp old-item) (schema-p (car old-item)))
	(setf old-item (car old-item)))
    (if (and (schema-p old-item)
	     (g-value old-item :parent)
	     (Is-In-Hierarchy comp-to-destroy old-item))
	(with-constants-disabled
	  (remove-local-component (g-value old-item :parent) old-item)))
    ;; Check to see if the comp-to-destroy still has a parent because
    ;; if the item was itself the component, it was just removed
    (if (and (schema-p comp-to-destroy) (g-value comp-to-destroy :parent))
	(remove-local-component alist comp-to-destroy))
    ;; Always remove aggrelist components before destroying them so
    ;; that the aggrelist bookkeeping will be done.
    (opal:destroy comp-to-destroy)))

(defun Recursive-Remove-Component (alist rank &optional old-item)
  (Remove-The-Component alist rank old-item)
  (dolist (inst (g-value alist :is-a-inv))
    (unless (has-slot-p inst :items)
      (Recursive-Remove-Component inst rank))))


(define-method :remove-local-item opal:aggrelist
               (alist &optional item &key (key #'opal:no-func))
  (let* ((items (or (g-local-value alist :items)
		    (copy-list (g-value alist :items))))
	 (rank (if item
		   (position item items
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (if (numberp items) items (length items))))))
    (cond (item
	   (s-value alist :old-items
		    (s-value alist :items (opal::delete-elt item items key)))
	   ;; Before destroying the component, remove the item from it
	   (let* ((comp-to-destroy (nth rank (g-value alist :components))))
	     (if (and (schema-p item)
		      (g-value item :parent)
		      (Is-In-Hierarchy comp-to-destroy item))
		 (with-constants-disabled
		   (remove-local-component (g-value item :parent) item)))
	     ;; Check to see if the comp-to-destroy still has a parent because
	     ;; if the item was itself the component, it was just removed
	     (if (g-value comp-to-destroy :parent)
		 (remove-local-component alist comp-to-destroy))
	     ;; Always remove aggrelist components before destroying them so
	     ;; that the aggrelist bookkeeping will be done.
	     (opal:destroy comp-to-destroy)))
	  (t
	   (if (numberp items)
	       (s-value alist :old-items (decf (g-value alist :items)))
	       (s-value alist :old-items
			(s-value alist :items (nbutlast items))))
	   (let ((comp-to-destroy (nth rank (g-value alist :components))))
	     (remove-local-component alist comp-to-destroy)
	     (opal:destroy comp-to-destroy))))))

(define-method :remove-item opal:aggrelist
               (alist &optional item &key (key #'opal:no-func))
  (let* (#+comment
	 (items (or (g-local-value alist :items)
		    (let ((i (g-value alist :items)))
		      (if (numberp i) i (copy-list i)))))
	 #+comment
	 (rank (if item
		   (position item items
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   ;; Remove last item if none are specified
		   (1- (if (numberp items) items (length items))))))
    ;; first remove from the prototype
    (remove-local-item alist item :key key)

    ;; now do instances
    #+comment
    (dolist (inst (g-value alist :is-a-inv))
      (unless (has-slot-p inst :items)
	;; The instances have already gotten changes in the :items list
	;; through inheritance, so just remove corresponding components
	(Recursive-Remove-Component inst rank)
	;; otherwise, :items is not inherited, so don't inherit changes
	))))


(defun Gadget-Add-Local-Item (gadget item slot args)
  (let (where locator key)
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    (let* ((old-items (or (g-local-value gadget :items)
			  (copy-list (g-value gadget :items))))
	   (items (opal::insert-item item old-items where locator key))
	   (rank (position item items
			   :test #'(lambda (x y)
				     (equal x (funcall key y)))))
	   (alist gadget))
      ;; The aggrelist may be more than one level deep, in which case the
      ;; slot parameter would be a list of slots.
      (if (consp slot)
	  (dolist (sl slot)
	    (setf alist (g-value alist sl)))
	  (setf alist (g-value alist slot)))
      (s-value alist :old-items
	       (s-value gadget :items items))
      (Add-The-Component alist rank)
      )))

(defun Gadget-Add-Item (gadget item slot args)
  (declare (ignore slot))
  (let (where locator key)
    (multiple-value-setq (where locator key) (opal::get-wheres args))
    ;; Do not pass SLOT to :add-local-item method -- the method knows it!
    (add-local-item gadget item where locator key)
    #+comment
    (let ((rank (position item (g-value gadget :items)
			  :test #'(lambda (x y)
				    (equal x (funcall key y))))))
      (dolist (inst (g-value gadget :is-a-inv))
	(unless (has-slot-p inst :items)
	  ;; Note:  Adding items to instances of gadgets where the aggrelist
	  ;; is more than one level deep is not implemented yet.
	  (Recursive-Add-Component (g-value inst slot) rank))))))

(defun Gadget-Remove-Local-Item (gadget item slot key)
  (let* ((items (or (g-local-value gadget :items)
		    (copy-list (g-value gadget :items))))
	 (rank (if item
		   (position item items
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (length items))))
	 (alist gadget))
    (if (consp slot)
	(dolist (sl slot)
	  (setf alist (g-value alist sl)))
	(setf alist (g-value alist slot)))
    ;; Gadgets always have lists for their :items values, so don't consider
    ;; the case where the value of :items is a number.
    (s-value alist :old-items
	     (s-value gadget
		      :items
		      (opal::delete-elt (or item (nth rank items)) items key)))
    ;; Before destroying the component, remove the item from it
    (let* ((comp-to-destroy (nth rank (g-value alist :components))))
      (if (and (schema-p item)
	       (g-value item :parent)
	       (Is-In-Hierarchy comp-to-destroy item))
	  (with-constants-disabled
	    (remove-local-component (g-value item :parent) item)))
      (if (g-value comp-to-destroy :parent)
	  (remove-local-component alist comp-to-destroy))
      (opal:destroy comp-to-destroy))
    ))



(defun Gadget-Remove-Item (gadget item slot key)
  (declare (ignore slot))
  (let* (#+comment
	 (items (g-value gadget :items))
	 #+comment
	 (rank (if item
		   (position item items
			     :test #'(lambda (x y)
				       (equal x (funcall key y))))
		   (1- (length items)))))
    ;; first remove from the prototype
    (remove-local-item gadget item :key key)

    ;; now do instances
    #+comment
    (dolist (inst (g-value gadget :is-a-inv))
      (unless (has-slot-p inst :items)
	;; The instances have already gotten changes in the :items list
	;; through inheritance, so just add corresponding components
	(Recursive-Remove-Component (g-value inst slot) rank)
	;; otherwise, :items is not inherited, so don't inherit changes
	))
    ))

(defun Motif-Buttons-Add-Local-Item (gadget item &rest args)
  (Gadget-Add-Local-Item gadget item :button-list args))
(defun Motif-Buttons-Add-Item (gadget item &rest args)
  (Gadget-Add-Item gadget item :button-list args))
(defun Motif-Buttons-Remove-Local-Item (gadget &optional item
					       &key (key #'opal:no-func))
  (Gadget-Remove-Local-Item gadget item :button-list key))
(defun Motif-Buttons-Remove-Item (gadget &optional item
					 &key (key #'opal:no-func))
  (Gadget-Remove-Item gadget item :button-list key))

(define-method :change-item aggrelist (agg new-item n)
  (let ((items (g-value agg :items)))
    (if (or (>= n (length items))
	    (< n 0))
	(warn "Bad index in change-item: ~A" n)
	(let ((old-item (nth n items)))
	  (cond
	    ((same-type-p new-item old-item)
	     (s-value agg :old-items
		      (s-value agg :items (substitute new-item old-item items
						      :test #'equal))))
	    (t (remove-item agg old-item)
	       (add-item agg new-item :at n)))))))


(define-method :remove-nth-item aggrelist (agg n)
  (let ((items (or (g-local-value agg :items)
		   (let ((i (g-value agg :items)))
		     (if (numberp i) i (copy-list i))))))
    (cond ((numberp items)
	   (s-value agg :old-items (s-value agg :items (1- items)))
	   (let ((comp-to-destroy (nth n (g-value agg :components))))
	     (remove-local-component agg comp-to-destroy)
	     (opal:destroy comp-to-destroy))
	   #+comment
	   (dolist (inst (g-value agg :is-a-inv))
	     (unless (has-slot-p inst :items)
	       (Recursive-Remove-Component inst n))))
	  (t
	   (remove-item agg (nth n items))))))

(define-method :remove-nth-component aggrelist (agg n)
  (let ((target (nth n (g-local-value agg :components))))
    (cond (target
	   (remove-local-component agg target)))))


