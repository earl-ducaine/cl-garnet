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
;;; The functions in this file handle the menu code
;;; that allows the user to select an interactor and
;;; have it appear in an appropriate interactor dialog
;;; box
;;;

(in-package "LAPIDARY")


(defvar interactors-list
  '("choice" "move/grow" "two-point" "text" "angle"))
(defvar interactor-menu nil)

;;; has-component-p -- see if aggregate has a component
;;;
(defun has-component-p (agg obj)
  (let ((comps (g-local-value agg :components)))
    (dolist (comp comps)
      (cond ((eq obj comp) (return t))))))


;;; has-leaf-p -- see if aggregate tree has a component
;;;
(defun has-leaf-p (agg obj)
  (let (comps)
    (setf comps (g-local-value agg :components))
    (dolist (comp comps)
      (cond ((eq obj comp) (return t))
	    ((and (is-a-p comp opal:aggregate)
		  (has-leaf-p comp obj))
	     (return t))))))

;;;====================================================================
;;;
;;; search-for-inter -- find interactors that might change the list of
;;; selected objects. if no objects are selected, return all interactors
;;; that are not in the lapidary package
;;;
;;;====================================================================

(defun search-for-inter (&optional (selected-objs nil))
  (let (interactor-list)
    ;; if selected-objs is passed in, make sure it is a list; if it is
    ;; not passed in, make it be the objects the user has selected
    (setf selected-objs
	  (if selected-objs
	      (if (listp selected-objs)
		  selected-objs
		  (list selected-objs))
	      (g-value *selection-info* :selected)))

    (dolist (interactor (list lapidary-button-interactor
			      lapidary-menu-interactor
			      lapidary-angle-interactor
			      lapidary-text-interactor
			      directional-move-grow-interactor
			      lapidary-two-point-interactor))
	    (dolist (child (g-value interactor :is-a-inv))
		    (setf interactor-list 
			  (search-specific-inter child selected-objs
						 interactor-list))))
  interactor-list))

(defun search-specific-inter (inter selected-objs interactor-list)
  (let (where)
	(dolist (child (g-value inter :is-a-inv))
		(setf interactor-list (search-specific-inter child
					       selected-objs
					       interactor-list)))
	;; when there are no selected objects, all lapidary interactors
	;; should be returned so return the interactor
	(cond ((null selected-objs) (push inter interactor-list))
	      (t (setf where (g-value inter :start-where))
		 (cond ((eq t where)
			(push inter interactor-list))
		       ((null where));; won't match
		       ((listp where)
			(let ((control (first where))
			      (agg (second where))
			      slot lst)
			  (if (member agg selected-objs)
			      (push inter interactor-list)
			      (case control
				((:element-of :element-of-or-none)
				 (cond ((null agg))
				       ((not (schema-p agg)))
				       ((dolist (obj selected-objs)
						(if (has-component-p agg obj)
						    (return t)))
					(push inter interactor-list))))
				((:list-element-of :list-element-of-or-none)
				 (setf slot (third where))
				 (cond 
				  ((and agg (schema-p agg))
				   (setf lst (g-value agg slot))
				   (cond
				    ((and (listp lst) 
					  (dolist (i lst)
						  (if (member i selected-objs)
						      (return t))))
				     (push inter interactor-list))))))
				((:list-leaf-element-of
				  :list-check-leaf-but-return-element
				  :list-leaf-element-of-or-none
				  :list-check-leaf-but-return-element-or-none
				  :check-leaf-but-return-element
				  :check-leaf-but-return-element-or-none
				  )
				 (setf slot (third where))
				 (cond
				  ((and agg (schema-p agg))
				   (setf lst (g-value agg slot))
				   (cond
				    ((and (listp lst)
					  (dolist (i lst)
						  (if (dolist (obj selected-objs)
							      (if (has-leaf-p i obj)
								  (return t)))
						      (return t))))
				     (push inter interactor-list))))))
				((:leaf-element-of
				  :leaf-element-of-or-none)
				 (cond ((and agg (schema-p agg) 
					     (dolist (obj selected-objs)
						     (if (has-leaf-p agg obj) (return t))))
					(push inter interactor-list))))
				((:in-box :in :in-but-not-on)
				 (cond ((member agg selected-objs)
					(push inter interactor-list))))
				(t nil)))))
		       (t nil))
	 
		 ;; end of :start-where search, now look at :feedback-obj...
		 (cond ((member (g-value inter :feedback-obj) selected-objs)
			(push inter interactor-list)))
	    
		 ;; check :final-feedback as well...
		 (cond ((member (g-value inter :final-feedback-obj) selected-objs)
			(push inter interactor-list)))
	    
		 ;; check :obj-to-change as well...
		 (cond ((member (g-value inter :obj-to-change) selected-objs)
			(push inter interactor-list)))))

	interactor-list))
	
;;;====================================================================
;;;
;;; find the list of interactors that are passed in 
;;; and displayed in the interactors menu
;;;
;;;====================================================================

;;; convert a pointer to an interactor to a string name for the interactor
(defun inter-to-string (inter)
  (cond ((null inter) "")
	((stringp inter) inter)
;	((g-value inter :known-as) (symbol-name (g-value inter :known-as)))
	(t (name-for-schema inter))))

(defun show-interactor-menu (left top)
  (declare (special interactor-menu))
  (let ((inters (search-for-inter))
	(menu (g-value interactor-menu :aggregate)))
    (s-value menu :items (append interactors-list inters))
    (s-value menu :selected-ranks nil)
    (opal:notice-items-changed (g-value menu :menu-item-list))
    (opal:set-position interactor-menu left top)
    ;; make the menu visible
    (s-value interactor-menu :visible t)))

;;;====================================================================
;;;
;;; make the interactor menu invisible, make the appropriate interactor
;;; dialog box visible, and insert the appropriate information into
;;; this dialog box.
;;;
;;;====================================================================

(defun interactor-menu-final-fct (menu-inter obj-over)
  (declare (special interactor-menu))
  (declare (ignore menu-inter))

    ;; make the menu invisible
    (s-value interactor-menu :visible nil)

    ;; now instantiate the proper interactor menu and make it visible
    (init-inter-menu (g-value obj-over :item)))

;;; figure out which interactor menu to use and instantiate it with the
;;; information from inter

(defun init-inter-menu (inter)
  (declare (special choice-interactor-win move-grow-inter-win 
		    text-interactor-win two-point-inter-win
		    angle-inter-win))
  (reset-undo)
  (SetHourGlassCursor (list interactor-menu))
  (let ((dialog-box
	 (if (not (stringp inter))
	     (cond ((or (is-a-p inter lapidary-menu-interactor) 
			(is-a-p inter lapidary-button-interactor))
		    (init-choice-inter-menu inter)
		    choice-interactor-win)
		   ((is-a-p inter directional-move-grow-interactor) 
		    (init-move-grow-inter-menu inter)
		    move-grow-inter-win)
		   ((is-a-p inter lapidary-text-interactor) 
		    (init-text-inter-menu inter)
		    text-interactor-win)
		   ((is-a-p inter lapidary-two-point-interactor) 
		    (init-two-point-inter-menu inter)
		    two-point-inter-win)
		   ((is-a-p inter lapidary-angle-interactor) 
		    (init-angle-inter-menu inter)
		    angle-inter-win))
	      (cond
		((string= inter "choice")
		 (init-choice-inter-menu lapidary-menu-interactor)
		 (setf inter lapidary-menu-interactor)
		 choice-interactor-win)
		((string= inter "move/grow")
		 (init-move-grow-inter-menu 
		  lapidary:directional-move-grow-interactor)
		 (setf inter lapidary:directional-move-grow-interactor)
		 move-grow-inter-win)
		((string= inter "text")
		 (init-text-inter-menu lapidary-text-interactor)
		 (setf inter lapidary-text-interactor)
		 text-interactor-win)
		((string= inter "two-point")
		 (init-two-point-inter-menu lapidary-two-point-interactor)
		 (setf inter lapidary-two-point-interactor)
		 two-point-inter-win)
		((string= inter "angle")
		 (init-angle-inter-menu lapidary-angle-interactor)
		 (setf inter lapidary-angle-interactor)
		 angle-inter-win)))))

    (s-value dialog-box :visible t)
    (opal:update dialog-box)
    (RestoreRegularCursor (list interactor-menu))))

(defun interactor-menu-do-go ()
  (let ((menu (create-instance nil garnet-gadgets:scrolling-menu
		 (:left 0) (:top 0)
		 (:num-visible 6)
		 (:scroll-on-left-p nil)
		 (:items interactors-list)
		 (:item-to-string-function #'inter-to-string)
		 (:menu-selection-function #'interactor-menu-final-fct))))

    ;; destroy the interactor menu if it already exists
    (interactor-menu-do-stop)

    ;; create the menu window
    (create-instance 'interactor-menu inter:interactor-window
		     (:visible nil)
		     #+apple (:top 50)
		     (:width (o-formula (gvl :aggregate :width)))
		     (:height (o-formula (gvl :aggregate :height)))
		     (:aggregate menu)
		     (:double-buffered-p nil))))

(defun interactor-menu-do-stop ()
  (when (and (boundp 'interactor-menu) interactor-menu)
    (opal:destroy interactor-menu))
  (text-inter-do-stop)
  (choice-inter-do-stop)
  (angle-inter-do-stop)
  (move-grow-inter-do-stop)
  (two-point-inter-do-stop))
