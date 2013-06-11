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
;;; This file contains miscellaneous support code for Lapidary,
;;; not specific to any other Lapidary file.  In particular it
;;; contains various GLOBALS as well as various functions...
;;;
;;; Globals:
;;;    *fnt*           -- a font object used throughout Lapidary
;;;    *vp-constraint-menu* -- window for constraints.  Declared here to give
;;;                       access to it before its "do-go" is executed.
;;;
;;; Support Functions:
;;;    classify-selections -- returns (Nil, 'zero-one, 'one-one, 'many-one,
;;;                       or 'one-many) depending on 1-ary and 2-ary selections
;;;    lapidary-beeps  -- sends beeps to the display
;;;    center-x        -- returns a constraint (formula) that
;;;                       centers the X posn of an object with respect
;;;                       to another object
;;;    center-y        -- same, but for the Y position
;;;    remember-interactor -- takes a window and an interactor; adds the
;;;                       interactor to the :my-interactors slot for
;;;                       bookkeeping purposes.
;;;    set-fast-store  -- takes an argument, T or NIL, and sets the backing
;;;                       store of every element of *lapidary-subwindows* to
;;;                       :always on T, or :not-useful on NIL.  A value of T
;;;                       means that the window will not generate an exposure
;;;                       event after iconifying, but the price, of course, is
;;;                       the memory of storing the window.  Actually, now
;;;                       takes a second OPTIONAL argument which is a list of
;;;                       the windows to be set.
;;;    create-lapidary-window -- this creates an interactor window, but it
;;;                       also does some lapidary bookkeeping, so it should
;;;                       be used.
;;;
;;;    make-lapidary-feedback-objs -- this creates the feedback objects
;;;                       that provide feedback when the user creates
;;;                       an object
;;;
;;;    lapidary-error -- print an error message in a window
;;;
;;;    lapidary-prompt-for-input -- prompt the user for input and return
;;;                       the input as a string
;;;
;;;    name-lapidary-obj -- provide a name for a newly created object

;;; Implemented by DSK and BVZ

(in-package "LAPIDARY")

;;; =================================================================
;;;
;;; CHANGE LOG
;;;
;;; 8/7/89 -- added a function to print error messages in a window
;;; =================================================================

;;; =======================================================================

(defvar *vp-constraint-menu* nil)

;;; ============================================================
;;; classify the selections on the screen according to the
;;; number of primary and secondary selections that have
;;; been made
;;; ============================================================

(defun classify-selections ()
  (declare (special *selection-info*))
  (let ((p-size (list-length (gv *selection-info* :p-selected)))
	(s-size (list-length (gv *selection-info* :s-selected))))
    (case p-size
      (0 (case s-size
	   (0 nil)
	   (1 'zero-one)
	   (t 'zero-many)))
      (1 (case s-size
	   (0 'one-zero)
	   (1 'one-one)
	   (t 'one-many)))
      (t (case s-size
	   (0 'many-zero)
	   (1 'many-one)
	   (t 'many-many))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputs BEEPS to the
;; lapidary display
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lapidary-beeps (count)
  (dotimes (dummy-var count)
    (inter:beep)))

;;; ==================================
;;; constraint that centers the x
;;; position of an object with respect
;;; to another object
;;; ==================================

(defun center-x (item1 item2)
  (o-formula (round (- (+ (gv item1 :left)
			     (/ (gv item1 :width) 2))
			  (/ (gv item2 :width) 2)))))

;;; ==================================
;;; constraint that centers the y
;;; position of an object with respect
;;; to another object
;;; ==================================

(defun center-y (item1 item2)
  (o-formula (round (- (+ (gv item1 :top)
			     (/ (gv item1 :height) 2))
			  (/ (gv item2 :height) 2)))))

;;; ================================
;;; remember interactor in :my-interactors slot
;;; ================================

(defun remember-interactor (window inter)
  (s-value window :my-interactors
	      (cons inter (g-value window :my-interactors))))

;;; ==================================
;;; this creates an interactor window, but it
;;; also does some lapidary bookkeeping,
;;; so it should be used. 
;;; ==================================

(defun create-lapidary-window (&key (left 0) (top 658)
				(width 450) (height 380) title)
  (let ((temp-window (kr:create-instance nil inter:interactor-window
				(:left left)
				(:top top)
				(:width width)
				(:height height)
				(:title title)
				(:visible t))))
    (s-value temp-window :known-as
	     (keyword-from-string title))
    temp-window)
)

;;; =====================================================
;;; create feedback objects that support the various
;;; operations in Lapidary
;;; =====================================================

(defun install-new-editor-window (window)
  (let (editor-win-agg editor-agg feedback-agg feedback
        (kr::*constants-disabled* t))
  
    ;;; create the top level aggregate in the editor window
    (setf editor-win-agg (kr:create-instance nil opal:aggregate))
    (s-value window :aggregate editor-win-agg)
  
    ;;; create the feedback aggregate
    (setf feedback-agg (kr:create-instance nil opal:aggregate
	   (:left 0)
	   (:top 0)
	   (:width (o-formula (gvl :window :width)))
	   (:height (o-formula (gvl :window :height)))
			))
    (opal:add-component editor-win-agg feedback-agg)
    (s-value window :feedback-agg feedback-agg)

    ;;; create the aggregate that contains the editable, selectable objects
    (setq editor-agg
	  (kr:create-instance nil opal:aggregadget 
	   (:left 0)
	   (:top 0)
	   (:width (o-formula (gvl :window :width)))
	   (:height (o-formula (gvl :window :height)))
	   (:selection-type (o-formula (classify-selections)))))
    
    (opal:add-component editor-win-agg editor-agg :where :back)
    (s-value window :editor-agg editor-agg)

    ;; add this window to the list of lapidary drawing windows
    (push window (g-value *selection-info* :window))

    (setf feedback 
	  (opal:add-component feedback-agg
			      (create-instance nil rectangle-feedback)))
    (s-value window :create-rect-feedback feedback)

    (setf feedback 
	  (opal:add-component feedback-agg
			      (create-instance nil line-feedback)))
    (s-value window :create-line-feedback feedback)

    (setf feedback 
	  (opal:add-component feedback-agg
				       (create-instance nil roundtangle-feedback)))
    (s-value window :create-roundtangle-feedback feedback)
	  
    (setf feedback 
	  (opal:add-component feedback-agg
			      (create-instance nil circle-feedback)))
    (s-value window :create-circle-feedback feedback)

    (setf feedback 
	  (opal:add-component feedback-agg
			      (create-instance nil text-feedback)))
    (s-value window :create-text-feedback feedback)

    (setf feedback
	  (opal:add-component feedback-agg
	      (create-instance nil aggrelist-feedback
		  (:item-prototype (create-instance nil dashed-rectangle))
		  (:items 1))))

    (s-value window :aggrelist-feedback feedback)

    ;;; create the feedback object for selecting a group of objects

    (setf feedback 
	  (opal:add-component feedback-agg
			      (create-instance nil rectangle-feedback)))
    (s-value window :selection-feedback feedback)

    
    ;;; create the feedback objects for moving or growing an object
    (s-value window :move-box-feedback (create-instance nil move-box-feedback))
    (s-value window :grow-box-feedback (create-instance nil grow-box-feedback))
    (s-value window :move-grow-line-feedback 
	     (create-instance nil move-grow-line-feedback))
    (opal:add-components feedback-agg
			 (g-value window :move-box-feedback)
			 (g-value window :grow-box-feedback)
			 (g-value window :move-grow-line-feedback))

    ;;; create the feedback object for creating an instance or copy of an object

    (setf feedback 
	  (opal:add-component feedback-agg
			      (create-instance nil move-box-feedback)))
    (s-value window :copy-instance-rect-feedback feedback)
    (setf feedback
	  (opal:add-component feedback-agg
			      (create-instance nil line-feedback
				 (:visible (o-formula (gvl :obj-over))))))
    (s-value window :copy-instance-line-feedback feedback)))

(defun lapidary-error (msg &optional (wait-p t))
  (if wait-p
      (garnet-gadgets:display-error-and-wait *lapidary-error-window* msg)
      (garnet-gadgets:display-error *lapidary-error-window* msg)))

(defun lapidary-prompt-for-input (prompt)
  (lapidary-beeps 1)
  (format t "~% ~A" prompt)
  (force-output)
  (read-line))

;; generate a name for a new object and set its :known-as slot

(defun name-lapidary-obj (obj)
  (let ((name (name-for-schema obj)))
    (setf (kr::schema-name obj) (read-from-string name))
    (set (kr::schema-name obj) obj)
    (s-value obj :known-as (keyword-from-string name))))

;;; ============================================================
;;; determine if the two objects have a common ancestor in the
;;; aggregate hierarchy. A simple way of doing this is to go
;;; to the tops of each of their aggregate hierarchies and 
;;; comparing the respective roots. Although a marking strategy
;;; could save us from going all the way to the top of the
;;; hierarchies, we would have to retract all the marks at
;;; the end, which would complicate the function. Also the
;;; hierarchies are likely to be shallow so marking probably
;;; would not save any time
;;; ============================================================


(defun get-root (obj)
  (let ((parent (or (g-value obj :parent)
		    (g-value obj :operates-on))))
    (if (is-a-p obj inter:interactor)
	(if parent
	    (get-root parent)
	    obj)
        (if (eq (g-value obj :parent)
		(g-value obj :window :editor-agg))
	    (return-from get-root obj)
	    (get-root parent)))))

(defun common-ancestor-p (obj1 obj2)
  (and obj1 obj2
       (eq (get-root obj1) (get-root obj2))))

