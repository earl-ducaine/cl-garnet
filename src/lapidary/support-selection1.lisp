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
;;; This file contains support code for "selection.lisp", which handles
;;; selections in the graphical editor. 
;;;
;;; It also contains some code which is used by the constraint menu.
;;;
;;; ABOUT SELECTION:
;;;  Selection is indicated by the presence of feedback objects: either
;;; :p-feedback-obj or :s-feedback-obj.  These slots will be nil when there
;;; is no selection.  In addition, all selected objects are listed in the
;;; :p-selected and :s-selected slots of the corresponding editor-agg:
;;; "x is p-selected" is true iff
;;;     (member x (g-value x :window :editor-agg :p-selected))
;;;
;;; use (is-p-selected x) and (is-s-selected x) to test for selection

;;; CHANGE LOG
;;;
;;; 08/10/93 bvz - modified the selection routines so that
;;;                  they could support either the selection of leaves or
;;;                  top-level objects.

(in-package "LAPIDARY")

(defvar *test-debug* NIL)

;;; =================================================================
;;; return a feedback object of the specified type. first determine
;;; if a feedback object of the given type already exists and return
;;; it if possible; otherwise create a new feedback object
;;; =================================================================

(defun get-feedback-obj (feedback-list feedback-type
				       &optional filling-style
				       &key (size 7))
  (let ((feedback-obj (if (null feedback-list)
			  (if (null filling-style)
			      (create-instance nil feedback-type)
			      (make-grow-feedback-obj filling-style 
						      feedback-type
						      :size size))
			  (pop feedback-list))))
    ;; sometimes a destroyed feedback object will be left on the
    ;; feedback list
    (when (not (schema-p feedback-obj))
	  (loop
	   (setf feedback-obj 
		 (if (null feedback-list)
		     (if (null filling-style)
			 (create-instance nil feedback-type)
			 (make-grow-feedback-obj filling-style 
						 feedback-type
						 :size size))
		     (pop feedback-list)))
	   (when (schema-p feedback-obj)
		 (return))))
    (values feedback-obj feedback-list)))

;;; =================================================================
;;; decide what type of feedback object is required (line, aggregate,
;;; leaf, or undersized). If the well of appropriate feedback objects 
;;; has been exhausted, allocate a new feedback object and add it to 
;;; the feedback aggregate; otherwise, get a feedback object from the 
;;; well.
;;; =================================================================

(defun select-feedback-obj (obj filling-style)
  (declare (special *undersized-feedback-list* *line-feedback-list*
		    *leaf-feedback-list* *agg-feedback-list*))
  (let (feedback-obj)
    ;; first determine if the object is undersized. an undersized aggregate 
    ;; is one whose width or height is less than
    ;; ((2 * size of aggregate feedback selection circle) + 4) pixels
    ;; and an undersized graphics object is one whose width or height is
    ;; less than
    ;; ((2 * size of graphics feedback selection box) + 4) pixels
    (loop
     (cond ((cond ((is-a-line-p obj)
		   (< (sqrt 
		       (+ (expt (- (g-value obj :y2) (g-value obj :y1)) 2)
			  (expt (- (g-value obj :x2) (g-value obj :x1)) 2)))
		      *min-leaf-size*))
		  ((is-a-p obj opal:aggregate)
		   (or (< (g-value obj :width) *min-agg-size*)
		       (< (g-value obj :height) *min-agg-size*)))
		  (t
		   (or (< (g-value obj :width) *min-leaf-size*)
		       (< (g-value obj :height) *min-leaf-size*))))
	    (multiple-value-setq (feedback-obj *undersized-feedback-list*)
				 (get-feedback-obj *undersized-feedback-list*
						   undersized-feedback-obj)))
	   ((is-a-line-p obj)
	    (multiple-value-setq (feedback-obj *line-feedback-list*)
				 (get-feedback-obj *line-feedback-list*
						   line-selection-boxes)))
	   ((is-a-p obj opal:aggregate)
	    (multiple-value-setq (feedback-obj *agg-feedback-list*)
	      (get-feedback-obj *agg-feedback-list* opal:circle 
				filling-style :size *agg-sel-circle-size*)))
	   (t 
	    (multiple-value-setq (feedback-obj *leaf-feedback-list*)
	        (get-feedback-obj *leaf-feedback-list* opal:rectangle
		 	      filling-style))))

      ;; ensure we have a valid feedback object. Invalid feedback objects
      ;; may be left around when windows are destroyed
      (when (and feedback-obj (kr::schema-name feedback-obj))
	(return)))
    
    ;; set the feedback object's filling style
    (s-value feedback-obj :filling-style filling-style)

    ;; feedback objects can move between windows and right now the
    ;; feedback object could be attached to the wrong window. If it
    ;; is, move it to the correct window
    (when (not (eq (g-value obj :window)
		   (g-value feedback-obj :window)))
      (let ((parent (get-local-value feedback-obj :parent)))
	(when parent (opal:remove-component parent feedback-obj)))
      (opal:add-component (g-value obj :window :feedback-agg) feedback-obj))
    
    ; set the feedback's obj-over slot so it will orient itself correctly
    (s-value feedback-obj :obj-over obj)
    
    ;; add the feedback object to the feedback object list kept
    ;; in *selection-info*
    (s-value *selection-info* :feedback 
	     (push feedback-obj (g-value *selection-info* :feedback)))
    feedback-obj))

;;; =================================================================
;;; return a feedback object to its feedback list and make it 
;;; invisible by setting its :obj-over slot to nil
;;; =================================================================

(defun return-feedback-to-list (obj feedback-obj)
  (declare (special *undersized-feedback-list* *line-feedback-list*
		    *leaf-feedback-list* *agg-feedback-list* *selection-info*))
  (cond ((is-a-p feedback-obj line-selection-boxes)
	 (push feedback-obj *line-feedback-list*))
	((is-a-p feedback-obj undersized-feedback-obj)
	 (push feedback-obj *undersized-feedback-list*))
	((is-a-p obj opal:aggregate)
	 (push feedback-obj *agg-feedback-list*))
	(t 
	 (push feedback-obj *leaf-feedback-list*)))
  (s-value feedback-obj :obj-over nil)
  ;; remove the feedback object from the feedback object list kept
  ;; in *selection-info*
  (s-value *selection-info* :feedback 
	   (delete feedback-obj (g-value *selection-info* :feedback))))

;;; ==========================================
;;; this function creates a feedback object
;;; with eight "grow" boxes
;;; ========================================

(defun make-grow-feedback-obj (filling-style obj-type &key (size 7))

  (let (main obj
	(sizeD2 (floor size 2)))

    (setq main
	  (kr:create-instance nil opal:aggregate
	      (:name "Top level selection aggregate")
	      (:left (o-formula (- (gvl :obj-over :left) sizeD2) 0))
	      (:top (o-formula (- (gvl :obj-over :top) sizeD2) 0))
	      (:width (o-formula (if (is-a-p (gvl :obj-over) opal:circle)
				     (+ (gvl :min-width-height) size)
				     (+ (gvl :obj-over :width) size))
				 0))
	      (:height (o-formula (if (is-a-p (gvl :obj-over) opal:circle)
				      (+ (gvl :min-width-height) size)
				      (+ (gvl :obj-over :height) size))
				  0))
	      (:visible (o-formula (gvl :obj-over)))
	      (:obj-over NIL)
	      (:min-width-height (o-formula (min (gvl :obj-over :width)
						 (gvl :obj-over :height))))
;	      (:fast-redraw-p t)
	      (:draw-function :xor)
	      (:filling-style filling-style)
	      (:line-style (o-formula (if (eql (gvl :filling-style) opal:black-fill)
					  nil
					  opal:thin-line)))))

    (do ((x '(l m r r r m l l) (cdr x))
	 (y '(t t t m b b b m) (cdr y))
	 (attach '(:nw :n :ne :e :se :s :sw :w) (cdr attach)))
	((null x)) ; end test
      (setq obj
	    (kr:create-instance nil obj-type
		  (:name "Selection box")
	          (:left (case (car x)
			   (l (o-formula (gv main :left)))
			   (m (o-formula (opal:gv-center-x-is-center-of main)))
			   (r (o-formula (- (opal:gv-right main) size)))))
	          (:top (case (car y)
			  ((t) (o-formula (gv main :top)))
			  (m (o-formula (opal:gv-center-y-is-center-of main)))
			  (b (o-formula (- (opal:gv-bottom main) size)))))
		  (:width size) (:height size)
		  (:draw-function :xor)
		  (:fast-redraw-p t)
		  (:where-attach (car attach))
		  (:filling-style (o-formula (gvl :parent :filling-style)))
		  (:line-style (o-formula (gvl :parent :line-style)))))
      (opal:add-components main obj))
    main))

;;; ============================================
;;; deselect the selected objects on the display
;;; ============================================

(defun deselect-objects (&optional (window nil))
    
  (primary-deselect-objects :none window)
  (secondary-deselect-objects :none window))

;;; ============================================
;;; deselect the designated primary selection
;;; object. if no object is designated, deselect
;;; everything
;;; ============================================

(defun primary-deselect-objects (selection &optional (window nil))
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null selection)
	(return-from primary-deselect-objects))
  (let ((selection-list 
	  (if (eql selection :none)
	      (if window
		  (g-value window :editor-agg :p-selected)
		  (g-value *selection-info* :p-selected))
	      (g-value selection :window :editor-agg :p-selected)))
    obj)

    (cond ((eq selection :none)
	   (dolist (item selection-list)
	     ; return the object's feedback obj to the feedback list
	     (primary-deselect item)))
	  
	   ; otherwise only one object should be deselected -- see if we
	   ; can find it:
	   
	  ((setf obj (leaf-to-selection selection :p-feedback-obj 
					(g-value selection :window :editor-agg)))
	   ; return the object's feedback obj to the feedback list
	   (primary-deselect obj))
	  (t
	   nil))))


;;; ============================================
;;; deselect the designated secondary selection
;;; object. if no object is designated, deselect
;;; everything
;;; ============================================

(defun secondary-deselect-objects (selection &optional (window nil))
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null selection)
	(return-from secondary-deselect-objects))
  (let ((selection-list 
	  (if (eql selection :none)
	      (if window
		  (g-value window :editor-agg :s-selected)
		  (g-value *selection-info* :s-selected))
	      (g-value selection :window :editor-agg :s-selected)))
	obj)

    (cond ((eql selection :none) 
	   (setf selection-list (g-value *selection-info* :s-selected)))
	  (t (setf selection-list 
		   (g-value selection :window :editor-agg :s-selected))))

    (cond ((eql selection :none)
	   (dolist (item selection-list)
	     ; return the object's feedback obj to the feedback list
	     (secondary-deselect item)))
	  
	  ; otherwise only one object should be deselected -- see if we
	  ; can find it
	   
	  ((setf obj (leaf-to-selection selection :s-feedback-obj 
					(g-value selection :window :editor-agg)))
	   ; return the object's feedback obj to the feedback list
	   (secondary-deselect obj))
	  (t
	   nil))))



;; starting from a leaf object, go up the parent links until you find
;; a selected object
;;  sel-type is either :p-feedback-obj :s-feedback-obj
;;  top is the editor window's top-level aggregate
(defun leaf-to-selection (leaf sel-type top)
  (cond ((eq leaf top) nil)
	((get-local-value leaf sel-type) leaf)
	(t
	 (leaf-to-selection (get-local-value leaf :parent) sel-type top))))


;; primary-deselect removes a primary feedback object from an object
;;
(defun primary-deselect (obj)
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null obj)
	(return-from primary-deselect))
  (let ((feedback-obj (get-local-value obj :p-feedback-obj))
	(editor-agg (g-value obj :window :editor-agg)))
    (return-feedback-to-list obj feedback-obj)
    (s-value obj :p-feedback-obj nil)
    (s-value editor-agg :p-selected
	     (delete obj (g-value editor-agg :p-selected)))
    (s-value *selection-info* :p-selected
	     (delete obj (g-value *selection-info* :p-selected)))  ))


;; secondary-deselect removes a secondary feedback object from an object
;;
(defun secondary-deselect (obj)
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null obj)
	(return-from secondary-deselect))
  (let ((feedback-obj (get-local-value obj :s-feedback-obj))
	(editor-agg (g-value obj :window :editor-agg)))
    (return-feedback-to-list obj feedback-obj)
    (s-value obj :s-feedback-obj nil)
    (s-value editor-agg :s-selected
	     (delete obj (g-value editor-agg :s-selected)))
    (s-value *selection-info* :s-selected
	     (delete obj (g-value *selection-info* :s-selected)))  ))


;; primary-select installs a primary feedback object on an object
(defun primary-select (obj)
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null obj)
	(return-from primary-select))
  (let ((feedback-obj (select-feedback-obj obj opal:black-fill)))
    (s-value obj :p-feedback-obj feedback-obj)
    (push obj (g-value (g-value obj :window :editor-agg) :p-selected))
    (push obj (g-value *selection-info* :p-selected))))
    
	
;; secondary-select installs a secondary feedback object on an object
(defun secondary-select (obj)
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null obj)
	(return-from secondary-select))
  (let ((feedback-obj (select-feedback-obj obj opal:white-fill)))
    (s-value obj :s-feedback-obj feedback-obj)
    (push obj (g-value (g-value obj :window :editor-agg) :s-selected))
    (push obj (g-value *selection-info* :s-selected))))
    

;;; ===============================================
;;; this function makes the next selection
;;; be the parent of the currently selected item.
;;; If the next selection would be the editor
;;; aggregate, then cycle back and select the
;;; provided leaf object.
;;; ===============================================

(defun toggle-selection (current-selection leaf-obj)
  (let* ((parent (get-local-value current-selection :parent))
	 (window (get-local-value current-selection :window))
	 (editor-agg (g-value window :editor-agg))
	 (next-selection (if (equal editor-agg parent)
			     leaf-obj
			     parent)))

    ; remove the current selection from the list and change the slots:
    (primary-deselect current-selection)

    ; select the object pointed at by next-selection if it is not already
    ; selected
    (cond ((not (is-p-selected next-selection))
	   (primary-select next-selection)))))

;;; =======================================================================



;;; ===============================================
;;; this function makes the next selection
;;; be the parent of the currently selected item.
;;; ===============================================

(defun toggle-secondary-selection (current-selection leaf-obj)
  (let* ((parent (get-local-value current-selection :parent))
	 (window (get-local-value current-selection :window))
	 (editor-agg (g-value window :editor-agg))
	 (next-selection (if (equal editor-agg parent)
				 leaf-obj
				 parent)))

    ; remove current selection:
    (secondary-deselect current-selection)

    ; select the object pointed at by next-selection if it is not already
    ; selected
    (cond ((not (is-s-selected next-selection))
	   (secondary-select next-selection)))))


;;; ===============================================
;;; primarily select an object. if the object or an 
;;; ancestor has been selected, cycle through the
;;; aggregate hierarchy; otherwise allocate a
;;; feedback object and tie it to the selected
;;; object.  When cycling, make sure the new
;;; selection is not already selected! If
;;; requested, deselect any other primary
;;; selections
;;; ===============================================

(defun make-p-selection (object &optional (deselect-p nil))
  (declare (special *selection-info*))
  ;; safety check--should never be called with a null object but it happens
  (when (null object)
	(return-from make-p-selection))
  (let ((current-selection
	 (leaf-to-selection object :p-feedback-obj
			    (g-value object :window :editor-agg))))
    (cond (current-selection
	   ; if the object has already been selected, toggle
	   ; the selection
	   (toggle-selection current-selection object))
	  (t
	   ; the object has not been selected;
	   ; if requested, deselect other primary selections
	   (when deselect-p
	     (primary-deselect-objects :none))

	   ; set the object up with a feedback object--determine whether
	   ; the object should be selected or whether the object's top-level
	   ; aggregate should be selected
	   (if (g-value *selection-info* :leaf)
	       (primary-select object)
	       (primary-select (get-root object)))))

    ; cause the constraint, line, filling, and
    ; object menus to be updated
;    (opal:update-all)
))



;;; ===============================================
;;; secondarily select an object. if the object has
;;; already been selected, cycle through the
;;; aggregate hierarchy; otherwise allocate a
;;; feedback object and tie it to the selected
;;; object. If requested, deselect any other
;;; secondary selections.
;;; ===============================================

(defun make-s-selection (object &optional (deselect-p nil))
  ;; safety check--should never be called with a null object but it happens
  (when (null object)
	(return-from make-s-selection))
  (let ((current-selection
	 (leaf-to-selection object :s-feedback-obj
			    (g-value object :window :editor-agg))))
    (cond (current-selection
	   ; if the object has already been selected, toggle
	   ; the selection
	   (toggle-secondary-selection current-selection object))
	  (t
	   ; the object has not been selected;
	   ; if requested, deselect other secondary selections
	   (when deselect-p
	     (secondary-deselect-objects :none))

	   ; set the object up with a feedback object--determine whether
	   ; the object should be selected or whether the object's top-level
	   ; aggregate should be selected
	   (if (g-value *selection-info* :leaf)
	       (secondary-select object)
	       (secondary-select (get-root object)))))

    ; cause the constraint, line, filling, and
    ; object menus to be updated
;    (opal:update-all)
))

