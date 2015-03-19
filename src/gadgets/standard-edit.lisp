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
;;;   Some standard editing functions used by multiple programs
;;;   Assuming: single window, use of Multi-Graphics-Selection
;;;
;;;   Each of these functions is designed to be called from a gadget.
;;;   Slots of that gadget:
;;;     :sel-gadget - the Multi-Graphics-Selection object
;;;     :agg-of-items - the aggregate that holds the items to be manipulated
;;;     :clipboard - contains a KR schema which has a slot called
;;;                  :value which holds the cut or copied objects.
;;;                  This indirection allows multiple different
;;;                  gadgets to be used for different commands.
;;;  The Standard-initialize-gadget sets these fields appropriately.
;;;
;;;  Implemented by Brad Myers based on code in GarnetDraw
;;;
;;;  *** EVENTUALLY:
;;;       ** SUPPORT UNDO
;;;       ** SUPPORT MULTIPLE WINDOWS
;;;
;;; When the selection gadget has a report-function, it is called for
;;; each operation here as follows:
;;;         (lambda (gadget main-op parameter obj oldval newval)
;;; where main-op is one of
;;;  :delete :delete-all :to-bottom :to-top :duplicate :copy :cut
;;;  :select-all :refresh :paste :group :ungroup
;;; parameter is not used, obj is the object modified, and oldval and
;;; newval aren't usually used.  For duplicate, newval
;;; will be the new objects, for group it will be the group object,
;;; for ungroup, it will be the list of sub-objects.
#|
==================================================================
Change log:
    6/23/93  Brad Myers - removed basic-moving-group, just use aggregadgets,
                        - default is one global clipboard for all applications
    4/23/93  Andrew Mickish - Added Is-A-Motif-Rect
     3/7/93  Brad Myers - made so could be used by Gilt.
                        - allow undoing of (only) delete and delete-all
    2/26/93  Rich McDaniel - Fixed report function bug.
    1/15/93  Brad Myers - Fixed bugs with copy/paste
    1/10/93  Brad Myers - distributed, fixed so no lazy copying into clipboard
    ?/??/92  Brad Myers - created based on garnetdraw code
==================================================================
|#

(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Standard-Delete Standard-Delete-All Standard-Undo-Last-Delete
	    Standard-To-Bottom Standard-To-Top
	    Standard-Duplicate Standard-Copy Standard-Cut Standard-Select-All
	    Standard-Refresh 
	    Standard-Paste-Same-Place Standard-Paste-Inc-Place
	    Standard-Initialize-Gadget Standard-NIY
	    Standard-Group Standard-UnGroup
				     
	    Clipboard-Object 
	    sort-objs-display-order Is-A-Motif-Background Is-A-Motif-Rect)))

(defparameter Default-Duplicate-Inc-Amt 10)

(create-instance 'Clipboard-Object NIL
  (:value NIL)
  (:paste-cnt 0) ; to allow multiple pastes to go in different places
  (:x-inc-amt NIL)  ; For duplicate.  If NIL, then Default-Duplicate-Inc-Amt
  (:y-inc-amt NIL))

;;; The clipboard used by all applications if they do not provide one,
;;; to allow multiple applications to cut/copy/paste among them.
(create-instance 'Default-Global-Clipboard Clipboard-Object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Utility internal functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun Report-If-Wanted (gadget operation obj &optional new)
  (let ((want-report (g-value gadget :sel-gadget :report-function)))
    (when want-report
      (funcall want-report gadget operation NIL obj NIL new))))

;; Returns a COPY of the list objs sorted as in the order of
;; draw-agg's components.  First item in returned list will be on
;; BOTTOM, last will be on TOP.
(defun sort-objs-display-order (objs draw-agg)
  (if (<= (length objs) 1)  ;; no need for expensive sort if 1 or 0 objs
      (copy-list objs)
      (let ((sorted-list (copy-list objs))
	    (reference-objs (g-value draw-agg :components)))
	;; sort is destructive, so copied list first
	(sort sorted-list #'(lambda (o1 o2)
			      (< (position o1 reference-objs)
				 (position o2 reference-objs)))))))

;;; Always call this before putting new things into the clipboard.
(defun Delete-Clipboard-Objs (clipboard-obj)
  (with-constants-disabled
      (dolist (obj (g-value clipboard-obj :value))
	(opal:destroy obj)))
  (s-value clipboard-obj :value NIL))

;;; Add copy of new objects to clipboard's list
(defun Add-Clipboard-Objs (clipboard-obj new-objs)
  (let (new-list)
    (dolist (o new-objs)
      (with-constants-disabled
	  (push (opal:copy-gadget o NIL) new-list)))
    (s-value clipboard-obj :value (nreverse new-list))
    (s-value clipboard-obj :paste-cnt 0)))

;;; Returns a copy of the objects in the clipboard for use on the screen.
(defun Return-Clipboard-Objs (clipboard-obj)
  (let ((vals (g-value clipboard-obj :value))
	ret)
    (dolist (obj vals)
      (with-constants-disabled
	  (push (opal:copy-gadget obj NIL) ret)))
    (nreverse ret)))

;;; If the object has a :box, :points or :point-list value, make a
;;; copy of the value.  Call this on the copied object after a duplicate.
;;; Will optionally move the objects.
(defun fix-box-points (obj x-offset y-offset)
  (cond ((g-value obj :line-p)
	 (let ((old (g-value obj :points)))
	   (s-value obj :points (list
				 (+ (first old) x-offset)
				 (+ (second old) y-offset)
				 (+ (third old) x-offset)
				 (+ (fourth old) y-offset)))))

	((g-value obj :polygon-p)
	 (let ((old (g-value obj :point-list)))
	   ;; calculate-new-pointlist destructively modifies the point-list
	   ;; calculate-new-pointlist is defined in multi-selection.lisp
	   (s-value obj :point-list (calculate-new-pointlist
				     (copy-list old)
				     0 x-offset
				     0 y-offset
				     1 1))))

	((g-value obj :group-p) (adjust-all-items-of-group-size
				 obj
				 (List (+ x-offset (g-value obj :left))
				       (+ y-offset (g-value obj :top))
				       (g-value obj :width)
				       (g-value obj :height))
				 (g-value obj :left)
				 (g-value obj :top)
				 (g-value obj :width)
				 (g-value obj :height)
				 T T NIL))
	(T ; treat as a box
	 (let ((old (g-value obj :box)))
	   (s-value obj :box (list
				 (+ (first old) x-offset)
				 (+ (second old) y-offset)
				 (third old)
				 (fourth old)))))))

(defun Is-A-Motif-Background (obj)
  (and (boundp 'garnet-gadgets::Motif-Background)
       (is-a-p obj garnet-gadgets::Motif-Background)))

(defun Is-A-Motif-Rect (obj)
  (and (boundp 'garnet-gadgets::Motif-Rect)
       (is-a-p obj garnet-gadgets::Motif-Rect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Standard-XXX functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Gadget should be the gadget from which all the commands will be
;;; called (e.g., a menubar).  sel-gadget is the multi-selection gadget.
;;; Agg-of-items is the aggregate that holds the created object.
(defun Standard-Initialize-Gadget (gadget sel-gadget agg-of-items
					  &key clipboard undo-delete?)
  (unless sel-gadget
    (error "Must supply selection gadget for ~s" gadget))
  (unless agg-of-items
    (error "Must supply aggregate for items for ~s" gadget))
  (s-value gadget :sel-gadget sel-gadget)
  (s-value gadget :agg-of-items agg-of-items)
  (s-value gadget :clipboard (or clipboard Default-Global-Clipboard))
  (s-value gadget :undo-delete? undo-delete?)
  (when undo-delete?
    (s-value gadget :undo-delete-list NIL)))


;; Not-Implemented-Yet
(defun Standard-NIY (gadget &rest args)
  (declare (ignore gadget args))
  (format T "Sorry, Not Implemented Yet~%")
  (inter:beep))


;;; Delete all objects
(defun Standard-Delete-All (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (copy-of-objs (copy-list (g-value agg-of-items :components))))
    (with-constants-disabled
	(dolist (obj copy-of-objs)
	  (opal:remove-component agg-of-items obj)))
    (gg:set-selection sel-gadget NIL)
    (Report-If-Wanted gadget :delete-all copy-of-objs NIL)
    (if (g-value gadget :undo-delete?)
	(push copy-of-objs (g-value gadget :undo-delete-list))
	;; else just destroy the objects
	(dolist (obj copy-of-objs)
	  (opal:destroy obj)))))

;;; Delete the selected object (Macintosh "clear")
(defun Standard-Delete (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (copy-of-sel (sort-objs-display-order (g-value sel-gadget :value)
					       agg-of-items)))

    (Report-If-Wanted gadget :delete copy-of-sel NIL)
    (gg:set-selection sel-gadget NIL)
    (with-constants-disabled
	(dolist (obj copy-of-sel)
	  (opal:remove-component agg-of-items obj)))
    (if (g-value gadget :undo-delete?)
	(push copy-of-sel (g-value gadget :undo-delete-list))
	;; else just destroy the objects
	(dolist (obj copy-of-sel)
	  (opal:destroy obj)))))

;;; Undo Last Delete 
(defun Standard-Undo-Last-Delete (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let ((sel-gadget (g-value gadget :sel-gadget))
	(agg-of-items (g-value gadget :agg-of-items))
	(objs (pop (g-value gadget :undo-delete-list))))
    (if (g-value gadget :undo-delete?)
	(if objs
	    (progn
	      (dolist (o objs)
		(with-constants-disabled
		    (opal:add-component agg-of-items o
					(when (is-a-motif-background o)
					  :back))))
	      (Garnet-Gadgets:Set-Selection sel-gadget objs))
	    ;; no obj, so beep
	    (inter:beep))
	;; else undo-ing not allowed
	(error "Undo-Last-Delete, but initialized as not supported"))))


;;; Move the selected objects to the top
(defun Standard-To-Top (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (g-value sel-gadget :value))
	 (sorted-objs (sort-objs-display-order objs agg-of-items)))
    (Report-If-Wanted gadget :to-top objs NIL)
    (with-constants-disabled
	(when objs
	  (dolist (obj sorted-objs)
	    (unless (is-a-motif-background obj)
	      (opal:move-component agg-of-items obj :where :front)))))))

;;; Move the selected objects to the bottom
(defun Standard-To-Bottom (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (g-value sel-gadget :value))
	 (first-obj (first (g-value agg-of-items :components)))
	 (background-p (is-a-motif-background first-obj)))
    (Report-If-Wanted gadget :to-bottom objs NIL)
    (when background-p ; make sure don't move the background
      (setq objs (delete first-obj objs)))
    (when objs
      (with-constants-disabled
	(dolist (obj (nreverse (sort-objs-display-order objs
				agg-of-items)))
	  (if background-p
	      (opal:move-component agg-of-items obj :in-front first-obj)
	      (opal:move-component agg-of-items obj :where :back)))))))

;;; Refresh the main window
(defun Standard-Refresh (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let ((agg-of-items (g-value gadget :agg-of-items)))
    (Report-If-Wanted gadget :refresh NIL NIL)
    (opal:update (g-value agg-of-items :window) T)))


;;; Select all the objects
(defun Standard-Select-All (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (copy-of-objs (copy-list (g-value agg-of-items :components))))
    (Report-If-Wanted gadget :select-all copy-of-objs)
    (gg:set-selection sel-gadget copy-of-objs)))



;; Cut into clipboard.  Objects can be later pasted.
(defun Standard-Cut (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (g-value sel-gadget :value))
	 (clipboard (g-value gadget :clipboard))
	 (new-objs (sort-objs-display-order objs agg-of-items)))
    (Report-If-Wanted gadget :cut new-objs)
    (Delete-Clipboard-Objs clipboard)
    (Add-Clipboard-Objs clipboard new-objs)
    (with-constants-disabled
	(dolist (item new-objs)
	  (opal:remove-component agg-of-items item)))
    (garnet-gadgets:set-selection sel-gadget NIL)))


;; Copy into clipboard.  Objects can be later pasted.  Does not need
;; to copy the gadgets, since they will be copied when pasted, if necessary.
(defun Standard-Copy (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (g-value sel-gadget :value))
	 (clipboard (g-value gadget :clipboard))
	 (new-objs (sort-objs-display-order objs agg-of-items)))
    (Delete-Clipboard-Objs clipboard)
    (Report-If-Wanted gadget :cut new-objs)
    (Add-Clipboard-Objs clipboard new-objs)
    (s-value clipboard :paste-cnt 1)))

;;; There are two popular forms of Paste: one that pastes in the same
;;; place, and one that increments that pasting place each time so the
;;; objects don't overlap if you do multiple pastes.  The first
;;; behavior is represented by Standard-Paste-Same-Place, and the
;;; second by Standard-Paste-Inc-Place
(defun Standard-Paste-Same-Place (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (clipboard (g-value gadget :clipboard))
	 (new-objs (Return-Clipboard-Objs clipboard)))
    (Report-If-Wanted gadget :paste new-objs)
    (with-constants-disabled
	(dolist (obj new-objs)
	  (opal:add-component agg-of-items obj :where
			      (if (is-a-motif-background obj)
				  :back :front))))
    (gg:set-selection sel-gadget new-objs)))

(defun Standard-Paste-Inc-Place (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (clipboard (g-value gadget :clipboard))
	 (paste-cnt (g-value clipboard :paste-cnt))
	 (x-inc-amt (* paste-cnt (or (g-value clipboard :x-inc-amt)
				     Default-Duplicate-Inc-Amt)))
	 (y-inc-amt (* paste-cnt (or (g-value clipboard :y-inc-amt)
				     Default-Duplicate-Inc-Amt)))
	 (new-objs (Return-Clipboard-Objs clipboard)))
    (dolist (obj new-objs)
      (fix-box-points obj x-inc-amt y-inc-amt))
    (Report-If-Wanted gadget :paste new-objs)
    (with-constants-disabled
	(dolist (obj new-objs)
	  (opal:add-component agg-of-items obj :where
			      (if (is-a-motif-background obj)
				  :back :front))))
    (s-value clipboard :paste-cnt (1+ paste-cnt))
    (gg:set-selection sel-gadget new-objs)))

;;; Copies the selected objects
(defun Standard-Duplicate (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (g-value sel-gadget :value))
	 (clipboard (g-value gadget :clipboard))
	 (x-inc-amt (or (g-value clipboard :x-inc-amt)
			Default-Duplicate-Inc-Amt))
	 (y-inc-amt (or (g-value clipboard :y-inc-amt)
			Default-Duplicate-Inc-Amt))
	 (sorted-objs (sort-objs-display-order objs agg-of-items))
	 new-obj new-obj-list)
    (with-constants-disabled
	(progn
	  (dolist (obj sorted-objs)
	    (unless (is-a-motif-background obj)
	      (setq new-obj (opal:copy-gadget obj NIL))
	      (fix-box-points new-obj x-inc-amt y-inc-amt)
	      (push new-obj new-obj-list)))
	  (setq new-obj-list (nreverse new-obj-list))
	  (dolist (obj new-obj-list)
	    (opal:add-component agg-of-items obj :where :front))))
    (Report-If-Wanted gadget :duplicate sorted-objs new-obj-list)
    (gg:set-selection sel-gadget new-obj-list)))

;;; Collect the selected objects into a group
(defun Standard-Group (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (g-value sel-gadget :value))
	 (sorted-objs (sort-objs-display-order objs agg-of-items))
	 new-agg)
    (when (> (list-length objs) 1)
      (setq new-agg (create-instance NIL opal:aggregadget
		      (:group-p T)(:grow-p T)))
      (with-constants-disabled
	  (opal:add-component agg-of-items new-agg)
	(dolist (obj sorted-objs)
	  (opal:remove-component agg-of-items obj)
	  (opal:add-component new-agg obj :where :front)))
      (Report-If-Wanted gadget :group sorted-objs new-agg)
      (garnet-gadgets:set-selection sel-gadget new-agg))))

;;; Remove any objects in any selected groups
(defun Standard-UnGroup (gadget &rest args)
  (declare (ignore args)) ; allow arbitrary args so can be used in
			  ; various menus 
  (let* ((sel-gadget (g-value gadget :sel-gadget)))
    (Do-Ungroup-objs gadget (g-value sel-gadget :value))))

;;; This version explicitly only ungroups these-objs
(defun Do-Ungroup-objs (gadget these-objs)    
  (let* ((sel-gadget (g-value gadget :sel-gadget))
	 (agg-of-items (g-value gadget :agg-of-items))
	 (objs (sort-objs-display-order these-objs agg-of-items))
	 ungrouped-objs)
     (when objs
       (dolist (item objs)
	 (if (g-value item :group-p)
	     (let ((location
		    (position item (g-value agg-of-items :components))))
	       (with-constants-disabled
		 (opal:remove-component agg-of-items item)
		 (dolist (sub-item (reverse (g-value item :components)))
		   (opal:remove-component item sub-item)
		   (push sub-item ungrouped-objs)
		   (opal:add-component agg-of-items sub-item :at location))))
	     ;; else just add item
	     (push item ungrouped-objs)))
       (setq ungrouped-objs (nreverse ungrouped-objs))
       (Report-If-Wanted gadget :un-group objs ungrouped-objs)
       (garnet-gadgets:set-selection sel-gadget ungrouped-objs))))
    




;; add
;;
;;     Standard-Read-From-File
;;     Standard-Save-To-File
;;     Standard-Save-As
;;     Standard-Print



;; ** set a global that something edited to be noticed if clear, quit,
;; open, etc.
