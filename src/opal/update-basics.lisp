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
;;; 20-May-93 AMICKISH In Set-Display-Slots, check that a-window is non-NIL
;;;               before pushing obj on its fix-update-slots-objects list
;;; 16-May-93 KOZ/AMICKISH Now maintain list of all objects that have a
;;;               :fix-update-slots method in Set-Display-Slots
;;; 22-Feb-93 KOZ/AMICKISH Remove-from-invalid-objects-list now checks the
;;;               invalid-view-objects list, also.
;;;  3-Feb-93 KOZ Changed remove-from-invalid-objects-list to not maintain
;;;               last-invalid-obj, and also to check invalid-*-fastdraws lists
;;;  6-Oct-92 KOZ Added :invalid-demon method for VIEW-OBJECT which adds
;;;               object to window's win-update-info-invalid-view-objects list
;;;               Also changed reference to #'update-slot-invalidated to
;;;               a g-value of opal:GRAPHICAL-OBJECT :invalidate-demon
;;;  6-Oct-92 KOZ Changed #'update-slot-invalidated into a *method* called
;;;               :invalidate-demon.  Moved it from VIEW-OBJECT to
;;;               GRAPHICAL-OBJECT, removed the special code in there
;;;               for windows and placed them in a WINDOW method
;;; 29-Apr-92 ECP In set-display-slots, added code for fast-redraw
;;;		  object being removed from aggregate.
;;;  2-Apr-92 RGM Added support for new multifont.
;;;  3-Jan-92 AMICKISH Changed invalidate demon into be a slot in view-object,
;;;               changed to call with-demon-disabled.
;;;  6-Aug-91 DZG Extra error checking in set-display-slots.
;;; 24-Apr-91 KOZ Patched Set-Display-Slots (see comment in text)
;;; 25-Mar-91 ECP In dovalues loops, use :local t so that we don't
;;;		  inherit values of :components.
;;; 12-Jun-90 BVZ Created clear-dirty-bits
;;;
(in-package "OPAL")

#||
;;; RGA  --- This is reduntant and actually screws us up.
(setf kr::*pre-set-demon* NIL)	;; NO MORE DEMON HERE!
||#
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;;;;;;;;              Invalidate Demons             ;;;;;;
       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                     ;;;;;;;  View Object ;;;;;;;;

;;; Newly-added functionality, as of 6-Oct-92 -- if you have :update-slots
;;; inside a VIEW-OBJECT, then when update-window is first called, the
;;; view object's :Fix-Update-Slots method will be invoked.  This method
;;; CAN HAVE SIDE EFFECTS which will be reflected during the same update.

;;; Note that this is presently implemented ** INEFFICIENTLY ** for TWO
;;; reasons (both having to do with the PUSHNEW):
;;;    (1) the CONS'ing of the new objects onto the invalid-view-objects list
;;;        (should be done cons-free, like Make-Object-Invalid does); and
;;;    (2) the implicit test in PUSHNEW (should be via :Invalid-P slot in the
;;;        object, but you have to make sure that opal:destroy,
;;;        opal:add-component, and opal:remove-component all correctly update
;;;        the bit and any pertinent windows' lists appropriately -- the way
;;;        it's done here is less efficient but far easier to do correctly..)

(define-method :invalidate-demon opal:VIEW-OBJECT (vob slot save)
  (declare (ignore slot save))
  (let (the-window win-update-info)
   (unless (null (setq the-window (g-local-value vob :window)))
     (setq win-update-info (g-local-value the-window :win-update-info))
     (setf (win-update-info-invalid-view-objects win-update-info)
           (pushnew vob
                    (win-update-info-invalid-view-objects win-update-info))))))

                        ;;;;;;;  Window ;;;;;;;;

;;; Window's update demon only adds newly-invalid slots to
;;; its invalid-slots list

(define-method :invalidate-demon opal::WINDOW (a-window slot save)
  (declare (ignore save))
  (let ((win-update-info (g-local-value a-window :win-update-info)))
    (if win-update-info
       (pushnew slot (win-update-info-invalid-slots win-update-info)))))

                      ;;;;;;;  Graphical Object ;;;;;;;;

;;; push the slot onto a list of changed slots and push the object
;;; onto its window's changed-objects list.
;;; This was the original "update demon", and used to live inside
;;; "view-object" and not "graphical-object"...

(define-method :invalidate-demon opal:GRAPHICAL-OBJECT (gob slot save)
  (declare (ignore save slot))
  (let* ((gob-update-info (g-local-value gob :update-info))
	 (the-window (and gob-update-info
			  (update-info-window gob-update-info))))
     (and  the-window
	   (not (update-info-invalid-p gob-update-info))
	   (make-object-invalid gob gob-update-info the-window))
   ))

;; Remove gob from invalid-objects list of win-update-info.
;; Now must check each invalid object list (objects and *-fastdraws)
(defun remove-from-invalid-objects-list (gob win-update-info)
  (when win-update-info
    (setf (win-update-info-invalid-objects win-update-info)
      (delete gob (win-update-info-invalid-objects win-update-info)))
    (setf (win-update-info-invalid-view-objects win-update-info)
      (delete gob (win-update-info-invalid-view-objects win-update-info)))
    (setf (win-update-info-invalid-xor-fastdraws win-update-info)
      (delete gob (win-update-info-invalid-xor-fastdraws win-update-info)))
    (setf (win-update-info-invalid-copy-fastdraws win-update-info)
      (delete gob (win-update-info-invalid-copy-fastdraws win-update-info)))))

;;;
;;; set the window and dirty slots of the object and 
;;; recursively set the same slots in its children
;;;
;;; If this is not at the top-level, this will also set the invalid-p entry
;;; in the :update-info to be NIL.
;;; The "with-demons-disabled" and "mark-window-slots-as-changed" were
;;; added by Koz to fix the following bug:  if you add, then
;;; remove, then add again some gadgets (eg, labeled-box), they would
;;; not appear.  This is because some parts depended on the :window
;;; slot, and so were added to the invalid-objects list of the window
;;; when the following lines were invoked...

(defun mark-window-slots-as-changed (object)
  (mark-as-changed object :window)
  ;; dzg - extra checks
  (let ((update-info (g-local-value object :update-info)))
    (when (and update-info (update-info-aggregate-p update-info))
      (dovalues (child object :components :local t)
		(mark-window-slots-as-changed child)))))

(defun set-display-slots (object a-window dirty-bit &optional (top-level T))
  (let ((update-info (g-local-value object :update-info)))
    (when update-info	; dzg
      ;; when removing fast-redraw object from aggregate, change update
      ;; slots values to make it seem as if it used to be invisible, so
      ;; that if you change its position it won't be redrawn in old position.
      (when (and (null a-window)
		 (not (update-info-aggregate-p update-info)))
	(let ((update-slots-values
	       (g-local-value object :update-slots-values))
	      (old-window (update-info-window update-info)))
	  (when (and update-slots-values
		     (aref update-slots-values 1)
		     old-window)
	    (setf (aref update-slots-values 0) NIL)
	    (remove-from-invalid-objects-list
	     object
	     (g-local-value old-window :win-update-info)))))

      (when (and (g-value object :fix-update-slots)
		 a-window)
	(pushnew object (win-update-info-fix-update-slots-objects
			 (g-local-value a-window :win-update-info))))
      
      (with-demon-disabled  (g-value opal:GRAPHICAL-OBJECT :invalidate-demon)
	(s-value object :window
		 (setf (update-info-window update-info) a-window)))

      (if dirty-bit
	  (when (g-value object :visible)
	    (setf (update-info-force-computation-p update-info) T)
	    (propagate-dirty-bit object update-info))
	  (setf (update-info-dirty-p update-info) NIL))
      (when (update-info-aggregate-p update-info)
	(dovalues (child object :components :local t)
		  (set-display-slots child a-window dirty-bit NIL)))
      (if top-level
	  (mark-window-slots-as-changed object)
	  (setf (update-info-invalid-p update-info) NIL)))))


;;;
;;; recursively set the dirty bits of any children to nil
;;;

(defun clear-dirty-bits (agg update-info)
  (setf (update-info-dirty-p update-info) nil)
  (dovalues (child agg :components :local t)
     (let ((child-update-info (g-local-value child :update-info)))
       (when (update-info-dirty-p child-update-info)
	 (clear-dirty-bits child child-update-info)))))


;; invalidate-object-bbox (object)
;;
;;   This function takes an Opal graphical object and merges the its
;;   visible bbox (if any) into the window's  bbox that is to be
;;   erased and redrawn on the next update.  This is for processing an
;;   object that has changed without invalidating an update-slot (which,
;;   in general, is a bad idea anyhow).  Note that if the object has changed
;;   its *position* (ie, it moved), and not invalidated an update-slot (which
;;   should be *impossible*), the object's new location will not be merged
;;   into the drawing clip region, and so the object may be redrawn only
;;   in part or not at all.  You can fix this by using "invalidate-bbox".

;;   Please note that these functions are for a VERY PARTICULAR USE --
;;   in general, if you change an interesting slot with demons disabled,
;;   you should use kr:MARK-AS-CHANGED to get the normal update procedure
;;   to notice and process the change.  Using the method described here
;;   instead, even when the object gets updated, unless you have provided
;;   YOUR OWN DRAW PROCEDURES, it will be drawn as it was last time!
;;
;; invalidate-bbox (a-window x1 y1 x2 y2)
;; 
;;   This function does the same as the one above, but it does not take
;;   an object; rather, it takes a bbox defined by (x1,y1) and (x2,y2) as
;;   the TopLeft and BottomRight.  It also must take a window (the previous
;;   function doesn't have to since it can extract it from the object).


;; This is just a support function, not to be exported!
(defun invalidate-bbox-support (a-window the-bbox)
  (when (and the-bbox (bbox-valid-p the-bbox) a-window)
    (let* ((win-ui (get-local-value a-window :update-info))
	   (win-old-bbox (when win-ui (update-info-old-bbox win-ui))))
      (when win-old-bbox
	(merge-bbox win-old-bbox the-bbox)))))

;; Please see comment above
(defun invalidate-object-bbox (object)
  (let ((obj-ui (get-local-value object :update-info)))
    (when obj-ui
      (invalidate-bbox-support (update-info-window obj-ui)
			       (update-info-old-bbox obj-ui)))))

;; Temporary storage for "invalidate-bbox" function below
(defvar *opal-temp-bbox* (opal::make-bbox :valid-p T))

;; Please see comment above
(defun invalidate-bbox (a-window x1 y1 x2 y2)
  (when a-window
    (setf (bbox-x1 *opal-temp-bbox*) x1)
    (setf (bbox-y1 *opal-temp-bbox*) y1)
    (setf (bbox-x2 *opal-temp-bbox*) x2)
    (setf (bbox-y2 *opal-temp-bbox*) y2)
    (invalidate-bbox-support a-window *opal-temp-bbox*)))
