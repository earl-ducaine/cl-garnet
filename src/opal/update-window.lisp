;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;-------------------------------------------------------------------;;

;;; $Id$


;;; Changes:
;;  09-Jun-94 Mickish - Called ccl:validate-view for Mac
;;  05-Mar-94 Mickish - Wrapped with-focused-view-or-gworld for Mac around
;;                      all drawing
;;  10-Aug-93 Koz/Mickish - Moved :set-frr-bbox and :set-styles methods to
;;                          fast-redraw.lisp; Created some sub-functions for update
;;                          method, since Allegro 4.1 couldn't handle large function.
;;  28-May-93 Mickish - Moved all total-p calculations to top of :update method
;;  17-May-93 koz Do not update children if only due to expose event
;;                (change suggested by bolt@cs.utk.edu)
;;  16-May-93 Mickish - Called :fix-update-slots method on all view-objects
;;                      during total-update.
;;   6-Apr-93 koz And omitted "clip-mask" as argument to update,
;;                and instead just set clip-mask at top level.
;;  27-Mar-93 koz Eliminated "flet" due to Allegro space leak.  Instead,
;;                now use "dothings" (defined in opal:macros.lisp), which
;;                is only a little clunkier...
;;  24-Mar-93 koz Changed code labeled "Recursively update children" so if
;;                it finds a child whose parent does not match the current
;;                window, it properly updates the re-parented child
;;   3-Feb-93 koz Reworked handling of fastdraws to now use invalid-*-fastdraws
;;                lists (see "flet" of "pre-process-fastdraws"...)
;;   3-Feb-93 koz Removed "last-invalid-obj" references.  Replaced calls to
;;                "free-invalid-objects" with "free-list".
;;   8-Jan-92 Mickish - Added reset of :in-progress quarantine slot
;;   6-Oct-92 koz Added code to handle invalid-view-objects, which lets you
;;                have :update-slots in view-objects (like AggreLists, etc.)
;;                and if they are invalidated, their :fix-update-slots method
;;                is invoked and is allowed to have side-effects!
;;  28-Sep-92 dzg,amickish - Added update-start-fn and update-stop-fn calls
;;  25-Sep-92 koz,amickish,hopkins  Only clear the window's invalid-objects
;;                 list (at beginning of update) if the window is visible.
;;  17-Sep-92 Mickish - When checking whether :width and :height of window
;;                      has really changed, ignore whether computed by formula.
;;  15-Jul-92 koz,dzg - Modified the :update method for windows to set the
;;		        list of invalid objects to NIL at the very beginning, to
;; 		        allow for reentrant invalidation by other processes (such
;; 		        as calling destroy-constraint from inside a formula during
;; 		        Update).
;;  28-Apr-92 williams Don't do total update on subwindow if only cursor
;; 		       changed.
;;  16-Apr-92 ecp Call xlib:display-force-output if any slots are invalid.
;;   9-Apr-92 ecp Removed patches of 4-Oct-91 from inner loop of update method.
;;  01-Apr-92 koz & Mickish Added a check in (**) loop for fast redraw objects
;;                          which handles the case when the parent has never 
;;                          been updated
;;  31-Mar-92 ecp Fixed yet another bug: changing position of subwindow.
;;  26-Mar-92 koz Fixed method for propagating changes to the bboxes of
;;                fastdraws' parent(s) so it is once again constant-time,
;;                except for one special case.  See comments in code.
;;                (Also added "swap" macro, to swap two variables.)
;;  25-Mar-92 Mickish Get-Values ---> G-Value; added THE type declarations
;;  19-Mar-92 Mickish Added type declarations in set-styles methods
;;  11-Mar-92 Pervin  New width and height fields of win-update-info.
;;  28-Feb-92 ecp If width or height of window are invalidated,
;; 		  make sure that the value has really changed.
;;  28-Feb-92 amickish kr::schema-slots --> schema-p
;;  25-Feb-92 ecp Don't invalidate bboxes until very end.
;;  19-Feb-92 ecp Implemented double-clip-masks as list of length 8.
;;  16-Dec-91 ecp Rewrote exposure of double-buffered window.  No longer
;; 		  re-allocate exposed-clip-mask.
;;  11-Dec-91 ecp Removed change of 18-Oct-91.  It was expensive, and
;; 		  apparantly Kosbie's changes of Nov-91 made them unnecessary.
;;  25-Nov-91 koz significant restructuring of the code to reduce the amount
;;                of overhead when there are no changes to be made (this is
;;                to speed up "update-all")
;;  25-Nov-91 koz rewrote fix-properties-and-validate invocation code so now
;;                it skips the macro call, and then makes a function call to
;;                fix-window-properties (instead of the old method invocation)
;;  20-Nov-91 Kosbie & Pervin
;; 		  Fixed flicker in backing-store windows.
;; 		  Also, removed all #+comment parts, since they are never used.
;;  18-Oct-91 ECP Remove :width or :height from invalid-slots list of a
;; 		  window if the values of those slots hadn't actually changed.
;;   4-Oct-91 ECP Patches for virtual aggregates (e.g. changed-bbox).
;;   3-Oct-91 Andrew Mickish
;;                Changed fast-redraw code so that :fast-redraw-p now takes
;;                the values T, :rectangle, and :redraw
;;   5-Feb-91 Dave Kosbie & Andrew Mickish
;; 		  Changed first UNLESS clause in :update method to fix bug
;;                in placement of subwindows.
;;  28-Sep-90 ECP After drawing a fastdraw object, update the bboxes of
;;                its ancestors
;;  23-Aug-90 ECP Added two lines to :update to activate invalidate-demons
;; 		  associated with :width and :height
;;  13-Aug-90 ECP Changed g-value to get-local-value in test to see
;; 		  if window already has a :drawable.
;;  18-Jul-90 ECP Fixed bug with resizing double-buffered window.
;;   9-Jul-90 ECP Added test for kr::schema-name of invalid objects.
;;   2-Jul-90 ECP If an expose event occurs, just refresh the parts
;; 		  of the window that were exposed.
;;  20-Jun-90 ECP Lots of debugging of double-buffering.  If you
;; 		  de-iconify such a window and no other changes
;; 		  have taken place, just do a copy-area.
;;   6-Jun-90 ECP Implemented double-buffering.



;;; comment ** (this refers to where '**' appears in the code below): this line
;; is necessary because: if object A's visibility depends on its parent P,
;; which is in window W, and we set P's visibility from
;; T to NIL, and then we do a total update on W, so it traverses P,
;; sees that it's not visible, and stops there.  This will put up the correct
;; picture, but...  The visibility of P is *valid* and NIL, and the visibility
;; of A is *invalid*.  Then, we set P's visibility to T.  This does not put
;; P on W's invalid-objects list because aggregates have no 'interesting'
;; slots; nor does it put A there, since its visibility slot was already
;; invalid!!!  Thus, there is no record that A is now visible (though invalid),
;; and nothing happens on subsequent updates.
;; The fix:  in a total update, get the visibility slot of all invalid objects
;; (via g-value).  Why does this work?  Because, in the previous example, A's
;; visible slot would NOT be invalid when we set P to visible, thus resulting
;; in A's visible slot being invalidated, so A would wind up on the
;; invalid-objects list.
;; Note that there's a little more to it, since you have to also *record* that
;; A was invalid in the :update-slots-values array (aref ... 0), and in the
;; valid-p entry of the old-bbox in the :update-info of A.  Ack!



(in-package "OPAL")

(declaim (inline fast-erase))
(defun fast-erase (object a-window line-style-gc filling-style-gc)
  (gem:set-clip-mask a-window :none line-style-gc filling-style-gc)
  (draw object a-window))


(defun copy-from-buffer-to-drawable (a-window bbox buffer drawable)
  (let ((x1 (bbox-x1 bbox))
	(x2 (bbox-x2 bbox))
	(y1 (bbox-y1 bbox))
	(y2 (bbox-y2 bbox)))
    (gem:bit-blit a-window buffer x1 y1 (- x2 x1) (- y2 y1)
		    drawable x1 y1)))

;;(defmacro swap(a b) `(let((.temp. ,a)) (setq ,a ,b) (setq ,b .temp.)))
(defmacro swap(a b) `(rotatef ,a ,b))



(defvar newly-invisible-fastdraws-bbox (make-bbox))
;; The following are necessary for propagating changes to the bboxes
;; of parent(s) of fastdraws, when the fastdraws' bboxes change
(defvar fastdraw-old-bbox (make-bbox))
(defvar parent-old-bbox (make-bbox))

(defvar exposed-clip-mask (make-list 4))

(defun install-drawable (window-agg a-window win-info)
  (when window-agg
    (set-display-slots window-agg a-window T)
    (setf (win-update-info-old-aggregate win-info) window-agg))
  (create-drawable a-window))


(defun fix-invalid-slots (invalid-slots win-info a-window)
  "Delete :width or :height from invalid-slots list of a window
if those slots contain formulas, and if the values
of those slots hadn't actually changed.
This is done to avoid unnecessary total updates."
  (when (and (member :width invalid-slots)
	     (eq (win-update-info-width win-info)
		 (g-value a-window :width)))
    (setq invalid-slots (delete :width invalid-slots)))
      (when (and (member :height invalid-slots)
		 (eq (win-update-info-height win-info)
		     (g-value a-window :height)))
	(setq invalid-slots (delete :height invalid-slots)))
      invalid-slots)


(defun process-invalid-slots (invalid-slots win-info a-window drawable)
  "Returns T iff update should become TOTAL update after this call."
  (fix-window-properties a-window invalid-slots drawable)
  (setf (win-update-info-invalid-slots win-info) NIL)
  (not (subsetp invalid-slots
		(if (g-local-value a-window :parent)
		    '(:cursor)
		    '(:left :top :cursor)))))


(defun do-fix-update-slots (win-info a-window)
  (let ((vobs (win-update-info-fix-update-slots-objects win-info)))
    (dolist (vob vobs)
      (if (and (schema-p vob)
	       (eq (g-value vob :window) a-window))
	  (fix-update-slots vob)
	  (setf (win-update-info-fix-update-slots-objects win-info)
		(delete vob vobs))))))


(defun do-total-update (invalid-objects invalid-xors invalid-copys a-window
					window-agg buffer exposed-clip-mask
					line-style-gc filling-style-gc)
  (let (exposed-bbox)			; Exposed-bbox tells whether the window was
					; just exposed and nothing else happened to it.
    (unless (and (setq exposed-bbox (and (null invalid-objects)
					 (null invalid-xors)
					 (null invalid-copys)
					 (g-value a-window :exposed-bbox)))
		 buffer)
      (if exposed-bbox 
	(progn
	  (bbox-to-clip-mask exposed-bbox exposed-clip-mask)
	  (erase-bbox exposed-bbox a-window nil))
	(if buffer
	  (clear-buffer a-window)
	  (gem:clear-area a-window)))
     
      (dothings (invalid-objects-list
		 invalid-objects invalid-xors invalid-copys)
	(dolist (object invalid-objects-list)
	  ;; See comment '**' above...
	  (let ((obj-us-values (g-local-value object :update-slots-values))
		(obj-update-info (the UPDATE-INFO
				      (g-local-value object :update-info))))
	    (g-value object :visible)
	    (and obj-us-values
		 (not (update-info-aggregate-p obj-update-info))
		 (setf (aref obj-us-values 0) NIL))
	    (setf (bbox-valid-p
		   (update-info-old-bbox obj-update-info))
		  NIL))
	  (let ((info (the UPDATE-INFO (g-local-value object :update-info))))
	    (when info
	      (setf (update-info-invalid-p info) NIL)))))

      (when (g-value window-agg :visible)
	(gem:set-clip-mask a-window (if exposed-bbox exposed-clip-mask :none)
			   line-style-gc filling-style-gc)
	(update-method-aggregate window-agg
				 (g-local-value window-agg :update-info)
				 line-style-gc filling-style-gc
				 exposed-bbox NIL (not exposed-bbox)))

      (free-list invalid-objects)
      (free-list invalid-xors)
      (free-list invalid-copys))))


(defun do-partial-update (invalid-objects invalid-xors invalid-copys a-window
			  window-agg buffer exposed-clip-mask
			  line-style-gc filling-style-gc obj-update-info
                          obj-update-slots-values win-info win-new-bbox
                          win-old-bbox fastdraw-objects)
  (let (obj-old-bbox f-obj-update-info f-obj-old-bbox)
    (setf (bbox-valid-p win-new-bbox) NIL)
    ;; First deal with FASTDRAWs.
    (dothings (fastdraws invalid-xors invalid-copys)
      (let (first-changed)
	(dolist (object fastdraws)
	  (when (and (schema-p object)	; check if it was already destroyed
		     (or (not (setq obj-update-slots-values
				    (g-local-value object
						   :update-slots-values)))
			 (aref obj-update-slots-values 1)
			 (not (aref obj-update-slots-values 0))))
	    (setf (update-info-invalid-p
		   (setq obj-update-info
			 (the UPDATE-INFO (g-local-value object :update-info))))
		  NIL)
	    ;; Check if it really has changed!
	    (when (setq first-changed
			(simple-update-slots-values-changed object))
	      ;; if it was visible, erase it...
	      (when (and obj-update-slots-values
			 (aref obj-update-slots-values 0))
		;; Change for values of :rectangle and :redraw in
		;; :fast-redraw-p slot  --Andrew Mickish
		(case (g-value object :fast-redraw-p)
		  (:rectangle
		   ;; Draw a rectangle over the bbox of the object.
		   ;; This rectangle will have the background filling
		   ;; style, so the object will disappear.
		   (set-frr-bbox object)
		   (setf (aref frr-update-vals +rect-fstyle+)
			 (g-value object :fast-redraw-filling-style))
		   (fast-erase fast-redraw-rectangle a-window
			       line-style-gc filling-style-gc))
		  (:redraw
		   ;; Set the filling and line styles of the object to be
		   ;; the background styles, redraw the object, restore
		   ;; its real styles (the changes occur in the update-
		   ;; values-slots array, not the object's style slots)
		   (set-styles object
			       (g-value object :fast-redraw-line-style)
			       (g-value object :fast-redraw-filling-style))
		   (fast-erase object a-window
			       line-style-gc filling-style-gc)
		   (set-styles object
			       (g-value object :line-style)
			       (g-value object :filling-style)))
		  (t
		   ;; The object is drawn with an :xor draw-function,
		   ;; so just draw the object again to erase it.
		   (fast-erase object a-window
			       line-style-gc filling-style-gc))))
	      (if (g-value object :visible)
		  (progn
		    ;; Add "first-changed" & object to
		    ;; the fastdraw list to draw later
		    (setq fastdraw-objects
			  (get-cons object
				    (get-cons first-changed
					      fastdraw-objects)))
		    (setf (update-info-on-fastdraw-list-p obj-update-info)
			  T))
		  ;; ELSE it's NOT VISIBLE....
		  (progn
		    (when obj-update-slots-values
		      (setf (aref obj-update-slots-values 0) NIL))
		    (merge-bbox newly-invisible-fastdraws-bbox
				(update-info-old-bbox obj-update-info))
		    (setf (bbox-valid-p (update-info-old-bbox obj-update-info))
			  NIL))))))
	(free-list fastdraws)))

	;; Now process non-FASTDRAWs
	(when invalid-objects
	  (dolist (object invalid-objects)
	    ;; The next line represents a temporary hack to deal with a
	    ;; problem discovered in demo-arith, in which occasionally
	    ;; objects marked as *DESTROYED* were still contained in
	    ;; the invalid objects list.
	    (when (schema-p object)
	      (setq obj-old-bbox
		    (update-info-old-bbox
		     (the UPDATE-INFO
			  (setq obj-update-info
				(g-local-value object :update-info)))))
	      (setf (update-info-invalid-p obj-update-info) NIL)
	      (setq obj-update-slots-values
		    (g-local-value object :update-slots-values))
	      (if (g-value object :visible)
		  ;; Object is a VISIBLE NORMAL OBJ
		  (if (bbox-valid-p obj-old-bbox)	
		      ;;object IS and WAS visible
		      (when (update-slots-values-changed object 0 obj-update-info)
			(merge-bbox win-old-bbox obj-old-bbox)
			(update-bbox object obj-old-bbox)
			(merge-bbox win-new-bbox obj-old-bbox)
			(propagate-dirty-bit object obj-update-info)
			)
		      (progn		; object IS and WAS NOT visible
			(update-bbox object obj-old-bbox)
			(update-slots-values-changed object 0 obj-update-info)
			(merge-bbox win-new-bbox obj-old-bbox)
			(propagate-dirty-bit object obj-update-info)
			))
		  (when (bbox-valid-p obj-old-bbox) ; object IS NOT and WAS visible
		    (merge-bbox win-old-bbox obj-old-bbox)
		    (setf (bbox-valid-p obj-old-bbox)
			  (setf (aref obj-update-slots-values 0)
				NIL)))
		  ;;if object IS NOT and WAS NOT
		  ;;visible, then do nothing!!
		  )))
	  (free-list invalid-objects))

	;; Now only perform the update if one
	;; of the two window's bboxes is valid.
	(let ((old-bbox-valid (bbox-valid-p win-old-bbox))
	      (new-bbox-valid (bbox-valid-p win-new-bbox))
	      (clip-mask-1 (win-update-info-clip-mask-1 win-info))
	      (clip-mask-2 (win-update-info-clip-mask-2 win-info))
	      two-bboxes-p)
	  (when (or new-bbox-valid old-bbox-valid)
	    (if (setq two-bboxes-p (and new-bbox-valid old-bbox-valid))
		(if (bbox-intersect-p win-old-bbox win-new-bbox) ; they intrsect?
		    (progn
		      (merge-bbox win-new-bbox win-old-bbox) ; merge into new
		      (setq two-bboxes-p NIL)		     ; really only 1!
;;;		      (setf (bbox-valid-p win-old-bbox) NIL) ; save until end
		      (erase-bbox win-new-bbox a-window buffer)
		      (bbox-to-clip-mask win-new-bbox clip-mask-1))
		    (progn
;;;		      (setf (bbox-valid-p win-old-bbox) NIL) ; save until end
		      (erase-bbox win-old-bbox a-window buffer)
		      (erase-bbox win-new-bbox a-window buffer)
		      (bbox-to-clip-mask win-old-bbox clip-mask-1)
		      (bbox-to-clip-mask win-new-bbox clip-mask-2)))

		(progn			; Only one valid bbox
		  (when old-bbox-valid
		    (swap win-old-bbox win-new-bbox)
;;;		    (setf (bbox-valid-p win-old-bbox) NIL) ; save 'til end
		    )
		  (erase-bbox win-new-bbox a-window buffer)
		  (bbox-to-clip-mask win-new-bbox clip-mask-1)))

	    (if two-bboxes-p
		(progn
		  (gem:set-clip-mask a-window clip-mask-2
				     line-style-gc filling-style-gc)
		  (update-method-aggregate
		   window-agg (g-local-value window-agg :update-info)
		   line-style-gc filling-style-gc
		   win-old-bbox win-new-bbox NIL))
		(progn
		  (gem:set-clip-mask a-window clip-mask-1
				     line-style-gc filling-style-gc)
		  (update-method-aggregate
		   window-agg (g-local-value window-agg :update-info)
		   line-style-gc filling-style-gc win-new-bbox NIL NIL)))))
	;; If there are fastdraw objects, draw
	;; them, then clear the list....
	(when fastdraw-objects
	  (do* ((flist         fastdraw-objects (cddr flist))
		(fastdraw-obj  (first flist) (first flist))
		(first-changed (second flist) (second flist)))
	       ((null flist))
	    (setq f-obj-old-bbox
		  (update-info-old-bbox
		   (the UPDATE-INFO
			(setq f-obj-update-info
			      (g-local-value fastdraw-obj :update-info)))))
	    (update-slots-values-changed fastdraw-obj first-changed
					 f-obj-update-info)
	    (when buffer
	      (merge-bbox win-old-bbox f-obj-old-bbox))

	    ;; Next 2 lines are for parent propagation (** below)
	    (swap fastdraw-old-bbox f-obj-old-bbox)
	    (setf (update-info-old-bbox f-obj-update-info) f-obj-old-bbox)

	    (update-bbox fastdraw-obj f-obj-old-bbox)
	    (when buffer
	      (merge-bbox win-old-bbox f-obj-old-bbox))
	    (unless buffer
	      (gem:set-clip-mask a-window :none line-style-gc filling-style-gc)
	      (draw fastdraw-obj a-window))

	    ;; (**) Now must propagate bbox changes to parent(s), but
	    ;; ONLY IF NECESSARY, and then as CHEAPLY AS POSSIBLE!!!(koz)
	    (let ((old-bbox fastdraw-old-bbox)
		  (new-bbox f-obj-old-bbox)
		  (object   fastdraw-obj)
		  parent parent-ui parent-bbox parent-changed?)
	      (loop

		 ;; If there is no parent, return!
		 (unless (setq parent (g-local-value object :parent))
		   (return))

		 ;; else, (re)set parent-ui, parent-bbox and parent-changed?
		 (setq parent-bbox
		       (update-info-old-bbox
			(the UPDATE-INFO
			     (setq parent-ui (g-local-value parent :update-info)))))
		 (setq parent-changed? NIL)

		 ;; If the parent-bbox has never been updated, then its
		 ;; valid-p will be NIL, so copy current fastdraw bbox
		 ;; into it, and set up to check parent's parent.
		 (if (null (bbox-valid-p parent-bbox))
		     (progn
		       (setq parent-changed? T)
		       (copy-bbox-fn parent-bbox new-bbox)
		       (setf (bbox-valid-p parent-old-bbox) NIL))

		     (progn		; else for (if (null (bbox-valid-p parent-bbox)) ...)
		       (when (or (< (bbox-x1 new-bbox) (bbox-x1 parent-bbox))
				 (> (bbox-x2 new-bbox) (bbox-x2 parent-bbox))
				 (< (bbox-y1 new-bbox) (bbox-y1 parent-bbox))
				 (> (bbox-y2 new-bbox) (bbox-y2 parent-bbox)))
			 (setq parent-changed? T)
			 ;; Must copy explicitly, instead of using SWAP, since
			 ;; the old values are also needed by merge-bbox below
			 (copy-bbox-fn parent-old-bbox parent-bbox)
			 (merge-bbox parent-bbox new-bbox))
		  

		       ;; Now, if for any dimension, both:
		       ;;  * old-bbox defines boundary of parent-bbox
		       ;;    (ie, old-bbox equals parent-bbox), and
		       ;;  * old-bbox does not equal new-bbox
		       ;; Then deleting the old-bbox contracts the parent-bbox.
		       (when
			   (and (bbox-valid-p old-bbox)
				(or (and (eql  (bbox-x1 old-bbox) (bbox-x1 parent-bbox))
					 (not (eql (bbox-x1 old-bbox) (bbox-x1 new-bbox))))
				    (and (eql  (bbox-x2 old-bbox) (bbox-x2 parent-bbox))
					 (not (eql (bbox-x2 old-bbox) (bbox-x2 new-bbox))))
				    (and (eql  (bbox-y1 old-bbox) (bbox-y1 parent-bbox))
					 (not (eql (bbox-y1 old-bbox) (bbox-y1 new-bbox))))
				    (and (eql  (bbox-y2 old-bbox) (bbox-y2 parent-bbox))
					 (not (eql (bbox-y2 old-bbox) (bbox-y2 new-bbox))))))

			 ;; so, if parent-changed? is NIL, set it to T and store
			 ;; parent-old-bbox.  Then we finally cannot avoid the
			 ;; expensive operation of update-bbox (ack!)
			 (unless parent-changed?
			   (setq parent-changed? T)
			   (swap parent-old-bbox parent-bbox)
			   (setf (update-info-old-bbox parent-ui) parent-bbox))
			 (update-bbox parent parent-bbox))))

		 ;; Finally, if parent-changed? is T, then set up
		 ;; variables for next iteration, else return!
		 (if parent-changed?
		     (progn
		       (swap parent-old-bbox fastdraw-old-bbox)
		       (setq old-bbox fastdraw-old-bbox)
		       (setq new-bbox parent-bbox)
		       (setq object   parent))
		     (return))))
	    ;; (**) Done propagating bbox changes to parent(s)

	    (when buffer
	      (gem:set-clip-mask a-window :none line-style-gc filling-style-gc)
	      (draw fastdraw-obj a-window))
	    (setf (update-info-on-fastdraw-list-p f-obj-update-info)
		  NIL))
	   (free-list fastdraw-objects))))

(define-method :update opal::window (a-window &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 1)))
  (with-update-lock-held
    (let* ((win-info (g-local-value a-window :win-update-info))
	   (drawable (g-local-value a-window :drawable))
	   (window-agg (g-local-value a-window :aggregate))
	   (invalid-slots (win-update-info-invalid-slots win-info))
	   invalid-vobs)

      (unless drawable
	(setq drawable (install-drawable window-agg a-window win-info))
	(setq total-p T))

      (when invalid-slots
	(setq invalid-slots (fix-invalid-slots invalid-slots win-info a-window))
	(if (process-invalid-slots invalid-slots win-info a-window drawable)
	    (setq total-p T)))
   
      ;; If this is a total update, call the :fix-update-slots method on every
      ;; object in the window that has one.
      (when total-p
	(do-fix-update-slots win-info a-window))

      ;; The "invalid-view-objects" code iterates through all view-objects
      ;; which were invalidated and (if they are still in the window) calls
      ;; their :fix-update-slots method.  Since these can invalidate *other*
      ;; view-objects with their side-effects, this must loop until no more
      ;; view-objects are being invalidated.
      (loop
	 (setq invalid-vobs (win-update-info-invalid-view-objects win-info))
	 (unless invalid-vobs (return))
	 (setf (win-update-info-invalid-view-objects win-info) NIL)
	 (dolist (vob invalid-vobs)
	   (when (and (schema-p vob) (eq a-window (g-local-value vob :window)))
	     (fix-update-slots vob)))
	 (free-list invalid-vobs))
	       
      (let* ((invalid-objects (win-update-info-invalid-objects win-info))
	     (invalid-xors    (win-update-info-invalid-xor-fastdraws win-info))
	     (invalid-copys   (win-update-info-invalid-copy-fastdraws win-info))
	     (visible         (eq (g-value a-window :visible) T))
	     (win-old-bbox    (update-info-old-bbox
			       (the UPDATE-INFO
				    (g-local-value a-window :update-info))))
	     (partial-p       (and window-agg
				   (g-value window-agg :visible)
				   (or invalid-objects
				       invalid-xors
				       invalid-copys
				       (bbox-valid-p win-old-bbox)))))

	(when visible
	  (setf (win-update-info-invalid-objects        win-info) nil
		(win-update-info-invalid-xor-fastdraws  win-info) nil
		(win-update-info-invalid-copy-fastdraws win-info) nil))
  
	;; At this point, we try to abort if possible -- only do the main part
	;; of update if something really has changed...
	(when (or total-p partial-p)
	  (let* ((win-new-bbox (win-update-info-new-bbox win-info))
		 (buffer (g-value a-window :buffer))
		 (display-info (g-value a-window :display-info))
		 (line-style-gc (display-info-line-style-gc display-info))
		 (filling-style-gc (display-info-filling-style-gc display-info))
		 fastdraw-objects
		 obj-update-slots-values
		 obj-update-info)

	    (if buffer
		(setf (bbox-valid-p newly-invisible-fastdraws-bbox) nil))

	    (when (and window-agg visible)
	      (if total-p

		  ;; This is a TOTAL window update.
		  (progn
		    (do-total-update invalid-objects invalid-xors invalid-copys a-window
				     window-agg buffer exposed-clip-mask
				     line-style-gc filling-style-gc))
      
		  ;;else this is a PARTIAL window update.
		  (do-partial-update invalid-objects invalid-xors invalid-copys a-window
				     window-agg buffer exposed-clip-mask
				     line-style-gc filling-style-gc obj-update-info
				     obj-update-slots-values win-info win-new-bbox
				     win-old-bbox fastdraw-objects)))

	    ;; When using double-buffering, copy buffer into window.
	    (when (and visible buffer)
	      (if (or total-p (null win-new-bbox))
		  (gem:bit-blit a-window buffer
				0 0 (g-value a-window :width) (g-value a-window :height)
				drawable 0 0)
		  (progn
		    (when win-new-bbox
		      (merge-bbox newly-invisible-fastdraws-bbox win-new-bbox))
		    (when win-old-bbox
		      (merge-bbox newly-invisible-fastdraws-bbox win-old-bbox))
		    (when (bbox-valid-p newly-invisible-fastdraws-bbox)
		      (copy-from-buffer-to-drawable a-window
						    newly-invisible-fastdraws-bbox
						    buffer drawable)))))

	    (setf (bbox-valid-p win-old-bbox) NIL
		  (bbox-valid-p win-new-bbox) NIL)

	    )) ;; end of (when (or total-p partial-p) ...)

	(when (or total-p partial-p invalid-slots)
	  (gem:flush-output a-window))

	;; Recursively update children
	(let ((base-children (g-value a-window :child)))
	  (if (and base-children
		   (not (g-value a-window :exposed-bbox)))
	      (do* ((children base-children    (rest children))
		    (child    (first children) (first children)))
		   ((null children))
		(unless (eq a-window (g-value child :parent))
		  ;; this code is for when a sub-window is re-parented, but then the
		  ;; old parent (ie, "a-window") is updated before the sub-window is.

		  ;; The pushnew of :parent makes sure the parent slot will be checked.
		  ;; The copy-list is needed because the ensuing update will
		  ;; destructively remove the sub-window from "a-window"'s :child list.
		  (pushnew :parent (win-update-info-invalid-slots
				    (g-value child :win-update-info)))
		  (setq children (copy-list children))
		  (unless (eq a-window (g-value child :old-parent))
		    (s-value a-window :child (delete child (g-value a-window :child)))))

		(update child total-p)))))))

  ;; Mark that we are finished updating this window (it was set by update-all)
  (s-value a-window :in-progress NIL))
