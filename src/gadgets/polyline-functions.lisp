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
;;; These functions enable a user to edit a polygon that has AT LEAST
;;; TWO (2) points.  If it has only one point, strange things will happen.
;;;
;;; These are the user visible functions:
;;;
;;; toggle-polyline-handles:  creates (if necessary) & displays the handles to 
;;;                           allow polyline editing.  If handles are already
;;;                           exposed, turns it off.
;;; destroy-polyline-handles:  destroys the polyline handles
;;;

;;; CHANGE LOG:
;;;
;;; 10/18/93  Andrew Mickish - Moved export list to polyline-creator.lisp
;;;  8/02/93  Andrew Mickish - Reordered function declarations
;;;  7/26/93  Andrew Mickish - Added special proclamation to avoid warning
;;;  6/23/93  Andrew Mickish - Exported hide-polyline-handles
;;;  6/17/93  Rajan Parthasarathy - Created!


(in-package "GARNET-GADGETS")

(declaim (special POLYLINE-POINT-DELETING-INTER))

;;; polyline-edit-handle:  This is the handle with which the user can
;;; drag, create, and delete polyline points.  Basically, it's a 7 by 7
;;; square centered around the point.  The point is specified by setting
;;; the :xy slot to be the xy pair.  The handle is visible when it is a
;;; member of the :polyline-handles-being-used slot of the polyline creator.
;;; This means that it's being used now, and so, it should be visible.

(create-instance 'polyline-edit-handle opal:rectangle
  (:xy '(0 0))
  (:x-coord (o-formula (first (gvl :xy))))
  (:y-coord (o-formula (second (gvl :xy))))
  (:polyline (o-formula (gvl :polyline-creator :polyline-being-edited)))
  (:polyline-creator NIL)
  (:rank (o-formula (position (gv :self)
			      (gvl :polyline-creator :polyline-handles-being-used))))
  (:visible (o-formula (member (gv :self)
			       (gvl :polyline-creator :polyline-handles-being-used))))
  (:width 7)
  (:height 7)
  (:left (o-formula (- (first (gvl :xy)) 3)))
  (:top (o-formula (- (second (gvl :xy)) 3)))
  (:draw-function :xor)
  (:fast-redraw-p T)
  (:filling-style opal:black-fill)
  (:line-style NIL))


;;; polyline-mover-feedback:  This is the dashed feedback polyline
;;; that appears when we move a point.  :attach-points are the two
;;; points on either side of the point being moved.  

(create-instance 'polyline-mover-feedback opal:polyline
  (:polyline (o-formula (gvl :parent :operates-on)))
  (:attach-points '(0 0 0 0))
  (:box '(0 0))
  (:draw-function :xor)
  (:fast-redraw-p T)
  (:point-list (o-formula
		(list (first (gvl :attach-points))
		      (second (gvl :attach-points))
		      (+ (first (gvl :box)) 3)
		      (+ (second (gvl :box)) 3)
		      (third (gvl :attach-points))
		      (fourth (gvl :attach-points)))))
  (:line-style opal:dashed-line))


;;; polyline-point-moving-inter-start-action:
;;; This is called by the point-moving interactor when it starts
;;; up.  Basically, we have to set the feedback object to be
;;; visible, and then set up the correct attach points for the
;;; feedback object.  Then, the interactor will set the :box slot
;;; of the feedback-obj and everything will sail smoothly.

(defun polyline-point-moving-inter-start-action (inter handle box)
  (when (g-value handle :visible)
    (let* ((poly (g-value handle :polyline))
	   (points (g-value poly :point-list))
	   (len (length points))
	   (attach-points NIL)
	   (rank (g-value handle :rank))
	   (feedback (g-value inter :feedback-obj)))
      
      
      (cond
	
	;; If this is the FIRST point in an open polyline,
	;; set the attach-points to be the second point
	((AND (eq rank 0)
	      (NOT (g-value poly :closed-polyline-p)))
	 (setf attach-points (list (third points) (fourth points)
				   (third points) (fourth points))))
	
	
	;; If this is the LAST point in an open polyline,
	;; set the attach-points to be the (last - 1)th point
	((AND (eq (* 2 (1+ rank)) len)
	      (NOT (g-value poly :closed-polyline-p)))
	 (setf attach-points
	       (list (nth (- len 4) points) (nth (- len 3) points)
		     (nth (- len 4) points) (nth (- len 3) points))))
	
	
	;; If this is the first or last point in a CLOSED polyline,
	;; set the attach-points to be the second and (last - 1)th
	;; points
	((AND (OR (eq rank 0) (eq (* 2 (1+ rank)) len))
	      (g-value poly :closed-polyline-p))
	 (setf attach-points
	       (list (third points) (fourth points)
		     (nth (- len 4) points) (nth (- len 3) points))))
	
	
	;; If this is NOT an end point, set the attach points to be the
	;; points before and after the current point.
	(T (setf attach-points
		 (list (nth (* 2 (1- rank)) points)
		       (nth (1+ (* 2 (1- rank))) points)
		       (nth (* 2 (1+ rank)) points)
		       (nth (1+ (* 2 (1+ rank))) points)))))
      
      (s-value feedback :attach-points attach-points)
      (s-value feedback :box (list (first box) (second box))))))


;;; polyline-point-moving-inter-stop-action:
;;; This is called with the point-moving interactor has stopped.
;;; We need to adjust the point-list of the polyline and the xy
;;; coordinates of the current handle to reflect the new position
;;; of the point.

(defun polyline-point-moving-inter-stop-action (inter handle box)
  (when (g-value handle :visible)
    (let* ((poly (g-value handle :polyline))
	   (points (copy-list (g-value poly :point-list)))
	   (x (+ 3 (first box)))
	   (y (+ 3 (second box)))
	   (rank (g-value handle :rank))
	   (len (1- (length points))))
      (s-value (g-value inter :feedback-obj) :visible NIL)

      ;; Change the points in the :point-list
      (setf (nth (* 2 rank) points) x)
      (setf (nth (1+ (* 2 rank)) points) y)

      ;; If we're changing the first point in a closed
      ;; polyline, remember to adjust the last point so
      ;; it stays closed!
      (when (AND (= rank 0)
		 (g-value poly :closed-polyline-p))
	(setf (nth len points) y)
	(setf (nth (1- len) points) x))

      ;; s-value the changes.
      (s-value poly :point-list points)
      (s-value handle :xy (list x y))
      (opal:update (g-value poly :window))

      ;; Call the modify-function.  If the :added-handle slot is set,
      ;; want to call it with action :add-move, else call it with action
      ;; :move.
      (if (g-value inter :added-handle)
	  (progn
	    (s-value inter :added-handle NIL)
	    (kr-send (g-value inter :operates-on) :modify-function
		     (g-value inter :operates-on) poly (* 2 rank) :add-move))
	  (kr-send (g-value inter :operates-on) :modify-function
		     (g-value inter :operates-on) poly (* 2 rank) :move)))))


;;; polyline-point-moving-inter:
;;; This is the interactor that moves the points.  It's waiting priority
;;; is high so that if the same button is used for both adding/moving a
;;; point, it'll move the point if we are using a handle.  The running
;;; action is for the case when someone is dragging a point and hits the
;;; delete key.  That point dies.  The abort action turns off the feedback
;;; line.  start-action and stop-action for this interactor are explained
;;; above.

(create-instance 'polyline-point-moving-inter inter:move-grow-interactor
  (:polyline-creator NIL)
  (:waiting-priority inter:running-priority-level)
  (:start-where
   (o-formula (list :list-element-of (gvl :polyline-creator)
		    :polyline-handles-being-used)))
  (:window (o-formula (gvl :polyline-creator :polyline-being-edited :window)))
  (:start-action #'polyline-point-moving-inter-start-action)

  ;; If someone hits the delete key while dragging a point, the point should
  ;; be deleted!
  (:running-action #'(lambda (inter handle loc)
		       (s-value (g-value inter :feedback-obj) :visible T)
		       (when (eq #\delete (inter:event-char inter:*current-event*))
			 (inter:abort-interactor inter)
			 (polyline-point-deleting-inter-stop-action inter handle))
		       (call-prototype-method inter handle loc)))
  (:stop-action #'polyline-point-moving-inter-stop-action)
  (:abort-action #'(lambda (inter handle)
		     ;; When handle has been added, remove it!
		     ;; This is inefficient, but since this case doesn't
		     ;; come up often, it's okay.
		     (when (g-value inter :added-handle)
		       (polyline-point-deleting-inter-stop-action
			inter (g-value inter :added-handle))
		       (s-value inter :added-handle NIL))
		     (s-value (g-value inter :feedback-obj) :visible NIL)
		     (call-prototype-method inter handle))))


;;; polyline-point-deleting-inter-stop-action:
;;; This is the stop action for the interactor that handles deleting
;;; a point.  Basically, we remove the handle from the
;;; :polyline-handles-being-used list, and remove the point from
;;; the :point-list.  We should put the handle for the deleted point
;;; in the :stored-polyline-handles list so that some other polyline
;;; can use that handle.
;;;
;;; We also have to be careful when we delete a point in
;;; a closed polyline, because a closed polyline should STAY CLOSED.
;;; Also, if there are only 2 points in the polyline, no more points
;;; can be deleted.

(defun polyline-point-deleting-inter-stop-action (inter handle)
  (when (g-value handle :visible)
    (let* ((poly (g-value handle :polyline))
	   (points (copy-list (g-value poly :point-list)))
	   (poly-creator (g-value poly :polyline-creator-obj))
	   (handles (copy-list (g-value poly-creator :polyline-handles-being-used)))
	   (len (length points))
	   (rank (g-value handle :rank)))

      ;; Polyline should have minimum 2 points
      (when (> (length handles) 2)

	;; If we are deleting the first or last point in
	;; a closed polyline, make sure it stays closed by
	;; putting a copy of the new first point at the
	;; end of the :point-list.
	(if (AND (g-value poly :closed-polyline-p)
		 (OR (eq rank 0) (eq (* 2 (1+ rank)) len)))
	    (progn
	      (setf handles (remove handle handles))
	      ;; have to remove handle from aggregate so we can reuse it.
	      ;; otherwise, we get the "already has a parent" error
	      (opal:remove-component (g-value poly :window :aggregate) handle)
	      (push handle (g-value poly-creator :stored-polyline-handles))
	      (setf points (subseq points 2 (- len 2)))
	      (setf points (append points (list (first points)
						(second points)))))
	    (progn
	      (setf handles (remove handle handles))
	      ;; have to remove handle from aggregate so we can reuse it.
	      ;; otherwise, we get the "already has a parent" error
	      (opal:remove-component (g-value poly :window :aggregate) handle)
	      (push handle (g-value poly-creator :stored-polyline-handles))
	      (setf points (append (subseq points 0 (* 2 rank))
				   (subseq points (+ 2 (* 2 rank)))))))

	;; s-value the changes so that update will notice it
	(s-value poly-creator :polyline-handles-being-used handles)
	(s-value poly :point-list points)
	(opal:update (g-value poly :window))

	;; call modify function with :delete UNLESS it a new handle has been
	;; added and control-g has been hit while dragging.
	(when (is-a-p inter polyline-point-deleting-inter)
	  (kr-send (g-value inter :operates-on) :modify-function
		   (g-value inter :operates-on) poly (* 2 rank) :delete))
	))))


;;; polyline-point-deleting-inter:
;;; This is the point interactor that deletes points.  It's just a non-
;;; continuous button interactor which deletes the point when it stops.
;;; The :stop-action function is explained above.

(create-instance 'polyline-point-deleting-inter inter:button-interactor
  (:continuous NIL)
  (:waiting-priority inter:high-priority-level)
  (:polyline-creator NIL)
  (:start-where
   (o-formula (list :list-element-of (gvl :polyline-creator)
		    :polyline-handles-being-used)))
  (:window (o-formula (gvl :polyline-creator :polyline-being-edited :window)))
  (:window (o-formula (gvl :polyline :window)))
  (:stop-action #'polyline-point-deleting-inter-stop-action))


;;; polyline-point-adding-inter-stop-action:
;;; This mutha is called by the point adding interactor.  To add a point,
;;; the user clicks on a line in the polyline.  Since the user can't
;;; ACCURATELY click on the line, there's a threshold so that if you click
;;; so many pixels around the line, it is interpreted as a click on the
;;; line.  Basically, we have to figure out between WHAT two points the
;;; new point lies.  Once we find that, we just insert the new point and
;;; the new handle for that point in the appropriate places in the :point-list
;;; and the :polyline-handles-being-used list.

(defun polyline-point-adding-inter-stop-action (inter poly)
  (when (g-value poly :polyline-creator-obj :polyline-handles-being-used)
    (let* ((x (inter:event-x inter:*current-event*))
	   (y (inter:event-y inter:*current-event*))
	   (thresh (g-value inter :threshold))
	   (points (copy-list (g-value poly :point-list)))
	   (len (length points))
	   (poly-creator (g-value poly :polyline-creator-obj))
	   (handles (copy-list (g-value poly-creator :polyline-handles-being-used)))
	   (rank NIL))
      
      ;; For each point in the polyline, if the new point lies between
      ;; it and the next point, we insert it at that place, updating the
      ;; :point-list and the :polyline-handles-being-used slot.  Then we
      ;; add the new handle to the window and get out of the loop.
      (dotimes (i (1- (/ len 2)))
	(when (opal::between-polyline-points
	       (nth (* 2 i) points) (nth (1+ (* 2 i)) points) x y
	       (nth (* 2 (1+ i)) points) (nth (1+ (* 2 (1+ i))) points) thresh)
	  (let ((new-handle
		 (if (g-value poly-creator :stored-polyline-handles)
		     (progn
		       (let ((handle-to-be-reused
			      (pop (g-value poly-creator :stored-polyline-handles))))
			 (s-value handle-to-be-reused :xy (list x y))
			 (s-value handle-to-be-reused :polyline-creator poly-creator)
			 handle-to-be-reused))
		     (create-instance NIL polyline-edit-handle
		       (:xy (list x y))
		       (:polyline-creator poly-creator)))))
	    (setf points (append (subseq points 0 (+ 2 (* 2 i)))
				 (list x y)
				 (subseq points (+ 2 (* 2 i)))))
	    (setf handles (append (subseq handles 0 (1+ i))
				  (list new-handle)
				  (subseq handles (1+ i))))
	    (s-value poly :point-list points)
	    (s-value poly-creator :polyline-handles-being-used handles)
	    (setf rank (* 2 (1+ i)))
	    ;; Add the handle to the window so that it's visible
	    (opal:add-component (g-value poly :window :aggregate) new-handle)
	    (opal:update (g-value poly :window))
	    
	    ;; When both the adding/moving interactors have the same 
	    ;; :start-event, we start the moving interactor.  This way,
	    ;; the user can create a point and move it immediately without
	    ;; releasing the mouse button.
	    
	    (if (eq (g-value inter :start-event)
		    (g-value poly :polyline-creator-obj :polyline-point-mover
			     :start-event))
		(progn
		  (s-value (g-value poly :polyline-creator-obj :polyline-point-mover)
			   :added-handle new-handle)
		  (inter:start-interactor (g-value poly :polyline-creator-obj
						   :polyline-point-mover)))

		;; If not moving point, call modify-function with :add action
		(kr-send (g-value inter :operates-on) :modify-function
			 (g-value inter :operates-on) poly rank :add))
	    
	    (return)))))))
      
;;; polyline-point-adding-inter-start-where:
;;; want the thing to start when you hit on an outline, since a click in the
;;; polyline is not necessarily a click on the outline of the polyline.  This
;;; function takes care of that.

(defun polyline-point-adding-inter-start-where (obj inter ev)
  ;; Gotta make sure event is in right window!
  ;; Then, check that it's on that polyline outline
  (if (and (eq (g-value obj :window) (inter:event-window ev))
	   (opal::point-in-polyline (inter:event-x ev) (inter:event-y ev)
				    (g-value obj :polyline-being-edited :point-list)
				    (g-value inter :threshold) T NIL))
      (g-value obj :polyline-being-edited)
      NIL))


;;; polyline-point-adding-inter:
;;; This is the interactor that adds points to the polyline.  It's non-
;;; continuous and all the work is done in the :stop-action, which is
;;; described above.

(create-instance 'polyline-point-adding-inter inter:button-interactor
  (:continuous NIL)
  (:waiting-priority inter:high-priority-level)  
  (:polyline-creator NIL)
  (:polyline (o-formula (gvl :polyline-creator :polyline-being-edited)))
  (:start-where (o-formula (list :custom (gvl :operates-on)
				 #'polyline-point-adding-inter-start-where)))
  (:window (o-formula (gvl :polyline :window)))
  (:stop-action #'polyline-point-adding-inter-stop-action))


;;; make-polyline-handles:
;;; This is the function that sets up the handles and the interactors.
;;; First, we check to see if a polyline is closed or not.  When we
;;; assign a handle to a point, we want to check the :stored-polyline-handles
;;; slot to see if there's any handles we can reuse.  If the :stored...
;;; list is empty, we create a new handle and put it in the
;;; :polyline-handles-being-used list.

(defun make-polyline-handles (poly-creator)
  (let ((poly (g-value poly-creator :polyline-being-edited)))
    ;; Decide whether the polyline is closed or not
    (polyline-closed-or-not poly)  
    (s-value poly :polyline-creator-obj poly-creator)
    
    ;; If polyline handles aren't already there, make them!
    (let* ((points (g-value poly :point-list))
	   (len (length points))
	   (closed (g-value poly :closed-polyline-p)))
      
      ;; If it is a closed polygon, don't create the last
      ;; handle, since that'll be 2 handles for one point!
      (dotimes (i (- (/ len 2)
		     (if closed 1 0)))
	(let* ((x (nth (* i 2) points))
	       (y (nth (1+ (* i 2)) points))

	       ;; Get handle from storage, else create new one.
	       (new-handle
		(if (g-value poly-creator :stored-polyline-handles)
		    (progn
		      (let ((handle-to-be-reused
			     (pop (g-value poly-creator :stored-polyline-handles))))
			(s-value handle-to-be-reused :xy (list x y))
			(s-value handle-to-be-reused :polyline-creator poly-creator)
			handle-to-be-reused))
		    (create-instance NIL polyline-edit-handle
		      (:xy (list x y))
		      (:polyline-creator poly-creator)))))

	  ;; update the :polyline-handles-being-used list and put the new handle
	  ;; in the aggregate so that it's visible
	  (push new-handle (g-value poly-creator :polyline-handles-being-used))
	  (opal:add-component (g-value poly :window :aggregate) new-handle)))

      ;; since we are pushing the handles in, the list is in REVERSE order, ie
      ;; the last handle on the list is for the first point.  So we reverse
      ;; the list so that the first handle in the list stands for the first
      ;; point, second handle for the second point, and so on (and so forth :)
      (s-value poly-creator :polyline-handles-being-used
	       (reverse (g-value poly-creator :polyline-handles-being-used))))

    (s-value poly-creator :mover-feedback-obj :visible NIL)
    (opal:add-component (g-value poly :window :aggregate)
			(g-value poly-creator :polyline-point-mover :feedback-obj))

    (opal:update (g-value poly :window))
    ))


;;; polyline-closed-or-not:
;;; If the polyline is closed (first & last points are equal),
;;; set the :closed-polyline-p slot to T

(defun polyline-closed-or-not (poly)
  (s-value poly :closed-polyline-p NIL)
  (let* ((points (g-value poly :point-list))
	 (len (1- (length points))))
    (when (AND (eq (first points)
		   (nth (1- len) points))
	       (eq (second points)
		   (nth len points)))
      (s-value poly :closed-polyline-p T))))


;;; hide-polyline-handles:
;;; We have to remove the handles from the aggregate first, and then
;;; remove the feedback object for the mover from the aggregate.
;;; Then, we push all the handles currently being used into storage.

(defun hide-polyline-handles (poly-creator)
  (let ((poly (g-value poly-creator :polyline-being-edited)))

    ;; Have to first remove the components from the agg -- the handles
    ;; and the durn feedback object
    (dolist (handle (g-value poly-creator :polyline-handles-being-used))
      (opal:remove-component (g-value poly :window :aggregate) handle))
    (when (member (g-value poly-creator :polyline-point-mover :feedback-obj)
		  (g-value poly-creator :window :aggregate :components))
      (opal:remove-component (g-value poly :window :aggregate)
			     (g-value poly-creator :polyline-point-mover :feedback-obj)))

    ;; Now put the stuff back into the storage
    (s-value poly-creator :stored-polyline-handles
	     (append (g-value poly-creator :stored-polyline-handles)
		     (g-value poly-creator :polyline-handles-being-used)))

    ;; Now set the :polyline-being-edited slot to NIL
    (s-value poly-creator :polyline-being-edited NIL)
    (s-value poly-creator :polyline-handles-being-used NIL)
    (opal:update (g-value poly :window))))


;;; toggle-polyline-handles:
;;; this function decides what object to show/hide polyline handles over, and
;;; calls make-polyline-handles, which makes the handles

(defun toggle-polyline-handles (polyline-creator-obj obj)
  (when (is-a-p obj opal:polyline)
  
    ;; Turn off feedback, otherwise, it follows you around!!
    (s-value (g-value polyline-creator-obj :mover-feedback-obj)
	     :visible NIL)
    (cond
      ;; If control-r over obj being edited, hide handles
      ((eq (g-value polyline-creator-obj :polyline-being-edited) obj)
       (gg::hide-polyline-handles polyline-creator-obj)
       (s-value polyline-creator-obj :polyline-being-edited NIL))
      
      ;; If control-r over different obj, hide current obj's handles
      ;; and display new obj's handles
      ((g-value polyline-creator-obj :polyline-being-edited)
       (gg::hide-polyline-handles polyline-creator-obj)
       (s-value polyline-creator-obj :polyline-being-edited obj)
       (gg::make-polyline-handles polyline-creator-obj))
      
      ;; Otherwise, just display the handles for current obj
      (T
       (s-value polyline-creator-obj :polyline-being-edited obj)
       (gg::make-polyline-handles polyline-creator-obj)))))

;;; destroy-polyline-handles:
;;; Destroys all the handles and interactors, and sets the
;;; slots to NIL, so that you can re-create the handles.
;;; If you don't set them to NIL, they contain the *destroyed*
;;; object and so, crashes.

(defun destroy-polyline-handles (creator &optional erase)
  (s-value creator :polyline-being-edited NIL)
  (dolist (handle (append
		   (g-value creator :polyline-handles-being-used)
		   (g-value creator :stored-polyline-handles)))
    (opal:destroy handle erase))
  
  (call-prototype-method creator erase))


;;  Tell the world that polyline-functions has been loaded
;;
(setf (get :garnet-modules :polyline-functions) T)
