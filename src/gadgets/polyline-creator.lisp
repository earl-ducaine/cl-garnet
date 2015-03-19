;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Polyline-Creator - a gadget to let the user enter a poly-line
;;;
;;;  User Interface:
;;;     1) User presses a button to start the interaction.  Each subsequent
;;;        button press causes a new segment to be added to the line.
;;;     2) Feedback is provided to the user.
;;;     3) Poly-Line stops when either:
;;; 		- the new point is close enough to the first point of the
;;;               polyline (in which case the polyline is closed)
;;; 		- the button pressed is different from the start event
;;;     4) The application might arrange that some other action (e.g. changing
;;;        the drawing mode using a palette) will stop or abort the polyline
;;;     5) Polylines can be edited by calling the (toggle-polyline-handles)
;;;        function.
;;;
;;;  Programmer Interface:
;;;     Customizable slots:
;;;       1) :selection-function - put a function here to be called with the
;;;          final point-list.  This function will typically create a new
;;;          poly-line using a COPY of the point-list (do not use the
;;;          point-list directly).  It is called with:
;;; 	       (lambda (gadget new-point-list)
;;; 	  2) :start-event - the event to start the whole process on,
;;; 	     default=:leftdown)
;;;       3) :start-where - where the mouse should be when the start-event
;;;          happens
;;;       4) :running-where - where run.  Default = T
;;;       5) :close-enough-value - how close (in pixels) the event needs to be
;;;          to	the first event to close the poly-line.  Default=3
;;;       6) :value - set by the gadget with the final point-list (do not set
;;;          this)
;;;       7) :active-p - whether you can use it or not
;;;
;;;       The following slots are for using the editor
;;;       8) :mover-start-event - which event starts moving a point
;;;       9) :mover-stop-event - which event stops moving a point
;;;      10) :adder-start-event - which event adds a point
;;;      11) :deleter-start-event - which event deletes a point
;;;      12) :threshold - how close to a line you have to click to add a point
;;;
;;;   Functions:
;;;      (Stop-Polyline-Creator gadget) - causes the gadget to create the
;;;          current object.  Ignores the current mouse position.
;;;          Useful if some other gadget (such as a palette changing
;;;          the drawing mode) wants to stop the gadget.  You can call
;;;          this even if the gadget is not operating. 
;;;      (Abort-Polyline-Creator gadget) - aborts the gadget without creating
;;;          the polyline.
;;;      (Toggle-Polyline-Handles gadget a-polyline) - turns on/off handles that
;;;          allow a-polyline to be edited.
;;;
;;;  Designed and written by Brad Myers


(in-package "GARNET-GADGETS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(Polyline-Creator Stop-Polyline-Creator Abort-Polyline-Creator
	    Toggle-Polyline-Handles Destroy-Polyline-Handles
	    Hide-Polyline-Handles))

  #+garnet-test
  (export '(Polyline-Creator-demo-go Polyline-Creator-demo-stop)))

(defun close-enough (point1 point2 close-enough-value)
  (and (<= (abs (- (car point1)(car point2))) close-enough-value)
       (<= (abs (- (cadr point1)(cadr point2))) close-enough-value)))

;;;You can call this to stop a running Polyline-Creator gadget and to use the points
;;; accumulated so far (but not the current cursor point) 
(defun Stop-Polyline-Creator (polyline-creator-gadget)
  (let ((feed-poly (g-value polyline-creator-gadget :feedback-polyline)))
    (inter:abort-interactor (g-value polyline-creator-gadget :first-inter))
    (when (g-value feed-poly :visible)
      ; then is running
      (s-value polyline-creator-gadget :value (g-value feed-poly :point-list))
      (kr-send polyline-creator-gadget :selection-function polyline-creator-gadget
	       (g-value feed-poly :point-list))
      (s-value feed-poly :visible NIL)
      (inter:abort-interactor (g-value polyline-creator-gadget :other-inter)))))

;; throw away the points so far
(defun Abort-Polyline-Creator (polyline-creator-gadget)
  (s-value (g-value polyline-creator-gadget :feedback-polyline) :visible NIL)
  (inter:abort-interactor (g-value polyline-creator-gadget :first-inter))
  (inter:abort-interactor (g-value polyline-creator-gadget :other-inter)))

    
(create-instance 'Polyline-Creator opal:aggregadget
  :declare ((:parameters :start-where :start-event :running-where
			 :close-enough-value :input-filter :selection-function)
	    (:type ((or list (member T)) :start-where :running-where)
		   ((or keyword character list) :start-event)
		   ((integer 0) :close-enough-value)
		   ((or list integer function symbol) :input-filter)
		   ((or null function symbol) :selection-function)))
  
  ;; user settable slots
  (:selection-function NIL) ; called when have full poly-line
  (:modify-function NIL) ; called when a polyline point is modified
  (:start-event :leftdown)
  (:start-where NIL)
  (:running-where T)
  (:close-enough-value 3)
  (:active-p T)
  (:input-filter NIL) ; described in twopoint-interactor
  
  ;; The following are slots to edit the polyline.
  
  (:mover-start-event :leftdown)         ; event to start mover
  (:mover-stop-event :leftup)            ; event to stop mover
  (:adder-start-event :leftdown)         ; event to start adder
  (:deleter-start-event :middledown)     ; event to start deleter
  (:threshold 3)                         ; how close to line to add a point
   
  ;; return value
  (:value NIL) ; set with final point list
  
  ;; internal slots
  (:first-point NIL) ; set with the initial point

  ;; this slot keeps track of polyline handles.  in order not to recreate
  ;; handles for each polyline, we keep only one set of handles.  when you
  ;; start editing a polyline, the handles are taken from storage, the :left
  ;; and :top slots are set appropriately, and the handles are put in the
  ;; :polyline-handles-being-used slot.
  ;;
  ;; when you are done editing a polyline, the handles from :p-h-b-used
  ;; will be copied back into the :stored-polyline-handles.  
  (:stored-polyline-handles NIL)
  (:polyline-handles-being-used NIL)

  ;; this contains the polyline that's being edited NOW, if any.
  (:polyline-being-edited NIL)

  ;; this is the destroy method
  (:destroy-me #'destroy-polyline-handles)
  (:parts `((:feedback-polyline ,opal:polyline
	     (:visible NIL)
	     (:point-list NIL)
	     (:draw-function :xor)
	     (:fast-redraw-p T)
	     (:line-style ,opal:dashed-line))
	    (:feedback-line ,opal:line
	     (:points '(0 0 0 0))
	     (:x1 ,(o-formula (first (gvl :points))))
	     (:y1 ,(o-formula (second (gvl :points))))
	     (:x2 ,(o-formula (third (gvl :points))))
	     (:y2 ,(o-formula (fourth (gvl :points))))
	     (:visible NIL)
	     (:draw-function :xor)
	     (:fast-redraw-p T)
	     (:line-style ,opal:dashed-line))))
  (:interactors
   `((:first-inter ,inter:two-point-interactor
      (:window ,(o-formula (gv-local :self :operates-on :window)))
      (:active ,(o-formula (and (gvl :window)
				(gvl :operates-on :active-p)
				(NOT (gvl :operates-on :polyline-being-edited)))))
      (:continuous T)
      (:start-event ,(o-formula (gvl :operates-on :start-event)))
      (:stop-event :any-mousedown)
      (:start-where ,(o-formula (gvl :operates-on :start-where)))
      (:running-where ,(o-formula (gvl :operates-on :running-where)))
      (:input-filter ,(o-formula (gvl :operates-on :input-filter)))
      (:line-p T)
      (:feedback-obj ,(o-formula (gvl :operates-on :feedback-line)))
      (:final-function 
       ,#'(lambda (inter point-list)
	    (let ((feed-poly (g-value inter :operates-on :feedback-polyline)))
	      (s-value feed-poly :point-list (copy-list point-list))
	      (s-value feed-poly :visible T)
	      (s-value (g-value inter :operates-on) :first-point
		       (list (car point-list) (cadr point-list)))
	      (s-value feed-poly :visible T)
	      (inter:start-interactor (g-value inter :operates-on
					       :other-inter))))))
     (:other-inter ,inter:two-point-interactor
      (:window ,(o-formula (gv-local :self :operates-on :window)))
      (:start-where T)
      (:continuous T)
      (:line-p T)
      (:feedback-obj ,(o-formula (gvl :operates-on :feedback-line)))
      (:input-filter ,(o-formula (gvl :operates-on :input-filter)))
      (:start-event NIL)
      (:running-where ,(o-formula (gvl :operates-on :running-where)))
      (:stop-event :any-mousedown)
      (:final-function 
       ,#'(lambda (inter point-list)
	    (let* ((obj (g-value inter :operates-on))
		   (feed-poly (g-value obj :feedback-polyline))
		   (first-point (g-value obj :first-point))
		   (new-point (list (third point-list)(fourth point-list)))
		   (close-enough-p (close-enough first-point new-point
						 (g-value obj :close-enough-value))))
	      ;; add in this point to object
	      (when close-enough-p ; use first-point instead of new point
		(setq new-point (copy-list first-point)))
	      (s-value feed-poly :point-list 
		       (nconc (g-value feed-poly :point-list) new-point))
	      (mark-as-changed feed-poly :point-list)
	      
	      ;; check whether quit or not
	      (if (or (not (eq (inter:event-char inter:*Current-event*)
			       (g-value obj :start-event)))
		      close-enough-p)
					; then stop
		  (progn
		    ;; if double click, make sure last two pts are not
		    ;; same!
		    (let* ((pts (copy-list (g-value feed-poly :point-list)))
			   (last (1- (length pts))))
		      (when (AND (eq (nth last pts)
				     (nth (- last 2) pts))
				 (eq (nth (1- last) pts)
				     (nth (- last 3) pts)))
			(s-value feed-poly :point-list
				 (subseq pts 0 (1- last)))))

				 
		    (s-value obj :value (g-value feed-poly :point-list))
		    (kr-send obj :selection-function obj
			     (g-value feed-poly :point-list))
		    (s-value feed-poly :visible NIL))
					; else go on
		  (inter:start-interactor inter))))))  ; restart me

     ;; This is the inter that turns editing off if there's a click outside
     (:outside-click-inter ,inter:button-interactor
      (:polyline-creator ,(o-formula (gvl :operates-on)))
      (:start-where ,(o-formula (list :in (gvl :polyline-creator :window))))
      (:start-event :any-mousedown)
      (:window ,(o-formula (gvl :operates-on :window)))
      (:continuous NIL)
      (:final-function ,#'(lambda(inter obj)
			    (declare (ignore obj))
			    (when (g-value inter :operates-on :polyline-being-edited)
			      (gg::hide-polyline-handles (g-value inter :operates-on))
			      (when (eq
				     (inter:event-char inter:*current-event*)
				     (g-value inter :operates-on :first-inter :start-event))
				(inter:start-interactor
				 (g-value inter :operates-on :first-inter)))))))
     
     ;; This is the interactor that MOVES the polyline stuff
     (:mover-feedback-obj ,polyline-mover-feedback)
     (:polyline-point-mover ,polyline-point-moving-inter
      (:active ,(o-formula (gvl :operates-on :polyline-being-edited)))
      (:input-filter ,(o-formula (gvl :operates-on :input-filter)))
      (:polyline-creator ,(o-formula (gvl :operates-on)))
      (:start-event ,(o-formula (gvl :operates-on :mover-start-event)))
      (:stop-event ,(o-formula (gvl :operates-on :mover-stop-event)))
      (:feedback-obj ,(o-formula (gvl :operates-on :mover-feedback-obj))))

     ;; This is the interactor that DELETES the points
     (:polyline-point-deleter ,polyline-point-deleting-inter
      (:active ,(o-formula (gvl :operates-on :polyline-being-edited)))
      (:start-event ,(o-formula (gvl :operates-on :deleter-start-event)))
      (:polyline-creator ,(o-formula (gvl :operates-on))))
     
     ;; This is the interactor that ADDS points
     (:polyline-point-adder ,polyline-point-adding-inter
      (:active ,(o-formula (gvl :operates-on :polyline-being-edited)))
      (:start-event ,(o-formula (gvl :operates-on :adder-start-event)))
      (:threshold ,(o-formula (gvl :operates-on :threshold)))
      (:polyline-creator ,(o-formula (gvl :operates-on))))
     
     )))

;;; from here down is the test program.

#+garnet-test
(defparameter multi-fill-val 3)

#+garnet-test
(defun Polyline-Creator-Demo-go ()
  (create-instance 'polyline-creator-win inter:interactor-window
	   (:aggregate (create-instance 'polyline-creator-agg opal:aggregate)))
				       
  (opal:update polyline-creator-win)

  (opal:add-component polyline-creator-agg 
	(create-instance 'Polyline-Creator-Obj Garnet-Gadgets:Polyline-Creator
		 (:start-where `(:in ,polyline-creator-win))
		 (:deleter-start-event :double-leftdown)
		 (:selection-function
		  #'(lambda (gadget new-points)
		     (declare (ignore gadget))
		     (let ((new-polyline
			    (create-instance NIL opal:polyline
				  (:point-list (copy-list new-points))
				  (:filling-style (case multi-fill-val
						    (0 opal:no-fill)
						    (1 opal:black-fill)
						    (2 opal:white-fill)
						    (3 opal:light-gray-fill)
						    (4 opal:gray-fill)
						    (T opal:dark-gray-fill)))
				  (:line-style opal:line-2))))
		       (opal:add-component polyline-creator-agg new-polyline)
		       (incf multi-fill-val)
		       (when (>= multi-fill-val 6) (setq multi-fill-val 0))
		       (format T "Created new polyline ~S~%" new-polyline)
		       )))))
  ;; This is the interactor that STARTS editing the polyline
  (create-instance 'edit-polyline-inter inter:button-interactor
    (:operates-on polyline-creator-obj)
    (:active T)
    (:start-where (list :element-of polyline-creator-agg))
    (:window polyline-creator-win)
    (:start-event :control-\r)
    (:continuous NIL)
    (:final-function #'(lambda(inter obj)
			 (toggle-polyline-handles (g-value inter :operates-on) obj))))
    
    

  (create-instance 'stop-abort-multi inter:button-interactor
	 (:continuous NIL)
	 (:start-where `(:in ,polyline-creator-win))
	 (:window polyline-creator-win)
	 (:start-event :any-keyboard :except :control-\r)
	 (:waiting-priority inter:running-priority-level)
	 (:final-function
	  #'(lambda (inter obj)
	     (declare (ignore obj))
	     (if (eq (g-value inter :start-char) :control-\g)
	       (Garnet-Gadgets:Abort-Polyline-Creator Polyline-Creator-Obj))
	     (if (eq (g-value inter :start-char) #\f)
	       (Garnet-Gadgets:Stop-Polyline-Creator Polyline-Creator-Obj)))))

  
  (format T "~%   Press with the left mouse button to start creating a polyline.~%")
  (format T "Each press adds a segment.  Pressing with a different mouse button~%")
  (format T "immediately finishes the polyline.  You can also finish the polyline~%")
  (format T "by clicking the left mouse button near the start point.  ^G aborts~%")
  (format T "the current object, and typing f finishes the polyline from the last~%")
  (format T "point clicked with the mouse.~%~%")

  (format T "   To edit a new polyline, put the pointer over the polyline and~%")
  (format T "type ^R.  Selection handles will appear, which you can drag.  Pressing~%")
  (format T "the left mouse button on a line segment will create a new point.~%~%")

  polyline-creator-win)

#+garnet-test
(defun Polyline-Creator-Demo-Stop ()
  (opal:destroy polyline-creator-win))
