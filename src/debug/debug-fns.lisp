;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-DEBUG; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

#|
ChangeLog:
  13 Oct 93 R J Williams - Look for inherited slots in DOSLOTS calls
  14 Sep 93 Andrew Mickish - Removed :x-substr case from opal::legal-type-p
  28 May 93 Dario Giuse - replace kr internals with calls to g-formula-value.
  23 Mar 93 Brad Myers - added new debug functions break-on-slot-set and
	  notify-on-slot-set and clear-slot-set, call-func-on-slot-set
  20 Jan 93 Mickish Cleared input buffer before IDENT's real event-case;
                    Added schema-p checks in IDENTIFY.
  18 Jan 93 Brad Myers - fixed Look-inter and added :next option
                       - changed output of std-proto to add package name
  10 Dec 92 Mickish *drawable-to-window-mapping* ---> *garnet-windows*
  21 Apr 92 Pervin Using new function opal:main-event-loop-process-running-p
  14 Apr 92 Pervin Got multi-process stuff to work on HP.

   6 Apr 92  DZG  Fixed explain-slot for release 2.0 of KR.

  27 Mar 92 Pervin    In ident, kill and re-launch the main-event-loop-process.
		      In std-proto, check that schema-name is symbol.

  27 Mar 92 amickish  Removed format statements from tests for conditionals in
                      flash because they rendered the code body unreachable;
                      Added ~A in format string of explain-nil.

  25 Mar 92 amickish  Get-Values--->G-Value;  Get-Local-Values--->G-Local-Value

  30 Oct 89  RBD  added direct-clx versions of flash and ident

  18 Oct 89  RBD  changed fix-up-aggregate and just-remove-component to
                  test for null objects,
                  fixed ident to return leaf elements (bug fix),
                  test for null objects in :start-where in look-inter,
                  declared flash-object.
  11 Dec 89  ECP  uncommented create-flash-object

  25 Jan 90  RBD  fixed code to use get-local-values to avoid "inheriting"
                  components,
                  print prompt to user when ident is started
                  improved test for bad objects in fix-up-window

  29 Jan 90  RBD  test for window in FLASH

  12 Mar 90  RBD  modified IDENT to return list of useful values,
                  fixed std-parent to take NIL, renamed to std-proto.

  14 Mar 90  RBD  fixed IDENT to not return when user types shift or ctrl.

  16 Apr 90  ECP  Got rid of defvars of previously defined objects
		  like opal:aggregate.
		  Took defun of fix-up-aggregate and fix-up-window
		  out of let statement.

  31 May 90  RBD  fixed explain-slot by properly accessing :kr-depends-on slot.
                  look now prints interactors of aggregadgets and aggrelists

  27 Jun 90  RBD  look-inter now prints all active interactors that operate
                  in a window if the first argument is a window

  12 Jul 90  RBD  minor change in output from IDENT

  31 Jul 90  RBD  look-inter now looks at :final-feed-avail, -inuse, and -obj slots
  
  12 Mar 91  ECP  Gave flash an optional argument: the number of blinks

  11 Dec 91  Amickish  Converted to new KR:  Added 'car' to :is-a references,
                       changed kr::name-accessor to kr::schema-name
|#

(in-package "GARNET-DEBUG")

(eval-when (eval load compile)
  (export '(explain-short explain-slot explain-nil
	    fix-up-window flash ident invert 
	    is-a-tree kids look look-inter 
	    uninvert what where #-apple windows break-on-slot-set
	    notify-on-slot-set clear-slot-set call-func-on-slot-set)))


(defvar flash-object)
(defvar blink-gc nil)


;;; blink-rectangle -- used by flash
;;;
(defun blink-rectangle (number-of-blinks win agg left top width height)
  (declare (ignore agg))
  (let ((window (g-value win :drawable)))
    (cond ((null window)
	   (format t "No :drawable for ~A~%" win)
	   (return-from blink-rectangle)))
#| 
    ;;; this is the opal-based blinking code
    ;; if flash-object does not exist yet, then create it
    (create-flash-object)
    
    (s-value flash-object :left left)
    (s-value flash-object :top top)
    (s-value flash-object :width width)
    (s-value flash-object :height height)
    (dotimes (n number-of-blinks)
      (add-component agg  flash-object)
      (update win)
      (sleep 0.1)
      (remove-component agg flash-object)
      (update win))
|#

    (dotimes (n number-of-blinks)
      #+apple
      (ccl:without-interrupts
       (ccl:with-focused-view window
         (gem:draw-rectangle win left top width height :xor NIL opal:black-fill)))
      #-apple
      (gem:draw-rectangle win left top width height :xor NIL opal:black-fill)
      (gem:flush-output win)
      (sleep 0.1))
    (setf blink-gc nil) ))


;;; create-flash-object -- create xor object unless it exists already
;;;
(defun create-flash-object ()
  (let (agg win)
    (cond ((or (null (boundp 'flash-object))
	       (null (schema-p flash-object)))
	   (create-instance 'flash-object opal:rectangle
			    (:draw-function :xor)
			    (:name "debug-opal's flash object")
			    (:filling-style opal:black-fill))))

    ;; if flash-object is already in use, remove it from parent
    (setf agg (g-value flash-object :parent))
    (setf win (g-value flash-object :window))
    (cond (agg
	   (remove-component agg flash-object)))
    (cond (win
	   (update win)))))



;;; EXPLAIN-SHORT -- tell what a slot depends upon
;;;
(defun explain-short (object slot) (explain-slot object slot 0 nil 2))

;; use this to avoid circularities and printing shared dependencies
(defvar explain-visited)



;;; EXPLAIN-SLOT -- tell why a slot has a value
;;;
(defun explain-slot (object slot &optional (indent 0) (use-inverse-method nil) 
			    (n-levels 0))
  (setf explain-visited nil)  ;start with an empty list
  (explain-2 object slot indent use-inverse-method n-levels)
  (setf explain-visited nil)  ;free storage for gc
  )


;;; explain-2 -- look for dependents, avoid circularities
;;;
(defun explain-2 (object slot indent use-inverse-method n-levels)
  (let ((pair (cons object slot)))
    (cond ((member pair explain-visited :test #'equal)
	   (indent-by indent)
	   (format t "~S's ~S -- already printed~%"
		   object slot)
	   (return-from explain-2 nil))
	  (t
	   (push pair explain-visited))))
  (let ((value (get-value object slot))
	depends-on)
    (indent-by indent)
    (cond ((formula-p value)
	   (let ((*print-pretty* nil))
	     (format t "~S's ~S is " object slot)
	     (print-a-slot object slot)
	     (format t ",~%")
	     (cond ((not (kr::cache-is-valid value))
		    (indent-by indent)
		    (format t 
 "(warning: this formula is invalid, so dependencies may be incorrect)~%"))))
	   (setf indent (+ indent 1))
	   (cond ((/= indent n-levels)   ;; stop recursion when ident = n-levels
		  (indent-by (1- indent))
		  (format t "which depends upon:~%")
		  (cond (use-inverse-method  ;; don't trust :depends-on
			 (dolist (win opal::*garnet-windows*)
			   (let ((agg (g-value win :aggregate)))
			     (if agg
				 (find-depended agg value indent))))
			 #|
			 (maphash
			  #'(lambda (x-window opal-window)
			      (declare (ignore x-window))
			      (let ((agg (g-value opal-window :aggregate)))
				(cond (agg
				       (find-depended agg value indent)))))
				  opal::*drawable-to-window-mapping*)
                         |#
			 )
			((setf depends-on (g-formula-value value :DEPENDS-ON))
			 (find-dependencies depends-on value indent n-levels))
			(t ;; couldn't find :kr-depends-on
			 (indent-by indent)
			 (format t "No dependencies recorded.~%"))))
		 (t ;; stop recursion but indicate something missing
		  (indent-by indent)
		  (format t "...~%"))))
	  (t  ;; not a formula, just print the value
	   (format t "~S's ~S is " object slot)
	   (print-a-slot object slot)
	   (format t "~%"))))
    NIL)



;;; EXPLAIN-NIL -- why did the update fail?
;;;
(defun explain-nil ()
  (let (schema
	slot)
    (cond ((formula-p kr::*last-formula*)
	   (setf schema (g-formula-value kr::*last-formula* :SCHEMA))
	   (setf slot (g-formula-value kr::*last-formula* :SLOT))
	   (format
	    t "~A~%~A~A~A~%~A~%~A~%~A~A~A~S~A~A.~%~A~A~A~A~%     ~A~%"
	    "Assuming that you just trapped into the debugger and you"
	    "   got a message like 'Wrong type argument, "
	    (kr::cached-value kr::*last-formula*) ", ...',"
	    "   OR you got a message 'bad formula: ...' then"
	    "   maybe a call to GV in a formula failed.  If so, the formula"
	    "   was " kr::*last-formula* " which is in the "
	    slot " slot of " schema
	    "   Formula " kr::*last-formula* " is: "
	    (g-formula-value kr::*last-formula* :LAMBDA)
	    " The dependencies (one of which is probably a null link) are:")
	   (if (schema-p schema)
	     (explain-slot schema slot 0 t)
	     (format t " formula's :schema-slot is ~S !~%" schema))
	   (setf kr::*last-formula* nil))
	  (t
	   (format t "No errors in formula evaluation detected~%")))))


;;; find-depended -- search window for back-links
;;;
(defun find-depended (aggregate formula indent)
  (doslots (slot aggregate T)
    (let ((depended-slots (kr::get-dependents aggregate slot)))
      (if (member formula depended-slots)
	(explain-2 aggregate slot indent t 0))))
  (dolist (child (g-local-value aggregate :components))
    (find-depended child formula indent)))


;;; find-dependencies -- search objects for back-links
;;;
(defun find-dependencies (obj-list formula indent level)
  (kr::do-one-or-list (obj obj-list)
    (doslots (slot obj T)
      (let ((dependents (kr::get-dependents obj slot)))
	(if (if (listp dependents)
	      (member formula dependents)
	      (eq formula dependents))
	  (explain-2 obj slot indent nil level))))))


(defvar there-is-a-problem NIL)

;;; fix-up-aggregate -- recursive function to implement fix-up-window
;;;
(defun fix-up-aggregate (agg)
  (let ((comps (g-local-value agg :components)))
    (dolist (comp comps)
      (cond ((null comp)
	     (format t "~S has NIL for a component.~%" agg)
	     (cond ((y-or-n-p "Do you want me to try to remove it? ")
		    (just-remove-component agg comp))
		   (t
		    (setf there-is-a-problem t))))
	    ((is-a-p comp opal:aggregate)
	     (fix-up-aggregate comp))
	    ((test-display-slots comp))
	    (t
	     (format t "~S has bad slots.~%" comp)
	     (cond ((y-or-n-p "Do you want me to try to remove it? ")
		    (just-remove-component agg comp))
		   (t
		    (setf there-is-a-problem t))))))))


		       
;;; FIX-UP-WINDOW -- help user fix a window that won't update
;;;
(defun fix-up-window (win)
  (let ((agg (g-value win :aggregate))
	(invalid-objects
	 (opal::win-update-info-invalid-objects
	  (g-value win :win-update-info))) 
	bad-objects)
    (setf there-is-a-problem nil)

    ;; check for null invalid objects
    (dolist (object invalid-objects)
      (cond ((and (null (get-local-value object :update-info))
		  (progn
		    (format t "~A has no :update-info. " object)
		    (y-or-n-p "Remove from invalid-objects? ")))
	     (pushnew object bad-objects))))
    (cond (bad-objects
	   (dolist (object bad-objects)
	     (setf invalid-objects (delete object invalid-objects)))
	   (setf (opal::win-update-info-invalid-objects 
		  (g-value win :win-update-info)) 
		 invalid-objects)))

    ;; check aggregate tree
    (cond ((null agg));; null agg is ok
	  ((is-a-p agg opal:aggregate)
	   (fix-up-aggregate agg))
	  (t;; :aggregate is not an aggregate!
	   (format t "The window's aggregate ~S is not an opal:aggregate~%"
		   agg)
	   (cond ((y-or-n-p "Do you want me to remove it? ")
		  (s-value win :aggregate nil)))))
    (cond ((null there-is-a-problem)
	   (opal:update win t)))))



;;; FLASH -- blink where an object is located
;;;
(defun flash (obj &optional (num-blinks 10))
  (let ((win (g-value obj :window))
	(left (g-value obj :left))
	(top (g-value obj :top))
	(width (g-value obj :width))
	(height (g-value obj :height))
	(minsize 10)
	agg  winwidth winheight
	aggleft aggtop aggwidth aggheight
	invisible-agg)
    
    (cond (win
	   (setf agg (g-value win :aggregate))
	   (setf winwidth (g-value win :width))
	   (setf winheight(g-value win :height))))
    (cond (agg
	   (setf aggleft (g-value agg :left))
	   (setf aggtop (g-value agg :top))
	   (setf aggwidth (g-value agg :width))
	   (setf aggheight (g-value agg :height))))
    (cond ((null win)
	   (format t "~S does not have a window~%" obj))
	  ((null agg)
	   (format t "The window of ~S does not have an aggregate~%" obj))
	  ((not (and left top width height))
	   (format t "~S does not have a left, top, width, and height:~%" obj)
	   (where obj))
	  ((and (not (eq win obj));; this is not a window
		(or
		 (< (+ top height) 0);; above the window
		 (< (+ left width) 0);; left of the window
		 (> top winheight);; below the window
		 (> left winwidth)));; right of the window
	   (format t "~S is outside of it's window:~%" obj)
	   (where obj))
	  ((null (g-value win :visible))
	   (format t "~S's window ~S's :visible slot is NIL~%" obj win))
	  ((null (g-value obj :visible))
	   (format t "~S's :visible slot is NIL~%" obj))
	  ((setf invisible-agg (search-invisible-agg agg))
	   (format t "Aggregate ~S's :visible slot is NIL~%" invisible-agg))
	  ((not (and aggleft aggtop aggwidth aggheight))
	   (format t
		   "~S's aggregate does not have a left, top, width, and height:~%"
		   obj)
	   (where agg))
	  ((and (not (eq obj win));; not flashing the window
		(or
		 (< top aggtop)
		 (< left aggleft)
		 (> (+ top height) (+ aggtop aggheight))
		 (> (+ left width) (+ aggleft aggwidth))))
	   (format t "~S is at least partially outside of its aggregate ~S~%"
		   obj agg)
	   (where obj)
	   (where agg))
	  (t
	   (cond ((eq obj win)
		  (setf left 0)
		  (setf top 0))) 
	   (cond ((or (< width minsize) (< height minsize))
		  (format 
		   t "Enlarging feedback to ~D by ~D because ~S is small:~%" 
		   (max width minsize) (max height minsize)
		   obj)
		  (where obj)))
	   (cond ((null (g-value obj :visible))
		  (format t "~S is not currently visible." obj)))
	   (blink-rectangle num-blinks win agg left top
			    (max width minsize) (max height minsize))))))


;;; get-an-opal-window -- find any opal window and return it
;;;
(defun get-an-opal-window ()
  (car opal::*garnet-windows*)
  #|
  (maphash #'(lambda (x-window opal-window)
	       (declare (ignore x-window))
	       (return-from get-an-opal-window opal-window))
	   opal::*drawable-to-window-mapping*)
  |#
  )


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



;;; IDENT -- identify an object by pointing
;;;
(defvar ident)  ;; ident gets the value you point to

#|
;;; this ident is based on interactors, and is superseded by the clx
;;; implementation below
(defun ident ()
  (cond ((or (not (boundp 'identify-interactor))
	     (not (schema-p identify-interactor)))
	 (create-identify-interactor)))
  ;; make sure the interactor is not still running -- this may
  ;;  not be necessary but it won't hurt
  (cond ((g-value identify-interactor :active)
	 (inter:change-active identify-interactor nil)))
  ;; start the interactor
  (inter:change-active identify-interactor t)
  (format t "Click or Type on any object or window...~%"))
|#

;;; create-identify-interactor -- create interactor for IDENT function
;;;
(defun create-identify-interactor ()
  (format t "making identify-interactor~%")
  (create-instance 
      'identify-interactor
      inter:button-interactor
    (:window t)
    (:active nil)
    (:continuous nil)
    (:waiting-priority inter:high-priority-level)
    (:start-where
     (o-formula (list :leaf-element-of-or-none
		      (gvl :current-window :aggregate))))
    (:feedback-obj nil)
    (:self-deactivate t)
    (:start-event '(:any-mousedown :any-mouseup :any-keyboard))
    (:stop-action 'identify-stop-action)))


;;; Identify-stop-action -- called by interactor to tell user about object
;;;
(defun identify-stop-action (interactor obj)
  (declare (ignore interactor))
  (let ((x (inter:event-x inter:*Current-Event*))
	(y (inter:event-y inter:*Current-Event*))
	(win (inter:event-window inter:*Current-Event*))
	(ch (inter:event-char inter:*Current-Event*))
        (indent 0) last-obj)
    (format t "in Window ~A, ~S at x=~D, y=~D:~%" win ch x y)
    (setf ident obj)
    (cond ((eq ident :none)
	   (setf ident win)))
    (setf obj ident)
    (format t "~S~%" obj)
    (do ()
	((null obj))
      (cond ((> indent 0)
	     (indent-by indent)
	     (format t "in ")))
      (what obj)
      (setf last-obj obj)
      (setf obj (g-value obj :parent))
      (setf indent (1+ indent)))
    (indent-by indent)
    (format t "in ")
    (what (g-value last-obj :window))
    ;; (inter:change-active interactor nil) -- :self-deactivate does this
    ))

#+apple
(defvar ident-info ())

#+apple
(defun mac-ident-event-handler ()
  ;; The WHAT field of *current-event* is 0 unless something like a keyboard
  ;; or mouse-click event is being handled -- see Inside Mac I-249
  (let* ((what (ccl:rref ccl:*current-event* :EventRecord.what))
         (mousedown? (eql what 1))
         (keydown? (eql what 3)))
    (when (or mousedown? keydown?)
      ;; Convert coordinates from global to event-window
      (let* ((where (ccl:rref ccl:*current-event* :EventRecord.where))
             (time (ccl:rref ccl:*current-event* :EventRecord.when))
             (view (ccl:find-view-containing-point NIL where))
             (wptr (ccl:wptr view))
             (top-window (ccl:window-object wptr))
             
             #|
             (opal-window (gem:window-from-drawable
                           gem::*root-window* gem::*active-drawable*))
             (where (ccl::%global-to-local
                     wptr (ccl:rref ccl:*current-event* :EventRecord.where)))
             |#
             )
        (setf where (ccl::%global-to-local wptr where))
        (unless (eq view top-window)
          (setf where (ccl:convert-coordinates where top-window view)))
        (let ((x (ccl:point-h where))
              (y (ccl:point-v where)))
          (setf ident-info (list view what time x y)))))))

(defun ident (&optional (verbose t))
  "Wait for user to point to a garnet object and returns (obj window x y code). 
  Use (ident nil) to suppress normal printout"
  (let ((a-window (get-an-opal-window))
        suspend-process
	opal-display display-info obj window loc-x loc-y garnet-code)
    (cond (a-window
	   (if verbose
	     (format t "Click or Type on any object or window...~%"))
           (when (opal:main-event-loop-process-running-p)
	     (setf suspend-process T)
	     (opal:kill-main-event-loop-process))
	   (setf opal-display (the opal::DISPLAY-INFO
				   (g-value a-window :display-info)))
	   (setf display-info (opal::display-info-display
			       opal-display))
         #+apple
         (unwind-protect
           (setf ident-info ())
           (gu:while (eq ident-info ())
             (setf ccl:*eventhook* 'mac-ident-event-handler))
           (setf ccl:*eventhook* NIL)
           (let* ((view (first ident-info))
                  (what (second ident-info))
                  (time (third ident-info))
                  (code (case what
                          (1 inter::*left-button*)
                          (3 (logand (ccl:rref ccl:*current-event*
                                               :EventRecord.message)
                                     #xFFFF))))
                  (state (gem::get-event-bits))
                  (x (fourth ident-info))
                  (y (fifth ident-info)))
             (setf window (gem:window-from-drawable gem::*root-window* view))
             (setf garnet-code
                   (case what
                     (1 (gem:translate-mouse-character
                         gem::*root-window* code state :BUTTON-PRESS))
                     (3 (gem:translate-character gem::*root-window*
                                                 x y state code time))))
             (setf loc-x x) (setf loc-y y)
             (setf obj (identify window x y garnet-code verbose)))
           )

         #-apple
         (progn
	   ;; first, throw away any pending events
	   (xlib:event-case (opal::*default-x-display* :discard-p t :timeout 1)
			    (:destroy-notify () NIL)  ; get rid of warnings
			    (otherwise () t))
	   (xlib:event-case
	    (display-info :discard-p t :force-output-p t)
	    (button-press
	     (event-window x y state code event-key)
	     (setf window (getf (xlib:drawable-plist event-window) :garnet))
	     (setf loc-x x)
	     (setf loc-y y)
	     (setf garnet-code (gem:translate-mouse-character
				gem::*root-window* code state event-key))
	     (setf obj (identify window x y garnet-code verbose))
	     t)
	    (key-press
	     (event-window x y state code)
	     (setf window (getf (xlib:drawable-plist event-window) :garnet))
	     (setf loc-x x)
	     (setf loc-y y)
	     (setf garnet-code (interactors::translate-character
				(opal::display-info-display
				 (the opal::DISPLAY-INFO
				      (g-value window :display-info)))
				code state))
	     (cond (garnet-code
		    (setf obj (identify window x y garnet-code verbose))
		    t);; exit the event loop
		   (t nil);; must have been shift key, etc.  Loop for more.
		   ))))
	   (when suspend-process
	     (opal:launch-main-event-loop-process))
	   ))
    (list obj window loc-x loc-y garnet-code)))



;;; identify -- find where an event is and print event code and x,y
;;;
(defun identify (window x y code verbose)
  (let (elem agg (indent 0) ident)
    (if verbose
      (format t "in Window ~A, ~S at x=~D, y=~D:~%" window code x y))
    (if (schema-p window) (setf agg (g-value window :aggregate)))
    (cond ((schema-p agg)
	   (setf elem
		 (if (opal:point-in-gob agg x y)
		   (kr-send agg :point-to-leaf agg x y)
		   nil))
	   (setf ident elem)
	   (cond ((and verbose elem)
		  (do ()
		      ((null elem))
		    (cond ((> indent 0)
			   (indent-by indent)
			   (format t "in ")))
		    (what elem)
		    (setf elem (g-value elem :parent))
		    (setf indent (1+ indent)))
		  (indent-by indent)
		  (format t "in ")
		  (what window)))))
    ident))


;;; indent-by -- indentation function used for printing trees
;;;
(defun indent-by (indent)
  (dotimes (i indent) (format t "   "))
  (* indent 3))


;;; INVERT -- invert a selected object using xor
;;;
(defun invert (obj)
  (let ((win (g-value obj :window))
	(left (g-value obj :left))
	(top (g-value obj :top))
	(width (g-value obj :width))
	(height (g-value obj :height))
	agg)
    (create-flash-object)

    (setf agg (g-value win :aggregate))
    (cond ((and agg left top width height)
	   (s-value flash-object :left left)
	   (s-value flash-object :top top)
	   (s-value flash-object :width width)
	   (s-value flash-object :height height)
	   (add-component agg flash-object)
	   (update win)))))



;;; IS-A-TREE -- print the is-a links recursively
;;;
(defun is-a-tree (obj &optional (indent 0))
  (let (isa)
    (indent-by indent)
    (format t "~S" obj)
    (setf isa (car (g-value obj :is-a)))
    (cond (isa
	   (format t " is-a~%")
	   (is-a-tree isa (1+ indent)))
	  (t
	   (format t "~%")))))



;;; just-remove-component -- remove component without graphical fixup
;;;
(defun just-remove-component (agg obj)
  (s-value agg :components
	   (delete obj (g-local-value agg :components) :from-end t :count
		   1))
  (if (and obj (schema-p obj))
      (s-value obj :parent nil))
  (mark-as-changed agg :components))



;;; KIDS -- tell the opal type of an object and children
;;;
(defun kids (object &key (indent 0))
  (look object 1 :indent indent))



;;; This is a type-checking facility, called any time an object's slot has
;;; changed its value.  It's only invoked if *opal-debug* is non-NIL.
;;; All boolean slots (:visible :fast-redraw-p :open-p :actual-heightp)
;;; cannot be type-checked, since any non-NIL value is True.

(defun opal::legal-type-p (object slot value)
  (if
   (case slot
    ((:top :left :x1 :x2 :y1 :y2 :head-x :head-y :from-x :from-y)
	(typep value 'integer))
    ((:width :height :radius :draw-radius :length :diameter)
	(and (typep value 'integer) (>= value 0)))
    (:line-style
	(or (null value)
	    (and (schema-p value)
	         (is-a-p value opal:line-style))))
    (:filling-style
	(or (null value)
	    (and (schema-p value)
	         (is-a-p value opal:filling-style))))
    (:draw-function
	(assoc value opal::*function-alist*))
    ((:angle1 :angle2)
	(numberp value))
    (:point-list
	(and (listp value)
	     (zerop (mod (length value) 2))		;; of even length?
	     (not (dolist (coord value)
		    (unless (typep coord 'integer) (return T))))))
    ((:string :title :icon-title)
	(or (null value) (stringp value)))
    (:font
	(and (schema-p value)
	     (or (is-a-p value opal:font)
		 (is-a-p value opal:font-from-file))))
    (:xfont
     #-apple (xlib:font-p value)
     #+apple (listp value))
    (:text-extents
	(listp value))
    (:cursor-index
	(or (null value)
	    (and (typep value 'integer) (>= value 0))))
    (:justification
	(member value '(:left :center :right)))
    (:cut-strings
	(and (listp value)
	     (not (dolist (cut-string-member value)
		    (unless (opal::cut-string-p cut-string-member) (return T))))))
    (:image			;; bitmap
     #-apple (xlib::image-p value)
     #+apple (eq gem::*MAC-BUFFER* (class-of value)))
    (:aggregate
	(or (null value)
	    (is-a-p value opal:aggregate)))
    (:parent			;; window's can only have window's!
	(or (null value)
	    (and (schema-p value)
		 (if (is-a-p object opal::window)
		     (is-a-p value opal::window)
		     T))))
    (:cursor
	(and (listp value)
	     (is-a-p (car value) opal:bitmap)
	     (is-a-p (cdr value) opal:bitmap)))
    (:display
	(stringp value))
    (otherwise
	T)
   )
  T
  (format t "*** Warning:  Object ~A, Slot ~A, Value ~A is illegal!~%"
	object slot value))
)



;;; LOOK -- describes opal objects, level specifies how much detail
;;;
(defun look (object &optional (detail 2) &key (indent 0))
  "Describe a graphical object, detail is one of:
  0 outputs one line, same as (what schema)
  1 also shows children, same as (kids schema)
  2 prints all children (default)
  3 also prints fields of schema
  4 also prints fields of children
  5 prints all fields of whole tree"
  (let (class children parent name next-detail proto)
    (indent-by indent)
    (setf name (g-value object :known-as))
    (cond (name
	   (format t "~A: " name)))
    (setf parent (car (g-value object :is-a)))
    (format t "~A is-a ~A" object parent)
    (setf class (std-proto parent))
    (cond (class
	   (format t " (~A)" class)))
    (format t "~%")
    (cond ((> detail 2)
	   (ps object :indent indent :control :default)
	   (indent-by indent)
	   (cond ((is-a-p object opal::window)
		  (format t "Aggregate is:~%"))
		 ((g-local-value object :components)
		  (format t " Components are:~%"))
		 (t
		  (format t "~%")))))
    (cond ((> detail 0)
	   (setf next-detail detail)	; detail parameter for recursive step
	   (cond ((is-a-p object opal::window)
		  (setf children (g-value object :aggregate))
		  (cond ((null children)
			 (indent-by indent)
			 (format t "   No aggregate!~%"))))
		 (t
		  (setf children (get-local-value object :components))
		  (cond ((or (= detail 1) (= detail 3) (= detail 4))
			 (setf next-detail (- detail 1)))))) ; suppress children's
					; fields
	   (dolist (c children) (look c next-detail :indent (+ indent 1)))
	   (setf children (g-local-value object :behaviors))
	   (cond (children
		  (indent-by indent)
		  (format t " Interactors are:~%")
		  (incf indent)
		  (dolist (i children)
		    (indent-by indent)
		    (setf name (g-value i :known-as))
		    (cond (name
			   (format t "~A: " name)))
		    (setf parent (car (g-value i :is-a)))
		    (format t "~A is-a ~A" i parent)
		    (setf class (std-proto i))
		    (cond (class
			   (format t " (~A)" class)))
		    (format t "~%")
		    (cond ((> detail 2)
			   (ps i :indent indent :control :default))))
		  (decf indent)))
	   (setf proto (get-local-value object :item-prototype-object))
	   (cond (proto
		  (indent-by indent)
		  (format t " Item Prototype is:~%")
		  (look proto next-detail :indent (+ indent 1)))) ))))



;;; LOOK-INTER -- print out the interactors
;;;
(defun look-inter (&optional inter)
  "Looks for interactors relevant to the parameters.
  inter can be:
    * an interactor to describe.
    * NIL to list all active interactors
    * a window, to list all active interactors on that window
    * an interactor priority-level, to list all active inters on that level
    * an opal object, to try to find all interactors that affect that object
    * :next to wait and describe the next interactor to be executed."

  ;; start with argument checking
  (fresh-line)
  (cond ((eq inter :next)
	 (format T "Will invoke look-inter on next interactor that runs.~%")
	 (setq inter:*debug-next-inter* 'look-inter)
	 (values))
	((null inter)
	 (format t "Interactors that are :ACTIVE and have a :WINDOW are:~%")
	 (inter:print-inter-levels))
	((is-a-p inter inter:interactor-window)
	 (format t "Interactors active over :WINDOW ~S are:~%" inter)
	 (inter:print-inter-levels :window inter))
	((is-a-p inter inter:priority-level)
	 (format t "Interactors active in priority level ~S are:~%" inter)
	 (inter:print-inter-levels :level inter))
	((is-a-p inter inter:interactor)
	 (format t "~S's :ACTIVE is " inter)
	 (print-a-slot inter :active)
	 (format t ", :WINDOW is ")
	 (print-a-slot inter :window)
	 (format t "~%   current priority-level ~s~%"
		 (g-value inter :current-priority-level))
	 (print-inter-details inter))
	((is-a-p inter opal:view-object)
	;; if called with non-interactor & non-window, search for interactors
	 ;; that might change the object, ignore detail argument
	 (search-for-inter inter))
	(T ; bad value
	 (format t "Bad value for inter parameter.  Options for inter are:
    * an interactor to describe.
    * NIL to describe all active interactors
    * a window, to describe all interactors on that window
    * an opal object, to try to find all interactors that affect that object
    * the special keyword :next to wait and describe the next interactor to
      be executed.
  detail can be
      0 just prints active interactors (default) or
      1 shows where they start
  Either or both args may be omitted.~%"))))


;;; print-inter-details -- print class, start-event and start-where
;;;
(defun print-inter-details (inter)
  (let (class where ev)
    (format t "~s" inter)
    (setf class (std-proto inter))
    (cond (class (format t " (which is a ~A)~%" class)))
    (setf ev (g-value inter :start-event))
    (setf where (g-value inter :start-where))
    (format t "   starts when ~S ~S~%" ev where)))
  

;;; print-a-slot -- prints value (and cache if value is formula)
;;;
(defun print-a-slot (obj slot)
  (let ((value (get-value obj slot))
	actual-cached-value cached-value-to-print validity)
    (cond ((formula-p value)
	   (setf actual-cached-value (g-cached-value obj slot))
	   (setf cached-value-to-print actual-cached-value)
	   (cond ((member slot '(:XFONT :TEXT-EXTENTS))
		  (setf cached-value-to-print "...")))
	   (setf validity (kr::cache-is-valid value))
	   (format t "~S (~S . ~A #~A)" value cached-value-to-print 
		   validity (kr::cache-mark value))
	   
	   )
	  (t
	   (format t "~S" value)))))

;;; search-for-inter -- find interactor that might change object
;;;
(defun search-for-inter (obj)
  (inter:Do-All-Interactors
      #'(lambda (inter)
	  (when (and (g-value inter :active)
		     (g-value inter :window))
	  (let (where)
	    ;; first check :start-where's
	    (setf where (g-local-value inter :start-where))
	    (cond ((eq t where)
		   (format t "~S has a :start-where of T~%" inter))
		  ((null where));; won't match
		  ((listp where)
		   (let ((control (first where))
			 (agg (second where))
			 slot lst)
		     (case control
		       ((:element-of :element-of-or-none)
			(cond ((null agg))
			      ((not (schema-p agg)))
			      ((has-component-p agg obj)
			       (format
				t "~S's :start-where is ~S,~% and ~S contains ~S~%"
				inter where agg obj))))
		       ((:list-element-of :list-element-of-or-none)
			(setf slot (third where))
			(cond 
			  ((and agg (schema-p agg))
			   (setf lst (g-value agg slot))
			   (cond
			     ((listp lst)
			      (dolist (i lst)
				(cond ((eq i obj)
				       (format
					t "~S's :start-where is ~S,~%   ~S's ~S is ~S,~%"
					inter where agg slot lst)))))))))
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
			     ((listp lst)
			      (dolist (i lst)
				(cond ((has-leaf-p i obj)
				       (format t
					       "~S's :start-where is ~S,~%   ~S's ~S is ~S, and~%   ~S contains ~S"
					       inter where agg slot lst i obj)))))))))
		       ((:leaf-element-of
			 :leaf-element-of-or-none)
			(cond ((and agg (schema-p agg) (has-leaf-p agg obj))
			       (format
				t "~S's :start-where is ~S,~% and ~S contains ~S~%"
				inter where agg obj))))
		       ((:in-box :in :in-but-not-on)
			(cond ((eq agg obj)
			       (format t "~S's :start-where is ~S~%"
				       inter where))))
		       (t (format t "~S has a bad :start-where : ~S~%" inter where)))))
		  (t (format t "~S has a bad :start-where : ~S~%"
			     inter where)))

	    ;; end of :start-where search, now look at :feedback-obj...
	    (cond ((eq obj (g-local-value inter :feedback-obj))
		   (format t "~S's :feedback-obj is ~S~%" inter obj)))

	    ;; look at final-feed-avail, final-feed-inuse, final-feedback-obj
	    (cond ((eq obj (g-local-value inter :final-feedback-obj))
	       (format t "~S's :final-feedback-obj is ~S~%" inter obj))
	      ((member obj (get-local-value inter :final-feed-inuse))
	       (format t "~S's :final-feed-inuse contains ~S~%" inter obj))
	      ((member obj (get-local-value inter :final-feed-avail))
	       (format t "~S's :final-feed-avail contains ~S~%" inter obj)))

	    ;; check :obj-to-change as well...
	    (cond ((eq obj (g-local-value inter :obj-to-change))
		   (format t "~S's :obj-to-change is ~S~%" inter obj))))))))
	
;;; search-invisible-agg -- look up parent tree for :visible nil
;;;
(defun search-invisible-agg (agg)
  (let (parent)
    (cond ((null (g-value agg :visible)) agg)
	  ((setf parent (g-value agg :parent))
	   (search-invisible-agg parent))
	  (t nil))))

#|
;; currently we do not determine standard names by lookup.

;;; *standard-names* -- names of opal objects
;;;
(defparameter *standard-names*
   (list
    (cons opal:black-fill             'opal:black-fill)
    (cons opal:white-fill             'opal:white-fill)
    (cons opal:gray-fill              'opal:gray-fill)
    (cons opal:light-gray-fill        'opal:light-gray-fill) 
    (cons opal:dark-gray-fill         'opal:dark-gray-fill)
    (cons opal:thin-line              'opal:thin-line)
    (cons opal:dotted-line            'opal:dotted-line)
    (cons opal:dashed-line            'opal:dashed-line)
    (cons opal:line-1                 'opal:line-1)
    (cons opal:line-2                 'opal:line-2)
    (cons opal:line-4                 'opal:line-4)
    (cons opal:line-8                 'opal:line-8)
    (cons opal:aggregate              'opal:aggregate)
    (cons opal:default-filling-style  'opal:default-filling-style)
    (cons opal:default-line-style     'opal:default-line-style)
    (cons opal:rectangle              'opal:rectangle)
    (cons opal:line                   'opal:line)
    (cons opal:roundtangle            'opal:roundtangle)
    (cons opal:circle                 'opal:circle)
    (cons opal:text                   'opal:text)
    (cons opal::window                'opal::window)

    (cons inter:angle-interactor      'inter:angle-interactor)
    (cons inter:interactor            'inter:interactor)
    (cons inter:move-grow-interactor  'inter:move-grow-interactor)
    (cons inter:text-interactor       'inter:text-interactor)
    (cons inter:menu-interactor       'inter:menu-interactor)
    (cons inter:two-point-interactor  'inter:two-point-interactor)
    (cons inter:button-interactor     'inter:button-interactor)
    (cons inter:interactor-window     'inter:interactor-window)
)
  "what name to ouput for standard schema")
|#

#|
;;; std-proto -- go up :is-a links to find opal or interactor parent
;;;
(defun std-proto (object)
  (let (result)
    (do ((ancestor object (car (g-value ancestor :is-a))))
	((or (null ancestor)
	     (setf result (assoc ancestor *standard-names*)))))
    (cond (result (cdr result))
	   (t nil))))
|#

;;; std-proto -- go up :is-a links to find non-KR-DEBUG package
;;;
(defun std-proto (object)
  (let (start)
    (if object (setf start (car (g-value object :is-a))))
    (do ((ancestor start (car (g-value ancestor :is-a))))
	((or (null ancestor)
             (not (symbolp (kr::schema-name ancestor)))
	     (and (not (eq (find-package "KR-DEBUG")
			   (symbol-package (kr::schema-name ancestor))))
		  (return-from std-proto (format NIL "~s" ancestor))))))
    NIL))



;;; test-display-slots -- type-check the update slots of an object
;;;
(defun test-display-slots (obj)
  (dolist (slot (g-value obj :update-slots))
    (cond ((opal::legal-type-p obj slot (g-value obj slot)))
	  (t  ;; bad type
	   (return-from test-display-slots nil))))
  t)


;;; UNIVERT -- remove inverted region
;;;
(defun uninvert () 
  ;; create-flash-object will remove flash-object and redisplay
  (create-flash-object))


;;; what -- tell the opal type of an object
;;;
(defun what (object &key (indent 0))
  (look object 0 :indent indent))


;;; where -- tell where an opal object is
;;;
(defun where (object)
  (format t "~S :TOP ~S :LEFT ~S :WIDTH ~S :HEIGHT ~S"
	  object
	  (g-value object :top)
	  (g-value object :left)
	  (g-value object :width)
	  (g-value object :height))
  (cond ((is-a-p object opal::window)
	 (format t "~%"))
	(t
	 (format t " :WINDOW ~S~%" (g-value object :window)))))

;;; WINDOWS -- tell me where windows are
;;;
(defun windows ()
  opal::*garnet-windows*
  #|
  (let (result)
    (maphash #'(lambda (x-window opal-window)
		 (declare (ignore x-window))
		 (where opal-window)
		 (push opal-window result))
	     opal::*drawable-to-window-mapping*)
    result)
  |#
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Stuff to break or notify when slots are set
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (special kr::*slot-setter-debug*))
(defparameter *object-slot-val-fnc-list* NIL)

;;; returns T if matches
(defun Matches? (schema slot new-value ctrl)
  (let ((o (first ctrl)))
    (if (or (eq o :*any*) (eq o schema))
	(let ((s (second ctrl)))
	  (if (or (eq s :*any*) (eq s slot))
	      (let ((v (third ctrl)))
		(if (or (eq v :*any*) (eq v new-value))
		    T
		    NIL))
	      NIL))
	NIL)))

;;; Called on every slot set, reason will be one of :S-VALUE,
;;; :FORMULA-EVALUATION, :INHERITANCE-PROPAGATION, :DESTROY-SLOT
(defun Debug-Check-Object-Slot-Val-Set (schema slot new-value reason)
  (let (fnc extra)
    (dolist (ctrl *object-slot-val-fnc-list*)
      (when (matches? schema slot new-value ctrl)
	(setq fnc (fourth ctrl))
	(setq extra (fifth ctrl))
	(return)))
    (when fnc
      (funcall fnc schema slot new-value reason extra))))

(defun Do-Break (schema slot new-value reason extra)
  (declare (ignore extra))
  (fresh-line)
  (break (format NIL "Because ~s slot ~s set with ~s due to ~a"
		 schema slot new-value reason)))

(defun Do-Notify (schema slot new-value reason extra)
  (declare (ignore extra))
  (fresh-line)
  (format T "#### ~s slot ~s set with ~s due to ~a~%"
	  schema slot new-value reason))

(defun Print-Current-Status ()
  (if *object-slot-val-fnc-list*
      (dolist (ctrl *object-slot-val-fnc-list*)
	(format T "~a when object ~s slot ~s is set with ~s~%"
		(if (eq (fourth ctrl) #'Do-Break) "Breaking" "Notifying")
		(first ctrl) (second ctrl)(third ctrl)))
      (format T "No breaks or notifies are active~%"))
  (values))
      
(defun call-func-on-slot-set (object slot value fnc extra-val)
 "Call the function when the slot of object is assigned value.  If
 value is :*any*, then call function when slot is set with any value.
 If slot and value are :*any*, then call function whenever any slot
 of object is set.  If object is :*any*, then call function when any
 object is set.  If object is NIL, then clears all breaks and notifies.
 The function is called as (lambda (obj slot val reason extra-val) and
 extra-val can be anything useful to the function. Reason will be one
 of :S-VALUE, :FORMULA-EVALUATION, :INHERITANCE-PROPAGATION, :DESTROY-SLOT"
  (cond ((null object)
	 (setq *object-slot-val-fnc-list* NIL)
	 (setq kr::*slot-setter-debug* NIL))
	((and (eq object :*any*) (eq slot :*any*) (eq value :*any*))
	 (Print-Current-Status))
	(T
	 (push (list object slot value fnc extra-val)
	       *object-slot-val-fnc-list*)
	 (setq kr::*slot-setter-debug* #'Debug-Check-Object-Slot-Val-Set)
	 T)))

(defun break-on-slot-set (&key (object :*any*) (slot :*any*) (value :*any*))
  "Break when the slot of object is assigned value.  If value is missing, then
 break when slot is set with any value.  If slot and value are missing, then 
 break whenever any slot of object is set.  If object is missing, then
 break when any object is set.
 If object is NIL, then clears all breaks and notifies.
 If all parameters are missing, show current status."
  (call-func-on-slot-set object slot value #'Do-Break NIL))

(defun notify-on-slot-set (&key (object :*any*) (slot :*any*) (value :*any*))
  "Print a message when the slot of object is assigned value.  If value is
 missing, then print msg when slot is set with any value.  If slot and value
 are missing, then print msg whenever any slot of object is set.  If
 object is missing, then break when any object is set.
 If object is NIL, then clears all breaks and notifies.
 If all parameters are missing, show current status."
  (call-func-on-slot-set object slot value #'do-notify NIL))

(defun clear-slot-set (&key (object :*any*) (slot :*any*) (value :*any*))
 "Clear the break or notify for object, slot and value.  If nothing
  specified or object is NIL, then clear all breaks and notifies."
  (if (or (null object)
	  (and (eq object :*any*) (eq slot :*any*) (eq value :*any*)))
      (setq *object-slot-val-fnc-list* NIL)
      ;; else remove the specific one
      (setq *object-slot-val-fnc-list*
	    (delete-if #'(lambda (element)
			   (and (eq (first element) object)
				(eq (second element) slot)
				(eq (third element) value)))
		       *object-slot-val-fnc-list*)))
  (when (null *object-slot-val-fnc-list*)
    (setq kr::*slot-setter-debug* NIL)))
