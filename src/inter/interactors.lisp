;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;         The Garnet User Interface Development Environment.        ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;
;; This file includes the design for mouse and keyboard interactors
;;
;; Designed and implemented by Brad A. Myers


;;; Change log:
;; 12/06/94 Charles Rich - Added UNLESS at end of handle-interactor-slot-changed
;; 06/01/94 Brad Myers - Mac version of Process-Event for multiple window inters
;; 02/04/94 Andrew Mickish - Set-Interest-In-Moved now in Gem
;; 12/03/93 Andrew Mickish - Always reference CLTL1:char-bits in Allegro
;; 11/22/93 Brad Myers - eliminate warning on :ACTIVE if destroy an inter proto
;; 10/22/93 Andrew Mickish - Fixed CHAR-BITS call for LispWorks and Allegro 4.2
;; 07/16/93 Brad Myers - BVZ: maintain :current-obj-over slot in inter
;;                     - BVZ: fixed so can destroy inter after change inter window
;;                     - BVZ: fixed window T bug in process-event
;; 07/12/93 Brad Myers - PAS: destroy window and :active NIL, still destroy inter
;; 06/02/93 Andrew Mickish - Added type definitions to inter::interactor, and
;;                           removed g-value's from Check-Required-Slots
;; 05/03/93 Brad Myers - Print-Inter-Levels robust if bad win (Jim Davis)
;; 03/17/93 Brad Myers - fix: menubar showed bug when recalc window while running
;; 03/12/93 Brad Myers - allow :short for trace-inter
;;                     - support :enter-window and :exit-window as :start-events
;; 03/10/93 Brad Myers - special slot in priority level called
;;                       :sorted-interactors that when non-NIL makes the
;;                       interactors run in sorted order by the number in the
;;                       :sort-order slot of each interactor (lowest
;;                       number runs first).  Then, if any runs, looks at
;;                       the :exclusivity-value slot, and won't run any other
;;                       interactors with the same value, unless NIL,
;;                       which means any other interactors can run.  This
;;                       slot can be any Lisp value.
;; 02/02/93 Brad Myers - small bug in priority levels: check for window destroyed
;; 01/18/93 Brad Myers - debug utility that operates on NEXT inter to
;;                       run, and :next parameter to trace-inter
;;                     - supply global accelerators that go BEFORE the inters
;;                     - allow :action procedures to return :stop to mean this
;;                       inter didn't grab the event (for text inter).
;; 12/9/92 Brad Vander Zanden - fixed bug when a level is not active
;; 10/22/92 Dave Kosbie - Added ACCELERATORS (window & global,mouse & keyboard)
;; 10/22/92 Dave Kosbie - Changed :stop-when of normal-priority-level to :if-any
;; 10/21/92 Dave Kosbie - Put "state" and "aggie" fields in "event"
;;                        defstruct (for Katie)
;; 10/13/92 Brad Myers - bug with priority-levels :stop-when=NIL
;; 09/02/92 Mike Salisbury - Fixed bug in check-leaf-but-return-element:
;;                           objects processed in wrong order
;; 08/22/92 Brad Myers - Fixed bug for interactor, start-event and where T 
;; 07/31/92 Brad Myers - Made :window slot be copied in case edited.
;; 07/17/92 Brad Myers - Fixed bugs with change-active and priority levels
;; 07/16/92 Brad Myers - Fixed bug where multiple windows in an inter
;;                       messed up modal windows (from bvz).
;; 07/07/92 Brad Myers - allow changing priority levels
;; 06/12/92 Brad Myers - change get-local to g-local in destroy-me (--dzg)
;; 06/10/92 Brad Myers - Fixed small bug with aborting from handle-change
;; 06/08/92 Brad Myers - Made multiple window interactors more robust
;; 06/04/92 Brad Myers - Support for animation-interactors
;; 05/25/92 Brad Myers - Support for animation sending :timer events
;; 05/22/92 Brad Myers - Start- and abort-inter must clear *changed first
;;                       Added :modal-p windows
;; 05/19/92 Brad Myers - make sure inter T gets newly created windows 
;; 04/08/92  Andrew Mickish - Removed :window from :local-only-slots for
;;                            inter:interactor, changed Check-Required-Slots 
;;                            to do a g-local-value (not g-value) on the
;;                            :window slot.
;; 04/03/92 Brad Myers  - fixed bug with priority levels not being modal
;; 03/25/92 Andrew Mickish - Added THE type declaration in Warp-Pointer
;; 03/23/92 Brad Myers - fix bugs bvz found with changing active slot
;; 03/18/92 Ed Pervin -  Added case in compare-and-get-possible-stop-event 
;;                       to convert control characters to keywords.  
;; 02/20/92 Andrew Mickish - Added schema-p call to :destroy-me method for
;;                           interactors so that remove-local-interactor is 
;;                           not called with an aggregadget that has already 
;;                           been destroyed.
;; 02/11/92 Brad Myers - made new grab more robust
;;                     - changed kr::*debug-switch* to #+Garnet-debug
;;                     - made remove-interactor-from-level-win more robust
;; 02/03/92 Brad Myers - New functions Interaction-Complete and Wait-Interaction-Complete.
;;                     - changed general-go into a direct call (not method)
;;                     - special call for when interactors' window or active
;;                       changes.  KR will call Inter:Notice-Interactor-Slot-Changed
;;                     - no longer allowed to REMOVE from priority-level-list
;;                     - priorities much more efficient
;;                     - no debug code unless kr::*debug-switch*
;;                     - default abort-event = ^G
;;                     - added support for double-click
;;                     - grab mouse when multi-windows
;; 05/13/91 Ed Pervin - In the case statement in Stop-Interactor,
;;                      changed (NIL NIL) to ((NIL) NIL).
;; 02/27/91 Brad Myers - Exported new function Warp-Pointer.
;; 01/14/91 Brad Myers - changed :custom to call the function, rather than
;;                       looking it up as a method, and doesn't check
;;                       the window of the object.
;; 10/11/90 Brad Myers - added Stop-Interactor
;; 09/21/90 Brad Myers - fixed final-feedback so works if :start-where
;;                       returns :none and when :start-where is T.
;;                       Exported new procedures:
;;                            Return-Final-Selection-Objs,
;;                            gv-Final-Selection-Objs
;;                            DeSelectObj, SelectObj
;; 07/26/90 Brad Myers - Added :custom branch to check-location
;; 07/25/90 Brad Myers - destroy-me removes inter from aggregadget
;; 07/11/90 Ed Pervin - new :destroy-me method
;; 06/05/90 Brad Myers - export new transcript functions
;; 04/09/90 Robert Cook - Changed append to copy-list.
;; 04/9/90 Brad Myers - fixed so start-event can be T and interactor
;;                      will start when created and won't stop.
;; 03/06/90  Brad Myers - :type in start-where can be a list,
;;                        export Insert-Text-Into-String
;; 12/11/89 Ed Pervin - Exporting *garnet-break-key*
;; 12/05/89  Ed Pervin - Removed extra `)'
;; 11/16/89 Brad Myers - Extra debugging output in general-go
;; 11/14/89 Ed Pervin - In Start-Interactor, added test to handle
;;                      when event is NIL.
;; 11/07/89  Ed Pervin - Main-event-loop, exit-main-event-loop and 
;;                       beep are exported.
;; 11/01/89  Ed Pervin - Split up check-location so it would compile
;;                       on Sun.
;; 10/26/89 Brad Myers - Add new function Abort-Interactor
;; 10/19/89 Brad Myers - If tracing any then also trace :events
;; 10/5/89 Brad Myers - If window is NIL, then don't run interactor,
;;                      Add new slots :first-obj-over, :start-char
;;                      Change *event* to *Current-Event*
;; 10/04/89 Roger Dannenberg - Change debugging output
;; 08/15/89 Brad Myers - Added :list-leaf-element-of-or-none and
;;                             :list-element-of-or-none,
;;                             :list-check-leaf-but-return-element-or-none
;;                             :check-leaf-but-return-element-or-none
;; 08/14/89 Brad Myers - Added multiple priority levels,
;;                       exported event procedures
;; 07/20/89 Brad Myers - Added new check-locations:
;;                       :list-check-leaf-but-return-element
;;                       :check-leaf-but-return-element
;; 07/03/89 Brad Myers - Allow interactors to have multiple values in
;;                       the window slot
;; 06/26/89 Brad Myers - Fixed to have quote for create-schema
;; 06/21/89 Brad Myers - Added :list-leaf-element-of
;; 06/09/89 Brad Myers - New procedure to cause an interactor to start
;;                       without waiting for its start event
;; 05/26/89 Brad Myers - Allow stop-event and running-where to be set at any
;;                       time. call-method => kr-send
;; 05/19/89 Brad Myers - Removed all get-local-values (except for :state) so
;;                       will work as prototypes
;; 05/11/89 Brad Myers - Make steal-mouse queue run all interactors there
;; 04/25/89 Brad Myers - Added :leaf-element-of-or-none
;; 04/19/89 Brad Myers - schema-call -> call-method fixed so no update call
;;                       if interactor destroyed, Window for interactor can
;;                       be specified after created
;;			 Fixed :in-box
;; 04/13/89 Brad Myers - add :list-element-of, fixed change-active
;; 04/07/89 Brad Myers - changed to new KR; merged Lynn's changes from 3/30
;; 04/05/89 Brad Myers - small change to get-gob-of-where for where=T
;; 03/30/89 Lynn Baumeister - altered code to work with portable events
;; 03/28/89 Brad Myers - make window slot be inheritable (in Check-Required-Slots),
;;                       and fixed destroy to look at the erase field so hopefully
;;                       it will be more robust
;; 03/02/89 Brad Myers - removed create and destroy procedures
;; 02/24/89 Brad Myers - add point-to-leaf and NIL in Check-location
;; 02/15/89 Lynn Baumeister - altered check-event big-time
;; 02/15/89 Lynn Baumeister - changed interactor funcs to receive an event 
;;                            instead of just x,y 
;; 01/15/89 Lynn Baumeister - merged button-down, key-pressed, and button-up
;;                            queues into one queue
;; 12/22/88 Brad Myers - moved calc-set-value to menuinter

;; 11/28/88 Brad Myers - changed to new Opal, moved Menus to their own file
;; 8/17/88 Brad Myers - moved to constraint version of KR
;; 7/24/88 Brad Myers - started 


(in-package "INTERACTORS")

;;; the exported functions
(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(
	    ;; for animation
	    start-animator Stop-Animator abort-animator animator-interactor
	    animator-wrap animator-bounce Reset-All-Timer-Processes
	    ;; entering and leaving main event loop
	    main-event-loop exit-main-event-loop *garnet-break-key*
	    ;; waiting for an interaction to complete
	    Interaction-Complete Wait-Interaction-Complete
	    ;;explicit control of interactors
	    Change-Active Start-Interactor Abort-Interactor Stop-Interactor
	    ;; Called by KR when :active or :window changes:
	    Notice-Interactor-Slot-Changed
	    ;; support for multiple priority levels
	    priority-level normal-priority-level high-priority-level
	    running-priority-level priority-level-list 
	    ;; the next ones are for debugging
	    Reset-Inter-Levels Print-Inter-Levels Print-Inter-Windows
	    trace-inter untrace-inter *debug-next-inter* Do-All-Interactors
	    ;; interactor event structure (copy of X's event structure)
	    *Current-Event* *Garnet-Break-Key*
	    event-x event-y event-char event-code event-mousep
	    event-downp event-window event-timestamp make-event
	    ;; for controlling double clicks
	    *double-click-time*
	    ;; key translations for text-inter
	    Bind-Key Unbind-Key Unbind-All-Keys Set-Default-Key-Translations
	    ;;transcripting functions
	    Transcript-Events-To-File Close-Transcript
	    Transcript-Events-From-File
	    ;; useful utility functions
	    Clip-And-Map Beep Insert-Text-Into-String Warp-Pointer
	    Pop-Up-Win-And-Start-Interactor
	    ;; functions for dealing with selection for button and menu
	    Return-Final-Selection-Objs DeSelectObj SelectObj
	    ;; the various exported interactor types
	    interactor interactor-window button-interactor text-interactor
	    two-point-interactor move-grow-interactor menu-interactor
	    angle-interactor)))


;;; Variables used for noticing changed slots
;;

(defparameter *Changed-Interactors* NIL
  "A list of the interactors that have a changed slot.  Clean-up
with (Check-and-handle-changed-inters)")

(defparameter *Inters-With-T-Window* NIL
  "A list of all the interactors that have a (:window T).  
This is needed because when new windows are  created,
they need to be added to these interactor's lists.")

(defparameter *Visible-Modal-Windows* NIL
  "A list of the windows that are modal (stealing all input)
that are visible.  When this list is non-nil, only interactors 
in these windows will operate.  Note that sub-windows of these
windows must have their :modal-p bit set.  This list is maintained
automatically by update on interactor windows looking at the
:modal-p and :visible slots of the windows.")

(defparameter *Special-Grab-Up-Inter* NIL
  "Holds a single interactor. When an interactor is supposed to work
over multiple windows, it won't get events when the mouse is over 
other garnet windows.  This variable is used as a hack to make
those events go to this interactor, when the interactor is running.
The variable is set by turn-on{off}-mouse-moved.")


;;;============================================================

(declaim (inline Check-and-handle-changed-inters))
(defun Check-and-handle-changed-inters ()
  "This should be called at update and when new events come in"
  (when *Changed-Interactors*
    (Handle-All-Changed-Interactors)))

;;; Handy functions
;;
(defun Beep ()
  "Causes a beep or bell to sound"
  (gem:beep (g-value opal::DEVICE-INFO :current-root)))

(defun Warp-Pointer (window x y)
  "Move the cursor to the specified x y position in the window."
  (gem:set-window-property window :POINTER-POSITION (cons x y)))



;;; debugging aids:
;;   if-debug is a macro for use around debugging code
;;   trace-test is a (possibly expensive) test to enable selective tracing
;;   trace-inter is a function to start tracing an interactor (T for all of them)
;;   untrace-inter is a function to stop tracing an interactor or all of them

;; ** The debugging information is only generated if the
;; #+garnet-debug feature is present.  Otherwise nothing is generated.
;;

(defparameter *int-debug* NIL
  "True if any debugging is enabled")
(defparameter *int-trace* NIL
  "List of interactors to be traced")
(defparameter *debug-next-inter* NIL
  "A function to be called on the next interactor to run")
(defparameter *Special-Trace-Values* '(:window :priority-level :mouse :event
				       :next :short))

(defmacro if-debug (inter &rest body)
  "Test for selective tracing. Put this around any print-out statements
**Only generates code if #+garnet-debug feature is present at compile time

NOTE: inter is an interactor or may be one of: 
      :window -- trace things about interactor windows (create, destroy, etc.)
      :priority-level -- trace changes to priority levels
      :mouse -- trace set-interested-in-moved and ungrab-mouse
      :event -- show all events that come in
      :next -- trace the next interactor to run"
  #+garnet-debug
    `(when (and *int-debug* (trace-test ,inter))
    (let ((*print-pretty* NIL))
      ,@body))
  )
  
(defmacro debug-p (inter)
  "Returns T or NIL based on whether should trace or not.  Should be same
test as in if-debug (used when debugging value being passed as a parameter).
If no debug (because :garnet-debug is not on *features*), just generate NIL"
  #+garnet-debug
  `(and *int-debug* (trace-test ,inter))
  #-garnet-debug
  nil)

(defun trace-test (inter)
  "Hook used to enable/disable fancy tracing"
  (or (eq *int-trace* t) ;trace all
      (and inter (member inter *int-trace*))))

(declaim (special interactor)) ; defined below

(defun Trace-Next-Inter-To-Run (an-interactor)
  "This function is set into the variable *debug-next-inter* when calling
(trace-inter :next)"
  (format T "~%------------~%Begin Tracing ~s~%~%" an-interactor)
  (trace-inter an-interactor))

;; trace-inter -- trace a function (or all functions)
;;
#+garnet-debug
(defun trace-inter (&optional (trace-what :status))
"Trace interactor execution.  (Inter:UnTrace-inter removes tracing.)
  Can be called more than once and adds new parameter to trace set.
  If no parameter, then prints what are tracing. Legal values for parameter are:
    T - trace everything
    an interactor - trace on that interactor
    NIL - untrace everything
    :window -- trace things about interactor windows (create, destroy, etc.)
    :priority-level -- trace changes to priority levels
    :mouse -- trace set-interested-in-moved and ungrab-mouse
    :event -- show all events that come in.
    :next -- trace the next interactor to run.
    :short -- just say what interactors operate on each event."
   (cond ((eq trace-what t)
	  (setf *int-trace* t))
	 ((eq trace-what :status) t);; no argument provided
	 ((null trace-what)
	  (untrace-inter))
	 ((eq *int-trace* t)
	  (format t "Already tracing everything!~%"))
	 ((member trace-what *int-trace*)
	  (format t "Already tracing ~S~%" trace-what))
	 ((not (or (is-a-p trace-what interactor)
		   (member trace-what *Special-Trace-Values*)))
	  (format t "*** ~S is not an interactor or one of the special values:
	     T NIL ~{~s ~}
 (Describe 'inter:Trace-Inter) for more information~%"
		  trace-what *Special-Trace-Values*))
	 ((eq trace-what :next)
	  (setq *debug-next-inter* #'Trace-Next-Inter-To-Run))
	 (t (push trace-what *int-trace*)
	    (pushnew :event *int-trace*))) ; always trace events when
					; tracing anything else
	  (setf *int-debug* (not (null *int-trace*)))
	  *int-trace*)

#-garnet-debug
(defun trace-inter (&optional (trace-what :status))
    (format T "** Can't debug since Interactors was compiled with
        :garnet-debug not in the *features* list (see garnet-loader.lisp)."))

#+garnet-debug
(defun untrace-inter (&optional untrace-what)
"Turns off tracing on the parameter.  If no parameter supplied, then turns off
 all tracing.  See trace-inter for description of parameters."
  (cond ((or (eq untrace-what t) (null untrace-what))
	       (setf *int-trace* nil))
	      ((member untrace-what *int-trace*)
	       (setf *int-trace* (delete untrace-what *int-trace*)))
	      (t
	       (format t "Not tracing ~S%" untrace-what)))
  ;; only enable debugging if user is tracing some interactor
  (setf *int-debug* (not (null *int-trace*)))
  *int-trace*)

#-garnet-debug
(defun untrace-inter (&optional untrace-what)
  (format T "** Can't debug since Interactors was compiled with
        :garnet-debug not in the *features* list (see garnet-loader.lisp)."))


;;; General code.
 
(Defun Error-Print (slotdesc value shouldbe inter)
  (Error "The ~a of the interactor ~s should be a ~a,~%     but it is ~s.~%"
	 slotdesc inter shouldbe value))

(defun Count-Interactors ()
  (multiple-value-bind (total inactive)
      (Count-Interactors-Internal Interactor)
    (format T "There are ~s interactors, of which ~s are in-active, ~s are active~%"
	    (1+ total) (1+ inactive)(- total inactive))))

(defun Count-Interactors-Internal (root)
  (let ((total (length (get-local-value root :is-a-inv)))
	(inactive 0))
    (dolist (inter (get-local-value root :is-a-inv))
      (unless (and (g-value inter :active)
		   (g-value inter :window))
	(incf inactive))
      (multiple-value-bind (sub-total sub-inactive)
	  (Count-Interactors-Internal inter)
	(incf total sub-total)
	(incf inactive sub-inactive)))
    (values total inactive)))

(declaim (inline  Check-and-handle-debug-next-inter))
(defun Check-and-handle-debug-next-inter ()
  "This is called by General-Go when there is a value in
*debug-next-inter*.  Calls the function in *debug-next-inter* and
clears the variable."
  (when *debug-next-inter*
    (Handle-Debug-Next-Inter an-interactor)))

(defun Handle-Debug-Next-Inter (an-interactor)
  (let ((fn *debug-next-inter*))
    (setq *debug-next-inter* NIL)	; clear first in case the function crashes
    (funcall fn an-interactor)))


;;; Macros to print debugging information
;;

(defun dbprinter (slot obj val feedbackp)
  (format T "  * Setting ~s of ~s~a to ~s~%" slot obj
	  (if feedbackp " (Feedback-Obj)" "")
	  val))

(declaim (inline dbprint))
(defun dbprint (slot obj val inter) 
  (if-debug inter (dbprinter slot obj val NIL)))

(declaim (inline dbprint-either))
(defun dbprint-either (slot obj val inter feedbackp) 
  (if-debug inter (dbprinter slot obj val feedbackp))) 

(declaim (inline dbprint-sel))
(defun dbprint-sel (slot obj val inter) 
  (if-debug inter (dbprinter slot obj val NIL)))

(declaim (inline dbprint-feed))
(defun dbprint-feed (slot obj val inter) 
  (if-debug inter (dbprinter slot obj val T)))

(defun dbstrprinter (obj feedbackp)
  (format T "  * Setting :string of ~s~a to ~s and :cursor-index to ~s~%" obj
	  (if feedbackp " (Feedback-Obj)" "")
	  (g-value obj :string)
	  (g-value obj :cursor-index)))

(declaim (inline dbprint-str))
(defun dbprint-str (obj inter feedbackp) 
  (if-debug inter (dbstrprinter obj feedbackp)))


;;; Priority levels
;;

(create-schema 'priority-level
   ;; bam: the interactors list is now stored with the
   ;; windows, not here in the interactors list
   (:active T)				; if NIL, then this level is totally ignored,
					; including its :stop-when field.  This can be a
					; formula, but if it changes to be NIL, interactors
					; will not be automatically aborted.  Use
					; change-active to get that behavior.
   (:stop-when :if-any)			; choices are :if-any, :always, NIL
					; :if-any - then doesn't go down to the next
					; 	level if anything on this level runs.
					; :always - then never goes down to next level
					; NIL - never stops after this level (always goes on)
   (:sorted-interactors NIL)		; if T then runs the inters
					; in order by the :sort-order
					; field of the inter and
					; stops at first to run
 )

(create-schema 'normal-priority-level
	       (:is-a priority-level)
	       (:active T)
	       (:stop-when :if-any))

(create-schema 'high-priority-level
	       (:is-a priority-level)
	       (:active T)
	       (:stop-when :if-any))

(create-schema 'running-priority-level
	       (:is-a priority-level)
	       (:active T)
	       (:stop-when :if-any))

(Defparameter priority-level-list
  (list running-priority-level high-priority-level normal-priority-level))


;; useful macro
;;
(defmacro DeletePlace (item Place)
  `(setf ,Place (delete ,item ,Place)))


;;; Events
;;

;; this defines event-x event-y event-char event-code event-mousep
;; event-downp event-window event-timestamp
(defstruct (event (:print-function print-event))
  (window NIL)
  (char :leftdown)
  (code 1)
  (mousep t)
  (downp t)
  (x 0)
  (y 0)
  (timestamp 0)
  (state NIL))

(defun print-event (event stream depth)
  (declare (ignore depth))
  (format stream "#EV<CHAR:~s CODE:~s MOUSE:~s DN:~s X:~s Y:~s TIME:~s WIN:~s>"
	  (event-char event) (event-code event) (event-mousep event)
	  (event-downp event) (event-x event) (event-y event)
	  (event-timestamp event)(event-window event)))

(defparameter *Current-Event* (make-event))

(declaim (special all-inter-windows))



;;; Exported procedures

;; Call this to change the active slot of an interactor or of an interactor
;; priority-level.  New-Value should be T or NIL.  This procedure will make
;; sure that the interactor (or all the interactors at the priority level) are
;; aborted if becoming in-active.  You can also set the active slot
;; of an interactor directly.
(defun Change-Active (inter-or-level 
		      &optional
			(new-value (g-value inter-or-level :active)))
  "Call this to change the status of an interactor or interactor priority-level
immediately.  If Active=T then will run, if active=NIL, then will not run."
  (cond ((is-a-p inter-or-level interactor)
	 (if new-value
	     (progn
	       (if-debug inter-or-level
			 (format T "Change interactor ~s TO active~%" inter-or-level))
	       (s-value inter-or-level :active T))
	     (progn
	       (if-debug inter-or-level 
			 (format T "Change interactor ~s TO IN-active~%" inter-or-level))
	       (s-value inter-or-level :active NIL)
	       (unless (eq :start (get-local-value inter-or-level :current-state))
		 (Kr-Send inter-or-level :Do-abort inter-or-level T NIL)
		 (opal:update-all)))))
	((is-a-p inter-or-level priority-level)
	 (if new-value 
	     (progn
	       (if-debug :priority-level
			 (format T "Change priority level ~s TO active~%"
				 inter-or-level))
	       (s-value inter-or-level :active T))
	     (progn
	       (if-debug :priority-level
			 (format T "Change priority level ~s TO IN-active~%"
				 inter-or-level))
	       (s-value inter-or-level :active NIL)
	       (dolist (win all-inter-windows)
		 (let ((localassoc (assoc inter-or-level
					  (g-value win :priority-level-assoc)))
		       inter)
		   (when localassoc
		     (dotimes (i (length (cdr localassoc)))
		       (setq inter (aref (cdr localassoc) i))
		       (unless (eq :start
				   (get-local-value inter :current-state))
			 (Kr-Send inter :Do-abort inter T NIL))))))
	       (opal:update-all))))
	(t (error "change active on object not interactor or priority level"))))


;; Causes the interactor to start running (go from :start state to
;; :running state) without waiting for the start event.  This does nothing
;; if the interactor is already running or if it is not active.
;;
;; If an event is passed in, then this is used as the x and y location to
;; start with.  If the event is T (the default), then the last event that
;; was processed is re-used.  Events are defined in i-windows.lisp and not
;; exported.  Only the x and y fields of the event are really needed.  If
;; the other fields are there also, then the event is also used to calculate
;; the stop event (needed if the start-event is
;; a list).  If the position of the event is not inside the object, then the
;; start object for the interactor will be NIL, which might be a problem
;; (especially for button-interactors, for example)
;;
;; NOTE: If the interactor being started should not start by itself,
;; its start-where can be set to NIL.
;;
(defun Start-Interactor (an-interactor &optional (event T))
  "Causes an interactor to start running without waiting for the start event.
Event can be T to use the previous event."
  (internal-start-interactor an-interactor event)
  (opal:update-all))

(defun Internal-Start-Interactor (an-interactor event)
  (Check-and-handle-changed-inters)
  (if-debug an-interactor
	    (format T "~% Starting ~s with event ~s~%" an-interactor event))
  (when (and (g-value an-interactor :active)
	     (eq :start (g-value an-interactor :current-state)))
    ;; first fix the generated stop-event of the interactor
    (when (eq event T)
      (setq event *Current-Event*))	; this is the last event
					; processed
    (unless (and event (check-event event :start-event an-interactor))
      ;; have to generate stop event here
      (Set-Invented-stop-event an-interactor))
    (let ((obj (when event (check-location event :start-where an-interactor))))
      ;; obj will sometimes be NIL, hope that's OK
      ;; first set the special slots of the interactor
      (s-value an-interactor :current-window (when event (event-window event)))
      (s-value an-interactor :first-obj-over obj)
      (s-value an-interactor :start-char (when event (event-char event)))
      ;; now start
      (Kr-Send an-interactor :Do-start an-interactor obj event))))

;; This procedure aborts the interactor if it is running.
(defun Abort-Interactor (inter)
  "Call this to abort the interactor if it is running."
  (Internal-abort-interactor inter)
  (opal:update-all))
  
(defun Internal-Abort-Interactor (inter)
  (Check-and-handle-changed-inters)
  (if-debug inter
	    (format T "Aborting interactor ~s explicitly~%" inter))
  (unless (eq :start (get-local-value inter :current-state))
    (Kr-Send inter :Do-abort inter NIL NIL)))

;; This procedure stops the interactor if it is running.  This is like
;; hitting the stop event, except that the previous value for the
;; interactor is used.  There are special messages in each interactor
;; type to allow stopping explicitly, since each one needs to do
;; something different to re-use the last value.
(defun Stop-Interactor (inter)
  "Call this to stop the interactor if it is running."
  (Check-and-handle-changed-inters)
  (if-debug inter
	    (format T "Stopping interactor ~s explicitly~%" inter))
  (case (get-local-value inter :current-state)
    ((NIL) NIL)				; ignore this object if state is NIL
    (:start NIL)			; if not running, just leave alone
    (:outside (if (eq :last (g-value inter :outside))
		  (Kr-Send inter :Do-explicit-stop inter)
		  (Kr-Send inter :Do-abort inter NIL NIL)))
    ((:running :animating) (Kr-Send inter :Do-explicit-stop inter))
    (T (error "**illegal state for ~s in Stop-Interactor" inter)))
  (opal:update-all))


(defun Top-Interactor-Initialize (self)
  "Top level initialize routine"
  (declare (ignore self))
  (error "** Cannot create an instance of a top-level interactor"))


;;; Utility procedures handling the priority levels for interactors

;; Design: There is a global list of priority levels.  Each window has
;; a local list of the levels used in that window stored as an
;; association list of (global-level . inter-array), where global-level is
;; a priority-level schema and inter-array is an array of interactors at that
;; level for this window.  Each level in the window has an array of interactors.
;; The global list does not contain any interactors.  When an interactor is
;; added to a window (by being created with a window slot or having its
;; window slot changed), the interactor is added to the right priority
;; level lists in the window(s) it is affiliated with.  If that window
;; does not have the named priority level, then it is added at the right
;; place (so the priority order is maintained).

;; It is not legal to destroy or modify the priority-level-list except to
;; add new priorities (anywhere) or to change :active of a level.  No
;; removing or changing order of priorities.

(defun Add-Interactor-To-Level-Win (inter level win multiwin)
  (let ((inter-array (cdr (assoc level (g-value win :priority-level-assoc)))))
    ;; first, deal with multiwin
    (when (and multiwin
	       (g-value inter :continuous))
      ;; then should wait with a special kind of grab
      (change-window-multi-grab win T))
    ;; now add interactor to the window's list
    (unless inter-array
      (setq inter-array (create-level-for-win level win)))
    (if (position inter inter-array)
	(progn
	  #+garnet-debug		; only do this printing when debugging is enabled
	  (format T "WARNING, pushing interactor ~s on level ~s for win ~s
but already there~%" inter level win)
	  )
	;; else add it. 10 is the amount to extend array if necessary
	(vector-push-extend inter inter-array 10))))


(defun Add-Interactor-To-Level (inter level just-moving)
  (if-debug :priority-level
	    (format T "adding ~s to priority level ~s~%" inter level))
  #+garnet-debug			; only do this when debugging
  ;; *test to see if interactor used more than once
  (let ((curlevel (g-value inter :current-priority-level)))
    (unless (null curlevel)
      (error "**Adding inter ~s to > 1 priority level. old: ~s new ~s~%"
	     inter curlevel level)))
  (s-value inter :current-priority-level level)
  (let ((wins (Get-Interactor-Windows inter)))
    (if (listp wins)
	(dolist (win wins)
	  (Add-Interactor-To-Level-Win inter level win
				       ;; don't need to deal with
				       ;; multi-win when just-moving
				       (if just-moving NIL T)))
	(Add-Interactor-To-Level-Win inter level wins NIL))))


(defun Remove-Interactor-From-Level-Win (inter level win multiwin)
  (when (schema-p win)
    (let ((inter-array (cdr (assoc level (g-value win :priority-level-assoc))))
	  temp pos)
      (if (and inter-array
	       (not (zerop (fill-pointer inter-array))))
	  (progn			; then remove it from array
	    (setq temp (vector-pop inter-array))
	    (unless (eq temp inter)
	      ;; remove inter by putting last element in its place
	      (setq pos (position inter inter-array))
	      ;; Can have no pos when destroying window just
	      ;; after changing the window slot since destroy can't
	      ;; call (Check-and-handle-changed-inters) since this
	      ;; causes an asynchronous window error.
	      (when pos
		(setf (aref inter-array pos) temp)))
	    (when multiwin ;; then see if any other multi-win inters
	      (Check-If-UnGrab-Win win)))
	  ;; else write a warning message
	  #+garnet-debug		; only do this printing when debugging is enabled
	  (format
	   T "WARNING, Removing inter ~s from window ~s not on level ~s~%"
	   inter win level)))))


(defun Remove-Interactor-From-Level (inter level just-moving)
  (if-debug :priority-level
	    (format T "removing ~s from priority level ~s~%" inter level))
  (s-value inter :current-priority-level NIL)
  (let ((wins (Get-Interactor-Windows inter)))
    (if (listp wins)
	(dolist (win wins)
	  (Remove-Interactor-From-Level-Win inter level win 
					    ;; if just-moving, then
					    ;; don't have to deal with
					    ;; multi-win
					    (if just-moving NIL T)))
	(Remove-Interactor-From-Level-Win inter level wins NIL))))


(defun Careful-Remove-Interactor-from-Level (inter level old-window)
  (if-debug :priority-level
	    (format T "careful removing ~s from priority level ~s~%" inter level))
  (s-value inter :current-priority-level NIL)
  (when (and level old-window)
    (if (listp old-window)
	(dolist (win old-window)
	  (Remove-Interactor-From-Level-Win inter level win T))
	(Remove-Interactor-From-Level-Win inter level old-window NIL))))


(defun Get-Interactor-Windows (inter)
  (let ((interwin (g-value inter :window)))
    (cond ((schema-p interwin) interwin)
	  ((null interwin) NIL)
	  ((eq interwin t) all-inter-windows)
	  ((listp interwin) interwin)
	  (t (error "bad window in inter ~s" inter)))))

;; search through global level list and put the local-level at the
;; right place in win's list.  Returns new inter-array
(defun create-level-for-win (glo-level win)
  (let* ((new-inter-array (make-array 5 :fill-pointer 0 :adjustable T))
	 (glo-list priority-level-list)
	 (local-list (g-value win :priority-level-assoc))
	 (prev-local NIL)
	 (new-item (list (cons glo-level new-inter-array))))
    (loop
       (when (null glo-list) (error "level ~s not found" glo-level))
       (when (or (eq (car glo-list) glo-level)
		 (null local-list))
	 (return))
       (when (eq (car glo-list)(caar local-list)) ; then go on to next local one
	 (setq prev-local local-list)
	 (setq local-list (cdr local-list)))
       (setq glo-list (cdr glo-list)))
    ;; 
    (if prev-local
	;; then attach to end of first element
	(setf (cdr prev-local) new-item)
	;; else set new one into window
	(s-value win :priority-level-assoc new-item))
    (setf (cdr new-item) local-list)
    new-inter-array))

;; check if any other inters use multiwin, if not call Change-window-multi-grab
;;
(defun Check-If-UnGrab-Win (win)
  (let (inter-array inter interwin)
    (dolist (levelassoc (g-value win :priority-level-assoc))
      (when (g-value (car levelassoc) :active)
	(setq inter-array (cdr levelassoc))
	(dotimes (i (length inter-array))
	  (setq inter (aref inter-array i))
	  (setq interwin (g-value inter :window))
	  (when (and (or (eq interwin T)
			 (listp interwin))
		     (g-value inter :continuous))
	    ;; then found one, so leave window being continuous
	    (return-from Check-If-UnGrab-Win))))))
  ;; if get here, then no multi-window interactors
  (Change-window-multi-grab win NIL))

(defun Print-Inter-Levels (&key level window)
  "Prints out the interactor priority levels (for debugging).  If :level,
 then only that level, if :window then only that window"
  (let ((*print-pretty* NIL)
	(winlist (if window (list window)
		     all-inter-windows))
	foundone)
    (dolist (level (if level (list level)
		       priority-level-list))
      (when foundone
	(setq foundone NIL)
	(format T "-------------~%"))
      (format T "Level ~s " level)
      (dolist (win winlist)
	(if (schema-p win)
	    (let ((localassoc (assoc level (g-value win
						    :priority-level-assoc))))
	      (when localassoc
		(unless foundone
		  (setq foundone T)
		  (terpri))
		(format T "  WINDOW ~s~%" win)
		(format T "      ~s~%" (cdr localassoc))))
	    ;; else invalid window
	    (progn
	      (format T "** Found invalid window ~s.  Removing.~%" win)
	      (setq all-inter-windows (delete win all-inter-windows)))))
      (unless foundone
	(format T "------ EMPTY -----~%")))))

(defun Do-All-Interactors (func &optional (root-inter interactor))
  "Calls func on each interactor that is-a root-inter (recursively).
   Does not call func on root-inter itself."
  (dolist (int (g-value root-inter :is-a-inv))
    (funcall func int)
    (Do-All-Interactors func int))) 

;; useful for debugging, gets rid of all interactors.  Doesn't
;; destroy them, but simply removes them from the levels.
(defun Reset-Inter-Levels (&optional level)
  "Removes all interactors from a level, or all levels if none supplied
 (for debugging)"
  (setf *Changed-Interactors* NIL)	; no interactors to fix either
  (Reset-All-Timer-Processes)
  (setf *Special-Grab-Up-Inter* NIL)
  (dolist (win all-inter-windows)
    (if level
	(let ((localassoc (assoc level (g-value win :priority-level-assoc))))
	  (when localassoc
	    (setf (cdr localassoc) NIL)))
	;; else do all levels
	(s-value win :priority-level-assoc NIL))))


;; checks to see if the actor should be destroyed when the window is.  This
;; has a side-effect of removing the window from the interactor's window list,
;; if it is in there.
(defun Check-actor-delete-window (win actor)
  (let ((interwin (g-value actor :window)))
    (cond ((schema-p interwin) (eq interwin win)) ; if just one, return eq'ness
	  ((null interwin))		; don't delete interactor if its window slot is nil
	  ((listp interwin)		; if list, return if no more windows for this inter
	   (null (with-demon-disabled #'inter-update-slot-invalidated
		   ;; don't need to push this interactor onto the
		   ;; changed list since being handled
		   (deleteplace win (g-value actor :window)))))
	  (t NIL))))			; otherwise, don't delete it

(defparameter *copy-array* (make-array 50 :adjustable T :fill-pointer T))

;; destroys all the interactors on the window
;; Be careful not to destroy an interactor if on this window and other
;; windows also.  
(defun destroy-all-interactors (window)
  (if-debug :window (format T "Destroy all interactors for win ~s~%" window))
  ;; copy the list since destroy will modify the list
  (dolist (inter (copy-list (g-value window :all-interactors)))
    (when (and (schema-p inter) (Check-actor-delete-window window inter))
      (opal:destroy inter NIL))))

;; Tells X to start reporting move events for the window of the
;; interactor.  If the interactor has multiple windows, then turns on move
;; events in all of them, and ungrabs the mouse.
(defun turn-on-mouse-moved (actor)
  (if-debug actor (format T "turning on mouse-moved for ~s, win=~s~%"
			  actor (g-value actor :window)))
  (let ((win (g-value actor :window)))
    (when win
      (cond ((schema-p win)
	     (pushnew actor (g-value win :current-want-moved-interactors))
	     (gem:set-interest-in-moved win T))	; OK to set if already interested
	    ((eq win T)				; do all windows
	     (dolist (w all-inter-windows)
	       (pushnew actor (g-value w :current-want-moved-interactors))
	       (gem:set-interest-in-moved w T))
	     (unless (eq (g-value actor :start-event) t)
	       (setf *Special-Grab-Up-Inter* actor))
	     )
	    ((listp win)		; then do each one 
	     (dolist (w win)
	       (pushnew actor (g-value w :current-want-moved-interactors))
	       (gem:set-interest-in-moved w T))
             (unless (eq (g-value actor :start-event) t)
	       (setf *Special-Grab-Up-Inter* actor))
	     )
	    (t (error "Window slot of inter ~s has wrong form" actor))))))

;; Checks to see if should turn off mouse moved events from the window(s)
;; because the specified interactor is no longer running, and if so, does it
(defun turn-off-mouse-moved (actor)
  (if-debug actor (format T "Turn off mouse moved for ~s~%" actor))
  (setf *Special-Grab-Up-Inter* NIL)
  (let ((win (g-value actor :window)))
    (when win
      (cond ((schema-p win)
	     (when (null
		    (deleteplace actor
				 (g-value win :current-want-moved-interactors)))
	       (gem:set-interest-in-moved win NIL))) ; turn it off if no more
	    ((eq win t)				     ; do all windows
	     (dolist (w all-inter-windows)
	       (when (null
		      (deleteplace actor
				   (g-value w :current-want-moved-interactors)))
		 (gem:set-interest-in-moved w NIL))))
	    ((listp win)		; then do each one 
	     (dolist (w win)
	       (when (null
		      (deleteplace actor
				   (g-value w :current-want-moved-interactors)))
		 (gem:set-interest-in-moved w NIL))))
	    (t (error "Window slot of inter ~s has wrong form" actor))))))
	
;; Adds the schema to the correct level and turns on mouse moved events,
;; if necessary
(defun Add-to-level (an-interactor running-p need-mouse-moved just-moving)
  (Add-Interactor-To-Level an-interactor (if running-p
					     (g-value an-interactor :running-priority)
					     (g-value an-interactor :waiting-priority))
			   just-moving)
  (when need-mouse-moved (turn-on-mouse-moved an-interactor)))

;;removes from running or non-running level
(defun remove-from-level (an-interactor running-p just-moving)
  (Remove-Interactor-From-Level an-interactor (if running-p
						  (g-value an-interactor :running-priority)
						  (g-value an-interactor :waiting-priority))
				just-moving)
  (when (and running-p
	     ;; check to see if there is no start event, in which case,
	     ;; should always be running, so don't turn off.
	     (or (not (eq (g-value an-interactor :start-event) t))
		 (not (g-value an-interactor :active))))
    (turn-off-mouse-moved an-interactor)))
  
(defun Move-Levels (an-interactor from-running-p to-running-p needMouseMoved)
  (remove-from-level an-interactor from-running-p T)
  (add-to-level an-interactor to-running-p needMouseMoved T))

(defun Remove-from-all-levels (an-interactor)
  (if-debug :priority-level (format T "removing ~s from all levels~%"
 				    an-interactor))
  (let ((level (g-value an-interactor  :current-priority-level)))
    (when level (Remove-Interactor-From-Level an-interactor level NIL))))


;;; Utility procedures for the various create procedures

;; returns T if schema :is-a typ, otherwise raises an error
(defun Check-Interactor-Type (schema typ)
  (if (is-a-p schema typ)
      T
      (error "** Wrong type: Is a ~s, Not a ~s" (g-value schema :is-a) typ)))

(defun Check-Required-Slots (schema)
  ;; dzg - Nov. 12, 1991 - fix a problem with KR 1.5.2
  ;; bam - seems to still be needed 7-22-93
  (let ((formula (get-value schema :window)))
    (if (formula-p formula)
	(kr::set-cache-is-valid formula NIL))))

;; Maintains a list of all the interactors associated with a window
;; which can be used when the window is deleted.  It is not
;; sufficient to use the :priority-level-assoc array since that only
;; contains interactors that are :active, and we want to delete the
;; non-active ones also.
(defun Handle-Inter-List-for-Window (old-window new-window inter)
  (when old-window
    (cond ((schema-p old-window)
	   (deleteplace inter (g-value old-window :all-interactors)))
	  ((eq old-window t)
	   (dolist (win all-inter-windows)
	     (deleteplace inter (g-value win :all-interactors))))
	  ((listp old-window) 
	   (dolist (win old-window)
	     (when (schema-p win)
	       (deleteplace inter (g-value win :all-interactors)))))))
  (when new-window
    (cond ((schema-p new-window)
	   (pushnew inter (g-value new-window :all-interactors)))
	  ((eq new-window t)
	   (dolist (win all-inter-windows)
	     (pushnew inter (g-value win :all-interactors))))
	  ((listp new-window) 
	   (dolist (win new-window)
	     (when (schema-p win)
	       (pushnew inter (g-value win :all-interactors))))))))

;; Adds to correct priority level, and starts it up if should.  Only
;; call this if :active and :window are non-nil
(defun Add-to-Level-and-maybe-start (schema)
  (let ((starting (eq :start (g-value schema :current-state))))
    (Add-to-level schema (not starting) NIL NIL)
    (when (and starting (eq (g-value schema :start-event) t))
      (Internal-Start-Interactor schema T))))

;; sets up the default values for slots.  Called from all initialize methods
(defun Set-Up-Defaults (schema)
  (when (eq (g-value schema :feedback-obj) t)
    (error "** Sorry, creating a default feedback obj NIY ****"))
  (s-value schema :current-state :start)
  (let ((win (g-value schema :window)))
    ;; keep a list of all interactors associated with a window, for
    ;; when window is destroyed
    (Handle-Inter-List-for-Window NIL win schema)
    (when (and win (g-value schema :active))
      (when (eq win T)
	;; maintain the global list for inters that have (:window T)
	(pushnew schema *Inters-With-T-Window*))
      (s-value schema :copy-old-window  ; used to tell if window changed
	       (if (listp win) (copy-list win) win))
      (Add-to-Level-and-Maybe-Start schema))))

;; Go through all the interactors on *Inters-With-T-Window* and add
;; win to them.  This is called when a new window is created.
(defun Handle-New-Window-for-T-Inters (win)
  (dolist (inter *Inters-With-T-Window*)
    (let* ((running-p (if (eq (g-value inter :current-state) :start)
			  NIL T))
	   (level (if running-p
		      (g-value inter :running-priority)
		      (g-value inter :waiting-priority))))
      (if-debug :priority-level
		(format T "adding ~s to NEW window ~s priority level ~s~%"
			inter win level))
      (Add-Interactor-To-Level-Win inter level win T)
      ;; add inter to list of all the interactors for this window
      (pushnew inter (g-value win :all-interactors)) 
      (when running-p
	(pushnew inter (g-value win :current-want-moved-interactors))
	(gem:set-interest-in-moved win T)))))
      
;; pulls the aggregate object out of the :where field specified
(defun Get-Gob-Of-Where (where)
  (cond ((null where) NIL)
	((eq where T) T)
	((listp where)
	 (case (first where)
	   ((:in :in-box :element-of :list-element-of :in-but-not-on
		 :full-object-in
		 :element-of-or-none :leaf-element-of :custom
		 :leaf-element-of-or-none :list-leaf-element-of
		 :list-check-leaf-but-return-element
		 :check-leaf-but-return-element
		 :list-element-of-or-none :list-leaf-element-of-or-none
		 :list-check-leaf-but-return-element-or-none
		 :check-leaf-but-return-element-or-none
		 )
	    (second where))
	   (otherwise (error "** Unknown keyword in Where"))))
	(T (error "** Bad where, should be T, NIL, or a list"))))


;;; Noticing slots changing
;;

;; This is called whenever the interactor's active or window slots are changed
;; Called from Opal:update-slot-invalidated which is the kr::*invalidate-demon*
;;
(defun inter-update-slot-invalidated (inter slot save)
  (declare (ignore save slot))
  (pushnew inter *Changed-Interactors*))

  
(defun Handle-All-Changed-Interactors ()
  ;; use temporary pointer, so more robust in case this procedure
  ;; crashes, *Changed-Interactors* list will be NIL 
  (let ((l *Changed-Interactors*))
    (setq *Changed-Interactors* NIL)
    (dolist (inter l)
      (when (schema-p inter)		; want to see if destroyed or not.
	(Handle-Interactor-Slot-Changed inter)))))


(defun Handle-Interactor-Slot-Changed (inter)
  (let ((curlevel (g-value inter :current-priority-level))
	(old-window (g-value inter :copy-old-window))
	(new-window (g-value inter :window))
	(new-active (g-value inter :active)))
    (if-debug inter (Format T "Automatic fixing of ~s win ~s active ~s~%"
			    inter new-window new-active))
    (s-value inter :copy-old-window	; used to tell if window changed
	     (if (listp new-window) (copy-list new-window) new-window))

    ;; keep a list of all interactors associated with a window, for
    ;; when window is destroyed
    (unless (eq old-window new-window)
      (Handle-Inter-List-for-Window old-window new-window inter))

    (if (and new-window new-active)
	;; then should be added to a level
	(progn 
	  ;; first, maintain the global list for inters using (:window T)
	  (if (eq new-window T)
	      (pushnew inter *Inters-With-T-Window*)
					; else remove
	      (when (eq old-window T)
		(deleteplace inter *Inters-With-T-Window*)))

	  (if curlevel ;; then is on a priority-level of some window now
	      ;; First check to see if correct window and level
	      (unless (and (equal old-window new-window)
			   (eq curlevel	;; find new level
			       (if (eq :start (g-value inter :current-state))
				   (g-value inter :waiting-priority)
				   (g-value inter :running-priority))))
		;; remove from old windows
		(let ((wins (cond ((schema-p old-window) old-window)
				  ((null old-window) NIL)
				  ((eq old-window t) all-inter-windows)
				  ((listp old-window) old-window)
				  (t (error "bad old-window ~s in inter ~s"
					    old-window inter)))))
		  (s-value inter :current-priority-level NIL)
		  (if (listp wins)
		      (dolist (win wins)
			(Remove-Interactor-From-Level-Win inter curlevel
							  win T))
		      (Remove-Interactor-From-Level-Win inter curlevel
							wins NIL))
		  (Add-to-Level-and-Maybe-Start inter)))
	      ;; else didn't used to be on any levels
	      (Add-to-Level-and-Maybe-Start inter))) ; add to the right level

	;; else should stop running and be removed
	(when curlevel			; then used to be running
	  (when (eq old-window T)
	    (deleteplace inter *Inters-With-T-Window*))
	  (unless (eq :start (get-local-value inter :current-state))
	    ;; have to abort this interactor
	    (with-demon-disabled #'inter-update-slot-invalidated
	      (s-value inter :window old-window)
	      (Internal-Abort-Interactor inter) ; abort may not work unless
					; window has old value
	      (unless (eq old-window new-window)
		(s-value inter :window new-window))))
	  (Careful-Remove-Interactor-from-Level inter
						;; can't use curlevel variable since
						;; abort might change the level, so
						;; g-value new value
						(g-value inter :current-priority-level)
						old-window)))))


;; This function is used in process event to sort the interactors by
;; their :sort-order slot.  If no :sort-order then put at end of list.
(defun Sort-Inters (i1 i2)
  (let ((s1 (g-value i1 :sort-order))
	(s2 (g-value i2 :sort-order)))
    (cond ((and s1 s2) (< s1 s2))
	  (s1 T)
	  (s2 NIL)
	  (T NIL))))
	

;;; Middle level dispatcher for events; called from i-windows
;;

(defvar *global-first-accelerators* NIL) ; will be set in accelerators.lisp
(defvar *global-accelerators* NIL) ; will be set in accelerators.lisp

(defun Process-Event (event)
  (Check-and-handle-changed-inters) ;; first, make sure lists are up-to-date
  (let (win grab-win)
    (when event
      (setq win (event-window event))
      (let ((fn (or (cdr (assoc (event-char event)
				(if win (g-value win :first-accelerators))))
		    (cdr (assoc (event-char event)
				*global-first-accelerators*)))))
	(when fn
	  (if-debug :priority-level
		    (format T "----- Event grabbed by accelerator ~s~%" fn))
	  (funcall fn event)
	  (return-from process-event T)))
      (if-debug :priority-level
		(format T "~%------Doing events for window ~s~%" win))
      (if *Visible-Modal-Windows*
	  (unless (member win *Visible-Modal-Windows*)
	    (if-debug :priority-level
		      (format T " Exiting because not one of the Modal windows~%"))
	    (unless (and (event-mousep event) ; don't beep on mouse
					; unless down press
			 (not (event-downp event)))
	      (inter:beep)) ;; Is beeping a good idea??
	    (return-from process-event))
	  ;; if no modal windows visible, then use this
	  ;; gross hack in case press window set move to another Garnet window.
	  (when 
	      (and *Special-Grab-Up-Inter*
		   (not (eq (setq grab-win
				  (g-value *Special-Grab-Up-Inter* :window))
			    t))
		   (not (member win grab-win)))
	    ;; translate point to be in a window
	    (let ((win2 (first grab-win)))
	      (multiple-value-bind (x2 y2)
		  (opal:convert-coordinates win (event-x event) (event-y event)
					    win2)
		(if-debug :event
			  (format T "~% <*><*> Converting to window ~s (~s ~s)~%"
				  win2 x2 y2))
		(setf (event-x event) x2)
		(setf (event-y event) y2)
		(setf (event-window event) win2)
		(setf win win2)))))

      (if (eq (event-char event) :timer)
	  (error "Timer event got to process-event")
	  ;; else a regular event.
          ;; Note: claimed? tells if the event was claimed by an :if-any
          ;;       interactor or an :always priority-level
	  (let (winlevels found-one claimed? inter-array len
			  sorted-inters inter skip-set ex-value)
	    (setq winlevels (g-value win :priority-level-assoc))
	    (dolist (glo-level priority-level-list)
	      (setq found-one NIL)
	      (setq sorted-inters NIL)
	      (setq skip-set NIL)
	      (if (g-value glo-level :active)
		  (progn
		    (if (eq glo-level (caar winlevels))
			(progn
			  (if-debug :priority-level
				    (format T "~%------Doing priority level ~s~%"
					    glo-level))
			  (setq inter-array (cdar winlevels))
			  (setq winlevels (cdr winlevels))
			  (setq len (length inter-array))
			  ;; first make a copy of the level since
			  ;; might be modified
			  (when (> len (array-dimension *copy-array* 0))
			    (adjust-array *copy-array* len))
			  (dotimes (i len)
			    (setf (aref *copy-array* i)
				  (aref inter-array i)))
			  (when (g-value glo-level :sorted-interactors)
			    (if-debug :priority-level
				      (format T "Sorting the interactors~%"))
			    (setq sorted-inters T)
			    ;; fill-pointer is used by sort
			    (setf (fill-pointer *copy-array*) len)
			    (setq *copy-array*
				  (sort *copy-array* #'Sort-Inters)))
			  ;; now process each interactor
			  (dotimes (i len)
			    (setq inter (aref *copy-array* i))
			    (when (and (schema-p inter)
				       ;; if skip due to exclusivity
				       (or (not sorted-inters)
					   (not (setq ex-value
						      (g-value inter :exclusivity-value)))
					   (not (member ex-value skip-set)))
				       (General-Go inter event))
			      (setf found-one T)
			      (when (and sorted-inters
					 ex-value)
				;; then set up skip set 
				(push ex-value skip-set)
				(if-debug :priority-level
					  (format T "**Pushing ~s into skip set~%"
						  ex-value))))))
					 
			;; else not in window's assoclist
			)
		    ;; since active, check whether stop or not
		    (case (g-value glo-level :stop-when)
		      (:if-any (when found-one
				 (setq claimed? T)
				 (return)))
		      (:always (setq claimed? T) (return))
		      ((NIL) NIL)	; NIL means always go on to next
		      (t (error "bad :stop-when: ~s in priority level ~s"
				(g-value glo-level :stop-when)
				glo-level))))
		  ;; else not active
		  (progn 
		    (if-debug :priority-level ;; print that skipped
			      (format T " **Skipped because this level is not active~%"))
		    ;; check to see whether go to next level in other list
		    (when (eq glo-level (caar winlevels))
		      (setq winlevels (cdr winlevels)))
		    )))

	    ;; Now handle accelerators if event is not claimed
	    (unless claimed?
	      (let ((fn (or (when (and win (schema-p win))
			      (cdr (assoc (event-char event)
					  (g-value win :accelerators))))
			    (cdr (assoc (event-char event)
					*global-accelerators*)))))
		(when fn
		  (if-debug :priority-level
			    (format T "Event grabbed by accelerator ~s~%" fn))
		  (funcall fn event)
		  (setq claimed? T))))

	    claimed? ;; now process-event returns T iff event was claimed
	    )))))

;; these all do the same thing, information is already coded into the event.
(declaim (inline button-pressed))
(defun button-pressed (event)
  (Process-Event event))
(declaim (inline button-released))
(defun button-released (event)
  (Process-Event event))
(declaim (inline key-pressed))
(defun key-pressed (event)
  (Process-Event event))
(declaim (inline mouse-moved))
(defun mouse-moved (event)
  (Process-Event event))

;;; Utility procedures for the GO procedures
;;

(defun GetNextItem (item sequence)
  "Return the item after the parameter in the sequence or NIL if not there"
  (let ((index (position item sequence)))
    (when index (nth (1+ index) sequence))))

(defun Get-Running-where (an-interactor)
  "Running where can either be supplied or generated.  
If generated, it might be from start-where or if running-where
was (:xx *). See Fix-Running-Where for full details."
  (or (get-local-value an-interactor :generated-running-where)
      (g-value an-interactor :running-where)))


(defun checkobjtype (obj type)
  "Check to see if obj is a type.  Type can be a list, in which case
checks to see if obj is any of the types in the list."
  (cond ((eq type t))
	((listp type) (dolist (ty type)
			(when (is-a-p obj ty)
			  (return-from checkobjtype T))) NIL)
	(t (is-a-p obj type))))
      
(defun list-element-of-branch (agg control slot type win x y)
  (let ((lst (g-value agg slot)))
    (or (dolist (i lst)
	  (when (and (checkobjtype i type)
		     (eq (g-value i :window) win)
		     (opal:point-in-gob i x y))
	    (return i)))
	;; if loop doesn't return anything, then return :none if
	;; supposed to
	(if (eq control :list-element-of-or-none)
	    :none
	    NIL))))

(defun list-leaf-element-of-branch (agg control slot type win x y)
  (let ((lst (g-value agg slot))
	ret)
    (or (dolist (i lst)
	  (when
	      (and (eq (g-value i :window) win)
		   (if (is-a-p i opal:aggregate)
		       ;; if aggregate, then if point-to-leaf
		       (progn
			 (setq ret
			       (kr-send i :point-to-leaf i x y :type type))
			 (when (and ret
				    (or (eq control :list-check-leaf-but-return-element)
					(eq control :list-check-leaf-but-return-element-or-none)))
			   (setq ret i))
			 ret)
		       ;; if not aggregate, then if inside
		       (progn
			 (when (and (checkobjtype i type)
				    (opal:point-in-gob i x y))
			   (setq ret i)))))
	    (return ret)))
	;; if loop doesn't return anything, then return :none if
	;; supposed to
	(if (or (eq control :list-leaf-element-of-or-none)
		(eq control :list-check-leaf-but-return-element-or-none))
	    :none
	    NIL))))

(defun check-leaf-but-return-element-branch (agg an-interactor control type
					     win x y)
  #-garnet-debug (declare (ignore an-interactor))
  (when (opal:point-in-gob agg x y)
    ;; otherwise, return NIL always
    (dolist (child (reverse (g-value agg :components)))
      ;; do them in reverse order, since components list is back to front
      (when (and (eq (g-value child :window) win)
		 (if (is-a-p child opal:aggregate)
		     ;; if aggregate, then if point-to-leaf
		     (kr-send child :point-to-leaf child x y :type type)
		     ;; if not aggregate, then if inside
		     (and (checkobjtype child type)
			  (opal:point-in-gob child x y))))
	(if-debug an-interactor
		  (format T " ** SUCCESS: ~s~%" child))
	(return-from check-leaf-but-return-element-branch child)))
    ;; if get here, then didn't find any children, return NIL or :none
    (if (eq control :check-leaf-but-return-element-or-none)
	:none
	NIL)))


(defun check-location (event which-where an-interactor)
  "XXX FMG Not sure if this matches the code. 
checks to see if x,y is inside WHERE, returns the object 
under the mouse if passes.  If WHERE is :element-of, this
will be the element object.  If WHERE is :in or :in-box, 
then will be the object itself.  Returns NIL if fails
******** BUG ***NO WAY FOR OVERLAPPING OBJECTS TO HIDE EACH OTHER FROM
******** THE MOUSE!  (Have to use priority levels)"
  (let ((where (case which-where
		 (:start-where (g-value an-interactor :start-where))
		 (:running-where (Get-Running-where an-interactor))
		 (t (error "bad which-where"))))
	result)
    (if-debug an-interactor
	      (format T "Checking ~s = " which-where))
    (setq result
	  (cond ((eq where t))		; T means anywhere in the window
		((null where))		; NIL as where means failure, useful to have
					; interactor not run (e.g, start-where is a formula)
		((listp where)
		 (let ((x (event-x event))
		       (y (event-y event))
		       (win (event-window event))
		       (control (first where))
		       (agg (second where))
		       (type (or (GetNextItem :type where) T)) ; T as type => everything
		       objwin slot)
		   (unless (schema-p agg)
		     (Error-Print (concatenate 'string "Object in :"
					       (symbol-name which-where))
				  agg "Opal Object" an-interactor))
		   (setf objwin (and agg (g-value agg :window)))
		   (if (or (eq control :list-element-of)
			   (eq control :list-leaf-element-of)
			   (eq control :list-check-leaf-but-return-element)
			   (eq control :list-element-of-or-none)
			   (eq control :list-leaf-element-of-or-none)
			   (eq control :list-check-leaf-but-return-element-or-none)
			   (eq control :custom))
		       ;; If a list, then objects might be in different
		       ;; windows, so do test inside the case statement.
		       ;; If Custom, let the user's procedure do the test.
		       (setf slot (third where))
		       ;; else check if window of object eq window of event
		       (unless (eq win objwin) ; otherwise test here
			 (if-debug an-interactor
				   (format T " **WINDOWS DON'T MATCH** ev-win=~s obj-win=~s~%" 
					   win objwin))
			 (return-from check-location NIL)))
		   (if-debug an-interactor (format T "~s of ~s" control agg)
			     (when slot (format T " slot ~s" slot)))
		   (case control
		     (:custom		; function from (third where) copied into "slot"
		      (apply slot agg an-interactor event (cdddr where)))
		     (:element-of
		      (kr-send agg :point-to-component agg x y :type type))
		     ((:list-element-of
		       :list-element-of-or-none)
		      (list-element-of-branch agg control slot type win x y))
		     ((:list-leaf-element-of
		       :list-check-leaf-but-return-element
		       :list-leaf-element-of-or-none
		       :list-check-leaf-but-return-element-or-none)
		      (list-leaf-element-of-branch agg control slot type win x y))
		     ((:check-leaf-but-return-element
		       :check-leaf-but-return-element-or-none)
		      (check-leaf-but-return-element-branch agg an-interactor
							    control type win x y))
		     (:leaf-element-of
		      (kr-send agg :point-to-leaf agg x y :type type))
		     (:leaf-element-of-or-none ;if in agg, then :none or object over
		      (if (opal:point-in-gob agg x y)
			  (or (kr-send agg :point-to-leaf agg x y :type type)
			      :none)	; return :none if inside and no child
					; else return NIL if not in agg
			  NIL))
		     (:in-box
		      ;; the top level point-in-gob method uses the bounding
		      ;; rectangle, whereas the specific object may have a special
		      ;; procedure
		      (and (kr-send opal:view-object :point-in-gob agg x y)
			   agg))	;return agg if in box
		     (:in
		      (and (opal:point-in-gob agg x y)
			   agg))	;return agg if in
;;;                      **** NIY **********************
;;;                      (:full-object-in ;;entire object to move ;
;;;                      (let ((obj (g-value an-interactor :obj-being-changed)))
;;;                      (unless obj
;;;                      (error "No object being changed for :full-object-in"))
;;;                      (and (opal:gob-in-gob agg obj)
;;;                      agg)))  ;return agg if in ;
;;;                      ******************
		 (:in-but-not-on
		  (and (not (kr-send agg :point-to-component agg x y
				     :type type))
		       (opal:point-in-gob  agg x y)
		       agg))
		 (:element-of-or-none ;if in agg, then :none or object over
		  (if (opal:point-in-gob agg x y)
		      (or (kr-send agg :point-to-component agg x y :type type)
			  :none)  ; return :none if inside and no child
		      ;; else return NIL if not in agg
		      NIL))
		 (t (error "** illegal where control: ~s" where)))))
	  (t (Error-Print (symbol-name which-where) where "T, NIL or list"
			  an-interactor))))
    (if-debug an-interactor
	      (if result (format T " **SUCCESS=~s~%" result)
		  (format T " **FAIL**~%")))
    result))



(defun convert-mouse-down (button-code)
  "handles the transformation of simple down events to up events.
this may not be  the most elegant way of doing this, but for now it is acceptable"
  (cond ((null button-code) :any-mouseup)
	((= inter::*left-button* button-code) :any-leftup)
	((= inter::*middle-button* button-code) :any-middleup)
	((= inter::*right-button* button-code) :any-rightup)
	((= inter::*double-left-button*  button-code) :any-leftup)
	((= inter::*double-middle-button* button-code) :any-middleup)
	((= inter::*double-right-button* button-code) :any-rightup)))

(defun compare-and-get-possible-stop-event (event event-desired)
  "checks to see if the event from the window manager (wm-event) matches
the desired event event-descriptor.  If so, returns matching stop-event
or NIL.

No checking for illegal keywords, they just always fail.

Converting and checking are combined, because they both have to go through
the same cases (for all the special keywords)."
  (let ((mousep (inter::event-mousep event))
	(code (inter::event-code event))
	(key-button (inter::event-char event))
	(downp (inter::event-downp event)))
    
    (cond ((eq event-desired key-button)
	   (cond ((eq key-button :window-enter) :window-exit)
		 ((eq key-button :window-exit) :window-exit)
		 ((and mousep
		       ;; returns the right value
		       (inter::convert-mouse-down code)))
		 (T key-button)))	; otherwise, just use value
	  ((and (eq event-desired :any-keyboard)
		(not mousep)
		(not (eq key-button :timer))) ; timer doesn't
	   ;; match keyboard
	   #\RETURN)
	  
	  ((or (and (eq event-desired :any-mouseup) mousep (null downp)
		    code)		; code is NIL for window-exit and -enter
	       (and (eq event-desired :any-mousedown) mousep downp))
	   :any-mouseup)
	  
	  ((and (eq event-desired :any-leftdown)
		mousep downp (eq code inter::*left-button*)) :any-leftup)
	  ((and (eq event-desired :any-middledown)
		mousep downp (eq code inter::*middle-button*)) :any-middleup)
	  ((and (eq event-desired :any-rightdown)
		mousep downp (eq code inter::*right-button*)) :any-rightup)
	  
	  ((and (eq event-desired :any-leftup)
		mousep (null downp) (eq code inter::*left-button*)) :any-leftup)
	  ((and (eq event-desired :any-middleup)
		mousep (null downp) (eq code inter::*middle-button*)) :any-middleup)
	  ((and (eq event-desired :any-rightup)
		mousep (null downp) (eq code inter::*right-button*)) :any-rightup))))
;; if none of these pass, then the event doesn't match

  

;; This procedure generates a stop event for an interactor based on its
;; :start-event.  This does not use an actual event from X, so it just picks
;; a plausible stop event.  This is called from Start-Interactor which starts
;; an interactor without a real event happening
(Defun Set-Invented-stop-event (an-interactor)
  (unless (g-value an-interactor :stop-event) ; don't bother if there is one
    (s-value an-interactor :generated-stop-event
	     (list :any-mouseup #\RETURN)))) ; this seems safe: either a
				             ; mouse up or a CR

(defun Check-Event (actual-event which-event interact)
  "Looks in the "which-event" field of the "interact" and compares that to
actual-event.  This comparison is fairly tricky because of all the
possible kinds of event descriptions (:any-xxx, lists with exceptions,
etc.)  If this is a start-event, then generates a corresponding stop
event, in case needed, and stores this in the interactor."
  (let ((result (Int-Check-event actual-event which-event interact)))
    (if-debug interact
	      (if result (format T " **SUCCESS=~s~%" result)
		  (format T " **FAIL~%")))
    result))

(defun Int-Check-event (actual-event which-event interact)
  (if-debug interact (format T "Checking ~s " which-event))

  (let ((stop-event nil)
	(events-desired (g-value interact which-event)))

    ;; check if actual-event is :mouse-moved
    (when (eq (event-char actual-event) :mouse-moved)
      (return-from int-check-event
	(if (eq which-event :start-event) ; then see if T
	    (if (eq events-desired T) :maybe NIL)
	    ;; else if not start-event, always return NIL for moved
	    NIL)))

    ;; actual-event is not mouse-moved  
    (when (and (null events-desired) (eq :stop-event which-event))
      ;; when null, use the generated default stop event, if any
      (setq events-desired (g-value interact :generated-stop-event)))

    (if-debug interact (format T " against wanted ~s" events-desired))
    (cond ((eq events-desired T)
	   ;; for :abort-event or :Stop-event, T means don't stop
	   (if (eq which-event :start-event)
	       (return-from int-check-event :maybe)
	       (return-from int-check-event NIL)))
	  ((null events-desired) (return-from int-check-event NIL))
	  ((listp events-desired)
	   ;; check the exceptions first
	   (dolist (exception (member :except events-desired))
	     (when (compare-and-get-possible-stop-event
                    actual-event exception)
	       (return-from int-check-event nil)))
	   ;; check allowable events
	   (dolist (option events-desired)
	     (if (eq option :except) (return-from int-check-event nil)) 
	     (when (setf stop-event
			 (compare-and-get-possible-stop-event
                          actual-event option))
	       (return))))		; break out of the dolist loop
	  ;; here, not a list
	  (t (setf stop-event (compare-and-get-possible-stop-event
                               actual-event events-desired))))
    
    (when stop-event			; then should return T, otherwise NIL
      (when (eq which-event :start-event)
	;; set default stop-event in case needed
	(s-value interact :generated-stop-event stop-event))
      t)))

(defun Fix-Running-Where (an-interactor new-obj-over)
  "If running-where is empty, then copies :start-where.
Otherwise, checks to see if running-where is of the form '(:xx *), 
then changes running-where to be '(:xx new-obj-over).
This is called from every interactor's start procedure 
if it is continuous."
  (let ((r-w (g-value an-interactor :running-where))
	r-w-copy)
    (if (null r-w) ; then copy start-where
	(s-value an-interactor :generated-running-where
		 (G-Value an-interactor :start-where))
	;; start-where cannot use the '* form, so don't have to worry about
	;; that if copying the start-where into running where.
	;; otherwise, check if need to have a special, edited start-where
	(if (and (listp r-w) (eq '* (second r-w)))
	    ;; then need to substitute new-obj-over for *
	    (progn
	      (unless (and (setq r-w-copy
				 (get-local-value an-interactor
						  :generated-running-where))
			   (not (eq r-w-copy T))
			   (not (eq (car r-w-copy)(car r-w))))
		;; make a copy of the running-where, in case it is inherited,
		;; since are going to destructively modify it
		;; BUG: if running-where changes to have a new :type or
		;; something, this will fail to notice it.
		(setq r-w-copy (copy-list r-w)))
	      (if (eq new-obj-over t)
		  (s-value an-interactor :generated-running-where T) ; then just use T
		  (progn					     ; else use the copy
		    (setf (second r-w-copy) new-obj-over)
		    (s-value an-interactor :generated-running-where r-w-copy))))
	  ;; otherwise, remove generated-running-where.
	  ;; Destroy-slot checks whether slot is there or not.
	  (destroy-slot an-interactor :generated-running-where)))))

(defun GoToRunningState (an-interactor needMouseMoved)
  (s-value an-interactor :current-state :running)
  (Move-levels an-interactor NIL T needMouseMoved))

(defun GoToStartState (an-interactor set-waiting-level)
  (s-value an-interactor :current-state :start)
  (when (g-value an-interactor :self-deactivate)
    (s-value an-interactor :active NIL))
  (when set-waiting-level
    (Move-levels an-interactor T NIL NIL)))

  

;;; Main General go procedure
;;

(defun General-Go (an-interactor event)
  "This is the main action procedure that makes the most 
interactors go. This procedure implements the state machine.  
It is called by the main dispatcher when an event happens.
The Event is the value returned by the window manager.
This procedure calls the Do-xxx procedures in the
interactor, which are specialized for the particular type 
of interactor. The do-xxx procedures in turn call the
xxx-action procedures.  These -action procedures my be
supplied by outside applications.

The complexity in the state machine implementation is that 
the same event may cause two things, e.g., both going outside 
and stop. We cannot count on getting different events for this."
  (let ((state (get-local-value an-interactor :current-state))
	(active (g-value an-interactor :active))
	(window (g-value an-interactor :window))
	(event-window (event-window event))
	return-val)
    (if-debug an-interactor
	      (format T "~%enter GO for ~s, state=~s...~%"
		      an-interactor state))

    (unless state
      (if-debug an-interactor
		(format T "returning because state=NIL~%"))
      (return-from General-Go NIL))	; quick return if state is NIL (which
					; means that the inter has been destroyed)
    ;; now, must have both :active and :window as Non-NIL
    (unless (and active window)
      (unless (eq :start state)
	(if-debug an-interactor (format T "** Implicit become inactive~%")) 
	(Kr-Send an-interactor :Do-abort an-interactor T event)
	(opal:update-all)
	)
      (if-debug an-interactor
		(format T "returning because ~a in inter is NIL~%"
			(if active ":window" ":active")))
      (return-from General-Go NIL))	; return NIL
    (cond ((schema-p window)
	   (when (not (eq window event-window))
	     (if-debug an-interactor
		       (format T "returning because event window ~s doesn't ~
                                  match inter windows ~s~%" event-window window))
	     (return-from General-Go NIL)))
	  ((eq window T))		; then just go on
	  ((listp window)		; then check if member
	   (unless (member event-window window)
	     (if-debug an-interactor
		       (format T "returning because window ~s isn't in list ~s~%"
			       event-window window))
	     (return-from General-Go NIL)))
	  (t (error "Window slot of inter ~s has wrong form" an-interactor)))

    ;; Finished preliminary tests, now get to work

    (let (obj should-stop)
      (case state
	(:animating NIL)		; just ignore animators
	(:start (s-value an-interactor :current-window event-window)
		(if (and (check-event event :start-event an-interactor)
			 ;; return of :maybe OK here
			 (setf obj
			       (check-location event :start-where an-interactor)))
		    (progn
		      (if-debug :short
				(format T "starting ~s~%" an-interactor))
		      (Check-and-handle-debug-next-inter)
		      ;; these next two slots might be used in formulas
		      (s-value an-interactor :first-obj-over obj)
		      (s-value an-interactor :current-obj-over obj)
		      (s-value an-interactor :start-char (event-char event))
		      (setq return-val (Kr-Send an-interactor :Do-start
						an-interactor obj event)))
		    ;; else exit and return NIL
		    (return-from general-go NIL)))
	(:running (Check-and-handle-debug-next-inter)
		  (s-value an-interactor :current-window event-window)
		  (if (check-event event :abort-event an-interactor)
		      (progn
			(if-debug :short
				  (format T "aborting ~s~%" an-interactor))
			(setq return-val (Kr-Send an-interactor :Do-abort
						  an-interactor NIL event)))
		      (progn ;; else
			(setf obj
			      (check-location event :running-where an-interactor))
			(setf should-stop
			      (check-event event :stop-event an-interactor))
			
			(if (null obj)	;went outside
			    (progn
			      (if-debug :short
					(format T "outside for ~s~%" an-interactor))
			      (setq return-val (Kr-Send an-interactor
							:Do-outside an-interactor))
			      (when (eq should-stop T)
				(setq return-val (Kr-Send an-interactor
							  :Do-outside-stop
							  an-interactor event))))
					;here still inside; obj is ok
			    (progn
			      (s-value an-interactor :current-obj-over obj)
			      (if (eq should-stop T)
				  (progn
				    (if-debug :short
					      (format T "stopping ~s~%" an-interactor))
				    (setq return-val (Kr-Send an-interactor
							      :Do-stop an-interactor obj event)))
				  (progn
				    (if-debug :short
					      (format T "running ~s~%" an-interactor))
				    (setq return-val 
					  (Kr-Send an-interactor
						   :Do-running an-interactor obj event)))))))))
	(:outside (Check-and-handle-debug-next-inter)
		  (s-value an-interactor :current-window event-window)
		  (if (check-event event :abort-event an-interactor)
		      (progn
			(if-debug :short
				  (format T "aborting ~s~%" an-interactor))
			(setq return-val (Kr-Send an-interactor :Do-abort
						  an-interactor NIL event)))
		      ;; else don't abort
		      (progn
			(setf obj
			      (check-location event :running-where an-interactor))
			(setf should-stop
			      (check-event event :stop-event an-interactor))
			(if-debug :short
				  (format T "outside for ~s~%" an-interactor))
			;; still outside
			(if (null obj)
			    (when should-stop
			      (setq return-val
				    (Kr-Send an-interactor
					     :Do-outside-stop an-interactor event)))
			    ;; else go back inside; obj is ok
			    (progn
			      (s-value an-interactor :current-obj-over obj)
			      (setq return-val (Kr-Send an-interactor
							:Do-back-inside an-interactor
							obj event))
			      (when should-stop
				(setq return-val 
				      (Kr-Send an-interactor
					       :Do-stop an-interactor obj event))))))))
	(otherwise (error "** illegal state ~s" state))))
    (opal:update-all)			; update all windows
    (not (eq return-val :stop))))



;;; Exported procedures
;;

;;============================================================
;; Main, top level inter:interactor object

(Create-Schema 'interactor
    :declare ((:type (inter-window-type :window)
		     ((is-a-p inter::priority-level)
		      :waiting-priority :running-priority)))
    (:current-state :start)

    (:self-deactivate NIL)
    (:window NIL)
    (:active T)
    (:continuous T)
    (:final-function NIL)
    (:waiting-priority normal-priority-level)
    (:running-priority running-priority-level)
    (:start-where :not-supplied)
    (:start-event :leftdown)
    (:stop-event NIL)
    (:abort-event :control-\g)
    (:running-where NIL)
    (:start-action :not-supplied)
    (:running-action :not-supplied)
    (:stop-action :not-supplied)
    (:abort-action :not-supplied)
    (:outside-action :not-supplied)
    (:back-inside-action :not-supplied)
    (:feedback-obj NIL)
    (:current-priority-level NIL)	; internal slot
    (:local-only-slots '(:current-state NIL)
		       '(:operates-on NIL)
		       '(:current-priority-level NIL))
    (:update-slots '(:active :window	; for KR
		     :waiting-priority :running-priority))
    (:invalidate-demon #'inter-update-slot-invalidated)
    (:Do-Start :not-supplied)		; these are
    (:Do-Running :not-supplied)		;   called by GO
    (:Do-Stop :not-supplied)		;   to do
    (:Do-Abort :not-supplied)		;   the real work.
    (:Do-Explicit-Stop :not-supplied)
    (:Do-Outside :not-supplied)		;   They call the
    (:Do-Back-Inside :not-supplied)	;   appropriate
    (:Do-Outside-Stop :not-supplied)	;   -action procedures
    (:Initialize 'Top-Interactor-Initialize)) ;proc to call when created


;; If erase is T, then aborts the interactor.  This may not be necessary,
;; for example if the window is about to be destroyed.
;; It is more robust to have erase NIL.
(define-method :destroy-me interactor (an-interactor &optional (erase T))
  (if-debug an-interactor
	    (format T "Interactor Destroying ~s erase=~s~%" an-interactor erase))
  (when (and erase (g-value an-interactor :active))
    (change-active an-interactor NIL))

  ;; if on changed list, remove it, should really
  ;; (Check-and-handle-changed-inters)  but this causes an Asynchronous
  ;; WINDOW-ERROR, so check in various places that the inter is still valid
  (deleteplace an-interactor *changed-interactors*)

  ;; if on T-list, remove it; does nothing if not there
  (deleteplace an-interactor *Inters-With-T-Window*) 
  ;; if special, remove it
  (when (eq an-interactor *Special-Grab-Up-Inter*)
    (setq *Special-Grab-Up-Inter* NIL))
  ;; update list of all interactors associated with a window, for
  ;; when window is destroyed
  (Handle-Inter-List-for-Window (g-value an-interactor :copy-old-window)
				NIL an-interactor)
  (Remove-from-all-levels an-interactor)
  
  (let ((in-obj (g-local-value an-interactor :operates-on)))
    (when (and (schema-p in-obj)
	       (not (kr::slot-constant-p in-obj :operates-on)))
      (if-debug an-interactor
		(format T "Removing me from object ~s~%" in-obj))
      (kr-send in-obj :remove-local-interactor in-obj an-interactor)))
  (destroy-schema an-interactor))

(define-method :destroy interactor (an-interactor &optional (erase T))
  (dolist (instance (copy-list (get-local-value an-interactor :is-a-inv)))
     (kr-send instance :destroy instance erase))
  (kr-send an-interactor :destroy-me an-interactor erase))



;; ============================================================
;; Future: Make move-grow, etc not eat keyboard events, and keyboard
;; interactors not eat mouse events.
;; ============================================================
