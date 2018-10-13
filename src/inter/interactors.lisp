;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;
;;;  The Garnet User Interface Development Environment.
;;;
;;;  This code was written as part of the Garnet project at Carnegie
;;;  Mellon University, and has been placed in the public domain.
;;;
;;;  This file includes the design for mouse and keyboard interactors
;;;  Designed and implemented by Brad A. Myers


(in-package :interactors)


;;; Variables used for noticing changed slots

(defparameter *changed-interactors* nil
  "A list of the interactors that have a changed slot.  Clean-up
with (Check-and-handle-changed-inters)")

(defparameter *inters-with-t-window* nil
  "A list of all the interactors that have a (:window T).
This is needed because when new windows are  created,
they need to be added to these interactor's lists.")

(defparameter *visible-modal-windows* nil
  "A list of the windows that are modal (stealing all input)
that are visible.  When this list is non-nil, only interactors
in these windows will operate.  Note that sub-windows of these
windows must have their :modal-p bit set.  This list is maintained
automatically by update on interactor windows looking at the
:modal-p and :visible slots of the windows.")

(defparameter *special-grab-up-inter* nil
  "Holds a single interactor. When an interactor is supposed to work
over multiple windows, it won't get events when the mouse is over
other garnet windows.  This variable is used as a hack to make
those events go to this interactor, when the interactor is running.
The variable is set by turn-on{off}-mouse-moved.")


(declaim (inline check-and-handle-changed-inters))
(defun check-and-handle-changed-inters ()
  "this should be called at update and when new events come in"
  (when *changed-interactors*
    (handle-all-changed-interactors)))

;;; handy functions
;;
(defun beep ()
  "causes a beep or bell to sound"
  (gem:beep (g-value gem:device-info :current-root)))

(defun warp-pointer (window x y)
  "move the cursor to the specified x y position in the window."
  (gem:set-window-property window :pointer-position (cons x y)))



;;; debugging aids:
;;   if-debug is a macro for use around debugging code
;;   trace-test is a (possibly expensive) test to enable selective tracing
;;   trace-inter is a function to start tracing an interactor (t for all of them)
;;   untrace-inter is a function to stop tracing an interactor or all of them

;; ** the debugging information is only generated if the
;; #+garnet-debug feature is present.  otherwise nothing is generated.
;;

(defparameter *int-debug* nil
  "true if any debugging is enabled")
(defparameter *int-trace* nil
  "list of interactors to be traced")
(defparameter *debug-next-inter* nil
  "a function to be called on the next interactor to run")
(defparameter *special-trace-values* '(:window :priority-level :mouse :event
				       :next :short))


;; test for selective tracing. put this around any print-out statements
;; ** only generates code if #+garnet-debug feature is present at compile time
;;
;; note: inter is an interactor or may be one of:
;;       :window -- trace things about interactor windows (create, destroy, etc.)
;;       :priority-level -- trace changes to priority levels
;;       :mouse -- trace set-interested-in-moved and ungrab-mouse
;;       :event -- show all events that come in
;;       :next -- trace the next interactor to run"
(defmacro if-debug (inter &rest body)
  #+garnet-debug
  `(when (and *int-debug* (trace-test ,inter))
     (let ((*print-pretty* nil))
       ,@body))
  #-garnet-debug
  (declare (ignore inter body))
  )

(defmacro debug-p (inter)
  "returns t or nil based on whether should trace or not.  should be same
test as in if-debug (used when debugging value being passed as a parameter).
if no debug (because :garnet-debug is not on *features*), just generate nil"
  #+garnet-debug
  `(and *int-debug* (trace-test ,inter))
  #-garnet-debug
  (declare (ignore inter))
  nil)

(defun trace-test (inter)
  "hook used to enable/disable fancy tracing"
  (or (eq *int-trace* t) ;trace all
      (and inter (member inter *int-trace*))))

(declaim (special interactor)) ; defined below

(defun trace-next-inter-to-run (an-interactor)
  "this function is set into the variable *debug-next-inter* when calling
(trace-inter :next)"
  (format t "~%------------~%begin tracing ~s~%~%" an-interactor)
  (trace-inter an-interactor))

;; trace-inter -- trace a function (or all functions)
;;
#+garnet-debug
(defun trace-inter (&optional (trace-what :status))
"trace interactor execution.  (inter:untrace-inter removes tracing.)
  can be called more than once and adds new parameter to trace set.
  if no parameter, then prints what are tracing. legal values for parameter are:
    t - trace everything
    an interactor - trace on that interactor
    nil - untrace everything
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
	  (format t "already tracing everything!~%"))
	 ((member trace-what *int-trace*)
	  (format t "already tracing ~s~%" trace-what))
	 ((not (or (is-a-p trace-what interactor)
		   (member trace-what *special-trace-values*)))
	  (format t "*** ~s is not an interactor or one of the special values:
	     t nil ~{~s ~}
 (describe 'inter:trace-inter) for more information~%"
		  trace-what *special-trace-values*))
	 ((eq trace-what :next)
	  (setq *debug-next-inter* #'trace-next-inter-to-run))
	 (t (push trace-what *int-trace*)
	    (pushnew :event *int-trace*))) ; always trace events when
					; tracing anything else
	  (setf *int-debug* (not (null *int-trace*)))
	  *int-trace*)

#-garnet-debug
(defun trace-inter (&optional (trace-what :status))
  (declare (ignore trace-what))
  (format t "** can't debug since interactors was compiled with
        :garnet-debug not in the *features* list (see garnet-loader.lisp)."))

#+garnet-debug
(defun untrace-inter (&optional untrace-what)
"turns off tracing on the parameter.  if no parameter supplied, then turns off
 all tracing.  see trace-inter for description of parameters."
  (cond ((or (eq untrace-what t) (null untrace-what))
	       (setf *int-trace* nil))
	      ((member untrace-what *int-trace*)
	       (setf *int-trace* (delete untrace-what *int-trace*)))
	      (t
	       (format t "not tracing ~s%" untrace-what)))
  ;; only enable debugging if user is tracing some interactor
  (setf *int-debug* (not (null *int-trace*)))
  *int-trace*)

#-garnet-debug
(defun untrace-inter (&optional untrace-what)
  (declare (ignore untrace-what))
  (format t "** can't debug since interactors was compiled with
        :garnet-debug not in the *features* list (see garnet-loader.lisp)."))


;;; general code.

(defun error-print (slotdesc value shouldbe inter)
  (error "the ~a of the interactor ~s should be a ~a,~%     but it is ~s.~%"
	 slotdesc inter shouldbe value))

(defun count-interactors ()
  (multiple-value-bind (total inactive)
      (count-interactors-internal interactor)
    (format t "there are ~s interactors, of which ~s are in-active, ~s are active~%"
	    (1+ total) (1+ inactive)(- total inactive))))

(defun count-interactors-internal (root)
  (let ((total (length (get-local-value root :is-a-inv)))
	(inactive 0))
    (dolist (inter (get-local-value root :is-a-inv))
      (unless (and (g-value inter :active)
		   (g-value inter :window))
	(incf inactive))
      (multiple-value-bind (sub-total sub-inactive)
	  (count-interactors-internal inter)
	(incf total sub-total)
	(incf inactive sub-inactive)))
    (values total inactive)))

(declaim (inline  check-and-handle-debug-next-inter))
(defun check-and-handle-debug-next-inter (an-interactor)
  "this is called by general-go when there is a value in
*debug-next-inter*.  calls the function in *debug-next-inter* and
clears the variable."
  (when *debug-next-inter*
    (handle-debug-next-inter an-interactor)))

(defun handle-debug-next-inter (an-interactor)
  (let ((fn *debug-next-inter*))
    (setq *debug-next-inter* nil)	; clear first in case the function crashes
    (funcall fn an-interactor)))


;;; macros to print debugging information
;;

(defun dbprinter (slot obj val feedbackp)
  (format t "  * setting ~s of ~s~a to ~s~%" slot obj
	  (if feedbackp " (feedback-obj)" "")
	  val))

(defmacro dbprint (slot obj val inter)
  `(if-debug ,inter (dbprinter ,slot ,obj ,val nil)))

(defun dbprint-either (slot obj val inter feedbackp)
  `(if-debug ,inter (dbprinter ,slot ,obj ,val ,feedbackp)))

(defmacro dbprint-sel (slot obj val inter)
  `(if-debug ,inter (dbprinter ,slot ,obj ,val nil)))

(defmacro dbprint-feed (slot obj val inter)
  `(if-debug ,inter (dbprinter ,slot ,obj ,val t)))

(defun dbstrprinter (obj feedbackp)
  (format t "  * setting :string of ~s~a to ~s and :cursor-index to ~s~%" obj
	  (if feedbackp " (feedback-obj)" "")
	  (g-value obj :string)
	  (g-value obj :cursor-index)))

(defmacro dbprint-str (obj inter feedbackp)
  `(if-debug ,inter (dbstrprinter ,obj ,feedbackp)))


;;; priority levels
;;

(create-schema 'priority-level
   ;; bam: the interactors list is now stored with the
   ;; windows, not here in the interactors list
   (:active t)				; if nil, then this level is totally ignored,
					; including its :stop-when field.  this can be a
					; formula, but if it changes to be nil, interactors
					; will not be automatically aborted.  use
					; change-active to get that behavior.
   (:stop-when :if-any)			; choices are :if-any, :always, nil
					; :if-any - then doesn't go down to the next
					; 	level if anything on this level runs.
					; :always - then never goes down to next level
					; nil - never stops after this level (always goes on)
   (:sorted-interactors nil)		; if t then runs the inters
					; in order by the :sort-order
					; field of the inter and
					; stops at first to run
 )

(create-schema 'normal-priority-level
	       (:is-a priority-level)
	       (:active t)
	       (:stop-when :if-any))

(create-schema 'high-priority-level
	       (:is-a priority-level)
	       (:active t)
	       (:stop-when :if-any))

(create-schema 'running-priority-level
	       (:is-a priority-level)
	       (:active t)
	       (:stop-when :if-any))

(defparameter priority-level-list
  (list running-priority-level high-priority-level normal-priority-level))


;; useful macro
;;
(defmacro deleteplace (item place)
  `(setf ,place (delete ,item ,place)))


;;; events
;;

;; this defines event-x event-y event-char event-code event-mousep
;; event-downp event-window event-timestamp
(defstruct (event (:print-function print-event))
  (window nil)
  (char :leftdown)
  (code 1)
  (mousep t)
  (downp t)
  (x 0)
  (y 0)
  (timestamp 0)
  (state nil))

(defun print-event (event stream depth)
  (declare (ignore depth))
  (format stream "#ev<char:~s code:~s mouse:~s dn:~s x:~s y:~s time:~s win:~s>"
	  (event-char event) (event-code event) (event-mousep event)
	  (event-downp event) (event-x event) (event-y event)
	  (event-timestamp event)(event-window event)))

(defparameter *current-event* (make-event))

(declaim (special all-inter-windows))



;;; exported procedures

;; call this to change the active slot of an interactor or of an interactor
;; priority-level.  new-value should be t or nil.  this procedure will make
;; sure that the interactor (or all the interactors at the priority level) are
;; aborted if becoming in-active.  you can also set the active slot
;; of an interactor directly.
(defun change-active (inter-or-level
		      &optional
			(new-value (g-value inter-or-level :active)))
  "call this to change the status of an interactor or interactor priority-level
immediately.  if active=t then will run, if active=nil, then will not run."
  (cond ((is-a-p inter-or-level interactor)
	 (if new-value
	     (progn
	       (if-debug inter-or-level
			 (format t "change interactor ~s to active~%" inter-or-level))
	       (s-value inter-or-level :active t))
	     (progn
	       (if-debug inter-or-level
			 (format t "change interactor ~s to in-active~%" inter-or-level))
	       (s-value inter-or-level :active nil)
	       (unless (eq :start (get-local-value inter-or-level :current-state))
		 (kr-send inter-or-level :do-abort inter-or-level t nil)
		 (opal:update-all)))))
	((is-a-p inter-or-level priority-level)
	 (if new-value
	     (progn
	       (if-debug :priority-level
			 (format t "change priority level ~s to active~%"
				 inter-or-level))
	       (s-value inter-or-level :active t))
	     (progn
	       (if-debug :priority-level
			 (format t "change priority level ~s to in-active~%"
				 inter-or-level))
	       (s-value inter-or-level :active nil)
	       (dolist (win all-inter-windows)
		 (let ((localassoc (assoc inter-or-level
					  (g-value win :priority-level-assoc)))
		       inter)
		   (when localassoc
		     (dotimes (i (length (cdr localassoc)))
		       (setq inter (aref (cdr localassoc) i))
		       (unless (eq :start
				   (get-local-value inter :current-state))
			 (kr-send inter :do-abort inter t nil))))))
	       (opal:update-all))))
	(t (error "change active on object not interactor or priority level"))))


;; causes the interactor to start running (go from :start state to
;; :running state) without waiting for the start event.  this does nothing
;; if the interactor is already running or if it is not active.
;;
;; if an event is passed in, then this is used as the x and y location to
;; start with.  if the event is t (the default), then the last event that
;; was processed is re-used.  events are defined in i-windows.lisp and not
;; exported.  only the x and y fields of the event are really needed.  if
;; the other fields are there also, then the event is also used to calculate
;; the stop event (needed if the start-event is
;; a list).  if the position of the event is not inside the object, then the
;; start object for the interactor will be nil, which might be a problem
;; (especially for button-interactors, for example)
;;
;; note: if the interactor being started should not start by itself,
;; its start-where can be set to nil.
;;
(defun start-interactor (an-interactor &optional (event t))
  "causes an interactor to start running without waiting for the start event.
event can be t to use the previous event."
  (internal-start-interactor an-interactor event)
  (opal:update-all))

(defun internal-start-interactor (an-interactor event)
  (check-and-handle-changed-inters)
  (if-debug an-interactor
	    (format t "~% starting ~s with event ~s~%" an-interactor event))
  (when (and (g-value an-interactor :active)
	     (eq :start (g-value an-interactor :current-state)))
    ;; first fix the generated stop-event of the interactor
    (when (eq event t)
      (setq event *current-event*))	; this is the last event
					; processed
    (unless (and event (check-event event :start-event an-interactor))
      ;; have to generate stop event here
      (set-invented-stop-event an-interactor))
    (let ((obj (when event (check-location event :start-where an-interactor))))
      ;; obj will sometimes be nil, hope that's ok
      ;; first set the special slots of the interactor
      (s-value an-interactor :current-window (when event (event-window event)))
      (s-value an-interactor :first-obj-over obj)
      (s-value an-interactor :start-char (when event (event-char event)))
      ;; now start
      (kr-send an-interactor :do-start an-interactor obj event))))

;; this procedure aborts the interactor if it is running.
(defun abort-interactor (inter)
  "call this to abort the interactor if it is running."
  (internal-abort-interactor inter)
  (opal:update-all))

(defun internal-abort-interactor (inter)
  (check-and-handle-changed-inters)
  (if-debug inter
	    (format t "aborting interactor ~s explicitly~%" inter))
  (unless (eq :start (get-local-value inter :current-state))
    (kr-send inter :do-abort inter nil nil)))

;; this procedure stops the interactor if it is running.  this is like
;; hitting the stop event, except that the previous value for the
;; interactor is used.  there are special messages in each interactor
;; type to allow stopping explicitly, since each one needs to do
;; something different to re-use the last value.
(defun stop-interactor (inter)
  "call this to stop the interactor if it is running."
  (check-and-handle-changed-inters)
  (if-debug inter
	    (format t "stopping interactor ~s explicitly~%" inter))
  (case (get-local-value inter :current-state)
    ((nil) nil)				; ignore this object if state is nil
    (:start nil)			; if not running, just leave alone
    (:outside (if (eq :last (g-value inter :outside))
		  (kr-send inter :do-explicit-stop inter)
		  (kr-send inter :do-abort inter nil nil)))
    ((:running :animating) (kr-send inter :do-explicit-stop inter))
    (t (error "**illegal state for ~s in stop-interactor" inter)))
  (opal:update-all))


(defun top-interactor-initialize (self)
  "top level initialize routine"
  (declare (ignore self))
  (error "** cannot create an instance of a top-level interactor"))


;;; utility procedures handling the priority levels for interactors

;; design: there is a global list of priority levels.  each window has
;; a local list of the levels used in that window stored as an
;; association list of (global-level . inter-array), where global-level is
;; a priority-level schema and inter-array is an array of interactors at that
;; level for this window.  each level in the window has an array of interactors.
;; the global list does not contain any interactors.  when an interactor is
;; added to a window (by being created with a window slot or having its
;; window slot changed), the interactor is added to the right priority
;; level lists in the window(s) it is affiliated with.  if that window
;; does not have the named priority level, then it is added at the right
;; place (so the priority order is maintained).

;; it is not legal to destroy or modify the priority-level-list except to
;; add new priorities (anywhere) or to change :active of a level.  no
;; removing or changing order of priorities.

(defun add-interactor-to-level-win (inter level win multiwin)
  (let ((inter-array (cdr (assoc level (g-value win :priority-level-assoc)))))
    ;; first, deal with multiwin
    (when (and multiwin
	       (g-value inter :continuous))
      ;; then should wait with a special kind of grab
      (change-window-multi-grab win t))
    ;; now add interactor to the window's list
    (unless inter-array
      (setq inter-array (create-level-for-win level win)))
    (if (position inter inter-array)
	(progn
	  #+garnet-debug		; only do this printing when debugging is enabled
	  (format t "warning, pushing interactor ~s on level ~s for win ~s
but already there~%" inter level win)
	  )
	;; else add it. 10 is the amount to extend array if necessary
	(vector-push-extend inter inter-array 10))))


(defun add-interactor-to-level (inter level just-moving)
  (if-debug :priority-level
	    (format t "adding ~s to priority level ~s~%" inter level))
  #+garnet-debug			; only do this when debugging
  ;; *test to see if interactor used more than once
  (let ((curlevel (g-value inter :current-priority-level)))
    (unless (null curlevel)
      (error "**adding inter ~s to > 1 priority level. old: ~s new ~s~%"
	     inter curlevel level)))
  (s-value inter :current-priority-level level)
  (let ((wins (get-interactor-windows inter)))
    (if (listp wins)
	(dolist (win wins)
	  (add-interactor-to-level-win inter level win
				       ;; don't need to deal with
				       ;; multi-win when just-moving
				       (if just-moving nil t)))
	(add-interactor-to-level-win inter level wins nil))))


(defun remove-interactor-from-level-win (inter level win multiwin)
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
	      ;; can have no pos when destroying window just
	      ;; after changing the window slot since destroy can't
	      ;; call (check-and-handle-changed-inters) since this
	      ;; causes an asynchronous window error.
	      (when pos
		(setf (aref inter-array pos) temp)))
	    (when multiwin ;; then see if any other multi-win inters
	      (check-if-ungrab-win win)))
	  ;; else write a warning message
	  #+garnet-debug		; only do this printing when debugging is enabled
	  (format
	   t "warning, removing inter ~s from window ~s not on level ~s~%"
	   inter win level)))))


(defun remove-interactor-from-level (inter level just-moving)
  (if-debug :priority-level
	    (format t "removing ~s from priority level ~s~%" inter level))
  (s-value inter :current-priority-level nil)
  (let ((wins (get-interactor-windows inter)))
    (if (listp wins)
	(dolist (win wins)
	  (remove-interactor-from-level-win inter level win
					    ;; if just-moving, then
					    ;; don't have to deal with
					    ;; multi-win
					    (if just-moving nil t)))
	(remove-interactor-from-level-win inter level wins nil))))


(defun careful-remove-interactor-from-level (inter level old-window)
  (if-debug :priority-level
	    (format t "careful removing ~s from priority level ~s~%" inter level))
  (s-value inter :current-priority-level nil)
  (when (and level old-window)
    (if (listp old-window)
	(dolist (win old-window)
	  (remove-interactor-from-level-win inter level win t))
	(remove-interactor-from-level-win inter level old-window nil))))


(defun get-interactor-windows (inter)
  (let ((interwin (g-value inter :window)))
    (cond ((schema-p interwin) interwin)
	  ((null interwin) nil)
	  ((eq interwin t) all-inter-windows)
	  ((listp interwin) interwin)
	  (t (error "bad window in inter ~s" inter)))))

;; search through global level list and put the local-level at the
;; right place in win's list.  returns new inter-array
(defun create-level-for-win (glo-level win)
  (let* ((new-inter-array (make-array 5 :fill-pointer 0 :adjustable t))
	 (glo-list priority-level-list)
	 (local-list (g-value win :priority-level-assoc))
	 (prev-local nil)
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

;; check if any other inters use multiwin, if not call change-window-multi-grab
;;
(defun check-if-ungrab-win (win)
  (let (inter-array inter interwin)
    (dolist (levelassoc (g-value win :priority-level-assoc))
      (when (g-value (car levelassoc) :active)
	(setq inter-array (cdr levelassoc))
	(dotimes (i (length inter-array))
	  (setq inter (aref inter-array i))
	  (setq interwin (g-value inter :window))
	  (when (and (or (eq interwin t)
			 (listp interwin))
		     (g-value inter :continuous))
	    ;; then found one, so leave window being continuous
	    (return-from check-if-ungrab-win))))))
  ;; if get here, then no multi-window interactors
  (change-window-multi-grab win nil))

(defun print-inter-levels (&key level window)
  "prints out the interactor priority levels (for debugging).  if :level,
 then only that level, if :window then only that window"
  (let ((*print-pretty* nil)
	(winlist (if window (list window)
		     all-inter-windows))
	foundone)
    (dolist (level (if level (list level)
		       priority-level-list))
      (when foundone
	(setq foundone nil)
	(format t "-------------~%"))
      (format t "level ~s " level)
      (dolist (win winlist)
	(if (schema-p win)
	    (let ((localassoc (assoc level (g-value win
						    :priority-level-assoc))))
	      (when localassoc
		(unless foundone
		  (setq foundone t)
		  (terpri))
		(format t "  window ~s~%" win)
		(format t "      ~s~%" (cdr localassoc))))
	    ;; else invalid window
	    (progn
	      (format t "** found invalid window ~s.  removing.~%" win)
	      (setq all-inter-windows (delete win all-inter-windows)))))
      (unless foundone
	(format t "------ empty -----~%")))))

(defun do-all-interactors (func &optional (root-inter interactor))
  "calls func on each interactor that is-a root-inter (recursively).
   does not call func on root-inter itself."
  (dolist (int (g-value root-inter :is-a-inv))
    (funcall func int)
    (do-all-interactors func int)))

;; useful for debugging, gets rid of all interactors.  doesn't
;; destroy them, but simply removes them from the levels.
(defun reset-inter-levels (&optional level)
  "removes all interactors from a level, or all levels if none supplied
 (for debugging)"
  (setf *changed-interactors* nil)	; no interactors to fix either
  (reset-all-timer-processes)
  (setf *special-grab-up-inter* nil)
  (dolist (win all-inter-windows)
    (if level
	(let ((localassoc (assoc level (g-value win :priority-level-assoc))))
	  (when localassoc
	    (setf (cdr localassoc) nil)))
	;; else do all levels
	(s-value win :priority-level-assoc nil))))


;; checks to see if the actor should be destroyed when the window is.  this
;; has a side-effect of removing the window from the interactor's window list,
;; if it is in there.
(defun check-actor-delete-window (win actor)
  (let ((interwin (g-value actor :window)))
    (cond ((schema-p interwin) (eq interwin win)) ; if just one, return eq'ness
	  ((null interwin))		; don't delete interactor if its window slot is nil
	  ((listp interwin)		; if list, return if no more windows for this inter
	   (null (with-demon-disabled #'inter-update-slot-invalidated
		   ;; don't need to push this interactor onto the
		   ;; changed list since being handled
		   (deleteplace win (g-value actor :window)))))
	  (t nil))))			; otherwise, don't delete it

(defparameter *copy-array* (make-array 50 :adjustable t :fill-pointer t))

;; destroys all the interactors on the window
;; be careful not to destroy an interactor if on this window and other
;; windows also.
(defun destroy-all-interactors (window)
  (if-debug :window (format t "destroy all interactors for win ~s~%" window))
  ;; copy the list since destroy will modify the list
  (dolist (inter (copy-list (g-value window :all-interactors)))
    (when (and (schema-p inter) (check-actor-delete-window window inter))
      (opal:destroy inter nil))))

;; tells x to start reporting move events for the window of the
;; interactor.  if the interactor has multiple windows, then turns on move
;; events in all of them, and ungrabs the mouse.
(defun turn-on-mouse-moved (actor)
  (if-debug actor (format t "turning on mouse-moved for ~s, win=~s~%"
			  actor (g-value actor :window)))
  (let ((win (g-value actor :window)))
    (when win
      (cond ((schema-p win)
	     (pushnew actor (g-value win :current-want-moved-interactors))
	     (gem:set-interest-in-moved win t))	; ok to set if already interested
	    ((eq win t)				; do all windows
	     (dolist (w all-inter-windows)
	       (pushnew actor (g-value w :current-want-moved-interactors))
	       (gem:set-interest-in-moved w t))
	     (unless (eq (g-value actor :start-event) t)
	       (setf *special-grab-up-inter* actor))
	     )
	    ((listp win)		; then do each one
	     (dolist (w win)
	       (pushnew actor (g-value w :current-want-moved-interactors))
	       (gem:set-interest-in-moved w t))
             (unless (eq (g-value actor :start-event) t)
	       (setf *special-grab-up-inter* actor))
	     )
	    (t (error "window slot of inter ~s has wrong form" actor))))))

;; checks to see if should turn off mouse moved events from the window(s)
;; because the specified interactor is no longer running, and if so, does it
(defun turn-off-mouse-moved (actor)
  (if-debug actor (format t "turn off mouse moved for ~s~%" actor))
  (setf *special-grab-up-inter* nil)
  (let ((win (g-value actor :window)))
    (when win
      (cond ((schema-p win)
	     (when (null
		    (deleteplace actor
				 (g-value win :current-want-moved-interactors)))
	       (gem:set-interest-in-moved win nil))) ; turn it off if no more
	    ((eq win t)				     ; do all windows
	     (dolist (w all-inter-windows)
	       (when (null
		      (deleteplace actor
				   (g-value w :current-want-moved-interactors)))
		 (gem:set-interest-in-moved w nil))))
	    ((listp win)		; then do each one
	     (dolist (w win)
	       (when (null
		      (deleteplace actor
				   (g-value w :current-want-moved-interactors)))
		 (gem:set-interest-in-moved w nil))))
	    (t (error "window slot of inter ~s has wrong form" actor))))))

;; adds the schema to the correct level and turns on mouse moved events,
;; if necessary
(defun add-to-level (an-interactor running-p need-mouse-moved just-moving)
  (add-interactor-to-level an-interactor (if running-p
					     (g-value an-interactor :running-priority)
					     (g-value an-interactor :waiting-priority))
			   just-moving)
  (when need-mouse-moved (turn-on-mouse-moved an-interactor)))

;;removes from running or non-running level
(defun remove-from-level (an-interactor running-p just-moving)
  (remove-interactor-from-level an-interactor (if running-p
						  (g-value an-interactor :running-priority)
						  (g-value an-interactor :waiting-priority))
				just-moving)
  (when (and running-p
	     ;; check to see if there is no start event, in which case,
	     ;; should always be running, so don't turn off.
	     (or (not (eq (g-value an-interactor :start-event) t))
		 (not (g-value an-interactor :active))))
    (turn-off-mouse-moved an-interactor)))

(defun move-levels (an-interactor from-running-p to-running-p needmousemoved)
  (remove-from-level an-interactor from-running-p t)
  (add-to-level an-interactor to-running-p needmousemoved t))

(defun remove-from-all-levels (an-interactor)
  (if-debug :priority-level (format t "removing ~s from all levels~%"
 				    an-interactor))
  (let ((level (g-value an-interactor  :current-priority-level)))
    (when level (remove-interactor-from-level an-interactor level nil))))


;;; utility procedures for the various create procedures

;; returns t if schema :is-a typ, otherwise raises an error
(defun check-interactor-type (schema typ)
  (if (is-a-p schema typ)
      t
      (error "** wrong type: is a ~s, not a ~s" (g-value schema :is-a) typ)))

(defun check-required-slots (schema)
  ;; dzg - nov. 12, 1991 - fix a problem with kr 1.5.2
  ;; bam - seems to still be needed 7-22-93
  (let ((formula (get-value schema :window)))
    (if (formula-p formula)
	(kr::set-cache-is-valid formula nil))))

;; maintains a list of all the interactors associated with a window
;; which can be used when the window is deleted.  it is not
;; sufficient to use the :priority-level-assoc array since that only
;; contains interactors that are :active, and we want to delete the
;; non-active ones also.
(defun handle-inter-list-for-window (old-window new-window inter)
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

;; adds to correct priority level, and starts it up if should.  only
;; call this if :active and :window are non-nil
(defun add-to-level-and-maybe-start (schema)
  (let ((starting (eq :start (g-value schema :current-state))))
    (add-to-level schema (not starting) nil nil)
    (when (and starting (eq (g-value schema :start-event) t))
      (internal-start-interactor schema t))))

;; sets up the default values for slots.  called from all initialize methods
(defun set-up-defaults (schema)
  (when (eq (g-value schema :feedback-obj) t)
    (error "** sorry, creating a default feedback obj niy ****"))
  (s-value schema :current-state :start)
  (let ((win (g-value schema :window)))
    ;; keep a list of all interactors associated with a window, for
    ;; when window is destroyed
    (handle-inter-list-for-window nil win schema)
    (when (and win (g-value schema :active))
      (when (eq win t)
	;; maintain the global list for inters that have (:window t)
	(pushnew schema *inters-with-t-window*))
      (s-value schema :copy-old-window  ; used to tell if window changed
	       (if (listp win) (copy-list win) win))
      (add-to-level-and-maybe-start schema))))

;; go through all the interactors on *inters-with-t-window* and add
;; win to them.  this is called when a new window is created.
(defun handle-new-window-for-t-inters (win)
  (dolist (inter *inters-with-t-window*)
    (let* ((running-p (if (eq (g-value inter :current-state) :start)
			  nil t))
	   (level (if running-p
		      (g-value inter :running-priority)
		      (g-value inter :waiting-priority))))
      (if-debug :priority-level
		(format t "adding ~s to new window ~s priority level ~s~%"
			inter win level))
      (add-interactor-to-level-win inter level win t)
      ;; add inter to list of all the interactors for this window
      (pushnew inter (g-value win :all-interactors))
      (when running-p
	(pushnew inter (g-value win :current-want-moved-interactors))
	(gem:set-interest-in-moved win t)))))

;; pulls the aggregate object out of the :where field specified
(defun get-gob-of-where (where)
  (cond ((null where) nil)
	((eq where t) t)
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
	   (otherwise (error "** unknown keyword in where"))))
	(t (error "** bad where, should be t, nil, or a list"))))


;;; noticing slots changing
;;

;; this is called whenever the interactor's active or window slots are changed
;; called from opal:update-slot-invalidated which is the kr::*invalidate-demon*
;;
(defun inter-update-slot-invalidated (inter slot save)
  (declare (ignore save slot))
  (pushnew inter *changed-interactors*))


(defun handle-all-changed-interactors ()
  ;; use temporary pointer, so more robust in case this procedure
  ;; crashes, *changed-interactors* list will be nil
  (let ((l *changed-interactors*))
    (setq *changed-interactors* nil)
    (dolist (inter l)
      (when (schema-p inter)		; want to see if destroyed or not.
	(handle-interactor-slot-changed inter)))))


(defun handle-interactor-slot-changed (inter)
  (let ((curlevel (g-value inter :current-priority-level))
	(old-window (g-value inter :copy-old-window))
	(new-window (g-value inter :window))
	(new-active (g-value inter :active)))
    (if-debug inter (format t "automatic fixing of ~s win ~s active ~s~%"
			    inter new-window new-active))
    (s-value inter :copy-old-window	; used to tell if window changed
	     (if (listp new-window) (copy-list new-window) new-window))

    ;; keep a list of all interactors associated with a window, for
    ;; when window is destroyed
    (unless (eq old-window new-window)
      (handle-inter-list-for-window old-window new-window inter))

    (if (and new-window new-active)
	;; then should be added to a level
	(progn
	  ;; first, maintain the global list for inters using (:window t)
	  (if (eq new-window t)
	      (pushnew inter *inters-with-t-window*)
					; else remove
	      (when (eq old-window t)
		(deleteplace inter *inters-with-t-window*)))

	  (if curlevel ;; then is on a priority-level of some window now
	      ;; first check to see if correct window and level
	      (unless (and (equal old-window new-window)
			   (eq curlevel	;; find new level
			       (if (eq :start (g-value inter :current-state))
				   (g-value inter :waiting-priority)
				   (g-value inter :running-priority))))
		;; remove from old windows
		(let ((wins (cond ((schema-p old-window) old-window)
				  ((null old-window) nil)
				  ((eq old-window t) all-inter-windows)
				  ((listp old-window) old-window)
				  (t (error "bad old-window ~s in inter ~s"
					    old-window inter)))))
		  (s-value inter :current-priority-level nil)
		  (if (listp wins)
		      (dolist (win wins)
			(remove-interactor-from-level-win inter curlevel
							  win t))
		      (remove-interactor-from-level-win inter curlevel
							wins nil))
		  (add-to-level-and-maybe-start inter)))
	      ;; else didn't used to be on any levels
	      (add-to-level-and-maybe-start inter))) ; add to the right level

	;; else should stop running and be removed
	(when curlevel			; then used to be running
	  (when (eq old-window t)
	    (deleteplace inter *inters-with-t-window*))
	  (unless (eq :start (get-local-value inter :current-state))
	    ;; have to abort this interactor
	    (with-demon-disabled #'inter-update-slot-invalidated
	      (s-value inter :window old-window)
	      (internal-abort-interactor inter) ; abort may not work unless
					; window has old value
	      (unless (eq old-window new-window)
		(s-value inter :window new-window))))
	  (careful-remove-interactor-from-level inter
						;; can't use curlevel variable since
						;; abort might change the level, so
						;; g-value new value
						(g-value inter :current-priority-level)
						old-window)))))


;; this function is used in process event to sort the interactors by
;; their :sort-order slot.  if no :sort-order then put at end of list.
(defun sort-inters (i1 i2)
  (let ((s1 (g-value i1 :sort-order))
	(s2 (g-value i2 :sort-order)))
    (cond ((and s1 s2) (< s1 s2))
	  (s1 t)
	  (s2 nil)
	  (t nil))))

;;; middle level dispatcher for events; called from i-windows

(defvar *global-first-accelerators* nil) ; will be set in accelerators.lisp
(defvar *global-accelerators* nil) ; will be set in accelerators.lisp

(defun process-event (event)
  ;; first, make sure lists are up-to-date
  (check-and-handle-changed-inters)
  (let (win grab-win)
    (when event
      (setq win (event-window event))
      (let ((fn (or (cdr (assoc (event-char event)
				(if win (g-value win :first-accelerators))))
		    (cdr (assoc (event-char event)
				*global-first-accelerators*)))))
	(when fn
	  (if-debug :priority-level
		    (format t "----- event grabbed by accelerator ~s~%" fn))
	  (funcall fn event)
	  (return-from process-event t)))
      (if-debug :priority-level
		(format t "~%------doing events for window ~s~%" win))
      (if *visible-modal-windows*
	  (unless (member win *visible-modal-windows*)
	    (if-debug :priority-level
		      (format t " exiting because not one of the modal windows~%"))
	    (unless (and (event-mousep event) ; don't beep on mouse
					; unless down press
			 (not (event-downp event)))
	      (inter:beep)) ;; is beeping a good idea??
	    (return-from process-event))
	  ;; if no modal windows visible, then use this
	  ;; gross hack in case press window set move to another garnet window.
	  (when
	      (and *special-grab-up-inter*
		   (not (eq (setq grab-win
				  (g-value *special-grab-up-inter* :window))
			    t))
		   (not (member win grab-win)))
	    ;; translate point to be in a window
	    (let ((win2 (first grab-win)))
	      (multiple-value-bind (x2 y2)
		  (opal:convert-coordinates win (event-x event) (event-y event)
					    win2)
		(if-debug :event
			  (format t "~% <*><*> converting to window ~s (~s ~s)~%"
				  win2 x2 y2))
		(setf (event-x event) x2)
		(setf (event-y event) y2)
		(setf (event-window event) win2)
		(setf win win2)))))
      (if (eq (event-char event) :timer)
	  (error "timer event got to process-event")
	  ;; else a regular event.
          ;; note: claimed? tells if the event was claimed by an :if-any
          ;;       interactor or an :always priority-level
	  (let (winlevels found-one claimed? inter-array len
			  sorted-inters inter skip-set ex-value)
	    (setq winlevels (g-value win :priority-level-assoc))
	    (dolist (glo-level priority-level-list)
	      (setq found-one nil)
	      (setq sorted-inters nil)
	      (setq skip-set nil)
	      (if (g-value glo-level :active)
		  (progn
		    ;; in window's assoclist
		    (when (eq glo-level (caar winlevels))
			(progn
			  (if-debug :priority-level
				    (format t "~%------doing priority level ~s~%"
					    glo-level))
			  (setq inter-array (cdar winlevels))
			  (setq winlevels (cdr winlevels))
			  (setq len (length inter-array))
			  ;; first make a copy of the level since
			  ;; might be modified
			  (when (> len (array-dimension *copy-array* 0))
			    (setf *copy-array* (adjust-array *copy-array* len)))
			  (dotimes (i len)
			    (setf (aref *copy-array* i)
				  (aref inter-array i)))
			  (when (g-value glo-level :sorted-interactors)
			    (if-debug :priority-level
				      (format t "sorting the interactors~%"))
			    (setq sorted-inters t)
			    ;; fill-pointer is used by sort
			    (setf (fill-pointer *copy-array*) len)
			    (setq *copy-array*
				  (sort *copy-array* #'sort-inters)))
			  ;; now process each interactor
			  (dotimes (i len)
			    (setq inter (aref *copy-array* i))
			    (when (and (schema-p inter)
				       ;; if skip due to exclusivity
				       (or (not sorted-inters)
					   (not (setq ex-value
						      (g-value inter :exclusivity-value)))
					   (not (member ex-value skip-set)))
				       (general-go inter event))
			      (setf found-one t)
			      (when (and sorted-inters
					 ex-value)
				;; then set up skip set
				(push ex-value skip-set)
				(if-debug :priority-level
					  (format t "**pushing ~s into skip set~%"
						  ex-value)))))))
		    ;; since active, check whether stop or not
		    (case (g-value glo-level :stop-when)
		      (:if-any (when found-one
				 (setq claimed? t)
				 (return)))
		      (:always (setq claimed? t) (return))
		      ;; nil means always go on to next
		      ((nil) nil)
		      (t (error "bad :stop-when: ~s in priority level ~s"
				(g-value glo-level :stop-when)
				glo-level))))
		  ;; else not active
		  (progn
		    (if-debug :priority-level ;; print that skipped
			      (format t " **skipped because this level is not active~%"))
		    ;; check to see whether go to next level in other
		    ;; list
		    (when (eq glo-level (caar winlevels))
		      (setq winlevels (cdr winlevels))))))
	    ;; now handle accelerators if event is not claimed
	    (unless claimed?
	      (let ((fn (or (when (and win (schema-p win))
			      (cdr (assoc (event-char event)
					  (g-value win :accelerators))))
			    (cdr (assoc (event-char event)
					*global-accelerators*)))))
		(when fn
		  (if-debug :priority-level
			    (format t "event grabbed by accelerator ~s~%" fn))
		  (funcall fn event)
		  (setq claimed? t))))
	    ;; now process-event returns t iff event was claimed
	    claimed? )))))

;; these all do the same thing, information is already coded into the event.
(declaim (inline button-pressed))
(defun button-pressed (event)
  (process-event event))
(declaim (inline button-released))
(defun button-released (event)
  (process-event event))
(declaim (inline key-pressed))
(defun key-pressed (event)
  (process-event event))
(declaim (inline mouse-moved))
(defun mouse-moved (event)
  (process-event event))

;;; utility procedures for the go procedures
;;

(defun getnextitem (item sequence)
  "return the item after the parameter in the sequence or nil if not there"
  (let ((index (position item sequence)))
    (when index (nth (1+ index) sequence))))

(defun get-running-where (an-interactor)
  "running where can either be supplied or generated.
if generated, it might be from start-where or if running-where
was (:xx *). see fix-running-where for full details."
  (or (get-local-value an-interactor :generated-running-where)
      (g-value an-interactor :running-where)))


(defun checkobjtype (obj type)
  "check to see if obj is a type.  type can be a list, in which case
checks to see if obj is any of the types in the list."
  (cond ((eq type t))
	((listp type) (dolist (ty type)
			(when (is-a-p obj ty)
			  (return-from checkobjtype t))) nil)
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
	    nil))))

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
	    nil))))

(defun check-leaf-but-return-element-branch (agg an-interactor control type
					     win x y)
  #-garnet-debug (declare (ignore an-interactor))
  (when (opal:point-in-gob agg x y)
    ;; otherwise, return nil always
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
		  (format t " ** success: ~s~%" child))
	(return-from check-leaf-but-return-element-branch child)))
    ;; if get here, then didn't find any children, return nil or :none
    (if (eq control :check-leaf-but-return-element-or-none)
	:none
	nil)))


(defun check-location (event which-where an-interactor)
  "xxx fmg not sure if this matches the code.
checks to see if x,y is inside where, returns the object
under the mouse if passes.  if where is :element-of, this
will be the element object.  if where is :in or :in-box,
then will be the object itself.  returns nil if fails
******** bug ***no way for overlapping objects to hide each other from
******** the mouse!  (have to use priority levels)"
  (let ((where (case which-where
		 (:start-where (g-value an-interactor :start-where))
		 (:running-where (get-running-where an-interactor))
		 (t (error "bad which-where"))))
	result)
    (if-debug an-interactor
	      (format t "checking ~s = " which-where))
    (setq result
	  (cond ((eq where t))		; t means anywhere in the window
		((null where))		; nil as where means failure, useful to have
					; interactor not run (e.g, start-where is a formula)
		((listp where)
		 (let ((x (event-x event))
		       (y (event-y event))
		       (win (event-window event))
		       (control (first where))
		       (agg (second where))
		       (type (or (getnextitem :type where) t)) ; t as type => everything
		       objwin slot)
		   (unless (schema-p agg)
		     (error-print (concatenate 'string "object in :"
					       (symbol-name which-where))
				  agg "opal object" an-interactor))
		   (setf objwin (and agg (g-value agg :window)))
		   (if (or (eq control :list-element-of)
			   (eq control :list-leaf-element-of)
			   (eq control :list-check-leaf-but-return-element)
			   (eq control :list-element-of-or-none)
			   (eq control :list-leaf-element-of-or-none)
			   (eq control :list-check-leaf-but-return-element-or-none)
			   (eq control :custom))
		       ;; if a list, then objects might be in different
		       ;; windows, so do test inside the case statement.
		       ;; if custom, let the user's procedure do the test.
		       (setf slot (third where))
		       ;; else check if window of object eq window of event
		       (unless (eq win objwin) ; otherwise test here
			 (if-debug an-interactor
				   (format t " **windows don't match** ev-win=~s obj-win=~s~%"
					   win objwin))
			 (return-from check-location nil)))
		   (if-debug an-interactor (format t "~s of ~s" control agg)
			     (when slot (format t " slot ~s" slot)))
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
					; else return nil if not in agg
			  nil))
		     (:in-box
		      ;; the top level point-in-gob method uses the bounding
		      ;; rectangle, whereas the specific object may have a special
		      ;; procedure
		      (and (kr-send opal:view-object :point-in-gob agg x y)
			   agg))	;return agg if in box
		     (:in
		      (and (opal:point-in-gob agg x y)
			   agg))	;return agg if in
;;;                      **** niy **********************
;;;                      (:full-object-in ;;entire object to move ;
;;;                      (let ((obj (g-value an-interactor :obj-being-changed)))
;;;                      (unless obj
;;;                      (error "no object being changed for :full-object-in"))
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
		      ;; else return nil if not in agg
		      nil))
		 (t (error "** illegal where control: ~s" where)))))
	  (t (error-print (symbol-name which-where) where "t, nil or list"
			  an-interactor))))
    (if-debug an-interactor
	      (if result (format t " **success=~s~%" result)
		  (format t " **fail**~%")))
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
the desired event event-descriptor.  if so, returns matching stop-event
or nil.

no checking for illegal keywords, they just always fail.

converting and checking are combined, because they both have to go through
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
		 (t key-button)))	; otherwise, just use value
	  ((and (eq event-desired :any-keyboard)
		(not mousep)
		(not (eq key-button :timer))) ; timer doesn't
	   ;; match keyboard
	   #\return)

	  ((or (and (eq event-desired :any-mouseup) mousep (null downp)
		    code)		; code is nil for window-exit and -enter
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



;; this procedure generates a stop event for an interactor based on its
;; :start-event.  this does not use an actual event from x, so it just picks
;; a plausible stop event.  this is called from start-interactor which starts
;; an interactor without a real event happening
(defun set-invented-stop-event (an-interactor)
  (unless (g-value an-interactor :stop-event) ; don't bother if there is one
    (s-value an-interactor :generated-stop-event
	     (list :any-mouseup #\return)))) ; this seems safe: either a
				             ; mouse up or a cr

(defun check-event (actual-event which-event interact)
  "looks in the "which-event" field of the "interact" and compares that to
actual-event.  this comparison is fairly tricky because of all the
possible kinds of event descriptions (:any-xxx, lists with exceptions,
etc.)  if this is a start-event, then generates a corresponding stop
event, in case needed, and stores this in the interactor."
  (let ((result (int-check-event actual-event which-event interact)))
    (if-debug interact
	      (if result (format t " **success=~s~%" result)
		  (format t " **fail~%")))
    result))

(defun int-check-event (actual-event which-event interact)
  (if-debug interact (format t "checking ~s " which-event))
  (let ((stop-event nil)
	(events-desired (g-value interact which-event)))
    ;; check if actual-event is :mouse-moved
    (when (eq (event-char actual-event) :mouse-moved)
      (return-from int-check-event
	(if (eq which-event :start-event) ; then see if t
	    (if (eq events-desired t) :maybe nil)
	    ;; else if not start-event, always return nil for moved
	    nil)))
    ;; actual-event is not mouse-moved
    (when (and (null events-desired) (eq :stop-event which-event))
      ;; when null, use the generated default stop event, if any
      (setq events-desired (g-value interact :generated-stop-event)))
    (if-debug interact (format t " against wanted ~s" events-desired))
    (cond ((eq events-desired t)
	   ;; for :abort-event or :stop-event, t means don't stop
	   (if (eq which-event :start-event)
	       (return-from int-check-event :maybe)
	       (return-from int-check-event nil)))
	  ((null events-desired) (return-from int-check-event nil))
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
    (when stop-event			; then should return t, otherwise nil
      (when (eq which-event :start-event)
	;; set default stop-event in case needed
	(s-value interact :generated-stop-event stop-event))
      t)))

(defun fix-running-where (an-interactor new-obj-over)
  "if running-where is empty, then copies :start-where.
otherwise, checks to see if running-where is of the form '(:xx *),
then changes running-where to be '(:xx new-obj-over).
this is called from every interactor's start procedure
if it is continuous."
  (let ((r-w (g-value an-interactor :running-where))
	r-w-copy)
    (if (null r-w) ; then copy start-where
	(s-value an-interactor :generated-running-where
		 (g-value an-interactor :start-where))
	;; start-where cannot use the '* form, so don't have to worry about
	;; that if copying the start-where into running where.
	;; otherwise, check if need to have a special, edited start-where
	(if (and (listp r-w) (eq '* (second r-w)))
	    ;; then need to substitute new-obj-over for *
	    (progn
	      (unless (and (setq r-w-copy
				 (get-local-value an-interactor
						  :generated-running-where))
			   (not (eq r-w-copy t))
			   (not (eq (car r-w-copy)(car r-w))))
		;; make a copy of the running-where, in case it is inherited,
		;; since are going to destructively modify it
		;; bug: if running-where changes to have a new :type or
		;; something, this will fail to notice it.
		(setq r-w-copy (copy-list r-w)))
	      (if (eq new-obj-over t)
		  (s-value an-interactor :generated-running-where t) ; then just use t
		  (progn					     ; else use the copy
		    (setf (second r-w-copy) new-obj-over)
		    (s-value an-interactor :generated-running-where r-w-copy))))
	  ;; otherwise, remove generated-running-where.
	  ;; destroy-slot checks whether slot is there or not.
	  (destroy-slot an-interactor :generated-running-where)))))

(defun gotorunningstate (an-interactor needmousemoved)
  (s-value an-interactor :current-state :running)
  (move-levels an-interactor nil t needmousemoved))

(defun gotostartstate (an-interactor set-waiting-level)
  (s-value an-interactor :current-state :start)
  (when (g-value an-interactor :self-deactivate)
    (s-value an-interactor :active nil))
  (when set-waiting-level
    (move-levels an-interactor t nil nil)))

(defun general-go (an-interactor event)
  "this is the main action procedure that makes the most interactors
   go. this procedure implements the state machine.  it is called by
   the main dispatcher when an event happens. the event is the value
   returned by the display server. this procedure calls the do-xxx
   procedures in the interactor, which are specialized for the
   particular type of interactor. the do-xxx procedures in turn call
   the xxx-action procedures.  these -action procedures my be supplied
   by outside applications.

   the complexity in the state machine implementation is that the same
   event may cause two things, e.g., both going outside and stop. we
   cannot count on getting different events for this."
  (let ((state (get-local-value an-interactor :current-state))
	(active (g-value an-interactor :active))
	(window (g-value an-interactor :window))
	(event-window (event-window event))
	return-val)
    (if-debug an-interactor
	      (format t "~%enter go for ~s, state=~s...~%"
		      an-interactor state))
    (unless state
      (if-debug an-interactor
		(format t "returning because state=nil~%"))
      ;; quick return if state is nil (which means that the inter has
      ;; been destroyed)
      (return-from general-go nil))
    ;; now, must have both :active and :window as non-nil
    (unless (and active window)
      (unless (eq :start state)
	(if-debug an-interactor (format t "** implicit become inactive~%"))
	(kr-send an-interactor :do-abort an-interactor t event)
	(opal:update-all))
      (if-debug an-interactor
		(format t "returning because ~a in inter is nil~%"
			(if active ":window" ":active")))
      ;; return nil
      (return-from general-go nil))
    (cond ((schema-p window)
	   (when (not (eq window event-window))
	     (if-debug an-interactor
		       (format t "returning because event window ~s doesn't ~
                                  match inter windows ~s~%" event-window window))
	     (return-from general-go nil)))
	  ;; then just go on
	  ((eq window t))
	  ;; then check if member
	  ((listp window)
	   (unless (member event-window window)
	     (if-debug an-interactor
		       (format t "returning because window ~s isn't in list ~s~%"
			       event-window window))
	     (return-from general-go nil)))
	  (t (error "window slot of inter ~s has wrong form" an-interactor)))
    ;; finished preliminary tests, now get to work
    (let (obj should-stop)
      (case state
	;; just ignore animators
	(:animating nil)
	(:start (s-value an-interactor :current-window event-window)
		(if (and (check-event event :start-event an-interactor)
			 ;; return of :maybe ok here
			 (setf obj
			       (check-location event :start-where an-interactor)))
		    (progn
		      (if-debug :short
				(format t "starting ~s~%" an-interactor))
		      (check-and-handle-debug-next-inter an-interactor)
		      ;; these next two slots might be used in formulas
		      (s-value an-interactor :first-obj-over obj)
		      (s-value an-interactor :current-obj-over obj)
		      (s-value an-interactor :start-char (event-char event))
		      (setq return-val (kr-send an-interactor :do-start
						an-interactor obj event)))
		    ;; else exit and return nil
		    (return-from general-go nil)))
	(:running (check-and-handle-debug-next-inter an-interactor)
		  (s-value an-interactor :current-window event-window)
		  (if (check-event event :abort-event an-interactor)
		      (progn
			(if-debug :short
				  (format t "aborting ~s~%" an-interactor))
			(setq return-val (kr-send an-interactor :do-abort
						  an-interactor nil event)))
		      (progn ;; else
			(setf obj
			      (check-location event :running-where an-interactor))
			(setf should-stop
			      (check-event event :stop-event an-interactor))
			(if (null obj)	;went outside
			    (progn
			      (if-debug :short
					(format t "outside for ~s~%" an-interactor))
			      (setq return-val (kr-send an-interactor
							:do-outside an-interactor))
			      (when (eq should-stop t)
				(setq return-val (kr-send an-interactor
							  :do-outside-stop
							  an-interactor event))))
					;here still inside; obj is ok
			    (progn
			      (s-value an-interactor :current-obj-over obj)
			      (if (eq should-stop t)
				  (progn
				    (if-debug :short
					      (format t "stopping ~s~%" an-interactor))
				    (setq return-val (kr-send an-interactor
							      :do-stop an-interactor obj event)))
				  (progn
				    (if-debug :short
					      (format t "running ~s~%" an-interactor))
				    (setq return-val
					  (kr-send an-interactor
						   :do-running an-interactor obj event)))))))))
	(:outside (check-and-handle-debug-next-inter an-interactor)
		  (s-value an-interactor :current-window event-window)
		  (if (check-event event :abort-event an-interactor)
		      (progn
			(if-debug :short
				  (format t "aborting ~s~%" an-interactor))
			(setq return-val (kr-send an-interactor :do-abort
						  an-interactor nil event)))
		      ;; else don't abort
		      (progn
			(setf obj
			      (check-location event :running-where an-interactor))
			(setf should-stop
			      (check-event event :stop-event an-interactor))
			(if-debug :short
				  (format t "outside for ~s~%" an-interactor))
			;; still outside
			(if (null obj)
			    (when should-stop
			      (setq return-val
				    (kr-send an-interactor
					     :do-outside-stop an-interactor event)))
			    ;; else go back inside; obj is ok
			    (progn
			      (s-value an-interactor :current-obj-over obj)
			      (setq return-val (kr-send an-interactor
							:do-back-inside an-interactor
							obj event))
			      (when should-stop
				(setq return-val
				      (kr-send an-interactor
					       :do-stop an-interactor obj event))))))))
	(otherwise (error "** illegal state ~s" state))))
    (opal:update-all)			; update all windows
    (not (eq return-val :stop))))

;; exported procedures
;; main, top level inter:interactor object
(create-schema 'interactor
    :declare ((:type (inter-window-type :window)
		     ((is-a-p inter::priority-level)
		      :waiting-priority :running-priority)))
    (:current-state :start)
    (:self-deactivate nil)
    (:window nil)
    (:active t)
    (:continuous t)
    (:final-function nil)
    (:waiting-priority normal-priority-level)
    (:running-priority running-priority-level)
    (:start-where :not-supplied)
    (:start-event :leftdown)
    (:stop-event nil)
    (:abort-event :control-\g)
    (:running-where nil)
    (:start-action :not-supplied)
    (:running-action :not-supplied)
    (:stop-action :not-supplied)
    (:abort-action :not-supplied)
    (:outside-action :not-supplied)
    (:back-inside-action :not-supplied)
    (:feedback-obj nil)
    (:current-priority-level nil)	; internal slot
    (:local-only-slots '(:current-state nil)
		       '(:operates-on nil)
		       '(:current-priority-level nil))
    (:update-slots '(:active :window	; for kr
		     :waiting-priority :running-priority))
    (:invalidate-demon #'inter-update-slot-invalidated)
    (:do-start :not-supplied)		; these are
    (:do-running :not-supplied)		;   called by go
    (:do-stop :not-supplied)		;   to do
    (:do-abort :not-supplied)		;   the real work.
    (:do-explicit-stop :not-supplied)
    (:do-outside :not-supplied)		;   they call the
    (:do-back-inside :not-supplied)	;   appropriate
    (:do-outside-stop :not-supplied)	;   -action procedures
    (:initialize 'top-interactor-initialize)) ;proc to call when created


;; if erase is t, then aborts the interactor.  this may not be necessary,
;; for example if the window is about to be destroyed.
;; it is more robust to have erase nil.
(define-method :destroy-me interactor (an-interactor &optional (erase t))
  (if-debug an-interactor
	    (format t "interactor destroying ~s erase=~s~%" an-interactor erase))
  (when (and erase (g-value an-interactor :active))
    (change-active an-interactor nil))

  ;; if on changed list, remove it, should really
  ;; (check-and-handle-changed-inters)  but this causes an asynchronous
  ;; window-error, so check in various places that the inter is still valid
  (deleteplace an-interactor *changed-interactors*)

  ;; if on t-list, remove it; does nothing if not there
  (deleteplace an-interactor *inters-with-t-window*)
  ;; if special, remove it
  (when (eq an-interactor *special-grab-up-inter*)
    (setq *special-grab-up-inter* nil))
  ;; update list of all interactors associated with a window, for
  ;; when window is destroyed
  (handle-inter-list-for-window (g-value an-interactor :copy-old-window)
				nil an-interactor)
  (remove-from-all-levels an-interactor)

  (let ((in-obj (g-local-value an-interactor :operates-on)))
    (when (and (schema-p in-obj)
	       (not (kr::slot-constant-p in-obj :operates-on)))
      (if-debug an-interactor
		(format t "removing me from object ~s~%" in-obj))
      (kr-send in-obj :remove-local-interactor in-obj an-interactor)))
  (destroy-schema an-interactor))

(define-method :destroy interactor (an-interactor &optional (erase t))
  (dolist (instance (copy-list (get-local-value an-interactor :is-a-inv)))
     (kr-send instance :destroy instance erase))
  (kr-send an-interactor :destroy-me an-interactor erase))



;; ============================================================
;; future: make move-grow, etc not eat keyboard events, and keyboard
;; interactors not eat mouse events.
;; ============================================================
