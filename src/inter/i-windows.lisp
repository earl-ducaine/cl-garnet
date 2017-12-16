;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$


(in-package :interactors)

;; Hitting the key *garnet-break-key* will cause an exit from the
;; main-event-loop and exit from replaying a transcript.

;;  Functions to deal with transcripts

(defparameter *trans-from-file* NIL) ; file to read from
(defparameter *trans-to-file* NIL) ; file to save to
(defparameter *transcript-window-list* NIL) ; list of windows
(defparameter *trans-to-file-motion* NIL) ; save motion events?
(defparameter *trans-time* 0)
(defparameter *trans-wait-elapsed-time* NIL) ; wait on read
(defparameter *transcript-verbose* T) ; use formats and warp-pointers?

(defparameter *util_month-list*
  '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun Time-To-String ()
  (multiple-value-bind
      (second minute hour date month year day-of-week savingsp time-zone)
      (get-decoded-time)
    (declare (ignore second time-zone day-of-week))
    (declare (fixnum hour date month year))
    (if (>= hour 12) (progn (setq savingsp " PM")
		       (when (> hour 12)(incf hour -12)))
	(setq savingsp " AM"))
    (concatenate 'string
               (nth month *util_month-list*) " "
               (princ-to-string date) ", " (princ-to-string year)
                      ", "
               (princ-to-string hour)
               ":"
               (if (< minute 10) "0" "")
               (princ-to-string minute) savingsp)))

(defun Transcript-Events-To-File (filename window-list &key (motion T)
					   (if-exists :supersede))
  (when *trans-to-file* (error "Already transcripting to ~s" *trans-to-file*))
  (when *trans-from-file* (error "Can't send to a file when events from a file: ~s"
				 *trans-from-file*))
  (setf *trans-to-file* (open filename :direction :output :if-exists if-exists))
  (setf *trans-to-file-motion* motion)
  (unless (listp window-list)
    (setq window-list (list window-list)))
  (setq window-list (Add-All-Subwindows window-list))
  (setf *transcript-window-list* window-list)
  (unless (eq if-exists :append)
    (Format *trans-to-file*
	    "Transcript of Garnet session from ~a.  Garnet Version = ~a~%"
	    (Time-To-String) Common-Lisp-User::Garnet-Version-Number)
    (format *trans-to-file*
	    "Form for events: CHAR CODE MOUSEP DOWNP X Y TIME WIN-INDEX~%")
    (Format *trans-to-file* "Windows are:~%")
    (format *trans-to-file* "(~{\"~s\" ~})~%" window-list))
  (setq *trans-time* (get-internal-real-time))
  *trans-to-file*)

(defun Close-Transcript ()
  ;; make a local copy first, in case close fails, will still be reset.
  (let ((old-to *trans-to-file*)(old-from *trans-from-file*))
    (setf *trans-to-file* NIL)
    (setf *trans-from-file* NIL)
    (when old-to
      (close old-to))
    (when old-from
      (close old-from))
    (or old-to old-from))) ; return whichever was closed

(defun Write-Transcript-Event (event)
  (let ((win (if (eq (event-window event) T)
		 0  ;; this will happen for :timer events
		 (position (event-window event) *transcript-window-list*))))
    (when win
      (format *trans-to-file*
	  "> ~s ~s ~s ~s ~s ~s ~s~%"
	  (event-char event) (event-code event) (event-mousep event)
	  (event-downp event) (event-x event) (event-y event)
	  (- (get-internal-real-time) *trans-time*)) ; store time difference
						    ; from start time
      (force-output *trans-to-file*)
      win)))

;; recursively add all the subwindows of the windows in win-list
(defun Add-All-Subwindows (win-list)
  (do* ((lst win-list (cdr lst))
	(item (car lst)(car lst))
	)
       ((null lst))
    (nconc lst (copy-list (g-value item :child))))
  (remove-duplicates win-list :from-end T))

;; Uses *current-event*
(defun Read-Transcript-Event ()
  (let ((val (read-char *trans-from-file* NIL NIL)))
    (when val
      ;; else return NIL
      (unless (eq val #\>)
	(error "Transcript out of sync: first char is ~s, but should be >.
	Execute (inter:close-transcript) to recover." val))
      (setf (event-char *current-event*) (read *trans-from-file*)
	    (event-code *current-event*) (read *trans-from-file*)
	    (event-mousep *current-event*) (read *trans-from-file*)
	    (event-downp *current-event*) (read *trans-from-file*)
	    (event-x *current-event*) (read *trans-from-file*)
	    (event-y *current-event*) (read *trans-from-file*)
	    (event-timestamp *current-event*) (read *trans-from-file*)
	    val (read *trans-from-file*))
      (setf (event-window *current-event*) (nth val *transcript-window-list*))
      *current-event*)))

(defun Transcript-Events-From-File (filename window-list &key
					     (wait-elapsed-time T)
					     (verbose T))
  (when *trans-from-file* (error "Already reading from transcript file ~s"
				 *trans-from-file*))
  (when *trans-to-file* (error "Can't read from a file when events to a file: ~s"
			       *trans-to-file*))
  (setf *transcript-verbose* verbose)
  (setf *trans-from-file* (open filename :direction :input))
  (setf *trans-wait-elapsed-time* wait-elapsed-time)
  (unless (listp window-list)
    (setq window-list (list window-list)))
  (setq window-list (Add-All-Subwindows window-list))
  (setf *transcript-window-list* window-list)
  ;;flush the header information: 3 lines
  (read-line *trans-from-file*)
  (read-line *trans-from-file*)
  (read-line *trans-from-file*)
  ;; read the old window list
  (let ((old-wins (read *trans-from-file*)))
    (if (/= (length old-wins)(length window-list))
	(error "Number of windows in transcript ~s is not the same as supplied ~s
	Execute (inter:close-transcript) to recover."
	       (length old-wins)(length window-list))
	(progn
	  (when *transcript-verbose*
	    (format T "Replaying transcript from ~s.~%" filename)
	    (format T "Window mappings are as follows:~%")
	    (do ((o old-wins (cdr o))
	         (c window-list (cdr c)))
	        ((null o))
	      (format T "  old=~a => current ~s~%" (car o) (car c)))
	    (format T "------------------- starting ----------------------~%"))
	  ;; now start reading the events
	  (Read-All-Transcript-Events)))))

(declaim (inline Process-Current-Event))
(defun Process-Current-Event ()
  (process-event *current-event*))

;; read the events
(defun Read-All-Transcript-Events ()
  (let ((display (gem:display-info-display
		  (the gem:DISPLAY-INFO
		       (g-value (car *transcript-window-list*)
				:display-info))))
	(start-time (get-internal-real-time))
	(last-event-time NIL)
	new-ev cur-wait-interval)
    (unwind-protect
	 (block eventloop
	   (loop
	      (setf new-ev (Read-Transcript-Event))
	      (if new-ev
		  (progn
		    (if-debug :event
			      (format t "~%<><><><> Event from transcript ~s~%"
				      new-ev))
		    (if *trans-wait-elapsed-time*
			(progn
			  (setf cur-wait-interval
				(if last-event-time
				    (- (event-timestamp new-ev)
				       last-event-time)
				    0))
			  (setf last-event-time (event-timestamp new-ev))
			  (loop		; until enough time elapsed
			     (when (eq (Trans-Check-CLX-Events display) :abort)
			       (return-from eventloop))
			     (when (>= (- (get-internal-real-time) start-time)
				       cur-wait-interval)
			       (return))) ; return from time wait loop
			  (setf start-time (get-internal-real-time)))
			;; else just check for events to see if abort
			;; or window refresh
			(when (eq (Trans-Check-CLX-Events display) :abort)
			  (return-from eventloop)))
		    (when *transcript-verbose*
		      (gem:set-window-property (event-window new-ev)
					       :POINTER-POSITION
					       (cons (event-x new-ev)
						     (event-y new-ev))))
		    (if (eq (event-char new-ev) :timer)
			(Handle-Timer-Event (event-code new-ev)) ;;index of inter
			(process-current-event)))		 ;; for KATIE (koz)
		  ;; else no more chars, so exit
		  (progn
		    (if *transcript-verbose*
			(format T "~%Transcript Complete~%"))
		    (return)))))
      ;; unwind-protect exit
      (setq opal::*inside-main-event-loop* NIL))
    (setq opal::*inside-main-event-loop* NIL)
    (beep)
    (close-transcript)))


(declaim (inline Trans-Out-Event))
(defun Trans-Out-Event (event)
  (when *trans-to-file* (write-transcript-event event)))

(defun Trans-Out-And-Process-Current-Event (&optional motion-event?)
  (when (and
	 *trans-to-file*
	 (or (not motion-event?) *trans-to-file-motion*))
    (write-transcript-event *current-event*))
  (process-current-event))

;; Since each clause returns NIL, this should loop until all pending
;; events are processed, at which time the :timeout 0 will cause it to
;; exit.
(defun Trans-Check-CLX-Events (display)
  (declare (ignore display))
  (let ((result
	 (gem:event-handler
	  (g-value gem:device-info :current-root)
	  T)))
    (if (eq result :abort)
	result
	T)))

;; Interactor Windows
;;
;; Holds a list of interactors that want to know about mouse moved
;; events on this window
(create-schema 'INTERACTOR-WINDOW
	       :declare ((:parameters :left top :width :height :border-width
				      :max-width :max-height :min-width :min-height
				      :cursor :title :icon-title :icon-bitmap
				      :omit-title-bar-p :position-by-hand
				      :draw-on-children :background-color
				      :double-buffered-p :save-under :visible)
			 (:type (integer :border-width)
				((or null integer) :max-width :max-height :min-width
				 :min-height)
				(cons :cursor)
				(string :title :icon-title)
				((or null (is-a-p opal:bitmap)) :icon-bitmap)
				((or null (is-a-p opal:color)) :background-color)
				(kr-boolean :omit-title-bar-p :position-by-hand
					    :draw-on-children :double-buffered-p
					    :save-under)))
	       (:is-a opal::window)
	       (:current-want-moved-interactors NIL))

(defparameter *last-grab-mouse-window* NIL)

(defun grab-mouse (window &key (owner-p NIL))
  (when window
    (let ((drawable (get-value window :drawable))
	  (want-enter-leave (g-value window :want-enter-leave-events)))
      (when drawable
	(setf *last-grab-mouse-window* window)
	(gem:mouse-grab window T want-enter-leave owner-p)))))

(defun Change-window-multi-grab (window needmultigrab)
  (if-debug :mouse (format T "Change-window-multi-grab to ~s for win ~s~%"
			   needmultigrab window))
  (let* ((want-enter-leave (g-value window :want-enter-leave-events))
	 (drawable (get-local-value window :drawable))
	 (old-val (g-value window :ignore-motion-em))
	 (em (if needmultigrab
	       (if want-enter-leave :E-G-K :G-K)
	       (if want-enter-leave :E-K :K))))
    (unless (eq em old-val)
      (s-value window :ignore-motion-em em) ; save for set-interest-in-moved
      (when drawable
	(s-value window :event-mask em)
	(gem:set-window-property window :EVENT-MASK em)))))

;; Releases the mouse grab that X does when there is a down press.
;; This should be called before set-interest-in-moved
(defun ungrab-mouse (window)
  (if-debug :mouse (format t "ungrabbing mouse~%"))
  (when window
    (let ((drawable (get-value window :drawable)))
    (when drawable
      (setf *last-grab-mouse-window* NIL)
      (gem:mouse-grab window NIL nil)))))

;; Iconified doesn't count as visible for the modal stuff
(defun Win-Visible (window)
  (let ((vis (g-value window :visible)))
    (and vis
	 (not (eq vis :ICONIFIED)))))

(define-method :update interactor-window (window &optional (total nil))
	       (opal::update-method-window window total)
	       ;; just call directly rather than using
	       ;; call-prototype-method
	       (Check-and-handle-changed-inters)
	       ;; if any interactors need fixing next make sure event
	       ;; mask is set correctly
  (let ((drawable (get-local-value window :drawable))
	(event-mask (get-local-value window :event-mask))
	(old-modal-and-visible (get-local-value window
						:old-modal-and-visible))
	;; windows that are iconified don't count
	(new-modal-and-visible (and (g-value window :modal-p)
				    (Win-Visible window))))
    ;;; handle when window changes :visible and :modal-p
    (unless (eq old-modal-and-visible new-modal-and-visible)
      (s-value window :old-modal-and-visible new-modal-and-visible)
      (if new-modal-and-visible		; then becoming modal and visible
	  (pushnew window *Visible-Modal-Windows*)
	  ;; else becoming either not modal or not visible
	  (deleteplace window *Visible-Modal-Windows*)))
    (unless event-mask		; then first time window has had a drawable
      (let* ((want-enter-leave (g-value window :want-enter-leave-events))
	     (em (if (g-value window :want-running-em)
		   (if want-enter-leave :E-K-M :K-M)
		   (or (g-value window :ignore-motion-em)
		       (if want-enter-leave :E-K :K)))))
	(gem:set-window-property window :EVENT-MASK em)
	(s-value window :event-mask em)))
    (if (and drawable (eq event-mask gem:*exposure-event-mask*))
        (gem:set-interest-in-moved window NIL))))

;; Debugging tools
;;
;; When true, *expose-throw-aways* will increment each time an
;; exposure event is thrown away
(defvar *expose-debug* nil)
(defvar *expose-throw-aways* 0)

(defun Key-Press (window x y state code time)
  ;; State is the modifier-bits code is the lookup in keysym
  (unless window
    (return-from key-press t))
  ;; if window was just destroyed, exit.
  (let ((c (gem:translate-character window x y state code time)))
    (when c
      (if-debug
       :event
       (format t "~%<><><><> Key ~S code=~s  state=~s  win=~s x=~s y=~s~%"
	       c code state window x y))
      (when (eq c *garnet-break-key*)
	(if-debug
	 :event
	 (format t
		 "Exiting main event loop because of *garnet-break-key*"))
	(exit-main-event-loop))
      (setf (event-char *Current-Event*) c
	    (event-mousep *Current-Event*) nil
	    (event-code *Current-Event*) code
	    (event-x *Current-Event*) x
	    (event-y *Current-Event*) y
	    (event-window *Current-Event*) window
	    (event-timestamp *Current-Event*) time
	    (event-state *Current-Event*) state)
      (trans-out-and-process-current-event)))
  t)


(defun Queue-Timer-Event (inter)
  (if-debug :event
	    (format t "~%<><><><> Timer Event, Inter Index=~s~%" inter))
  ;; only need the event for the transcripting
  (when *trans-to-file*
    (setf (event-char *Current-Event*) :timer
	  (event-mousep *Current-Event*) nil
	  (event-code *Current-Event*) inter
	  (event-x *Current-Event*) 0
	  (event-y *Current-Event*) 0
	  (event-window *Current-Event*) T
	  (event-timestamp *Current-Event*) NIL
	  )
    (trans-out-event *Current-event*))
  (Handle-Timer-Event inter)
  t)

(defun Button-Press (window x y state code event-key time)
  (let (c)
    ;; Check for null window before getting gem:Check-Double-Press code.
    (unless window			; if window was just destroyed, exit.
      (return-from button-press t))
    (setf code (gem:Check-Double-Press window state code time))
    (unless (and window code)		; if window was just destroyed, exit.
					; if bad double press (Mac) then exit
      (return-from button-press t))
    (setf c (gem:translate-mouse-character window code state event-key))
    (if-debug
     :event
     (format
      t "~%<><><><> Button down ~s event=~s code=~s state=~s window=~s"
      c event-key code state window)
     (format t " time=~s x=~s  y=~S~%" time x y))
    (setf (event-char *Current-Event*) c
	  (event-mousep *Current-Event*) t
	  (event-x *Current-Event*) x
	  (event-y *Current-Event*) y
 	  (event-code *Current-Event*) code
	  (event-downp *Current-Event*) t
	  (event-window *Current-Event*) window
	  (event-timestamp *Current-Event*) time
	  (event-state *Current-Event*) state
	  )
    (trans-out-and-process-current-event))
  t)

(defun Button-Release (window x y state code event-key time)
  (unless window			; if window was just destroyed, exit.
    (return-from button-release t))
  (let ((c (gem:translate-mouse-character window code state event-key)))
    ;; FMG Do we have to do this again?
    (unless window
      (return-from button-release t))	; if window was just destroyed, exit.
    (if-debug :event
      (format t "~%<><><><> Button Up ~s event=~s code=~s state=~s window=~s"
	      c event-key code state window)
      (format t " time=~s x=~s  y=~s~%" time x y))
    (setf (event-char *Current-Event*) c
	  (event-mousep *Current-Event*) t
	  (event-x *Current-Event*) x
	  (event-y *Current-Event*) y
 	  (event-code *Current-Event*) code
 	  (event-downp *Current-Event*) nil
	  (event-window *Current-Event*) window
	  (event-timestamp *Current-Event*) time
	  (event-state *Current-Event*) state
	  )
    (trans-out-and-process-current-event))
  t)

(defun Window-Enter (window x y time)
  (unless window
    (return-from Window-Enter t))	; if window was just destroyed, exit.
  (if-debug :event
	    (format t "~%<><><><> Enter Window ~s event=~s"
		    window :window-enter)
	    (format t " time=~s x=~s  y=~S~%" time x y))
  (setf (event-char *Current-Event*) :Window-enter
	(event-mousep *Current-Event*) t
	(event-x *Current-Event*) x
	(event-y *Current-Event*) y
	(event-code *Current-Event*) NIL
	(event-downp *Current-Event*) NIL
	(event-window *Current-Event*) window
	(event-timestamp *Current-Event*) time
	(event-state *Current-Event*) NIL
	)
  (trans-out-and-process-current-event)
  t)

(defun Window-Leave (window x y time)
  (unless window
    (return-from Window-Leave t))	; if window was just destroyed, exit.
  (if-debug :event
	    (format t "~%<><><><> Leave Window ~s event=~s"
		    window :window-Leave)
	    (format t " time=~s x=~s  y=~S~%" time x y))
  (setf (event-char *Current-Event*) :Window-Leave
	(event-mousep *Current-Event*) t
	(event-x *Current-Event*) x
	(event-y *Current-Event*) y
	(event-code *Current-Event*) NIL
	(event-downp *Current-Event*) NIL
	(event-window *Current-Event*) window
	(event-timestamp *Current-Event*) time
	(event-state *Current-Event*) NIL)
  (trans-out-and-process-current-event)
  t)

(defun Motion-Notify (window x y)
  (unless window
    (return-from motion-notify t))	; if window was just destroyed, exit.
  (let ((current-x x)
	(current-y y)
	(current-win window))
    (multiple-value-bind (e-x e-y e-w)
	(gem:discard-mouse-moved-events window)
      (when e-x
	(setf current-x e-x
	      current-y e-y
	      current-win e-w)))
    ;; done throwing away interim mouse-moved events
    (unless (and current-win  (eq current-win window))
      (return-from motion-notify t))	; if window was destroyed, exit.

    (if-debug :event
	      (format t "~%<><><><> Mouse Moved ~s ~s  window=~s~%"
		      current-x current-y window))
    (setf (event-char *Current-Event*) :mouse-moved
	  (event-code *Current-Event*) NIL
	  (event-mousep *Current-Event*) t
	  (event-x *Current-Event*) current-x
	  (event-y *Current-Event*) current-y
	  (event-window *Current-Event*) window
	  (event-timestamp *Current-Event*) 0)

    (trans-out-and-process-current-event T)) ; T --> Motion Notify
  t)

(defun do-button-press (a-window x y state code time event-key)
  (if *trans-from-file*
    T					; ignore events when read transcript
    (when a-window			; RGA ignore if event window is
					; no longer valid.
      (Button-Press a-window x y state code event-key time))))

(defun do-button-release (a-window x y state code time event-key)
  (if *trans-from-file*
    T					;ignore events when read trnscript
    (when a-window			;RGA ignore if event window is
					;no longer valid.
      (Button-Release a-window x y state code event-key time))))

(defun do-circulate-notify ()
  (opal::Circulate-Notify (debug-p :event)))

(defun do-configure-notify (a-window x y width height
			    above-sibling)
  (when a-window			;RGA Ignore if event-window no
					;longer valid.
    (opal::Configure-Notify (debug-p :event) x y
			    width height
			    a-window above-sibling)))

(defun do-enter-notify (a-window x y time)
  (if *trans-from-file*
    T					; ignore events when read
					; transcript
    (when a-window			;RGA ignore if window is no
					;longer valid.
	(Window-Enter a-window x y time))))

(defun do-exposure (a-window x y width height count display)
  (when a-window			;RGA ignore if window is no
					;longer valid.
    (opal::Exposure (debug-p :event) a-window count x y
		    width height display)))

(defun do-gravity-notify ()
  (opal::Gravity-Notify (debug-p :event)))

(defun do-key-press (a-window x y state code time)
  (if *trans-from-file*
    T					; ignore events when read transcript
    (when a-window			;RGA -- skip if window no
					;longer valid.
      (Key-Press a-window x y state code time))))

(defun do-leave-notify (a-window x y time)
  (if *trans-from-file*
    T					; ignore events when read transcript
    (Window-Leave a-window x y time)))

(defun do-map-notify (a-window)
  (opal::Map-Notify (debug-p :event) a-window))

(defun do-motion-notify (a-window x y display)
  (declare (ignore display))
  (if *trans-from-file*
    T					; ignore events when read transcript
    (Motion-Notify a-window x y)))

(defun do-unmap-notify (a-window)
  (opal::Unmap-Notify (debug-p :event) a-window))

(defun do-client-message (event-window type data format display)
  (cond ((and (eq format 32)
	      (eq type :WM_PROTOCOLS)
	      (eq (xlib:atom-name
		   display
		   (aref (the (simple-array (unsigned-byte 32) (5)) data) 0))
		  :WM_DELETE_WINDOW))
	 (opal::Delete-Notify NIL event-window))
	((and (eq format 32)
	      (eq type :TIMER_EVENT))
	 (if interactors::*trans-from-file*
	   T
	   ;; ignore events when read transcript
	   (interactors::Queue-Timer-Event
	    (aref (the (simple-array (unsigned-byte 32) (5)) data) 0)))))
  NIL)


;; We want this to run once and then exit.  (We used to want this to
;; run forever until exit-main-event-loop is called, but now we are
;; introducing a parellel process to run the event handler.
;; Therefore, main-event-loop etc. must put the call to
;; default-event-handler into a loop statement.
(defun default-event-handler (root-window)
  "Event handler for the interactor windows"
  ;; defined in inter:animation-process.lisp
  (setq *process-with-main-event-loop*
	#+allegro mp:*current-process*
	#+(and cmu mp) mp:*current-process*
	#+sb-thread sb-thread:*current-thread*
	#-(or allegro sb-thread (and cmu mp)) NIL)
  (gem:event-handler root-window NIL))


(opal:launch-main-event-loop-process)

(defun main-event-loop (&key (exit-when-no-window-visible :on))
  "Event handler for the interactor windows"
  (unless opal::*main-event-loop-process*
    (if (and (eq exit-when-no-window-visible :on)
	     (not (opal::any-top-level-window-visible)))
	(format t "Cannot call main-event-loop when no window is visible~%")
	;; else do real work
	(unless opal::*inside-main-event-loop*
	  (let ((root-window (g-value gem:device-info :current-root)))
	    (if (eq exit-when-no-window-visible :on)
		(setq opal::*inside-main-event-loop* t)
		(setq opal::*inside-main-event-loop* :dont-care))
;;	    (opal::m-e-l-new)		; Defined in opal/process.lisp
	    (opal::m-e-l)		; Defined in opal/process.lisp

	    (setq opal::*inside-main-event-loop* NIL)
	    (gem:discard-pending-events root-window 5))))))


(defun exit-main-event-loop ()
  (when (and (null opal::*main-event-loop-process*)
	     opal::*inside-main-event-loop*)
    (setq opal::*inside-main-event-loop* NIL)
    (throw 'exit-main-loop-exception t)))

(defvar opal::*exit-main-event-loop-function* #'exit-main-event-loop
  "This variable tells opal what function to call when you delete the
last window, so that main-loop will be exited automatically.")


;;; Wait-Interaction-Complete
;;

(defparameter *waiting-for-exit-wait-interaction-complete* 0)

;; FMG Need to properly interface this with the protected-eval stuff
;; so we pull out the inner loop so that we can have a version wrapped
;; in WITH-GARNET-ERROR-HANDLING.

(defun W-I-C-I-L ()
  "Wait-Interaction-Complete-Inner-Loop; pulled out so it can be smashed
by the protected-eval code."
  (catch 'exit-wait-interaction-complete
    (incf *waiting-for-exit-wait-interaction-complete*)
    (if *trans-from-file*
	(loop			      ; in a loop in case event handler exits
	   (Read-All-Transcript-Events)) ; read from transcript
	;; else get event from event handler
	(catch 'exit-main-loop-exception
	  (loop
	     (default-event-handler
		 (g-value gem:DEVICE-INFO :current-root)))
	  ))))

(defun Wait-Interaction-Complete (&optional window-to-raise)
  "Processes events, but this procedure does not exit unless the
  Interaction-Complete procedure is called.  The value returned by
  Wait-Interaction-Complete is the value passed to Interaction-Complete"
  (let (;; the next boolean is T if the main-event-loop process is running BUT
	;; Wait-Interaction-Complete was NOT called inside it.  If
	;; w-i-c was called from inside the m-e-l process, then don't
	;; want to kill the process.
	(running-mel-process-elsewhere
	 (opal:running-main-event-loop-process-elsewhere-p))
	(old-count *waiting-for-exit-wait-interaction-complete*))
    (when running-mel-process-elsewhere
      (opal:kill-main-event-loop-process))
    ;; need to update AFTER process is killed so the update message
    ;; won't be sent to the old process, since that might cause
    ;; exposure and configure events to begin to be processed in the
    ;; m-e-l-p which would cause a race condition.
    (when window-to-raise (opal:raise-window window-to-raise))
    (unwind-protect
	 ;; Note: the result of this form gets returned.
	 (w-i-c-i-l)
      ;; unwind-protect cleanups.
      (setf *waiting-for-exit-wait-interaction-complete* old-count)
      ;; discard current event which was the one that caused the exit
      (gem:discard-pending-events (g-value gem:device-info :current-root) 0)
      (when running-mel-process-elsewhere
	(opal:launch-main-event-loop-process)))
    ))

(defun Interaction-Complete (&optional val)
  "Call this to exit the Wait-Interaction-Complete procedure.  Typically,
   Interaction-Complete would be called from a :selection-function or
   :final-function"
  (if (zerop *waiting-for-exit-wait-interaction-complete*)
      (format T "WARNING: Interaction-Complete called but not inside Wait-Interaction-Complete~%")
      (throw 'exit-wait-interaction-complete val)))

(defparameter all-inter-windows NIL)

(defun Print-Inter-Windows ()
"Prints all the interactor windows.  Useful for debugging"
  (let ((*print-pretty* NIL))
    (dolist (i all-inter-windows)
      (format T "~s  " i))
    (format T "~%")))

;; Removes all interactors from the window, removes the window from the
;; global list
(define-method :destroy-me interactor-window (window)
  "Method to kill an interactor-window"
  (if-debug :window (format T "Destroying interactor window ~s~%" window))
  (Check-and-handle-changed-inters) ; make sure inters are up to date
				    ; before window is destroyed
  (setf all-inter-windows (delete window all-inter-windows))
  ;; doesn't do anything if not there
  (setf *Visible-Modal-Windows* (delete window *Visible-Modal-Windows*))
  (destroy-all-interactors window)
  (call-prototype-method window))

(define-method :initialize interactor-window (window)
  "Method to initialize an interactor-window"
  (if-debug :window
	    (format T "Initializing new interactor window ~s~%" window))
  (when (or *trans-from-file* *trans-to-file*)
	;; if transcripting is going on then the window will be added to the
	;; list of windows to be transcripted
        (setf *transcript-window-list* (nconc *transcript-window-list*
                                              (list window))))
  (pushnew window all-inter-windows)
  (Change-window-multi-grab window NIL)	     ; initialize for single-window inters
  (s-value window :priority-level-assoc NIL) ; associates interactors with this window
  (Handle-New-Window-for-T-Inters window)
  (call-prototype-method window)
  (if (and (g-value window :modal-p)
	   (Win-Visible window))
      (progn
	(s-value window :old-modal-and-visible T)
	(pushnew window *Visible-Modal-Windows*))
      (s-value window :old-modal-and-visible NIL))
  )


;; The following functions are most useful for making a pop-up menu that
;; is in its own window.
;;
(defun pop-up-win-and-start-interactor (a-window an-interactor
						 &optional left top)
  (when a-window
    (when left (s-value a-window :left left))
    (when top (s-value a-window :top top))
    (s-value a-window :visible T)
    (opal:update a-window)
    (ungrab-mouse a-window)
    (grab-mouse a-window))
  (when an-interactor
    (start-interactor an-interactor)))
