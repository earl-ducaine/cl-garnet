;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file contains the mouse and keyboard interactors to input an
;;; edited line of text.
;;; It should be loaded after Interactor
;;;
;;; Designed and implemented by Brad A. Myers

#|
============================================================
Change log:

 1/18/93  Brad Myers - stop when press outside object, unless
             :button-outside-stop? is NIL (default T)
 1/12/93  Brad Myers - made more robust when :continuous NIL
07/14/92  Brad Myers - fixed bug in auto-scroll call
06/19/92  Rajan Parthasarathy - Added auto-scroll
06/4/92   Brad Myers - added :input-filter like movegrow.  Used when create
                    new object (feedback object)
04/01/92  Andrew Mickish - kr::*debug-switch* ---> #-garnet-debug
03/18/92  Ed Pervin - Changed all control characters to keywords.
02/24/92  Andrew Mickish - Replaced previous s-value's of :visible slot with
            s-value's of :obj-over slot
02/12/92  Andrew Mickish - Removed s-value of :visible slot
01/21/92  Ed Pervin - Changes for CMUCL on Sparc
03/26/91  Greg Sylvain - changes for kcl
10/11/90  Brad Myers - added explicit Stop-Interactor code
06/18/90  Brad Myers - When starts, sets feedback-obj's :obj-over slot
06/01/90  Brad Myers - change so edit function is a method
03/14/90  Brad Myers - change to new key translation mechanism -- much code
            moved to textkeybindings.lisp
03/07/90  Brad Myers - Allow change of cursor position using the mouse while
            editing; ^C copies string to X cut buffer; ^W does delete-back-
            word; added new function Insert-Text-Into-String
12/11/89  Brad Myers - Fixed translate-event so no errors for uparrow, etc.
            on Sun
11/01/89  Ed Pervin - Altered Translate-event to handle symbol characters like
            :leftarrow on Sun.  Beep now beeps on Sun.
10/05/89  Brad Myers - Added Final-Function;  Removed :new-obj-over
            (use :first-obj-over)
10/04/89  Roger Dannenberg - Change debugging output
09/22/89  Brad Myers - Made more robust when :start-where = T
09/11/89  Brad Myers - Fixed for multi-line text
08/14/89  Brad Myers - Fixed for multiple priority levels
07/27/89  Brad Myers - Cursor goes where press by default
06/26/89  Brad Myers - Fixed to have quote for create-schema; changed to have
            obj-to-change
06/19/89  Brad Myers - Copy cut buffer into string if use ^y or INSERT
06/07/89  Brad Myers - Fixed to work with lucid lisp also
05/30/89  Brad Myers - Call-method -> kr-send; allow running-where to be set
            after initialized
04/20/89  Brad Myers - schema-call -> call-method
04/14/89  Brad Myers - fixed self-deactivate
04/07/89  Brad Myers and Dario Giuse - fixed for new KR
03/01/89  Brad Myers - radically change behavior
02/10/89  Lynn Baumeister - added do-xxx and text-int-xxx funcs
01/09/89  Lynn Baumeister - made sure that top 3 funcs worked
11/29/88  Brad Myers - started


============================================================
|#

(in-package "INTERACTORS")

;;;============================================================
;;;============================================================
;;;============================================================


;;;============================================================
;;; Helper procedures for the default procedures to go into the slots
;;;============================================================

;;; note:  In the case that the start eventis a mouse-down, the next event
;;;        to be preccessed is a leftup, which will get passed to the
;;;        translate-event routine, hence the (if (not (event-mousep event)))
;;; also,  uparrow and downarrow aren't printable in some fonts, 
;;;        so have to disallow them



 
;; edit either the feedback object or the main object
(defun obj-or-feedback-edit (an-interactor obj-over feedback-obj event)
  ;; the function used as :edit-func is edit-string which is in the file
  ;; textkeyhandling
  (let ((obj-to-edit (or feedback-obj obj-over)))
    (kr-send an-interactor :edit-func an-interactor obj-to-edit event)
    (when (g-value obj-to-edit :auto-scroll-p)
      (kr-send obj-to-edit :auto-scroll obj-to-edit))
    (dbprint-str obj-to-edit an-interactor feedback-obj)))

;; turn the cursor visibility on or off
(defun obj-or-feedback-cursor-on-off (obj-over feedback-obj turn-on-p inter)
  #-garnet-debug (declare (ignore inter))
  (when (or feedback-obj (schema-p obj-over)) ; otherwise, just exit because no
                                              ; object to set
    (let ((obj (or feedback-obj obj-over))
	  val)
      (if turn-on-p
	  (progn
	    (setq val (g-value obj :saved-cursor-index))
	    (dbprint-either :cursor-index obj val inter feedback-obj)
	    (s-value obj :cursor-index val))
	  ; else save current index and turn off cursor
	  (progn 
	    (s-value obj :saved-cursor-index (g-value obj :cursor-index))
	    (dbprint-either :cursor-index obj NIL inter feedback-obj)
	    (s-value obj :cursor-index NIL))))))

;; Copies the 2 values into an existing list if there, otherwise creates one 
(defun set-obj-list2-slot (obj slot val1 val2)
  (let ((oldval (get-local-value obj slot)))
    (if (and oldval (listp oldval) (>= (length oldval) 2))
	; then re-use old slots so no cons-ing
	(progn
	  (setf (first oldval) val1)
	  (setf (second oldval) val2)
	  (Mark-As-Changed obj slot)) ; do this to get constraints to go
	; else create a new one
	(s-value obj slot (list val1 val2)))))

(defun Get-Cursor-Position (an-interactor obj event)
  (or (and (g-value an-interactor :cursor-where-press)
	   (opal:get-cursor-index obj (event-x event)
				  (event-y event)))
      (length (g-value obj :string))))

;;;============================================================
;;; Default Procedures to go into the slots
;;;============================================================

(declaim (special Text-Interactor))

(defun Text-Interactor-Initialize (new-Text-schema)
  (if-debug new-Text-schema (format T "Text initialize ~s~%" new-Text-schema))
  (Check-Interactor-Type new-Text-schema inter:text-interactor)
  (Check-Required-Slots new-Text-schema)
  (Set-Up-Defaults new-Text-schema)
  ) ;end initialize procedure

;;; make a copy of the orignal string in case :abort happens
(defun Text-Int-Start-Action (an-interactor new-obj-over start-event)
  (if-debug an-interactor (format T "Text int-start over ~s~%" new-obj-over))
  (let ((feedback (g-value an-interactor :feedback-obj))
	indx)
    (multiple-value-bind (startx starty)
	(Get-Filtered-Input an-interactor start-event)
      (s-value an-interactor :startx startx)
      (s-value an-interactor :starty starty)
      (if feedback
	  (progn
	    (if-debug an-interactor
	      (format T "  * Setting :box of ~s (feedback-obj) to (~s ~s ..)~%"
			      feedback startx starty))
	    (set-obj-list2-slot feedback :box startx starty)
	    (dbprint-feed :obj-over feedback new-obj-over an-interactor)
	    (s-value feedback :obj-over new-obj-over)
	    (s-value an-interactor :original-string
		     (copy-seq (g-value feedback :string)))
	    (setq indx (Get-Cursor-Position an-interactor
					    feedback start-event))
	    (dbprint-feed :cursor-index feedback indx an-interactor)
	    (s-value feedback :cursor-index indx)
	    ;;	  (dbprint-feed :visible feedback T an-interactor)
	    ;;	  (s-value feedback :visible T)
	    )
	  ;; else modify new-obj-over
	  (progn
	    (s-value an-interactor :original-string
		     (copy-seq (g-value new-obj-over :string)))
	    (when (schema-p new-obj-over)
	      (setq indx (Get-Cursor-Position an-interactor
					      new-obj-over start-event))
	      (dbprint :cursor-index new-obj-over indx an-interactor)
	      (s-value new-obj-over :cursor-index indx))))
      (obj-or-feedback-edit an-interactor new-obj-over feedback start-event))))

(defun Text-Int-Running-Action (an-interactor obj-over event)
  (if-debug an-interactor (format T "Text int-running, edit-char = ~S~%"
				  (event-char event)))
  (obj-or-feedback-edit an-interactor obj-over
			(g-value an-interactor :feedback-obj) event))

(defun Text-Int-Outside-Action (an-interactor last-obj-over)
  (if-debug an-interactor (format T "Text int-outside object=~s~%" last-obj-over))
  (obj-or-feedback-cursor-on-off last-obj-over
				 (g-value an-interactor :feedback-obj) NIL
				 an-interactor))

(defun Text-Int-Back-Inside-Action (an-interactor obj-over event)
  (if-debug an-interactor (format T "Text int-back-inside, obj-ever = ~S ~% "
				  obj-over))
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (obj-or-feedback-cursor-on-off obj-over feedback T an-interactor)
    (obj-or-feedback-edit an-interactor obj-over feedback event)))

(defun Text-Int-Stop-Action (an-interactor obj-over event)
  (if-debug an-interactor (format T "Text int-stop over ~s~%" obj-over))
  (let ((feedback (g-value an-interactor :feedback-obj))
	val)
    ;; ** NOTE final character is NOT edited into the string
    ;; ** (obj-or-feedback-edit an-interactor obj-over feedback event)
    (obj-or-feedback-cursor-on-off obj-over feedback NIL an-interactor)
    (when (and feedback (schema-p obj-over))
      (setq val (g-value feedback :string))
      (dbprint :string obj-over val an-interactor)
      (s-value obj-over :string (copy-seq val)))  ; copy the string so it
						  ; is not shared with the
						  ; feedback object
    ; amickish - 2/24/92
    (when feedback
      (dbprint-feed :obj-over feedback NIL an-interactor)
      (s-value feedback :obj-over NIL)
      )
;    (when feedback
;      (dbprint-feed :visible feedback NIL an-interactor)
;      (s-value feedback :visible NIL)
;      )
    (when (g-value an-interactor :final-function)
      (let ((str ; try to come up with a final string for final-function
	     (if (schema-p obj-over)
		 (g-value obj-over :string)
		 (if feedback (g-value feedback :string) NIL)))
	    startx starty)
	(if (g-value an-interactor :continuous)
	    (progn (setf startx (g-value an-interactor :startx))
	      (setf starty (g-value an-interactor :starty)))
	    (progn (setf startx (event-x event))
	      (setf starty (event-y event))))
	(KR-Send an-interactor :final-function an-interactor obj-over event
		 str startx starty)))))

(defun Text-Int-Abort-Action (an-interactor orig-obj-over event)
  (declare (ignore event))
  (if-debug an-interactor (format T "Text int-abort over ~s~%" orig-obj-over))
  (let ((feedback (g-value an-interactor :feedback-obj)))
    (if feedback
	(progn
	  (s-value feedback :string
		   (copy-seq (g-value an-interactor :original-string)))
	  (s-value feedback :cursor-index NIL)
	  (dbprint-str feedback an-interactor T)
	  ; amickish 2/24/92
	  (dbprint-feed :obj-over feedback NIL an-interactor)
	  (s-value feedback :obj-over NIL)
;	  (dbprint-feed :visible feedback NIL an-interactor)
;	  (s-value feedback :visible NIL)
	  )
	(when (schema-p orig-obj-over)
	  (progn
	    (s-value orig-obj-over :string
		     (copy-seq (g-value an-interactor :original-string)))
	    (s-value orig-obj-over :cursor-index NIL)
	    (dbprint-str orig-obj-over an-interactor NIL))))))

;;;============================================================
;;; Go procedure utilities
;;;============================================================


;;; if continuous: (remove from start level, add to stop and abort
;;; 		    level, change state to running
;;; 		    )
(defun Text-do-start (an-interactor obj-over event)
  (if-debug an-interactor (format T "Text starting over ~s~%" obj-over))
        ;; if obj-to-change supplied, then use that, otherwise use whatever was
	;; under the mouse when started
  (let ((obj (or (g-value an-interactor :obj-to-change) obj-over)))
    (if (g-value an-interactor :continuous)  ;then will go to running state
	(progn
	  (Fix-Running-Where an-interactor obj-over)
	  (s-value an-interactor :remembered-object obj) ; object to edit
	  (GoToRunningState an-interactor
			    (if (eq T (Get-Running-where an-interactor))
				NIL  ; run anywhere so don't get mouse-moved
				T))  ; need mouse moved to see if outside
	  (kr-send an-interactor :start-action an-interactor obj event))
	;else call stop-action
	(progn
	  (GoToStartState an-interactor NIL)
	  (kr-send an-interactor :stop-action an-interactor obj event)))))

(defun Text-do-running (an-interactor obj-over event)
  #-garnet-debug (declare (ignore obj-over))
  (if-debug an-interactor (format T "Text running over ~s~%" obj-over))
  (let ((obj (g-value an-interactor :remembered-object)))
    (if (and (event-mousep event)
	     (event-code event) ; code is non-nil for "real" events
	     (g-value an-interactor :button-outside-stop?)
	     (event-downp event)
	     (or (not (schema-p obj))
		 (not (eq (event-window event)
			  (g-value obj :window)))
		 (not (opal:point-in-gob obj (event-x event)
					 (event-y event)))))
	;; then button outside object, so stop
	(progn
	  (text-explicit-stop an-interactor)
	  :stop)  ;; return a special value so this event is re-processed
	;; else process normally
	(kr-send an-interactor :running-action an-interactor obj event))))

;;; Will be inside
;;; Remove from running level, add to start level
;;; unless :self-deactivate, change state to start, call stop procedure
(defun Text-do-stop (an-interactor obj-over event)
  #-garnet-debug (declare (ignore obj-over))
  (if-debug an-interactor (format T "Text stop over ~s~%" obj-over))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Stop-Action an-interactor
	       (g-value an-interactor :remembered-object) event))

(defun Text-Explicit-Stop (an-interactor)
  (if-debug an-interactor (format T "Text explicit stop~%"))
  (GoToStartState an-interactor T)
  ;; Fortunately, the stop-event is not edited into the string, so we can
  ;; afford to use the current event, even though it may contain a
  ;; character, or be outside, or whatever.
  (kr-send an-interactor :Stop-Action an-interactor
	   (g-value an-interactor :remembered-object) *current-event*))


;;;remove from running level, put on start level, change state to
;;; start, call abort procedure    Become-inactive ignored because :active
;;; set before this is called
(defun Text-do-abort (an-interactor become-inactive event)
  (declare (ignore become-inactive))
  (if-debug an-interactor (format T "Text aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor
	       (g-value an-interactor :remembered-object) event))

;;; call outside procedure, change state to outside
(defun Text-do-outside (an-interactor)
  (if-debug an-interactor (format T "Text outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	       (g-value an-interactor :remembered-object)))

(defun Text-do-outside-stop (an-interactor event)
  (if-debug an-interactor (format T "Text stop outside~%"))
  (if (eq :last (g-value an-interactor :outside))
      (text-do-stop an-interactor NIL event)
      (text-do-abort an-interactor NIL event)))

;;; call back-inside procedure, change state to running
(defun Text-do-back-inside (an-interactor obj-over event)
  #-garnet-debug (declare (ignore obj-over))
  (if-debug an-interactor (format T "Text back-inside over ~s~%" obj-over))
  (s-value an-interactor :current-state :running)
  (kr-send an-interactor :back-inside-action an-interactor
	       (g-value an-interactor :remembered-object) event))


;;;============================================================
;;; Text schema
;;;============================================================

(Create-Schema 'inter:text-interactor (:is-a inter:interactor)
  (:start-action 'Text-Int-Start-Action)
  (:running-action 'Text-Int-Running-Action)
  (:obj-to-change NIL)			;supply if don't want to affect
					; result of :start-where
  (:running-where T)
  (:button-outside-stop? T) 
  (:cursor-where-press T)
  (:key-translation-table NIL)		;table of translations; set below
  (:edit-func 'Edit-String)
  (:input-filter NIL)
  (:stop-action 'Text-Int-Stop-Action)
  (:stop-event '(#\return :num-pad-enter))
  (:abort-event '(:control-\g :control-g))
  (:abort-action 'Text-Int-Abort-Action)
  (:outside-action 'Text-Int-Outside-Action)
  (:back-inside-action 'Text-Int-Back-Inside-Action)
  (:remembered-last-object NIL)
  (:Go 'General-Go)			; proc executed when events happen
  (:Do-Start 'Text-Do-Start)		; these are
  (:Do-Running 'Text-Do-Running)	;   called by GO
  (:Do-Explicit-Stop 'Text-Explicit-Stop) ;for stop-interactor
  (:Do-Stop 'Text-Do-Stop)		;   to do
  (:Do-Abort 'Text-Do-Abort)		;   the real work.
  (:Do-Outside 'Text-Do-Outside)	;   They call the
  (:Do-Back-Inside 'Text-Do-Back-Inside) ; appropriate
  (:Do-Outside-Stop 'Text-Do-Outside-Stop) ; -action procedures
  (:initialize 'Text-Interactor-Initialize)) ;proc to call
							   ; when created

(Set-Default-Key-Translations inter:text-interactor)
