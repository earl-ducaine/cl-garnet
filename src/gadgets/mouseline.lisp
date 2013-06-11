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
;;;   MouseLine is a quite demo aggregadget that puts a line at the bottom of
;;;   the window showing what the mouse buttons will do:
;;;
;;;  Implemented by Brad Myers
;;;
;;;  This exports two gadgets.  The MouseLine when added to a window
;;;  will display the :help-string line of any object as a string.
;;;  The default position is at the bottom of the window, but that can
;;;  be overridden.  The MouseLinePopup pops up a window displaying
;;;  the help string text. 
;;;
;;;  Slots for MouseLine:
;;;    :left - default 5
;;;    :top - default =  bottom of window
;;;    :windows - windows whose objects to search through.  The
;;;               default is the single window containing the mouseline gadget
;;;    :wait-amount - time in seconds to wait before showing string.
;;;               If 0, then does immediately. Default: 0
;;;
;;;  Slots for MouseLinePopup
;;;      :start-event - event to cause immediate pop-up.  Default
;;;                     :SHIFT-CONTROL-META-LEFTDOWN 
;;;      :windows - windows whose objects to search through.  The
;;;           default is the single window containing the MouseLinePopup gadget
;;;      :wait-amount - time in seconds to wait before popping up window.
;;;                     If 0, then does immediately. Default: 3
;;;
;;;  Slot of objects that move over:
;;;     :help-string - must contain a string of helpful information, or NIL

#|
============================================================
Change log:
   8-25-96 Russell Almond -- Changed windows list to include popup windows 
          necessary for Mac Behavior problems.
   8-12-93 Brad Myers - When no process, then always use :wait-amount of 0.
                      - when pop-up already vis. and move, changes to new obj
   7-27-93 Andrew Mickish - To implement lazy loading of text-buttons for demo,
                            check :garnet-modules hash table
   6-25-93 Brad Myers - Added delayed invocation
 13-Oct-92 Brad Myers - created based on idea from Wayne Johnson and
                        Ken Meltsner
============================================================
|#


(in-package "GARNET-GADGETS")

(eval-when (eval load compile)
  (export '(MouseLine MouseLinePopup)))

(unless (boundp 'MouseLinePriorityLevel)
  (create-instance 'MouseLinePriorityLevel inter:priority-level
    (:active T)
    (:stop-when NIL))
  (push MouseLinePriorityLevel inter:priority-level-list))

(defun Launch-MouseLine-Process (inter obj wait-amount)
  (let ((old-obj (g-value inter :waiting-over-obj))
	(process-status (g-value inter :timer-wait-status)))
    (unless (and (eq old-obj obj)
		 process-status)
      ;; then start process
      (when process-status (inter::Kill-Timer-Process inter))
      (s-value inter :waiting-over-obj obj)
      (if obj
	  (progn 
	    (s-value inter :timer-wait-status :started)
	    (inter::launch-timer-process inter wait-amount T))
	  (progn
	    (s-value inter :timer-wait-status NIL))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun MouseLineTimerHandler (inter)
  ;; if wasn't killed, then should be still over the object, so display it
  (s-value inter :timer-wait-status NIL)
  (Display-MouseLine-Text inter (g-value inter :waiting-over-obj)))

(defun MouseLineRunAction (inter prev-obj new-obj)
  (declare (ignore prev-obj))
  (let ((wait-amount #+garnet-processes (g-value inter :wait-amount)
		     #-garnet-processes NIL))
    (if (and wait-amount
	     (> wait-amount 0))
	(Launch-MouseLine-Process inter new-obj wait-amount)
	(Display-MouseLine-Text inter new-obj))))

;;; Returns NIL if no text, otherwise returns string after setting it
(defun Display-MouseLine-Text (inter obj)
  (let ((str NIL))
    ;; check for the :help-string string, if not there, then
    ;; check parents all the way up
    (when obj
      (loop
       (setq str (g-value obj :help-string))
       (when str (return))
       (setq obj (g-value obj :parent))
       (unless obj (return))))
    (if str
	(s-value (g-value inter :operates-on) :string str) ; returns str
	(progn
	  (s-value (g-value inter :operates-on) :string "<no info>")
	  NIL)))) ; return NIL

(defun List-Of-Objects-Func ()
  (let ((wins (gvl :windows)))
    (if (listp wins)
	(let (l)
	  (dolist (w wins)
	    (setq l (append l (gv w :aggregate :components))))
	  (reverse l))
	;; else
	(reverse (gv wins :aggregate :components)))))

;; set all windows to want enter and leave events, and return the windows
(defun Set-Want-Enter-Leave (interwin)
  (cond ((schema-p interwin) (s-value interwin :want-enter-leave-events T))
	((null interwin) NIL)
	((eq t interwin)
	 (dolist (win inter::all-inter-windows)
	   (s-value win :want-enter-leave-events T)))
	((listp interwin)
	 (dolist (win interwin)
	   (s-value win :want-enter-leave-events T))))
  interwin)

(create-instance 'MouseLine opal:aggregadget
  (:left 5)
  (:top (o-formula (- (gvl :window :height)
		      (gvl :label :height)
		      5)))
  (:windows (o-formula (gvl :window)))
  (:wait-amount 0)

  ;; internal slots
  (:popupwin (o-formula (if (gvl :operates-on) (gvl :operates-on :popupwin))))
  ;; RGA -- need to add popupwin to user supplied list of windows
  (:all-wins (o-formula (if (gvl :popupwin)
                            (if (listp (gvl :windows))
                                (cons (gvl :popupwin) (gvl :windows))
                              (list (gvl :popupwin) (gvl :windows)))
                          (gvl :windows))))
 (:list-of-objects (o-formula (List-Of-Objects-Func)))

  (:string "<No info>") ; set directly by interactor
  (:parts
   `((:label ,opal:multi-text
      (:string ,(o-formula (gvl :parent :string)))
      (:left ,(o-formula (gvl :parent :left)))
      (:top ,(o-formula (gvl :parent :top))))))
  (:interactors 
   `((:checkit ,inter:menu-interactor
      (:start-where ,(o-formula (list :list-leaf-element-of
				      (gvl :operates-on)
				      :list-of-objects)))
      (:start-event T)  ; always running
      (:wait-amount ,(o-formula (gvl :operates-on :wait-amount)))
      (:stop-event NIL)
      (:continuous T)
      (:timer-handler MouseLineTimerHandler)
      (:slots-to-set (:ml-interim-selected :ml-selected :ml-selected))
      (:destroy-me ,#'(lambda (inter &optional (erase T))
			(when (g-value inter :timer-wait-status)
			  (inter::Kill-Timer-Process inter))
			(call-prototype-method inter erase)))
      (:window ,(o-formula (Set-Want-Enter-Leave (gvl :operates-on :all-wins))))
      (:waiting-priority ,MouseLinePriorityLevel)
      (:running-priority ,MouseLinePriorityLevel)
      (:running-action MouseLineRunAction)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun HidePopMouseLine (inter obj) 
  (declare (ignore obj))
  (inter::Kill-Timer-Process inter)
  (s-value inter :waiting-over-obj NIL)
  (let ((win (g-value inter :operates-on :popupwin)))
    (when win
      (s-value win :visible NIL))))

(defun InsideOutsideHandler (inter obj)
  (declare (ignore obj))
  (case (inter:event-char inter:*current-event*)
    (:WINDOW-LEAVE  ;; kill the process attached to checkit
     (HidePopMouseLine (g-value inter :operates-on :pop-checkit) NIL))
    ;;Should deal with looking for object at cursor when enter, but
    ;;obj parameter is always NIL, so don't bother.  Hope we get a
    ;;subsequent move event.
    ;; (:WINDOW-ENTER (when (schema-p obj)
    ;;		     (PopMouseLineRunning-Action
    ;;		      (g-value inter :operates-on :pop-checkit) NIL obj)))
    ))

(defun PopUpMouseLineTimerHandler (inter)
  ;; if wasn't killed, then should be still over the object, so display it
  (s-value inter :timer-wait-status NIL)
  (PopUp-MouseLine-Text inter (g-value inter :waiting-over-obj)))

(defun PopMouseLineRunning-Action (inter prev new)
  (declare (ignore prev))
  (let ((wait-amount #+garnet-processes (g-value inter :wait-amount)
		     #-garnet-processes NIL)
	win)
    (if (and wait-amount
	     (> wait-amount 0))
	(progn
	  (if new ; if there is an object, then wait, otherwise remove window
	      (let ((win (g-value inter :operates-on :popupwin)))
		(if (and win (gv win :visible))
		    ;; then window is already visible, move it to the new obj
		    (PopUp-MouseLine-Text inter new)
		    ;; else window not yet displayed, so wait.
		    (Launch-MouseLine-Process inter new wait-amount)))
	      (progn
		(when (and (setq win (g-value inter :operates-on :popupwin))
			   (schema-p win))
		  (s-value win :visible NIL))
		(HidePopMouseLine inter NIL))))
	(PopUp-MouseLine-Text inter new))))

(defun PopUp-MouseLine-Text (inter obj)
  (when obj
    (let ((win (g-value inter :operates-on :popupwin)))
      (multiple-value-bind (x y)
	  (opal:convert-coordinates (inter:event-window inter:*current-event*)
				    (inter:event-x inter:*current-event*)
				    (inter:event-y inter:*current-event*)
				    NIL)
	(s-value win :left x)
	(s-value win :top y)
	(if (Display-MouseLine-Text inter obj) ; sets string slot
	    (progn  ; then found a string
	      (s-value win :visible T)
	      (opal:raise-window win)) ; so will be on top
	    (progn ; no string to display
	      (s-value win :visible NIL)
	      (opal:update win))))))) ; in case was visible before

(create-instance 'pantone192 opal:color
  (:BLUE  0.35000002)
  (:GREEN  0.060000002)
  (:RED  1))
(create-instance 'pantone192Line opal:line-style
  (:line-thickness 3)(:foreground-color pantone192))

(create-instance 'pink opal:color
  (:BLUE  0.9)
  (:GREEN  0.9)
  (:RED  1.0))
(create-instance 'pinkfill opal:filling-style
  (:foreground-color pink))

(create-instance 'MouseLinePopup opal:aggregadget
  (:start-event :SHIFT-CONTROL-META-LEFTDOWN)
  (:windows (o-formula (gvl :window)))
  (:wait-amount 3)

  ;; internal slots
  ;; RGA -- need to add popupwin to user supplied list of windows
  (:all-wins (o-formula (if (gvl :popupwin)
                 (if (listp (gvl :windows))
                     (cons (gvl :popupwin) (gvl :windows))
                   (list (gvl :popupwin) (gvl :windows)))
               (gvl :windows))))
  (:list-of-objects (o-formula (List-Of-Objects-Func)))

  (:string "<No info>") ; set directly by interactor
  (:popup NIL)
  (:popupwin NIL)
  
  (:interactors 
   `((:pop-checkit ,inter:menu-interactor
      (:window ,(o-formula (Set-Want-Enter-Leave (gvl :operates-on :all-wins))))
      (:start-where ,(o-formula (list :list-leaf-element-of
				      (gvl :operates-on)
				      :list-of-objects)))
      (:start-event T)  ; always running
      (:wait-amount ,(o-formula (gvl :operates-on :wait-amount)))
      (:stop-event NIL)
      (:continuous T)
      (:timer-handler PopUpMouseLineTimerHandler)
      (:slots-to-set (:ml-interim-selected :ml-selected :ml-selected))
      (:destroy-me ,#'(lambda (inter &optional (erase T))
			(when (g-value inter :timer-wait-status)
			  (inter::Kill-Timer-Process inter))
			(call-prototype-method inter erase)))
      (:waiting-priority ,MouseLinePriorityLevel)
      (:running-priority ,MouseLinePriorityLevel)
      (:running-action PopMouseLineRunning-Action)
      (:back-inside-action ,#'(lambda (inter out prev new)
				(declare (ignore out))
				(PopMouseLineRunning-Action inter prev new)))
      (:outside-action ,#'(lambda (inter out prev)
			    (declare (ignore out prev))
			    (HidePopMouseLine inter NIL))))
     (:pop-inside-outside ,inter:button-interactor
      (:window ,(o-formula (gvl :operates-on :all-wins)))
      (:start-where T)
      (:start-event (:window-leave :window-enter))
      (:wait-amount ,(o-formula (gvl :operates-on :wait-amount)))
      (:continuous NIL)
      (:slots-to-set (:ml-interim-selected :ml-selected :ml-selected))
      (:waiting-priority ,MouseLinePriorityLevel)
      (:running-priority ,MouseLinePriorityLevel)
      (:final-function InsideOutsideHandler))
     
     (:pop-immediate-show ,inter:menu-interactor
      (:start-where ,(o-formula (list :list-leaf-element-of
				      (gvl :operates-on)
				      :list-of-objects)))
      (:start-event ,(o-formula (gvl :operates-on :start-event)))
      (:wait-amount 0) ; always show immediately
      (:slots-to-set (:ml-interim-selected :ml-selected :ml-selected))
      (:continuous T)
      (:window ,(o-formula (gvl :operates-on :all-wins)))
      (:waiting-priority ,MouseLinePriorityLevel)
      (:running-priority ,MouseLinePriorityLevel)
      (:start-action PopUp-MouseLine-Text)
      (:running-action PopMouseLineRunning-Action)
      (:stop-action HidePopMouseLine)
      (:abort-action HidePopMouseLine)))))

(create-instance 'MouseLinePopupParts opal:aggregadget
  (:popup NIL)
  (:parts
   `((:background ,opal:rectangle
      (:left 0)(:top 0)
      (:width ,(o-formula (gvl :window :width)))
      (:height ,(o-formula (gvl :window :height)))
      (:line-style ,(if (gv opal:color :color-p)
			pantone192Line
			opal:line-2))
      (:filling-style ,(if (gv opal:color :color-p)
			   pinkfill
			   NIL)))
     (:label ,opal:multi-text
      (:string ,(o-formula (gvl :parent :popup :string)))
      (:left 5)
      (:top 5)))))
      
(define-method :initialize mouselinepopup (ml)
   (call-prototype-method ml)
   (let ((kr::*demons-disabled* NIL) ; turn on all demons
	 (parts (create-instance NIL MouseLinePopupParts))
	 window)
     #-garnet-processes (when (g-value ml :wait-amount)
       (fresh-line)
       (format T "*** WARNING: wait-amount being ignored in mouseline because
***          this lisp does not support multiple processes."))
     (s-value parts :popup ml)
     (setq window (create-instance NIL inter:interactor-window
		    (:omit-title-bar-p T)
		    (:save-under T)
		    (:border-width 1)
		    (:aggregate parts)
		    (:string (g-value parts :label))
		    (:height (o-formula (+ 10 (gvl :string :height))))
		    (:width (o-formula (+ 10 (gvl :string :width))))
		    (:visible NIL)))
     (s-value ml :popup parts)
     (s-value ml :popupwin window)
     (opal:Update window)))
      
(define-method :destroy-me mouselinepopup (popup &optional erase)
  (let ((popup-win (g-value popup :popupwin)))
    ;; abort the interactors because destroying the window seems to
    ;; generate an error when the interactors are running 
    (inter:abort-interactor (g-value popup :pop-checkit))
    (inter:abort-interactor (g-value popup :pop-immediate-show))
    (when popup-win
      (opal:destroy popup-win))
    (call-prototype-method popup erase)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+garnet-test
(eval-when (eval load compile)
 (export '(Mouseline-Go Mouseline-Stop)))

#+garnet-test
(defun MouseLine-Go ()
    (unless (get :garnet-modules :text-buttons)
      (common-lisp-user::garnet-load "gg:text-buttons-loader"))
    (create-instance 'MouseLine-Win inter:interactor-window
      (:title "MouseLine Tester Window")
      (:aggregate (create-instance 'MouseLine-Agg opal:aggregate)))
    (create-instance 'Extra-MouseLine-Win inter:interactor-window
      (:top 320)(:height 85)
      (:title "Extra MouseLine Window")
      (:aggregate (create-instance 'MouseLine-Extra-Agg opal:aggregate)))
    (opal:add-components MouseLine-Extra-Agg
			 (create-instance NIL opal:rectangle
			   (:left 10)(:top 10)(:width 40)(:height 40)
			   (:filling-style opal:green-fill)
			   (:help-string "Green rectangle")))
    (opal:add-components MouseLine-Agg
			 (create-instance NIL opal:rectangle
			   (:left 10)(:top 10)(:width 40)(:height 40)
			   (:filling-style opal:blue-fill)
			   (:help-string "Blue Rectangle"))
		       (create-instance NIL opal:circle
			 (:left 10)(:top 30)(:width 40)(:height 40)
			 (:filling-style opal:red-fill)
			 (:help-string "Red circle"))
		       (create-instance NIL gg:text-button
			 (:left 100)(:top 10)
			 (:string "Push Me")
			 (:help-string "Left Button Pushes Button"))
		       (create-instance 'mouseline-subagg opal:aggregate)
		       (create-instance 'MouseLine-obj MouseLine
			 (:windows (list Extra-MouseLine-Win MouseLine-Win)))
		       (create-instance 'mouselinepopup-obj mouselinepopup
			 (:windows (list Extra-MouseLine-Win MouseLine-Win)))
		       )
    (opal:add-components mouseline-subagg
			 (create-instance NIL opal:rectangle
			   (:left 10)(:top 100)
			   (:help-string "Plain rectangle"))
			 (create-instance NIL opal:text
			   (:left 120)(:top 100)
			   (:string "no message with me")))
    (opal:update MouseLine-Win)
    (opal:update Extra-MouseLine-Win)
    "Moving around updates mouseline, hitting :SHIFT-CONTROL-META-LEFTDOWN
pops up a window")

#+garnet-test
(defun MouseLine-Stop ()
  (opal:destroy MouseLine-Win)
  (opal:destroy Extra-MouseLine-Win))


