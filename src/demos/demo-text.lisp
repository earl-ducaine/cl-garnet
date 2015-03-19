;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-TEXT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$


;;; This file contains demo code for testing the text interactor
;;;
;;; This is intended as a test and demonstration of the text interactor
;;; as part of the Garnet project.
;;; 
;;; ** Call (Do-Go) to start and (Do-Stop) to stop **
;;;
;;; Designed and implemented by Brad A. Myers


(in-package :DEMO-TEXT)

(declaim (special VP TOP-AGG AGG VARIABLE-FONT TEXT-OBJ SPECIAL-OBJ
		  THE-FEEDBACK-OBJ))

(defvar *test-debug* NIL)

;; Load multifont stuff.
(unless (get :garnet-modules :multifont)
   (common-lisp-user::garnet-load (concatenate 'string "opal:" "multifont-loader")))

  
;;; ================================================================

(defun Create-string (copy-string-obj agg)
  (let (obj)
    (setq obj (create-instance NIL opal:multifont-text
		   (:left (g-value copy-string-obj :left))
		   (:top (g-value copy-string-obj :top))
		   (:initial-text (opal:get-text copy-string-obj))))
    (opal:add-component agg obj)))

;;; ================================================================

(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  ;;; create a viewport
  (create-instance 'vp inter:interactor-window (:left 10) (:top 50)
			 (:width 650) (:height 400)
			 (:double-buffered-p double-buffered-p)
 			 (:title "GARNET TEXT") (:icon-title "Text"))
  (s-value vp :aggregate
	   (create-instance 'top-agg opal:aggregate
				 (:overlapping T)))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-TEXT"))
     (g-value vp :destroy-hooks)))

  (create-instance 'agg opal:aggregate
				 (:overlapping T)
				 (:left 0)(:top 0)(:width 650)(:height 400))
  (opal:add-component top-agg agg)

  (create-instance 'variable-font opal:font
		   (:family :serif)
		   (:face :roman)
		   (:size :large))

  (create-instance 'text-obj opal:multifont-text
    (:initial-text
     (list
      (list
       (cons "left edit; middle create; right either"
	     variable-font))))
    (:left 10) (:top 10))
  (opal:add-component agg text-obj)

  (create-instance 'special-obj opal:multifont-text
    (:initial-text
     (list
      (list
       (cons "Press with shift-left anywhere to edit me"
	     variable-font))))
    (:left 10) (:top 40))
  (opal:add-component agg special-obj)

  (create-instance 'the-feedback-obj opal:multifont-text
    (:initial-text NIL)
    (:visible NIL)
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box)))))
  (opal:add-component top-agg the-feedback-obj)
 
  (opal:update vp)

  (create-instance 'edit-text-inter inter:multifont-text-interactor
    (:feedback-obj NIL)
    (:start-event (list :any-leftdown :except :shift-leftdown))
    (:start-where `(:element-of ,agg))
    (:window vp)
    (:abort-event :control-\g) ; \g so lower case
    (:stop-event '(:control-\j)))
  ;;-------------

   (create-instance 'create-text-inter inter:multifont-text-interactor
     (:feedback-obj the-feedback-obj)
     (:start-where T)
     (:window vp)
     (:start-event :any-middledown)
     (:abort-event :control-\g)
     (:stop-event '(:control-\j))
     (:stop-action
      #'(lambda (an-interactor obj-over stop-event)
	  ;; call parent to turn off feedback object visibility
	  (call-prototype-method an-interactor obj-over stop-event)
	  (let* ((feedback (g-value an-interactor :feedback-obj)))
	    (create-string feedback agg)
	    (opal:set-text feedback NIL)))))

;;-------------

      (create-instance 'create-or-edit-text-inter inter:multifont-text-interactor
	(:feedback-obj
	 (o-formula (if (eq :none (gvl :first-obj-over))
			;; then create a new object, so use feedback-obj
			the-feedback-obj
			;; else use object returned by mouse
			NIL)))
	(:start-where `(:element-of-or-none ,agg :type ,opal:multifont-text))
	(:window vp)
	(:start-event :any-rightdown)
	(:abort-event :control-\g)
	(:stop-event '(:control-\j))
	(:stop-action
	 #'(lambda (an-interactor obj-over stop-event)
	     ;; call parent to turn off feedback object visibility
	     (call-prototype-method an-interactor obj-over stop-event)
	     (when (eq :none (g-value an-interactor :first-obj-over))
	       (let* ((feedback (g-value an-interactor :feedback-obj)))
		 (create-string feedback agg)
		 (opal:set-text feedback NIL))))))

;;-------------

      (create-instance 'anywhere-press inter:multifont-text-interactor
	(:feedback-obj NIL)
	(:start-where T)
	(:window vp)
	(:start-event :shift-leftdown)
	(:stop-event '(:control-\j))
	(:obj-to-change special-obj))

  (opal:update vp)
  (Format T "~%Demo-Text:
  This creates and edits multi-line text objects.
  Type RETURN (ENTER) to go to a new line in the same string.
  Press with the left mouse button over a string to start editing it.
  Press inside string to move cursor or drag through to create a selection region.
  Press with middle button to create a new string and start editing.
  Press with the right button to edit a string if over one, otherwise create one.
  Press with shift-left button over anywhere to edit the second string.
  Stop by pressing any mouse button outside or hitting ^J.
  Abort by typing ^G.
Keyboard commands:
 ^f = forward char    meta-f = forward word     ^d = delete next char
 ^b = backward char   meta-b = backward word    del,bksp,^h = delete prev char
 ^p = prev line    ^n = next line  ^, = beginning of string  ^. = end of string
 ^a = beginning of line   ^e = end of line
 meta-d = delete next word      meta-h = delete prev word  ^u = delete all
 ^k = kill lines  ^u = delete entire string, ^w, CUT = delete selection
  META-w, COPY = copy selection to cut buffer
  ^c = copy entire string to X cut buffer
  ^y, PASTE = yank kill buffer or X cut buffer
  ^Y, ^PASTE = yank X buffer
  meta-y, meta-PASTE = yank kill buffer

The following ones extend the selection while moving
   ^leftarrow, ^rightarrow = prev, next char selecting
   meta-leftarrow, meta-rightarrow = prev, next word selecting
   ^uparrow, ^downarrow = up-line, down-line selecting
   ^HOME ^END = beginning, end of string selecting
   ^* = select all

Font changing:
 ^-shift-B = toggle bold  ^-shift-I = toggle italic
 ^-shift-F = fixed (courier)  ^-shift-T = times (serif)
               ^-shift-H = helvetica (sans-serif)
 ^-shift-< = smaller font  ^-shift-> = bigger font
 ^1 ^2 ^3 ^4 = small, medium, large and very-large fonts~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )


(defun do-stop ()
  (opal:destroy vp))

