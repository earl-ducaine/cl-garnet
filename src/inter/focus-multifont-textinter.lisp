;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; This file contains the keyboard interactors to input a multifont
;;  text line using a noncontinuous mode.
;;  It should be loaded after multifont-textinter



(in-package "INTERACTORS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(FOCUS-MULTIFONT-TEXTINTER
     SET-FOCUS
     CUT-SELECTION
     COPY-SELECTION
     PASTE-SELECTION)))


(declaim (special Focus-Multifont-Textinter))

;; Helper procedures for the default procedure to go into the slots

(defun Focus-Interactor-Initialize (new-Text-schema)
  (if-debug new-Text-schema (format T "Text initialize ~s~%" new-Text-schema))
   (Check-Interactor-Type new-Text-schema inter:focus-multifont-textinter)
   (Check-Required-Slots new-Text-schema)
   (Set-Up-Defaults new-Text-schema)
   (when (g-value new-Text-schema :obj-to-change)
      (set-focus new-Text-schema (g-value new-Text-schema :obj-to-change))))



;;; Go procedure utilities

(defun Focus-Do-Start (an-interactor obj-over event)
  (if-debug an-interactor (format T "Text starting over ~s~%" obj-over))
  ;; if obj-to-change supplied, then use that, otherwise use whatever was
  ;; under the mouse when started
  (let ((obj (g-value an-interactor :obj-to-change)))
    (when obj
      (kr-send an-interactor :stop-action an-interactor obj event))))


(defun Focus-Error (an-interactor &rest args)
   (declare (ignore args))
   (error "Focus-Multifont-Textinter has had an illegal procedure called.
Only the Do-Start procedure state is allowed to be used in this
interactor.  Somehow the interactor was pulled out of the start state.
The offending interactor was ~S." an-interactor)
)


(defun SET-FOCUS (interactor multifont)
  "Change focus of interactor from one multifont-text to another."
  (if (or (null multifont) (is-a-p multifont opal:multifont-text))
      (let ((obj (g-value interactor :obj-to-change)))
	(when obj
	  (opal:set-cursor-visible obj nil)
	  (opal:toggle-selection obj nil)
	  (when (g-value interactor :lisp-mode-p)
	    (turn-off-match interactor obj)))
	(if multifont
            (progn
	      (opal:set-cursor-visible multifont t)
	      (s-value interactor :obj-to-change multifont))
            (s-value interactor :obj-to-change nil))
	(s-value interactor :kill-mode nil)
	(curs-move interactor multifont))
      (error "Tried to set focus of ~S to ~S." interactor multifont)))


(defun CUT-SELECTION (interactor)
   (let ((string-object (g-value interactor :obj-to-change)))
      (when string-object
         (let ((deleted-stuff (if (g-value interactor :lisp-mode-p)
				  (inter:delete-lisp-region string-object)
				  (opal:delete-selection string-object))))
            (s-value interactor :cut-buffer deleted-stuff)
	    (curs-move interactor string-object)))))


(defun COPY-SELECTION (interactor)
   (let ((string-object (g-value interactor :obj-to-change)))
     (when string-object
       (let ((copied-stuff (opal:copy-selected-text string-object)))
	 (s-value interactor :cut-buffer copied-stuff)))))


(defun PASTE-SELECTION (interactor)
   (let ((string-object (g-value interactor :obj-to-change)))
      (when string-object
         (let ((copied-stuff (g-value interactor :cut-buffer)))
            (opal:insert-text string-object copied-stuff)
	    (curs-move interactor string-object))))) 

(defun Focus-Int-Stop-Action (an-interactor obj-over event)
  (if-debug an-interactor (format T "Focus Text int-stop, edit-char = ~S~%"
				  (event-char event)))
  ;; even though never actually starts, allow a stop event to call the
  ;; final function,
  (if (compare-and-get-possible-stop-event event
	                                   (g-value an-interactor :stop-event))
      (kr-send an-interactor :final-function an-interactor obj-over event
	       (opal:get-string obj-over)
	       (inter:event-x event) (inter:event-y event))
      ;; otherwise, edit char into string
      (obj-or-feedback-edit an-interactor obj-over
			    (g-value an-interactor :feedback-obj) event)))


;;; Focus-Multifont-Textinter schema
(Create-Schema 'inter:focus-multifont-textinter
   (:is-a inter:interactor)
   (:name :First-Focus-Text-interactor)
   (:stop-event NIL)
   (:stop-action 'Focus-Int-Stop-Action)
   (:start-where T)
   (:lisp-mode-p NIL)
   (:match-parens-p NIL)
   (:match-obj NIL)
   (:start-event '(:any-keyboard))
   (:obj-to-change NIL)			; Must supply.  Determines which multifont-text has
					; the keyboard focus.
   (:cursor-where-press T)
   (:remembered-last-object NIL)
   (:key-translation-table (o-formula (if (gvl :lisp-mode-p)
					  (gvl :lisp-translation-table)
					  (gvl :standard-translation-table))))
   (:standard-translation-table NIL)	; table of translations; set below
   (:lisp-translation-table NIL)	; table of translations for lisp; set below
   (:after-cursor-moves-func (o-formula (when (gvl :match-parens-p)
					  #'check-parens)))
   (:kill-mode NIL)
   (:edit-func 'Multifont-Edit-String)	; same as regular multifont-textinter
   (:Go 'General-Go)			; Proc executed when events happen.
   (:Do-Start 'Focus-Do-Start)		; Proc executed when event handled.
   (:Do-Running 'Focus-Error)
   (:Do-Explicit-Stop 'Focus-Error)
   (:Do-Stop 'Focus-Error)
   (:Do-Abort 'Focus-Error)
   (:Do-Outside 'Focus-Error)
   (:Do-Back-Inside 'Focus-Error)
   (:Do-Outside-Stop 'Focus-Error)
   (:initialize 'Focus-Interactor-Initialize) ; proc to call when created
)



;;; The key translations are the same for the
;; focus-multifont-textinter and the multifont-textinter

(Set-MultiFont-Default-Key-Translations inter:focus-multifont-textinter)
(Set-Lisp-Key-Translations inter:focus-multifont-textinter)
