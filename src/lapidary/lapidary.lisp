;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-

;;;         The Garnet User Interface Development Environment.

;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.
;;;
;;; This file contains code for initializing Lapidary. It presumes
;;; that Garnet is already loaded.  It defines "(do-go)", "(do-stop)",
;;; and the basic initialization code which creates lapidary windows
;;;
;;; Designed by Brad A. Myers, Brad Vander Zanden, and Roger
;;; Dannenberg.  Implemented by DSK...

(in-package :lapidary)

;;; Creates the editor window (in which all the drawing occurs)
(defun make-drawing-window ()
  (let ((new (create-lapidary-window
	       :left *new-vp-editor-left*
	       :top *new-vp-editor-top*
	       :width *new-vp-editor-width*
	       :height *new-vp-editor-height*
	       :title
	       (format nil "Drawing Window ~D" (incf *window-count*)))))
    (install-new-editor-window new)
    new))

(defun do-go ()
  (format t "Starting Lapidary...~%")
  ;; boot the interactor dialog boxes
  (when *load-db*
    (format t "Setting up interactor dialog boxes")
    (interactor-db-do-go))
  (format t "Setting up editor main menu~%")
  (editor-menu-do-go)
  ;; editor-menu must exist first because move-inter's :active slot
  ;; depends upon the editor-menu :build-p slot
  (move-grow-do-go)
  (make-drawing-window)
  (format t "Creating object menu~%")
  (shapes-do-go)
  (format t "Setting up editor functionality~%")
  (selection-do-go)
  (text-do-go)
  (create-object-do-go)
  (format t "Setting up box constraint menus~%")
  (gg:box-constraint-do-go)
  (format t "Setting up line constraint menus~%")
  (gg:line-constraint-do-go)
  ;; connect lapidary to the constraint gadget
  (s-value gg::*constraint-gadget* :obj-to-constrain
	   (o-formula (car (gv lapidary::*selection-info* :p-selected))))
  (s-value gg::*constraint-gadget* :obj-to-reference
	   (o-formula (car (gv lapidary::*selection-info* :s-selected))))
  (s-value gg::*constraint-gadget* :top-level-agg
	   (o-formula (gvl :obj-to-constrain :window :editor-agg)))
  ;; (line-constraint-do-go)
  (format t "Setting up property menus~%")
  ;; (line-do-go)
  ;; (shade-do-go)
  ;; (draw-fct-do-go)
  (format t "Setting up copy and instance interactors...~%")
  (copy-instance-inter-do-go)
  (save-restore-do-go)
  (format t "Save/Restore interactor started...~%")
  ;; (delete-window-inter-do-go)
  ;; (format t "Delete window interactor started...~%")
  (format t "Creating the interactor menus")
  (interactor-menu-do-go)
  (opal:update-all)
  (lapidary-beeps 3)
  (format t "~%~%Lapidary Started.~%~%")
  (setf kr::*constants-disabled* t)
  (inter:main-event-loop))

;; todo -- do-stop should leave the system in a state where do-go can restart
;; Lapidary.  Garnet often has problems shutting down X11 windows in a
;; way that allows the application to restart without stopping and
;; restarting the entire Lisp process.
(defun do-stop ()
  (declare (special *selection-info* aggrelist-feedback))
  (format t "Stopping Lapidary...~%")
  ;; kill the interactor dialog boxes
  (when *load-db*
    (format t "Destroying interactor dialog boxes")
    (interactor-db-do-stop))
  (move-grow-do-stop)
  (format t "Destroying object menu~%")
  (shapes-do-stop)
  (format t "Destroying editor functionality~%")
  (deselect-objects)
  (selection-do-stop)
  (create-object-do-stop)
  (text-do-stop)
  (format t "Destroying box constraint menu~%")
  (gg:box-constraint-do-stop)
  (format t "Destroying property menus~%")
  (format t "Destroying copy and instance interactors...~%")
  (copy-instance-inter-do-stop)
  (save-restore-do-stop)
  (format t "Save/Restore interactor destroyed...~%")
  (format t "Destroying the interactor menus...~%")
  (interactor-menu-do-stop)
  ;; for some crazy reason, the slots in *selection-info* are not set
  ;; properly if the drawing windows are destroyed first
  (let ((window-list (copy-list (g-value *selection-info* :window))))
    ;; The following three lines were added to make it possible
    ;; to restart Lapidary without loading all of Garnet. -- ECP 5/6/92
    (s-value *selection-info* :window NIL)
    (s-value *selection-info* :visible-windows nil)
    (s-value aggrelist-feedback :shape-menu NIL)
    (format t "Destroying drawing windows...~%")
    (dolist (window window-list)
	    (opal:destroy window)))
  (format t "Destroying editor main menu~%")
  (editor-menu-do-stop)
  (lapidary-beeps 3)
  (format t "~%~%Lapidary stopped.~%~%"))

;;; Tries to fix lapidary without destroying all windows
(defun fix-it ()
  ;; undo last change
  (undo)
  ;; abort any interactors that are running
  (dolist (inter inter:running-priority-level)
    (inter:abort-interactor inter))
  ;; make sure the selection code is stable
  (deselect-objects)
  (opal:update-all))

;;; Destroys all lapidary windows. to be used when all else fails and
;;; lapidary seems stuck
(defun clean-up ()
  (let ((windows (g-value *selection-info* :window)))
    ;; create a new window first, so that lapidary's interactors will not
    ;; be blown away
    (s-value *selection-info* :window nil)
    (make-drawing-window)
    ;; now destroy the windows
    (dolist (window windows)
      (opal:destroy window))
    ;; reset the slots related to selection in *selection-info*
    (s-value *selection-info* :p-selected nil)
    (s-value *selection-info* :s-selected nil)
    (s-value *selection-info* :feedback nil)
    ;; abort any interactors that are running
    (dolist (inter inter:running-priority-level)
      (inter:abort-interactor inter))
    ;; now make the new window visible
    (opal:update (car (g-value *selection-info* :window)))))
