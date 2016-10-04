
;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ANIMATOR; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; Designed and implemented by Brad Myers
;;;

(defpackage :pixmap-lab
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(in-package :pixmap-lab)

(defparameter pixmapfilename "eye")

(defparameter pixmaps
  (list (opal:read-xpm-file (merge-pathnames
			     "eye1.xpm"
			     common-lisp-user::garnet-pixmap-pathname))))

(defun do-go (&key dont-enter-main-event-loop (double-buffered-p t))
  (let (agg)
    ;;;create top-level window
    (create-instance 'TOP-WIN inter:interactor-window
      (:left 500)
      (:top 100)
       (:double-buffered-p double-buffered-p)
       (:width 300)
       (:height 200)
       (:title "GARNET Animator Demo")
       (:icon-title "Animator"))
    (s-value top-win :aggregate
	     (setq agg (create-instance NIL opal:aggregate)))

    ;; If we get clobbered by the window manager, let the demos
    ;; controller know (if it's there).
    (when (fboundp 'common-lisp-user::garnet-note-quitted)
      (pushnew
       #'(lambda (win)
	   (declare (ignore win))
	   (common-lisp-user::garnet-note-quitted "demo-animator"))
       (g-value top-win :destroy-hooks)))

    (create-instance 'pixmap opal:pixmap
      (:left 5)
      (:top 168)
      (:count 0)
      (:image (o-formula (nth (gvl :count) pixmaps))))

    (opal:add-components agg pixmap)
    (opal:update top-win)
    (inter:main-event-loop)))

(defun do-stop ()
  (opal:destroy top-win))
