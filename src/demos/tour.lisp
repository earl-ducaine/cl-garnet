;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id::                                                             $	

;;; This is the loader file for the special Garnet Tour

(in-package :COMMON-LISP-USER)
(use-package '(:COMMON-LISP :KR :KR-DEBUG :GARNET-DEBUG))

(defvar MYWINDOW NIL)

(dolist (file '("gg:radio-buttons-loader" 
                "gg:v-slider-loader"
                "gg:menu-loader"
                "gg:v-scroll-loader"))
   (garnet-load file))

(dolist (file '("demos:mge" "demos:demo-othello" "demos:tour-transcript"))
   (garnet-load file))

(create-instance 'moving-rectangle opal:rectangle
		 (:box '(80 20 100 150))
		 (:left (o-formula (first (gvl :box))))
		 (:top (o-formula (second (gvl :box))))
		 (:width (o-formula (third (gvl :box))))
		 (:height (o-formula (fourth (gvl :box)))))

(defun start-othello ()
  (demo-othello:do-go)
  T)

(defun start-editing ()
  (mge:do-go)
  T)

(defun stop-othello ()
  (demo-othello:do-stop)
  "Bye-bye from Othello")

(defun stop-tour ()
  (when (schema-p mywindow)
    (opal:destroy mywindow))
  (demo-othello:do-stop)  ; this stops mge also
  "Thank you for your interest in the Garnet Project")

(Format T "~%~%Garnet-Tour Load Complete.~%
Welcome to the Garnet Tour.  You can now start typing.~%
Typing (do-tour) starts the tour.")
