;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;*******************************************************************;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;*******************************************************************;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;*******************************************************************;;


;;; $Id::                                                             $
;;

;;; Change log:
;;   10/2/03 RGA            - Changed from switching on #+apple
;;                            to #+(and apple (not clx))
;;   6/4/92 Brad Myers      - animation-interactor
;;   5/28/92 Brad Myers     - new animation-process
;;   4/2/92  McDaniel       - New multifont.
;;   2/20/92 Andrew Mickish - Moved INTERACTORS package
;;                            definiton into Garnet-Loader
;;   1/30/92 Brad Myers     - removed kcl control-reader
;;   6/18/91 Ed Pervin      - added multifont-textinter.
;;   3/22/90 Robert Cook    - Define the package "INTERACTORS"
;; 			      for the TI Explorer.
;;   3/14/90 Brad Myers     - added textkeyhandling
;;   1/4/90 Ed Pervin       - Added version number
;;   6/7/89 Brad Myers      - Made to work with Sun Lucid Lisp also
;;   5/24/89 Brad Myers     - Added angleinter
;;   4/13/89 Brad Myers     - Changed name "interactors-loader" to "inter-loader"
;;   4/7/89 Brad Myers      - Added new key translation files
;;   3/11/89 lkb            - removed loading of cursor-text which was moved to opal


(in-package "COMMON-LISP-USER")

(defparameter Interactors-Version-Number "1.0")

(format t "Loading Interactors...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Inter-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Inter-PathName before loading interactors."))


;;; ---- Load interactors themselves

(Defvar Garnet-Inter-Files
  '(
    ;; key translation files
    "garnet-keytrans"
    "define-mouse-keys"

    "x-define-keys"
    "x-inter"

    ;; interactor files
    "interactors"
    "accelerators"
    "animation-process"
    "i-windows"
    "menuinter"
    "movegrowinter"
    "buttoninter"
    "twopointinter"
    "textkeyhandling"
    "textinter"
    "angleinter"
    "animatorinter"))

(dolist (file Garnet-Inter-Files)
  (load (merge-pathnames file Garnet-Inter-PathName)
	:verbose T))

(setf (get :garnet-modules :inter)  t)
(format t "...Done Interactors.~%")

