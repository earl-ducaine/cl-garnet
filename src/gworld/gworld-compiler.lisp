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
;;; Changes:
;;; 10/02/03 RGA --- Flattened directory structure.
;;; 7/28/96 RGA --- changed to use garnet-compile/load
;;; 03/16/94 Mickish - Created



(in-package "COMMON-LISP-USER")

(defvar Gworld-Version-Number "1.0")

(format t "Compiling Gworld...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Gworld-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gworld-PathName before loading gem."))

(garnet-mkdir-if-needed Garnet-Gworld-Pathname)

;;;  Compile Gworld  ...
(Defparameter Garnet-Gworld-Files
  '(
    "gcable-macptrs"
    "utility"
    "pixmaps"
    "gworld"
    ))

(dolist (file Garnet-Gworld-Files)
  (let ((gfile (concatenate 'string "gworld:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Gworld-Src Garnet-Gworld-Pathname
		   '("gworld-loader.lisp"))


(setf (get :garnet-modules :gworld) t)
(format t "...Done Gworld.~%")
