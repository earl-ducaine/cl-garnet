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
;;; 10/02/03 RGA - Flattened directory structure, adapted to new load/compile
;;; 03/16/94 Mickish - Created

;;;
;;;   The Gworld module has been taken in its entirety from code contributed
;;;   by Alan Ruttenberg to the MCL repository on cambridge.apple.cmu, in
;;;   /pub/mcl/contrib/gworld.sit.hqx.  Although there may have been slight
;;;   changes from the original code, the Garnet project has maintained the
;;;   original structure and organization of the files.  Not all Gworld files
;;;   that were contributed are included in the Garnet project.

(in-package "COMMON-LISP-USER")

(defparameter Gworld-Version-Number "1.0")

(format t "Loading Gworld...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Gworld-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gworld-PathName before loading gem."))

;;;  Load Gworld  ...
(Defparameter Garnet-Gworld-Files
  '(
    "gcable-macptrs"
    "utility"
    "pixmaps"
    "gworld"
    ))

(let ((ccl:*WARN-IF-REDEFINE* NIL))
  (unless (get :garnet-modules :gworld)
    (dolist (file Garnet-Gworld-Files)
       #+comment
      (load (merge-pathnames file Garnet-Gworld-Pathname))
      ;; Don't truncate, because gworld files are in sub-subdirectories
      (garnet-load (concatenate 'string "gworld:" file)))))

(setf (get :garnet-modules :gworld) t)
(format t "...Done Gworld.~%")
