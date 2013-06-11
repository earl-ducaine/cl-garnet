;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          The Garnet User Interface Development Environment.       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


(in-package "COMMON-LISP-USER")

(defparameter Truetype-Version-Number "0.1")

(format t "Loading Truetype Dependencies...~%")

(defvar *truetype-dependencies*
  '(:zpb-ttf :cl-vectors :cl-paths-ttf :cl-aa :cl-fad :cl-store))

(dolist (dep *truetype-dependencies*)
  (require dep))

(format t "Loading Truetype...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Truetype-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Truetype-PathName before loading Truetype."))

;;;  Load Truetype  ...

(Defparameter Garnet-Truetype-Files
  '(
    "package-truetype"
    "clx-truetype-utils"
    "font-cache"
    "clx-truetype"
    ))

(dolist (file Garnet-Truetype-Files)
  (let ((gfile (merge-pathnames file Garnet-Truetype-PathName)))
    (load gfile)))

(setf (get :garnet-modules :truetype) t)
(format t "...Done Truetype.~%")
