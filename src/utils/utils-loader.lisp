;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;                                                                   ;;
;;*******************************************************************;;
;;  The Garnet User Interface Development Environment                ;;
;;  Copyright (c) 1989, 1990 Carnegie Mellon University              ;;
;;  All rights reserved.  The CMU software License Agreement         ;;
;;  specifies the terms and conditions for use and redistribution.   ;;
;;*******************************************************************;;

;;; $Id$
;;

;;; Change log:
;;	4/ 5/93 Dave Kosbie - created


(in-package "COMMON-LISP-USER")

(format t "Loading Utils...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Utils-PathName)
  (error
   "Load 'Garnet-Loader' first to set Garnet-Utils-PathName before loading Utils."))

(Defparameter Garnet-Utils-Files '(
	"general"
	))

(dolist (file Garnet-Utils-Files)
  (garnet-load (concatenate 'string "utils:" file)))

(setf (get :garnet-modules :utils)  t)
(format t "...Done Utils.~%")
