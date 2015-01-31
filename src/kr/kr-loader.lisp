;;; -*- Mode: COMMON-LISP; Package: COMMON-LISP-USER -*-
;; 
;; ______________________________________________________________________
;;
;; The Garnet User Interface Development Environment
;; Copyright (c) 1989, 1990 Carnegie Mellon University
;; All rights reserved.  The CMU software License Agreement specifies
;; the terms and conditions for use and redistribution.
;;
;; ______________________________________________________________________

;;; $Id::                                                                $
;;


(in-package "COMMON-LISP-USER")

(defparameter KR-Version-Number "2.3.4")

(format t "Loading KR...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Kr-PathName)
  (error
   "Load 'Garnet-Loader' first to set Garnet-Kr-PathName before loading KR."))

(Defparameter Garnet-Kr-Files
  '("kr-macros"
    "kr-doc"
    "kr"
    "constraints"))

(dolist (file Garnet-Kr-Files)
  (garnet-load (concatenate 'string "kr:" file)))

(setf (get :garnet-modules :kr)  t)
(format t "...Done Kr.~%")
