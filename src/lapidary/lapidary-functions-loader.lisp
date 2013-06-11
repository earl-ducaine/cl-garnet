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

#|
==================================================================
Change log:
   5/4/92  Ed Pervin -- Commented out provide.
   5/27/91 Brad Vander Zanden -- created
==================================================================
|#

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Lapidary-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Lapidary-PathName before loading this file."))


#+explorer
(unless (find-package "LAPIDARY")
  (make-package "LAPIDARY" :use '("LISP" "KR")))

;;; Now load the Lapidary-Functions module
;;;
(unless (get :garnet-modules :Lapidary-Functions)
  (format t "Loading Lapidary functions...~%")
  (garnet-load "lapidary:lapidary-functions")
  (format t "...Done Lapidary-Functions.~%"))

(setf (get :garnet-modules :Lapidary-Functions) t)
;;; (provide 'Lapidary-Functions)

