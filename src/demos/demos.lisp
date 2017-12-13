;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.

(in-package :garnet-user)

(defun go-demos ()
  (demos-controller:do-go))

(defun stop-demos()
  (demos-controller:do-stop))

(format t (str "~%**Use (progn (in-package :garnet-user) (go-demos) to start all demos"
	       "**   and (stop-demos) to stop all the demos~%"))
