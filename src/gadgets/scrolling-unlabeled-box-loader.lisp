;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

;;; Created 6/25/92 RGA

;;; Loader for protected eval stuff.

(in-package "COMMON-LISP-USER")

(unless (get :garnet-modules :scrolling-unlabeled-box)
  (format t "Loading Scrolling Unlabeled Box~%")
  (dolist (pair '(			;(:new-types "new-types")
		  (:scrolling-labeled-box "scrolling-labeled-box-loader")
		  (:scrolling-unlabeled-box "scrolling-unlabeled-box")
		  ))
    (unless (get :garnet-modules (car pair))
      (load (merge-pathnames (cadr pair) Garnet-Gadgets-Pathname)
	    :verbose T)))
  (format t "...Done Scrolling Unlabeled box.~%"))

(setf (get :garnet-modules :new-types) t)
(setf (get :garnet-modules :scrolling-unlabeled-box) t)

