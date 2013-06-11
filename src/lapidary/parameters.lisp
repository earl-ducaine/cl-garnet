;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This file contains the global parameters for lapidary.
;;; Designed by DSK

(in-package "LAPIDARY")

(defparameter *box-constraint-menu-dimensions* (list 772 0 375 510))
(defparameter *editor-window-dimensions* (list 0 #-apple 0 #+apple 50))
(defparameter *shape-menu-dimensions* (list 438 #-apple 0 #+apple 50
					    115 462))
(defparameter *shade-menu-dimensions* (list 551 #-apple 0 #+apple 50
					    115 295))
(defparameter *draw-fct-menu-dimensions* (list 666 251 0 215))
(defparameter *line-menu-dimensions* (list 666
					   #-apple 0 #+apple 50
					   110 252))
(defparameter *new-vp-editor-left* 0)
(defparameter *new-vp-editor-top* 461)
(defparameter *new-vp-editor-width* 450)
(defparameter *new-vp-editor-height* 500)
