;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-

(in-package "USER" :use '("LISP" "KR"))

(defvar MFG-INIT
(dolist (gadget '("multifont-gadget-loader"
		  ))
  (load (merge-pathnames gadget
			 user::Garnet-Gadgets-PathName))))

(defvar BOLD (opal:get-standard-font NIL :bold NIL))
(defvar ITALIC (opal:get-standard-font NIL :italic NIL))

(when (boundp 'WIN) (opal:destroy WIN))

(setf kr::*constants-disabled* T)

(create-instance 'WIN inter:interactor-window
  (:left 700) (:top 10)
  (:width 190) (:height 92)
  (:aggregate (create-instance 'TOP-AGG opal:aggregate)))
(opal:update WIN)

(create-instance 'MF opal:multifont-text
  (:left 5) (:top 5)
  (:text-width 180)
  (:word-wrap-p T)
  (:initial-text (list (list "The" (cons " multifont-gadget " BOLD))
		       (list "combines the multifont" (cons " objects " ITALIC)
			     "and" (cons " interactors " ITALIC)
			     "in a gadget that allows text editing with word- wrap"))))

(opal:add-component TOP-AGG MF)
(opal:update WIN)

(create-instance 'MFG-E opal:text
  (:left 170) (:top 2)
  (:string "(e)")
  (:font BOLD))
(opal:add-component TOP-AGG MFG-E)
(opal:update WIN)