;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: LAPIDARY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode: Lisp; Package: LAPIDARY -*-
;;;
;;; This file contains the button bindings for
;;; various editor operations

(in-package "LAPIDARY")

(defparameter *sec-select-one-obj*              :middledown)
(defparameter *sec-add-to-select*               :shift-middledown)
(defparameter *prim-select-one-obj*             :leftdown)
(defparameter *prim-add-to-select*              :shift-leftdown)
(defparameter *obj-creation-button*		:rightdown)
(defparameter *primary-deselection-button*	:control-leftdown)
(defparameter *secondary-deselection-button*	:control-middledown)
;(defparameter *modal-button*                    :shift-rightdown)
;(defparameter *delete-window-button*            :shift-rightdown)
(defparameter *grow-button*			:middledown)
(defparameter *move-button*                     :leftdown)
(defparameter *copy-button*                     :shift-rightdown)
(defparameter *instance-button*                 :control-rightdown)
(defparameter *prim-push-sel-under-button*      :shift-control-leftdown) 
(defparameter *sec-push-sel-under-button*       :shift-control-middledown) 

; number of pixels one can drag the mouse before it becomes a selection box
(defparameter *selection-threshold*             3)  

; object create			right
; primary (de)select		(control) left
; 2ndary (de)select		(control) middle
; primary add to selection  	shift-left
; secondary add to selection    shift-middle
; primary select covered obj    shift-control-leftdown
; secondary select covered obj  shift-control-middledown
; grow			 	middle
; move				left
; delete window			shift-right
; move between windows		shift-right
; copy between windows		shift-right
; instance between windows	shift-right

