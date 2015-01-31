;;; -*- Mode: COMMON-LISP; Package: COMMON-LISP-USER; Base: 10 -*-   ;;
;;-------------------------------------------------------------------;;
;;            Copyright 1993 Russell G. Almond                       ;;
;;-------------------------------------------------------------------;;
;; This code is in the Public Domain.  Anyone who can get some use   ;;
;; from it is welcome.                                               ;;
;; This code comes with no warranty.                                 ;;
;;-------------------------------------------------------------------;;

;;; $Id$

;;; Garnet 3.3 --- FMG Sorted out code in this directory, integrated
;;                 into Garnet build system.
;;  10/2/03 RGA --- Moved to new home.
;;  Created 6/25/92 RGA


;;; protected-eval exports the following interfaces:
;;
;; (Described in prompter.doc)
;;
;; Protected-Eval-Error-Gadget 
;; Garnet-Error-Handler Garnet-User-Error-Handler
;; With-Garnet-Error-Handling With-Garnet-User-Error-Handling
;; With-Abort
;; Garnet-Protected-Eval Garnet-Protected-Read-From-String
;;
;; Other exported symbols:
;;
;; Error-Prompter-Gadget
;; Garnet-Protected-Read Do-Prompt
;; With-Normal-Cursor *normal-cursor-pair*
;; Prompting-Error-Handler
;; Do-Abort
;; *user-type*
;;
;; In prompter.lisp:
;;
;; Prompter-Gadget
;; Display-Prompt Display-Prompt-And-Wait
;;
;; In abstract-errors.lisp / garnet-errors.lisp:
;;
;; prompting-protected-eval 
;; prompting-protected-read prompting-protected-read-from-string 
;; prompter
;; protect-errors with-protected-errors 
;; protected-eval
;; protected-read protected-read-from-string
;; call-prompter
;; displayer call-displayer 
;; selector call-selector
;; *application-long-name* *application-short-name*
;; *user-type*



;;; Loader for protected eval stuff.

(in-package "COMMON-LISP-USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Protected-Eval-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Protected-Eval-PathName
before loading Protected-Eval."))

(defvar protected-eval-load-files
  '(;; Load these from the gadgets directory.
    "gadgets:motif-error-gadget-loader"
    "gadgets:motif-scrolling-labeled-box-loader"
    ;; Load these from the protected-eval directory.
    "protected-eval:error"
    "protected-eval:prompter"
    "protected-eval:protected-eval"
    "protected-eval:protected-process"
    "protected-eval:abstract-errors"
    "protected-eval:garnet-errors"))


(unless (get :garnet-modules :protected-eval)
  (format t "Loading Protected-eval~%")
  
  (dolist (file protected-eval-load-files)
    (garnet-load file))
  
  (format t "...Done Protected-Eval.~%")

  (setf (get :garnet-modules :protected-eval) t))




