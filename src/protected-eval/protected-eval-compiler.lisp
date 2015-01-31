;;; -*- Mode: COMMON-LISP; Package: COMMON-LISP-USER;  -*-           ;;
;;-------------------------------------------------------------------;;
;;            Copyright 1993 Russell G. Almond                       ;;
;;-------------------------------------------------------------------;;
;; This code is in the Public Domain.  Anyone who can get some use   ;;
;; from it is welcome.                                               ;;
;; This code comes with no warranty.                                 ;;
;;-------------------------------------------------------------------;;

;;; $Id$

;;; RGA  Compile script for prompter/protected-eval/etc.
;;       

;;  Version 3.3 --- FMG sorted out the code in this directory,
;;                  integrated it into the Garnet build system.
;;  10/02/03 RGA --- New compile/load protocol
;;  10/02/03 RGA Moved to protected-eval directory.
;;  10/02/03 RGA Moved to scrolling-unlabeled-box to gadgets.
;;  08/31/93 RGA Added search path stuff for CMU lisp.

(in-package :common-lisp-user)

(defvar *debug-protected-eval-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-protected-eval-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))


(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-Protected-Eval-Pathname))

(defvar Protected-Eval-Compile-Files
  '(
    "error"
    "prompter"
    "protected-eval"
    "protected-process"
    "abstract-errors"
    "garnet-errors"))

(with-compilation-unit ()
  (dolist (file Protected-Eval-Compile-Files)
    (let ((peval-str (concatenate 'string "protected-eval:" file)))
      (garnet-compile peval-str)
      (garnet-load peval-str))))


(garnet-copy-files Garnet-Protected-Eval-Src Garnet-Protected-Eval-Pathname
		   '("protected-eval-loader.lisp"))


(progn
  (setf (get :garnet-modules :protected-eval) t)
  )
