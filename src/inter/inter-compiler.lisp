;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; Changes:
;;  10/2/03 RGA --- New compile/load protocol
;;       7/28/96 RGA --- changed to use garnet-compile/load
;;  26-May-93 Mickish/Goldberg  Added lispkeyhandling
;;  24-Feb-93 Mickish   Make sure multifont is loaded before compiling;
;;                      note that multifont has been loaded at end.
;;   5-Feb-93 Mickish   Added load of multifont
;;   4-Jun-92 Myers     New animation interactor
;;  29-May-92 Pervin    New animation-process
;;  19-May-92 Pervin    Removed call to install-new-char-reader.
;;   2-Apr-92 McDaniel  New multifont.
;;  20-Jan-92 Mickish   Moved make-package call into Garnet-Loader
;;  18-Jun-91 Pervin	Added multifont-textinter.
;;  26-Mar-91 Pervin    Changes for kcl.
;;  26-Mar-91 Pervin    Load compiled files in Lucid.
;;  22-Mar-91 Pervin    Added #-cmu setf and provide at end.
;;  12-Apr-90 Mitchell  Added #+allegro (gc t)
;;  3/22/90 Robert Cook Define the "INTERACTORS" package for the
;; 			TI Explorer


(in-package "COMMON-LISP-USER")

(defvar *debug-inter-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-inter-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))



;; Only loads this file when not compiling all of Garnet.
(unless (or (and (boundp 'compile-opal-p) compile-opal-p)
            (boundp 'opal::multifont-text))
  (load (merge-pathnames "multifont" Garnet-Opal-PathName)))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-Inter-Pathname))


;;; defvar rather than defparameter so can setq
;;  this variable before loading if only want
;;  to compile some of these files
(Defvar Garnet-Inter-Files

  '(
    ;; key translation files
    "garnet-keytrans"
    "define-mouse-keys"
    "x-define-keys"
    "x-inter"

    ;; interactor files
    "interactors"
    "accelerators"
    "animation-process"
    "i-windows"
    "menuinter"
    "movegrowinter"
    "buttoninter"
    "twopointinter"
    "textkeyhandling"
    "lispkeyhandling"
    "textinter"
    "multifont-textinter"
    "focus-multifont-textinter"
    "selection-interactor"
    "angleinter"
    "animatorinter"))

(dolist (file Garnet-Inter-Files)
  (let ((gfile (concatenate 'string "inter:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Inter-Src Garnet-Inter-Pathname
		   '("inter-loader.lisp"))

(setf (get :garnet-modules :inter) t)
(setf (get :garnet-modules :multifont) t)

