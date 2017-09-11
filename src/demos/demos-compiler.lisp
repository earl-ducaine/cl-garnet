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
;;; $Id::                                                             $
(in-package "COMMON-LISP-USER")

(defvar *debug-demos-mode* nil)



(unless (get :garnet-modules :multifont)
  (load (merge-pathnames "multifont-loader" Garnet-Opal-PathName)))
(unless (get :garnet-modules :aggregraphs)
  (load Garnet-Aggregraphs-Loader))
(unless (get :garnet-modules :gadgets)
  (load Garnet-Gadgets-Loader))
(unless (get :garnet-modules :ps)
  (load Garnet-PS-Loader))
(unless (get :garnet-modules :gesture)
  (load Garnet-Gesture-Loader))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (garnet-mkdir-if-needed Garnet-demos-Pathname))

(defvar Garnet-Demos-Files              ; defvar rather than defparameter so can setq
					; this variable before loading if only want
					; to compile some of these files
  '(
    ))

(dolist (file Garnet-Demos-Files)
  (let ((gfile (concatenate 'string "demos:" file)))
    (garnet-compile gfile)))

;; (garnet-copy-files Garnet-Demos-Src Garnet-Demos-Pathname
;; 		   '("demos-loader.lisp"))
