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

(defvar *debug-gem-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-gem-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))


(Defvar Garnet-Gem-Files
  '(
    "gem"
    "define-methods"
    "x"
    ))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-gem-Pathname))

(dolist (file Garnet-Gem-Files)
  (let ((gfile (concatenate 'string "gem:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Gem-Src Garnet-Gem-Pathname
		   '("gem-loader.lisp"))

(setf (get :garnet-modules :gem) T)
