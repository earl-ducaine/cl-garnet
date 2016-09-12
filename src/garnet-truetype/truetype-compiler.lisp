;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$

(in-package "COMMON-LISP-USER")


(defvar *debug-truetype-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-truetype-mode*
       (and (boundp '*garnet-compile-debug-settings*)
	    *garnet-compile-debug-settings*)
       ;; Global default settings.
       (and (boundp '*default-garnet-proclaim*) 
	    *default-garnet-proclaim*))))


(format t "Loading Truetype Dependencies...~%")

(defvar *truetype-dependencies*
  '(:zpb-ttf :cl-vectors :cl-paths-ttf :cl-aa :cl-fad :cl-store :trivial-features))

(dolist (dep *truetype-dependencies*)
  (require dep))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-Truetype-Pathname))

(Defparameter Garnet-Truetype-Files
  '(
    "package-truetype"
    "clx-truetype-utils"
    "font-cache"
    "clx-truetype"
    ))

(dolist (file Garnet-Truetype-Files)
  (let ((gfile (concatenate 'string "truetype:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Truetype-Src Garnet-Truetype-Pathname
		   '("truetype-loader.lisp"))

(setf (get :garnet-modules :truetype) T)
