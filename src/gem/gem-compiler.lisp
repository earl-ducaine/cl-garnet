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

;;; Changes:
;;; 10/2/03 RGA --- New compile/load protocol
;;;        7/28/96 RGA --- changed to use garnet-compile/load
;;;  1-Nov-93 Mickish - Created


(in-package "COMMON-LISP-USER")

(defvar *debug-gem-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-gem-mode*
       (and (boundp '*garnet-compile-debug-settings*)
	    *garnet-compile-debug-settings*)
       ;; Global default settings.
       (and (boundp '*default-garnet-proclaim*) 
	    *default-garnet-proclaim*))))


(Defvar Garnet-Gem-Files
  '(
    "gem"
    "define-methods"
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
