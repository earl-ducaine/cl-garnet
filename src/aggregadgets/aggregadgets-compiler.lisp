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

(defvar *debug-aggregadgets-mode* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim
   (if *debug-aggregadgets-mode*
       (and (boundp 'Garnet-Compile-Debug-Settings)
	    Garnet-Compile-Debug-Settings)
       ;; Global default settings.
       (and (boundp 'Default-Garnet-Proclaim) 
	    Default-Garnet-Proclaim))))


(Defvar Garnet-Aggregadgets-Files
  '(
    "agg-macros"
    "agg-utils"
    "aggregadgets"
    "aggrelists"
    "add-agg"
    "agg-fix-slots"
    "copy-agg"
    "save-agg"
    "string-edit"
    "agg-labels"
    "rectangle-conflict-object"
    "aggregraphs"
    "scalable-aggregraph"
    "scalable-aggregraph-image"
    ))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-Aggregadgets-Pathname))

(dolist (file Garnet-Aggregadgets-Files)
  (let ((gfile (concatenate 'string "aggregadgets:" file)))
    (garnet-compile gfile)
    (garnet-load gfile)))

(garnet-copy-files Garnet-Aggregadgets-Src Garnet-Aggregadgets-Pathname
		   '("aggregadgets-loader.lisp"
		     "aggregraphs-loader.lisp"))

(setf (get :garnet-modules :aggregadgets) t)
(setf (get :garnet-modules :aggregraphs) t)
