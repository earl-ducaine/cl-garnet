;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-

;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain. If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.

(in-package :common-lisp-user)

(defvar *debug-lapidary-mode* nil)


;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (garnet-load "opal:multifont-loader"))

(unless (get :garnet-modules :debug)
  (garnet-load "debug:debug-loader"))


(unless (get :garnet-modules :gilt-functions)
  (garnet-load "gilt:gilt-functions-loader"))

(unless (get :garnet-modules :path-functions)
  (garnet-load "gilt:path-functions-loader"))

;; (unless (get :garnet-modules :c32)
;;   (load garnet-c32-loader))

;;; Create the Lapidary Directory
(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-Lapidary-Pathname))


;;; Compile and load the constraint gadget

(defvar Garnet-Constraint-Gadget-Pathname Garnet-Lapidary-Pathname)
(defvar Garnet-Constraint-Gadget-Src Garnet-Lapidary-Src)

(defparameter Garnet-Constraint-Gadget-Compiler
  (merge-pathnames "constraint-gadget-compiler"
		    Garnet-Constraint-Gadget-PathName))
