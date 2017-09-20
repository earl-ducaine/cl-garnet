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


(in-package "COMMON-LISP-USER")

(defvar *debug-gilt-mode* nil)

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (proclaim
;;    (if *debug-gilt-mode*
;;        (and (boundp 'Garnet-Compile-Debug-Settings)
;; 	    Garnet-Compile-Debug-Settings)
;;        ;; Global default settings.
;;        (and (boundp 'Default-Garnet-Proclaim)
;; 	    Default-Garnet-Proclaim))))



(format t "Compiling Gilt...~%")

;; check first to see if pathname variable is set
(unless (boundp 'Garnet-Gilt-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gilt-PathName before loading this file."))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (garnet-mkdir-if-needed Garnet-gilt-Pathname))

(defvar gilt-files
  '(
    ;; "gilt-functions"
    ;; "path-functions"
    ;; "filter-functions"
    ;; "gilt-font-imp"
    ;; "motif-gilt-font-props"
    ;; "gilt-gadget-utils"
    ;; "gilt-gadgets"
    ;; "motif-gilt-gadgets"
    ;; "gilt"
    ;; "motif-gilt-save"
    ;; "motif-gilt-read"
    ;; "color-imp"
    ;; "motif-color-props"
    ;; "line-imp"
    ;; "motif-line-props"
    ;; "fill-imp"
    ;; "motif-fill-props"
    ;; "align"
    ;; "value-control"
    ;; "enable-control"
    ;; "error-check"
    ))

(dolist (file gilt-files)
  (let ((gilt-str (concatenate 'string "gilt:" file)))
    (garnet-compile gilt-str)
    (garnet-load gilt-str))
  #+allegroV3.1(common-lisp-user::gc t))

;; (garnet-copy-files Garnet-Gilt-Src Garnet-Gilt-Pathname
;; 		   '("filter-functions-loader.lisp"
;; 		     "gilt-loader.lisp"
;; 		     "gilt-functions-loader.lisp"
;; 		     "path-functions-loader.lisp"
;; 		     ))


(setf (get :garnet-modules :gilt) t)
(setf (get :garnet-modules :gilt-functions) t)

(format t "... Done Compiling Gilt~%")
