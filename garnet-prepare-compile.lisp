;;; -*- Mode: Lisp, Fill, Save; Package: COMMON-LISP-USER -*-
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;; Legacy compilation stuff.  Needs to be cleaned up.

(defvar Garnet-Garnet-Debug t)

(in-package :COMMON-LISP-USER)

(defvar compile-opal-p T)
#-(and)(defvar compile-truetype-p t)
(defvar compile-inter-p T)
(defvar compile-gesture-p T)
(defvar compile-ps-p T)
(defvar compile-aggregadgets-p T)
(defvar compile-gadgets-p T)
(defvar compile-debug-p T)
(defvar compile-demos-p T)
(defvar compile-gilt-p T)
(defvar compile-c32-p T)
(defvar compile-protected-eval-p T)


;;; first, don't load anything, just load garnet-loader to set up file names
(defvar load-utils-p NIL)
(defvar load-gworld-p NIL)
(defvar load-gem-p NIL)
(defvar load-opal-p NIL)
(defvar load-inter-p NIL)
(defvar load-multifont-p NIL)
(defvar load-gesture-p NIL)
(defvar load-ps-p NIL)
(defvar load-aggregadgets-p NIL)
(defvar load-aggregraphs-p NIL)
(defvar load-gadgets-p NIL)
(defvar load-debug-p NIL)
(defvar load-demos-p NIL)
(defvar load-gilt-p NIL)
(defvar load-c32-p NIL)
(defvar load-protected-eval-p NIL)


(defparameter load-utils-p-copy (if (boundp 'load-utils-p)
				    load-utils-p T))
(defparameter load-gworld-p-copy (if (boundp 'load-gworld-p)
				     load-gworld-p T))
(defparameter load-gem-p-copy (if (boundp 'load-gem-p)
				  load-gem-p T))
(defparameter load-opal-p-copy (if (boundp 'load-opal-p)
				   load-opal-p T))
#-(and)
(defparameter load-truetype-p-copy (if (boundp 'load-truetype-p)
				       load-truetype-p T))
(defparameter load-inter-p-copy (if (boundp 'load-inter-p)
				    load-inter-p T))
(defparameter load-multifont-p-copy (if (boundp 'load-multifont-p)
					load-multifont-p T))
(defparameter load-gesture-p-copy (if (boundp 'load-inter-p)
				      load-gesture-p T))
(defparameter load-ps-p-copy (if (boundp 'load-ps-p)
				 load-ps-p T))
(defparameter load-aggregadgets-p-copy (if (boundp 'load-aggregadgets-p)
					   load-aggregadgets-p T))
(defparameter load-aggregraphs-p-copy (if (boundp 'load-aggregraphs-p)
					  load-aggregraphs-p T))
(defparameter load-gadgets-p-copy (if (boundp 'load-gadgets-p)
				      load-gadgets-p T))
(defparameter load-debug-p-copy (if (boundp 'load-debug-p)
				    load-debug-p T))

(defparameter load-demos-p-copy (if (boundp 'load-demos-p)
				    load-demos-p NIL))

(defparameter load-gilt-p-copy (if (boundp 'load-gilt-p)
				   load-gilt-p NIL))
(defparameter load-c32-p-copy (if (boundp 'load-c32-p)
				  load-c32-p NIL))
(defparameter load-protected-eval-p-copy (if (boundp 'load-protected-eval-p)
					     load-protected-eval-p NIL))


;; tell garnet-loader to not launch the main event loop process.
(defvar launch-process-p NIL)

;; tell garnet-loader to load the binaries from the same place as the
;; source files.

(defvar *Garnet-Going-To-Compile* T)

(format T "** Now load garnet-loader, and then load garnet-compiler~%")
