;;; -*- Mode: Lisp, Fill, Save; Package: COMMON-LISP-USER -*-
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id::                                                             $
;;


;;; This file prepares to compile all the garnet modules.  Of course, you
;;  need to have write priviledges on all the directories where the files
;;  are stored.  (These directories are set in garnet-loader).
;;  
;;  First load this file: 	garnet-prepare-compile
;;  Then load 			garnet-loader
;;  Then load 			garnet-compiler
;; 
;;  The result will be that all the files will be compiled and loaded (the
;;  initial files need to be loaded before later files can be compiled
;;  anyway).  Note that this process does *NOT* check for compile errors,
;;  that is up to you.
;; 
;;  ** To prevent certain parts from being compiled, first set
;;       user::compile-XX-p to NIL.  To compile lapidary, set
;;  	 compile-lapidary-p to T (the default is not to compile it)
;;  ** To have the demos or lapidary be loaded after they are
;;  	 compiled, set user::load-demos-p and user::load-lapidary-p
;;  	 to T (the default is to NOT load these after compiling them
;;  ** To override where something is loaded from, set Garnet-xx-PathName before
;;       loading this file.
;; 
;;  The controlling variables are:
;;  
;;   compile-utils-p        (Default: T   => utils compiled and loaded)
;;   compile-kr-p           (Default: T   => kr compiled and loaded)
;;   compile-kr-doc-p       (Default: NIL => kr-doc compiled and loaded)
;;   compile-gem-p          (Default: T   => gem compiled and loaded)
;;   compile-opal-p         (Default: T   => opal compiled and loaded)
;;   compile-truetype-p     (Default: T   => truetype compiled and loaded)
;;   compile-inter-p        (Default: T   => interactors compiled and loaded)
;;   compile-gesture-p      (Default: T   => gestures compiled and loaded)
;;   compile-ps-p           (Default: T   => PS module compiled and loaded)
;;   compile-aggregadgets-p (Default: T   => aggregadgets compiled & loaded)
;;   compile-gadgets-p      (Default: T   => gadgets compiled and loaded)
;;   compile-debug-p        (Default: T   => debug compiled and loaded)
;;   compile-demos-p        (Default: T   => demos compiled but *not* loaded)
;;   compile-lapidary-p     (Default: T   => lapidary compiled and loaded)
;;   compile-gilt-p         (Default: T   => gilt compiled and loaded)
;;   compile-c32-p          (Default: T   => C32 compiled and loaded)
;;   compile-protected-eval-p (Default: T => protected-eval compiled
;;                                           and loaded)
;; 
;;  To override any particular file name place, it is only necessary to
;;  assign the variable name Garnet-XX-Pathname before this file is loaded
;;  (since they are defined here using defvar, the old name will stay in affect). 
;; 


(in-package :COMMON-LISP-USER)

(defvar compile-utils-p T)
(defvar compile-kr-p T)
(defvar compile-kr-doc-p NIL)
(defvar compile-gworld-p T)
(defvar compile-gem-p T)
(defvar compile-opal-p T)
#-(and)(defvar compile-truetype-p t)
(defvar compile-inter-p T)
(defvar compile-gesture-p T)
(defvar compile-ps-p T)
(defvar compile-aggregadgets-p T)
(defvar compile-gadgets-p T)
(defvar compile-debug-p T)
(defvar compile-demos-p T)
(defvar compile-lapidary-p T)
(defvar compile-gilt-p T)
(defvar compile-c32-p T)
(defvar compile-protected-eval-p T)


;;; first, don't load anything, just load garnet-loader to set up file names
(defvar load-utils-p NIL)
(defvar load-kr-p NIL)
(defvar load-kr-doc-p NIL)
(defvar load-gworld-p NIL)
(defvar load-gem-p NIL)
(defvar load-opal-p NIL)
#-(and)(defvar load-truetype-p NIL)
(defvar load-inter-p NIL)
(defvar load-multifont-p NIL)
(defvar load-gesture-p NIL)
(defvar load-ps-p NIL)
(defvar load-aggregadgets-p NIL)
(defvar load-aggregraphs-p NIL)
(defvar load-gadgets-p NIL)
(defvar load-debug-p NIL)
(defvar load-demos-p NIL)
(defvar load-lapidary-p NIL)
(defvar load-gilt-p NIL)
(defvar load-c32-p NIL)
(defvar load-protected-eval-p NIL)


(defparameter load-utils-p-copy (if (boundp 'load-utils-p)
				    load-utils-p T))
(defparameter load-kr-p-copy (if (boundp 'load-kr-p)
				 load-kr-p T))
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
(defparameter load-lapidary-p-copy (if (boundp 'load-lapidary-p)
				       load-lapidary-p NIL))
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
