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

;;; This file  compiles all the garnet modules.
;;; First load the file: 	garnet-prepare-compile
;;; Then load 			garnet-loader
;;; Then load this file:	garnet-compiler
;;;
;;; ** See the comments at the top of garnet-prepare-compile
;;;
;;;

#|
============================================================
Change log:
         10/05/03 Russell Almond - Changed to use
                  garnet-compile/garnet-load so we can apply MCL
                  #\linefeed bug workaround.
         10/02/03 Russell Almond - Added Kr-Doc
         10/02/03 Russell Almond - Added Protected Eval
         03/16/94 Andrew Mickish - Added Gworld module for Mac
         11/01/93 Andrew Mickish - Added GEM
         10/14/93 Andrew Mickish - Removed Lucid compiler proclamation
         05/12/93 Dave Kosbie - Moved gadgets before debug
         04/15/93 Andrew Mickish - Added lucid memory-management instruction
         04/05/93 Dave Kosbie - Added Garnet-Utils
         07/23/92 Dario Giuse - Moved C32 before Lapidary
         06/24/92 Andrew Mickish - Added C32
	 04/02/92 McDaniel - new multifont
         02/20/92 Andrew Mickish - Added gestures
         03/11/91 Andrew Mickish - Added aggregraphs
         12/5/89 Brad Myers - Fixed so works when files are in /src directories
         10/30/89 Brad Myers - Added debug, changed names
         8/18/89 Brad Myers - Added Gadgets
         6/21/89 Brad Myers - Created
============================================================
|#

(in-package "COMMON-LISP-USER")

(unless (and (boundp 'load-utils-p-copy)        (boundp 'Garnet-Utils-Src)
             (boundp 'load-kr-p-copy)           (boundp 'Garnet-KR-Src)
	     (boundp 'load-gworld-p-copy)       (boundp 'Garnet-Gworld-Src)
	     (boundp 'load-gem-p-copy)          (boundp 'Garnet-Gem-Src)
	     (boundp 'load-opal-p-copy)         (boundp 'Garnet-Opal-Src)
	     (boundp 'load-truetype-p-copy)     (boundp 'Garnet-Truetype-Src)
	     (boundp 'load-inter-p-copy)        (boundp 'Garnet-Inter-Src)
	     (boundp 'load-multifont-p-copy)
	     (boundp 'load-ps-p-copy)           (boundp 'Garnet-PS-Src)
	     (boundp 'load-aggregadgets-p-copy) (boundp 'Garnet-Aggregadgets-Src)
	     (boundp 'load-aggregraphs-p-copy)  
	     (boundp 'load-debug-p-copy)        (boundp 'Garnet-Debug-Src)
	     (boundp 'load-gadgets-p-copy)      (boundp 'Garnet-Gadgets-Src)
	     (boundp 'load-gesture-p-copy)      (boundp 'Garnet-Gesture-Src)
	     (boundp 'load-demos-p-copy)        (boundp 'Garnet-Demos-Src)
	     (boundp 'load-C32-p-copy)          (boundp 'Garnet-C32-Src)
	     (boundp 'load-lapidary-p-copy)     (boundp 'Garnet-Lapidary-Src)
	     (boundp 'load-gilt-p-copy)         (boundp 'Garnet-Gilt-Src)
	     (boundp 'load-protected-eval-p-copy)    (boundp 'Garnet-protected-eval-Src)
	     )
  (error "** Must load Garnet-Prepare-Compile and Garnet-Loader before
  loading this file"))

;;; RGA create bin directory if needed.
(when Multiple-Garnet-Bin-Dirs
  (garnet-mkdir-if-needed Garnet-Binary-Pathname))

(when compile-utils-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Utils %%%%%%%%%%%%%%% ~%")
  (garnet-load "utils-src:utils-compiler"))
(unless compile-utils-p
  (load Garnet-Utils-Loader))


(when compile-kr-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling KR %%%%%%%%%%%%%%% ~%")
  (garnet-load "kr-src:kr-compiler"))
(unless compile-kr-p
  (load Garnet-KR-Loader))

(when compile-kr-doc-p
  (garnet-compile "kr:kr-doc")
  (garnet-load "kr:kr-doc"))

(when compile-gem-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gem %%%%%%%%%%%%%%% ~%")
  (garnet-load "gem-src:gem-compiler"))
(unless compile-gem-p
  (load Garnet-Gem-Loader))

(when compile-opal-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Opal %%%%%%%%%%%%%%% ~%")
  (garnet-load "opal-src:opal-compiler"))
(unless compile-opal-p
  (load Garnet-Opal-Loader))

#-(or allegro CMU)
(when compile-truetype-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Truetype %%%%%%%%%%%%%%% ~%")
  (garnet-load "truetype-src:truetype-compiler"))
#-(or allegro CMU)
(unless compile-truetype-p
  (load Garnet-Truetype-Loader))

(when compile-inter-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Inter %%%%%%%%%%%%%%% ~%")
  (garnet-load "inter-src:inter-compiler"))
(unless compile-inter-p
  (load Garnet-Inter-Loader))  ; have to load this to go on


(when compile-PS-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling PS %%%%%%%%%%%%%%% ~%")
  (garnet-load "ps-src:ps-compiler"))
(unless compile-PS-p
  (load Garnet-PS-Loader))  ; have to load this to go on


(when compile-aggregadgets-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Aggregadgets %%%%%%%%%%%%%%% ~%")
  (garnet-load "aggregadgets-src:aggregadgets-compiler"))
(when (or load-aggregadgets-p-copy compile-demos-p
	     compile-lapidary-p compile-gadgets-p)
  (unless compile-aggregadgets-p
    (load Garnet-Aggregadgets-Loader)))  ; need this if compile demos, gadgets,
				         ; or lapidary


(when compile-gadgets-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gadgets %%%%%%%%%%%%%%% ~%")
  (garnet-load "gadgets-src:gadgets-compiler"))
(when (or load-gadgets-p-copy compile-demos-p compile-lapidary-p)
  (unless compile-gadgets-p
    (load Garnet-Gadgets-Loader)))


(when compile-debug-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Debugging Routines %%%%%%%%%%%%% ~%")
  (garnet-load "debug-src:debug-compiler"))
(when load-debug-p-copy
  (unless compile-debug-p
    (load Garnet-Debug-Loader)))


(when compile-gesture-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gestures %%%%%%%%%%%%%%% ~%")
  (garnet-load "gesture-src:gesture-compiler"))
(unless compile-gesture-p
  (load Garnet-Gesture-Loader))  ; have to load this to go on


(when compile-demos-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Demos %%%%%%%%%%%%%%% ~%")
  (garnet-load "demos-src:demos-compiler"))
(when load-demos-p-copy
  (unless compile-demos-p
    (load Garnet-Demos-Loader)))


(when compile-gilt-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Gilt %%%%%%%%%%%%%%% ~%")
  (garnet-load "gilt-src:gilt-compiler"))
(when load-gilt-p-copy
  (unless compile-gilt-p
    (load Garnet-Gilt-Loader)))


(when compile-C32-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling C32 %%%%%%%%%%%%%%% ~%")
  (garnet-load "c32-src:c32-compiler"))
(when load-C32-p-copy
  (unless compile-C32-p
    (load Garnet-C32-Loader)))


(when compile-lapidary-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Lapidary %%%%%%%%%%%%%%% ~%")
  (garnet-load "lapidary-src:lapidary-compiler"))
(when load-lapidary-p-copy
  (unless compile-lapidary-p
    (load Garnet-Lapidary-Loader)))

(when compile-protected-eval-p
  (format T "~%  %%%%%%%%%%%%%%  Compiling Protected-Eval %%%%%%%%%%%%%%% ~%")
  (garnet-load "protected-eval-src:protected-eval-compiler"))
(when load-protected-eval-p-copy
  (unless compile-protected-eval-p
    (load Garnet-protected-eval-Loader)))


(setf *Garnet-Going-To-Compile* NIL)  ; no longer in compile mode
