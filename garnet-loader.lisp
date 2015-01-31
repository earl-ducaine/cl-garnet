;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;          The Garnet User Interface Development Environment.      ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at          ;;
;;  Carnegie Mellon University, and has been placed in the public   ;;
;;  domain.  If you are using this code or any part of Garnet,      ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list. ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;
;; $Id::                                                             $

;;; This file loads all the garnet modules.
;;
;; ** To prevent certain parts from being loaded, first set
;;      common-lisp-user::load-XX-p to NIL.
;; ** To get some of the parts which are not loaded by default to be
;;    loaded, set common-lisp-user::load-XX-p to T.
;; ** To override where something is loaded from, set
;;    Garnet-xx-PathName before loading this file and/or Garnet-xx-src
;;
;; The controlling variables are:
;;
;;      load-clx-p          (Default: NIL => clx not loaded)
;;      load-utils-p        (Default: T   => utilities loaded)
;;      load-kr-p           (Default: T   => kr loaded)
;;      load-kr-doc-p       (Default: NIL => kr-doc *NOT* loaded)
;;      load-gworld-p       (Default: T   => gworld loaded for Mac)
;;      load-gem-p          (Default: T   => gem loaded)
;;      load-opal-p         (Default: T   => opal loaded)
;;      load-inter-p        (Default: T   => interactors loaded)
;;      load-truetype-p     (default: T   => truetype support loaded)
;;      load-multifont-p    (Default: NIL => multifont *NOT* loaded)
;;      load-gesture-p      (Default: NIL => gestures *NOT* loaded)
;;      load-ps-p           (Default: T   => ps loaded)
;;      load-aggregadgets-p (Default: T   => aggregadgets loaded)
;;      load-aggregraphs-p  (Default: NIL => aggregraphs *NOT* loaded)
;;      load-gadgets-p      (Default: NIL => gadgets *NOT* loaded)
;;      load-debug-p        (Default: T   => debugging tools loaded)
;;      load-demos-p        (Default: NIL => demos *NOT* loaded)
;;      load-c32-p          (Default: NIL => C32 *NOT* loaded)
;;      load-gilt-p         (Default: NIL => gilt *NOT* loaded)
;;      load-lapidary-p     (Default: NIL => lapidary *NOT* loaded)
;;      load-protected-eval-p (Default: <lisp dependent> =>
;;                            protected-eval loaded for lisps with
;;                            multi-processor support.
;;
;; Part of this file lists the file names where the various parts of
;; Garnet come from. While the system attempts to determine the
;; appropriate information automatically, it may need to be entered
;; manually.
;;
;; To override any particular file name place, it is only necessary to
;; assign the variable name Garnet-XX-Pathname before this file is loaded
;; (since they are defined here using defvar, the old name will stay in
;; affect).
;;
;;


(in-package :COMMON-LISP-USER)

(defparameter Garnet-Version-Number "3.3")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*)
(setf *features* (delete :GARNET-V3.0 *features*))
(pushnew :GARNET-V3.3 *features*)


;; The :GARNET-PROCESSES keyword goes on the *features* list if this
;; version of lisp supports multiple processes. Then things like the
;; animation interactor can use the #+garnet-processes switch, instead
;; of referring explicitly to different versions of lisp.
;;
;; In reality, process code still needs to be conditionalized
;; per-implementation.
(pushnew :GARNET-PROCESSES *features*)

#+allegro
(defvar Garnet-Readtable *readtable*
  "This variable is used by Allegro to restore the old value of the
*readtable* when a saved image is restarted (see opal:make-image in
opal/utils.lisp).")


;;; The following variables are used for customizing various aspects
;;  of building to allow debugging or build faster systems.
;;

;; The :GARNET-DEBUG feature allows many different kinds of run-time
;; checking, and also loads some extra test code. After you have
;; debugged your code and want it to run faster, remove :GARNET-DEBUG
;; from the *features* list and RECOMPILE all of Garnet and your code.
;; The result will be smaller and somewhat faster.
;;
;; To remove :GARNET-DEBUG from the *features* list, either defvar
;; Garnet-Garnet-Debug to NIL before you load the garnet-loader, or
;; simply edit the following defvar to set Garnet-Garnet-Debug to nil.
(defvar Garnet-Garnet-Debug t)
(if Garnet-Garnet-Debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))


;; The following variable affects compiler policy. Setting it to T
;; uses the settings in *garnet-compile-debug-settings*. Setting it to
;; NIL uses the ones in *garnet-compile-production-settings*. By
;; default we simply mirror Garnet-Garnet-Debug.
(defvar Garnet-Compile-Debug-Mode Garnet-Garnet-Debug
  "Setting this variable to T sets the policy for the entire system
to make it more debuggable.")

(defvar Garnet-Compile-Debug-Settings
  '(optimize (speed 2) (safety 3) (debug 3) 
    #-ccl (space 2.5) #+ccl (space 2)
    #+ccl (compilation-speed 3))
  "Use these settings for globally debugging the system or for debugging
a specific module. They emphasize debuggability at the cost of some speed.

With SBCL:

- These settings are type-safe. 

- They prevent functions declared inline from being expanded inline. 
  Note that as part of this version I have tried to make most 
  non-syntactic macros into inline functions.

- They allow all possible debugging features.")

(defvar Garnet-Compile-Production-Settings
  '(optimize (speed 3) 
    (safety 1)
    (space 1)
    #-cmu (debug 1) #+cmu (debug 0.5)
    (compilation-speed 0)
    #+cmu (ext:inhibit-warnings 3))
  "Production compiler policy settings. Emphasize speed, de-emphasize debugging.")
#+(and sbcl (not garnet-debug))
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

  
(defvar Default-Garnet-Proclaim
  (if Garnet-Compile-Debug-Mode
      Garnet-Compile-Debug-Settings
      Garnet-Compile-Production-Settings)
  "Set compiler optimization settings.

1. If you want everything debugged, set Garnet-Compile-Debug-Mode to t.

2. If you want to debug specific modules, set Garnet-Compile-Debug-Mode
   to nil. Then set the variable in the modules you want debugged to enable
   debugging that module.

3. Otherwise (for 'production' builds) just set Garnet-Compile-Debug-Mode 
   to nil and leave everything else alone.")


 (when Default-Garnet-Proclaim
   (proclaim Default-Garnet-Proclaim))

 
 ;;; Defpackages.
 (progn
   (defpackage :GARNET-UTILS (:use :COMMON-LISP) (:nicknames :GU))
   (defpackage :KR-DEBUG (:use :COMMON-LISP))
   (defpackage :KR (:use :COMMON-LISP :KR-DEBUG))
   (defpackage :GEM (:use :COMMON-LISP :KR :KR-DEBUG))
   (defpackage :OPAL (:use :COMMON-LISP :KR))
   (defpackage :INTERACTORS (:use :COMMON-LISP :KR) (:nicknames :INTER)
     (:export *GARNET-BREAK-KEY* *LEFT-BUTTON* *TRANS-FROM-FILE*))
   (defpackage :GARNET-GADGETS (:use :COMMON-LISP :KR) (:nicknames :GG))
   (defpackage :GARNET-DEBUG (:use :COMMON-LISP :KR :OPAL) (:nicknames :GD))
   (defpackage :GILT (:use :COMMON-LISP :KR))
   (defpackage :C32 (:use :COMMON-LISP :KR))
   (defpackage :LAPIDARY (:use :COMMON-LISP :KR))
   (defpackage :AGATE (:use :COMMON-LISP :KR))

   (defpackage :DEMO-3D (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-MULTIWIN (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-MULTIFONT (:use :COMMON-LISP KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-ANIMATOR (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-ANGLE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-OTHELLO (:use :KR :COMMON-LISP) (:nicknames :DOTH)
     (:export DO-GO DO-STOP START-GAME STOP-GAME SET-SCORE))
   (defpackage :DEMO-PIXMAP (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-ARITH (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-SCHEMA-BROWSER (:use :COMMON-LISP :KR)
     (:export DO-GO DO-STOP SCHEMA-BROWSER SCHEMA-BROWSER-WIN
	      SCHEMA-BROWSER-TOP-AGG))
   (defpackage :DEMO-ARRAY (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-SCROLLBAR (:use :COMMON-LISP :KR)
     (:export DO-GO DO-STOP
	      MAC-obj MAC-Go MAC-Stop
	      Open-obj Open-Go Open-Stop
	      NEXT-obj NEXT-Go NEXT-Stop
	      Motif-obj Motif-Go Motif-Stop))
   (defpackage :DEMO-CLOCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-SEQUENCE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-EDITOR (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-TEXT (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-FILE-BROWSER (:use :COMMON-LISP :KR)
     (:export DO-GO DO-STOP FILE-BROWSER FILE-BROWSER-WIN
	      FILE-BROWSER-TOP-AGG))
   (defpackage :DEMO-TRUCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-GADGETS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-TWOP (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-GESTURE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-UNISTROKES (:use :COMMON-LISP :KR :INTER) (:export DO-GO DO-STOP))
   (defpackage :DEMO-GRAPH (:use :COMMON-LISP :KR)
     (:export DO-GO DO-STOP SCHEMA-GRAPH DEMO-GRAPH-ERROR-GADGET ROOT-BOX
	      RELAYOUT DEMO-GRAPH-WIN))
   (defpackage :DEMO-VIRTUAL-AGG (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-GROW (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   (defpackage :DEMO-XASPERATE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-LOGO (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP RE-ANIMATE))
   (defpackage :DEMOS-CONTROLLER (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP MESSAGE))
   (defpackage :DEMO-MANYOBJS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP MOVE))
   (defpackage :DEMO-MENU (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :GARNET-CALCULATOR (:use :COMMON-LISP :KR)
     (:export START-CALC STOP-CALC DO-GO DO-STOP))
   (defpackage :DEMO-MODE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :GARNETDRAW (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-MOTIF (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :MGE (:use :COMMON-LISP :KR)
     (:export DO-GO DO-STOP
	      CREATE-PIECE DESTROY-PIECE DESTROY-ALL-PIECES
	      GO-INITIALIZE EDITOR-SHOW-WINDOW))
   (defpackage :DEMO-MOVELINE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
   )

(use-package '(:kr :kr-debug :garnet-debug))

 
(defparameter *dont-load-modules-twice* t
  "*dont-load-modules-twice* tells whether to re-load modules
if a user loads garnet-loader.lisp a second time.")

;; load-XX-p control whether the various parts are loaded or not
;; Because these use defvar, if they are set before this file is
;; loaded, their original value will be used.
(unless (boundp '*Garnet-Going-To-Compile*)
  (defvar load-utils-p T)
  (defvar load-kr-p T)
  (defvar load-kr-doc-p T)
  (defvar load-gworld-p NIL)
  (defvar load-gem-p T)
  (defvar load-opal-p T)
  (defvar load-inter-p T)
  #-(and)
  (defvar load-truetype-p nil)
  #-(and)
  (defvar load-truetype-p T)
  (defvar load-multifont-p NIL)
  (defvar load-gesture-p NIL)
  (defvar load-ps-p T)
  (defvar load-aggregadgets-p T)
  (defvar load-aggregraphs-p NIL)
  (defvar load-debug-p #+garnet-debug T #-garnet-debug NIL)
  (defvar load-gadgets-p NIL)
  (defvar load-demos-p NIL)
  (defvar load-protected-eval-p T)
  (defvar load-lapidary-p NIL)
  (defvar load-gilt-p NIL)
  (defvar load-c32-p NIL))


 
;; launch-process-p controls whether Garnet will launch
;; a separate process to detect keyboard and mouse events.
(defvar launch-process-p T)

;; update-locking-p controls whether process locks will be activated
;; around the update method (this keeps two processes from calling update
;; at the same time).
(defvar update-locking-p T
  "If T, uses process locks to keep Update in a process from interrupting
   itself in a different process.")

(defvar Multiple-Garnet-Binary-Directories T
  "T if you want the name of the binary directory to vary with the
   version of garnet, handy if you are debugging with multiple Lisp
   versions. NIL means store binaries under the \"bin\" directory.")

(format T "~&** Loading Garnet Version ~a~%" Garnet-Version-Number)

;; The following sets the pathname of the garnet directory. All the
;; rest of the pathnames will depend on this pathname.
;;

;;; CLX
;; Don't configure CLX any more. It should be made available through
;; other means, such as quicklisp. Instead, we (require :clx).
;;
;; Now do this in build script. I.e. could also use quicklisp if
;; desired. If not using the build script, you must put something here
;; that loads CLX.
;; (require :clx)

;;; Garnet Root pathname
;;
(defvar Your-Garnet-Pathname
  ;; Ansi compliant way to find the build directory.
  ;; If this doesn't work for some reason, just hard-code the pathname
  ;; here.
  (namestring
   (make-pathname
    :directory
    ;; Hard coded directory path example.
    ;; (:ABSOLUTE "usr" "local" "lib" "lisp" "garnet")
    ;; Let system determine directory path.
    (pathname-directory *load-truename*)))
  "Root of the Garnet directory tree.")

;; ripped from the bowels of asdf-install... [2006/01/05:rpg]       
(defun directorify (name)
  ;; input name may or may not have a training #\/, but we know we
  ;; want a directory
  (let ((path (pathname name)))
    (if (pathname-name path) 
	;; The pathname lacks a trailing slash.
        (merge-pathnames
         (make-pathname :directory `(:relative ,(pathname-name path)))
         (make-pathname :directory (pathname-directory path)
                        :host (pathname-host path)))
	;; The pathname is OK as it stands.
        path)))


(defun append-directory (directory sub-directory)
  "This is a little utility for accessing the subdirectory of a
directory. It assumes that 'sub-directory' is directly under
'directory'."
  (let ((dir (pathname-directory directory))
	(subdir (if (listp sub-directory) 
		    sub-directory
		    (list sub-directory))))
    (make-pathname :directory (append dir subdir))))


;; the following is just bloody awful, but I won't be able to fix it
;; until version 4 comes along. [2006/01/05:rpg]
(defun Get-Garnet-Binary-Pathname ()
  (let ((directory-name
	 (if Multiple-Garnet-Binary-Directories

	     ;; Per-implementation binary directory
	     ;; Not bin.xxxx but bin-xxxx to avoid any confusion regarding
	     ;; filename types.
	     #+sbcl "bin-sbcl"
	     #+ccl "bin-ccl"
	     #+cmu "bin-cmu"
	     #+allegro "bin-allegro"
	     #-(or sbcl ccl cmu allegro)
	     (error "Garnet doesn't currently work with ~S (ports welcome!)"
		    (lisp-implementation-type))

	     ;; Single binary directory
	     "bin")))
    (append-directory Your-Garnet-Pathname directory-name)))


(defvar Garnet-Src-Pathname (append-directory  Your-Garnet-Pathname "src"))
(defvar Garnet-Lib-Pathname (append-directory Your-Garnet-Pathname "lib"))
(defvar Garnet-Binary-Pathname (Get-Garnet-Binary-Pathname))


;;; Pathnames.
(defvar Garnet-Utils-Src
  (append-directory Garnet-Src-Pathname "utils"))
(defvar Garnet-Utils-Pathname
  (append-directory Garnet-Binary-Pathname "utils"))
(defvar Garnet-KR-Src
  (append-directory Garnet-Src-Pathname "kr"))
(defvar Garnet-KR-Pathname
  (append-directory Garnet-Binary-Pathname "kr"))
(defvar Garnet-Gworld-Src
  (append-directory Garnet-Src-Pathname "gworld"))
(defvar Garnet-Gworld-Pathname
  (append-directory Garnet-Binary-Pathname "gworld"))
(defvar Garnet-Gem-Src
  (append-directory Garnet-Src-Pathname "gem"))
(defvar Garnet-Gem-Pathname
  (append-directory Garnet-Binary-Pathname "gem"))
(defvar Garnet-Opal-Src
  (append-directory Garnet-Src-Pathname "opal"))
(defvar Garnet-Opal-Pathname
  (append-directory Garnet-Binary-Pathname "opal"))
#-(and)
(defvar Garnet-Truetype-Src
  (append-directory Garnet-Src-Pathname "truetype"))
#-(and)
(defvar Garnet-Truetype-Pathname
  (append-directory Garnet-Binary-Pathname "truetype"))
(defvar Garnet-Inter-Src
  (append-directory Garnet-Src-Pathname "inter"))
(defvar Garnet-Inter-Pathname
  (append-directory Garnet-Binary-Pathname "inter"))
(defvar Garnet-Gesture-Src
  (append-directory Garnet-Src-Pathname "gesture"))
(defvar Garnet-Gesture-Pathname
  (append-directory Garnet-Binary-Pathname "gesture"))
(defvar Garnet-Aggregadgets-Src
  (append-directory Garnet-Src-Pathname "aggregadgets"))
(defvar Garnet-Aggregadgets-Pathname
  (append-directory Garnet-Binary-Pathname "aggregadgets"))
(defvar Garnet-PS-Src
  (append-directory Garnet-Src-Pathname "ps"))
(defvar Garnet-PS-Pathname
  (append-directory Garnet-Binary-Pathname "ps"))
(defvar Garnet-Gadgets-Src
  (append-directory Garnet-Src-Pathname "gadgets"))
(defvar Garnet-Gadgets-Pathname
  (append-directory Garnet-Binary-Pathname "gadgets"))
(defvar Garnet-Debug-Src
  (append-directory Garnet-Src-Pathname "debug"))
(defvar Garnet-Debug-Pathname
  (append-directory Garnet-Binary-Pathname "debug"))
(defvar Garnet-Demos-Src
  (append-directory Garnet-Src-Pathname "demos"))
(defvar Garnet-Demos-Pathname
  (append-directory Garnet-Binary-Pathname "demos"))
(defvar Garnet-Gilt-Src
  (append-directory Garnet-Src-Pathname "gilt"))
(defvar Garnet-Gilt-Pathname
  (append-directory Garnet-Binary-Pathname "gilt"))
(defvar Garnet-C32-Src
  (append-directory Garnet-Src-Pathname "c32"))
(defvar Garnet-C32-Pathname
  (append-directory Garnet-Binary-Pathname "c32"))
(defvar Garnet-Lapidary-Src
  (append-directory Garnet-Src-Pathname "lapidary"))
(defvar Garnet-Lapidary-Pathname
  (append-directory Garnet-Binary-Pathname "lapidary"))
(defvar Garnet-Contrib-Src
  (append-directory Garnet-Src-Pathname "contrib"))
(defvar Garnet-Contrib-Pathname
  (append-directory Garnet-Binary-Pathname "contrib"))
(defvar Garnet-Protected-Eval-Src
  (append-directory Garnet-Src-Pathname "protected-eval"))
(defvar Garnet-Protected-Eval-Pathname
  (append-directory Garnet-Binary-Pathname "protected-eval"))

(defvar Garnet-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "bitmaps"))
(defvar Garnet-Pixmap-Pathname
  (append-directory Garnet-Lib-Pathname "pixmaps"))
(defvar Garnet-Gilt-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "gilt"))
(defvar Garnet-C32-Bitmap-Pathname
  (append-directory Garnet-Lib-Pathname "c32"))
(defvar Garnet-DataFile-Pathname
  (append-directory Garnet-Lib-Pathname "data"))
(defvar Garnet-Gesture-Data-Pathname
  (append-directory Garnet-Lib-Pathname "gesture"))


;;;----------------------------------------------------------

;; When compiling, the binaries will be in the same directories as the
;; source files, so make all the path names be the same
;;
;; After compilation is finished, the user should move all the binaries
;; into their own directories, as specified by the pathnames above.
(defvar *Garnet-Going-To-Compile* nil)



;;;----------------------------------------------------------
;; If using cmucl, then set up CMUCL search lists. XXX This sort of
;; thing should disappear; use logical pathnames instead.
;;
;; Even though CMUCL search lists are a cool feature....

#+cmu
(progn
  (setf (ext:search-list "utils:")
	(list (namestring Garnet-Utils-PathName)))
  (setf (ext:search-list "utils-src:")
	(list (namestring Garnet-Utils-Src)))

  (setf (ext:search-list "kr:")
	(list (namestring Garnet-KR-PathName)))
  (setf (ext:search-list "kr-src:")
	(list (namestring Garnet-KR-Src)))

  (setf (ext:search-list "gem:")
	(list (namestring Garnet-Gem-PathName)))
  (setf (ext:search-list "gem-src:")
	(list (namestring Garnet-Gem-Src)))

  (setf (ext:search-list "opal:")
	(list (namestring Garnet-Opal-PathName)))
  (setf (ext:search-list "opal-src:")
	(list (namestring Garnet-Opal-Src)))

  (setf (ext:search-list "inter:")
	(list (namestring Garnet-Inter-PathName)))
  (setf (ext:search-list "inter-src:")
	(list (namestring Garnet-Inter-Src)))

  (setf (ext:search-list "gesture:")
	(list (namestring Garnet-Gesture-PathName)))
  (setf (ext:search-list "gesture-src:")
	(list (namestring Garnet-Gesture-Src)))
  (setf (ext:search-list "gesture-data:")
	(list (namestring Garnet-Gesture-Data-PathName)))

  (setf (ext:search-list "ps:")
	(list (namestring Garnet-PS-PathName)))
  (setf (ext:search-list "ps-src:")
	(list (namestring Garnet-PS-Src)))

  (setf (ext:search-list "aggregadgets:")
	(list (namestring Garnet-Aggregadgets-PathName)))
  (setf (ext:search-list "aggregadgets-src:")
	(list (namestring Garnet-Aggregadgets-Src)))

  (setf (ext:search-list "gadgets:")
	(list (namestring Garnet-Gadgets-PathName)))
  (setf (ext:search-list "gadgets-src:")
	(list (namestring Garnet-Gadgets-Src)))

  (setf (ext:search-list "debug:")
	(list (namestring Garnet-Debug-PathName)))
  (setf (ext:search-list "debug-src:")
	(list (namestring Garnet-Debug-Src)))

  (setf (ext:search-list "demos:")
	(list (namestring Garnet-Demos-PathName)))
  (setf (ext:search-list "demos-src:")
	(list (namestring Garnet-Demos-Src)))

  (setf (ext:search-list "gilt:")
	(list (namestring Garnet-Gilt-PathName)))
  (setf (ext:search-list "gilt-src:")
	(list (namestring Garnet-Gilt-Src)))

  (setf (ext:search-list "c32:")
	(list (namestring Garnet-C32-PathName)))
  (setf (ext:search-list "c32-src:")
	(list (namestring Garnet-C32-Src)))

  (setf (ext:search-list "lapidary:")
	(list (namestring Garnet-Lapidary-PathName)))
  (setf (ext:search-list "lapidary-src:")
	(list (namestring Garnet-Lapidary-Src)))

  (setf (ext:search-list "contrib:")
	(list (namestring Garnet-Contrib-PathName)))
  (setf (ext:search-list "contrib-src:")
	(list (namestring Garnet-Contrib-Src)))

  )


;;; Names of loader files.
;;
(defparameter Garnet-Utils-Loader (merge-pathnames "utils-loader" Garnet-Utils-PathName))
(defparameter Garnet-KR-Loader (merge-pathnames "kr-loader" Garnet-KR-PathName))
(defparameter Garnet-Gworld-Loader (merge-pathnames "gworld-loader" Garnet-Gworld-PathName))
(defparameter Garnet-Gem-Loader (merge-pathnames "gem-loader" Garnet-Gem-PathName))
(defparameter Garnet-Opal-Loader (merge-pathnames "opal-loader" Garnet-Opal-PathName))
#-(and)
(defparameter Garnet-Truetype-Loader (merge-pathnames "truetype-loader" Garnet-Truetype-PathName))
(defparameter Garnet-Inter-Loader (merge-pathnames "inter-loader" Garnet-Inter-PathName))
(defparameter Garnet-Multifont-Loader (merge-pathnames "multifont-loader" Garnet-Opal-PathName))
(defparameter Garnet-Gesture-Loader (merge-pathnames "gesture-loader" Garnet-Gesture-PathName))
(defparameter Garnet-PS-Loader (merge-pathnames "ps-loader" Garnet-PS-PathName))
(defparameter Garnet-Aggregadgets-Loader (merge-pathnames "aggregadgets-loader" Garnet-Aggregadgets-PathName))
(defparameter Garnet-Aggregraphs-Loader (merge-pathnames "aggregraphs-loader" Garnet-Aggregadgets-PathName))
(defparameter Garnet-Gadgets-Loader (merge-pathnames "gadgets-loader" Garnet-Gadgets-PathName))
(defparameter Garnet-Debug-Loader (merge-pathnames "debug-loader" Garnet-Debug-PathName))
(defparameter Garnet-Demos-Loader (merge-pathnames "demos-loader" Garnet-Demos-PathName))
(defparameter Garnet-Gilt-Loader (merge-pathnames "gilt-loader" Garnet-Gilt-PathName))
(defparameter Garnet-C32-Loader (merge-pathnames "c32-loader" Garnet-C32-PathName))
(defparameter Garnet-Lapidary-Loader (merge-pathnames "lapidary-loader" Garnet-Lapidary-PathName))
(defparameter garnet-protected-eval-Loader (merge-pathnames "protected-eval-loader" Garnet-Protected-Eval-PathName))


;;;--------------------------------------------------------------------
;; Packages to load and the locations of those packages.

(defparameter Garnet-Load-Alist
;;; Target directories (binarys)  
  `(("gg"                 . ,Garnet-Gadgets-PathName)
    ("gadgets"            . ,Garnet-Gadgets-PathName)
    ("utils"              . ,Garnet-Utils-PathName)
    ("kr"                 . ,Garnet-KR-PathName)
    ("gworld"             . ,Garnet-Gworld-Pathname)
    ("gem"                . ,Garnet-Gem-Pathname)
    ("opal"               . ,Garnet-Opal-Pathname)
    #-(and)
    ("truetype"           . ,Garnet-Truetype-PathName)
    ("inter"              . ,Garnet-Inter-PathName)
    ("gesture"            . ,Garnet-Gesture-PathName)
    ("gestures"           . ,Garnet-Gesture-PathName)
    ("ps"                 . ,Garnet-PS-PathName)
    ("aggregadgets"       . ,Garnet-Aggregadgets-PathName)
    ("debug"              . ,Garnet-Debug-PathName)
    ("demos"              . ,Garnet-Demos-PathName)
    ("demo"               . ,Garnet-Demos-PathName)
    ("gilt"               . ,Garnet-Gilt-PathName)
    ("c32"                . ,Garnet-C32-PathName)
    ("lapidary"           . ,Garnet-Lapidary-PathName)
    ("contrib"            . ,Garnet-Contrib-PathName)
    ("protected-eval"     . ,Garnet-Protected-Eval-PathName)
;;; Source directories.
    ("utils-src"          . ,Garnet-Utils-Src)
    ("kr-src"             . ,Garnet-KR-Src)
    ("gworld-src"         . ,Garnet-Gworld-Src)
    ("gem-src"            . ,Garnet-Gem-Src)
    ("opal-src"           . ,Garnet-Opal-Src)
    #-(and)
    ("truetype-src"       . ,Garnet-Truetype-Src)
    ("inter-src"          . ,Garnet-Inter-Src)
    ("gesture-src"        . ,Garnet-Gesture-Src)
    ("gestures-src"       . ,Garnet-Gesture-Src)
    ("ps-src"             . ,Garnet-PS-Src)
    ("aggregadgets-src"   . ,Garnet-Aggregadgets-Src)
    ("gadgets-src"        . ,Garnet-Gadgets-Src)
    ("gg-src"             . ,Garnet-Gadgets-Src)
    ("debug-src"          . ,Garnet-Debug-Src)
    ("demos-src"          . ,Garnet-Demos-Src)
    ("demo-src"           . ,Garnet-Demos-Src)
    ("gilt-src"           . ,Garnet-Gilt-Src)
    ("c32-src"            . ,Garnet-C32-Src)
    ("lapidary-src"       . ,Garnet-Lapidary-Src)
    ("contrib-src"        . ,Garnet-Contrib-Src)
    ("protected-eval-src" . ,Garnet-Protected-eval-Src)
    ))


;;; The actual loader code.
;;
(defun Add-Garnet-Load-Prefix (prefix pathname)
  (push (cons prefix pathname) Garnet-Load-Alist))

(defun Garnet-Load (filename)
  "Load a file. If the file is prefixed with a Garnet module name, get
the file from the proper directory in the Garnet source tree.
Otherwise just load the filename as given."
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((module (subseq filename 0 pos))
	       (name (subseq filename (1+ pos)))
	       (module-src-directory
		(or (cdr (assoc module Garnet-Load-Alist :test #'string=))
		    (error "Module ~S is not a Garnet module" module)))
	       (src-pathname (make-pathname :name name
					    ;; For Windows.
					    :device (pathname-device module-src-directory)
					    :directory (pathname-directory
							module-src-directory))))
	  (force-output *error-output*)
	  (format T "~&Loading ~s~%" src-pathname)
	  (force-output)
	  (load src-pathname))
	;; else no module name found; load regular.
	(progn
	  (format T "No module name given: Loading ~s~%" filename)
	  (load filename)))))


;;; Garnet-Compile.
;; This function will compile your garnet files while keeping the
;; sources and binaries separated.  If you want to just compile one
;; file from Garnet, like the gadget file gauge.lisp, then you could
;; use this function to compile the source file and automatically
;; save the binary file in the proper directory in the binary tree.
;;
;; Example:
;;    (garnet-compile "gadgets:gauge")
;;    Takes the source file from Garnet-Gadgets-Src, compiles it, and
;;    saves the binary file in Garnet-Gadgets-Pathname (the binary
;;    gadgets directory).
;;
(defvar *compiler-extension*
  (pathname-type (compile-file-pathname "foo.lisp")))

;;; RGA  This will lose on Windows XXX
(defun garnet-mkdir-if-needed (dirname)
  "Creates the directory if it does not exist."
  (ensure-directories-exist dirname :verbose t))

(defun Garnet-Compile (filename)
  "Compile a single Garnet file, finding the source in the Garnet
source tree and installing the result in the corresponding directory
in the binary tree.

Example:
    (garnet-compile \"gadgets:gauge\")
    Takes the source file from Garnet-Gadgets-Src, compiles it, and
    saves the binary file in Garnet-Gadgets-Pathname (the binary
    gadgets directory)."

  (let* ((pos (position #\: filename))
	 (module (if pos
		     (subseq filename 0 pos)
		     ;; else no colon, abort
		     (error
		      "The filename ~A is not prefixed by a garnet module name. Aborting compile" 
		      filename)))
	 ;; We want to extract just the name part, without the .lisp if present.
	 (filepath (subseq filename (1+ pos)))
	 (name (pathname-name filepath))
	 (type (pathname-type name))
	 (module-src (concatenate 'string module "-src"))
	 (module-src-directory
	  (or (cdr (assoc module-src Garnet-Load-Alist
			  :test #'string=))
	      (cdr (assoc module Garnet-Load-Alist
			  :test #'string=))
	      (error "Module named ~S not found in Garnet-Load-Alist"
		     module)))
	 (module-bin-directory
	  (or (cdr (assoc module Garnet-Load-Alist
			  :test #'string=))
	      (error "Module named ~S not found in Garnet-Load-Alist"
		     module)))
	 (src-pathname (make-pathname :name name
				      ;; If no user supplied type, add default.
				      :type (or type "lisp")
				      :device (pathname-device module-src-directory)
				      :directory (pathname-directory module-src-directory)))
	 (bin-pathname (make-pathname :name name
				      :type *compiler-extension*
				      :device (pathname-device module-bin-directory)
				      :directory (pathname-directory module-bin-directory))))
    (force-output *error-output*)
    (format T "~&Compiling ~s~%" src-pathname)
    (format T "for output to ~s~%" bin-pathname)
    (force-output)
    ;; sds: make sure that bin/foo directory is already there
    (garnet-mkdir-if-needed bin-pathname)
    (let ((*compile-verbose* Garnet-Garnet-Debug)
	  (*compile-print* Garnet-Garnet-Debug)
	  #+cmu (*gc-verbose* nil)
	  )
      (compile-file src-pathname :output-file bin-pathname))))


;;; ----------------------------------------
;;
;; The real load
;;

(format t "...Loading Garnet ...~%")
(setf *load-verbose* t)

;;
;;  Functions that will determine whether the display can be opened
;;
(defun get-full-display-name ()
  ;; If you have CLX, you most likely have this.
  (xlib::getenv "DISPLAY"))

(defun get-display-name (display)
  ;; The display name is everything up to the first colon.
  (unless (find #\: display)
    (error "The display specification  \"~A\" is ill-formed: missing colon" display))
  (subseq display 0 (position #\: display)))

(defun get-display-number (display)
  ;; The display number is everything from the colon to the period (if
  ;; it is present).
  (unless (find #\: display)
    (error "The display specification  \"~A\" is ill-formed: missing colon" display))
  (let ((display-number
	 (ignore-errors
	   (parse-integer
	    (subseq display
		    (1+ (position #\: display))
		    (position #\. display))))))
    (unless (numberp display-number)
      (error "The display specification  \"~A\" is invalid: bad display number" display))
    display-number))


(defun verify-display-can-be-opened ()
  (let* ((full-display-name (get-full-display-name))
	 (d-name (if full-display-name
		     (get-display-name full-display-name)
		     (machine-instance)))
	 (d-number (get-display-number full-display-name))
	 (val nil)
	 (errorp nil))
    (unwind-protect
	 (progn
	   (multiple-value-setq (val errorp)
	     (ignore-errors (xlib:open-display d-name :display d-number)))
	   (if errorp
	       (error "Could not open a display for ~S.
     You must already be running X to load or compile Garnet.  Your DISPLAY
environment variable must be set with the name of the machine on which the
Garnet windows will be displayed.  Please exit lisp and execute a command
like the following to the unix shell before loading or compiling Garnet:
  \"setenv DISPLAY windowmachine.cs.cmu.edu:0.0\"
  \"setenv DISPLAY unix:0.0\"
  \"setenv DISPLAY :0.0\"
  \"setenv DISPLAY :0\"
The last three values may be more efficient when you want the Garnet windows
to appear on the same machine that Garnet is running on.
     Additionally, you must execute the command \"xhost +\" on the machine
that the windows will be displayed on, if it is different from the machine
running Garnet."
		      full-display-name)))
      (when val
	(xlib:close-display val)))
      T))

;;; RGA --- sometimes I only want to load KR.  Don't need a display we
;;; aren't doing graphics.

(defvar *kr-only* nil
  "Only loading KR so don't need to open displays.")

(unless *kr-only*
  (verify-display-can-be-opened))


;;;
;; Now back to loading Garnet
;;
(if load-utils-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :utils))
	(format T "~%****** Utils already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Utils %%%%%%%%~%")
          (load Garnet-Utils-Loader)))
    (format T "~%****** NOT Loading Utils *******~%"))

(if load-kr-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :kr))
	(format T "~%****** KR already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading KR %%%%%%%%~%")
          (load Garnet-KR-Loader)))
    (format T "~%****** NOT Loading KR *******~%"))

(if load-kr-doc-p (garnet-load "kr:kr-doc"))

(if load-gem-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gem))
	(format T "~%****** Gem already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gem %%%%%%%%~%")
          (load Garnet-Gem-Loader)))
    (format T "~%****** NOT Loading Gem *******~%"))

(if load-opal-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :opal))
	(format T "~%****** Opal already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Opal %%%%%%%%~%")
          (load Garnet-Opal-Loader)))
    (format T "~%****** NOT Loading Opal *******~%"))

#-(and)
(if load-truetype-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :truetype))
	(format T "~%****** Truetype already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Truetype %%%%%%%%~%")
          (load Garnet-Truetype-Loader)))
    (format T "~%****** NOT Loading Truetype *******~%"))

(if load-inter-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :inter))
	(format T "~%****** Interactors already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Interactors %%%%%%%%~%")
          (load Garnet-Inter-Loader)))
    (format T "~%****** NOT Loading Interactors *******~%"))

(if load-multifont-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :multifont))
	(format T "~%****** Multifont already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Multifont %%%%%%%%~%")
          (load Garnet-Multifont-Loader)))
    (format T "~%****** NOT Loading Multifont *******~%"))

(if load-gesture-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gesture))
	(format T "~%****** Gestures already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gestures %%%%%%%%~%")
          (load Garnet-Gesture-Loader)))
    (format T "~%****** NOT Loading Gestures *******~%"))

(if load-ps-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :ps))
	(format T "~%****** PS already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading PS %%%%%%%%~%")
          (load Garnet-PS-Loader)))
    (format T "~%****** NOT Loading PS *******~%"))

(if load-aggregadgets-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :aggregadgets))
	(format T "~%****** Aggregadgets already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Aggregadgets %%%%%%%%~%")
          (load Garnet-Aggregadgets-Loader)))
    (format T "~%****** NOT Loading Aggregadgets *******~%"))

(if load-aggregraphs-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :aggregraphs))
	(format T "~%****** Aggregraphs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Aggregraphs %%%%%%%%~%")
          (load Garnet-Aggregraphs-Loader)))
    (format T "~%****** NOT Loading Aggregraphs *******
** To load aggregraph programs, execute (load Garnet-Aggregraphs-Loader)~%"))


(if load-gadgets-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gadgets))
	(format T "~%****** Gadgets already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gadgets %%%%%%%%~%")
          (load Garnet-Gadgets-Loader)))
    (format T "~%****** NOT Loading Gadgets *******~%"))

(if load-debug-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :debug))
	(format T "~%****** Debugging programs already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Debugging programs %%%%%%%%~%")
          (load Garnet-Debug-Loader)))
    (format T "~%****** NOT Loading Debug Files *******
** To load debug programs, execute (load Garnet-Debug-Loader)~%"))

(if load-demos-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :demos))
	(format T "~%****** Demos already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Demos %%%%%%%%~%")
          (load Garnet-Demos-Loader)))
    (format T "~%****** NOT Loading Demos *******
** To load Demos, execute (load Garnet-Demos-Loader)~%"))

(if load-gilt-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :gilt))
	(format T "~%****** Gilt already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Gilt %%%%%%%%~%")
          (load Garnet-Gilt-Loader)))
    (format T "~%****** NOT Loading Gilt *******
** To load Gilt, execute (load Garnet-Gilt-Loader)~%"))

(if load-c32-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :c32))
	(format T "~%****** C32 already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading C32 %%%%%%%%~%")
          (load Garnet-C32-Loader)))
    (format T "~%****** NOT Loading C32 *******
** To load C32, execute (load Garnet-C32-Loader)~%"))

(if load-lapidary-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :lapidary))
	(format T "~%****** Lapidary already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Lapidary %%%%%%%%~%")
          (load Garnet-Lapidary-Loader)))
    (format T "~%****** NOT Loading Lapidary *******
** To load Lapidary, execute (load Garnet-Lapidary-Loader)~%"))

(if load-protected-eval-p
    (if (and *dont-load-modules-twice* (get :garnet-modules :protected-eval))
	(format T "~%****** Protected-eval already loaded *******~%")
        (progn
          (format T "~% %%%%%%%% Loading Protected-eval %%%%%%%%~%")
          (load Garnet-Protected-Eval-Loader)))
    (format T "~%****** NOT Loading Protected-Eval *******
** To load Protected-Eval, execute (load Garnet-Protected-Eval-Loader)~%"))



;;
;; Only set the k-reader if we are loading, not compiling
 ;; (if (get :garnet-modules :kr)
 ;;   (set-dispatch-macro-character #\# #\k (function kr::k-reader)))


;; RGA added two auxiliary functions for doing file manipulations.
;; Right now they are used to copy the xxx-loader files into the
;; target directories.
;;
(defun garnet-shell-exec (command)
  "This is a quick and dirty version of opal:shell-exec used just
   for the compiler.  This currently looses on Mac OS."
  #+allegro (excl:run-shell-command command :wait NIL :output :stream
				    :error-output :stream)
  #+cmu
  (ext:process-output (ext:run-program "/bin/sh" (list "-c" command)
				       :wait NIL :output :stream))

  #+ccl
  (ccl:external-process-output-stream 
   (ccl:run-program "/bin/sh" (list "-c" command)
		    :wait NIL :output :stream))
  #+sbcl
  (sb-ext:process-output (sb-ext:run-program "/bin/sh" (list "-c" command)
				       :wait NIL :output :stream)))

(defun garnet-copy-files (src-dir bin-dir file-list)
  "Copies a list of files (usually loader files) from source directory
  to binary directory."
  (dolist (file file-list)
    (let ((src (merge-pathnames file src-dir))
	  (dest (merge-pathnames file bin-dir)))
      (garnet-shell-exec (format nil "cp ~A ~A~%" src dest)))))

(format t "~%... Garnet Load Complete ...~%")

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb-ext:*muffled-warnings* 'sb-kernel::uninteresting-redefinition))
