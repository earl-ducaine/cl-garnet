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
;;      user::load-XX-p to NIL.
;; ** To get some of the parts which are not loaded by default to be loaded,
;;    set user::load-XX-p to T.
;; ** If you are a non-CMU user, set Your-Garnet-Pathname to be your local
;;    Garnet directory, and set Your-CLX-Pathname to be your local CLX
;;    directory.
;; ** To override where something is loaded from, set Garnet-xx-PathName
;;    before loading this file and/or Garnet-xx-src
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
;; The first part of this file lists the file names where the various
;; parts of Garnet come from.  This will need to be modified for each new
;; installation of Garnet.
;;
;; To override any particular file name place, it is only necessary to
;; assign the variable name Garnet-XX-Pathname before this file is loaded
;; (since they are defined here using defvar, the old name will stay in
;; affect).
;;
;;


;;; ============================================================
;; Change log:
;; 15-Feb-2010 Fred Gilham - Updates for SBCL, added Allegro 8.1.
;; 10/04/03 Russell Almond - Changed #+garnet-protected-eval to
;;                           (load-protected-eval-p) (Protected-eval
;; 			     basically replaces code in processes.lisp
;; 10/04/03 Russell Almond - Added fix for MCL #\return vs #\linefeed
;;                           issue. (do-load function).
;; 10/04/03 Russell Almond - Added new extensions for MCL-5.0
;; 10/02/03 Russell Almond - Added KR-doc flags.
;; 10/02/03 Russell Almond - Added support for Protected-Eval
;; 29-sep-2003 Robert Goldman - Add trial version of Allegro-specific code to open
;;                           display using Xauthorization information.
;; 15-Nov-2002 Fred Gilham - Add protected-eval module (from contrib/prompter code).
;;                           Added #+garnet-protected-eval feature to allow process
;;                           code to be compiled appropriately.
;; 08/20/98 Fred Gilham    - Auto-detect CMUCL binary name.  Make
;;                           :external the default for garnet-version.
;; ???????? Russell Almond - Changed to use (require :clx) instead of
;;                           loading CLX explicitly.
;; ???????? Russell Almond - Better support for multiple external
;;                           versions of Garnet.
;; 01/30/95 Andrew Mickish - New redefinitions of :CL and :CL-USER for CMUCL
;; 01/05/95 Andrew Mickish - Added :CL-USER nickname in defpackage
;;                           redefinition of :COMMON-LISP-USER package
;;                           and reordered :USE arguments by suggestion
;; 03/17/94 Andrew Mickish - Added Gworld for Mac
;; 12/04/93 Andrew Mickish - Added Mac switches
;; 11/01/93 Andrew Mickish - Added GEM
;; 09/22/93 Bruno Haible   - Added FLET for merge-pathnames in CLISP
;; 08/13/93 Andrew Mickish - Added user::Garnet-Readtable
;; 08/12/93 Andrew Mickish - Closed display in Verify-Display-Can-Be-Opened;
;;                           added #+garnet-processes to *features* list
;; 05/17/93 Andrew Mickish - Added compiler optimization proclamation
;; 05/13/93 Andrew Mickish - Removed commas from Garnet-Load-Alist so it notices
;;                           changes in the values of the pathname variables
;; 04/15/93 Andrew Mickish - Added lucid memory-management instruction
;; 04/ 5/93 Dave Kosbie    - Added Garnet-Utils package (where Garnet-independent
;;                           Lisp utilities will now reside)
;; 03/25/93 Andrew Mickish - Made Garnet-Load use an association list
;; 03/17/93 Andrew Mickish - Removed Motif-Gilt-Loader
;; 10/23/92 Dave Kosbie    - Added KATIE package
;; 08/17/92 Andrew Mickish - Added display check, changed names of switches,
;;                           changed names of directories, changed method
;;                           for determining Garnet pathnames
;; 07/29/92 Andrew Mickish - :cmu-sparc now loads from cmu-bin, removed :cmu
;;                           and :test versions.
;; 07/23/92 Dario Giuse    - moved loading of C32 before Lapidary, which needs it.
;; 05/27/92 Joly           - Interactors package should use kr package.
;; 05/25/92 Joly/Pervin    - Package CLTL1 does not exist in LispWorks.
;; 05/21/92 Dario Giuse    - Added load-c32-p.
;; 05/14/92 Szekely/Pervin - Do not launch process if compiling.
;; 05/04/92 Russell Almond - Added allegro-v4.1 switches.
;; 04/22/92 Ed Pervin      - Added launch-process-p switch.
;; 04/10/92 Andrew Mickish - Added "gg:" prefix to garnet-load and garnet-compile
;; 04/02/92 Rich McDaniel  - Added load-multifont-p
;; 03/13/92 Ed Pervin      - Added :cmu-test
;; 03/11/92 Andrew Mickish - Removed unlesses from CMU ext:search-list setfs
;; 03/06/92 Andrew Mickish - Added *compiler-extension* switches
;; 02/20/92 Andrew Mickish - Added package definitions for Explorer lisp;
;;                           Added gesture pathnames, etc.
;; 02/11/92 Andrew Mickish - Added :garnet-debug to *features* list;  changed
;;                           pathnames from /afs/cs/ to /afs/cs.cmu.edu/.
;; 04/25/91 Ed Pervin      - Official release of version 1.4; alpha directory
;;                           changed back to test.  No longer support
;;                           :cmu-lucid3.1 and :cmu-lucid4.0.
;; 04/19/91 Ed Pervin      - Added lispworks to switches.
;; 04/15/91 Ed Pervin      - Changed (make-packages **) to
;;                           (unless (find-package **) (make-package **)).
;; 04/03/91 Ed Pervin      - Changed :sparc-test4.0 --> :sparc-test and
;;                           added :pmax-test.
;; 03/21/91 Ed Pervin      - Release 1.4; test directory changed to alpha.
;; 03/07/91 Andrew Mickish - Added aggregraphs.
;; 03/07/91 Brad Myers     - Made new motif-gilt-loader, and also garnet-load.
;; 03/01/91 Ed Pervin      - Added :sparc-test for version compiled in Allegro 4.0.
;; 02/27/91 Dilip D'Souza  - Added everything with #+allegro-v4.0 switches.
;; 02/25/91 Ed Pervin      - Pushed :garnet on *features* list.
;; 01/24/91 Andrew Mickish - Added Gilt.
;; 01/02/90 Andrew Mickish - Added :rt-test and :sparc-test options.
;; 11/29/90 Brad Myers     - Added :cmu-sparc option.
;; 10/05/90 Ed Pervin      - New variables Your-Garnet-Pathname and Your-CLX-Pathname
;;                           which determine all the :external pathnames.
;; 08/09/90 Ed Pervin      - Release 1.3
;; 08/07/90 Ed Pervin      - rbd --> ecp
;; 07/25/90 Ed Pervin      - Added *dont-load-modules-twice*;  amickish --> preddy
;; 04/02/90 Ed Pervin      - Call xlib:load-clx in Lucid explicitly.
;; 03/19/90 Ed Pervin      - Got rid of Garnet-Font-Pathname
;; 02/14/90 Ed Pervin      - Added color screen option
;; 01/04/90 Ed Pervin      - Added :external option and version number
;; 12/19/89 Ed Pervin      - Now loads CLX.
;; 12/13/89 Ed Pervin      - Added :cmu-allegro option.
;; 12/05/89 Brad Myers     - Fixed so works with garnet-compiler
;; 10/30/89 Brad Myers     - New file structure and src directories;  changed
;;                           dont-load-xx to load-xxx-p
;; 10/17/89 Brad Myers     - Added debug
;; 08/18/89 Brad Myers     - Added Toolkit
;; 06/07/89 Brad Myers     - Created
;; ============================================================



;;; This is a table of various lisp flavors which Garnet has at one time
;;  or another been compiled on:
;;
;; allegro (Allegro Common Lisp, Commercial).  Starting with version 3.0 (currently
;;         at 6.2).  Note that allegro has a special switch (allegro>= x
;;         y) which allows one to test for a specific version or later.
;; lucid (Lucid Common Lisp, Commerical).  Now out of business, bought by
;;       Allegro.
;; lispworks
;; cmu (Carnegie Mellon Lisp, Free).  Open source lisp.  Later versions
;;     support multiple processes (and cmu mp).
;; clisp (C-Lisp, Free) Lisp to C compiler.
;; kcl (Kyoto Common Lisp, Free).  Open source lisp.
;; 
;; 
;; mcl (Macintosh Common Lisp, Commercial).  Common Lisp for Macintosh.
;;     Version 3 and later has multiprocess support.  Note was previously
;;     called "Coral Common Lisp" so uses abbreviations ccl, ccl-3 &c.
;; 
;; apple This nominally refers to lisps running on Apple Macintosh
;;       computers.  Apple paid for/assisted in support for porting
;;       Garnet to Mac OS in early 90s.  With Mac OS X, Apple provides X
;;       windows support, so nominally we could run under CLX too.
;;       Probably many of these switches need to be changed to
;;       (and apple (not clx)).  I'm still experimenting with Mac OS X
;;       version of garnet.
;; 
;; Note that nobody ever paid the CMU development team for Windows port.
;; Contributions are welcome here.  I am also working on an OpenGL port
;; which may make it easier to run cross platform.
;; 
;;      --Russell Almond 10/02/03



(in-package :COMMON-LISP-USER)

(defparameter Garnet-Version-Number "3.3")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3 *features*)
(setf *features* (delete :GARNET-V3.0 *features*))
(pushnew :GARNET-V3.3 *features*)


;; The :GARNET-PROCESSES keyword goes on the *features* list if this version
;; of lisp supports multiple processes.  Then things like the animation
;; interactor can use the #+garnet-processes switch, instead of referring
;; explicitly to different versions of lisp.
(pushnew :GARNET-PROCESSES *features*)

#+allegro
(defvar Garnet-Readtable *readtable*
  "This variable is used by Allegro to restore the old value of the *readtable*
when a saved image is restarted (see opal:make-image in opal/utils.lisp).")


;;; The following variables are used for customizing various aspects
;;  of building to allow debugging or build faster systems.
;;

;; The :GARNET-DEBUG option allows many different kinds of run-time checking,
;; and also loads some extra test code.  After you have debugged your code
;; and want it to run faster, remove :GARNET-DEBUG from the *features* list
;; and RECOMPILE all of Garnet and your code.  The result will be smaller and
;; somewhat faster.
;; To remove :GARNET-DEBUG from the *features* list, either defvar
;; Garnet-Garnet-Debug to NIL before you load the garnet-loader, or simply
;; comment out the next few lines.
(defvar Garnet-Garnet-Debug T)
(if Garnet-Garnet-Debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))


(defvar *garnet-compile-debug-mode* nil
  "Setting this variable to T sets the policy for the entire system
to make it more debuggable.")

(defvar *garnet-compile-debug-settings*
  '(optimize (speed 2) (safety 3) (debug 3) (space 3) (compilation-speed 0))
  "Use these settings for globally debugging the system or for debugging
a specific module. They emphasize debuggability at the cost of some speed.

With SBCL:

- These settings are type-safe. 

- They prevent functions declared inline from being expanded inline. 
  Note that as part of this version I have tried to make most 
  non-syntactic macros into inline functions.

- They allow all possible debugging features.")

(defvar *garnet-compile-production-settings*
  '(optimize (speed 3) (safety 1) (space 0) (debug 2) (compilation-speed 0))
  "Production compiler policy settings. Emphasize speed, de-emphasize debugging.")
  
(defvar *default-garnet-proclaim*
  (if *garnet-compile-debug-mode*
      *garnet-compile-debug-settings*
      *garnet-compile-production-settings*)
  "Set compiler optimization settings.

1. If you want everything debugged, set *garnet-compile-debug-mode* to t.

2. If you want to debug specific modules, set *garnet-compile-debug-mode*
   to nil. Then set the variable in the modules you want debugged to enable
   debugging that module.

3. Otherwise (for 'production' builds) just set *garnet-compile-debug-mode* 
   to nil and leave everything else alone.")


 #+sbcl
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (setf sb-ext:*muffled-warnings* 'sb-kernel::style-warning))

 (when *default-garnet-proclaim*
   (proclaim *default-garnet-proclaim*))

 
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
   (defpackage :DEMOS-CONTROLLER (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
   (defpackage :DEMO-MANYOBJS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
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
   #+(or allegro cmu)
   (defvar load-truetype-p nil)
   #-(or allegro cmu)
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

 
 ;; RGA --- This means that the local lisp installation takes care of
 ;; finding CLX and we don't have to (require :clx)
 (defvar load-clx-p #+clx NIL #-clx T)

;; launch-process-p controls whether Garnet will launch
;; a separate process to detect keyboard and mouse events.
(defvar launch-process-p T)

;; update-locking-p controls whether process locks will be activated
;; around the update method (this keeps two processes from calling update
;; at the same time).
(defvar update-locking-p T
  "If T, uses process locks to keep Update in a process from interrupting
   itself in a different process.")

(defun Version-Error ()
  (error "Could not determine which compiled binaries are appropriate to
load into your lisp.  Please set common-lisp-user::Garnet-Version before loading
Garnet-Loader again."))

(defvar Multiple-Garnet-Bin-Dirs t
  "Set this to T if you want the name of the bin directory to
   vary with the version of garnet, handy if you are debugging
   with multiple Lisp versions.")

;; Garnet-Version controls where the files are loaded from.
;; Because this is a defvar, if Garnet-Version is set before this file is
;; loaded, its original value will be used.

;; Garnet-Version should be set to :external for non-CMU users, and
;; Your-Garnet-Pathname should be set appropriately.
;;
#+NOTUSED
(defun Get-Garnet-Version ()
  :external)

#+NOTUSED
(defvar garnet-version (Get-Garnet-Version))

(format T "~&** Loading Garnet Version ~a~%" Garnet-Version-Number)

;; Insert your pathname of Garnet into Your-Garnet-Pathname and where
;; your CLX comes from into Your-CLX-pathname.  All the :external pathnames
;; will depend on these two pathnames.
;;
;; On CMU's Andrew system, do
;; (setf Your-CLX-Pathname "/usr/local/lib/cl/lib/code/")
;; before loading garnet-loader.lisp.

;; RGA --- This can be made mostly obsolete by simply doing (require
;; :clx) before loading garnet.
					;(require :clx)
;;; RGA I moved this line up earlier to make register in the defvar.
#+NOTUSED
(defvar Your-CLX-Pathname
      "**FILL THIS IN**"                ;; SET THIS
)

(defvar Your-Garnet-Pathname
  ;; This should work on Ansi compliant lisps.  Try it, if not
  ;; hardcode the pathname.
  (namestring (make-pathname :directory
			     (pathname-directory *load-truename*)))
  #+comment"**FILL THIS IN**"                ;; SET THIS
  )

;; ripped from the bowels of asdf-install... [2006/01/05:rpg]       
(defun directorify (name)
  ;; input name may or may not have a training #\/, but we know we
  ;; want a directory
  (let ((path (pathname name)))
    (if (pathname-name path)
        (merge-pathnames
         (make-pathname :directory `(:relative ,(pathname-name path)))
         (make-pathname :directory (pathname-directory path)
                        :host (pathname-host path)))
        path)))

;; RGA added this function as a cleaner way of handling the differences
;; between Unix and Mac file naming conventions.  This will loose on pre-ansi
;; lisps, but I can live with that (I think).

;; RGA's original form really doesn't work properly in some lisps,
;; because we are using it to create directory pathnames, but what
;; you get out will often NOT be a directory. [2006/01/05:rpg]
(defun append-directory (pathnme dirstring)
  "This is a little utility for accessing the subdirectory of a
directory."
  (let ((pnd (pathname-directory pathnme))
	(dlist (if (listp dirstring) 
		   dirstring
		   (list dirstring))))
    (let ((raw-pathname
	   (merge-pathnames (make-pathname :directory (append pnd dlist))
			    pathnme)))
      (directorify raw-pathname))))

;; the following is just bloody awful, but I won't be able to fix it
;; until version 4 comes along. [2006/01/05:rpg]
(defun Get-Garnet-Binary-Pathname ()
  (let ((directory-name
	 #+sbcl "bin.sbcl"
	 #+cmu "bin.cmu"
	 #+allegro "bin.allegro"
	 #-(or sbcl cmu allegro) (error "~S is an invalid garnet-version" version)))
    (unless multiple-garnet-bin-dirs (setq directory-name "bin"))
    (append-directory Your-Garnet-Pathname directory-name)))


(defvar Garnet-Src-Pathname
  (append-directory  Your-Garnet-Pathname "src"))
(defvar Garnet-Binary-Pathname (Get-Garnet-Binary-Pathname))
(defvar Garnet-Lib-Pathname
  (append-directory Your-Garnet-Pathname "lib"))
#+NOTUSED
(defvar CLX-Pathname Your-CLX-Pathname)


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
(defvar Garnet-Truetype-Src
  (append-directory Garnet-Src-Pathname "truetype"))
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



;; RGA commented this out.  We can just use the garnet-compile
;; function to compile directly into the source directory, no muss, no fuss.
;; This way compilation does not depend on a unix utility.
 ;; (when (and (boundp '*Garnet-Going-To-Compile*)
 ;; 	   *Garnet-Going-To-Compile*)
 ;;  (setf Garnet-Utils-Pathname Garnet-Utils-Src)
 ;;  (setf Garnet-KR-Pathname Garnet-KR-Src)
 ;;  (setf Garnet-Gworld-Pathname Garnet-Gworld-Src)
 ;;  (setf Garnet-Gem-Pathname Garnet-Gem-Src)
 ;;  (setf Garnet-Opal-Pathname Garnet-Opal-Src)
 ;;  (setf Garnet-Inter-Pathname Garnet-Inter-Src)
 ;;  (setf Garnet-Gesture-Pathname Garnet-Gesture-Src)
 ;;  (setf Garnet-PS-Pathname Garnet-PS-Src)
 ;;  (setf Garnet-Aggregadgets-Pathname Garnet-Aggregadgets-Src)
 ;;  (setf Garnet-Gadgets-Pathname Garnet-Gadgets-Src)
 ;;  (setf Garnet-Debug-Pathname Garnet-Debug-Src)
 ;;  (setf Garnet-Demos-Pathname Garnet-Demos-Src)
 ;;  (setf Garnet-Gilt-Pathname Garnet-Gilt-Src)
 ;;  (setf Garnet-C32-Pathname Garnet-C32-Src)
 ;;  (setf Garnet-Lapidary-Pathname Garnet-Lapidary-Src)
 ;;  (setf Garnet-Contrib-Pathname Garnet-Contrib-Src)
 ;;  )


;;;----------------------------------------------------------
;; If at cmu, then set up the search lists
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
  `(("gg" . Garnet-Gadgets-PathName)
    ("gadgets" . Garnet-Gadgets-PathName)
    ("utils" . Garnet-Utils-PathName)
    ("kr" . Garnet-KR-PathName)
    ("gworld" . Garnet-Gworld-Pathname)
    ("gem" . Garnet-Gem-Pathname)
    ("opal" . Garnet-Opal-Pathname)
    ("truetype" . Garnet-Truetype-PathName)
    ("inter" . Garnet-Inter-PathName)
    ("gesture" . Garnet-Gesture-PathName)
    ("gestures" . Garnet-Gesture-PathName)
    ("ps" . Garnet-PS-PathName)
    ("aggregadgets" . Garnet-Aggregadgets-PathName)
    ("debug" . Garnet-Debug-PathName)
    ("demos" . Garnet-Demos-PathName)
    ("demo" . Garnet-Demos-PathName)
    ("gilt" . Garnet-Gilt-PathName)
    ("c32" . Garnet-C32-PathName)
    ("lapidary" . Garnet-Lapidary-PathName)
    ("contrib" . Garnet-Contrib-PathName)
    ("protected-eval" . Garnet-Protected-Eval-PathName)
    ("utils-src" . Garnet-Utils-Src)
    ("kr-src" . Garnet-KR-Src)
    ("gworld-src" . Garnet-Gworld-Src)
    ("gem-src" . Garnet-Gem-Src)
    ("opal-src" . Garnet-Opal-Src)
    ("truetype-src" . Garnet-Truetype-Src)
    ("inter-src" . Garnet-Inter-Src)
    ("gesture-src" . Garnet-Gesture-Src)
    ("gestures-src" . Garnet-Gesture-Src)
    ("ps-src" . Garnet-PS-Src)
    ("aggregadgets-src" . Garnet-Aggregadgets-Src)
    ("gadgets-src" . Garnet-Gadgets-Src)
    ("gg-src" . Garnet-Gadgets-Src)
    ("debug-src" . Garnet-Debug-Src)
    ("demos-src" . Garnet-Demos-Src)
    ("demo-src" . Garnet-Demos-Src)
    ("gilt-src" . Garnet-Gilt-Src)
    ("c32-src" . Garnet-C32-Src)
    ("lapidary-src" . Garnet-Lapidary-Src)
    ("contrib-src" . Garnet-Contrib-Src)
    ("protected-eval-src" . Garnet-Protected-eval-Src)
;;    ("clx" . CLX-PathName)
    ))


;;; The actual loader code.
;;
(defun Add-Garnet-Load-Prefix (prefix pathname)
  (push (cons prefix pathname) Garnet-Load-Alist))

(defun Garnet-Load (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (prefix (or (eval (cdr (assoc head Garnet-Load-Alist
					     :test #'string=)))
			   (error "Bad prefix ~S~%" head)))
	       (finalname (merge-pathnames tail prefix)))
	  (format T "Loading ~s~%" finalname)
	  (load finalname))
	;; else no colon, load regular
	(progn
	  (format T "NO COLON, Loading ~s~%" filename)
	  (load filename)))))


;;; Garnet-Compile.
;; This function will compile your garnet files while keeping the
;; sources and binaries separated.  If you want to just compile one
;; file from Garnet, like the gadget file gauge.lisp, then you could
;; use this function to compile the source file and automatically
;; save the binary file in the bin directory.
;;
;; Example:
;;    (garnet-compile "gadgets:gauge")
;;    Takes the source file from Garnet-Gadgets-Src, compiles it, and
;;    saves the binary file in Garnet-Gadgets-Pathname (the binary
;;    gadgets directory).
;;
(defvar *compiler-extension*
  (concatenate 'string "." (pathname-type (compile-file-pathname "foo.lisp"))))

;;; RGA  This will lose on Windows
(defun garnet-mkdir-if-needed (dirname)
  "Creates the directory if it does not exist."
  (ensure-directories-exist dirname :verbose t))

(defun Garnet-Compile (filename)
  (let ((pos (position #\: filename)))
    (if pos
	(let* ((head (subseq filename 0 pos))
	       (tail (subseq filename (1+ pos)))
	       (head-src (concatenate 'string head "-src"))
	       (src-prefix
		(or (eval (cdr (assoc head-src Garnet-Load-Alist
				      :test #'string=)))
		    (eval (cdr (assoc head Garnet-Load-Alist
				      :test #'string=)))
		    (error "Prefix ~S not found in Garnet-Load-Alist"
			   head)))
	       (bin-prefix
		(or (eval (cdr (assoc head Garnet-Load-Alist
				      :test #'string=)))
		    (error "Prefix ~S not found in Garnet-Load-Alist"
			   head)))
               (src-finalname (merge-pathnames
                               (concatenate 'string tail ".lisp") src-prefix))
               (bin-finalname (merge-pathnames
                               (concatenate 'string tail *compiler-extension*)
                               bin-prefix)))
          (format T "Compiling ~s~%" src-finalname)
          (format T "for output to ~s~%" bin-finalname)
          ;; sds: make sure that bin/foo directory is already there
          (garnet-mkdir-if-needed bin-finalname)
          (compile-file src-finalname :output-file bin-finalname))
        ;; else no colon, abort
        (error "NO COLON, aborting compile"))))


;;; ----------------------------------------
;;
;; The real load
;;

(format t "...Loading Garnet ...~%")
(setf *load-verbose* t)

(cond
  (load-clx-p
   (format T "~% %%%%%%% Loading ~A %%%%%%%%~%" "CLX")
   (require :clx)
   #+sbcl (require :sb-posix)
   )
  (t
   (format T "~%****** NOT Loading CLX *******~%")))

;;
;;  Functions that will determine whether the display can be opened
;;
(defun get-full-display-name ()
  ;; added the "or" because I couldn't figure a good way to trap the 
  ;; "no other case fit" case with the reader macros... [2009/12/07:rpg]
  (or
   #+cmu (cdr (assoc :DISPLAY lisp::*environment-list*))
   #+allegro (sys::getenv "DISPLAY")
   #+sbcl (sb-posix:getenv "DISPLAY")
   ;; RGA hope this works as a sensible default.  Need a new function to
   ;; support other Lisp.
   ":0"
   ))

(defun get-display-name (display)
  (do* ((dlist (coerce display 'list) (cdr dlist))
        (c (car dlist) (car dlist))
        (namelist nil))
       ((or (eq c nil) (eq c '#\:)) (coerce (reverse namelist) 'string))
    (push c namelist)))

(defun get-display-number (display)
  (let* ((dlist (coerce display 'list))
         (numstr (progn
                   (do ((c (pop dlist) (pop dlist)))
                       ((or (eq c nil) (eq c '#\:))))
                   (do ((c (pop dlist) (pop dlist))
                        (numlist nil)
                        )
                       ((or (eq c nil) (eq c '#\.))
                        (coerce (reverse numlist) 'string))
                       (push c numlist)
                       )
                   ))
         (num (if (equal numstr "") 0 (read-from-string numstr)))
         )
    num))

(defun verify-display-can-be-opened ()
  (let* ((full-display-name (get-full-display-name))
	 (d-name (if full-display-name
		     (get-display-name full-display-name)
		     (machine-instance)))
	 (d-number (get-display-number full-display-name)))
    (multiple-value-bind (val errorp)
	(ignore-errors (xlib:open-display d-name :display d-number))
      (if errorp
	  (error "Could not open a display for ~S.
     You must already be running X to load or compile Garnet.  Your DISPLAY
environment variable must be set with the name of the machine on which the
Garnet windows will be displayed.  Please exit lisp and execute a command
like the following to the unix shell before loading or compiling Garnet:
  \"setenv DISPLAY windowmachine.cs.cmu.edu:0.0\"
  \"setenv DISPLAY unix:0.0\"
  \"setenv DISPLAY 0.0\"
The last two values may be more efficient when you want the Garnet windows
to appear on the same machine that Garnet is running on.
     Additionally, you must execute the command \"xhost +\" on the machine
that the windows will be displayed on, if it is different from the machine
running Garnet."
		 full-display-name)
	  (xlib:close-display val))
      T)))

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
;;
(defun garnet-shell-exec (command)
  "This is a quick and dirty version of opal:shell-exec used just
   for the compiler.  This currently looses on Mac OS."
  #+allegro (excl:run-shell-command command :wait NIL :output :stream
				    :error-output :stream)
  #+cmu
  (ext:process-output (ext:run-program "/bin/sh" (list "-c" command)
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
