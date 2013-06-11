;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;; MCL users, use "Open Unix ..." to view/edit this file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

;;; $Id$
;;;
;;; This file basically moves all of the moving parts of garnet-loader
;;;into one place, it also establishes a few other options which may
;;;be useful when working in Garnet.   As the corresponding constants
;;;are defined in garnet-loader with a defvar statement, you can
;;;modify this and then load garnet-loader.
;;; 
;;; 

;;; This next variable should be set to the location of all
;;; garent sources, libs, binaries.  If for some reason this load form
;;; does not work for you, set this variable appropriately.  This form
;;; is also loaded (as a defvar) in garnet-loader so you can comment
;;; this line out if your config-garnet is in a different directory
;;; than your garnet-loader.
(setq Your-Garnet-Pathame
  (namestring (make-pathname :directory
			     (pathname-directory *load-truename*))))

;;; The Unix implemenation of Garnet runs on top of CLX. If CLX was
;;; installed with your version of Lisp (probably true) then the 
;;; following form will likely work.  If not, you probably need to
;;; define the variable Your-CLX-Pathname instead.
(require :clx)
;(setq Your-CLX-Pathname "**FILL THIS IN**")


;;;Set this to T if you want the name of the bin directory to
;;;vary with the version of garnet, handy if you are debugging
;;;with multiple Lisp versions.
(setq Multiple-Garnet-Bin-Dirs nil)


;;; The :GARNET-DEBUG option allows many different kinds of run-time checking,
;;; and also loads some extra test code.  [After you have debugged your code
;;; and want it to run faster, remove :GARNET-DEBUG from the *features* list
;;; and RECOMPILE all of Garnet and your code.  The result will be smaller and
;;; somewhat faster.]  In practice turning garnet-debug off turns off
;;; many features which are necessary for proper error handling and is
;;; not really recommended.  It also has not been well tested.
(defvar Garnet-Garnet-Debug T)

;;; The extra test code described above (mainly examples of how to use
;;;gadgets) is now controlled by an new feature.  Uncomment the
;;;following line if you want to load/compile it.  Note that this is separate
;;;test code from the demos.
;(pushnew :garnet-test *features*)


;;; ** To prevent certain parts from being loaded, first set
;;;      user::load-XX-p to NIL.
;;; ** To get some of the parts which are not loaded by default to be loaded,
;;;    set user::load-XX-p to T.
;;; ** If you are a non-CMU user, set Your-Garnet-Pathname to be your local
;;;    Garnet directory, and set Your-CLX-Pathname to be your local CLX
;;;    directory.
;;; ** To override where something is loaded from, set Garnet-xx-PathName
;;;    before loading this file and/or Garnet-xx-src
;;;
;;; The controlling variables are:
;;; 
;;;      load-clx-p          (Default: NIL => clx not loaded)
;;;      load-utils-p        (Default: T   => utilities loaded)
;;;      load-kr-p           (Default: T   => kr loaded)
;;;      load-kr-doc-p       (Default: NIL => kr-doc *NOT* loaded)
;;;      load-gworld-p       (Default: T   => gworld loaded for Mac)
;;;      load-gem-p          (Default: T   => gem loaded)
;;;      load-opal-p         (Default: T   => opal loaded)
;;;      load-inter-p        (Default: T   => interactors loaded)
;;;      load-multifont-p    (Default: NIL => multifont *NOT* loaded)
;;;      load-gesture-p      (Default: NIL => gestures *NOT* loaded)
;;;      load-ps-p           (Default: T   => ps loaded)
;;;      load-aggregadgets-p (Default: T   => aggregadgets loaded)
;;;      load-aggregraphs-p  (Default: NIL => aggregraphs *NOT* loaded)
;;;      load-gadgets-p      (Default: NIL => gadgets *NOT* loaded)
;;;      load-debug-p        (Default: T   => debugging tools loaded)
;;;      load-demos-p        (Default: NIL => demos *NOT* loaded)
;;;      load-c32-p          (Default: NIL => C32 *NOT* loaded)
;;;      load-gilt-p         (Default: NIL => gilt *NOT* loaded)
;;;      load-lapidary-p     (Default: NIL => lapidary *NOT* loaded)
;;;      load-protected-eval-p (Default: <lisp dependent> =>
;;;                            protected-eval loaded for lisps with
;;;                            multi-processor support.
;;;
;  (setq load-utils-p T)
;  (setq load-kr-p T)
;  (setq load-kr-doc-p NIL)
;  (setq load-gworld-p T)
;  (setq load-gem-p T)
;  (setq load-opal-p T)
;  (setq load-inter-p T)
;  (setq load-multifont-p NIL)
;  (setq load-gesture-p NIL)
;  (setq load-ps-p T)
;  (setq load-aggregadgets-p T)
;  (setq load-aggregraphs-p NIL)
;  (setq load-debug-p #+garnet-debug T #-garnet-debug NIL)
;  (setq load-gadgets-p NIL)
;  (setq load-demos-p NIL)
;  (setq load-protected-eval-p T)
;  (setq load-lapidary-p NIL)
;  (setq load-gilt-p NIL)
;  (setq load-c32-p NIL)

;;; If we are only loading KR, we don't need to load the X stuff.
;;; This flag controls that.
;(setq *kr-only* t)
