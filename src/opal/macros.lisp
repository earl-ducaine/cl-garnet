;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Opal:Macros.Lisp
;;;
;;; This file contains all the defmacros which are used by Opal.
;;;
;;; Change Log:
;;;     date     who    what
;;;     ----     ---    ----
;;;   21-May-93 amickish Restored old definition of Black-XOR-Hack (known
;;;                      previously as hack-for-black-xor-on-color-screen)
;;;                      and renamed to HP-XOR-Hack
;;;   17-Apr-93 amickish Moved aggrelist macros here from virtual-aggs
;;;   12-Apr-93 koz     Moved set-*-style fns to new-defs.lisp, since they
;;;                     use macros defined there.
;;;    6-Apr-93 koz     Changed with-*-styles macros to set-*-style FUNCTIONS.
;;;                     Also REMOVED clip-mask handling from set-*-style!
;;;                     Also changed black-xor-hack from fn to macro.
;;;                     And nixed unused macros "old", "old-value", "old-valid"
;;;    5-Apr-93 koz     Fixed "dothings" to handle case when one of the
;;;                     'things' is NIL (old version aborted at that point).
;;;   27-Mar-93 koz     Added "dothings" macro (to avoid using "flet"
;;;			in "update-window" because of Allegro space leak)
;;;   13-Jan-93 amickish  Removed Fix-Properties (obsolete)
;;;    5-Jan-93 amickish  Rewrote hack-for-black-xor-on-color-screen
;;;   31-Dec-92 koz     Fixed :clip-mask branch of set-gc
;;;   20-Nov-92 amickish  Bound THE-SCHEMA in each macro so that SCHEMA is not
;;;                     evaluated twice
;;;    6-Oct-92 koz     Added fix-update-slots
;;;    5-Oct-92 koz/amickish  Added get-old-thickness
;;;   27-May-92  dzg    In with-line-style and with-filling-style, exit
;;;			immediately if draw-function is boole-2 = :no-op.
;;;   19-Feb-92  ecp    Implemented double-clip-masks as list of length 8
;;;   24-Jan-92  ecp    Changed with-filling-style and with-line-style
;;;			to draw xor objects correctly when *black* = 0.
;;;    9-Dec-91  ecp    Rewrote :clip-mask branch of set-gc to only
;;;                     copy clip-mask if it has changed.
;;;   25-Nov-91  koz    changed get-bbox-vals to set-frr-bbox
;;;   25-Nov-91  koz    eliminated fix-properties-and-validate (yeah!)
;;;    6-Nov-91  ecp    Made move-component a method.
;;;    4-Oct-91  amick  Added set-styles and get-bbox-vals macros
;;;    1-Mar-91  ecp    If a white xor-ed object is drawn on a color
;;;			screen for which *black* is 0, then it must
;;;			be drawn black instead.
;;;   13-Mar-91  ecp    Same as 3-Aug-90 change, but also don't do a total
;;;                     update if only :cursor is changed.
;;;    7-Mar-91  ecp    If a black xor-ed object is drawn on a color
;;;			screen for which *black* is 0, then it must
;;;			be drawn white instead.
;;;    3-Aug-90  ecp    In fix-properties-and-validate, do not return t if
;;;			only :top or :left has been changed (since then we
;;;			do not want a total update).
;;;   11-Jul-90  ecp    new :destroy-me method
;;;    9-Apr-90  cook   Indented format statement in get-stipple-pixmap-schema
;;;   19-Mar-90  ecp    Changed tile to stipple
;;;   12-Mar-90  ecp    Fixed bug so gray lines are possible.
;;;   13-Feb-90  ecp	Implemented color.
;;;   13-Feb-90  dzg    Certain macros, such as gv-bottom, have been
;;;			converted to defuns for efficiency.  They are
;;;			now declared in basics.lisp.
;;;   25-Jan-90  ecp    Image-p is not in the R4 release of CLX.
;;;   14-Jun-89  koz    Created.  Simply extracted all defmacros from all the
;;;			Opal files.  No modifications were made to them.

(in-package "OPAL")

;;; General Use
;;
;; FMG Change to inline functions wherever possible.
;;

(defmacro add-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :add-component the-schema ,@args)))

(defmacro remove-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :remove-component the-schema ,@args)))

(defmacro move-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :move-component the-schema ,@args)))

(defmacro do-all-components (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :do-all-components the-schema ,@args)))

(defmacro do-components (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :do-components the-schema ,@args)))

;;;---------------------------------------------------------------------------
;;; Added do-items because it would be very helpful to operate over the
;;; items of a virtual-aggregate or an aggrelist. [2003/09/16:rpg]
;;;---------------------------------------------------------------------------

(defmacro do-items (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :do-items the-schema ,@args)))

(defmacro point-to-component (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :point-to-component the-schema ,@args)))

(defmacro point-to-leaf (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :point-to-leaf the-schema ,@args)))

(defmacro fix-update-slots (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :fix-update-slots the-schema ,@args)))

(defmacro initialize (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :initialize the-schema ,@args)))

(defmacro destroy-me (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :destroy-me the-schema ,@args)))

(defmacro destroy (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :destroy the-schema ,@args)))

(defmacro rotate (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :rotate the-schema ,@args)))

(defmacro update (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :update the-schema ,@args)))

(defmacro draw (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :draw the-schema ,@args)))

(defmacro point-in-gob (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :point-in-gob the-schema ,@args)))

(defmacro set-styles (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :set-styles the-schema ,@args)))

(defmacro set-frr-bbox (schema &rest args)
  `(let ((the-schema ,schema))
    (kr-send the-schema :set-frr-bbox the-schema ,@args)))

(defmacro dothings ((varname &rest things) &body body)
 "Same as 'dolist', except 'things' are not a list.  Does not cons."
 (let ((count (length things))
       (tagname   (gensym "TOP-TAG"))
       (countname (gensym "COUNT"))
       case-entries)
  (dolist (thing things)
    (push (list (decf count) thing) case-entries))
  (setq case-entries (nreverse case-entries))
  `(let ((,countname ,(length things))
         ,varname)
    (tagbody
      ,tagname
      (unless (zerop ,countname)
	(setq ,varname (case (decf ,countname) ,@case-entries))
        ,@body
        (go ,tagname))))))



;;; For "objects.lisp"
;;


(declaim (inline get-thickness))	   
(defun get-thickness (gob)
  (let* ((line-style (g-value gob :line-style))
	 (thickness  (and line-style (g-value line-style :line-thickness))))
    (if thickness (max thickness 1) 0)))

;; This version of get-thickness aref's the update-vals array for the
;; line thickness, rather than g-valuing the :line-style slot.  Thus, we get
;; the "old" line thickness.
(declaim (inline get-old-thickness))
(defun get-old-thickness (gob line-style-index update-vals)
  (declare (ignore gob))
  (let* ((line-style (aref update-vals line-style-index))
	 (thickness  (and line-style (g-value line-style :line-thickness))))
    (if thickness (max thickness 1) 0)))

(declaim (inline point-in-rectangle))
(defun point-in-rectangle (x y left top right bottom)
  (and (<= left x right)
       (<= top y bottom)))

;;;  TEXT MACROS

(declaim (inline the-width))
(defun the-width (text-extents)
  (first text-extents))

(declaim (inline the-actual-ascent))
(defun the-actual-ascent (text-extents)
  (second text-extents))

(declaim (inline the-actual-descent))
(defun the-actual-descent (text-extents)
  (third text-extents))

(declaim (inline the-left-bearing))
(defun the-left-bearing (text-extents)
  (fourth text-extents))

(declaim (inline the-right-bearing))
(defun the-right-bearing (text-extents)
  (fifth text-extents))

(declaim (inline the-font-ascent))
(defun the-font-ascent (text-extents)
  (sixth text-extents))

(declaim (inline the-font-descent))
(defun the-font-descent (text-extents)
  (seventh text-extents))

;;;   IMAGE MACROS
(declaim (inline read-image))
(defun read-image (pathname &optional root-window)
  (gem:read-an-image (or root-window
			 (g-value device-info :current-root))
		     pathname))

(declaim (inline write-image))
(defun write-image (pathname image &optional root-window)
  (gem:write-an-image (or root-window
			  (g-value device-info :current-root))
		      pathname image))


;; For "basics.lisp"

;;; The accessors for the sides of the gob adjust both the dimensions, and
;;  position of the gob based on the given value.

(declaim (inline left-side))
(defun left-side (gob)
  (g-value gob :left))

(declaim (inline right-side))
(defun right-side (gob)
  (right gob))

(declaim (inline top-side))
(defun top-side (gob)
  (g-value gob :top))

(declaim (inline bottom-side))
(defun bottom-side (gob)
  (bottom gob))


;;; For "text-fonts.lisp"

;; Font-From-File

(declaim (inline extract-dir))
(defun extract-dir (font-name)
  (subseq font-name 0 (1+ (position #\/ font-name :from-end t))))

(declaim (inline extract-font-name))
(defun extract-font-name (font-name)
  (subseq  font-name
	   (1+ (position #\/ font-name :from-end t))
	   (position #\. font-name :from-end t)))

;; For "windows.lisp"

(declaim (inline get-parent-win))
(defun get-parent-win (a-window display-info)
  (let ((win-parent (g-value a-window :parent)))
    (if win-parent
	(g-value win-parent :drawable)
	(display-info-root-window display-info))))

;; For "clean-up.lisp"

(defmacro opal-window (window-pair)
  `(cdr ,window-pair))

(defmacro clx-window (window-pair)
  `(car ,window-pair))


;; For aggregadgets, aggrelists, etc.

(defmacro add-item (schema &rest args)
 `(let ((the-schema ,schema))
   (kr-send the-schema :add-item the-schema ,@args)))

(defmacro change-item (schema &rest args)
  "
Change-Item puts the specified item in the :items list, replacing the
item that was previously in the specified position.
    agg  - the aggrelist or gadget to be changed
    item - the new item to put in the :items list
    n    - the position of the old item to be replaced"
  `(let ((the-schema ,schema))
    (kr-send the-schema :change-item the-schema ,@args)))

(defmacro remove-item (schema &rest args)
 `(let ((the-schema ,schema))
   (kr-send the-schema :remove-item the-schema ,@args)))

