;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id$


;;; This file contains all the defvars, defconstants, defstructs, etc.,
;;  which are used by Opal.  This does not contain any defmacros, however.
;;  This file also contains the export list for Opal.


(in-package "OPAL")

;;; This is the export list for *some* of OPAL
(eval-when (:execute :load-toplevel :compile-toplevel)
  (import '(gem:Display-Info
	    gem:Make-Display-Info gem:Copy-Display-Info
	    gem:Display-Info-Display gem:Display-Info-Screen gem:Display-Info-Root-Window
	    gem:Display-Info-Line-Style-GC gem:Display-Info-Filling-Style-GC
	    gem:*update-lock* gem:*screen-width* gem:*screen-height*

	    gem:*Fixed-Font-Family* gem:*Serif-Font-Family* gem:*Sans-Serif-Font-Family*
	    gem:*Small-Font-Size* gem:*Medium-Font-Size*
	    gem:*Large-Font-Size* gem:*Very-Large-Font-Size*
	    gem:*Small-Font-Point-Size* gem:*Medium-Font-Point-Size* 
	    gem:*Large-Font-Point-Size* gem:*Very-Large-Font-Point-Size*
	    gem:default-font-from-file
	    ) (find-package "OPAL"))
)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(bottom right center-x center-y
	    gv-bottom gv-right gv-center-x gv-center-y
	    gv-center-x-is-center-of gv-center-y-is-center-of
	    gv-right-is-left-of gv-bottom-is-top-of
	    top-side left-side bottom-side right-side
	    center set-center
	    bounding-box set-bounding-box
	    set-position set-size
	    draw erase rotate
	    initialize calculate-bounding-box point-in-gob
	    halftone halftone-darker halftone-lighter
	    halftone-image halftone-image-darker halftone-image-lighter
	    read-image write-image
	    add-component remove-component move-component
	    add-components remove-components remove-all-components
	    do-components do-all-components
	    point-to-component point-to-leaf
	    set-aggregate-hit-threshold
	    update destroy destroy-me
	    raise-window lower-window iconify-window deiconify-window
	    zoom-window fullzoom-window
	    
	    ;; Class names
	    aggregate view-object graphical-object line rectangle
	    roundtangle multipoint polyline polygon text bitmap arc oval
	    circle arrowhead multi-text cursor-multi-text
	    
	    line-style default-line-style filling-style default-filling-style
	    font cursor-text graphic-quality font-from-file cursor-font
	    arrow-cursor arrow-cursor-mask arrow-pair
	    hourglass-cursor hourglass-cursor-mask hourglass-pair
	    garbage-cursor garbage-cursor-mask garbage-pair
	    with-hourglass-cursor with-cursor default-font
;;;	    display-info-display display-info-screen
;;;	    display-info-root-window display-info-line-style-gc
;;	    display-info-filling-style-gc
	    device-info
	    convert-coordinates get-cursor-index string-width string-height
	    change-cursors restore-cursors char-width
	    move-cursor-down-one-line
	    move-cursor-up-one-line
	    move-cursor-to-beginning-of-line
	    move-cursor-to-end-of-line
	    
	    Get-X-Cut-Buffer Set-X-Cut-Buffer ; for interactors' use
	    leaf-objects-in-rectangle components-in-rectangle obj-in-rectangle
	    
	    ;; filling and line style constants
	    no-fill black-fill white-fill
	    gray-fill light-gray-fill dark-gray-fill
	    red-fill green-fill blue-fill yellow-fill
	    cyan-fill orange-fill purple-fill
	    motif-gray-fill motif-blue-fill motif-orange-fill motif-green-fill
	    motif-light-gray-fill motif-light-blue-fill motif-light-orange-fill
	    motif-light-green-fill
	    
	    make-filling-style
	    diamond-fill
	    
	    no-line thin-line line-0 line-1 line-2 line-4 line-8 gray-line
	    dotted-line dashed-line 
	    red-line green-line blue-line yellow-line
	    cyan-line orange-line purple-line white-line
	    
	    ;; size of screen
	    *screen-width* *screen-height*
	    
	    ;; Colors
	    color white black red green blue cyan yellow orange purple
	    motif-gray motif-blue motif-orange motif-green motif-light-gray
	    motif-light-blue motif-light-orange motif-light-green
	    color-to-index
	    
	    ;; From Clean-Up.Lisp
	    clean-up change-garnet-display update-all reset-cursor
	    
	    ;; From open-and-close.lisp
	    disconnect-garnet reconnect-garnet
	    
	    ;; From process.lisp
	    launch-main-event-loop-process
	    kill-main-event-loop-process
	    main-event-loop-process-running-p
	    running-main-event-loop-process-elsewhere-p
	    
	    ;; From virtual-aggregates.lisp
	    virtual-aggregate remove-item add-item change-item point-to-rank
	    recalculate-virtual-aggregate-bboxes do-in-clip-rect
	    do-items ;; [2003/09/16:rpg]

	    
	    get-standard-font

	    ;; Stuff that should be exported IMHO (fmg).
	    *garnet-windows*
	    )))



;;; DefConstants
;;

(defconstant +twopi+ (min (* 2 pi) (coerce (* 2 pi) 'short-float)))


;;; DefParameters


;; The :current-root slot of the following schema indicates the current
;; device.  This is used for all calls to Gem which occur in places where
;; explicit device information is not available.
;; The :active-devices slot contains the list of all the devices that
;; have been initialized.
;;
(create-schema 'DEVICE-INFO
  (:current-root NIL)
  (:active-devices NIL))



;;; Moved all the X display / screen / etc. stuff into x.lisp to
;;  respect the modularity of the code.

;; (defvar *default-x-display-name*)
;; (setf (documentation '*default-x-display-name* 'variable)
;;       "This is an unfortunately misnamed entity, since it is
;; actually an X HOSTNAME and not a display name in the sense of
;; being a string display designator.")
;; (defvar *default-x-display*)
;; (defvar *default-x-display-number*)
;; (defvar *default-x-screen-number*)
;; (defvar *default-x-screen*)
;; (defvar *default-x-root*)
;; (defvar *default-x-colormap*)
;; (defvar *screen-width*)
;; (defvar *screen-height*)
;; (defvar *white*)
;; (defvar *black*)
;; (defvar *function-alist*)

(defvar *colormap-index-table*
  ;;arguments to make-hash-table NOT well thought out! [1995/09/12:goldman]
  (make-hash-table :size 256))

;;; This is also called in reconnect-garnet.
;;;
(defun initialize-device-values (full-display-name root-window)
  ;; Set up all the Opal variables used to identify display, screen, etc.
  ;; Unfortunately, these are needed by discard-all-pending-events (in
  ;; process.lisp), which is called by launch-main-event-loop-process.
  (gem:set-device-variables root-window full-display-name)
  (gem:set-screen-color-attribute-variables root-window)
  (with-constants-disabled
    (s-value opal::COLOR :color-p gem:*color-screen-p*))
  )
  


;;; DefVars
;;

(declaim (fixnum *halftone-table-size*))
(defvar *halftone-table-size* 17)
(defvar *halftone-table* nil)		; used to be set to (build-halftone-table)
					; but now that's a forward reference.  So,
					; now we setq this after defining that fn.

(defvar *default-text-extents* (make-list 9 :initial-element 0))

(defvar no-fill nil)
(defvar no-line nil)

(defvar *garnet-windows* NIL)

;; debugging tools
(defvar *event-debug* nil)
(defvar *expose-throw-aways* 0)

(declaim (fixnum *opal-window-count*))
(defvar *opal-window-count* 0)

(defvar diamond-fill NIL)		;; set in halftones.lisp

;;; DefStructs
;;

;; This defstruct generates the functions Make-Halftone, Copy-Halftone,
;; Halftone-Percent, Halftone-Device-Image, Halftone-Filling-Style, and
;; Halftone-P.
(defstruct (HALFTONE (:print-function halftone-print))
  (percent 0)
  (device-image nil)
  (filling-style nil))

;;; This defstruct generates the functions Make-Cut-String, Copy-Cut-String,
;;; Cut-String-String, Cut-String-Width, and Cut-String-Left-Bearing.
(defstruct CUT-STRING
  (string)
  (width 0 :type fixnum)
  (left-bearing))



;;; DefSetfs
;;

;;; Accessors that do calculation from basic gob properties

;; The accessors for the bottom and right of the gob, make it easier to
;; adjust the far side of the gob's bounding box.

(defsetf bottom (gob) (value)
  `(setf (g-value ,gob :top) (1+ (- ,value (the fixnum (g-value ,gob :height))))))

(defsetf right (gob) (value)
  `(setf (g-value ,gob :left) (1+ (- ,value (the fixnum (g-value ,gob :width))))))

;; The accessors for the sides of the gob adjust both the dimensions, and
;; position of the gob based on the given value.

(defsetf left-side (gob) (value)
  `(progn
     (setf (g-value ,gob :width)
           (- (the fixnum (g-value ,gob :width)) (- ,value (the fixnum (g-value ,gob :left)))))
     (setf (g-value ,gob :left) ,value)))

(defsetf right-side (gob) (value)
  `(setf (g-value ,gob :width)
         (+ (the fixnum (g-value ,gob :width)) (- ,value (right ,gob)))))

(defsetf top-side (gob) (value)
  `(progn
     (setf (g-value ,gob :height)
           (- (the fixnum (g-value ,gob :height)) (- ,value (the fixnum (g-value ,gob :top)))))
     (setf (g-value ,gob :top) ,value)))

(defsetf bottom-side (gob) (value)
  `(setf (g-value ,gob :height)
         (+ (the fixnum (g-value ,gob :height)) (- ,value (bottom ,gob)))))

;; The following allow access and setting to the gobs center
;; position.

(defsetf center-x (gob) (value)
  `(setf (g-value ,gob :left)
         (- ,value (truncate (the fixnum (g-value ,gob :width)) 2))))

(defsetf center-y (gob) (value)
  `(setf (g-value ,gob :top)
         (- ,value (truncate (the fixnum (g-value ,gob :height)) 2))))
