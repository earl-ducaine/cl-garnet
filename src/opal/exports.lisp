;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;
;; The Garnet User Interface Development Environment.
;;
;; This code was written as part of the Garnet project at Carnegie
;; Mellon University, and has been placed in the public domain.


;;  This file contains the export list for Opal.

(in-package :opal)

(defvar *debug-opal-mode* nil)

;;; This is the export list for as much of OPAL as I could find
;;; Import some stuff from GEM that used to be in OPAL.
(eval-when (:execute :load-toplevel :compile-toplevel)
  (import '(gem:Display-Info
	    gem:Make-Display-Info gem:Copy-Display-Info
	    gem:Display-Info-Display gem:Display-Info-Screen gem:Display-Info-Root-Window
	    gem:Display-Info-Line-Style-GC gem:Display-Info-Filling-Style-GC
;;;	    gem:*update-lock*
;;;	    gem:*screen-width* gem:*screen-height*

	    gem:*Fixed-Font-Family* gem:*Serif-Font-Family* gem:*Sans-Serif-Font-Family*
	    gem:*Small-Font-Size* gem:*Medium-Font-Size*
	    gem:*Large-Font-Size* gem:*Very-Large-Font-Size*
	    gem:*Small-Font-Point-Size* gem:*Medium-Font-Point-Size*
	    gem:*Large-Font-Point-Size* gem:*Very-Large-Font-Point-Size*
	    gem:default-font-from-file)
	  (find-package "OPAL")))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(*debug-opal-mode* bottom right center-x center-y
	    gv-bottom gv-right gv-center-x gv-center-y
	    gv-center-x-is-center-of gv-center-y-is-center-of
	    gv-right-is-left-of gv-bottom-is-top-of
	    q-max q-min q-abs
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

	    ;; Multifont stuff.
	    MULTIFONT-TEXT

	    SET-CURSOR-VISIBLE
	    SET-CURSOR-TO-X-Y-POSITION
	    SET-CURSOR-TO-LINE-CHAR-POSITION
	    GET-CURSOR-LINE-CHAR-POSITION
	    GO-TO-NEXT-CHAR
	    GO-TO-PREV-CHAR
	    GO-TO-NEXT-WORD
	    GO-TO-PREV-WORD
	    GO-TO-NEXT-LINE
	    GO-TO-PREV-LINE
	    GO-TO-BEGINNING-OF-TEXT
	    GO-TO-END-OF-TEXT
	    GO-TO-BEGINNING-OF-LINE
	    GO-TO-END-OF-LINE

	    FETCH-NEXT-CHAR
	    FETCH-PREV-CHAR

	    TOGGLE-SELECTION
	    SET-SELECTION-TO-X-Y-POSITION
	    SET-SELECTION-TO-LINE-CHAR-POSITION
	    GET-SELECTION-LINE-CHAR-POSITION
	    CHANGE-FONT-OF-SELECTION
	    CHANGE-COLOR-OF-SELECTION

	    ADD-CHAR
	    DELETE-CHAR
	    DELETE-PREV-CHAR
	    INSERT-STRING
	    ADD-OBJECT
	    SEARCH-FOR-MARK
	    SEARCH-BACKWARDS-FOR-MARK
	    BETWEEN-MARKS-P
	    MARK
	    INSERT-MARK
	    INSERT-TEXT
	    DELETE-SUBSTRING
	    DELETE-WORD
	    DELETE-PREV-WORD
	    KILL-REST-OF-LINE

	    COPY-SELECTED-TEXT
	    DELETE-SELECTION

	    SET-TEXT
	    GET-STRING
	    GET-TEXT
	    GET-OBJECTS

	    NOTICE-RESIZE-OBJECT

	    TEXT-TO-PURE-LIST
	    PURE-LIST-TO-TEXT
	    TEXT-TO-STRING
	    CONCATENATE-TEXT
	    EMPTY-TEXT-P

	    ;; Pixmap stuff.
	    pixmap write-xpm-file read-xpm-file
	    create-pixmap-image window-to-pixmap-image

	    ;; Utils.
	    make-image get-garnet-bitmap directory-p
            time-to-string clip-and-map drawable-to-window)))
