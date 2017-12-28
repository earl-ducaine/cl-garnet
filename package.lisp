


(defpackage :garnet-utils
  (:use :common-lisp)
  (:nicknames :gu)
  (:export *garnet-break-key*
	   2pi -2pi
	   add-to-list
	   black
	   directory-p
	   do2lists
	   dolist2
	   m
	   m1
	   pi/2
	   pi3/2
	   safe-functionp
	   short-pi
	   str
	   string+
	   till
	   until
	   verify-binding
	   white
	   while))

(defpackage :kr-debug
  (:use :common-lisp))

(defpackage :kr
  (:use :common-lisp :kr-debug)
  (:export schema
	   create-instance
	   create-prototype
	   create-relation
	   create-schema
	   formula
	   o-formula
	   schema-p
	   relation-p
	   is-a-p
	   has-slot-p
	   formula-p
	   s-value
	   g-value
	   g-cached-value
	   g-local-value
	   gv
	   gvl
	   gv-local
	   get-value
	   get-local-value
	   dovalues
	   doslots
	   define-method
	   kr-send
	   call-prototype-method
	   apply-prototype-method
	   method-trace
	   *print-as-structure*
	   with-constants-disabled
	   with-types-disabled
	   with-demons-disabled
	   with-demon-disabled
	   with-demon-enabled
	   change-formula
	   move-formula
	   recompute-formula
	   copy-formula
	   kr-path
	   mark-as-changed
	   mark-as-invalid
	   ps
	   call-on-ps-slots
	   name-for-schema
	   declare-constant
	   slot-constant-p
	   destroy-slot
	   destroy-schema
	   destroy-constraint
	   def-kr-type
	   g-type
	   s-type
	   check-slot-type
	   kr-boolean
	   get-slot-doc
	   set-slot-doc
	   get-type-documentation
	   set-type-documentation
	   get-type-definition
	   get-declarations
	   get-slot-declarations
	   g-formula-value
	   s-formula-value
	   self-old-value))

(defpackage :jewel
  (:use :common-lisp :kr :kr-debug)
  (:export *exposure-event-mask*
	   *fixed-font-family*
	   *large-font-point-size*
	   *large-font-size*
	   *medium-font-point-size*
	   *medium-font-size*
	   *sans-serif-font-family*
	   *serif-font-family*
	   *small-font-point-size*
	   *small-font-size*
	   *very-large-font-point-size*
	   *very-large-font-size*
	   *black*
	   *color-screen-p*
	   *function-alist*
	   *read-write-colormap-cells-p*
	   *screen-height*
	   *screen-width*
	   *update-lock*
	   *white*
	   device-info
	   copy-display-info
	   default-font-from-file
	   display-info
	   display-info-display
	   display-info-filling-style-gc
	   display-info-line-style-gc
	   display-info-root-window
	   display-info-screen
	   init-device
	   make-display-info))

(defpackage :gem
  (:use :common-lisp :kr :kr-debug)
  (:export *exposure-event-mask*
	   *fixed-font-family*
	   *large-font-point-size*
	   *large-font-size*
	   *medium-font-point-size*
	   *medium-font-size*
	   *sans-serif-font-family*
	   *serif-font-family*
	   *small-font-point-size*
	   *small-font-size*
	   *very-large-font-point-size*
	   *very-large-font-size*
	   *black*
	   *color-screen-p*
	   *function-alist*
	   *read-write-colormap-cells-p*
	   *screen-height*
	   *screen-width*
	   *update-lock*
	   *white*
	   device-info
	   copy-display-info
	   default-font-from-file
	   display-info
	   display-info-display
	   display-info-filling-style-gc
	   display-info-line-style-gc
	   display-info-root-window
	   display-info-screen
	   init-device
	   make-display-info
	   READ-AN-IMAGE
	   WRITE-AN-IMAGE


	   WINDOW-FROM-DRAWABLE
	   MAX-CHARACTER-ASCENT
	   MAX-CHARACTER-DEscent
	   MAKE-FONT-NAME

	   FONT-TO-INTERNAL

	   CLEAR-AREA)
  (:import-from :garnet-utils :black)
  (:import-from :garnet-utils :white))

(defpackage :interactors
  (:use :common-lisp :kr)
  (:nicknames :inter)
  (:import-from :garnet-utils *garnet-break-key*)
  (:export *garnet-break-key*
	   *left-button* *trans-from-file*
	   ;; for animation
	   start-animator Stop-Animator abort-animator animator-interactor
	   animator-wrap animator-bounce Reset-All-Timer-Processes
	   ;; entering and leaving main event loop
	   main-event-loop
	   exit-main-event-loop
	   ;; waiting for an interaction to complete
	   Interaction-Complete Wait-Interaction-Complete
	   ;;explicit control of interactors
	   Change-Active Start-Interactor Abort-Interactor Stop-Interactor
	   ;; Called by KR when :active or :window changes:
	   Notice-Interactor-Slot-Changed
	   ;; support for multiple priority levels
	   priority-level normal-priority-level high-priority-level
	   running-priority-level priority-level-list
	   ;; the next ones are for debugging
	   Reset-Inter-Levels Print-Inter-Levels Print-Inter-Windows
	   trace-inter untrace-inter *debug-next-inter* Do-All-Interactors
	   ;; interactor event structure (copy of X's event structure)
	   *Current-Event*
	   event-x
	   event-y
	   event-char
	   event-code
	   event-mousep
	   event-downp
	   event-window
	   event-timestamp
	   make-event
	   ;; for controlling double clicks
	   *double-click-time*
	   ;; key translations for text-inter
	   Bind-Key Unbind-Key Unbind-All-Keys Set-Default-Key-Translations
	   ;;transcripting functions
	   Transcript-Events-To-File Close-Transcript
	   Transcript-Events-From-File
	   ;; useful utility functions
	   Clip-And-Map Beep Insert-Text-Into-String Warp-Pointer
	   Pop-Up-Win-And-Start-Interactor

	   *default-global-accelerators*
	   *global-accelerators*
	   *global-first-accelerators*
	   *int-debug*
	   *int-trace*
	   add-global-accelerator
	   add-window-accelerator
	   angle-interactor
	   button-interactor
	   clear-global-accelerators
	   clear-window-accelerators
	   default-global-accelerators
	   deselectobj
	   interactor
	   interactor-window
	   menu-interactor
	   move-grow-interactor
	   multifont-text-interactor
	   remove-global-accelerator
	   remove-window-accelerator
	   return-final-selection-objs
	   scroll-wheel-interactor
	   selectobj
	   text-interactor
	   two-point-interactor
	   ))

(defpackage :demo
  (:use :common-lisp :kr))

(defpackage :garnet-user
  (:use common-lisp kr garnet-utils)
  (:export go-demos
	   *debug-kr-mode*))

(defpackage :garnet-truetype
  (:documentation
   "package contains api for truetype text rendering using clx,
    xrender.  glyphs information is obtained by zpb-ttf. font
    rasterization is made by cl-vectors.")
  (:nicknames :garnet-xft)
  (:use :cl :kr)
  (:export
   :font
   :font-family
   :font-subfamily
   :font-size
   :font-underline
   :font-strikethrough
   :font-overline
   :font-background
   :font-foreground
   :font-overwrite-gcontext
   :cache-font-file
   :*font-dirs*
   :drawable-screen
   :font-ascent
   :font-descent
   :text-bounding-box
   :xmin
   :ymin
   :xmax
   :ymax
   :screen-default-dpi
   :screen-dpi
   :draw-text
   :draw-text-line
   :get-font-families
   :get-font-subfamilies
   :text-height
   :text-width
   :text-line-bounding-box
   :text-line-width
   :text-line-height
   :font-line-gap
   :baseline-to-baseline
   :font-antialias
   :font-lines-height
   :cache-fonts
   :font-equal))

(defpackage :garnet-gadgets
  (:use common-lisp kr)
  (:nicknames gg)
  (:export *application-long-name*
   *application-short-name*
   *normal-cursor-pair*
   *user-type*
   call-displayer
   call-prompter
   call-selector
   careful-eval
   careful-eval-formula-lambda
   careful-read-from-string
   careful-string-eval
   demo-motif-text-button
   demo-motif-text-button-panel
   display-error
   display-error-and-wait
   display-query
   display-query-and-wait
   displayer
   do-prompt
   error-prompter-gadget
   garnet-error-handler
   garnet-protected-eval
   garnet-protected-read
   garnet-protected-read-from-string
   garnet-user-error-hander
   insert-text-into-box
   insert-text-into-sis
   motif-error-gadget
   motif-query-gadget
   motif-text-button
   motif-text-button-panel
   motif-text-buttons-go
   motif-text-buttons-stop
   motif-text-buttons-top-agg
   motif-text-buttons-win
   mouseline-go
   mouseline-stop
   prompter
   prompter-gadget-go
   prompter-gadget-stop
   prompting-error-handler
   prompting-protected-eval
   prompting-protected-read
   prompting-protected-read-from-string
   protect-errors
   protected-eval
   protected-eval-error-gadget
   protected-read
   protected-read-from-string
   scrolling-input-string
   scrolling-input-string-go
   scrolling-input-string-obj
   scrolling-input-string-stop
   scrolling-input-string-top-agg
   scrolling-input-string-win
   selector
   text-button
   text-button-obj
   text-button-obj1
   text-button-panel
   text-buttons-go
   text-buttons-obj
   text-buttons-stop
   text-buttons-top-agg
   text-buttons-win
   v-scroll-bar
   v-scroll-go
   v-scroll-obj
   v-scroll-stop
   v-scroll-top-agg
   v-scroll-win
   with-garnet-error-handling
   with-garnet-user-error-handling
   with-normal-cursor
   with-protected-errors))

(defpackage :test
  (:use common-lisp))

(defpackage :opal
  (:use common-lisp kr)
  (:import-from garnet-utils black)
  (:import-from garnet-utils white)
  (:export *debug-opal-mode*
	   *garnet-windows*
	   add-char
	   add-component
	   add-components
	   add-item
	   add-object
	   aggregadget
	   aggregate
	   arc
	   arrow-cursor
	   arrow-cursor-mask
	   arrow-pair
	   arrowhead
	   between-marks-p
	   bitmap
	   black
	   black-fill
	   blue
	   blue-fill
	   blue-line
	   bottom
	   bottom-side
	   bounding-box
	   calculate-bounding-box
	   center
	   center-x
	   center-y
	   change-color-of-selection
	   change-cursors
	   change-font-of-selection
	   change-garnet-display
	   change-item
	   char-width
	   circle
	   clean-up
	   clip-and-map
	   color
	   color-to-index
	   components-in-rectangle
	   concatenate-text
	   convert-coordinates
	   copy-selected-text
	   create-pixmap-image
	   cursor-font
	   cursor-multi-text
	   cursor-text
	   cyan
	   cyan-fill
	   cyan-line
	   dark-gray-fill
	   dashed-line
	   default-filling-style
	   default-font
	   default-line-style
	   deiconify-window
	   delete-char
	   delete-prev-char
	   delete-prev-word
	   delete-selection
	   delete-substring
	   delete-word
	   destroy
	   destroy-me
	   diamond-fill
	   directory-p
	   disconnect-garnet
	   do-all-components
	   do-components
	   do-in-clip-rect
	   do-items
	   dotted-line
	   draw
	   drawable-to-window
	   empty-text-p
	   erase
	   fetch-next-char
	   fetch-prev-char
	   filling-style
	   font
	   font-from-file
	   fullzoom-window
	   garbage-cursor
	   garbage-cursor-mask
	   garbage-pair
	   get-cursor-index
	   get-cursor-line-char-position
	   get-garnet-bitmap
	   get-objects
	   get-selection-line-char-position
	   get-standard-font
	   get-string
	   get-text
	   get-x-cut-buffer
	   go-to-beginning-of-line
	   go-to-beginning-of-text
	   go-to-end-of-line
	   go-to-end-of-text
	   go-to-next-char
	   go-to-next-line
	   go-to-next-word
	   go-to-prev-char
	   go-to-prev-line
	   go-to-prev-word
	   graphic-quality
	   graphical-object
	   gray-fill
	   gray-line
	   green
	   green-fill
	   green-line
	   gv-bottom
	   gv-bottom-is-top-of
	   gv-center-x
	   gv-center-x-is-center-of
	   gv-center-y
	   gv-center-y-is-center-of
	   gv-right
	   gv-right-is-left-of
	   gvl-sibling
	   halftone
	   halftone-darker
	   halftone-image
	   halftone-image-darker
	   halftone-image-lighter
	   halftone-lighter
	   hourglass-cursor
	   hourglass-cursor-mask
	   hourglass-pair
	   iconify-window
	   initialize
	   insert-mark
	   insert-string
	   insert-text
	   kill-main-event-loop-process
	   kill-rest-of-line
	   launch-main-event-loop-process
	   leaf-objects-in-rectangle
	   left-side
	   light-gray-fill
	   line
	   line-0
	   line-1
	   line-2
	   line-4
	   line-8
	   line-style
	   lower-window
	   main-event-loop-process-running-p
	   make-filling-style
	   make-image
	   mark
	   motif-blue
	   motif-blue-fill
	   motif-gray
	   motif-gray-fill
	   motif-green
	   motif-green-fill
	   motif-light-blue
	   motif-light-blue-fill
	   motif-light-gray
	   motif-light-gray-fill
	   motif-light-green
	   motif-light-green-fill
	   motif-light-orange
	   motif-light-orange-fill
	   motif-orange
	   motif-orange-fill
	   move-component
	   move-cursor-down-one-line
	   move-cursor-to-beginning-of-line
	   move-cursor-to-end-of-line
	   move-cursor-up-one-line
	   multi-text
	   multifont-text
	   multipoint
	   no-fill
	   no-line
	   notice-resize-object
	   obj-in-rectangle
	   orange
	   orange-fill
	   orange-line
	   oval
	   pixmap
	   point-in-gob
	   point-to-component
	   point-to-leaf
	   point-to-rank
	   polygon
	   polyline
	   pure-list-to-text
	   purple
	   purple-fill
	   purple-line
	   q-abs
	   q-max
	   q-min
	   raise-window
	   read-image
	   read-xpm-file
	   recalculate-virtual-aggregate-bboxes
	   reconnect-garnet
	   rectangle
	   red
	   red-fill
	   red-line
	   remove-all-components
	   remove-component
	   remove-components
	   remove-item
	   reset-cursor
	   restore-cursors
	   right
	   right-side
	   rotate
	   roundtangle
	   running-main-event-loop-process-elsewhere-p
	   search-backwards-for-mark
	   search-for-mark
	   set-aggregate-hit-threshold
	   set-bounding-box
	   set-center
	   set-cursor-to-line-char-position
	   set-cursor-to-x-y-position
	   set-cursor-visible
	   set-position
	   set-selection-to-line-char-position
	   set-selection-to-x-y-position
	   set-size
	   set-text
	   set-x-cut-buffer
	   string-height
	   string-width
	   text
	   text-to-pure-list
	   text-to-string
	   thin-line
	   time-to-string
	   toggle-selection
	   top-side
	   update
	   update-all
	   view-object
	   virtual-aggregate
	   white
	   white-fill
	   white-line
	   window-to-pixmap-image
	   with-cursor
	   with-hourglass-cursor
	   write-image
	   write-xpm-file
	   yellow
	   yellow-fill
	   yellow-line
	   zoom-window))

(defpackage :garnet-debug
  (:use :common-lisp :kr :opal) (:nicknames :gd))

(defpackage :gilt
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :c32
  (:use common-lisp kr))

(defpackage :lapidary
  (:use common-lisp kr)
  (:export do-go do-stop fix-it clean-up))

(defpackage :agate
  (:use :common-lisp :kr))

(defpackage :demo-3d
  (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :xomax
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-multiwin
  (:use kr common-lisp)
  (:export do-go do-stop))

(defpackage :demo-multifont
  (:use common-lisp kr)
  (:export do-go do-stop))

(defpackage :demo-animator (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-angle (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-othello (:use :kr :common-lisp) (:nicknames :doth)
	    (:export do-go do-stop start-game stop-game set-score))

(defpackage :demo-pixmap (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-arith (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-schema-browser (:use :common-lisp :kr)
	    (:export do-go do-stop schema-browser schema-browser-win
		     schema-browser-top-agg))
(defpackage :demo-array (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-scrollbar (:use :common-lisp :kr)
	    (:export do-go do-stop
		     mac-obj mac-go mac-stop
		     open-obj open-go open-stop
		     next-obj next-go next-stop
		     motif-obj motif-go motif-stop))
(defpackage :demo-clock (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-sequence (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-editor (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-text (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-file-browser (:use :common-lisp :kr)
	    (:export do-go do-stop file-browser file-browser-win
		     file-browser-top-agg))
(defpackage :demo-truck (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-gadgets (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-twop (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-gesture (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-unistrokes
  (:use :common-lisp :kr :inter)
  (:export do-go do-stop))

(defpackage :demo-graph (:use :common-lisp :kr)
	    (:export do-go do-stop schema-graph
		     demo-graph-error-gadget root-box relayout
		     demo-graph-win))

(defpackage :demo-virtual-agg
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-grow (:use :kr :common-lisp) (:export do-go do-stop))

(defpackage :demo-xasperate
  (:use :common-lisp :kr)
  (:export do-go do-stop))

(defpackage :demo-logo
  (:use :common-lisp :kr :garnet-utils)
  (:export do-go do-stop re-animate))

(defpackage :demos-controller
  (:use :common-lisp :kr)
  (:export do-go do-stop message))

(defpackage :demo-manyobjs
  (:use :common-lisp :kr)
  (:export do-go do-stop move))

(defpackage :demo-menu (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :garnet-calculator (:use :common-lisp :kr)
	    (:export start-calc stop-calc do-go do-stop))

(defpackage :demo-mode (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :garnetdraw (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :demo-motif (:use :common-lisp :kr) (:export do-go do-stop))

(defpackage :mge (:use :common-lisp :kr)
	    (:export do-go do-stop
		     create-piece destroy-piece destroy-all-pieces
		     go-initialize editor-show-window))

(defpackage :demo-moveline (:use :kr :common-lisp) (:export do-go do-stop))
