;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GEM; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CHANGE LOG:
;;; 10/03/03  Russell Almond - Added a change so that hopefully Macs
;;;           will read pixmaps with #\linefeed for #\newline
;;; 10/03/03  Russell Almond - Commented out code which doesn't work
;;;           on Carbon toolbox, actually most of this comes from
;;;           Quickdraw.lisp 
;;;  1/06/02  Russell Almond - Revised to use corresponding Quickdraw
;;; function calls instead of trap calls.  Hopefully, we can avoid loading the
;;; trap library and hence will be much quicker when MCL 4.4 comes out.
;;;  8/12/96  Russell Almond - implemented inject-event
;;; 12/12/94  Andrew Mickish - Implemented invert-p for draw-text
;;; 12/02/94  Andrew Mickish - Removed stippled-p parameter from draw-image
;;; 07/01/94  Andrew Mickish - Added MAC-Reparent; fudged dotted/dashed lines
;;;             with stippled patterns; added color-device-attached?
;;; 12/04/93  Andrew Mickish - Created

(in-package "GEM")

(eval-when (:compile-toplevel :eval-toplevel :execute)
(import '(ccl:new-region ccl:set-rect-region ccl:difference-region
	  ccl:set-empty-region ccl:copy-region ccl:offset-region
	  ccl:copy-bits ccl:global-to-local ccl:local-to-global
          ccl:get-polygon ccl:start-polygon))
)
#|
;;; RGA replacement for function no longer supported in MCL.
;;; **TODO** figure out how to do these functions under carbon.
;;; Looks like these are all in quickdraw.lisp
(defun new-region () 
  #-ccl-5.0 (ccl:new-region)
  #+ccl-5.0 (cerror "Return nil." "new-region not implemented."))

(defun set-rect-region (region left top right bot)
  #-ccl-5.0(ccl:set-rect-region region left top right bot)
  #+ccl-5.0(error "set-rect-region not implemented."))

(defun difference-region (region1 region2 region3)
  #-ccl-5.0(ccl:difference-region region1 region2 region3)
  #+ccl-5.0(error "difference-region not implemented."))

(defun set-empty-region (a-region)
  #-ccl-5.0 (ccl:set-empty-region a-region)
  #+ccl-5.0(error "set-empty-region not implemented."))

(defun copy-region (region1 region2)
  #-ccl-5.0(ccl:copy-region region1 region2)
  #+ccl-5.0(error "copy-region not implemented."))

(defun offset-region (region pos-x pos-y)
  #-ccl-5.0(ccl:offset-region region pos-x pos-y)
  #+ccl-5.0(error "offset-region not implemented."))

(defun copy-bits (src dest src-bound dst-bound &rest keys)
  #-ccl-5.0(apply #'ccl:Copy-Bits src dest src-bounds dst-bounds keys)
  #+ccl-5.0(error "copy-bits not implemented."))

(defun local-to-global (draw x y)
  #-ccl-5.0(ccl:local-to-global draw x y)
  #+ccl-5.0(error "local-to-global not implemented."))

(defun global-to-local (draw x y)
  #-ccl-5.0(ccl:global-to-local draw x y)
  #+ccl-5.0(error "global-to-local not implemented."))
|#

;; Set up the variables *spare-region-1* and *spare-region-2* to be
;; reinitialized whenever a saved lisp image is restarted, using
;; ccl:def-load-pointers.  Extreme care must be exercised when using these
;; regions, since many internal GEM functions OVERWRITE their contents.
;; The GEM functions must stay synchronized to use these regions symbiotically.
(defparameter *spare-region-1* (new-region))
(defparameter *spare-region-2* (new-region))
(ccl:def-load-pointers initialize-spare-regions ()
  (setf *spare-region-1* (new-region))
  (setf *spare-region-2*  (new-region)))


(defparameter *spare-rect-1* (ccl:make-record (:rect :storage :pointer)))
(defparameter *spare-rect-2* (ccl:make-record (:rect :storage :pointer)))
(ccl:def-load-pointers initialize-spare-rects ()
  (setf *spare-rect-1* (ccl:make-record (:rect :storage :pointer)))
  (setf *spare-rect-2* (ccl:make-record (:rect :storage :pointer))))


;; If the halftone-table was initialized before the current session was
;; restarted (as a saved application), then we have to reinitialize it.
;; Otherwise, all the halftone images are dead MACPTRs.
(ccl:def-load-pointers initialize-*halftone-table* ()
  (when opal::*halftone-table*
    (dotimes (n (length opal::*halftone-table*))
      (let ((entry (aref opal::*halftone-table* n)))
        (ccl:dispose-record (opal::halftone-device-image entry))
        (setf (opal::halftone-device-image entry)
              (gem:device-image (g-value DEVICE-INFO :current-device) n))))
    ;; Now update the pointers from the filling-styles to the new MACPTRs
    (opal::install-bitmap-images)))

(defvar *mac-font-faces* '(:roman :bold :italic :bold-italic
                           :plain :condense :extend :outline
                           :shadow :underline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAC Drawable prototype and interface functions


(defclass garnet-view-mixin ()
  ((plist :initarg :plist
          :initform :plist-init
          :accessor plist)
   ;; Need to implement background-color manually.  See MAC-Create-Window and
   ;; :around view-draw-contents method.
   (background-color
    :initarg :background-color
    :initform ccl:*white-color*
    :accessor background-color)
   (view-visrgn
    :initarg :view-visrgn
    :initform NIL
    :accessor view-visrgn)
   (view-visrgn-valid?
    :initarg :view-visrgn-valid?
    :initform NIL
    :accessor view-visrgn-valid?)))


(defparameter *MAC-DRAWABLE*
  (defclass MAC-DRAWABLE (garnet-view-mixin ccl:window) NIL))

(defparameter *MAC-SUBDRAWABLE*
  (defclass MAC-SUBDRAWABLE (garnet-view-mixin ccl:view) NIL))

(defparameter *MAC-BUFFER*
  (defclass MAC-BUFFER
    (ccl::gworld)
    ((plist :initarg :plist
            :initform :plist-init
            :accessor plist)
     (my-poly :initarg :my-poly
              :initform NIL
              :accessor my-poly))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions for event methods and other Mac GEM methods

;; Side effect is to remove the grow-region from the-rgn.  The grow-region is
;; obtained from top-level-window, so you have to send the TOP-LEVEL window in
;; the hierarchy you're dealing with.
(defun diff-with-grow-rgn (the-rgn the-view top-level-window)
  (let ((dest-window-size (ccl:view-size top-level-window)))
    (unless (eq the-view top-level-window)
      (setf dest-window-size
            (ccl:convert-coordinates dest-window-size top-level-window the-view)))
    (let ((dest-window-right (ccl:point-h dest-window-size))
          (dest-window-bottom (ccl:point-v dest-window-size)))
      (set-rect-region *spare-region-2* (- dest-window-right 15)
                                     (- dest-window-bottom 15)
                                     dest-window-right dest-window-bottom)
      (difference-region the-rgn *spare-region-2* the-rgn))))


;; Since we are controlling the clip-mask in its entirety, we are giving up
;; the automatic clipping that MCL would normally do to keep objects from being
;; drawn over subviews.  We have to handle this clipping out of subview rgns.
;; The top-level-window parameter is specifically for use by MAC-BIT-BLIT.
;; In that procedure, we are drawing directly into the top window, rather than
;; drawing into a subview.  We have to manually compute the offsets.
(defun diff-with-subview-rgns (the-rgn the-view &optional top-level-window)
  (ccl:do-subviews (a-subview the-view)
    (let ((subview-pos (ccl:view-position a-subview)))
      (when (and top-level-window
                 (not (eq the-view top-level-window)))
        (setf subview-pos
              (ccl:convert-coordinates subview-pos
                                       the-view
                                       top-level-window)))
      (let ((subview-botright (ccl:add-points subview-pos
                                              (ccl:view-size a-subview))))
        (set-rect-region *spare-region-2* subview-pos subview-botright)
        (difference-region the-rgn *spare-region-2* the-rgn)))))

;;; NIY? -- Need to find unique identifier for a mac drawable, like
;;; in xlib:window-id.
(defun connected-mac-drawable-p (event-window)
  (let ((opal-window (MAC-window-from-drawable NIL event-window)))
    (if opal-window
        (let ((drawable (g-value opal-window :drawable)))
          (and drawable (eq event-window drawable))))))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for backing-stores (a.k.a. GWorlds) that will allow them to be
;;; treated like views when polygons are being drawn into them

;; Start-polygon and Get-Polygon methods should be consistent with the methods
;; for views, found in ccl:library:quickdraw.lisp.
(defmethod start-polygon ((the-gworld MAC-BUFFER))
  (setf (slot-value the-gworld 'my-poly) (#_OpenPoly))
  nil)

(defmethod get-polygon ((the-gworld MAC-BUFFER))
  (let ((my-poly (slot-value the-gworld 'my-poly)))
    (if my-poly
        (prog1
          my-poly
          (#_ClosePoly)
          (setf (slot-value the-gworld 'my-poly) NIL))
        (error "Polygon for ~a has not been started" the-gworld))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   The STATE parameter
;;;
;;;    The modifier keys for the current keystroke or mouse-click are passed
;;; around in the STATE parameter.  This is a set of four bits that correspond
;;; to the shift, caps-lock, control, and option keys.  This parameter is
;;; generally set by the top-level event-handler that corresponds to a
;;; key or mouse event, and is passed to the support functions like
;;; MAC-translate-character.  The single byte STATE spec can be used as an
;;; index into the *prefixes* array, and is used analogously to index into the
;;; *mouse-down-translations* array (see garnet-keytrans.lisp and
;;; define-mouse-keys.lisp).
;;;
;;; These particular positions for each bit correspond to the state information
;;; generated by X windows.  The values were carried into the Mac version so
;;; that existing Interactors code could be shared by both implementations.
;;; 

;; Modifiers
(defconstant *shift-bit*     1)
(defconstant *caps-lock-bit* 2)
(defconstant *control-bit*   4)
(defconstant *option-bit*    8)  ;; the meta key

(defun get-event-bits ()
  (+ (if (ccl:option-key-p)    *option-bit* 0)
     (if (ccl:control-key-p)   *control-bit* 0)
     (if (ccl:caps-lock-key-p) *caps-lock-bit* 0)
     (if (ccl:shift-key-p)     *shift-bit* 0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENT HANDLING


;;; NIY:
;; The function ccl:find-view-containing-point does not always return the value
;; we expect.  If there are two overlapping subviews, it will return the deepest
;; one in the window hierarchy, even if the other one is obscuring it.  Consider
;; Agate, where you have both a scrolling-window and an error-gadget as
;; subwindows of the main window.  The error-gadget covers the scrolling-window,
;; but ccl:find-view-containing-point will return the scrolling-window's
;; inner-window when you try to click on the error-gadget button.
;;
;; Caveat to previous paragraph: After writing that comment, I finished
;; implementing show-drawable and hide-drawable so that subviews are removed
;; from their window hierarchy when they become invisible.  By reordering the
;; subviews of the parent view, I coincidentally "fixed" the error-gadget
;; problem in Agate.  Now the expected subview is being identified as the one
;; in which the event happened, but this is only by chance.  The MCL function
;; is still returning just the first subview that the event occurs in, rather
;; than the topmost subview.
;;
(defun find-overlapping-view-containing-point (event-window where)
  (do* ((subviews (ccl:subviews event-window) (cdr subviews))
        (subview (car subviews) (car subviews))
        (found? (if subview (ccl:view-contains-point-p subview where))
                (if subview (ccl:view-contains-point-p subview where)))
        (found-list (if found? (list subview))
                    (if found? (cons subview found-list))))
       ((null subview) found-list)
    ))
        
;;; RGA This is a test data array for seeing why mouse button mapping seems to fail
;;; Under MacOS X/Classic
;;(defparameter debug-mac-events t)
(defvar key-mac-event-list)
(defvar mouse-mac-event-list)

(defmethod ccl:view-key-event-handler ((event-window gem::MAC-DRAWABLE) char)
  (declare (ignore char))
  (let* ((time (ccl:rref ccl:*current-event* :EventRecord.when))
         (message (ccl:rref ccl:*current-event* :EventRecord.message))
         ;; The "where" is in screen coordinates
         (where (ccl::%global-to-local
                 (ccl:wptr event-window)
                 (ccl:rref ccl:*current-event* :EventRecord.where)))
         ;; CODE contains the "key code" in the high-order word, and the
         ;; "character code" in the low-order word
         (code (logand message #xFFFF))
         (state (get-event-bits)))
    #+debug-mac-events
    (push (list 'key-down :time time :message message 
                  :where where :state state :code code :char char)
            key-mac-event-list)
    ;; The key-event-handler is called on the "active (frontmost) window", as
    ;; stated on p. 376 of the MCL manual.  This means we have to determine
    ;; ourselves which subview the keystroke should actually go to -- argh!
    (let ((subwindow (ccl:find-view-containing-point event-window where)))
      (unless (eq event-window subwindow)
        (setf where (ccl::convert-coordinates where event-window subwindow))
        (setf event-window subwindow)))
    (let ((x (ccl:point-h where))
          (y (ccl:point-v where))
          (win (MAC-window-from-drawable *root-window* event-window)))
      ;(format t "DOWN code = ~S~%" code)
      (inter::Do-Key-Press win x y state code time)))
  #+comment
  (call-next-method))


;; Implemented in conjunction with MAC-translate-character.  The key-up-event-
;; handler perceives simulated mouse-up events generated by keyboard keys
;; (esp. middledown and rightdown).  MAC-translate-character perceives the
;; simulated mouse-down events.
;;
(defmethod ccl:window-key-up-event-handler ((event-window gem::MAC-DRAWABLE))
  (let* ((raw-message (ccl:rref ccl:*current-event* :EventRecord.message))
         (message (logand raw-message #xFFFF))
         (key-code (ash message -8))             ;; high-order word
  ;;RGA         (code (key-code-to-button-code key-code)))
         (state (get-event-bits))
         (code (key-code-to-button-code key-code nil)))
      #+debug-mac-events
            (push (list 'key-up :message raw-message 
                        :state state :code code :key-code key-code)
                  key-mac-event-list)
    (when code
      (let* ((time (ccl:rref ccl:*current-event* :EventRecord.when))
             ;; The "where" is in screen coordinates
             (where (ccl::%global-to-local
                     (ccl:wptr event-window)
                     (ccl:rref ccl:*current-event* :EventRecord.where)))
             (event-key :BUTTON-RELEASE))
    ;; The key-event-handler is called on the "active (frontmost) window", as
    ;; stated on p. 376 of the MCL manual.  This means we have to determine
    ;; ourselves which subview the keystroke should actually go to -- argh!
        (let ((subwindow (ccl:find-view-containing-point event-window where)))
          (unless (eq event-window subwindow)
            (setf where (ccl::convert-coordinates where event-window subwindow))
            (setf event-window subwindow)))
        (let ((x (ccl:point-h where))
              (y (ccl:point-v where))
              (win (MAC-window-from-drawable *root-window* event-window)))
          ;(format t "UP = ~S~%" code)
          (inter::Do-Button-Release win x y state code time event-key)))))
  #+comment
  (call-next-method))


(defmethod ccl:view-click-event-handler ((event-window gem::MAC-DRAWABLE) where)
  (let ((message (ccl:rref ccl:*current-event* :EventRecord.message))
        (state (get-event-bits))
        (time (ccl:rref ccl:*current-event* :EventRecord.when))
        (code inter::*left-button*)
        (event-key :BUTTON-PRESS))
    #-debug-mac-events
    (declare (ignore message))
    #+debug-mac-events
      (push (list 'mouse-down :time time :message message 
                  :where where :state state :code code )
            mouse-mac-event-list)
    ;(format t "code = ~S~%" code)
    ;; If we adhered to the object-oriented event-handler paradigm, we would
    ;; allow MCL to call the view-click-event-handler on each of the window's
    ;; subviews, giving each one a chance to handle the event.  However, the
    ;; implementation here bypasses the intervening views (including the top
    ;; window, if appropriate) to only allow the deepest view to handle the
    ;; event.  This is consistent with the X windows model.

    (let ((subwindow (ccl:find-view-containing-point event-window where)))
      (unless (eq event-window subwindow)
        (setf where (ccl::convert-coordinates where event-window subwindow))
        (setf event-window subwindow)))

    ;; Do bookkeeping required for mouse-move events that might be ahead...
    (process-activate-event event-window)
    (let ((x (ccl:point-h where))
          (y (ccl:point-v where))
          (win (MAC-window-from-drawable *root-window* event-window)))
      (inter::Do-Button-Press win x y state code time event-key)))
  (call-next-method))

(defmethod ccl:window-mouse-up-event-handler ((event-window gem::MAC-DRAWABLE))
  (let* ((message (ccl:rref ccl:*current-event* :EventRecord.message))
         (state (get-event-bits))
         (time (ccl:rref ccl:*current-event* :EventRecord.when))
         (code inter::*left-button*)
         (event-key :BUTTON-RELEASE)
         (where (ccl::%global-to-local
                 (ccl:wptr event-window)
                 (ccl:rref ccl:*current-event* :EventRecord.where))))
    #-debug-mac-events
    (declare (ignore message))
    #+debug-mac-events
      (push (list 'mouse-up :time time :message message 
                  :where where :state state :code code )
            mouse-mac-event-list)
    ;; See comments for ccl:view-click-event-handler
    (let ((subwindow (ccl:find-view-containing-point event-window where)))
      (unless (eq event-window subwindow)
        (setf where (ccl::convert-coordinates where event-window subwindow))
        (setf event-window subwindow)))
    (let ((x (ccl:point-h where))
          (y (ccl:point-v where))
          (win (MAC-window-from-drawable *root-window* event-window)))
      (inter::Do-Button-Release win x y state code time event-key)))
  (call-next-method))

(defmethod ccl:view-draw-contents ((event-window garnet-view-mixin))
  (ccl:with-back-color (background-color event-window)
    (let* ((opal-window (MAC-window-from-drawable *root-window* event-window))
           (view-size-point (ccl:view-size event-window))
           (left 0)
           (top 0)
           (right (ccl:point-h view-size-point))
           (bottom (ccl:point-v view-size-point)))

      ;; Draw the background color of the window.  Without this call, the
      ;; background color would still be drawn successfully by MAC-Clear-Area,
      ;; but only if the window has an aggregate.  The update method does not
      ;; focus on the view to draw anything unless there is an aggregate.
      (set-rect-region *spare-region-1* 0 0 right bottom)
      (unless (g-value opal-window :omit-title-bar-p)
        (diff-with-grow-rgn *spare-region-1* event-window
                            (ccl:window-object (ccl:wptr event-window))))
      (set-empty-region *spare-region-1*)

      ;; Redraw the objects in the window
      (if (connected-mac-drawable-p event-window)
          (inter::do-exposure opal-window left top right bottom
                              0 NIL)))) ; Count and Display are unused by Mac
  (call-next-method))

(defmethod (setf background-color) :after (color (view garnet-view-mixin))
  (declare (ignore color))
  (ccl:invalidate-view view))


(defmethod ccl:window-grow-event-handler :after
             ((event-window gem::MAC-DRAWABLE) where)
  (declare (ignore where))
  (when (gem::connected-mac-drawable-p event-window)
    (let* ((opal-window (gem::MAC-window-from-drawable
                         NIL event-window))
           (view-pos-point (ccl:view-position event-window))
           (view-size-point (ccl:view-size event-window))
           (x (ccl:point-h view-pos-point))
           (y (ccl:point-v view-pos-point))
           (width (ccl:point-h view-size-point))
           (height (ccl:point-v view-size-point))
           (above-sibling NIL))
      (inter::do-configure-notify opal-window x y width height
                                  above-sibling))))

(defmethod ccl:window-drag-event-handler :after
             ((event-window gem::MAC-DRAWABLE) where)
  (declare (ignore where))
  (when (gem::connected-mac-drawable-p event-window)
    (let* ((opal-window (gem::MAC-window-from-drawable
                         NIL event-window))
           (view-pos-point (ccl:view-position event-window))
           (view-size-point (ccl:view-size event-window))
           (x (ccl:point-h view-pos-point))
           (y (ccl:point-v view-pos-point))
           (width (ccl:point-h view-size-point))
           (height (ccl:point-v view-size-point))
           (above-sibling NIL))
      (inter::do-configure-notify opal-window x y width height
                                  above-sibling))))
  
;;; END Event Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; From Inside Macintosh V: "During a CopyBits call, the foreground and back-
;; ground colors are applied to the image.  To avoid unwanted coloring of the
;; image, set the foreground to black and the background to white before
;; calling this routine." -- argh, ARGH!
(defmacro with-copybits-colors ((the-gworld) &body body)
  (let ((old-fore (gensym))
        (old-back (gensym))
        (old-rgb-fore (gensym))
        (old-rgb-back (gensym))
        (port (gensym)))
    `(let* ((,port ,the-gworld)
            (,old-fore (ccl:rref ,port windowrecord.fgcolor))
            (,old-back (ccl:rref ,port windowrecord.bkcolor))
            (,old-rgb-fore (ccl:rref ,port cgrafport.rgbfgcolor))
            (,old-rgb-back (ccl:rref ,port cgrafport.rgbbkcolor)))
       (#_rgbforecolor ccl:*black-rgb*)
       (#_rgbbackcolor ccl:*white-rgb*)
       ,@body
       (ccl:rset ,port windowrecord.fgcolor ,old-fore)
       (ccl:rset ,port windowrecord.bkcolor ,old-back)
       (ccl:rset ,port cgrafport.rgbfgcolor ,old-rgb-fore)
       (ccl:rset ,port cgrafport.rgbbkcolor ,old-rgb-back))))


(defun MAC-beep (root-window)
  (declare (ignore root-window))
  (ccl::beep))

(defun MAC-black-white-pixel (window)
  (declare (ignore window))
  (values ccl:*black-color* ccl:*white-color*))


;; Nomenclature:  There is some inconsistency with the word "pixmaps" in Gem.
;; All of the functions concerned with pixmaps have "-pixmap-" in the function
;; name, when appropriate.  However, the object that is passed around is a
;; Mac GWORLD object, which has a gworld pointer in a slot, and from this gworld
;; pointer we can get the underlying pixmap.
;;
;; source -- a gworld CLOS object, the :buffer of the window
;; destination -- a view
;;
(defun MAC-bit-blit (window source s-x s-y width height destination d-x d-y)
  (let* ((wptr (ccl:wptr destination))
         (gworld (ccl::gworld source))
         (pixmap (#_GetGWorldPixmap gworld))
         ;; The source is really a gworld (not a drawable), but this is the
         ;; function we use to access its plist to get its Garnet window
         (source-window (MAC-window-from-drawable window source))
         (source-drawable (unless (eq source-window :pixmap)
                            (g-value source-window :drawable)))
         ;; Dest-window is the top-level window in the destination's hierarchy,
         ;; not necessarily the same as the view associated with the buffer
         (dest-window (ccl:window-object wptr)))

    ;;    When MCL focuses to a subview, part of what it "really" does is it
    ;; changes the origin of the top-level window to be the upper-left corner
    ;; of the subview, and then draws into the top-level window as usual.
    ;; It also sets the clip-mask so that all drawing is restricted to the
    ;; focused view.
    ;;   When we are bit-blitting, we are transferring between two independent
    ;; coordinate systems (from the gworld to the grafport) and we have to
    ;; establish these relationships ourselves.  We do this by incrementing
    ;; d-x and d-y by the natural offset of the view in the window, and then
    ;; calling #_SetOrigin below.  We also have to supply the clip-region of
    ;; the view to #_CopyBits.
    ;;
    (unless (eq source-window :pixmap)
      (copy-region (ccl:view-clip-region source-drawable) *spare-region-1*)
      (unless (eq source-drawable dest-window)
        (let* ((view-pos (ccl:convert-coordinates 0 source-drawable dest-window))
               (view-pos-x (ccl:point-h view-pos))
               (view-pos-y (ccl:point-v view-pos)))
          (incf d-x (ccl:point-h view-pos))
          (incf d-y (ccl:point-v view-pos))
          (offset-region *spare-region-1* view-pos-x view-pos-y))))

    ;; Wait until d-x and d-y are converted before binding d-right and d-bottom
    (let ((s-right (+ s-x width))
          (s-bottom (+ s-y height))
          (d-right (+ d-x width))
          (d-bottom (+ d-y height)))
      (ccl:validate-view destination)
      (ccl:with-port wptr
        (ccl:with-pointers ((src pixmap)
                            (dest (ccl:rref wptr :grafport.portbits)))
          (ccl::with-rectangle-arg (src-bounds s-x s-y s-right s-bottom)
            (ccl::with-rectangle-arg (dst-bounds d-x d-y d-right d-bottom)
              (with-copybits-colors (gworld)
                (#_ClipRect dst-bounds)
                (#_SetOrigin :long 0)
                ;; Don't draw over the grow box
                (unless (g-value source-window :omit-title-bar-p)
                  (diff-with-grow-rgn *spare-region-1* dest-window dest-window))
                (diff-with-subview-rgns *spare-region-1*
                                        source-drawable dest-window)
                (Copy-Bits src dest src-bounds dst-bounds :patCopy *spare-region-1*)
                ))))))
    ))

(defun MAC-character-width (root-window opal-font the-char-code)
  (MAC-text-width root-window opal-font (string (code-char the-char-code))))

(defun MAC-clear-area (window &optional (x 0) (y 0) width height clear-buffer-p)
  (let* ((drawable (g-value window :drawable))
         (bcolor (background-color drawable)))
    (ccl:with-back-color bcolor
      (if clear-buffer-p
          ;; Clear the window's buffer
          (let* ((buffer (g-value window :buffer))
                 (gworld (ccl::gworld buffer))
                 (pixmap (#_GetGWorldPixmap gworld)))
            ;; Without this with-focused-gworld call, the Erase operation may
            ;; take place in the wrong window.  If the call is removed, you can
            ;; make the main window of Demos-Controller go blank whenever you
            ;; click on a demo's button -- the demo's window is supposed to be
            ;; cleared, but instead the main window is cleared.
            (ccl::with-focused-gworld (buffer)
              (if x
                  ;; clear only a region
                  (ccl::with-rectangle-arg (r x y (+ x width) (+ y height))
                    (#_EraseRect r))
                  ;; clear the entire buffer
                  (ccl:with-pointers ((pm pixmap))
                    (let ((bounds (ccl:rref pm :pixmap.bounds
                                            :storage :pointer)))
                      (#_EraseRect bounds))))))

          ;; Else, clear window's drawable
          (let ((view-size-point (ccl:view-size drawable))
                (wptr (ccl:wptr drawable)))
            (if (not width)  (setq width  (ccl:point-h view-size-point)))
            (if (not height) (setq height (ccl:point-v view-size-point)))
            (set-rect-region *spare-region-1* x y (+ x width) (+ y height))
            ;; Don't draw over the grow box.  Have to send TOP-LEVEL window!
            (unless (or (g-value window :omit-title-bar-p)
                        ;; Subwindow may not have been added to a parent yet,
                        ;; like when an error-gadget dialog is displayed.
                        (null wptr))
              (diff-with-grow-rgn *spare-region-1* drawable
                                  (ccl:window-object wptr)))
            (diff-with-subview-rgns *spare-region-1* drawable)
            (set-empty-region *spare-region-1*)
            )))))

;; Note: On the Mac, we do not really have indices; we have six-digit
;; hexadecimal RGB values.  These values are stored in the :xcolor slot
;; of each Opal color object (the :colormap-index slot is unused).
;;
(defun MAC-color-to-index (root-window a-color)
  (declare (ignore root-window))
  (if (g-value opal::color :color-p)
      (if a-color (g-value a-color :xcolor) ccl:*white-color*)
      (if (eq a-color opal::black)
          ccl:*black-color*
          ccl:*white-color*)))

(defun MAC-colormap-property (root-window property &optional a b c)
  (declare (ignore root-window))
  (case property
    (:FIRST-ALLOCATABLE-INDEX 1)
    (:MAKE-COLOR (ccl:make-color (opal:clip-and-map a 0 1 0 65535)
                                 (opal:clip-and-map b 0 1 0 65535)
                                 (opal:clip-and-map c 0 1 0 65535)))
    (:ALLOC-COLOR a)
    (:LOOKUP-COLOR 0)
    ;; RGA this seems to have been missing from this translation
    (:query-colors 
     (values (ccl:color-red a) (ccl:color-green a) (ccl:color-blue a)))
    (:lookup-rgb
     (values (ccl:color-red a) (ccl:color-green a) (ccl:color-blue a)))
    (t (warn "~S fell through CASE in gem::MAC-colormap-property" property))))


(defun MAC-create-image (root-window width height depth from-data-p
				   &optional color-or-data properties
				   bits-per-pixel left-pad data-array)
  (declare (ignore left-pad bits-per-pixel properties from-data-p))
  (let ((new-image (MAC-create-pixmap root-window width height depth
                                      color-or-data NIL data-array)))
    (ccl:without-interrupts
     (ccl::with-focused-gworld (new-image)
       (cond
        ((schema-p color-or-data)
         (ccl:with-rgb (rgb (g-value color-or-data :xcolor))
           (dotimes (h height)
             (dotimes (w width)
               (#_SetCPixel w h rgb)))))
        (t
         (dotimes (h height)
           (dotimes (w width)
             (ccl:with-rgb (rgb (aref color-or-data h w))
               (#_SetCPixel w h rgb))
             ))))))
    (push :pixmap (slot-value new-image 'plist))
    (push :garnet (slot-value new-image 'plist))
    new-image))

(defun MAC-create-image-array (root-window width height depth)
  (declare (ignore root-window depth))
  (make-array (list height width)))


(defun MAC-create-pixmap (window width height depth
                          &optional image bitmap-p data-array)
  (declare (ignore window bitmap-p))
  (unless data-array
    (setf data-array (make-array (list width height)
                                 :initial-element
                                 (if (is-a-p image opal:color)
                                     (g-value image :xcolor)
                                     0))))
  (let ((the-gworld (make-instance 'MAC-BUFFER
                      :depth depth
                      :gray? NIL
                      :size (ccl:make-point width height)
                      :plist (list :pixarray data-array))))
    (ccl::with-focused-gworld (the-gworld)
      (set-rect-region *spare-region-1* 0 0 width height)
      (set-empty-region *spare-region-1*))
    the-gworld))


(defun MAC-create-state-mask (root-window modifier)
  (declare (ignore root-window))
  (case modifier
    (:shift                   *shift-bit*)
    (:lock                    *caps-lock-bit*)
    ((:control :command)      *control-bit*)
    ((:mod-1 :meta :option)   *option-bit*)))


(defun MAC-create-window (parent-window x y width height title icon-name
                          background border-width save-under visible
                          min-width min-height max-width max-height
                          user-specified-position-p user-specified-size-p
                          override-redirect)
  (declare (ignore icon-name border-width save-under min-width min-height
                   max-width max-height
                   user-specified-position-p user-specified-size-p))
  (let ((drawable 
         (if (and parent-window (not (eq *root-window* parent-window)))
             (make-instance 'MAC-SUBDRAWABLE
               :view-container (if visible
                                   (g-value parent-window :drawable))
               :view-position (ccl:make-point x y)
               :view-size (ccl:make-point width height)
               :plist NIL
               :background-color background)

             (make-instance 'MAC-DRAWABLE
               :view-position (ccl:make-point x y)
               :view-size (ccl:make-point width height)
               :close-box-p T
               :window-type (if (eq :on override-redirect)
                                :single-edge-box
                                :document-with-zoom)
               :window-show NIL
               :window-title title
               :color-p opal::*is-this-a-color-screen?*
               :plist NIL
               :background-color background))))
    drawable))

;;; NIY
(defun MAC-delete-font (root-window fnt)
  (declare (ignore root-window fnt))
  )


(defun MAC-delete-pixmap (window gworld &optional buffer-too)
  (declare (ignore window buffer-too))
  (ccl::dispose gworld))

(defun MAC-delete-window (root-window mac-window)
  (declare (ignore root-window))
  (setf (slot-value mac-window 'plist) NIL)
  ;; You can't close a MAC-SUBDRAWABLE (a view)
  (if (eq *MAC-DRAWABLE* (class-of mac-window))
      (ccl:window-close mac-window)))

;;; This function could be made more efficient by compressing the
;;; arithmetic.  For now, leave it expanded for clarity.
(defun bit-vector-to-int (b)
  (let ((b0 (eql 1 (aref b 0)))
        (b1 (eql 1 (aref b 1)))
        (b2 (eql 1 (aref b 2)))
        (b3 (eql 1 (aref b 3))))
    (+ (if b0 128 0)
       (if b1  64 0)
       (if b2  32 0)
       (if b3  16 0)
       (if b0   8 0)
       (if b1   4 0)
       (if b2   2 0)
       (if b3   1 0))))
#-ccl-3
(defun MAC-device-image (root-window index)
  (declare (ignore root-window))
  (let* ((desc (opal::get-descriptor index))
         (b0 (bit-vector-to-int (first desc)))
         (b1 (bit-vector-to-int (second desc)))
         (b2 (bit-vector-to-int (third desc)))
         (b3 (bit-vector-to-int (fourth desc))))
    (ccl:make-record (:pattern :storage :pointer)
            (:array 0) b0   (:array 1) b1
            (:array 2) b2   (:array 3) b3
            (:array 4) b0   (:array 5) b1
            (:array 6) b2   (:array 7) b3)))
#+ccl-3
(defun MAC-device-image (root-window index)
  (declare (ignore root-window))
  (let* ((desc (opal::get-descriptor index))
         (b0 (bit-vector-to-int (first desc)))
         (b1 (bit-vector-to-int (second desc)))
         (b2 (bit-vector-to-int (third desc)))
         (b3 (bit-vector-to-int (fourth desc)))
         (pp (ccl:make-record (:pattern :storage :pointer))))
    (setf (ccl:pref pp (:pattern.pat 0)) b0)
    (setf (ccl:pref pp (:pattern.pat 1)) b1)
    (setf (ccl:pref pp (:pattern.pat 2)) b2)
    (setf (ccl:pref pp (:pattern.pat 3)) b3)
    (setf (ccl:pref pp (:pattern.pat 4)) b0)
    (setf (ccl:pref pp (:pattern.pat 5)) b1)
    (setf (ccl:pref pp (:pattern.pat 6)) b2)
    (setf (ccl:pref pp (:pattern.pat 7)) b3)
    pp))


;;; RGA I can't seem to find this function, I'm commenting it out and hoping
;;; its not called in a cruicial place.
(defun MAC-discard-pending-events (root-window &optional (timeout 1))
  (declare (ignore root-window timeout))
  #|
  (ccl:_FlushEvents)
  |#
)


(defun fill-to-pattern (fill)
  (if fill
      (let ((stipple (g-value fill :stipple)))
        (if stipple
            (g-value stipple :image)
            ccl:*black-pattern*))))

(defun linestyle-to-pattern (linestyle)
  (if linestyle
      (let ((stipple (g-value linestyle :stipple)))
        (if stipple
            (g-value stipple :image)
            ;; Fudge for dashed/dotted lines
            (if (eq :solid (g-value linestyle :line-style))
                ccl:*black-pattern*
                ccl:*gray-pattern*)))))

(defun style-to-xcolors (style)
  (if style
      (let ((fcolor (g-value style :foreground-color :xcolor))
            (bcolor (g-value style :background-color :xcolor)))
        (values fcolor bcolor))))

(defmacro with-mac-colors (style &body body)
  `(multiple-value-bind (fcolor bcolor)
       (style-to-xcolors ,style)
     (when fcolor
       (ccl:with-fore-color fcolor
         (ccl:with-back-color bcolor
           ,@body)))))

(defun MAC-draw-arc (window x y width height angle1 angle2 function
			  line-style fill-style &optional pie-slice-p)
  (declare (ignore window pie-slice-p))
  (setf angle1 (- 90 (opal:clip-and-map angle1 gu:-2PI gu:2PI -360 360)))
  (setf angle2 (opal:clip-and-map angle2 gu:-2PI gu:2PI 360 -360))
  (let* ((thickness (if line-style
                        (max 1 (g-value line-style :line-thickness))
                        0))
         (fill-pattern (fill-to-pattern fill-style))
         (line-pattern (linestyle-to-pattern line-style))
         (right (+ x width))
         (bottom (+ y height)))
    (setf function (get function :x-draw-function))
    (#_PenMode (position function ccl:*pen-modes*))
    (when fill-pattern
      (#_PenPat fill-pattern)
      (with-mac-colors fill-style
        (ccl::with-rectangle-arg (r (+ x thickness) (+ y thickness)
                                    (- right thickness) (- bottom thickness))
          (#_PaintArc r angle1 angle2))))
    (when line-pattern
      (with-mac-colors line-style
        (#_PenPat line-pattern)
        (#_PenSize thickness thickness)
        (ccl::with-rectangle-arg (r x y right bottom)
          (#_FrameArc r angle1 angle2))))
    ))

(defun MAC-draw-image (window left top width height image function fill-style)
  (declare (ignore function fill-style))
  (let* ((s-x 0) (s-y 0) (s-right width) (s-bottom height)
         (d-x left) (d-y top) (d-right (+ left width)) (d-bottom (+ top height))
         (src-gworld (ccl::gworld image))
         (src-pixmap (#_GetGWorldPixmap src-gworld))
         dest-pixmap)

    ;; Determine whether the destination is a visible view or another offscreen
    ;; gworld (which would be the :buffer of a window).  The correct way to
    ;; do this is to use #_GetPort or something that returns what you are
    ;; currently focused on.  I couldn't get this to work, so here is a kludge
    ;; that just asks whether the window parameter has a buffer.
    (let* ((drawable (g-value window :drawable))
           (drawable-wptr (ccl:wptr drawable))
           (buffer (g-value window :buffer)))
      (if buffer
          (setf dest-pixmap (#_GetGworldPixmap (ccl::gworld buffer)))
          (setf dest-pixmap (ccl:rref drawable-wptr :grafport.portbits)))

      (ccl:with-pointers ((src src-pixmap)
                          (dest dest-pixmap))
        (ccl::with-rectangle-arg (src-bounds s-x s-y s-right s-bottom)
          (ccl::with-rectangle-arg (dst-bounds d-x d-y d-right d-bottom)
            (with-copybits-colors (src-gworld)
              (copy-region (ccl:view-clip-region drawable) *spare-region-1*)
              (unless (g-value window :omit-title-bar-p)
                (diff-with-grow-rgn *spare-region-1* drawable
                                    (ccl:window-object drawable-wptr)))
              (copy-bits src dest src-bounds dst-bounds :patCopy *spare-region-1*)
              )))))))

(defun MAC-draw-line (window x1 y1 x2 y2 function line-style &optional drawable)
  (declare (ignore window drawable))
  (if line-style
      (let* ((thickness (max 1 (g-value line-style :line-thickness)))
             (th/2 (floor thickness 2))
             (line-pattern (linestyle-to-pattern line-style)))
        (setf function (get function :x-draw-function))
        (with-mac-colors line-style
          (#_PenMode (position function ccl:*pen-modes*))
          (#_PenPat line-pattern)
          (#_PenSize thickness thickness)
          (#_MoveTo :long (ccl:make-point (- x1 th/2) (- y1 th/2)))
          (#_LineTo :long (ccl:make-point (- x2 th/2) (- y2 th/2)))))))

(defun MAC-draw-lines (window point-list function line-style fill-style)
  (ccl:without-interrupts
   (let* ((drawable (the-drawable window))
          (thickness (if line-style
                       (max 1 (g-value line-style :line-thickness)) 0))
          (th/2 (floor thickness 2)))
     (setf function (get function :x-draw-function))
     (#_PenMode (position function ccl:*pen-modes*))
     (#_PenSize thickness thickness)
     (start-polygon drawable)
     (#_MoveTo :long (ccl:make-point (- (first point-list) th/2)
                                     (- (second point-list) th/2)))
     (do* ((points (cddr point-list) (cddr points)))
          ((null points))
      (#_LineTo :long (ccl:make-point (- (first points) th/2)
                                      (- (second points) th/2))))
     (let ((polygon (get-polygon drawable)))
       (when fill-style
         (#_PenPat (fill-to-pattern fill-style))
         (with-mac-colors fill-style
           (#_PaintPoly polygon)))
       (when line-style
         (#_PenPat (linestyle-to-pattern line-style))
         (with-mac-colors line-style
           (#_FramePoly polygon)))
       (#_KillPoly polygon)))))

(defun MAC-draw-points (window point-list function line-style)
  (declare (ignore window point-list function line-style))
  (format t "Drawing multi-points NIY~%"))

(defun MAC-draw-rectangle (window left top width height function
				  line-style fill-style)
  (declare (ignore window))
  (let* ((thickness (if line-style
                        (max 1 (g-value line-style :line-thickness))
                        0))
         (fill-pattern (fill-to-pattern fill-style))
         (line-pattern (linestyle-to-pattern line-style))
         (right (+ left width))
         (bottom (+ top height)))
    (setf function (get function :x-draw-function))
    (#_PenMode (position function ccl:*pen-modes*))
    (when fill-pattern
      (#_PenPat fill-pattern)
      (with-mac-colors fill-style
        (ccl::with-rectangle-arg (r (+ left thickness) (+ top thickness)
                                    (- right thickness) (- bottom thickness))
          (#_PaintRect r))))
    (when line-pattern
      (with-mac-colors line-style
        (#_PenPat line-pattern)
        (#_PenSize thickness thickness)
        (ccl::with-rectangle-arg (r left top right bottom)
          (#_FrameRect r)))
      )))

(defun MAC-draw-roundtangle (window left top width height
				    x-radius y-radius function
				    line-style fill-style)
  (declare (ignore window))
  (let* ((thickness (if line-style
                      (max 1 (g-value line-style :line-thickness))
                      0))
         (th2 (+ thickness thickness))
         (fill-pattern (fill-to-pattern fill-style))
         (line-pattern (linestyle-to-pattern line-style))
         (corner-width (+ x-radius x-radius))
         (corner-height (+ y-radius y-radius))
         (right (+ left width))
         (bottom (+ top height)))
    (setf function (get function :x-draw-function))
    (#_PenMode (position function ccl:*pen-modes*))
    (when fill-pattern
      (#_PenPat fill-pattern)
      (with-mac-colors fill-style
        (ccl::with-rectangle-arg (r (+ left thickness) (+ top thickness)
                                    (- right thickness) (- bottom thickness))
          (#_PaintRoundRect r (- corner-width th2)
                              (- corner-height th2)))))
    (when line-pattern
      (with-mac-colors line-style
        (#_PenPat line-pattern)
        (#_PenSize thickness thickness)
        (ccl::with-rectangle-arg (r left top right bottom)
          (#_FrameRoundRect r corner-width corner-height))
        ))))

;;; Inside Macintosh V recommends that text be drawn in :SrcOr mode (for
;;; efficiency reasons).  Also, all documentation says text should be drawn
;;; with Src modes, but the current value of *function-alist* is Pat modes.
;;; Apparently the old version of MACL distinguised between Pat and Src modes,
;;; but the new version does not.
;;;
(defun MAC-draw-text (window x y string font function
			     line-style &optional fill-background invert-p)
  (declare (ignore window))
  (let ((font-codes (g-value font :xfont)))

    ;; Given the "Garnet" draw-function and fill-background, find the
    ;; "Mac" draw-function that will produce the desired results (need to
    ;; expand this to the four permutations of fill-p/copy-p?)
    (if (eq :copy function)
        (if fill-background
            (setf function (get function :x-draw-function))
            (setf function (get :OR :x-draw-function)))  ; <--- the usual case
        (setf function (get function :x-draw-function)))

    ;; Pick apart the font information and set the current window to use it.
    (let* ((font-face-code (first font-codes))
           (face-code (ash (logand #xFF00 font-face-code) -8))
           (font-code (ash font-face-code -16))
           (mode-code (position function ccl:*pen-modes*))
           (size-code (second (g-value font :font-from-file :font-name))))
      (#_TextFont font-code)
      (#_TextFace face-code)
      (#_TextMode mode-code)
      (#_TextSize size-code))
                                
    (#_MoveTo :long (ccl:make-point x y))
    ;; This clause could be optimized by expanding all the with-*
    ;; macros, and only focusing the view once.  We have already expanded
    ;; the gem::with-mac-colors macro.
    (multiple-value-bind (fcolor bcolor)
       (style-to-xcolors line-style)
      (if invert-p
          (let ((temp fcolor))
            (setq fcolor bcolor)
            (setq bcolor temp)))
      (when fcolor
        (ccl:with-fore-color fcolor
          (ccl:with-back-color bcolor
            (ccl:grafport-write-string string 0 (length string))))))))

;; Event-handling on the Mac is governed by particular methods for each type
;; of event (see "defmethod ccl:view-click-event-handler" above).  There is no
;; top-level loop like in X, so this function is a no-op on the Mac.
;; Maybe it should not even be a GEM method?
;;
;; This function has been particularly customized to only be called from
;; inside inter:wait-interaction-complete.  Without this throw, you would
;; never break out of the loop, and the event (a button click in the modal
;; dialog box) could never be processed.
;;
(defun MAC-event-handler (root-window ignore-keys)
  (declare (ignore root-window ignore-keys))
  (when (boundp 'ccl:*current-event*)
    (unless (zerop (ccl:rref ccl:*current-event* :EventRecord.what))
      (throw 'inter::exit-main-loop-exception NIL))))
    

(defun MAC-flush-output (window)
  (declare (ignore window))
  )

;;   This function used to be called by opal::get-index, in text-functions.lisp.
;; There are apparently no fixed-width fonts on the Mac, because each font
;; has special characters (e.g., the TM symbol in courier) that wreck the
;; otherwise fixed-widthness of the font.
;;   Instead of asking the Mac whether the font is fixed-width, Garnet checks
;; the :family slot of the font.  This is going to cause problems if someone
;; manages to insert one of the odd-width characters into a string...
;;
(defun MAC-font-max-min-width (root-window opal-font min-too)
  (declare (ignore root-window))
  (let ((font-spec (g-value opal-font :font-from-file :font-name)))
    (multiple-value-bind (ascent descent maxwidth leading)
          (ccl:font-info font-spec)
      (declare (ignore ascent descent leading))
      (cond (min-too
             ;; NIY -- how do you get the min-char-width?
             (values maxwidth maxwidth))
            (t maxwidth)))))
        

(defun MAC-font-name-p (root-window arg)
  (declare (ignore root-window))
  (listp arg))

;; NIY
(defun MAC-font-exists-p (root-window name)
  (declare (ignore root-window name))
  T)

(defun MAC-font-to-internal (root-window font-from-file)
  ;; Is display used on the Mac?  Maybe it's always NIL...
  (let ((plist (g-value font-from-file :display-font-plist))
        (display (the-display root-window)))
    (or (getf plist display)
        (multiple-value-bind (ff-code ms-code ff-mask ms-mask)
             (ccl:font-codes (g-value font-from-file :font-name))
          (let ((mac-font (list ff-code ms-code ff-mask ms-mask)))
            (s-value font-from-file :display-font-plist
                     (cons display (cons mac-font plist)))
            mac-font)))))

;; See mac-set-cut-buffer
(defun MAC-get-cut-buffer (window)
  (declare (ignore window))
  (ccl:get-scrap :TEXT))

(defun MAC-image-from-bits (root-window patterns)
  (declare (ignore patterns))
  (MAC-device-image root-window 16))

(defun MAC-image-size (root-window image)
  (declare (ignore root-window))
  (let* ((size-point (slot-value image 'ccl::size))
         (depth (slot-value image 'ccl::depth))
         (width (ccl:point-h size-point))
         (height (ccl:point-v size-point)))
    (values width height depth)))

(defun MAC-image-to-array (root-window image)
  (declare (ignore root-window))
  (getf (slot-value image 'plist) :pixarray))

(defun MAC-initialize-device (root-window)
  (declare (ignore root-window))
  (opal::make-display-info :display :ANDY-DISPLAY
		           :screen  :ANDY-SCREEN
		           :root-window :ANDY-ROOT-WINDOW
		           :line-style-gc :ANDY-LINE-STYLE-GC
		           :filling-style-gc :ANDY-FILLING-STYLE-GC))

(defun MAC-initialize-window-borders (window drawable)
  (declare (ignore window drawable)))

(defun MAC-make-font-name (root-window key)
  (declare (ignore root-window))
  (let ((family-part
         (case (first key)
           (:fixed      opal::*Fixed-Font-Family*)
           (:serif      opal::*Serif-Font-Family*)
           (:sans-serif opal::*Sans-Serif-Font-Family*)
           (otherwise   NIL)))
        (face-part
         (let ((face-spec (if (consp (second key))
                              (second key)
                              (list (second key)))))
           (if (subsetp face-spec *mac-font-faces*)
               face-spec)))
        (size-part
         (case (third key)
           (:small      opal::*Small-Font-Size*)
           (:medium     opal::*Medium-Font-Size*)
           (:large      opal::*Large-Font-Size*)
           (:very-large opal::*Very-Large-Font-Size*)
           (otherwise   nil))))
    (cond ((null family-part)
           (cons (first key) :family)) ;; for reporting error
          ((null face-part)
           (cons (second key) :face))
          ((null size-part)
           (cons (third key) :size))
          (t
           (let (adjusted-face-part)
             ;; Have to convert from Garnet keywords to Mac keywords
             (setf adjusted-face-part (subst :plain :roman face-part))
             (when (member :bold-italic adjusted-face-part)
               (setf adjusted-face-part
                     (remove :bold-italic adjusted-face-part))
               (push :italic adjusted-face-part)
               (push :bold adjusted-face-part))
             ;; Now cons up the full spec
             (push size-part adjusted-face-part)
             (push family-part adjusted-face-part))))))


(defun show-drawable (drawable)
  (cond
   ((eq *MAC-DRAWABLE* (class-of drawable))
    (if (not (ccl:window-shown-p drawable))
        (ccl:window-show drawable)))
   ((eq *MAC-SUBDRAWABLE* (class-of drawable))
    (if (not (ccl:view-container drawable))
        (let ((opal-window (MAC-window-from-drawable NIL drawable)))
          (ccl:set-view-container drawable
                                  (g-value opal-window :parent :drawable)))))))

(defun hide-drawable (drawable)
  (cond
   ((eq *MAC-DRAWABLE* (class-of drawable))
    (if (ccl:window-shown-p drawable)
        (ccl:window-hide drawable)))
   ((eq *MAC-SUBDRAWABLE* (class-of drawable))
    (when (ccl:view-container drawable)
      (ccl:set-view-container drawable NIL)))))
      

(defun MAC-map-and-wait (a-window drawable)
  (declare (ignore a-window))
  (show-drawable drawable))

(defun MAC-max-character-ascent (root-window opal-font)
  (declare (ignore root-window))
  (ccl:font-info (g-value opal-font :font-from-file :font-name)))

(defun MAC-max-character-descent (root-window opal-font)
  (declare (ignore root-window))
  (multiple-value-bind (ascent descent)
      (ccl:font-info (g-value opal-font :font-from-file :font-name))
    (declare (ignore ascent))
    descent))

(defun MAC-mouse-grab (window grab-p want-enter-leave &optional owner-p)
  (declare (ignore window grab-p want-enter-leave &optional owner-p)))

(defun MAC-raise-or-lower (window raise-p)
  (let ((drawable (g-value window :drawable))
        (new-layer (if raise-p 0 most-positive-fixnum)))
    ;; You can't raise or lower a MAC-SUBDRAWABLE
    (if (eq *MAC-DRAWABLE* (class-of drawable))
	(ccl:set-window-layer drawable new-layer T))))

(defun MAC-reparent (window new-parent drawable left top)
  (declare (ignore left top))
  (unless (or (null new-parent)
              (is-a-p new-parent opal::window))
    (error "Parent ~S of window ~S is not of type window~%"
           new-parent window))
  (let ((old-buffer (g-value window :buffer))
        (children (g-value window :child))
        new-window)
    (hide-drawable drawable)
    (when old-buffer
      (s-value window :buffer NIL)
      (ccl::dispose old-buffer))
    (setf new-window (opal::create-drawable window))
    (dolist (child children)
      (ccl:set-view-container (g-value child :drawable) new-window))))


;;; A function to determine the next multiple of 8 past the current #
;;; used for x-read-an-image function to determine width of array
(defun get-width (n)
  (let ((remainder (mod n 8)))
    (if (zerop remainder) n (+ n (- 8 remainder)))))


;;; Read and return the numbers at the end of a line on stream bitstream
;;;
(defun get-nums (bitstream)
     (do ((ch (peek-char t bitstream) (peek-char t bitstream)))
	 ((digit-char-p ch))
          (read-char bitstream))
     (parse-integer (read-line bitstream)))


;;; A function that punts each line of text that begins with #
;;; helper function for x-read-an-image
(defun punt-till (stream)
  (let ((ch (read-char stream)))
    (if (char= ch #\#)
	(progn (read-line stream)
	       (punt-till stream)))))


;;; Reads a bitmap file into an array and displays on screen in window
;;; root-window

(defun zap-to (stream char &aux input)
  ;; throws away all characters up to and including char
  (loop 
    (setq input (read-char stream))
    (when (eql char input) (return))))

;;; RGA --- Need to make this #\return vs #\newline neutral
(defun MAC-read-an-image (root-window pathname)
  (with-open-file (bitstream pathname :direction :input
                             #+ccl-5.0 :external-format #+ccl-5.0 :unix)
    (let* ((width (get-nums bitstream))
	   (height (get-nums bitstream))
           (data-width (get-width width))
           (depth 1)
           (pic (MAC-create-image-array root-window width height depth))
	   (char-map '((#\0  0) (#\1  1) (#\2  2) (#\3  3)
		       (#\4  4) (#\5  5) (#\6  6) (#\7  7)
		       (#\8  8) (#\9  9) (#\a 10) (#\b 11)
		       (#\c 12) (#\d 13) (#\e 14) (#\f 15)))
           (row-index 0)
           ch ch2 char-num)
      (punt-till bitstream)                ; punt chars till data

      (dotimes (h height)
        (dotimes (w (/ data-width 8))
          (zap-to bitstream #\0); throw away all chars till 0
	  (read-char bitstream)              ; throw away x character
	  (setf ch2 (read-char bitstream))   ; read 1st hex #
	  (setf ch (read-char bitstream))    ; read 2nd hex #
          (read-char bitstream)              ; throw away comma
          (dotimes (n 2)
            (setf char-num (second (assoc ch char-map :test #'char-equal)))
            (dotimes (i 4)
              (when (< row-index width)
                (setf (aref pic h row-index)
                      (if (logbitp i char-num)
                          ccl:*black-color*
                          ccl:*white-color*))
                (incf row-index)))
            (setf ch ch2)))
        (setf row-index 0))

      (MAC-create-image root-window width height depth T
                        pic NIL NIL NIL pic))))


;;; RGA --- We won't actually bother to "inject" this event, we will just 
;;; process it.
(defun mac-inject-event (window index)
  (declare (ignore window))
  (ccl:eval-enqueue
   `(inter::queue-timer-event ,index)))

(defun MAC-set-clip-mask (a-window clip-mask &optional lstyle-ogc fstyle-ogc)
  (declare (ignore lstyle-ogc fstyle-ogc))
  (let* ((drawable (g-value a-window :drawable)))
    (cond ((eq :none clip-mask)
           ;; Even though we might be setting the clip-mask of the buffer,
           ;; we have to use the drawable to get the size
           (let ((view-size-point (ccl:view-size drawable))
                 (old-clip-rgn (ccl:view-clip-region drawable))
                 (wptr (ccl:wptr drawable)))
             ;; Will clip into *spare-region-1*
             (set-rect-region *spare-region-1* 0 0 (ccl:point-h view-size-point)
                                                 (ccl:point-v view-size-point))
             ;; Might not have been set yet, like when an error-gadget
             ;; dialog box is becoming visible
             (if old-clip-rgn
                 (#_SectRgn *spare-region-1* old-clip-rgn *spare-region-1*))
             ;; Don't draw over the grow box.  Have to send TOP-LEVEL window!
             (unless (or (g-value a-window :omit-title-bar-p)
                         (null wptr))
               (diff-with-grow-rgn *spare-region-1* drawable
                                   (ccl:window-object wptr)))
            (diff-with-subview-rgns *spare-region-1* drawable)
             (#_SetClip *spare-region-1*)))
          (t
           (multiple-value-bind (l1 t1 w1 h1 l2 t2 w2 h2)
               (values-list clip-mask)
             ;; Will clip into *spare-region-1*
             (set-rect-region *spare-region-1* l1 t1 (+ l1 w1) (+ t1 h1))
             ;; When there is a second clip-mask, then create another region
             ;; and union it with region-1, storing in region-1, then throw
             ;; away region-2 and continue using region-1
             (when l2
               (set-rect-region *spare-region-2* l2 t2 (+ l2 w2) (+ t2 h2))
               (#_UnionRgn *spare-region-1* *spare-region-2*
                           *spare-region-1*))
             ;; Now intersect Opal's computed clip-region with the bounding
             ;; region of the view
             (#_SectRgn *spare-region-1* (ccl:view-clip-region drawable)
                        *spare-region-1*)
             ;; Don't draw over the grow box.  Have to send TOP-LEVEL window!
             (unless (g-value a-window :omit-title-bar-p)
               (diff-with-grow-rgn *spare-region-1* drawable
                                   (ccl:window-object (ccl:wptr drawable))))
             (diff-with-subview-rgns *spare-region-1* drawable)
             (#_SetClip *spare-region-1*))))))
                                                   
;;; Since gem:Set-Device-Variables is only called inside Initialize-X11-Values,
;;; it really shouldn't be a GEM method.
(defun MAC-set-device-variables (root-window)
  (declare (ignore root-window))
  (format t "inside MAC-set-device-variables...~%"))

(defun MAC-set-draw-function-alist (window)
  (declare (ignore window))
  (setq opal::*function-alist*
        `((:clear . :patBic)
          (:set . :patCopy)
          (:copy . :patCopy)
          (:no-op . :patCopy)
          (:copy-inverted . :patCopy)
          (:invert . :patCopy)
          (:and . :patCopy)
          (:or . :patOr)
          (:xor . :patXor)
          (:equiv . :srcXor)
          (:nand . :patCopy)
          (:nor . :patCopy)
          (:and-inverted . :patCopy)
          (:and-reverse . :patCopy)
          (:or-inverted . :patCopy)
          (:or-reverse . :patCopy))))

(defun MAC-set-drawable-to-window (window drawable)
  ;; Need to define Garnet-gworld mixin to provide plist slot
  (setf (slot-value drawable 'plist) (list :garnet window)))


;; There are three types of scrap -- :text, :lisp, and :fred.  The last
;; argument to ccl:put-scrap clears out what was previously in the scrap.
(defun MAC-set-cut-buffer (window string)
  (declare (ignore window))
  (ccl:put-scrap :TEXT string T))


(defun MAC-set-window-property (window property value)
  (case property
    (:BACKGROUND-COLOR
     (let ((drawable (g-value window :drawable))
           (index (MAC-color-to-index window value)))
       (setf (background-color drawable) index)
       NIL))
    (:LEFT (let ((drawable (g-value window :drawable)))
             (ccl:set-view-position drawable value
                                    (ccl:point-v
                                     (ccl:view-position drawable)))
             NIL))
    (:TOP (let ((drawable (g-value window :drawable)))
            (ccl:set-view-position drawable
                                   (ccl:point-h
                                    (ccl:view-position drawable))
                                   value)
            NIL))
    (:WIDTH (let ((drawable (g-value window :drawable))
                  (old-buffer (g-value window :buffer)))
              (ccl:set-view-size drawable (max 0 value)
                                 (ccl:point-v
                                  (ccl:view-size drawable)))
              (setf (opal::win-update-info-width
                     (g-value window :win-update-info))
	            value)
              ;; Does the buffer need to be recreated?
              (and old-buffer
                   (> value (ccl:point-v
                             (slot-value old-buffer 'ccl::size))))))
    (:HEIGHT (let ((drawable (g-value window :drawable))
                   (old-buffer (g-value window :buffer)))
               (ccl:set-view-size drawable (ccl:point-h
                                            (ccl:view-size drawable))
                                  (max 0 value))
               (setf (opal::win-update-info-height
                      (g-value window :win-update-info))
	             value)
               ;; Does the buffer need to be recreated?
               (and old-buffer
                    (> value (ccl:point-h
                              (slot-value old-buffer 'ccl::size))))))
    (:EVENT-POSITION (let ((x (first value))
                           (y (second value)))
                       (s-value window :left x)
                       (s-value window :top y)))

    (:TITLE (let ((drawable (g-value window :drawable)))
	      ;; You can't set the title of a MAC-SUBDRAWABLE
	      (if (eq *MAC-DRAWABLE* (class-of drawable))
		  (ccl:set-window-title drawable value))
              NIL))
    ;; Iconified windows NIY (???)
    (:VISIBLE (let ((drawable (g-value window :drawable))
                    (vis (g-value window :visible)))
                (cond ((eq vis t)
                       (show-drawable drawable))
                      ((eq vis :iconified)
                       (warn "What does it mean to iconify a window on the Mac?"))
                      ((eq vis nil)
                       (hide-drawable drawable)))))
    (:REPORT-ASYNCHRONOUS-ERRORS ())
;    (T
;     (warn "Unknown property ~S in gem:MAC-set-window-property.~%"
;	     property))
    )
  )

(defun MAC-stippled-p (root-window)
  (declare (ignore root-window))
  NIL)

(defun MAC-text-extents (root-window font string)
  (declare (ignore root-window))
  ;; This g-value works for instances of both opal:font and opal:font-from-file
  (let ((font-spec (g-value font :font-from-file :font-name)))
    (multiple-value-bind (ascent descent max-width left-bearing)
        (ccl:font-info font-spec)
      (declare (ignore max-width))
      (values
       (ccl:string-width string font-spec) ascent descent left-bearing))))

(defun MAC-text-width (root-window opal-font string)
  (declare (ignore root-window))
  (ccl:string-width string (g-value opal-font :font-from-file :font-name)))


(defun multiple-value-point (point)
  (values (ccl:point-h point)
          (ccl:point-v point)))

(defun MAC-translate-coordinates (root-window win1 x y &optional win2)
  (declare (ignore root-window))
  (let ((draw1 (if win1 (g-value win1 :drawable)))
        (draw2 (if win2 (g-value win2 :drawable))))
    (cond
     ((and draw1 draw2
           (eq (g-value win1 :parent)(g-value win2 :parent)))
      (let ((left1 (g-value win1 :left))
            (left2 (g-value win2 :left))
            (top1 (g-value win1 :top))
            (top2 (g-value win2 :top)))
        (values (+ (- left1 left2) x)
                (+ (- top1 top2) y))))
     ((and draw1 draw2) ;; different parent windows
      (multiple-value-bind (x1 y1)
            (multiple-value-point (local-to-global draw1 x y))
        (multiple-value-point (global-to-local draw2 x1 y1))))
     (draw2 (multiple-value-point (global-to-local draw2 x y)))
     (draw1 (multiple-value-point (local-to-global draw1 x y)))
     (T (error "translate coord both wins NIL")))))

;;; NIY
(defun MAC-window-debug-id (window)
  (declare (ignore window))
  666)  

;;; NIY
(defun MAC-window-depth (window)
  (declare (ignore window))
  8)

(defun MAC-window-has-grown (window width height)
  (let ((old-buffer (g-value window :buffer)))
    (if old-buffer
        (let* ((old-size (slot-value old-buffer 'ccl::size))
               (old-width (ccl:point-h old-size))
               (old-height (ccl:point-v old-size)))
          (or (> width old-width)
              (> height old-height))))))


;; Given an X drawable, returns the associated Opal window.
;;
;; The documented way to get this function is through opal:drawable-to-window,
;; which only takes one parameter: the mac-drawable.
(defun MAC-window-from-drawable (root-window mac-drawable)
  (declare (ignore root-window))
  (if (or (eq (class-of mac-drawable) *MAC-DRAWABLE*)
          (eq (class-of mac-drawable) *MAC-SUBDRAWABLE*)
          (eq (class-of mac-drawable) *MAC-BUFFER*))
      (getf (slot-value mac-drawable 'plist) :garnet)
      NIL))


(defun color-device-attached? ()
  (do* ((screen-gdevice  (#_GetDeviceList)
                         (#_GetNextDevice screen-gdevice))
        (color? (#_TestDeviceAttribute screen-gdevice #$gdDevType)
                (or color? (#_TestDeviceAttribute screen-gdevice #$gdDevType))))
       ((ccl:%null-ptr-p (#_GetNextDevice screen-gdevice)) color?)))


(defun attach-MAC-methods (mac-device)
  (attach-method mac-device :beep 'mac-beep)
  (attach-method mac-device :bit-blit 'mac-bit-blit)
  (attach-method mac-device :black-white-pixel 'mac-black-white-pixel)
  (attach-method mac-device :clear-area 'mac-clear-area)
  (attach-method mac-device :character-width 'mac-character-width)
  (attach-method mac-device :color-to-index 'mac-color-to-index)
  (attach-method mac-device :colormap-property 'mac-colormap-property)
  (attach-method mac-device :create-image 'mac-create-image)
  (attach-method mac-device :create-image-array 'mac-create-image-array)
  (attach-method mac-device :create-pixmap 'mac-create-pixmap)
  (attach-method mac-device :create-state-mask 'mac-create-state-mask)
  (attach-method mac-device :create-window 'mac-create-window)
  (attach-method mac-device :delete-font 'mac-delete-font)
  (attach-method mac-device :delete-pixmap 'mac-delete-pixmap)
  (attach-method mac-device :delete-window 'mac-delete-window)
  (attach-method mac-device :device-image 'mac-device-image)
  (attach-method mac-device :discard-mouse-moved-events
                 'mac-discard-mouse-moved-events)
  (attach-method mac-device :discard-pending-events 'mac-discard-pending-events)
  (attach-method mac-device :draw-arc 'mac-draw-arc)
  (attach-method mac-device :draw-image 'mac-draw-image)
  (attach-method mac-device :draw-line 'mac-draw-line)
  (attach-method mac-device :draw-lines 'mac-draw-lines)
  (attach-method mac-device :draw-rectangle 'mac-draw-rectangle)
  (attach-method mac-device :draw-roundtangle 'mac-draw-roundtangle)
  (attach-method mac-device :draw-text 'mac-draw-text)
  (attach-method mac-device :event-handler 'mac-event-handler)
  (attach-method mac-device :flush-output 'mac-flush-output)
  (attach-method mac-device :font-max-min-width 'mac-font-max-min-width)
  (attach-method mac-device :font-exists-p 'mac-font-exists-p)
  (attach-method mac-device :font-name-p 'mac-font-name-p)
  (attach-method mac-device :font-to-internal 'mac-font-to-internal)
  (attach-method mac-device :get-cut-buffer 'mac-get-cut-buffer)
  (attach-method mac-device :image-size 'mac-image-size)
  (attach-method mac-device :image-from-bits 'mac-image-from-bits)
  (attach-method mac-device :image-to-array 'mac-image-to-array)
  (attach-method mac-device :initialize-device 'mac-initialize-device)
  (attach-method mac-device :initialize-window-borders 'mac-initialize-window-borders)
  ;; RGA added inject-event
  (attach-method mac-device :inject-event 'mac-inject-event)
  (attach-method mac-device :make-font-name 'mac-make-font-name)
  (attach-method mac-device :map-and-wait 'mac-map-and-wait)
  (attach-method mac-device :max-character-ascent 'mac-max-character-ascent)
  (attach-method mac-device :max-character-descent 'mac-max-character-descent)
  (attach-method mac-device :mouse-grab 'mac-mouse-grab)
  (attach-method mac-device :raise-or-lower 'mac-raise-or-lower)
  (attach-method mac-device :read-an-image 'mac-read-an-image)
  (attach-method mac-device :reparent 'mac-reparent)
  (attach-method mac-device :set-clip-mask 'mac-set-clip-mask)
  (attach-method mac-device :set-cut-buffer 'mac-set-cut-buffer)
  (attach-method mac-device :set-device-variables 'mac-set-device-variables)
  (attach-method mac-device :set-drawable-to-window 'mac-set-drawable-to-window)
  (attach-method mac-device :set-draw-function-alist 'mac-set-draw-function-alist)
  (attach-method mac-device :set-window-property 'mac-set-window-property)
  (attach-method mac-device :stippled-p 'mac-stippled-p)
  (attach-method mac-device :text-extents 'mac-text-extents)
  (attach-method mac-device :text-width 'mac-text-width)
  (attach-method mac-device :translate-coordinates 'mac-translate-coordinates)
  (attach-method mac-device :window-debug-id 'mac-window-debug-id)
  (attach-method mac-device :window-from-drawable 'mac-window-from-drawable)
  (attach-method mac-device :window-depth 'mac-window-depth)

  (attach-method mac-device :check-double-press 'mac-check-double-press)
  (attach-method mac-device :set-interest-in-moved 'mac-set-interest-in-moved)
  (attach-method mac-device :translate-mouse-character
		            'mac-translate-mouse-character)
  (attach-method mac-device :translate-character 'mac-translate-character)
  (attach-method mac-device :window-has-grown 'mac-window-has-grown)

  (set-window-methods opal::window mac-device)
  )

;;;
;;;  Note:  Besides the reinitialization procedures that occur in this function
;;;  when an image is restarted, there are also the MACPTR redefinitions that
;;;  are automated by the ccl:def-load-pointers calls at the top of this file.
;;;
(defun MAC-TOP-LEVEL-INITIALIZE (display-name)
  (declare (ignore display-name))

  (create-schema '*root-window*
    (:is-a opal::window))

  (create-schema 'MAC-DEVICE
    (:root-window *root-window*)
    (:device-type :mac))

  (attach-MAC-methods MAC-DEVICE)

  (s-value DEVICE-INFO :current-root *root-window*)
  (s-value DEVICE-INFO :current-device MAC-DEVICE)
  (pushnew MAC-DEVICE (g-value DEVICE-INFO :active-devices))

  (let ((display-info (mac-initialize-device NIL)))
    (s-value *root-window* :drawable
	     (opal::display-info-root-window display-info))
    (s-value *root-window* :display-info display-info))

  ;; This is supposed to determine if you have a color screen.
  (with-constants-disabled
    (s-value opal::COLOR :color-p
             (setf opal::*is-this-a-color-screen?*
                   (color-device-attached?))))

  ;; Enable processing of key-up events.  By default, Garnet defines function
  ;; keys to simulate leftdown, middledown, and rightdown.  If we didn't get
  ;; key-up events, we couldn't generate up-events for these keys.
  ;; RGA: changed to internal symbol [Is this safe? no, breaks under carbon.]
  (#_SetEventMask #-carbon-compat traps::$EveryEvent
		  #+carbon-compat #$everyevent)

  (setf opal:*screen-width* ccl:*screen-width*)
  (setf opal:*screen-height* ccl:*screen-height*)

  ;; *black* and *white* are used heavily by the X implementation, but I think
  ;; only the PostScript module uses them on the Mac.
  (setf opal::*black* ccl:*black-color*)
  (setf opal::*white* ccl:*white-color*)

  (opal::set-draw-functions)
  (opal::initialize-halftones)

  *root-window*)


;;; Make the initializer function available to the outside world.
;;;
(setf *device-initializers* (list (cons :MAC #'MAC-TOP-LEVEL-INITIALIZE)))
