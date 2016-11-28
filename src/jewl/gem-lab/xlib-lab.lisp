(ql:quickload :clx)

(defpackage :xlib-lab
  (:use :common-lisp :kr)
  (:export))

(in-package :xlib-lab)


;; To Run
;; (ql:quickload :xoanon.gui.garnet)
;; (load "/home/rett/dev/garnet/garnet/src/gem/gem-lab/xlib-lab.lisp")
;; (load "/home/rett/dev/garnet/garnet/src/gem/gem-lab/gem-pixmap-lab.lisp")
;; (in-package :xlib-lab)
;; (trace-display)
;; (setf *top-win* (create-window :width 400 :height 410))
;; (draw-triangle-on-window (first *app-windows*))

(defparameter *system-initialized* nil)
(defparameter *display* nil)
(defparameter *screen* nil)
(defparameter *root-window* nil)
(defparameter *color-map* nil)

(defun init-xlib ()
  (setf *display* (xlib:open-default-display))
  (unless *display*
    (error "Unable to get valid default display."))
  (setf *screen* (first (xlib:display-roots *display*)))
  (unless *screen*
    (error "Unable to get valid default screen (display root)."))
  (setf *root-window* (xlib:screen-root *screen*))
  (unless *root-window*
    (error "Unable to get valid root window of the screen."))
  (setf *color-map* (xlib:screen-default-colormap *screen*))
  (unless *color-map*
    (error "Unable to get valid root window of the screen."))
  (setf *system-initialized* t))

;; toplevel windows of the application
(defparameter *app-windows* nil)

;; simplified model.  All calls
;; 1) perform any needed initialization in X
;; 2) map window if needed
;; 3) flush buffer
(defun create-window (&key (x 0) (y 0) width height)
  (unless *system-initialized*
    (init-xlib))
  (let ((app-window
	 (xlib:create-window :parent *root-window*
			     :x x
			     :y y
			     :width width
			     :height height)))
    (push app-window *app-windows*)
    (xlib:map-window app-window)
    (xlib:display-force-output *display*)))



;; TODO -- can we assume:
;; #<XLIB:BITMAP-FORMAT unit 32 pad 32 LSB first>
(defun display-bitmap-format ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-bitmap-format *display*))

;; TODO -- can we assume:
;;
;; :LSBFIRST
;;
;; Presumably not, due to architecture endianness
(defun display-byte-order ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-byte-order *display*))

;; not really a predicate
(defun display-image-lsb-first-p ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-image-lsb-first-p *display*))

;; TODO -- Is this always?:
;; 8
;; 255
(defun display-keycode-range ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-keycode-range *display*))

;; TODO -- see display-keycode-range:
(defun display-max-keycode ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-max-keycode *display*))

;; TODO -- Is this now always 65535?
(defun display-max-request-length ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-max-request-length *display*))

;; TODO -- see display-keycode-range:
(defun display-min-keycode ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-min-keycode *display*))

;; TODO -- ;; TODO -- Is this always:
;; 256
(defun display-motion-buffer-size ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-motion-buffer-size *display*))


;; TODO -- Do we always assume the following:
;; #<XLIB:PIXMAP-FORMAT depth 32 bits-per-pixel 32 scanline-pad 32>)
(defun display-pixmap-formats ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-pixmap-formats *display*))

;; TODO -- Do we always assume the following:
;; 11
;; 0
(defun display-protocol-version ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-protocol-version *display*))

;; TODO -- Do we always assume the following:
;; 11
;; 0
(defun display-vendor ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-vendor *display*))

;; TODO -- Doesn't seem like it would be useful (assigning resource
;; ids)
(defun display-xid ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-xid *display*))

(defun display-finish-output ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-finish-output *display*))

(defun display-force-output ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:display-force-output *display*))

(defun close-display ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:close-display *display*))

(defun screen-backing-stores ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:close-display *display*))

(defun screen-backing-stores ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-backing-stores *screen*))

(defun screen-backing-stores ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-backing-stores *screen*))

(defun screen-black-pixel ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-black-pixel *screen*))

(defun screen-height-in-millimeters ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-height-in-millimeters *screen*))

(defun screen-width-in-millimeters ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-width-in-millimeters *screen*))


(defun screen-height ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-height *screen*))

(defun screen-width ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-width *screen*))

(defun screen-root ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-root *screen*))

(defun screen-root-depth ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:screen-root-depth *screen*))

(defun trace-display ()
  (unless *system-initialized*
    (init-xlib))
  (xlib:trace-display *display*))






(defmacro with-xlib ((&key timeout inline)
		     &body body)
  ;; TODO -- It's not clear what this is useful for, nor, given some
  ;; other issues, whether it works.
  ;;
  ;; This macro is for use in a multi-process environment.  It
  ;; provides exclusive access to the local display object for
  ;; multiple request generation.  It need not provide immediate
  ;; exclusive access for replies; that is, if another process is
  ;; waiting for a reply (while not in a with-display), then
  ;; synchronization need not (but can) occur immediately.  Except
  ;; where noted, all routines effectively contain an implicit
  ;; with-display where needed, so that correct synchronization is
  ;; always provided at the interface level on a per-call basis.
  ;; Nested uses of this macro will work correctly.  This macro does
  ;; not prevent concurrent event processing; see with-event-queue.
  `(progn
     (unless *system-initialized*
       (init-xlib))
     (xlib:with-display (,*display*
			  ,@(and timeout `(:timeout ,timeout))
			  ,@(and inline `(:inline ,inline)))
	,@body)))



;; look for the wmDeleteMessage message
