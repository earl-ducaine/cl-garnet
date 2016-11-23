(ql:quickload :clx)

(defpackage :xlib-lab
  (:use :common-lisp)
  (:export))

(in-package :xlib-lab)

(defparameter *system-initialized* nil)
(defparameter *display* nil)
(defparameter *screen* nil)

(defun init-xlib ()
  (setf *display* (xlib:open-default-display))
  (unless *display*
    (error "Unable to get valid default display."))
  (setf *screen* (first (xlib:display-roots *display*)))
  (unless *screen*
    (error "Unable to get valid default screen (display root)."))
  (setf *system-initialized* t))


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
