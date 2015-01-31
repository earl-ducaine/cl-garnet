;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;*******************************************************************;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;


;;; Changes:
;; 
;;  16-Mar-94 Mickish Added Mac version
;;  23-Aug-93 Mickish Added CLISP switches
;;  15-Apr-92 Pervin  Adopted code from opal/defs.lisp to find display-name.


(in-package :COMMON-LISP-USER)

(let* ((display-name (xlib::getenv "DISPLAY"))
       (colon-posn (position #\: display-name)))
  (when colon-posn (setq display-name (subseq display-name 0 colon-posn)))
  (setq display-name (or display-name
			 #-allegro (machine-instance)
			 #+allegro (short-site-name)))
  (let* ((display (xlib:open-display display-name))
         (screen (xlib:display-default-screen display))
         (root (xlib:screen-root screen))
         (black (xlib:screen-black-pixel screen))
         (white (xlib:screen-white-pixel screen))
         (window (xlib:create-window :parent root :x 500 :y 100
				     :width 400 :height 400
				     :background white :border black
				     :event-mask '(:button-press :key-press)
				     :border-width 2 :override-redirect :off)))
    (xlib:map-window window)
    (xlib:display-force-output display)
    (format t "~%A window should appear on your screen.~%")
    (format t "Place your cursor inside the window and start typing.~%")
    (format t "Each time you hit a key, two numbers will be printed.~%")
    (format t "The first number is the keysym associated with the key you hit.~%")
    (format t "The second number is the keysym associated with the~%")
    (format t "       shift of the key you hit.~%")
    (format t "Those number is used in the file x-define-keys.lisp~%")
    (format t "       to map keysyms to keys.~%")
    (format t "~%")
    (format t "Get out of this by clicking any mouse button while in the window.~%")
    (format t "~%")
    (force-output)
    (xlib:event-case (display :discard-p t)
		     (:key-press (code)
				 (format t "keysym = ~a  ~a~%"
					 (xlib:keycode->keysym display code 0)
					 (xlib:keycode->keysym display code 1))
				 (force-output)
				 nil)
		     (:button-press () t)
		     (t () nil))
    (xlib:unmap-window window)
    (xlib:display-force-output display)))
