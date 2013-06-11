;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;; Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;


(in-package "INTERACTORS")


;; definitions of garnet mouse keywords for the left button release
(define-mouse-up *left-button* nil :leftup)

(define-mouse-up *left-button* '(:control) :control-leftup)
(define-mouse-up *left-button* '(:shift) :shift-leftup)
(define-mouse-up *left-button* '(:lock) :shift-leftup)
(define-mouse-up *left-button* '(:meta) :meta-leftup)


(define-mouse-up *left-button* '(:control :shift) :shift-control-leftup) 
(define-mouse-up *left-button* '(:control :lock) :shift-control-leftup) 
(define-mouse-up *left-button* '(:control :meta) :control-meta-leftup)

(define-mouse-up *left-button* '(:shift :lock) :shift-leftup)
(define-mouse-up *left-button* '(:shift :meta) :shift-meta-leftup)

(define-mouse-up *left-button* '(:lock :meta) :shift-meta-leftup)


(define-mouse-up *left-button* '(:control :shift :lock) :shift-control-leftup)
(define-mouse-up *left-button* '(:control :shift :meta) :shift-control-meta-leftup)

(define-mouse-up *left-button* '(:shift :lock :meta) :shift-meta-leftup)

(define-mouse-up *left-button* '(:control :shift :lock :meta) :shift-control-meta-leftup)


;; definitions of garnet mouse keywords for the middle button release
(define-mouse-up *middle-button* nil :middleup)

(define-mouse-up *middle-button* '(:control) :control-middleup)
(define-mouse-up *middle-button* '(:shift) :shift-middleup)
(define-mouse-up *middle-button* '(:lock) :shift-middleup)
(define-mouse-up *middle-button* '(:meta) :meta-middleup)


(define-mouse-up *middle-button* '(:control :shift) :shift-control-middleup) 
(define-mouse-up *middle-button* '(:control :lock) :shift-control-middleup) 
(define-mouse-up *middle-button* '(:control :meta) :control-meta-middleup)

(define-mouse-up *middle-button* '(:shift :lock) :shift-middleup)
(define-mouse-up *middle-button* '(:shift :meta) :shift-meta-middleup)

(define-mouse-up *middle-button* '(:lock :meta) :shift-meta-middleup)


(define-mouse-up *middle-button* '(:control :shift :lock) :shift-control-middleup)
(define-mouse-up *middle-button* '(:control :shift :meta) :shift-control-meta-middleup)

(define-mouse-up *middle-button* '(:shift :lock :meta) :shift-meta-middleup)


(define-mouse-up *middle-button* '(:control :shift :lock :meta)
  :shift-control-meta-middleup)



;;    definitions of garnet mouse keywords for the right button release
(define-mouse-up *right-button* nil :rightup)

(define-mouse-up *right-button* '(:control) :control-rightup)
(define-mouse-up *right-button* '(:shift) :shift-rightup)
(define-mouse-up *right-button* '(:lock) :shift-rightup)
(define-mouse-up *right-button* '(:meta) :meta-rightup)


(define-mouse-up *right-button* '(:control :shift) :shift-control-rightup) 
(define-mouse-up *right-button* '(:control :lock) :shift-control-rightup) 
(define-mouse-up *right-button* '(:control :meta) :control-meta-rightup)

(define-mouse-up *right-button* '(:shift :lock) :shift-rightup)
(define-mouse-up *right-button* '(:shift :meta) :shift-meta-rightup)

(define-mouse-up *right-button* '(:lock :meta) :shift-meta-rightup)


(define-mouse-up *right-button* '(:control :shift :lock) :shift-control-rightup)
(define-mouse-up *right-button* '(:control :shift :meta) :shift-control-meta-rightup)

(define-mouse-up *right-button* '(:shift :lock :meta) :shift-meta-rightup)


(define-mouse-up *right-button* '(:control :shift :lock :meta)
  :shift-control-meta-rightup)



;;    definitions of garnet mouse keywords for the left button press
(define-mouse-down *left-button* nil :leftdown)

(define-mouse-down *left-button* '(:control) :control-leftdown)
(define-mouse-down *left-button* '(:shift) :shift-leftdown)
(define-mouse-down *left-button* '(:lock) :shift-leftdown)
(define-mouse-down *left-button* '(:meta) :meta-leftdown)


(define-mouse-down *left-button* '(:control :shift) :shift-control-leftdown) 
(define-mouse-down *left-button* '(:control :lock) :shift-control-leftdown) 
(define-mouse-down *left-button* '(:control :meta) :control-meta-leftdown)

(define-mouse-down *left-button* '(:shift :lock) :shift-leftdown)
(define-mouse-down *left-button* '(:shift :meta) :shift-meta-leftdown)

(define-mouse-down *left-button* '(:lock :meta) :shift-meta-leftdown)


(define-mouse-down *left-button* '(:control :shift :lock) :shift-control-leftdown)
(define-mouse-down *left-button* '(:control :shift :meta) :shift-control-meta-leftdown)

(define-mouse-down *left-button* '(:shift :lock :meta) :shift-meta-leftdown)

(define-mouse-down *left-button* '(:control :shift :lock :meta) :shift-control-meta-leftdown)


;;;;    definitions of garnet mouse keywords for the middle button press
(define-mouse-down *middle-button* nil :middledown)

(define-mouse-down *middle-button* '(:control) :control-middledown)
(define-mouse-down *middle-button* '(:shift) :shift-middledown)
(define-mouse-down *middle-button* '(:lock) :shift-middledown)
(define-mouse-down *middle-button* '(:meta) :meta-middledown)


(define-mouse-down *middle-button* '(:control :shift) :shift-control-middledown) 
(define-mouse-down *middle-button* '(:control :lock) :shift-control-middledown) 
(define-mouse-down *middle-button* '(:control :meta) :control-meta-middledown)

(define-mouse-down *middle-button* '(:shift :lock) :shift-middledown)
(define-mouse-down *middle-button* '(:shift :meta) :shift-meta-middledown)

(define-mouse-down *middle-button* '(:lock :meta) :shift-meta-middledown)


(define-mouse-down *middle-button* '(:control :shift :lock) :shift-control-middledown)
(define-mouse-down *middle-button* '(:control :shift :meta) :shift-control-meta-middledown)

(define-mouse-down *middle-button* '(:shift :lock :meta) :shift-meta-middledown)


(define-mouse-down *middle-button* '(:control :shift :lock :meta) :shift-control-meta-middledown)



;;    definitions of garnet mouse keywords for the right button press
(define-mouse-down *right-button* nil :rightdown)

(define-mouse-down *right-button* '(:control) :control-rightdown)
(define-mouse-down *right-button* '(:shift) :shift-rightdown)
(define-mouse-down *right-button* '(:lock) :shift-rightdown)
(define-mouse-down *right-button* '(:meta) :meta-rightdown)


(define-mouse-down *right-button* '(:control :shift) :shift-control-rightdown) 
(define-mouse-down *right-button* '(:control :lock) :shift-control-rightdown) 
(define-mouse-down *right-button* '(:control :meta) :control-meta-rightdown)

(define-mouse-down *right-button* '(:shift :lock) :shift-rightdown)
(define-mouse-down *right-button* '(:shift :meta) :shift-meta-rightdown)

(define-mouse-down *right-button* '(:lock :meta) :shift-meta-rightdown)


(define-mouse-down *right-button* '(:control :shift :lock) :shift-control-rightdown)
(define-mouse-down *right-button* '(:control :shift :meta) :shift-control-meta-rightdown)

(define-mouse-down *right-button* '(:shift :lock :meta) :shift-meta-rightdown)


(define-mouse-down *right-button* '(:control :shift :lock :meta)
  :shift-control-meta-rightdown)



;;; Scroll wheel buttons.
;;

;;    definitions of garnet mouse keywords for the up-scroll button release
(define-mouse-up *up-scroll-button* nil :upscrollup)

(define-mouse-up *up-scroll-button* '(:control) :control-upscrollup)
(define-mouse-up *up-scroll-button* '(:shift) :shift-upscrollup)
(define-mouse-up *up-scroll-button* '(:lock) :shift-upscrollup)
(define-mouse-up *up-scroll-button* '(:meta) :meta-upscrollup)


(define-mouse-up *up-scroll-button* '(:control :shift) :shift-control-upscrollup) 
(define-mouse-up *up-scroll-button* '(:control :lock) :shift-control-upscrollup) 
(define-mouse-up *up-scroll-button* '(:control :meta) :control-meta-upscrollup)

(define-mouse-up *up-scroll-button* '(:shift :lock) :shift-upscrollup)
(define-mouse-up *up-scroll-button* '(:shift :meta) :shift-meta-upscrollup)

(define-mouse-up *up-scroll-button* '(:lock :meta) :shift-meta-upscrollup)


(define-mouse-up *up-scroll-button* '(:control :shift :lock) :shift-control-upscrollup)
(define-mouse-up *up-scroll-button* '(:control :shift :meta) :shift-control-meta-upscrollup)

(define-mouse-up *up-scroll-button* '(:shift :lock :meta) :shift-meta-upscrollup)


(define-mouse-up *up-scroll-button* '(:control :shift :lock :meta)
  :shift-control-meta-upscrollup)


;;;    definitions of garnet mouse keywords for the down-scroll button release
(define-mouse-up *down-scroll-button* nil :downscrollup)

(define-mouse-up *down-scroll-button* '(:control) :control-downscrollup)
(define-mouse-up *down-scroll-button* '(:shift) :shift-downscrollup)
(define-mouse-up *down-scroll-button* '(:lock) :shift-downscrollup)
(define-mouse-up *down-scroll-button* '(:meta) :meta-downscrollup)


(define-mouse-up *down-scroll-button* '(:control :shift) :shift-control-downscrollup) 
(define-mouse-up *down-scroll-button* '(:control :lock) :shift-control-downscrollup) 
(define-mouse-up *down-scroll-button* '(:control :meta) :control-meta-downscrollup)

(define-mouse-up *down-scroll-button* '(:shift :lock) :shift-downscrollup)
(define-mouse-up *down-scroll-button* '(:shift :meta) :shift-meta-downscrollup)

(define-mouse-up *down-scroll-button* '(:lock :meta) :shift-meta-downscrollup)


(define-mouse-up *down-scroll-button* '(:control :shift :lock) :shift-control-downscrollup)
(define-mouse-up *down-scroll-button* '(:control :shift :meta) :shift-control-meta-downscrollup)

(define-mouse-up *down-scroll-button* '(:shift :lock :meta) :shift-meta-downscrollup)


(define-mouse-up *down-scroll-button* '(:control :shift :lock :meta)
  :shift-control-meta-downscrollup)



;;;    definitions of garnet mouse keywords for the up-scroll button press

(define-mouse-down *up-scroll-button* nil :upscrolldown)

(define-mouse-down *up-scroll-button* '(:control) :control-upscrolldown)
(define-mouse-down *up-scroll-button* '(:shift) :shift-upscrolldown)
(define-mouse-down *up-scroll-button* '(:lock) :shift-upscrolldown)
(define-mouse-down *up-scroll-button* '(:meta) :meta-upscrolldown)


(define-mouse-down *up-scroll-button* '(:control :shift) :shift-control-upscrolldown) 
(define-mouse-down *up-scroll-button* '(:control :lock) :shift-control-upscrolldown) 
(define-mouse-down *up-scroll-button* '(:control :meta) :control-meta-upscrolldown)

(define-mouse-down *up-scroll-button* '(:shift :lock) :shift-upscrolldown)
(define-mouse-down *up-scroll-button* '(:shift :meta) :shift-meta-upscrolldown)

(define-mouse-down *up-scroll-button* '(:lock :meta) :shift-meta-upscrolldown)


(define-mouse-down *up-scroll-button* '(:control :shift :lock) :shift-control-upscrolldown)
(define-mouse-down *up-scroll-button* '(:control :shift :meta) :shift-control-meta-upscrolldown)

(define-mouse-down *up-scroll-button* '(:shift :lock :meta) :shift-meta-upscrolldown)


(define-mouse-down *up-scroll-button* '(:control :shift :lock :meta)
  :shift-control-meta-upscrolldown)



;;;  definitions of garnet mouse keywords for the down-scroll button press

(define-mouse-down *down-scroll-button* nil :downscrolldown)

(define-mouse-down *down-scroll-button* '(:control) :control-downscrolldown)
(define-mouse-down *down-scroll-button* '(:shift) :shift-downscrolldown)
(define-mouse-down *down-scroll-button* '(:lock) :shift-downscrolldown)
(define-mouse-down *down-scroll-button* '(:meta) :meta-downscrolldown)


(define-mouse-down *down-scroll-button* '(:control :shift) :shift-control-downscrolldown) 
(define-mouse-down *down-scroll-button* '(:control :lock) :shift-control-downscrolldown) 
(define-mouse-down *down-scroll-button* '(:control :meta) :control-meta-downscrolldown)

(define-mouse-down *down-scroll-button* '(:shift :lock) :shift-downscrolldown)
(define-mouse-down *down-scroll-button* '(:shift :meta) :shift-meta-downscrolldown)

(define-mouse-down *down-scroll-button* '(:lock :meta) :shift-meta-downscrolldown)


(define-mouse-down *down-scroll-button* '(:control :shift :lock) :shift-control-downscrolldown)
(define-mouse-down *down-scroll-button* '(:control :shift :meta) :shift-control-meta-downscrolldown)

(define-mouse-down *down-scroll-button* '(:shift :lock :meta) :shift-meta-downscrolldown)


(define-mouse-down *down-scroll-button* '(:control :shift :lock :meta)
  :shift-control-meta-downscrolldown)


;;; Now repeat the entire set for the double-click version
;;

;;;    definitions of garnet mouse keywords for the left button release
(define-mouse-up *double-left-button* nil :double-leftup)

(define-mouse-up *double-left-button* '(:control) :control-double-leftup)
(define-mouse-up *double-left-button* '(:shift) :shift-double-leftup)
(define-mouse-up *double-left-button* '(:lock) :shift-double-leftup)
(define-mouse-up *double-left-button* '(:meta) :meta-double-leftup)


(define-mouse-up *double-left-button* '(:control :shift) :shift-control-double-leftup) 
(define-mouse-up *double-left-button* '(:control :lock) :shift-control-double-leftup) 
(define-mouse-up *double-left-button* '(:control :meta) :control-meta-double-leftup)

(define-mouse-up *double-left-button* '(:shift :lock) :shift-double-leftup)
(define-mouse-up *double-left-button* '(:shift :meta) :shift-meta-double-leftup)

(define-mouse-up *double-left-button* '(:lock :meta) :shift-meta-double-leftup)


(define-mouse-up *double-left-button* '(:control :shift :lock) :shift-control-double-leftup)
(define-mouse-up *double-left-button* '(:control :shift :meta) :shift-control-meta-double-leftup)

(define-mouse-up *double-left-button* '(:shift :lock :meta)
  :shift-meta-double-leftup)

(define-mouse-up *double-left-button* '(:control :shift :lock :meta)
  :shift-control-meta-double-leftup)


;;; definitions of garnet mouse keywords for the middle button release
(define-mouse-up *double-middle-button* nil :double-middleup)

(define-mouse-up *double-middle-button* '(:control) :control-double-middleup)
(define-mouse-up *double-middle-button* '(:shift) :shift-double-middleup)
(define-mouse-up *double-middle-button* '(:lock) :shift-double-middleup)
(define-mouse-up *double-middle-button* '(:meta) :meta-double-middleup)


(define-mouse-up *double-middle-button* '(:control :shift)
  :shift-control-double-middleup) 
(define-mouse-up *double-middle-button* '(:control :lock)
  :shift-control-double-middleup) 
(define-mouse-up *double-middle-button* '(:control :meta)
  :control-meta-double-middleup)

(define-mouse-up *double-middle-button* '(:shift :lock)
  :shift-double-middleup)
(define-mouse-up *double-middle-button* '(:shift :meta)
  :shift-meta-double-middleup)

(define-mouse-up *double-middle-button* '(:lock :meta)
  :shift-meta-double-middleup)


(define-mouse-up *double-middle-button* '(:control :shift :lock)
  :shift-control-double-middleup)
(define-mouse-up *double-middle-button* '(:control :shift :meta)
  :shift-control-meta-double-middleup)

(define-mouse-up *double-middle-button* '(:shift :lock :meta)
  :shift-meta-double-middleup)


(define-mouse-up *double-middle-button* '(:control :shift :lock :meta)
  :shift-control-meta-double-middleup)



;;; definitions of garnet mouse keywords for the right button release
(define-mouse-up *double-right-button* nil :double-rightup)

(define-mouse-up *double-right-button* '(:control) :control-double-rightup)
(define-mouse-up *double-right-button* '(:shift) :shift-double-rightup)
(define-mouse-up *double-right-button* '(:lock) :shift-double-rightup)
(define-mouse-up *double-right-button* '(:meta) :meta-double-rightup)


(define-mouse-up *double-right-button* '(:control :shift)
  :shift-control-double-rightup) 
(define-mouse-up *double-right-button* '(:control :lock)
  :shift-control-double-rightup) 
(define-mouse-up *double-right-button* '(:control :meta)
  :control-meta-double-rightup)

(define-mouse-up *double-right-button* '(:shift :lock) :shift-double-rightup)
(define-mouse-up *double-right-button* '(:shift :meta)
  :shift-meta-double-rightup)

(define-mouse-up *double-right-button* '(:lock :meta)
  :shift-meta-double-rightup)


(define-mouse-up *double-right-button* '(:control :shift :lock)
  :shift-control-double-rightup)
(define-mouse-up *double-right-button* '(:control :shift :meta)
  :shift-control-meta-double-rightup)

(define-mouse-up *double-right-button* '(:shift :lock :meta)
  :shift-meta-double-rightup)


(define-mouse-up *double-right-button* '(:control :shift :lock :meta)
  :shift-control-meta-double-rightup)



;;; definitions of garnet mouse keywords for the left button press
(define-mouse-down *double-left-button* nil :double-leftdown)

(define-mouse-down *double-left-button* '(:control) :control-double-leftdown)
(define-mouse-down *double-left-button* '(:shift) :shift-double-leftdown)
(define-mouse-down *double-left-button* '(:lock) :shift-double-leftdown)
(define-mouse-down *double-left-button* '(:meta) :meta-double-leftdown)


(define-mouse-down *double-left-button* '(:control :shift)
  :shift-control-double-leftdown) 
(define-mouse-down *double-left-button* '(:control :lock)
  :shift-control-double-leftdown) 
(define-mouse-down *double-left-button* '(:control :meta)
  :control-meta-double-leftdown)

(define-mouse-down *double-left-button* '(:shift :lock)
  :shift-double-leftdown)
(define-mouse-down *double-left-button* '(:shift :meta)
  :shift-meta-double-leftdown)

(define-mouse-down *double-left-button* '(:lock :meta)
  :shift-meta-double-leftdown)


(define-mouse-down *double-left-button* '(:control :shift :lock)
  :shift-control-double-leftdown)
(define-mouse-down *double-left-button* '(:control :shift :meta)
  :shift-control-meta-double-leftdown)

(define-mouse-down *double-left-button* '(:shift :lock :meta)
  :shift-meta-double-leftdown)

(define-mouse-down *double-left-button* '(:control :shift :lock :meta)
  :shift-control-meta-double-leftdown)


;;; definitions of garnet mouse keywords for the middle button press
(define-mouse-down *double-middle-button* nil :double-middledown)

(define-mouse-down *double-middle-button* '(:control)
  :control-double-middledown)
(define-mouse-down *double-middle-button* '(:shift) :shift-double-middledown)
(define-mouse-down *double-middle-button* '(:lock) :shift-double-middledown)
(define-mouse-down *double-middle-button* '(:meta) :meta-double-middledown)


(define-mouse-down *double-middle-button* '(:control :shift)
  :shift-control-double-middledown) 
(define-mouse-down *double-middle-button* '(:control :lock)
  :shift-control-double-middledown) 
(define-mouse-down *double-middle-button* '(:control :meta)
  :control-meta-double-middledown)

(define-mouse-down *double-middle-button* '(:shift :lock)
  :shift-double-middledown)
(define-mouse-down *double-middle-button* '(:shift :meta)
  :shift-meta-double-middledown)

(define-mouse-down *double-middle-button* '(:lock :meta)
  :shift-meta-double-middledown)


(define-mouse-down *double-middle-button* '(:control :shift :lock)
  :shift-control-double-middledown)
(define-mouse-down *double-middle-button* '(:control :shift :meta)
  :shift-control-meta-double-middledown)

(define-mouse-down *double-middle-button* '(:shift :lock :meta)
  :shift-meta-double-middledown)


(define-mouse-down *double-middle-button* '(:control :shift :lock :meta)
  :shift-control-meta-double-middledown)



;;; definitions of garnet mouse keywords for the right button press
(define-mouse-down *double-right-button* nil :double-rightdown)

(define-mouse-down *double-right-button* '(:control) :control-double-rightdown)
(define-mouse-down *double-right-button* '(:shift) :shift-double-rightdown)
(define-mouse-down *double-right-button* '(:lock) :shift-double-rightdown)
(define-mouse-down *double-right-button* '(:meta) :meta-double-rightdown)


(define-mouse-down *double-right-button* '(:control :shift)
  :shift-control-double-rightdown) 
(define-mouse-down *double-right-button* '(:control :lock)
  :shift-control-double-rightdown) 
(define-mouse-down *double-right-button* '(:control :meta)
  :control-meta-double-rightdown)

(define-mouse-down *double-right-button* '(:shift :lock)
  :shift-double-rightdown)
(define-mouse-down *double-right-button* '(:shift :meta)
  :shift-meta-double-rightdown)

(define-mouse-down *double-right-button* '(:lock :meta)
  :shift-meta-double-rightdown)


(define-mouse-down *double-right-button* '(:control :shift :lock)
  :shift-control-double-rightdown)
(define-mouse-down *double-right-button* '(:control :shift :meta)
  :shift-control-meta-double-rightdown)

(define-mouse-down *double-right-button* '(:shift :lock :meta)
  :shift-meta-double-rightdown)


(define-mouse-down *double-right-button* '(:control :shift :lock :meta)
  :shift-control-meta-double-rightdown)

