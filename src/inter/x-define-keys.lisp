;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; $Id$
;;;
;;; **********************************************************************
;;; This file was originally written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;; This file initializes character translation.
;;;
#|
============================================================
Change log:
        1/19/94 - Andrew Mickish - Renamed to x-define-keys.lisp
	3/20/92 - Ed Pervin - Almost total rewrite.  Define-keysym takes
			only one argument.  Number pad keys bound to ordinary
			chars.  Minimize number of keyboard-dependent keys.
	3/12/92 - Ed Pervin - Use #+ibm-rt-pc to see if machine is RT.
	1/21/91 - Ed Pervin - Changes for CMUCL on Sparc station.
	3/26/91 - Greg Sylvain - Changes for kcl.
        8/22/90 - Brad Myers - Removed CMU #\F1, etc. use :F1 like others
        8/21/90 - Ed Pervin - Added DECSystem keys
	3/23/90 - Ed Pervin - Added Sun :F10
	11/89 - Ed Pervin - Revised to work on Sun keyboard
	3/89 - Incorporated into Garnet
	Written by Bill Chiles
============================================================
|#
;;; The IBM RT keyboard has X11 keysyms defined for the following modifier
;;; keys, but we leave them mapped to nil indicating that they are non-events
;;; to be ignored:
;;;    ctrl		65507
;;;    meta (left)	65513  -- 65511 on Sun
;;;    meta (right)	65514  -- 65512 on Sun
;;;    shift (left)	65505
;;;    shift (right)	65506
;;;    lock		65509

(in-package "INTERACTORS")

(define-keysym 0 :Alt-Graph)

(define-keysym 32 #\space)
(define-keysym 33 #\!)
(define-keysym 34 #\")
(define-keysym 35 #\#)
(define-keysym 36 #\$)
(define-keysym 37 #\%)
(define-keysym 38 #\&)
(define-keysym 39 #\')
(define-keysym 40 #\()
(define-keysym 41 #\))
(define-keysym 42 #\*)
(define-keysym 43 #\+)
(define-keysym 44 #\,)
(define-keysym 45 #\-)
(define-keysym 46 #\.)
(define-keysym 47 #\/)
(define-keysym 48 #\0)
(define-keysym 49 #\1) 
(define-keysym 50 #\2) 
(define-keysym 51 #\3) 
(define-keysym 52 #\4)
(define-keysym 53 #\5)
(define-keysym 54 #\6)
(define-keysym 55 #\7)
(define-keysym 56 #\8)
(define-keysym 57 #\9)
(define-keysym 58 #\:)
(define-keysym 59 #\;)
(define-keysym 60 #\<)
(define-keysym 61 #\=)
(define-keysym 62 #\>)
(define-keysym 63 #\?)
(define-keysym 64 #\@)
(define-keysym 65 #\A)
(define-keysym 66 #\B)
(define-keysym 67 #\C)
(define-keysym 68 #\D)
(define-keysym 69 #\E)
(define-keysym 70 #\F)
(define-keysym 71 #\G)
(define-keysym 72 #\H)
(define-keysym 73 #\I)
(define-keysym 74 #\J)
(define-keysym 75 #\K)
(define-keysym 76 #\L)
(define-keysym 77 #\M)
(define-keysym 78 #\N)
(define-keysym 79 #\O)
(define-keysym 80 #\P)
(define-keysym 81 #\Q)
(define-keysym 82 #\R)
(define-keysym 83 #\S)
(define-keysym 84 #\T)
(define-keysym 85 #\U)
(define-keysym 86 #\V)
(define-keysym 87 #\W)
(define-keysym 88 #\X)
(define-keysym 89 #\Y)
(define-keysym 90 #\Z)
(define-keysym 91 #\[) 
(define-keysym 92 #\\) 
(define-keysym 93 #\]) 
(define-keysym 94 #\^)
(define-keysym 95 #\_)
(define-keysym 96 #\`)
(define-keysym 97 #\a) 
(define-keysym 98 #\b) 
(define-keysym 99 #\c) 
(define-keysym 100 #\d) 
(define-keysym 101 #\e) 
(define-keysym 102 #\f) 
(define-keysym 103 #\g) 
(define-keysym 104 #\h) 
(define-keysym 105 #\i) 
(define-keysym 106 #\j) 
(define-keysym 107 #\k) 
(define-keysym 108 #\l) 
(define-keysym 109 #\m) 
(define-keysym 110 #\n) 
(define-keysym 111 #\o) 
(define-keysym 112 #\p) 
(define-keysym 113 #\q) 
(define-keysym 114 #\r) 
(define-keysym 115 #\s) 
(define-keysym 116 #\t) 
(define-keysym 117 #\u) 
(define-keysym 118 #\v) 
(define-keysym 119 #\w) 
(define-keysym 120 #\x) 
(define-keysym 121 #\y) 
(define-keysym 122 #\z) 
(define-keysym 126 #\~)
(define-keysym 123 #\{)
(define-keysym 125 #\})
(define-keysym 124 #\|)

(define-keysym 65288 #\backspace)
(define-keysym 65289 #\tab)
(define-keysym 65290 #\linefeed)
(define-keysym 65291 :Clear)
(define-keysym 65293 #\return)			;enter on RT
(define-keysym 65299 :pause)
(define-keysym 65307 #\esc)
(define-keysym 65312 :compose)
(define-keysym 65360 :home)
(define-keysym 65361 :leftarrow)
(define-keysym 65362 :uparrow)
(define-keysym 65363 :rightarrow)
(define-keysym 65364 :downarrow)
(define-keysym 65365 :prev)
(define-keysym 65366 :next)
(define-keysym 65367 :end)
(define-keysym 65376 :select)
(define-keysym 65377 :print)
(define-keysym 65378 :Enter)
(define-keysym 65379 :insert)
(define-keysym 65383 :menu)
(define-keysym 65384 :Find)
(define-keysym 65385 :Stop)
(define-keysym 65386 :Help)
(define-keysym 65387 :Break)
(define-keysym 65407 :numlock)
(define-keysym 65421 #\return) ;; number pad
(define-keysym 65425 :PF1)
(define-keysym 65426 :PF2)
(define-keysym 65427 :PF3)
(define-keysym 65428 :PF4)
(define-keysym 65450 #\*)      ;; number pad
(define-keysym 65451 #\+)      ;; number pad
(define-keysym 65452 #\,)      ;; number pad
(define-keysym 65453 #\-)      ;; number pad
(define-keysym 65454 #\.)      ;; number pad
(define-keysym 65455 #\/)      ;; number pad
(define-keysym 65456 #\0)      ;; number pad
(define-keysym 65457 #\1)      ;; number pad
(define-keysym 65458 #\2)      ;; number pad
(define-keysym 65459 #\3)      ;; number pad
(define-keysym 65460 #\4)      ;; number pad
(define-keysym 65461 #\5)      ;; number pad
(define-keysym 65462 #\6)      ;; number pad
(define-keysym 65463 #\7)      ;; number pad
(define-keysym 65464 #\8)      ;; number pad
(define-keysym 65465 #\9)      ;; number pad
(define-keysym 65469 #\=)      ;; number pad
(define-keysym 65470 :F1)
(define-keysym 65471 :F2)
(define-keysym 65472 :F3)
(define-keysym 65473 :F4)
(define-keysym 65474 :F5)
(define-keysym 65475 :F6)
(define-keysym 65476 :F7)
(define-keysym 65477 :F8)
(define-keysym 65478 :F9)
(define-keysym 65479 :F10)
(define-keysym 65480 :F11)
(define-keysym 65481 :F12)
(define-keysym 65482 :L3)
(define-keysym 65483 :L4)
(define-keysym 65484 :L5)
(define-keysym 65485 :L6)
(define-keysym 65486 :L7)
(define-keysym 65487 :L8)
(define-keysym 65488 :L9)
(define-keysym 65489 :L10)
(define-keysym 65490 :R1)
(define-keysym 65491 :R2)
(define-keysym 65492 :R3)
(define-keysym 65493 :R4)
(define-keysym 65494 :R5)
(define-keysym 65495 :R6)
(define-keysym 65496 :R7)
(define-keysym 65497 :R8)
(define-keysym 65498 :R9)
(define-keysym 65499 :R10)
(define-keysym 65500 :R11)
(define-keysym 65501 :R12)
(define-keysym 65502 :R13)
(define-keysym 65503 :R14)
(define-keysym 65504 :R15)
(define-keysym 65535 #-kcl #\delete #+kcl #\rubout)

(define-keysym 268500736 :Remove)
(define-keysym 268500844 :Reset)
(define-keysym 268500845 :User)
(define-keysym 268500846 :System)
(define-keysym 268500847 :Clear-Line)
(define-keysym 268500848 :Insert-Line)
(define-keysym 268500849 :Delete-Line)
(define-keysym 268500850 :Insert-Char)
(define-keysym 268500851 :Delete-Char)

;;; Function keys which are defined in a
;;; nonstandard way for the IBM RT.
#+ibm-rt-pc
(progn
  (define-keysym 65291 :Clear-Display)
  (define-keysym 65365 :pageup)
  (define-keysym 65366 :pagedown)
  (define-keysym 65377 :Print-Screen)
) ; end +ibm-rt-pc

;;; Function keys which are defined in a
;;; nonstandard way for the DecStation keyboard
#+(or vax dec3100 dec5000)
(progn 
  (define-keysym 65307 :F11)
  (define-keysym 65312 :COMPOSE-CHARACTER)
  (define-keysym 65379 :INSERT-HERE)
  (define-keysym 65383 :DO)
  (define-keysym 65482 :F13)
  (define-keysym 65483 :F14)
  (define-keysym 65486 :F17)
  (define-keysym 65487 :F18)
  (define-keysym 65488 :F19)
  (define-keysym 65489 :F20)
)
