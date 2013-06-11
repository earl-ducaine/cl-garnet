;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mac-keytrans.lisp:  This file initializes character translation on the Mac.
;;;
;;;  NOTE:  These are not character-codes!
;;;         Don't try CODE-CHAR with these nums!
;;;
;;;  These key/code mappings correspond to the physical keys on the keyboard.
;;;  That is, you get the same KEY-CODE when you type "c" or "control-c", but
;;;  you wouldn't get the same CHARACTER-CODE.  The two types of codes are
;;;  stored in the high- and low-order bytes of the EventRecord.message field
;;;  of ccl:*current-event*.
;;;
;;;  These mappings are documented in Inside Macintosh I, p. 251.
;;;  As of 1/19/94, this file only supports the U.S. keyboard.
;;;
#|
============================================================
Change log:
6/14/94 - Brad Myers - Changed some characters to keywords (like :HELP)       
1/19/94 - Andrew Mickish - Created, modeled after garnet-keytrans.lisp
============================================================
|#

(in-package "INTERACTORS")
#|
(defparameter *leftdown-key* 105)
(defparameter *middledown-key* 107)
(defparameter *rightdown-key* 113)
|#
;;; F10-11-12 for powerbook
;;; RGA Function keys seem to loose under MacOS X/Classic.  Switch to keypad
#|
(defparameter *leftdown-key* 109)
(defparameter *middledown-key* 103)
(defparameter *rightdown-key* 111)
|#
;;; F10-11-12 for powerbook
(defparameter *leftdown-key* 81) ;; Keypad =
(defparameter *middledown-key* 75) ;; Keypad /
(defparameter *rightdown-key* 67) ;; keypad *

(define-keysym 50 #\`)
(define-keysym 18 #\1)
(define-keysym 19 #\2)
(define-keysym 20 #\3)
(define-keysym 21 #\4)
(define-keysym 23 #\5)
(define-keysym 22 #\6)
(define-keysym 26 #\7)
(define-keysym 28 #\8)
(define-keysym 25 #\9)
(define-keysym 29 #\0)
(define-keysym 27 #\-)
(define-keysym 24 #\=)
(define-keysym 51 #\Delete)

(define-keysym 48 #\Tab)
(define-keysym 12 #\q)
(define-keysym 13 #\w)
(define-keysym 14 #\e)
(define-keysym 15 #\r)
(define-keysym 17 #\t)
(define-keysym 16 #\y)
(define-keysym 32 #\u)
(define-keysym 34 #\i)
(define-keysym 31 #\o)
(define-keysym 35 #\p)
(define-keysym 33 #\[)
(define-keysym 30 #\])
(define-keysym 42 #\\)

;(define-keysym 57 #\Caps-Lock)
(define-keysym 0 #\a)
(define-keysym 1 #\s)
(define-keysym 2 #\d)
(define-keysym 3 #\f)
(define-keysym 5 #\g)
(define-keysym 4 #\h)
(define-keysym 38 #\j)
(define-keysym 40 #\k)
(define-keysym 37 #\l)
(define-keysym 41 #\;)
(define-keysym 39 #\')
(define-keysym 36 #\Return)  ; a.k.a. #\Newline

;(define-keysym 56 #\Shift)
(define-keysym 6 #\z)
(define-keysym 7 #\x)
(define-keysym 8 #\c)
(define-keysym 9 #\v)
(define-keysym 11 #\b)
(define-keysym 45 #\n)
(define-keysym 46 #\m)
(define-keysym 43 #\,)
(define-keysym 47 #\.)
(define-keysym 44 #\/)

;(define-keysym 58 #\Option)
;(define-keysym 55 #\Command)
(define-keysym 49 #\ )
;(define-keysym 52 #\Enter)


(define-keysym 53 #\Escape)
(define-keysym 122 :F1)
(define-keysym 120 :F2)
(define-keysym  99 :F3)
(define-keysym 118 :F4)
(define-keysym  96 :F5)
(define-keysym  97 :F6)
(define-keysym  98 :F7)
(define-keysym 100 :F8)
(define-keysym 101 :F9)
(define-keysym 109 :F10)
(define-keysym 103 :F11)
(define-keysym 111 :F12)
(define-keysym 105 :F13)
(define-keysym 107 :F14)
(define-keysym 113 :F15)

(define-keysym 126 :UpArrow)
(define-keysym 125 :DownArrow)
(define-keysym 123 :LeftArrow)
(define-keysym 124 :RightArrow)

#|
   ;; These special characters (e.g., #\UpArrow) are defined on the Mac,
   ;; but it is easier to adopt the X convention already built into Interactors
   ;; and use keywords.  See inter/textkeyhandling.lisp.
   (define-keysym 126 #\UpArrow)
   (define-keysym 125 #\DownArrow)
   (define-keysym 123 #\BackArrow)
   (define-keysym 124 #\ForwardArrow)
|#

(define-keysym 114 :HELP)
(define-keysym 115 :HOME)
(define-keysym 116 :PAGE-UP)
(define-keysym 117 #\DEL)
(define-keysym 119 :END)
(define-keysym 121 :PAGE)

;; Keypad
(define-keysym 71 #\ESC)
(define-keysym 81 #\=)
(define-keysym 75 #\/)
(define-keysym 67 #\*)
(define-keysym 89 #\7)
(define-keysym 91 #\8)
(define-keysym 92 #\9)
(define-keysym 78 #\-)
(define-keysym 86 #\4)
(define-keysym 87 #\5)
(define-keysym 88 #\6)
(define-keysym 69 #\+)
(define-keysym 83 #\1)
(define-keysym 84 #\2)
(define-keysym 85 #\3)
(define-keysym 76 #\Enter)
(define-keysym 82 #\0)
(define-keysym 65 #\.)
