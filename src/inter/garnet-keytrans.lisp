;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;*******************************************************************;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;

;;  Most of the code in here was snarfed from Hemlock. Certainly the concepts were.
;;  The snarfed Hemlock code/documentation was written by Bill Chiles.
;;  Incorporated in the Garnet code in March 1989.

;;  This code is part of the effort to make the event handling code in Garnet
;;  as portable as possible.  
;;  There are four tables.  Two of them map the mouse buttons to 
;;  Garnet mouse keywords (like :leftdown, :shift-control-rightup, etc)
;;  The other two map keyboard keysyms to Lisp characters.  
;;  To alter the contents of the tables, see the (define-xxx ...) calls in
;;  define-xxx-keys.lisp


;;; Change log:
;;  01/20/94 Andrew MIckish  - Moved device-specific stuff into x-inter.lisp
;;                             and mac-inter.lisp
;;  10/21/92 Dave Kosbie     - added *katie-base-char* global to hold keysym of
;; 		               most recent char translated (and split #'translate-character
;;                             so it calls base-char-to-character, which is needed by Katie
;;   3/18/92 Ed Pervin       - Eliminated *shifted-keysym-translations*
;; 		               Always convert control characters to keywords.
;;   3/02/92 Andrew Mickish  - Added #-cmu switches in translate-character
;;                             to avoid binding LOCKP.
;;   1/30/92 Brad Myers      - added support for double-click
;;   1/21/92 Ed Pervin       - Changes for CMUCL on Sparc station.
;;   8/21/90 Ken Meltsner    - Added *ignore-undefined-keys*
;;  10/25/89 Brad Myers      - modified so that keywords can be used as key
;;                             names
;;   9/16/89 Brad Myers      - Removed the exports to avoid name conflicts
;;   4/7/89  Brad Myers      - Moved to Interactors package
;;   3/15/89 Lynn Baumeister - created by editing Hemlock code


(in-package "INTERACTORS")

;;; begin a section snarfed from Hemlock

;;; X modifier bits translation
;;;
(defvar *modifier-translations* ())

(defun define-keyboard-modifier (clx-mask modifier-name)
  "Causes clx-mask to be interpreted as modifier-name which must be one of
   :control, :meta, :super, :hyper, :shift, or :lock."
  (let ((map (rassoc clx-mask *modifier-translations*)))
    (if map
	(rplacd map modifier-name)
	(push (cons clx-mask modifier-name) *modifier-translations*))))

(let ((root (g-value opal:device-info :current-root)))
  (define-keyboard-modifier (gem:create-state-mask root :control) :control)
  (define-keyboard-modifier (gem:create-state-mask root :mod-1) :meta)
  (define-keyboard-modifier (gem:create-state-mask root :shift) :shift)
  (define-keyboard-modifier (gem:create-state-mask root :lock) :lock))

;;; end section snarfed from Hemlock

(defparameter *num-modifier-keys* 4)


(defparameter *num-mouse-buttons* 24
  "XX At least 11 buttons....(was) 3 buttons * 2 (for double-click")
(defparameter *mouse-translation-dimensions*
  (list  (1+ *num-mouse-buttons*) (* *num-modifier-keys* *num-modifier-keys*)))

(defparameter *mouse-down-translations* (make-array *mouse-translation-dimensions*))
(defparameter *mouse-up-translations* (make-array *mouse-translation-dimensions*))
 
;; modifier-bits = '(:shift :control)

(defmacro mouse-index (modifier-bits)
  `(let ((sum 0))
     (dolist (mod-bit ,modifier-bits)
       (incf sum (car (rassoc mod-bit *modifier-translations*)))) sum))

;; X11 documentation merely says that pointer keycode numbers begin at 1
;; at CMU on the RT's, they get numbered left->right (makes sense)
(defvar *left-button*        1)
(defvar *middle-button*      2)
(defvar *right-button*       3)
;;; Scroll wheel "buttons".
(defvar *up-scroll-button*   4)
(defvar *down-scroll-button* 5)

;; double click values
(defvar *double-offset*      5)		; The amount to add to *xx-button to get double-xx
					; For example: (+ *left-button* *double-offset*)
					;              = *double-left-button*
(defvar *double-left-button*   (+ *left-button*   *double-offset*))
(defvar *double-middle-button* (+ *middle-button* *double-offset*))
(defvar *double-right-button*  (+ *right-button*  *double-offset*))

;; Controls spacing between clicks in a multiple-click event for X
(defparameter *double-click-time* 250) ; in milleseconds

(defmacro define-mouse-up (button modifier-bits garnet-keyword)
  `(setf (aref *mouse-up-translations* ,button (mouse-index, modifier-bits))
	,garnet-keyword))

(defmacro define-mouse-down (button modifier-bits garnet-keyword)
  `(setf (aref *mouse-down-translations* ,button (mouse-index, modifier-bits))
	,garnet-keyword))

(defmacro modifier-index (incoming-bits)
  `(let ((sum 0))
    (dolist (ele *modifier-translations*)
      (let ((bit (car ele)))
	(unless (zerop (logand bit ,incoming-bits))
	  (incf sum bit))))
    sum))



;;; Borrowed from Hemlock -- substitute "Garnet" for "Hemlock" below
;;  Hemlock uses its own keysym to character translation since this is easier
;;  and more versatile than the CLX design.  Also, using CLX's mechanism is no
;;  more portable than writing our own translation based on the X11 protocol
;;  keysym specification.
;; 
;;  In the hash table, nil indicates a non-event which is pertinent to
;;  ignoring modifier keys being pressed prior to pressing a key to be modified.
;; 
;;  This mapping is initialized with DEFINE-KEYSYM in define-keys.Lisp

(defvar *ignore-undefined-keys* T)

(defvar *keysym-translations* (make-hash-table))
(defvar *the-keyword-package* (find-package 'keyword))

;; Will also handle symbols in the keyword package as characters
(defun define-keysym (keysym char)
  "Defines a keysym for Hemlock's translation."
  (if (and (symbolp char)(eq (symbol-package char) *the-keyword-package*))
      ;; then just hash the keyword
      (setf (gethash keysym *keysym-translations*) char)
      ;; else make sure it is a character, and hash
      (progn
	(check-type char character)
	(setf (gethash keysym *keysym-translations*) char)))
  t)

;;; The *prefixes* array is used by both the X and Mac modules.  It is a
;;  mapping from a set of bits into a set of modifier keys.
;;      ---------------------------------
;;      | META | CONTROL | SHIFT | LOCK |    (shift and lock may be backwards)
;;      ---------------------------------
;;         8        4        2       1
;;  Given four bits, each bit corresponds to one of these modifiers.  If the
;;  bit is 1, then that modifier key was being held down during the event.
;;  You can just use the set of bits as an index into this array to get the
;;  corresponding set of modifiers.
;; 
;;  The interface to the *prefixes* array is through the function
;;  Base-Char-To-Character, defined below.

(defparameter *prefixes* 
  (make-array 16 :initial-contents
	      '(NIL			 ;0
		"SHIFT-"		 ;1
		"SHIFT-"		 ;2
		"SHIFT-"		 ;3
		"CONTROL-"		 ;4
		"SHIFT-CONTROL-"	 ;5
		"SHIFT-CONTROL-"	 ;6
		"SHIFT-CONTROL-"	 ;7
		"META-"			 ;8
		"SHIFT-META-"		 ;9
		"SHIFT-META-"		 ;10
		"SHIFT-META-"		 ;11
		"CONTROL-META-"		 ;12
		"SHIFT-CONTROL-META-"	 ;13
		"SHIFT-CONTROL-META-"	 ;14
		"SHIFT-CONTROL-META-"))) ;15

(defun make-keyword-char (symbol bits)
  (let ((prefix (aref *prefixes* (logand bits 15))))
    (if prefix
      (intern (concatenate 'simple-string
			    prefix
			    ;; Unicode broke me. At least with cmucl and sbcl.
			    #-(and)
			    (if (characterp symbol)
				(let ((name (char-name symbol)))
				  (if name
				      (string-upcase name)
				      (string symbol)))
				(symbol-name symbol))
			    (if (characterp symbol)
				(if (standard-char-p symbol)
				    (string symbol)
				    (string-upcase (char-name symbol)))
				(symbol-name symbol)))
	      'keyword)
      symbol)))

(defun base-char-to-character (base-char bits)
  (if (keywordp base-char)		; special, create a symbol for the keyword
      (make-keyword-char base-char bits)
      (if (< bits 4)			; nothing except maybe shift,
	  base-char
	  (make-keyword-char base-char (logand bits 12)))))

