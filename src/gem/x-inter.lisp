;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GEM; Base: 10 -*-
;;;
;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.  If
;;; you are using this code or any part of Garnet, please contact
;;; garnet@cs.cmu.edu to be put on the mailing list.

(in-package :gem)

(defparameter *last-state* NIL)
(defparameter *last-code* NIL)
(declaim (integer *last-time*))
(defparameter *last-time* 0)

(defun x-Check-Double-Press (root-window state code time)
  (declare (ignore root-window))
  (declare (integer time))
  (if inter::*double-click-time*
      (let (newcode)
	(if (and (eq state *last-state*)
		 (eq code *last-code*)
		 (<= (- time *last-time*) inter::*double-click-time*))
	    (setf newcode (+ code inter::*double-offset*)) ;; is double click
	    (setf newcode code))   ;; else not double click
	;; set up for next time
	(setf *last-state* state)
	(setf *last-code* code)
	(setf *last-time* time)
	newcode)
      ;; else not interested in double click
      code))

(defun x-set-interest-in-moved (window interestedp)
  ;; Macroexpansion of (inter::if-debug :mouse
  ;;                    (format t "interested in mouse moved now ~s~%"
  ;;                              interestedp))
  ;; required because compiler does not allow a forward reference to a macro
  ;; BEGIN
  #+garnet-debug
  (IF (AND INTER::*INT-DEBUG* (INTER::TRACE-TEST :MOUSE))
      (LET ((*PRINT-PRETTY* NIL))
	(FORMAT T "interested in mouse moved now ~s~%" INTERESTEDP)))
  ;; END
  (let ((drawable (get-value window :drawable)))
    (if drawable
	(if interestedp
	    (let* ((want-enter-leave (g-value window :want-enter-leave-events))
		   (em (if want-enter-leave :E-K-M :K-M)))
	      ;; this will change an active grab if one is in progress because
	      ;; changing the window's event mask will have no effect if
	      ;; there is an active grab in session
	      (gem:mouse-grab window T want-enter-leave :CHANGE)
	      (gem:set-window-property window :EVENT-MASK em)
	      (s-value window :event-mask em))
	    ;; else turn want-motion off.  The pem to use depends on
	    ;; whether there are any multi-window interactors on this window
	    (let ((em (or (g-value window :ignore-motion-em)
			  (if (g-value window :want-enter-leave-events)
                              :E-K
			      :K))))
	      (gem:set-window-property window :EVENT-MASK em)
	      (s-value window :event-mask em)))
	;; here no drawable yet, set the field in the window so it will
	;; use the right one when the drawable is created
	(s-value window :want-running-em interestedp))))

(defun x-translate-mouse-character (root-window button-code modifier-bits
                                    event-key)
  (declare (ignore root-window))
  (case event-key
    (:button-release
     (aref inter::*mouse-up-translations*  button-code
           (inter::modifier-index modifier-bits)))
    (:button-press
     (aref inter::*mouse-down-translations*  button-code
           (inter::modifier-index modifier-bits)))))

(defun x-translate-character (window x y bits scan-code time)
  "Translates scan-code and modifier bits to a Lisp character.  The
   scan code is first mapped to a keysym with index 0 for unshifted
   and index 1 for shifted or lock.  If this keysym does not map to a
   character, and it is not a modifier key (shift, ctrl, etc.), then
   an error is signaled.  If the keysym is a modifier key, then nil is
   returned."
  (declare (ignore x y time))
  (let (shiftp)
    (dolist (ele inter::*modifier-translations*)
      (unless (zerop (logand (car ele) bits))
	(case (cdr ele)
	  (:shift (setf shiftp t))
	  (:lock (setf shiftp t)))))
    (let* ((keysym (gem:translate-code window scan-code shiftp))
	   (temp-char (gethash keysym inter::*keysym-translations*)))
      (if (null temp-char)
	  (if (<= 65505 keysym 65518)	;modifier keys.
	      nil
	      (unless inter::*ignore-undefined-keys*
		(error "Undefined keysym ~S, describe Inter:DEFINE-KEYSYM."
		       keysym)))
	  (inter::base-char-to-character temp-char bits)))))
