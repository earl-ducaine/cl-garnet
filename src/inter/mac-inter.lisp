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
;;; 10/03/03 Russell Almond -- Carbon Compatibility
;;; 09/24/96 Bernd Meyer see below (tagged by BM)
;;; 03/16/94  Andrew Mickish - Macroexpanded If-Debug call in Set-Interest-...
;;; 01/19/94  Andrew Mickish - Created

(in-package "GEM")


;; Internal variables that maintain state when checking for double clicking
(defparameter *last-state* NIL)
(defparameter *last-code* NIL)
(defparameter *last-time* 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EVENT HANDLING for mouse-move events -- see also MAC-Set-Interest-In-Moved
;;;    (other event-handler methods are defined in mac.lisp)

(defvar *active-drawable* NIL)

;; The ccl:*eventhook* variable contains a function that gets first pick on
;; all the events.  We set this variable with MAC-mouse-move-event-handler
;; when we want to get mouse-move events.  If the function returns T, then
;; MCL assumes the event has been handled, and does not pass it to the other
;; event-handling methods.
;;
;; The MAC-mouse-event-handler executes under the assumption that Garnet is
;; currently interested in mouse-move events.  (If we didn't want them, then
;; inter::Set-Interest-In-Moved or the view-[de]activate-event-handers would
;; have set ccl:*eventhook* to NIL.)
;;
(let ((old-x 0)
      (old-y 0))
  (defun mac-mouse-move-event-handler ()
    ;; The WHAT field of *current-event* is 0 unless something like a keyboard
    ;; or mouse-click event is being handled
    (when (and (zerop (ccl:rref ccl:*current-event* :EventRecord.what))
               gem::*active-drawable*)
      ;; Convert coordinates from global to event-window
      (let* ((opal-window (MAC-window-from-drawable
                           *root-window* gem::*active-drawable*))
;;; BM             (wptr (ccl:wptr gem::*active-drawable*))
;;; BM             (top-window (ccl:window-object wptr)
;;; BM             (where (ccl:%global-to-local
;;; BM                    wptr (ccl:rref ccl:*current-event* :EventRecord.where))))
;;; BM The ccl:%global-to-local function used originally in Garnet does not work 
;;; BM correectly in this setting with PPC MCL 3.9. Why not use the officially
;;; BM documented function ccl:global-to-local ?
             (where (ccl:global-to-local
                     gem::*active-drawable* (ccl:rref ccl:*current-event* :EventRecord.where))))
;;; BM       (unless (eq *active-drawable* top-window)
;;; BM         (setf where (ccl:convert-coordinates where
;;; BM                                              top-window *active-drawable*)))
        (setq er ccl:*current-event*)
        (setq ad gem::*active-drawable*)
        (let ((x (ccl:point-h where))
              (y (ccl:point-v where)))
;;; BM          (unless (and (< -2 (- old-x x) 2) (< -2 (- old-y y) 2))
          (unless (and (= x old-x) (= y old-y))
            (setf old-x x old-y y)
            (inter::Do-Motion-Notify opal-window x y NIL))
;;; BM 24/9/96 jitter elimination is very well, but true should be returned in 
;;; any case, otherwise it will not be eliminated from the event queue.
          T)))))

;; The view-[de]activate-event-handlers maintain the currently active Garnet
;; window in *active-drawable* and toggle interest in mouse-move events via
;; the ccl:*eventhook* variable.
;;
(defun process-activate-event (event-window)
  (let ((opal-window (MAC-window-from-drawable *root-window* event-window)))
    (setf gem::*active-drawable* event-window)
    (setf ccl:*eventhook* (if (and opal-window (g-value opal-window :interested?))
                              'mac-mouse-move-event-handler))))

(defmethod ccl:view-activate-event-handler ((event-window gem::MAC-DRAWABLE))
  (process-activate-event event-window)
  (call-next-method))

(defmethod ccl:view-activate-event-handler ((event-window gem::MAC-SUBDRAWABLE))
  (process-activate-event event-window)
  (call-next-method))

(defmethod ccl:view-deactivate-event-handler
    ((event-window gem::MAC-DRAWABLE))
  (when (eq event-window gem::*active-drawable*)
    (setf gem::*active-drawable* NIL)
    (setf ccl:*eventhook* NIL))
  (call-next-method))

(defmethod ccl:view-deactivate-event-handler
    ((event-window gem::MAC-SUBDRAWABLE))
  (when (eq event-window gem::*active-drawable*)
    (setf gem::*active-drawable* NIL)
    (setf ccl:*eventhook* NIL))
  (call-next-method))
                  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The state parameter that is passed around in the mouse event handler functions
;; for the Mac corresponds to modifier keys like "control", and also whether the
;; event is a double click.  This function sets a bit in the state when it
;; determines that we are processing the second click of a double click.
;;
(defun MAC-Check-Double-Press (root-window state code time)
  (declare (ignore root-window))
  (if inter::*double-click-time*    ; just check if non-nil
      (let ((what (ccl:rref ccl:*current-event* :EventRecord.what)))
	;; Don't count double-click if it is because of key repeat (return NIL)
	(unless (eq what 5)
	  ;; else do check
	  (let ((newcode (if (and (eq state *last-state*)
				  (eq code *last-code*)
				  (<= (- time *last-time*) (#_getdbltime)))
			     (+ code inter::*double-offset*) ;; is double click
			     code)))  ;; else not double click
	    ;; set up for next time
	    (setf *last-state* state)
	    (setf *last-code* code)
	    (setf *last-time* time)
	    ;;(format t "code = ~S   newcode = ~b~%" code newcode)
	    newcode)))
      ;; else not interested in double click
      code))


(defun MAC-discard-mouse-moved-events (root-window)
  (declare (ignore root-window)))

(defun MAC-set-interest-in-moved (window interestedp)

  ;; This macroexpansion of
  ;;   (inter::if-debug :mouse
  ;;       (format t "interested in mouse moved now ~s~%" interestedp))
  ;; is required because if-debug is a macro which hasn't been defined yet.
  (IF (AND INTERACTORS::*INT-DEBUG*
           (INTERACTORS::TRACE-TEST :MOUSE))
    (PROGN (LET ((*PRINT-PRETTY* NIL))
             (FORMAT T "interested in mouse moved now ~s~%" INTERESTEDP))))

  (let ((drawable (get-value window :drawable)))
    (s-value window :interested? interestedp)
    (if (and drawable (eq drawable gem::*active-drawable*))
        (setf ccl:*eventhook* (if interestedp
                                  'mac-mouse-move-event-handler)))))

  
(defun MAC-translate-mouse-character (root-window button-code modifier-bits
                                    event-key)
  (declare (ignore root-window))
  (case event-key
    (:button-release
     (aref inter::*mouse-up-translations*  button-code
           (inter::modifier-index modifier-bits)))
    (:button-press
     (aref inter::*mouse-down-translations*  button-code
           (inter::modifier-index modifier-bits)))
    ))
;;; RGA this hack is meant to get garnet working with the info products three button mouse. 
;;; It sends a frimsy key on the down press (which can be translated with quickkeys), 
;;; but a generic key-up on the upstroke.  This function will map any key-up event to the
;;; last mousedown code.
(let ((last-butpress nil))
(defun key-code-to-button-code (key-code down)
  (if (eq inter::*middledown-key* key-code)
      (if down (setq last-butpress inter::*middle-button*)
        inter::*middle-button*)
    (if (eq inter::*rightdown-key* key-code)
        (if down (setq last-butpress inter::*right-button*)
          inter::*right-button*)
      (if (eq inter::*leftdown-key* key-code)
          (if down (setq last-butpress inter::*left-button*)
              inter::*left-button*)
        ;; Dropped through to here because it did not match special key code.
        (if down (setq last-butpress nil)
            (prog1 last-butpress
              (setq last-butpress nil)))
        )))))
#|
;;; Old version
(defun key-code-to-button-code (key-code)
  (if (eq inter::*middledown-key* key-code)
      inter::*middle-button*
      (if (eq inter::*rightdown-key* key-code)
          inter::*right-button*
          (if (eq inter::*leftdown-key* key-code)
              inter::*left-button*))))
|#


;;; The code parameter is the message field of ccl:*current-event*, documented
;;; in Inside Macintosh I, p. 252.  The "key-code" is the high-order word,
;;; which corresponds to the physical key on the keyboard.
;;;
;;; The key-code is important in distinguishing which key generated a
;;; character.  For example, ctrl-a and the Home key both generate the
;;; character-code #\Home, so you wouldn't be able to catch the CONTROL key
;;; during the ctrl-a event if you just looked at the character-code.
;;; You have to look at the key-code, too.
;;;
;;; Mapping of input keystrokes to character/symbol generated by this function:
;;;
;;;     main key       control?     shift?      RESULT
;;;     --------       --------     ------      ------
;;;    alphabetic        NIL         NIL         #\a
;;;    alphabetic        NIL          T          #\A
;;;    alphabetic         T          NIL      :control-\a
;;;    alphabetic         T           T       :control-A
;;;
;;;    numeric,special
;;;    such as [         NIL         NIL         #\[
;;;        [             NIL          T          #\{       <-- mapped by MCL
;;;        [              T         T/NIL     :control-[   <-- shift ignored
;;;
;;; The treatment of the Meta (option) key is identical to Control.
;;; With numeric and special characters, we ignore the shift key in the
;;; presence of the Control and Meta keys.  This is because the Mac does
;;; not return the expected character from a shift-modified special key.
;;; For example, you would expect shift-control-[ to map to control-{,
;;; but the Mac says it is control-esc.  There is no mapping that we can
;;; impose manually, like the arithmetic adjustment of #x20 that we perform
;;; on alphabetic characters (maybe you could build the required hash table,
;;; but this would change with different key-caps settings and different
;;; keyboards).


(defun MAC-translate-character (window x y state code time)
  (let* ((character-code (logand code #xFF))  ;; low-order word
         (key-code (ash code -8))             ;; high-order word
;;RGA         (button-code (key-code-to-button-code key-code)))
         (button-code (key-code-to-button-code key-code t)))
    (if button-code
        (progn
          (inter::Do-Button-Press window x y state button-code time :BUTTON-PRESS)
          ;(format t "---->~X ~X ~S~%" key-code character-code button-code)
          NIL)

        (let ((char (gethash key-code inter::*keysym-translations*)))
          ;; Raise to upper case if necessary
          (when (characterp char)    ;; Unless it's a function key like :F1
            (let ((mapped-code (char-code char)))
              ;; If alphabetic...
              (if (<= #x61 mapped-code #x7A)
                (if (or (ccl:shift-key-p) (ccl:caps-lock-key-p))
                  (setf char (code-char (- (char-code char) #x20))))
                ;; Not alphabetic, so use Mac-generated character code.
                (unless (or (ccl:control-key-p) (ccl:option-key-p))
                  (setf char (code-char character-code))))))

          ;; BITS is ultimately an index into the inter::*prefixes* array that
          ;; corresponds to the set of modifier prefixes.
          (let ((bits (logand #b1100 state)))  ;; Throw away shifts
            (setf char (inter::base-char-to-character char bits)))
          
          ;(format t "---->~X ~X ~S~%" key-code character-code char)
          char))))
