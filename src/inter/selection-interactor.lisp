;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;          The Garnet User Interface Development Environment.       ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    ;;

;;; $Id$
;;
;; This file contains the mouse interactor to select portions of a
;; multi-font text object with the mouse.
;;



(in-package "INTERACTORS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(SELECTION-INTERACTOR)))


;; Timer handler
;;
(defun Selection-Timer-Handler (inter)
  (let* ((dir (g-value inter :direction-to-scroll))
	 (obj (g-value inter :obj-being-changed))
	 (line NIL) (char NIL))
    (multiple-value-setq (line char) (opal:get-cursor-line-char-position obj))
    (case dir
      ((UP)   (opal:set-cursor-to-line-char-position obj (1- line) char))
      ((DOWN) (opal:set-cursor-to-line-char-position obj (1+ line) char)))
    (kr-send obj :auto-scroll obj)
    (curs-move inter obj)))		; curs-move defined in multifont-textinter.lisp


;; helper Procedures used in word-mode
;;
(defun prev-space (text-obj)
  (let ((frag (g-value text-obj :cursor-frag))
	(frag-pos (g-value text-obj :cursor-frag-pos))
	line-pos char-pos)
    (multiple-value-setq (line-pos char-pos)
      (opal:get-cursor-line-char-position text-obj))
    (do ((char (and frag (not (opal::frag-object-p frag))
		    (< frag-pos (opal::frag-length frag))
		    (schar (opal::frag-string frag) frag-pos))
	       (and frag (not (opal::frag-object-p frag))
		    (schar (opal::frag-string frag) frag-pos))))
	((or (null frag) (eq char #\space)) (values line-pos (1+ char-pos)))
      (decf frag-pos)
      (decf char-pos)
      (when (< frag-pos 0)
	(setf frag (opal::frag-prev frag))
	(do () ((or (null frag) (> (opal::frag-length frag) 0)))
	  (setf frag (opal::frag-prev frag)))
	(when frag
	  (setf frag-pos (1- (opal::frag-length frag))))))))

(defun next-space (text-obj)
  (let ((frag (g-value text-obj :cursor-frag))
	(frag-pos (g-value text-obj :cursor-frag-pos))
	line-pos char-pos)
    (when (= frag-pos (opal::frag-length frag))
      (setf frag (opal::frag-next frag))
      (when frag
	(setf frag-pos 0)))
    (multiple-value-setq (line-pos char-pos)
      (opal:get-cursor-line-char-position text-obj))
    (do ((char (and frag (not (opal::frag-object-p frag))
		    (schar (opal::frag-string frag) frag-pos))
	       (and frag (not (opal::frag-object-p frag))
		    (schar (opal::frag-string frag) frag-pos))))
	((or (null frag) (eq char #\space)) (values line-pos char-pos))
      (incf frag-pos)
      (if (>= frag-pos (opal::frag-length frag))
	  (progn
	    (setf frag (opal::frag-next frag))
	    (when frag
	      (setf frag-pos 0)
	      (incf char-pos)))
	  (incf char-pos)))))



;;; Default Procedures to go into the slots
;;

(declaim (special Selection-Interactor))

(defun Selection-Interactor-Initialize (new-selection-schema)
  (if-debug new-selection-schema (format T "Selection initialize ~s~%"
					 new-selection-schema))
  (Check-Interactor-Type new-selection-schema inter:Selection-Interactor)
  (Check-Required-Slots new-selection-schema)
  (Set-Up-Defaults new-selection-schema))


(defun Selection-Int-Start-Action (an-interactor object mouse-event points)
  (if-debug an-interactor (format T "Selection int-start first points=~s~%"
				  points))
  (let ((focus-inter (g-value an-interactor :focus-interactor))
	(x (car points))
	(y (cadr points)))
    (when focus-inter
      (unless (eq object (g-value focus-inter :obj-to-change))
	(inter:Set-focus focus-inter object)
	(curs-move an-interactor object))) ; from multifont-textinter.lisp
    (when mouse-event
      (case mouse-event
	(:start-selection
	 (opal:toggle-selection object nil)
	 (opal:set-cursor-to-x-y-position object x y)
	 (opal:toggle-selection object T)
	 (curs-move an-interactor object)
	 (s-value an-interactor :drag-mode 0))
	(:start-word-selection
	 (opal:toggle-selection object nil)
	 (case (g-value an-interactor :drag-mode)
	   (0 (multiple-value-call
		  'opal:set-selection-to-line-char-position
		object (prev-space object))
	      (multiple-value-call
		  'opal:set-cursor-to-line-char-position
		object (next-space object))
	      (s-value an-interactor :drag-mode 1))
	   (1 (opal:set-selection-to-x-y-position object 0 y)
	      (opal:set-cursor-to-x-y-position
	       object (g-value object :width) y)
	      (s-value an-interactor :drag-mode 2))
	   (2 (opal:set-selection-to-line-char-position object 0 0)
	      (opal:go-to-end-of-text object)))
	 (opal:toggle-selection object T)
	 (curs-move an-interactor object))
	(:start-selection-continue
	 (opal:toggle-selection object T)
	 (opal:set-cursor-to-x-y-position object x y)
	 (opal:toggle-selection object T)
	 (curs-move an-interactor object)
	 (s-value an-interactor :drag-mode 0))
	(:start-word-selection-continue
	 (opal:toggle-selection object T)
	 (case (g-value an-interactor :drag-mode)
	   (0 (if (opal::higher-cursor (g-value object :cursor-line)
				       (g-value object :cursor-position)
				       (g-value object :select-line)
				       (g-value object :select-position))
		  (multiple-value-call 'opal:set-cursor-to-line-char-position
		    object (prev-space object))
		  (multiple-value-call 'opal:set-cursor-to-line-char-position
		    object (next-space object)))
	      (s-value an-interactor :drag-mode 1))
	   (1 (if (opal::higher-cursor (g-value object :cursor-line)
				       (g-value object :cursor-position)
				       (g-value object :select-line)
				       (g-value object :select-position))
		  (opal:set-cursor-to-x-y-position object 0 y)
		  (opal:set-cursor-to-x-y-position
		   object (g-value object :width) y))
	      (s-value an-interactor :drag-mode 2))
	   (2 (opal:set-selection-to-line-char-position object 0 0)
	      (opal:go-to-end-of-text object)))
	 (opal:toggle-selection object T)
	 (curs-move an-interactor object))
	(T nil)))))


(defun Selection-Int-Running-Action (an-interactor object points)
   (if-debug an-interactor (format T "Selection int-running, points=~s~%"
				   points))
   (case (g-value an-interactor :drag-mode)
     (0 (opal:set-cursor-to-x-y-position object (car points) (cadr points)))
     (1 (opal:set-cursor-to-x-y-position object (car points) (cadr points))
	(if (opal::higher-cursor (g-value object :cursor-line)
				 (g-value object :cursor-position)
				 (g-value object :select-line)
				 (g-value object :select-position))
	    (multiple-value-call 'opal:set-cursor-to-line-char-position
	      object (prev-space object))
	    (multiple-value-call 'opal:set-cursor-to-line-char-position
	      object (next-space object))))
     (2 (if (opal::higher-cursor (g-value object :cursor-line)
				 (g-value object :cursor-position)
				 (g-value object :select-line)
				 (g-value object :select-position))
	    (opal:set-cursor-to-x-y-position object 0 (cadr points))
	    (opal:set-cursor-to-x-y-position object
			   (g-value object :width) (cadr points)))))
   (kr-send object :auto-scroll object)
   (curs-move an-interactor object))

(defun Selection-Int-Outside-Action (an-interactor outside-control object)
;;;   #.(unless kr::*debug-switch* '(declare (ignore an-interactor)))
  (declare (ignore outside-control object))
  (if-debug an-interactor (format T "Selection int-outside~%")))


(defun Selection-Int-Back-Inside-Action (an-interactor outside-control
                                         object points)
  (declare (ignore outside-control))
  (if-debug an-interactor (format T "Selection int-back-in, new points=~s~%"
				  points))
  (opal:set-cursor-to-x-y-position object (car points) (cadr points))
  (curs-move an-interactor object))


(defun Selection-Int-Stop-Action (an-interactor object points)
  (if-debug an-interactor (format T "Selection int-stop, final-points=~s~%"
				  points))
  ;;turn off feedback
;;;  (when points
;;;   (opal:set-cursor-to-x-y-position object (car points) (cadr points)))
  (kill-timer-process an-interactor)
  (s-value an-interactor :direction-to-scroll NIL)
  (kr-send object :auto-scroll object)
  (kr-send an-interactor :final-function an-interactor object points)
  (curs-move an-interactor object))


(defun Selection-Int-Abort-Action (an-interactor object)
  ;;   #.(unless kr::*debug-switch* '(declare (ignore an-interactor)))
  (if-debug an-interactor (format T "Selection int-abort~%"))
  (kill-timer-process an-interactor)
  (s-value an-interactor :direction-to-scroll NIL)
  (opal:toggle-selection object nil)
  (curs-move an-interactor object))



;;; Go procedure utilities
;;

;; if continuous: (remove from start level, add to stop and abort
;; 		    level, change state to running)
;; save object over, call start procedure.
(defun Selection-Do-Start (an-interactor new-obj-over event)
  (if-debug an-interactor
	    (format T "Selection starting over ~s~%" new-obj-over))
  (let* ((obj (or (g-value an-interactor :obj-to-change) new-obj-over))
	 (x (event-x event))
	 (y (event-y event))
	 (points (list x y)))
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)
    (s-value an-interactor :saved-window (event-window event))
    (s-value an-interactor :obj-being-changed obj)
    (s-value an-interactor :direction-to-scroll NIL)
    (if (g-value an-interactor :continuous) ; then will go to running state
	(progn
	  (Fix-Running-Where an-interactor obj)	 ; not likely needed
	  (when (g-value an-interactor :outside) ; needed if stop while outside
	    (set-obj-list4-slot-no-db an-interactor
				      :saved-last-points points))
	  (GoToRunningState an-interactor T)
	  (kr-send an-interactor :start-action an-interactor obj
		   (inter::Translate-key (event-char event) an-interactor)
		   points))
	;; else call stop-action
	(progn
	  (kr-send an-interactor :stop-action an-interactor obj points)
	  (GoToStartState an-interactor NIL)))))


;; filtering based on :last is handled by the :outside-action procedure
;;  (unless (eq :last (g-value an-interactor :outside))
;;    (s-value an-interactor :remembered-last-object NIL)))
(defun Selection-Do-Outside (an-interactor)
  (if-debug an-interactor (format T "Selection outside~%"))
  (s-value an-interactor :current-state :outside)
  (kr-send an-interactor :outside-action an-interactor
	   (g-value an-interactor :outside)
	   (g-value an-interactor :obj-being-changed)))


(defun Selection-Do-Back-Inside (an-interactor obj event)
  ;;   #.(unless kr::*debug-switch* '(declare (ignore obj)))
  (if-debug an-interactor (format T "Selection back-inside over ~s~%" obj))
  (let* ((x (event-x event))
	 (y (event-y event))
	 (points (list x y)))
    (unless (eq (event-window event) (g-value an-interactor :saved-window))
      (multiple-value-setq (x y)
	(opal:convert-coordinates (event-window event)
				  x y (g-value an-interactor :saved-window))))
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)
    (s-value an-interactor :current-state :running)
    (when (g-value an-interactor :outside) ;needed if stop while outside
      (set-obj-list4-slot-no-db an-interactor :saved-last-points points))
    (kr-send an-interactor :back-inside-action an-interactor
	     (g-value an-interactor :outside)
	     (g-value an-interactor :obj-being-changed) (list x y))))


(defun Selection-Do-Running (an-interactor obj event)
  ;;   #.(unless kr::*debug-switch* '(declare (ignore obj)))
  (if-debug an-interactor (format T "Selection running over ~s~%" obj))
  (let* ((x (event-x event))
	 (y (event-y event))
	 (points (list x y)))
    (unless (eq (event-window event) (g-value an-interactor :saved-window))
      (multiple-value-setq (x y)
	(opal:convert-coordinates (event-window event)
				  x y (g-value an-interactor :saved-window))))
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)

    ;; The following stuff is for the timer.
    (let ((sw (g-value an-interactor :saved-window :scroll-win-gadget)))
      (when sw
	(cond ((<= y (- (g-value sw :y-offset)))
	       (s-value an-interactor :direction-to-scroll 'UP))
	      ((>= y (- (g-value sw :clip-window :height)
			(g-value sw :y-offset)))
	       (s-value an-interactor :direction-to-scroll 'DOWN))
	      (T (s-value an-interactor :direction-to-scroll NIL)))

	(if (g-value an-interactor :direction-to-scroll)
	    (launch-timer-process an-interactor
				  (g-value an-interactor :timer-repeat-wait)
				  NIL)
	    (kill-timer-process an-interactor))))
    ;; The above stuff is for the timer.

    (when (g-value an-interactor :outside) ;needed if stop while outside
      (set-obj-list4-slot-no-db an-interactor :saved-last-points points))
    (kr-send an-interactor :running-action an-interactor
	     (g-value an-interactor :obj-being-changed) (list x y))))


;;; Move-Grow-do-stop-helper is defined in movegrowinter.lisp.

(defun Selection-Do-Stop (an-interactor obj event)
  ;;   #.(unless kr::*debug-switch* '(declare (ignore obj)))
  (if-debug an-interactor (format T "Selection stop over ~s~%" obj))
  (let ((x (event-x event))
	(y (event-y event)))
    (unless (eq (event-window event) (g-value an-interactor :saved-window))
      (multiple-value-setq (x y)
	(opal:convert-coordinates (event-window event)
				  x y (g-value an-interactor :saved-window))))
    (s-value an-interactor :x x)
    (s-value an-interactor :y y)
    (Move-grow-do-stop-helper an-interactor (list x y))))

;; This is used if explicitly call Stop-Interactor.  It uses the last point.
(defun Selection-Explicit-Stop (an-interactor)
  (if-debug an-interactor (format T "Selection explicit stop~%"))
  (let ((x (g-value an-interactor :x))
	(y (g-value an-interactor :y)))
    (Move-grow-do-stop-helper an-interactor (list x y))))

(defun Selection-Do-Abort (an-interactor become-inactive event)
  (declare (ignore event become-inactive))
  (if-debug an-interactor (format T "Two-Point aborting~%"))
  (GoToStartState an-interactor T)
  (kr-send an-interactor :Abort-Action an-interactor
	   (g-value an-interactor :obj-being-changed)))


;; check to see if need to stop or abort based on whether :outside = :last
(defun Selection-Do-Outside-Stop (an-interactor event)
  (if-debug an-interactor (format T "Selection stop outside~%"))
  (if (and (eq :last (g-value an-interactor :outside))
	   (car (g-value an-interactor :saved-last-points)))
      (Move-grow-do-stop-helper an-interactor
				(g-value an-interactor :saved-last-points))
      (selection-do-abort an-interactor NIL event)))

;;; Selection schema
(Create-Schema 'inter:Selection-Interactor
   (:is-a inter:interactor)
   (:match-parens-p NIL)
   (:match-obj NIL)
   (:after-cursor-moves-func (o-formula (when (gvl :match-parens-p)
					  #'check-parens)))
   (:name :First-Selection-interactor)
   (:start-action 'Selection-Int-Start-Action)
   (:running-action 'Selection-Int-Running-Action)
   (:stop-action 'Selection-Int-Stop-Action)
   (:abort-action 'Selection-Int-Abort-Action)
   (:outside-action 'Selection-Int-Outside-Action)
   (:back-inside-action 'Selection-Int-Back-Inside-Action)
   (:timer-repeat-wait 0.01)
   (:timer-repeat-p T)
   (:timer-handler 'Selection-Timer-Handler)
   (:start-event '(:any-leftdown :rightdown :double-rightdown :double-leftdown
		   :shift-double-leftdown :meta-double-leftdown
		   :control-double-leftdown :control-shift-double-leftdown
		   :meta-shift-double-leftdown))
   (:stop-event '(:any-leftup :rightup))
   (:running-where T)
   (:saved-last-points NIL) ; used if stop and outside and
                            ; outside control is :last
   (:drag-mode 0)           ; used to signal char, word, line, or all mode
   (:focus-interactor NIL)  ; identifies keyboard interactor
   (:Go 'General-Go)  ; proc executed when events happen
   (:Do-Start 'Selection-Do-Start)     ; these are
   (:Do-Running 'Selection-Do-Running) ;   called by GO
   (:Do-Explicit-Stop 'Selection-Explicit-Stop) ;for stop-inter
   (:Do-Stop 'Selection-Do-Stop)	;   to do
   (:Do-Abort 'Selection-Do-Abort)     ;   the real work.
   (:Do-Outside 'Selection-Do-Outside) ;   They call the
   (:Do-Back-Inside 'Selection-Do-Back-Inside)  ; appropriate
   (:Do-Outside-Stop 'Selection-Do-Outside-Stop); -action procedures
   (:initialize 'Selection-Interactor-Initialize))


;; Map for button press types
;; Initializes the hash table of an-interactor with the standard
;; translations.  If there is no table in an-interactor, creates one.
;; Otherwise, removes any translations that are there before adding
;; the new ones.
(defun Set-Default-Button-Translations (an-interactor)
  (let ((ht (get-local-value an-interactor :key-translation-table)))
    (if (not (hash-table-p ht))
	(s-value an-interactor :key-translation-table
		 (setq ht (make-hash-table)))
	;; else re-initialize ht
	(clrhash ht))
    (bind-key-internal :leftdown :start-selection ht)
    (bind-key-internal :double-leftdown :start-word-selection ht)
    (bind-key-internal :shift-leftdown :start-selection-continue ht)
    (bind-key-internal :shift-double-leftdown
		       :start-word-selection-continue ht)
    (bind-key-internal :control-leftdown :start-selection ht)
    (bind-key-internal :control-double-leftdown :start-word-selection ht)
    (bind-key-internal :control-shift-leftdown :start-selection-continue ht)
    (bind-key-internal :control-shift-double-leftdown
		       :start-word-selection-continue ht)
    (bind-key-internal :meta-leftdown :start-selection ht)
    (bind-key-internal :meta-double-leftdown :start-word-selection ht)
    (bind-key-internal :meta-shift-leftdown :start-selection-continue ht)
    (bind-key-internal :meta-shift-double-leftdown
		       :start-word-selection-cont ht)
    (bind-key-internal :rightdown :start-selection-continue ht)
    (bind-key-internal :double-rightdown :start-word-selection-continue ht)))

(Set-Default-Button-Translations inter:selection-interactor)
