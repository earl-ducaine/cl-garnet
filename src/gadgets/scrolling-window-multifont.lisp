;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;  Scrolling Window Parts
;;;      see the file Scrolling-window for an explanation


(in-package "GARNET-GADGETS")

;; This method takes in a multifont text object and a scrolling-window and
;; scrolls so that the cursor is somewhat centered vertically in
;; the window.
;;
;; When either the cursor bottom or the cursor top is out of bounds,
;; this will call show-box with the top being the top of the cursor
;; minus 1/2 the window-height, and the bottom being the bottom minus
;; 1/2 the window height.  This will somewhat center it vertically.
;;

(define-method :auto-scroll opal:multifont-text (text-obj)
  (when (g-value text-obj :auto-scroll-p)
    (let ((scr-win (g-value text-obj :scrolling-window)))
      (if (Is-A-Scrolling-Window scr-win)
	  (let* ((win-height (g-value scr-win :clip-window :height))
		 (win-top (- (g-value scr-win :inner-window :top)))
		 (win-bottom (- win-height (g-value scr-win :y-offset)))
		 (cursor-obj (g-value text-obj :cursor))
		 (cur-top (g-value cursor-obj :top))
		 (cur-left (g-value cursor-obj :left))
		 (cur-height (g-value cursor-obj :height))
		 (cur-bottom (+ cur-top cur-height))
		 (cur-right (+ cur-left (g-value cursor-obj :width))))
	    
	    (cond
	      ((OR (> cur-bottom (- (+ win-height cur-height)
				    (g-value scr-win :y-offset)))
		   (<= cur-top (- (+ cur-bottom
				     (g-value scr-win :y-offset)))))
	       (show-box scr-win cur-left
			 (if (> 0 (decf cur-top (ceiling win-height 2)))
			     0                     ;; Top of scr-win
			     cur-top)
			 cur-right (incf cur-bottom (ceiling win-height 2))))
	      
	      ((> cur-bottom (- win-height (g-value scr-win :y-offset)))
	       (show-box scr-win cur-left (incf win-top cur-height)
			 cur-right cur-bottom))
	      
	      ((< cur-top (- (g-value scr-win :y-offset)))
	       (show-box scr-win cur-left cur-top cur-right
			 (decf win-bottom cur-height)))))
	  
	  (warn "Tried to scroll ~S for ~S,
but it is not a scrolling window." scr-win text-obj)))))

