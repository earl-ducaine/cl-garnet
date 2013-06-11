;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-ARRAY; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change log
;; 05-22-94 Marty Geier - Changed top of main window so its grabble
;; 10-24-91 BAM  made clicking change a single square, made lines between
;;               squares



(in-package :DEMO-ARRAY)

(declaim (special W A FEED-RECT MY-SQUARE))

(defvar the-array)
(defvar *vp*)
(defvar *square* 6)
(defvar *square-size* (1- *square*))
(defvar *window-size* 300)

(defun My-Point-To-Rank (gob x y)
  (declare (ignore gob))
  (values (floor x *square*) (floor y *square*)))


(create-instance 'my-square opal:rectangle
  (:filling-style (o-formula (if (zerop (gvl :item-values))
				opal:black-fill opal:white-fill)))
  (:line-style nil)
  (:left (o-formula (* *square* (gvl :rank1))))
  (:top (o-formula (* *square* (gvl :rank2))))
  (:width *square-size*)
  (:height *square-size*))


(defun WHITEN-RECTANGLE (dum xy)
  (declare (ignore dum))
  (opal:do-in-clip-rect (x y the-array xy)
    (opal:change-item the-array 1 x y)))

(defun REVERSE-RECTANGLE (dum xy)
  (declare (ignore dum))
  (let ((item-array (g-value the-array :item-array)))
    (opal:do-in-clip-rect (x y the-array xy)
      (opal:change-item the-array (- 1 (aref item-array x y)) x y))))

(defun BLACKEN-RECTANGLE (dum xy)
  (declare (ignore dum))
  (opal:do-in-clip-rect (x y the-array xy)
    (opal:change-item the-array 0 x y)))


(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  (create-instance 'w inter:interactor-window
     (:top 100) (:left 200) ; needed too grab title bar
     (:double-buffered-p double-buffered-p)
     (:width *window-size*) (:height *window-size*)
     (:aggregate (create-instance 'a opal:aggregate)))
  (setq *vp* w)

  (setq the-array
    (create-instance nil opal:virtual-aggregate
      (:item-prototype my-square)
      (:point-to-rank #'my-point-to-rank)
      (:item-array (make-array (list (floor *window-size* *square*)
				     (floor *window-size* *square*))
				:element-type 'bit :initial-element 0))))

  (opal:add-component a the-array)

  (opal:add-component a
     (create-instance 'feed-rect opal:rectangle
        (:fast-redraw-p t)
        (:draw-function :xor)
        (:left (formula '(first (gvl :box))))
        (:top (formula '(second (gvl :box))))
        (:width (formula '(third (gvl :box))))
        (:height (formula '(fourth (gvl :box))))
        (:visible NIL)
        (:box '(0 0 0 0))
        (:line-style opal:dashed-line)))


  (create-instance 'WHITER inter:two-point-interactor
     (:start-event :leftdown)
     (:continuous T)
     (:start-where `(:in ,the-array))
     (:window w)
     (:feedback-obj feed-rect)
     (:final-function #'Whiten-rectangle))
  (create-instance 'REVERSER inter:two-point-interactor
     (:start-event :middledown)
     (:continuous T)
     (:start-where `(:in ,the-array))
     (:window w)
     (:feedback-obj feed-rect)
     (:final-function #'Reverse-rectangle))
  (create-instance 'BLACKENER inter:two-point-interactor
     (:start-event :rightdown)
     (:continuous T)
     (:start-where `(:in ,the-array))
     (:window w)
     (:feedback-obj feed-rect)
     (:final-function #'Blacken-rectangle))

  (opal:update w)

  (format t "~%Click and drag with left   button to whiten  bits,~%")
  (format t   "Click and drag with middle button to toggle  bits,~%")
  (format t   "Click and drag with right  button to blacken bits,~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

)


(defun Do-Stop ()
  (opal:destroy w))
