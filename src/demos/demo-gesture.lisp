;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-GESTURE; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$	


;;; demo-gesture.lisp 
;;;
;;; This is a simple demonstration of using the Garnet gesture interactor.
;;;
;;; Designed and implemented by James A. Landay 


(in-package :DEMO-GESTURE)

;; Load the gesture interactor, unless already loaded 
(defvar DEMO-GESTURE-INIT
    ;; load gesture-loader
    (common-lisp-user::garnet-load (concatenate 'string "gesture:" "gesture-loader")))


;; global variables definitions
(defvar TOP-WIN NIL)
(defvar SHAPE-AGG)
(defvar GESTURE-INTER NIL)

(declaim (special MOVING-CIRCLE MOVING-RECTANGLE TOP-AGG TOP-WIN))

;; create a moving circle and rectangle prototype
(create-instance 'MOVING-CIRCLE opal:circle
    (:box '(0 0 40 40))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
)
(create-instance 'MOVING-RECTANGLE opal:rectangle
    (:box '(0 0 60 35))
    (:left (o-formula (first (gvl :box))))
    (:top (o-formula (second (gvl :box))))
    (:width (o-formula (third (gvl :box))))
    (:height (o-formula (fourth (gvl :box))))
)


;; handle-gesture is called by the gesture interactor after it
;; classifies a gesture. Handle-gesture will perform the appropriate
;; operation in the TOP-WIN and print out the gesture name in
;; the Lisp window.
(defun handle-gesture (inter first-obj-over gesture-name attribs
                       points-array nap dist)
    (declare (ignore inter first-obj-over points-array))

    ;; print a message and quit the handler if we can't recognize it
    (when (null gesture-name)
        (print "unrecognized gesture ...")   
        (terpri)
        (return-from handle-gesture)
    )

    ;; use the bounding box for the size of the new objects 
    (let ((bbox (opal::make-bbox :x1 (inter:gest-attributes-minx attribs)
                                 :y1 (inter:gest-attributes-miny attribs)
                                 :x2 (inter:gest-attributes-maxx attribs)
                                 :y2 (inter:gest-attributes-maxy attribs)
                                 :valid-p T)))
        (format T "~s with probability of ~s and distance of ~s~%~%" 
                gesture-name nap dist)

        (case gesture-name
            (:CIRCLE
                ;; create a new circle with the same radius as the gesture
                (opal:add-components SHAPE-AGG 
                    (create-instance NIL MOVING-CIRCLE
                        (:box (list (opal::bbox-x1 bbox)
                                    (opal::bbox-y1 bbox)
                                    (- (opal::bbox-x2 bbox)
                                       (opal::bbox-x1 bbox))
                                    (- (opal::bbox-x2 bbox)
                                       (opal::bbox-x1 bbox))))))
            )

            (:COPY 
                ;; copy the object that the gesture started in
                ;; and offset the new object from the original
                (let ((to-copy (opal:point-to-component SHAPE-AGG
                                   (inter:gest-attributes-startx attribs)
                                   (inter:gest-attributes-starty attribs))))
                    (when to-copy
                        (opal:add-components SHAPE-AGG
                            (create-instance NIL to-copy 
                                (:box (list 
                                      (+ 10 (first (g-value to-copy :box)))
                                      (+ 10 (second (g-value to-copy :box)))
                                      (third (g-value to-copy :box))
                                      (fourth (g-value to-copy :box))))))
                    )
                )
            )

            (:DELETE 
                ;; find the objects intersecting in the bounding box of the 
                ;; delete gesture and remove them
                (let ((to_delete (opal:components-in-rectangle SHAPE-AGG 
                                     (inter:gest-attributes-miny attribs)
                                     (inter:gest-attributes-minx attribs)
                                     (inter:gest-attributes-maxy attribs)
                                     (inter:gest-attributes-maxx attribs)
                                     :intersect T)))
                    (dolist (cur to_delete)
                        ; only delete object that haven't been deleted
                        (when (kr:schema-p cur)
                            (opal:destroy cur)
                        )
                    )
                )
            )

            (:RECTANGLE
                ;; create a new rectangle with the same length and
                ;; width as the gesture
                (opal:add-components SHAPE-AGG 
                    (create-instance NIL MOVING-RECTANGLE
                        (:box (list (opal::bbox-x1 bbox)
                                    (opal::bbox-y1 bbox)
                                    (- (opal::bbox-x2 bbox)
                                       (opal::bbox-x1 bbox))
                                    (- (opal::bbox-y2 bbox)
                                       (opal::bbox-y1 bbox))))))
            )

            (otherwise 
                (format T "unrecognized gesture ...~%~%")   
            )
        )
        (opal:update TOP-WIN)
    )
)


;; do-go creates the necessary windows and Garnet objects, and 
;; then starts the application.
;;
;; Parmeters:
;;      
(defun do-go (&key dont-enter-main-event-loop double-buffered-p)

    ;; create top-level window
    (create-instance 'TOP-WIN inter:interactor-window
      (:left 750) (:top 80)
      (:width 520) (:height 400)
      (:double-buffered-p double-buffered-p)
      (:title "GARNET Gesture Recognition Demo")
      (:icon-title "Gest-Demo")
      (:background-color opal:motif-gray)
      )

    ;; create the top level aggregate in the window
    (s-value TOP-WIN :aggregate
        (create-instance 'TOP-AGG opal:aggregate
            (:left 0) (:top -2)
            (:width (o-formula (gvl :window :width)))
            (:height (o-formula (gvl :window :height)))))

  ;; If we get clobbered by the window manager, let the demos
  ;; controller know (if it's there).
  (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
    (pushnew
     #'(lambda (win)
	 (declare (ignore win))
	 (common-lisp-user::Garnet-Note-Quitted "DEMO-GESTURE"))
     (g-value top-win :destroy-hooks)))


    ;; create an aggregate to hold the shapes we will create
    (create-instance 'SHAPE-AGG opal:aggregate)
    (opal:add-components TOP-AGG SHAPE-AGG)
    (opal:update TOP-WIN)

    ;; create an interactor to allow us to move the shapes
    (create-instance 'MG-INTER inter:move-grow-interactor
        (:window TOP-WIN)
        (:start-where (list :element-of SHAPE-AGG))
        (:running-where T)
        (:start-event :MIDDLEDOWN)
    )

    ;; create a gesture interactor that will allow us to create,
    ;; delete, and copy shapes.
    (create-instance 'GESTURE-INTER inter:gesture-interactor
        (:window TOP-WIN)
        (:start-where (list :in TOP-WIN)) 
        (:running-where (list :in TOP-WIN))
        (:start-event :LEFTDOWN)
        (:classifier (inter:gest-classifier-read
                         (merge-pathnames "demo-gesture.classifier"
                                          common-lisp-user::Garnet-Gesture-Data-Pathname)))
        (:final-function #'handle-gesture)
        (:min-non-ambig-prob .95)
        (:max-dist-to-mean 60)
    )

    (format t "~%Demo-Gesture:
   Demo Gesture allows the user to create circles and rectangles by giving
   mouse gestures.  To create a circle, draw a circle while holding down the
   left mouse button.  Similarly, a rectangle can be created by drawing an
   `L' shape.  The shapes can be moved by dragging with the middle button pressed.
   The shapes can be deleted by drawing a single-path `X' over the shapes.
   Shapes can be copied by drawing a `C' on a shape with the `C' starting inside
   the shape to be copied.~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
)


;; do-stop destroys the application window and everything beneath it.
;;
;; Parmeters:
;;     none
(defun do-stop ()
    (opal:destroy TOP-WIN)
)
