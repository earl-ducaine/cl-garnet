
(defparameter *composite-events*
  '((:control (:mapped-event ())
     (:sub-events ((:right-button-release :click))))
    (:right-button-press ((:mapped-event :right-button-press)
     (:sub-events ((:right-button-release ((:mapped-event :click)(:sub-events ())))))))))


;; composite event: ((:right-button-press :right-button-press
;;                    (:right-button-release :click)))
;; leaf-event:

(defun leaf-composite-event-p (event composite-events)
  (second (assoc :mapped-event (second (assoc event composite-events)))))

(defun get-sub-events (event composite-events)
  (second (assoc :sub-events (second (assoc event composite-events)))))

(defun get-mapped-event (event composite-events)
  (second (assoc :mapped-event (second (assoc event composite-events)))))

(defun get-mapped-event-iter (event composit-events)
  (let ((sub-events (assoc (first event) sub-composit-events)))
    (cond
      ((and (= (length event) 1)
	    (leaf-composite-event-p sub-events))
       sub-events)
      ((and (> (length event) 1)
	    (not (leaf-composite-event-p sub-events)))
       (get-mapped-event-iter (rest event) sub-events))
      (t '()))))

(defun get-mapped-event (events)
  (get-mapped-event-iter events *composite-events*))
