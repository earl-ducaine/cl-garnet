
(in-package "OPAL")

(declaim (inline center))
(defun center (gob)
  (values (center-x gob) (center-y gob)))

(declaim (inline bottom))
(defun bottom (gob)
  (when gob (1- (+ (g-value-fixnum gob :top)
		   (g-value-fixnum gob :height)))))

(declaim (inline right))
(defun right (gob)
  (when gob (1- (+ (g-value-fixnum gob :left)
		   (g-value-fixnum gob :width)))))

(declaim (inline unchecked-gv-bottom))
(defun unchecked-gv-bottom (gob)
  (1- (+ (gv-fixnum gob :top) (gv-fixnum gob :height))))

(declaim (inline gv-bottom))
(defun gv-bottom (gob)
  (if gob (unchecked-gv-bottom gob)
      (kr::broken-link-throw nil :top)))

(define-method :initialize view-object (gob)
  (let ((temp-info (make-update-info)))
    (setf (update-info-bits temp-info) 0)
    (setf (update-info-old-bbox temp-info) (make-bbox))
    (s-value gob :update-info temp-info)))

(define-method :initialize graphical-object (gob)
  (call-prototype-method gob)
  ;; This is not an aggregate!  Used by update algorithm for efficiency
  (setf (update-info-aggregate-p (g-local-value gob :update-info)) NIL))
