
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

(declaim (inline unchecked-gv-right))
(defun unchecked-gv-right (gob)
  (1- (+ (gv-fixnum gob :left) (gv-fixnum gob :width))))

(declaim (inline gv-right))
(defun gv-right (gob)
  (if gob (unchecked-gv-right gob)
          (kr::broken-link-throw nil :left)))

(declaim (inline unchecked-gv-center-x))
(defun unchecked-gv-center-x (gob)
  (+ (gv-fixnum gob :left) (truncate (gv-fixnum gob :width) 2)))

(declaim (inline gv-center-x))
(defun gv-center-x (gob)
  (if gob (unchecked-gv-center-x gob)
      (kr::broken-link-throw nil :left)))


(declaim (inline unchecked-gv-center-y))
(defun unchecked-gv-center-y (gob)
  (+ (gv-fixnum gob :top) (truncate (gv-fixnum gob :height) 2)))

(declaim (inline gv-center-y))
(defun gv-center-y (gob)
  (if gob (unchecked-gv-center-y gob)
      (kr::broken-link-throw nil :top)))

;;; For formulas that want to set an object's center, right or bottom.

;; Gives the value for :left such that (gv-right :self) equals
;; (gv gob :left)

(declaim (inline unchecked-gv-right-is-left-of))
(defun unchecked-gv-right-is-left-of (gob)
  (- (gv-fixnum gob :left) (gvl-fixnum :width)))

(declaim (inline gv-right-is-left-of))
(defun gv-right-is-left-of (gob)
  (if gob (unchecked-gv-right-is-left-of gob)
          (kr::broken-link-throw nil :left)))

(declaim (inline unchecked-gv-bottom-is-top-of))
(defun unchecked-gv-bottom-is-top-of (gob)
  (- (gv-fixnum gob :top) (gvl-fixnum :height)))

;; Gives the value for :top such that (gv-bottom :self) equals
;; (gv gob :top)
;; (declaim (inline gv-bottom-is-top-of))
;; (defun gv-bottom-is-top-of (gob)
;;   (if gob (unchecked-gv-bottom-is-top-of gob)
;;       (kr::broken-link-throw nil :top)))

;; Gives the value for :left such that (gv-center-x :self) equals
;; (gv-center-x gob)
;; (defun gv-center-x-is-center-of (gob)
;;   (if gob
;;       (if (and (is-a-p gob WINDOW)
;; 	       (or (eq (gvl :window) gob) (eq (gvl :parent) gob)))
;; 	  ;; If I am trying to center myself within a window, and I am going
;; 	  ;; to be drawn w.r.t the window's coordinate system (i.e., I am an
;; 	  ;; object or child in gob), then I want to ignore the window's :left
;; 	  (truncate (- (gv-fixnum gob :width) (gvl-fixnum :width)) 2)
;; 	  (- (the fixnum (gv-center-x gob)) (truncate (gvl-fixnum :width) 2)))
;;       (kr::broken-link-throw nil :left)))

; Gives the value for :top such that (gv-center-y :self) equals (gv-center-y
; gob)
;; (defun gv-center-y-is-center-of (gob)
;;   (if gob
;;       (if (and (is-a-p gob WINDOW)
;; 	       (or (eq (gvl :window) gob) (eq (gvl :parent) gob)))
;; 	  ;; If I am trying to center myself within a window, and I am going
;; 	  ;; to be drawn w.r.t the window's coordinate system (i.e., I am an
;; 	  ;; object or child in gob), then I want to ignore the window's :top
;; 	  (truncate (- (gv-fixnum gob :height) (gvl-fixnum :height)) 2)
;; 	  (- (the fixnum (unchecked-gv-center-y gob)) (truncate (gvl-fixnum :height) 2)))
;;       (kr::broken-link-throw nil :top)))

;; (defun bounding-box (gob)
;;   (values (g-value-fixnum gob :left) (g-value-fixnum gob :top)
;; 	  (g-value-fixnum gob :width) (g-value-fixnum gob :height)))

(define-method :initialize view-object (gob)
  (let ((temp-info (make-update-info)))
    (setf (update-info-bits temp-info) 0)
    (setf (update-info-old-bbox temp-info) (make-bbox))
    (s-value gob :update-info temp-info)))


(define-method :initialize graphical-object (gob)
  (call-prototype-method gob)
  ;; This is not an aggregate!  Used by update algorithm for efficiency
  (setf (update-info-aggregate-p (g-local-value gob :update-info)) NIL))
