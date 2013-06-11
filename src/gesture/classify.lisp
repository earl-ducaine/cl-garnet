;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; classify.lisp
;;;
;;; Routines for training gesture classifiers and classifying gestures.   
;;; (based on Dean Rubine's C implementation)
;;;
;;; Designed and implemented by James A. Landay

#|
============================================================
Change log:
    07/26/93 James Landay - define gest-class here
    03/24/92 James Landay - added a test before divide by zero.
    02/26/92 James Landay - made gest-classify () return NIL values
                            if the passed classifier is NIL.
    01/31/92 James Landay - changed classify () to gest-classify (), and
                            new-classifier () to gest-new-classifier ()
    01/22/92 James Landay - made classify () also return the attributes
                            vector.
    12/25/91 James Landay - made classify () take an array of points
                            rather than a list.
    12/21/91 James Landay - started
============================================================
|#

(in-package "INTERACTORS")


;; structure definitions
;; structure to store a gesture class.
(defstruct g-class
    name                ;; classifier name
    average             ;; average of example feature vectors
    sumcov              ;; covariance matrix
    num-examples        ;; number of examples
)

;; structure to store a single gesture class's examples
;; used by Agate (define here so that will be defined in interactors package)
(defstruct gest-class
  (name NIL)                    ;; name of gesture class
  (examples NIL)                ;; list of examples of this gesture class
)                               ;;    (each example is an array)

;; structure to store a classifier.
(defstruct classifier
    classes             ;; the actual classes for this classifier
    num-classes         ;; number of classes
    cnst                ;; constant term of discrimination function
    weights             ;; coefficient weights
    invavgcov           ;; inverse covariance matrix
)


;; global variables definitions
(defvar last-class nil)     ;; last class looked up in class-name-lookup
(defvar last-classifier nil);; last classifier "    "     " 


;; gest-classify returns the class from the given classifier that most 
;; closely matches the features of the given gesture. If the gesture 
;; is not recognized, it returns NIL. In addition it returns the attributes 
;; vector of the gesture. It also returns the non-ambiguity of the
;; class chosen and the distance to the mean of the chosen class
;; as additional (multiple) values if those parameters are non-NIL.
;; If the class does not meet the cutoff values given by the 
;; non_ambig_prob and distance it will return the gesture as
;; unrecognized, i.e. NIL.
;;  
;; Parameters:
;;    g-points           - array of points [x1 y1 x2 y2 ...] making up 
;;                         gesture. 
;;    classifier         - classifier to match with 
;;    min-non-ambig-prob - minimum value for non-ambiguity prob. of class 
;;    max-dist           - maximum for mean distance from chosen class
;;
(defun gest-classify (g-points classifier 
                      &optional min-non-ambig-prob max-dist)
    (if (null classifier)
        (return-from gest-classify (values nil nil nil nil))
    )
    (if (null (classifier-weights classifier))
        (error "gest-classify () - classifier has not been trained")
    )

    (let ((max-class (car (classifier-classes classifier)))
          (max-disc -1e20)
          (class-found nil)
          (denom 0)
          (attributes nil)
          (fv nil)
          (nap nil)
          (distance nil)
          (disc (make-array `(,(classifier-num-classes classifier)))))

        ;; calc feature and attributes vectors for pts.
        (multiple-value-setq (fv attributes) (features g-points))

        ;; make sure the feature vector isn't empty
        (if (null fv)
            (return-from gest-classify (values nil nil nil nil))
        )
            
        ;; find the class with the maximum inner prodcut (discriminant)
        (do ((c 0 (1+ c))
             (rest-classes (cdr (classifier-classes classifier)) 
                           (cdr rest-classes))
             (cur-class (car (classifier-classes classifier)) 
                        (car rest-classes)))
            ((>= c (classifier-num-classes classifier))) ;; exit cond.
            (setf (aref disc c) 
                  (+ (aref (classifier-cnst classifier) c)
                     (inner-product 
                         fv 
                         (aref (classifier-weights classifier) c))))
            (when (> (aref disc c) max-disc)
                (setf max-disc (aref disc c))
                (setf max-class cur-class)
            )
        )

        (setf class-found (g-class-name max-class))

        ;; calculate the probability of non-ambiguity
        (when (not (null min-non-ambig-prob))
            (do ((c 0 (1+ c))
                 (d 0))
                ((>= c (classifier-num-classes classifier)))
                ;; quick check to avoid computing negligible term
                (if (> (setf d (- (aref disc c) max-disc)) -7)
                    (incf denom (exp d))
                )
            )
            (if (equalp denom 0)
;                (setf nap 'INFINITY)
                (setf nap 1e20)
                (setf nap (/ 1 denom))
            )
            (when (< nap min-non-ambig-prob)
                (setf class-found nil))
        )

        ;; calculate the probability of non-ambiguity
        ;; calculate distance to mean of chosen class
        (when (not (null max-dist))
            (setf distance (mdistance fv 
                                      (g-class-average max-class) 
                                      (classifier-invavgcov classifier))) 
            (when (> distance max-dist)
                (setf class-found nil))
        )

        ;; return the best match along with an idea of how good it is
        (values class-found attributes nap distance)
    )
)


;; mdistance returns the mahalanobis distance: (v - u)' sigma (v - u)
;; given v, u, and sigma.
;;
;; Parameters:
;;    v     - feature vector
;;    u     - average vector of max-class
;;    sigma - inv. avg. covariance matrix
;;
(defun mdistance (v u sigma)
    (let* ((rows (array-dimension v 0))
           (space (make-array `(,rows))))
        (do ((i 0 (1+ i)))
            ((>= i rows))
            (setf (aref space i) (- (aref v i) (aref u i))) 
        )
        
        (quadratic-form space sigma)
    )
)


;; gest-new-classifier returns a newly allocated and initialized classifier.
;;
;; Parameters:
;;    none
;;
(defun gest-new-classifier ()
    (let ((new-classifier (make-classifier)))
        (setf (classifier-num-classes new-classifier) 0)
        new-classifier             ;; return the new classifier
    )
)
