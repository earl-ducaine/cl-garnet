;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; train.lisp
;;;
;;; Routines for training gesture classifiers. 
;;; (uses structures defined in classify.lisp)
;;; (based on Dean Rubine's C implementation)
;;;
;;; Designed and implemented by James A. Landay


(in-package "INTERACTORS")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(gest-add-example
	    gest-classifier-train
	    gest-done-adding 
	    gest-remove-example
	    gest-strip-sumcov)))


;; constant definitions
(defconstant EPS-SM 1.0e-6)     ;; for singular matrix check


;; global variables definitions
(defvar *last-class* nil)     ;; last class looked up in name lookup
(defvar *last-classifier* nil);; last classifier "    "     " 


;; add-class allocates and returns a new class with the given name 
;; in the given classifier.
;;
;; Parameters:
;;    classifier - classifier to add class to 
;;    class-name - class to add 
;;
(defun add-class (classifier class-name)
    (let ((new-class (make-g-class)))
        (setf (g-class-name new-class) class-name)
        (setf (g-class-num-examples new-class) 0)
        (incf (classifier-num-classes classifier) 1)  
        (setf (classifier-classes classifier)
                (append (classifier-classes classifier) (list new-class)))
;;              (cons new-class (classifier-classes classifier)))
        new-class          ;; return the new class
    )
)


;; remove-class removes the given class from the given classifier.
;;
;; Parameters:
;;    classifier - classifier to remove class from 
;;    class-name - class to remove 
;;
(defun remove-class (classifier class-name)
    ;; if this class was the last one looked up, uncache it!
    (when (and *last-class*
               (equal class-name (g-class-name *last-class*))
               (equal classifier *last-classifier*))
        (setf *last-class* NIL)
        (setf *last-classifier* NIL)
    )

    ;; search classifier for it 
    (setf (classifier-classes classifier)
          (remove class-name (classifier-classes classifier)
             :test #'equal :key #'g-class-name)) 
    (decf (classifier-num-classes classifier) 1)  

)


;; gest-add-example adds the given example to the given class in
;; the given classifier. Returns the updated classifier if
;; successful, NIL if failure.
;;
;; Parameters: 
;;    g-points   - array of points [x1 y1 x2 y2 ...] making up example 
;;    class-name - class to add to
;;    classifier - classifier that contains g-class 
;;
(defun gest-add-example (g-points class-name classifier)
    (cond 
        ((null g-points) nil)
        ((null classifier) nil)
        (t (let ((g-class nil)
                 (nfv (make-array `(,NUM-FEATURES))) ;; temporay
                 (fv (make-array `(,NUM-FEATURES)))  ;; feature vector
                 (nm1on 0)      ;; num-examples minus 1 over num-examples
                 (recipn 0))    ;; recipricol of num-examples

              ;; calculate the features for the given points
              (setf fv (features g-points))
              (if (null fv)
                  (return-from gest-add-example nil)
              )

              ;; get g-class structure or add a new one if doesn't exist
              (setf g-class (class-name-lookup classifier class-name))
              (when (null g-class)
                  (setf g-class (add-class classifier class-name))
              )

              ;; initialize the average vector and covariance matrix
              (when (zerop (g-class-num-examples g-class))
                  (setf (g-class-average g-class) 
                        (make-array `(,NUM-FEATURES) 
                                    :initial-element 0))
                  (setf (g-class-sumcov g-class)
                        (make-array `(,NUM-FEATURES 
                                      ,NUM-FEATURES) 
                            :initial-element 0))
              )

              ;; increment the number of examples & calc. constants
              (incf (g-class-num-examples g-class) 1)
              (setf nm1on (/ (1- (g-class-num-examples g-class))
                             (g-class-num-examples g-class)))
              (setf recipn (/ 1 (g-class-num-examples g-class)))
         
              ;; incrementally update covariance matrix
              (do ((i 0 (1+ i)))      ;; loop through each 
                  ((>= i NUM-FEATURES))
                  (setf (aref nfv i) 
                        (- (aref fv i) 
                           (aref (g-class-average g-class) i)))
              )

              ;; only upper triangular part computed 
              (do ((i 0 (1+ i)))      
                  ((>= i NUM-FEATURES))             ;; exit condition
                  (do ((j i (1+ j)))
                     ((>= j NUM-FEATURES))          ;; exit condition
                     (incf (aref (g-class-sumcov g-class) i j) 
                           (* nm1on (aref nfv i) (aref nfv j)))
                  )
              )
           
              ;; incrementally update mean vector 
              (do ((i 0 (1+ i)))      
                  ((>= i NUM-FEATURES))             ;; exit condition
                  (setf (aref (g-class-average g-class) i)
                        (+ (* nm1on (aref (g-class-average g-class) i))
                           (* recipn (aref fv i))))
              )
              classifier)                    ;; return the classifier
        )
    )
)


;; gest-remove-example removes the given example to the given class in
;; the given classifier. Returns the updated classifier if
;; successful, NIL if failure.
;;
;; Parameters: 
;;    g-points   - array of points [x1 y1 x2 y2 ...] making up example 
;;    class-name - class to remove from 
;;    classifier - classifier that contains g-class 
;;
(defun gest-remove-example (g-points class-name classifier)
    (cond 
        ((null g-points) nil)
        ((null classifier) nil)
        (t (let ((g-class nil)
                 (nfv (make-array `(,NUM-FEATURES))) ;; temporay
                 (fv (make-array `(,NUM-FEATURES)))  ;; feature vector
                 (nm1on 0)      ;; num-examples minus 1 over num-examples
                 (recipn 0))    ;; recipricol of num-examples
                
              ;; calculate the features for the given points
              (setf fv (features g-points))
              (if (null fv)
                  (return-from gest-remove-example nil)
              )

              ;; get g-class structure (make sure we have examples already)
              (setf g-class (class-name-lookup classifier class-name))
              (when (or (null g-class) 
                        (zerop (g-class-num-examples g-class)))
                  (return-from gest-remove-example nil)
              )

              ;; decrement the number of examples & calc. constants
              ;; if this is the last example, we can get rid of the class
              (decf (g-class-num-examples g-class) 1)
              (when (< (g-class-num-examples g-class) 1)
                  (remove-class classifier class-name)
                  (return-from gest-remove-example classifier)
              )
              (setf nm1on (/ (1- (g-class-num-examples g-class))
                             (g-class-num-examples g-class)))
              (setf recipn (/ 1 (g-class-num-examples g-class)))
         
              ;; incrementally update covariance matrix
              (do ((i 0 (1+ i)))      ;; loop through each 
                  ((>= i NUM-FEATURES))
                  (setf (aref nfv i) 
                        (- (aref fv i) 
                           (aref (g-class-average g-class) i)))
              )

              ;; only upper triangular part computed 
              (do ((i 0 (1+ i)))      
                  ((>= i NUM-FEATURES))             ;; exit condition
                  (do ((j i (1+ j)))
                     ((>= j NUM-FEATURES))          ;; exit condition
                     (decf (aref (g-class-sumcov g-class) i j) 
                           (* nm1on (aref nfv i) (aref nfv j)))
                  )
              )
           
              ;; incrementally update mean vector 
              (do ((i 0 (1+ i)))      
                  ((>= i NUM-FEATURES))             ;; exit condition
                  (setf (aref (g-class-average g-class) i)
                        (- (* nm1on (aref (g-class-average g-class) i))
                           (* recipn (aref fv i))))
              )
              classifier)                    ;; return the classifier
        )
    )
)




;; class-name-lookup checks to see if the given class exists in 
;; the given classifier.  If it does, it returns the g-class
;; structure for the class.  Otherwise, it returns nil. 
;;
;; Parameters:
;;    classifier - classifier to look class up in 
;;    class-name - class to look up
;;
(defun class-name-lookup (classifier class-name)
    (let ((class-found nil))
        (cond 
            ;; see if this class was the last one looked up
            ((and (not (null *last-class*))
                  (equal class-name (g-class-name *last-class*)) 
                  (equal classifier *last-classifier*)) *last-class*)

            ;; not last one looked up, search all classes
            (t (setf class-found 
                     (some #'(lambda (class) 
                                 (if (equalp class-name 
                                             (g-class-name class)) 
                                      class 
                                      nil
                                 )
                             )
                             (classifier-classes classifier)))
                (when (not (null class-found))
                    (setf *last-class* class-found)
                    (setf *last-classifier* classifier)
                )
                class-found            ;; return class
            )
        )
    )
)


;; gest-done-adding calculates the classifier weights, inverse covariance
;; matrix and discrimination constant for a given classifier
;; that has had examples of classes already given.  
;; Returns nil value on error, T on success, and 'FIX if fix-classifier
;; was called.
;;
;; Parameters: 
;;    classifier - classifier to finish calculating 
;;
(defun gest-done-adding (classifier)
    (let ((avgcov (make-array `(,NUM-FEATURES ,NUM-FEATURES)
                       :initial-element 0));; average covariance matrix
          (ne 0)          ;; total number of examples in classifier
          (denom 0)
          (oneoverdenom 0)
	  (result T)      ;; assume successful
          (det 0))        ;; determinant of inv. avg. covariance matrix

        (when (zerop (classifier-num-classes classifier))
            (warn "gest-done-adding () - empty classifier passed")
            (return-from gest-done-adding nil)
        )

        ;; given covariance matrices for each class, compute 
        ;; the average (common) covariance matrix by
        ;; first adding up all the covariance values for each class 
        ;; and then dividing by the total number of examples
        (do* ((rest-classes (classifier-classes classifier) 
                            (cdr rest-classes))
              (cur-class (car (classifier-classes classifier)) 
                         (car rest-classes)))
             ((null rest-classes))          ;; exit cond.
            (incf ne (g-class-num-examples cur-class))  
            (do ((i 0 (1+ i)))
                ((>= i NUM-FEATURES))      ;; exit condition
                (do ((j 0 (1+ j)))
                    ((>= j NUM-FEATURES))  ;; exit condition
                    (incf (aref avgcov i j) 
                          (aref (g-class-sumcov cur-class) i j))
                 )
            )
        )

        ;; make sure we have enough examples
        (setf denom (- ne (classifier-num-classes classifier))) 
        (when (<= denom 0)
            (warn "gest-done-adding () - classifier has too few examples")
            (return-from gest-done-adding nil)
        )

        ;; divide by the total number of examples
        (setf oneoverdenom (/ 1 denom))
        (do ((i 0 (1+ i)))
            ((>= i NUM-FEATURES))           ;; exit condition
            (do ((j i (1+ j)))
                ((>= j NUM-FEATURES))       ;; exit condition
                (setf (aref avgcov i j) 
                      (* (aref avgcov i j) oneoverdenom))
                (setf (aref avgcov j i) (aref avgcov i j)) 
            )
        )

        ;; invert the average covariance matrix
        (setf (classifier-invavgcov classifier) 
              (make-array `(,NUM-FEATURES ,NUM-FEATURES)))
        (setf det 
              (invert-matrix avgcov (classifier-invavgcov classifier)))

        ;; check to make sure it was invertible (i.e. not singular!)
        ;; if it wasn't make it invertible by throwing out features
        (when (<= (abs det) EPS-SM)
            (fix-classifier classifier avgcov)
	    ;; *** WORK *** remove if implement fix-classifier	    
	    (setf result 'FIX)
        )

        ;; now compute discrimination functions
        (setf (classifier-cnst classifier) 
              (make-array `(,(classifier-num-classes classifier))))
        (setf (classifier-weights classifier) 
              (make-array `(,(classifier-num-classes classifier))))
        (do ((c 0 (1+ c))
             (rest-classes (cdr (classifier-classes classifier)) 
                           (cdr rest-classes))
             (cur-class (car (classifier-classes classifier)) 
                        (car rest-classes))
             (w (make-array `(,NUM-FEATURES))))
             ((>= c (classifier-num-classes classifier))) ;; exit cond.
            (setf w (vector-times-matrix 
                        (g-class-average cur-class)
                        (classifier-invavgcov classifier)))
            (setf (aref (classifier-cnst classifier) c)
                  (* -.5 (inner-product(g-class-average cur-class) w)))
            (setf (aref (classifier-weights classifier) c) w)
        )
	result                                   ;; return result
    )
)


;; fix-classifier tries to make the given avgcov matrix for the
;; given classifier invertible by throwing out features that cause
;; it to be non-invertible.
;;
;; Parameters: 
;;    classifier - classifier to fix 
;;    avgcov     - covariance matrix that is non-invertible
(defun fix-classifier (classifier avgcov)
    (declare (ignore classifier avgcov))
    ;; add the features one by one, discarding any that cause
    ;; the matrix to be non-invertible
;;(warn "fix-classifier () called")
;;    (do ((i 0 (1+ i)))
;;        ((i >= NUM-FEATURES))
;;  ******************* WORK ************************** 
)


;; gest-strip-sumcov compacts the given classifier by setting the sumcov 
;; field to nil and the num-examples field to zero for each of the 
;; classes in the given classifier.
;; NOTE - by doing this, the classifier will not be able to have 
;; any new examples added to it properly.
;;
;; Parameters:
;;    classifier - classifier to strip from 
(defun gest-strip-sumcov (classifier)
    (do* ((rest-classes (classifier-classes classifier) 
                        (cdr rest-classes))
          (cur-class (car (classifier-classes classifier)) 
                     (car rest-classes)))
         ((null rest-classes))          ;; exit cond.
        (setf (g-class-num-examples cur-class) 0)  
        (setf (g-class-sumcov cur-class) nil)  
    )
)


;; gest-classifier-train trains the given classifier to recognize
;; the given class from the given examples.  If the given class
;; does not exist it will be created, otherwise the examples
;; are added to the examples the classifier already knows about.
;;
;; Parameters: 
;;    classifier - classifier to train
;;    class-name - name of class to recognize
;;    examples   - list of examples to train from 
;;                 (each examples is an array of points [x1 y1 x2 y2 ...]
;;
(defun gest-classifier-train (classifier class-name examples)
  (unless
      (every 'null
	     (mapcar #'(lambda (example)
			 (gest-add-example example class-name classifier))
		     examples)))
      nil
      ;;        (gest-done-adding classifier)
      )
