;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; matrix.lisp
;;;
;;; Routines for calculating matrix operations 
;;; (based on Dean Rubine's C implementation)
;;;
;;; Designed and implemented by James A. Landay

#|
============================================================
Change log:
    03/24/92 James Landay - check for zero before dividing with
                            equalp () rather than equal ()
    12/21/91 James Landay - started
============================================================
|#

(in-package "INTERACTORS")


;; inner-product returns the inner product of the two given vectors.
;;
;; Parameters:
;;    v1, v2 - column vectors to calculate inner product of
;;
(defun inner-product (v1 v2)
    (let ((dim (array-dimension v1 0))
          (result 0))
        (if (not (equal dim (array-dimension v2 0)))
            (error "inner-product () - vectors are different sizes")
        )

        (do ((n 0 (1+ n)))
            ((>= n dim))
            (incf result (* (aref v1 n) (aref v2 n)))
        )
        result               ;; return the result
    )
)


;; invert-matrix inverts the in-matrix and places the result
;; in out-matrix and then returns the determinant.  The matrices
;; must be square and of the same size.
;;
;; Parameters:
;;    in-matrix  - matrix to invert
;;    out-matrix - matrix to return result in
;;
(defun invert-matrix (in-matrix out-matrix)
    (let ((dim (array-dimension in-matrix 0))   ;; dimension of matrix
          (det 1)                               ;; determinant of matrix
          (l nil)                               ;; permutation vector
          (m nil)                               ;; permutation vector
          (temp 0))       

        (if (not (equal dim (array-dimension in-matrix 1)))
            (error "invert-matrix () - matrix not square")
        )

        (if (not (equal (array-dimensions in-matrix) 
                        (array-dimensions out-matrix)))
            (error "invert-matrix () - matrices not of the same size")
        )

        ;; copy in-matrix to out-matrix if they are not the same
        (when (not (equal in-matrix out-matrix))
            (do ((i 0 (1+ i)))
                ((>= i dim))    
                (do ((j 0 (1+ j)))
                    ((>= j dim)) 
                    (setf (aref out-matrix i j) (aref in-matrix i j))
                )
            )
        )

        ;; allocate permutation vectors for l and m, with the 
        ;; same origin as the matrix
        (setf l (make-array `(,dim)))
        (setf m (make-array `(,dim)))

        (do ((k 0 (1+ k))
             (biga 0)
             (recip-biga 0))
            ((>= k dim))

            (setf (aref l k) k)
            (setf (aref m k) k)
            (setf biga (aref out-matrix k k))

            ;; find the biggest element in the submatrix
            (do ((i k (1+ i)))
                ((>= i dim))    
                (do ((j k (1+ j)))
                    ((>= j dim)) 
                    (when (> (abs (aref out-matrix i j)) (abs biga))
                        (setf biga (aref out-matrix i j))
                        (setf (aref l k) i)
                        (setf (aref m k) j)
                    )
                )
            )

            ;; interchange rows
            (if (> (aref l k) k)
                (do ((j 0 (1+ j))
                     (i (aref l k)))
                    ((>= j dim)) 
                    (setf temp (- (aref out-matrix k j)))
                    (setf (aref out-matrix k j) (aref out-matrix i j))
                    (setf (aref out-matrix i j) temp)
                )
            )

            ;; interchange columns 
            (if (> (aref m k) k)
                (do ((i 0 (1+ i))
                     (j (aref m k)))
                    ((>= i dim)) 
                    (setf temp (- (aref out-matrix i k)))
                    (setf (aref out-matrix i k) (aref out-matrix i j))
                    (setf (aref out-matrix i j) temp)
                )
            )

            ;; divide column by minus pivot (value of pivot 
            ;; element is in biga)
            (if (equalp biga 0) 
                (return-from invert-matrix 0)
            )
            (setf recip-biga (/ 1 biga))
            (do ((i 0 (1+ i)))
                ((>= i dim)) 
                (if (not (equal i k))
                    (setf (aref out-matrix i k) 
                          (* (aref out-matrix i k) (- recip-biga)))
                )
            )

            ;; reduce matrix
            (do ((i 0 (1+ i)))
                ((>= i dim)) 
                (when (not (equal i k))
                    (setf temp (aref out-matrix i k))
                    (do ((j 0 (1+ j)))
                        ((>= j dim)) 
                        (if (not (equal j k))
                            (incf (aref out-matrix i j) 
                                  (* temp (aref out-matrix k j)))
                        )
                    )
                )
            )

            ;; divide row by pivot
            (do ((j 0 (1+ j)))
                ((>= j dim)) 
                (if (not (equal j k))
                    (setf (aref out-matrix k j)
                          (* (aref out-matrix k j) recip-biga))
                )
            )

            (setf det (* det biga))       ;; product of pivots
            (setf (aref out-matrix k k) recip-biga)
        ) ;; k loop

        ;; final row & column interchanges
        (do ((k (1- dim) (1- k)))
            ((< k 0))
            (if (> (aref l k) k)
                (do ((j 0 (1+ j))
                     (i (aref l k)))
                    ((>= j dim))
                    (setf temp (aref out-matrix j k))
                    (setf (aref out-matrix j k) 
                          (- (aref out-matrix j i)))
                    (setf (aref out-matrix j i) temp)
                )
            )
            (if (> (aref m k) k)
                (do ((i 0 (1+ i))
                     (j (aref m k)))
                    ((>= i dim))
                    (setf temp (aref out-matrix k i))
                    (setf (aref out-matrix k i) 
                          (- (aref out-matrix j i)))
                    (setf (aref out-matrix j i) temp)
                )
            )
        )
        det                     ;; return determinant
    )
)


;; print-square-matrix prints out the square matrix of the given size
;;
;; Parameters:
;;     matrix - matrix to print
;;     size   - size of matrix
(defun print-square-matrix (matrix size)
    (do ((i 0 (1+ i)))
        ((>= i size))      ;; exit condition
        (do ((j 0 (1+ j)))
            ((>= j size))  ;; exit condition
            (format t "~9,4F " (aref matrix i j))
        )
        (terpri)
    )
    (terpri)
)


;; quadratic-form returns the result of v'mv given 
;; a vector v and a matrix m. 
;;
;; Parameters:
;;    v - column vector
;;    m - matrix
;;
(defun quadratic-form (v m)
    (let ((rows (array-dimension v 0))
          (result 0))
        (if (not (equal rows (array-dimension m 0)))
            (error "quadratic-form: bad matrix size")
        )

        (do ((i 0 (1+ i)))
            ((>= i rows))
            (do ((j 0 (1+ j)))
                ((>= j rows))
                (incf result (* (aref m i j) (aref v i) (aref v j)))
            )
        )
        result              ;; return the result
    )
)


;; vector-times-matrix returns the result of multiplying
;; the given vector of dimensions (r x 1) by the given matrix
;; of dimensions (r x c).  The result is a column vector of 
;; dimensions (c x 1).
;;
;; Parameters:
;;    v - column vector to multiply (r x 1) 
;;    m - matrix to multiply (r x c)
;;
(defun vector-times-matrix (v m)
    (let ((result nil)
          (cols (array-dimension m 1))
          (rows (array-dimension m 0))) 
        (if (not (equal (array-dimension v 0) rows))
            (error "vector-times-matrix () - wrong matrix dimensions")
        )

        (setf result (make-array `(,cols) :initial-element 0))

        (do ((j 0 (1+ j)))
            ((>= j cols))
            (do ((i 0 (1+ i)))
                ((>= i rows))
                (incf (aref result j) (* (aref v i) (aref m i j)))
            )
        )
        result                  ;; return the result
    )
)
