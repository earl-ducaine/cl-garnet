;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: INTERACTORS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; fileio.lisp
;;;
;;; Routines for loading and saving gesture classifiers.   
;;;
;;; Designed and implemented by James A. Landay

#|
============================================================
Change log:
    06/10/93 James Landay - read/write also read & write class examples
     4/07/92 Pedro Szekely - Let *print-array* be T in gest-classifier-write
		so that arrays will not be printed as #<...> in Lucid.
    12/31/92 James Landay - changed function names
    12/21/92 James Landay - started
============================================================
|#

(in-package "INTERACTORS")


;; gest-classifier-read returns the classifier and class examples 
;; (if present -- in items lists format) read from the given file.
;;
;; Parameters:
;;    filename   - file name to read from 
;;
(defun gest-classifier-read (filename)
  (let ((*print-level* nil)
	(*print-length* nil))
    (with-open-file (str filename :direction :input)
		    (values (read str)             ;; read classifier
			    (read str NIL NIL))))) ;; read examples, if present


;; gest-classifier-write writes the given classifier and the optional
;; class examples to disk with the given file name.
;;
;; Parameters:
;;    classifier    - classifier to write
;;    filename      - file name to save under
;;    class-exampes - optional set of examples
;;
(defun gest-classifier-write (classifier filename &optional class-examples)
    (let ((*print-level* nil)
          (*print-length* nil)
	  (*print-array* T))
        (with-open-file (str filename :direction :output 
                                      :if-exists :supersede)
                        (write classifier :stream str :pretty nil)
			(if class-examples
			    (write class-examples :stream str :pretty nil)))))


;; gest-classifier-convert returns the classifier read from the
;; given file that is in Dean Rubine's format and converts it
;; to our format.
;;
;; Parameters:
;;    filename - file name to read from 
;;
(defun gest-classifier-convert (filename)
    (let ((classifier (gest-new-classifier))
          (classes nil))
        
        (with-open-file (stream filename :direction :input) 

            ; get the number of classes
            (setf (classifier-num-classes classifier) (read stream))
            (read stream)     ; throw away string 'classes'

            ; read in and add the new class names
            (do ((cur (read stream))
                 (i (classifier-num-classes classifier) (decf i)))
                ((zerop i))

                (setf classes (append classes (list cur)))
                (when (not (= i 1))
                      (setf cur (read stream)))
            )
           
            ;; initialize the classifier fields
            (setf (classifier-cnst classifier) 
                  (make-array `(,(classifier-num-classes classifier))))
            (setf (classifier-weights classifier) 
                  (make-array `(,(classifier-num-classes classifier))))

            ; read average vector and weight vector for each class
            (do ((new-class (make-g-class) (make-g-class))
                 (cur-class (car classes) (car classes))
                 (ignore 0)
                 (i 0 (incf i)))
                ((equalp i (classifier-num-classes classifier)))

                (setf (g-class-name new-class) cur-class)
                (setf (g-class-num-examples new-class) 0)
                
                ; drop the "V" and find out # of rows to skip at end
                (read stream)
                (setf ignore (- (read stream) NUM-FEATURES))

                ; read the average vector 
                (setf (g-class-average new-class) 
                      (read-vector stream NUM-FEATURES))
                (skip-input-items stream ignore)

                ; read the weight vector
                (read stream)               ; skip the "V"
                (read stream)               ;   and the size
                (setf (aref (classifier-weights classifier) i)
                      (read-vector stream NUM-FEATURES))
                (skip-input-items stream ignore)

                (setf (classifier-classes classifier)
                      (append (classifier-classes classifier) 
                              (list new-class)))
                (setf classes (cdr classes))
            )

            ; read in the constant vector 
            (read stream)               ; skip the "V"
            (read stream)               ;   and the size
            (setf (classifier-cnst classifier)
                  (read-vector stream 
                               (classifier-num-classes classifier)))
        
            ; read in the inverse convariance matrix
            (read stream)               ; skip the "M"
            (read stream)               ;   and the first size
            (setf (classifier-invavgcov classifier)     
                  (read-square-matrix stream NUM-FEATURES 
                      (- (read stream) NUM-FEATURES)))
        )
        (print classifier)
        classifier              ;; return the new classifier
    )
)


;; gest-read-example reads the next example in Dean Rubine's format
;; from the given open stream and returns a list with the name
;; of the class and the name of the example at the head followed by 
;; the points.
;;
;; Parameters:
;;    stream - the open stream to read from 
;;
(defun gest-read-example (stream)
    (let ((g-points nil))
        (read stream)                       ; ignore the leading x
        ; get the class name and the example name
        (setf g-points (list (read stream) (read stream)))

        (do ()
            ((or (equal #\x (peek-char nil stream nil 'EOF))
                 (equal 'EOF (peek-char nil stream nil 'EOF))))

            (read stream)           ; ignore the zero

            ; add the next x,y coordinate to the list
            (setf g-points (append g-points (list (read stream))
                                            (list (read stream))))
            (read stream)           ; ignore time 
        )
        g-points                ; return the example
    )
)


;; read-vector reads and returns a vector of the given
;; size from the given open stream. 
;;
;; Parameters:
;;    stream - the open stream to read from
;;    size   - number of elements in the vector to read
(defun read-vector (stream size)
    (let ((vector (make-array `(,size):initial-element 0)))
        (do ((cur (read stream))
             (j 0 (incf j)))
            ((equal j size) vector)         ; return the vector 

            (setf (aref vector j) cur) 
            (when (not (= (1- size) j))
                  (setf cur (read stream)))
        )
    )
)


;; read-square-matrix reads and returns a square matrix of the given
;; size from the given open stream. 
;;
;; Parameters:
;;    stream - the open stream to read from
;;    size   - number of elements in a row or column of the matrix 
;;    skip   - number of elements to skip at the end of each column
(defun read-square-matrix (stream size skip)
    (do ((j 0 (incf j))
         (matrix (make-array `(,size):initial-element 0)))
        ((equal j size) matrix)         ; return the matrix

        (setf (aref matrix j) (read-vector stream size))
        (when (not (= (1- size) j))
              (skip-input-items stream skip))
    )
)


;; skip-input-items will do enough reads of the input
;; to skip the give number of items. NOTE: the file must
;; already be open into the given stream.
;;
;; Parameters:
;;    stream    - the stream we are reading from
;;    num-items - number of items to skip
;;
(defun skip-input-items (stream num-items)
    (do ((i num-items (decf i)))
        ((< i 1))
        (read stream)
    )
)
