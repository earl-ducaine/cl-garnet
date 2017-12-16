;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-UTILS; Base: 10 -*-


;; The Garnet User Interface Development Environment.
;;
;; This code was written as part of the Garnet project at Carnegie
;; Mellon University, and has been placed in the public domain.
;;
;; by David S. Kosbie

;; Host of Lisp utilities used by other Garnet code.

(in-package :garnet-utils)

(defvar *debug-utils-mode* t)

(defconstant pi/2 (/ pi 2))
(defconstant pi3/2 (* 3 (/ pi 2)))
(defconstant 2PI (* 2 PI))
(defconstant -2PI (- (* 2 PI)))
(defconstant short-PI (coerce PI 'short-float))

(defmacro while (test &rest body)
  "Loop while test is true. If already not true, don't loop at all."
  `(do ()
     ((not ,test))
     ,@body))

(defmacro till (test &body body)
  "Loop until test is true. If already true, don't loop at all."
  `(do ()
       (,test)
     ,@body))

;; Original Garnet version (loops at least once).
(defmacro until (test &body body)
  "Loop until test is true. Loops at least once."
  `(loop ,@body
      (when ,test (return))))

(defmacro do2lists ((var1 list1 var2 list2 &key either?) &rest body)
 (let ((list1var  (gensym))
       (list2var  (gensym))
       (done-test (if either? 'and 'or)))
  `(let ((,list1var ,list1)
         (,list2var ,list2)
         ,var1 ,var2)
      (while (,done-test ,list1var ,list2var)
        (setq ,var1 (car ,list1var))
        (setq ,var2 (car ,list2var))
        (setq ,list1var (cdr ,list1var))
        (setq ,list2var (cdr ,list2var))
       ,@body))))

(defmacro dolist2 ((var1 var2 list) &rest body)
  (let ((listvar (gensym)))
  `(let ((,listvar ,list) ,var1 ,var2)
     (while ,listvar
       (setq ,var1 (car ,listvar))
       (setq ,var2 (cadr ,listvar))
       (setq ,listvar (cddr ,listvar))
       ,@body))))

(defmacro m (s-expr)
  `(pprint (macroexpand (quote ,s-expr))))

(defmacro m1 (s-expr)
  `(pprint (macroexpand-1 (quote ,s-expr))))

(defmacro string+ (&rest args) `(concatenate 'string ,@args))

(defun add-to-list (element list &optional where locator)
  "Add-to-list legal invocations:
     (add-to-list element list)
     (add-to-list element list :head) (or :front)
     (add-to-list element list :tail) (or :back)
     (add-to-list element list :before other-element)
     (add-to-list element list :after other-element)"
 (let  ((new-cons (list element))
        result)
   (if (null list)
       (setq result new-cons)
       (case where
         ((:head :front NIL)
          (setq result (cons element list)))
         ((:before)
          (if (eq (first list) locator)
              (setq result (cons element list))
              (do ((cons1 list (cdr cons1))
                   (cons2 (cdr list) (cdr cons2)))
                  ((null cons2))
                (when (eq (first cons2) locator)
                  (setf (cdr new-cons) cons2)
                  (setf (cdr cons1) new-cons)
                  (setq result list)
                  (return)))))
         ((:after)
          (do ((cons1 list (cdr cons1))
               (cons2 (cdr list) (cdr cons2)))
              ((null cons1))
            (when (eq (first cons1) locator)
              (setf (cdr new-cons) cons2)
              (setf (cdr cons1) new-cons)
              (setq result list)
              (return))))))
 (unless result
   (setf (cdr (last list)) new-cons)
   (setq result list))
 result))


;; Verify-Binding implementation
;;
;; Keep --- a demo uses this
(defun verify-binding (string)
  "Takes a string and returns the symbol coercion of the string if the
   symbol is bound.  Note: The suffix of the string is converted to
   all uppercase characters before checking if it is bound in the
   package."
  (let ((result-1 (verify-binding-aux string 0)))
    (if result-1
        (let* ((colon-p (first result-1))
               (prefix (second result-1))
               (symbol-1 (values (read-from-string prefix)))
               (index (third result-1)))
          (if colon-p
              ;; Then symbol-1 indicates a package name
              (when (find-package symbol-1)
                ;; Then symbol-1 is a valid package name
                (let ((result-2 (verify-binding-aux string (+ 1 index))))
                  (when result-2
                    ;; Then suffix indicates a var in the package symbol-1
                    (let* ((suffix (string-upcase (second result-2)))
                           (access-internal-p (fourth result-2)))
                      (multiple-value-call
                          #'(lambda (symbol-2 access)
                              (if symbol-2
                                  (if (or (eq access :external)
                                          access-internal-p)
                                      ;; verify that symbol-2 is not a function
                                      (when (boundp symbol-2)
                                        (values (read-from-string string)))
                                      )))
                        (find-symbol suffix symbol-1))))))
              ;; Then symbol indicates a var in the working package
              (when (and (not (numberp symbol-1)) (boundp symbol-1)) symbol-1))))))

(defun verify-binding-aux (string start)
  "Split the string at the colon(s) to return either the package name
   or the symbol name if called where the first character is a colon."
  (let ((str-len (length string)))
    (when (> str-len start)
      ;; Skip second colon if there is a double colon between package and var
      (let ((access-internal-p (when (and (char= (char string start) #\:)
                                          (/= start 0))
                                 (incf start))))
        ;; Abort if a special character begins the string
        (unless (or (char= (char string start) #\:)
                    (char= (char string start) #\#))
          ;; Return the part of the string up to but not including the colon
          ;; and the index of the last character checked
          ;; FMG --- Just use lisp utilities to do this stuff.
          (let* ((colon (position #\: string :start start :test #'char=))
                 (new-string (subseq string start colon)))
            (list (not (null colon))
                  new-string
                  (or colon (1- str-len))
                  access-internal-p)))))))

(defun safe-functionp (fn)
  (or (functionp fn)
      (and (symbolp fn) (fboundp fn))))

(defun directory-p (pathname)
  (cl-fad:directory-exists-p filename))

(defmacro str (&rest rest)
  `(concatenate 'string ,@rest))
