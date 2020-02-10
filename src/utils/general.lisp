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


(defun safe-functionp (fn)
  (or (functionp fn)
      (and (symbolp fn) (fboundp fn))))
