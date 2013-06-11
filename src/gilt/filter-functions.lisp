;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GILT; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The "Value Filter" extension to Gilt is a tool for assigning dependencies
;;; among objects.  With the Value Control module, the values returned by
;;; gadgets can be defined by a user-supplied expression.  The Enable Control
;;; module allows the user to define expressions that regulate whether gadgets
;;; are enabled and may be operated.  The Error Checking module allows the
;;; definition of an error handling routine, complete with a customized
;;; error dialog box.
;;;
;;; Designed by Brad Myers
;;; Implemented by Andrew Mickish

;;
;;  Functions for Value Filter module
;;

(in-package "GILT")

(eval-when (eval load compile)
  (export '(Get-Standard-Font Get-Font-From-File
	    Make-Number Make-Keyword Make-Atom Make-Color-Fill))
  (proclaim '(special *error-gadget*)))

;; The function Wrap-In-Error-Check implements the error checking which was
;; set up by the error check menu.  Set-Filter-Slot is the function that sets
;; the :filtered-value of the object.  The filter that the user typed in is
;; wrapped in a function call to check whether that expression returned one
;; of the error values in the error check association list.  If an error
;; message is found corresponding to the current filtered value, then the gilt
;; error gadget appears and gives the user the appropriate message.
;;
;; Wrap-In-Error-Check looks for a pair in the error-check-alist alist that
;; has a 'car' equal to the current filtered value.  If it finds a pair,
;; then it evals the cdr, which should be a format statement (which was
;; generated in the error-check module, and sends the resulting string
;; to the Gilt error gadget.
;;
(defun Wrap-In-Error-Check (expr)
  (let* ((ecv (gvl :error-check-alist))
	 (format-expr (cdr (assoc expr ecv :test #'equal))))
    (when format-expr
      (garnet-gadgets:display-error *error-gadget* (eval format-expr)))
    expr))


;;=========================================================================
;;
;;  Standard functions to use in filter expressions
;;
;;=========================================================================

;;   Each standard function supplied by the filter dialog boxes has an
;; associated parameter type list.  This plist is stored in the global
;; association list *Param-Type-List*.  These lists contain the lisp type
;; specifiers for the parameters to the functions.
;;
;; Example:  (typep 60 '(satisfies numberp))
;;           (typep :pop keyword)

(defparameter *Param-Type-List* NIL)


(push (cons 'Get-Standard-Font '(keyword keyword keyword)) *Param-Type-List*)
(defun Get-Standard-Font (&optional family face size)
  (if (and (typep family 'keyword)
	   (typep face 'keyword)
	   (typep size 'keyword))
      (progn
	(case family
	  (:times (setf family :serif))
	  (:helvetica (setf family :sans-serif))
	  (:courier (setf family :fixed)))
	(opal:get-standard-font family face size))
      :NOT-FONT))


(push (cons 'Get-Font-From-File '(string)) *Param-Type-List*)
(defun Get-Font-From-File (name)
  ;; This checks that the parameter is a string and that it is a valid
  ;; font name.  Returns NIL if name is not a string, and returns :NOT-FONT
  ;; if name is not a valid font name.
  (when (typep name 'string)
    (if (gem:font-exists-p (g-value opal::DEVICE-INFO :current-root) name)
	(create-instance NIL opal:font-from-file
	   (:font-name name))
	:NOT-FONT)))


(push (cons 'Make-Number '(string)) *Param-Type-List*)
(defun Make-Number (string)
  (and (typep string 'string)
       (let* ((symbol (gg:careful-read-from-string string
						   *error-gadget*))
	      (number (when (numberp symbol) symbol)))
	 number)))


(push (cons 'Make-Keyword '(string)) *Param-Type-List*)
(defun Make-Keyword (string)
  (and (typep string 'string)
       (let ((coloned-string (substitute #\- #\space
			       (if (eq #\: (char string 0))
				   string
				   (concatenate 'string ":" string)))))
	 (gg:careful-read-from-string coloned-string
				      *error-gadget*))))


(push (cons 'Make-Atom '(string)) *Param-Type-List*)
(defun Make-Atom (string)
  (and (typep string 'string)
       (let ((quoted-string (substitute #\- #\space
			      (if (eq #\' (char string 0))
				  string
				  (concatenate 'string "'" string)))))
	 (gg:careful-read-from-string quoted-string
				      *error-gadget*))))


(push (cons 'Make-Color-Fill '(number number number)) *Param-Type-List*)
(defun Make-Color-Fill (red green blue)
  (and (typep red 'number)
       (typep green 'number)
       (typep blue 'number)
       (create-instance NIL opal:default-filling-style
          (:foreground-color (create-instance NIL opal:color
			        (:red red) (:green green) (:blue blue))))))
