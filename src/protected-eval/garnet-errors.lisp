;;; -*- Mode: COMMON-LISP; Package: GARNET-GADGETS -*-               ;;
;;-------------------------------------------------------------------;;
;;            Copyright 1993 Russell G. Almond                       ;;
;;-------------------------------------------------------------------;;
;; This code is in the Public Domain.  Anyone who can get some use   ;;
;; from it is welcome.                                               ;;
;; This code comes with no warranty.                                 ;;
;;-------------------------------------------------------------------;;

;;; $Id$

(in-package "GARNET-GADGETS")


;;; Garnet error handler functions
;;
;;
;;  These functions provide a concrete instantiation of some of 
;;  the abstract error handling facilities in abstract-errors.lisp. 
;;  This file must be loaded after abstract-errors.lisp.

(defun protect-errors (context condition &key
					 (allow-debugger
					  (eql *user-type* :programmer)))
  "Error handler which prompts user for a choice of
   :ABORT, :DEBUG, :CONTINE, :USE-VALUE and :STORE-VALUE
   restarts.

<context> is used to supply a context for the error.
<allow-debugger> is used to determine whether or not the user can
enter the LISP debugger.

Should be invoked with an expression such as:
  (handler-bind 
    ((error \#'(lambda (condition)
		 (protect-errors context-string condition))))
  ...)

"
  (gg:garnet-error-handler context condition :allow-debugger allow-debugger))


(defmacro with-protected-errors (context &body forms)
  "Executes forms in a protected environment where errors are handled
by prompting-error-handler, which creates queries the user with
options to abort or continue, possibly with various recovery
strategies.  If rga:*user-type* is :programmer, then allows debugging.

<context> should be a string describing user meaningful context in
which error occured."
  `(handler-bind
       ((error 
	 (lambda (condition)
	   (protect-errors ,context condition))))
     ,.forms))




(defun protected-eval (form &rest args
		       &key (default-value nil dv?)
			    (context (format nil "Evaluating ~S" form))
			    (allow-debug (eq *user-type* :programmer))
			    (local-abort nil)
			    (abort-val nil))
  "This function executes a form in an environment where errors are
caught and handled by a special protected-error.  This
gadget prints the error message and allows for several different
restarts:  ABORT, DEBUG and CONTINUE, USE-VALUE and STORE-VALUE.

<form> is the form to be evaluated.
<default-value> if supplied, produces a continue restart which returns
that value.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter.

If <allow-debug> is nil (defaul (eq *user-type* :programmer)) then the
debug switch is suppressed.

<context> is a string defining the context of the error.  Default
value is `Evaluating <form>'.

This abtract function allows the type of error handler to be hidden
from the routine which sets it up.  In particular, both
promting-protected-eval and protected-eval could be bound to
this symbol."
  (declare (ignore default-value dv? context local-abort abort-val))
  (apply #'gg:garnet-protected-eval form :allow-debug allow-debug args))

(defun protected-read (&optional (stream *standard-input*)
				 &rest args
		       &key (context (format nil "Reading from ~S" stream))
			    (read-package *package*)
			    (read-bindings nil)
			    (default-value nil)
			    (allow-debug nil)
			    (local-abort nil)
			    (abort-val nil))
  "This works rather like protected-eval except it tries to
read from the <stream>.

<stream> is the stream to be read from (if omitted *standard-input*).

<read-package> (default :user) selects the package to read from.  This
is because I don't want to make any assumptions about what the binding
of package will be at eval time especially in a multiprocessed lisp,
and I think this is safer.  If you want the string to be read in a
different package,  you can try using :read-package *package*

<read-bindings> is a list of (var . form)'s as in a let statement.
These bindings are made (with the let) before reading the string to
allow for effects such as binding the readtable.

<default-value> (default nil) this establishes a continue restart
which returns this value.  Note that this is slightly different from
protected-eval in that it is always available.

<allow-debug> (default (eq *user-type* :programmer) if true, this
includes a button which allows the debugger to be entered on an error.
Note that the default value is different from protected-eval.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter. (Same as
protected-eval).

This abtract function allows the type of error handler to be hidden
from the routine which sets it up.  In particular, both
promting-protected-eval and protected-eval could be bound to
this symbol."
  (declare (ignore context read-package read-bindings
		   default-value local-abort abort-val))

  (apply #'gg:garnet-protected-read stream :allow-debug allow-debug args))

(defun protected-read-from-string (string
				   &rest args
				   &key (start 0)
				        (context (format nil "Parsing ~S" string))
				        (end (length string))
				        (read-package *package*)
				        (read-bindings nil)
				        (default-value nil)
				        (allow-debug nil)
				        (local-abort nil)
				        (abort-val nil))
  "This works rather like protected-eval except it tries to
read from the <stream>.

<string> is the string to be read from (probably the :string of a text
input gadget).

<start> and <end> allow selecting a substring.

<read-package> (default :user) selects the package to read from.  This
is because I don't want to make any assumptions about what the binding
of package will be at eval time especially in a multiprocessed lisp,
and I think this is safer.  If you want the string to be read in a
different package,  you can try using :read-package *package*

<read-bindings> is a list of (var . form)'s as in a let statement.
These bindings are made (with the let) before reading the string to
allow for effects such as binding the readtable.

<default-value> (default nil) this establishes a continue restart
which returns this value.  Note that this is slightly different from
protected-eval in that it is always available.

<allow-debug> (default (eq *user-type* :programmer) if true, this
includes a button which allows the debugger to be entered on an error.
Note that the default value is different from protected-eval.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter. (Same as
protected-eval)."

  (declare (ignore start context end read-package read-bindings 
		   default-value local-abort abort-val))

  (apply #'gg:garnet-protected-read-from-string string :allow-debug allow-debug args))


(defun call-prompter (prompt 
		      &rest args
		      &key (stream *query-io*)
			   (local-abort nil)
			   (default-value nil dv?)
			   (abort-val :ABORT)
			   (eval-input? nil)
			   (satisfy-test #'(lambda (obj) T))
		      &allow-other-keys)
  "Prompts user for an input.  <Prompt> is printed with ~A as a prompt.
<stream> defaults to *query-io*.  If <local-abort> is true a local
abort is set up which will return the values <abort-val> and :ABORT.
If <default-value> is supplied, a CONTINUE restart is set up which
allows the user to select the default value.

If <eval-input?> is true, then the expression is evaluated before it
is returned; if not, the unevaluated expression is returned.  

The value supplied by the user is passed to <satisfy-test>.  If that
test fails, the user is prompted again.

This is mostly a dummy function for hiding the prompter type from the
implementation mechanism."

  (declare (ignore stream local-abort default-value dv? abort-val
		   eval-input? satisfy-test))
  (apply #'gg:do-prompt prompt :allow-other-keys t args))

(kr:s-value (kr:g-value gg:Error-prompter-gadget :window)
    :title (format nil "~A:Prompter" *application-long-name*))
    
(kr:s-value (kr:g-value gg:Error-prompter-gadget :window)
    :icon-title (format nil "~A:Prompter" *application-short-name*))

(kr:s-value (kr:g-value gg:protected-eval-error-gadget :window)
    :title (format nil "~A:Error-Handler" *application-long-name*))
    
(kr:s-value (kr:g-value gg:protected-eval-error-gadget :window)
    :icon-title (format nil "~A:Error" *application-short-name*))

(kr:create-instance 'message-display gg:Motif-Error-Gadget
  (:modal-p nil)
  (:window-top (floor gem:*screen-height* 2))
  (:window-left (floor gem:*screen-width* 2)))


(kr:s-value (kr:g-value message-display :window)
    :title (format nil "~A:Message" *application-long-name*))
    
(kr:s-value (kr:g-value message-display :window)
    :icon-title (format nil "~A:Message" *application-short-name*))


(defun call-displayer (message &rest keys
		       &key (stream *standard-output*)
			    (beep t)
			    (wait nil)
		       &allow-other-keys)
    "A generic display function which sends a display to the specified
location. <stream> indicates the stream to which the message is to be
sent in the text based version.  <beep> is a logical value indicating
whether or not the device should make some sort of alert signal.  This
is meant to be called through call-displayer."
    (declare (ignore keys stream))
    (kr:s-value message-display :beep-p beep)
    (if wait
	(gg:display-error-and-wait message-display message)
      (gg:display-error message-display message)))


(kr:create-instance 'selector-display gg:motif-query-gadget  #-(and)gg:query-gadget
  (:modal-p nil)
  (:window-top (floor gem:*screen-height* 2))
  (:window-left (floor gem:*screen-width* 2)))

(kr:s-value (kr:g-value selector-display :window)
    :title (format nil "~A:Selection" *application-long-name*))
    
(kr:s-value (kr:g-value selector-display :window)
    :icon-title (format nil "~A:Selection" *application-short-name*))



(defun call-selector (message &rest keys
		      &key (stream *query-io*)
			   (in-stream stream)
			   (out-stream stream)
			   (beep t)
			   (option-list '(:yes :no))
		      &allow-other-keys)
  "This function offers the user a choice of items from a menu of
keywords.  The user can either type the keword or select the option by
number.  <stream> is the stream (default *query-io*) and <option-list>
is the list of options (default '(:yes no)).  <message> is displayed
first on the stream as a prompt."
  (declare (type String message) (type Stream stream in-stream out-stream)
	   (type List option-list)
	   #-(and)(:returns (type (Member option-list) option)))
  (declare (ignore keys in-stream out-stream))
  (kr:s-value selector-display :beep-p beep)
  (gg:display-query-and-wait selector-display message option-list))


