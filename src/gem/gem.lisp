;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GEM; Base: 10 -*-

;;; The Garnet User Interface Development Environment.
;;;
;;; This code was written as part of the Garnet project at Carnegie
;;; Mellon University, and has been placed in the public domain.
;;;
;;; This file defines gem-method, which is used to declare, create,
;;; and export the generic Gem methods.  Methods are implemented as
;;; macros which dispatch on the :methods slot of a window (or font)
;;; to find the appropriate method for each device.

(in-package :gem)

;; Moved here from opal:defs.lisp for the sake of modularity. This is
;; sort of the X footprint in opal. So we import the symbols into
;; opal.
;;
;; This defstruct generates the functions make-display-info,
;; copy-display-info, display-info-display, display-info-screen,
;; display-info-root-window, display-info-line-style-gc, and
;; display-info-filling-style-gc.
(defstruct (display-info (:print-function display-info-printer))
  display
  screen
  root-window
  line-style-gc
  filling-style-gc)

(defvar *method-names* nil
  "Holds the method names.  This is used to create the Gem interface macros.")

(defvar *device-initializers* nil
  "An a-list which associates device types with the function to be called
     to initialize them.")

;;; Methods mechanism
(defun attach-method (device method-name method)
  (let ((number (find-or-create-name method-name))
	(methods (g-value device :methods)))
    (unless methods
      (s-value device :methods
	       (setf methods (make-array (length *method-names*)))))
    (when (<= (length methods) number)
      (let ((array (make-array (1+ number))))
	(dotimes (i (length methods))
	  (setf (aref array i) (aref methods i)))
	(setf methods array)
	(s-value device :methods array)))
    (setf (aref methods number)
	  (lambda (&rest args)
		 ;; all 'x' methods should initial the display if it
		 ;; hasn't been.  By adding this as a wrapper to all
		 ;; 'attach-method' api entry points, we avoid having
		 ;; to call this at compile or change the way the gem
		 ;; api.
		 (init-device)
		 (apply method args)))))

(defun set-window-methods (window device)
  (s-value window :device device)
  (s-value window :methods (g-value device :methods)))

;;; Interface definitions
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun find-or-create-name (name)
    (let ((pos (position name *method-names*)))
      (unless pos
	(if *method-names*
	    (setf (cdr (last *method-names*)) (list name))
	    (setf *method-names* (list name)))
	(setf pos (1- (length *method-names*))))
      pos)))


;; The name is determined by the <method-name> (which should be a
;; keyword).  The <args> are used for the macro definition.  The first
;; of the <args> must be a window, which determines the device (and
;; hence the method).  The generated macro simply does a funcall on
;; the appropriate method for the device, passing all the arguments
;; verbatim.
;;
;; Things are handled properly when <args> contains &rest, &optional, or
;; &key parameters."
;;
;; XXX FMG the above appears to be incorrect: the following generates a defun
;; while the commented out code below generates a defmacro (and the one below
;; that generates a defun as well).
(defmacro gem-method (method-name (&rest args))
  (let ((function-name (intern (symbol-name method-name) (find-package "GEM")))
	(has-rest (find '&rest args)))
    `(progn
       ;; Make sure the method name is defined when we load this.
       (find-or-create-name ,method-name)
       ;; Define the interface function itself, which will dispatch on
       ;; its first argument (a window) to find the appropriate
       ;; device-specific argument.
       (defun ,function-name (,@args)
	 (,(if has-rest 'APPLY 'FUNCALL)
	   (aref (g-value ,(car args) :METHODS)
		 ,(find-or-create-name method-name))
	   ,@(if (or has-rest (intersection '(&key &optional) args))
		 ;; We must manipulate the arguments list.
		 (do ((head args (cdr head))
		      (in-key NIL)
		      (final nil))
		     ((null head)
		      (nreverse final))
		   (case (car head)
		     ((&optional &rest))
		     (&key
		      (setf in-key T))
		     (T
		      (let ((symbol (car head)))
			(if (listp symbol)
			    (setf symbol (car symbol)))
			(if in-key
			    (push (intern (symbol-name symbol)
					  (find-package "KEYWORD"))
				  final))
			(push symbol final)))))
		 ;; Arguments list is OK as is.
		 args)))
       ;; Export the interface function from the Gem package.
       (eval-when (:execute :load-toplevel :compile-toplevel) (export ',function-name)))))

;; Same, generates a function instead of a macro.
#-(and)
(defmacro gem-method (method-name (&rest args))
  (let ((macro-name (intern (symbol-name method-name) (find-package "GEM")))
	(has-rest (find '&rest args)))
    `(progn
       (defmacro ,macro-name (,@args)
	 (,(if has-rest 'LIST* 'LIST)
	   'FUNCALL
	   (list 'aref (list 'g-value ,(car args) :METHODS)
		 ,(find-or-create-name method-name))
	   ,@(if (or has-rest (intersection '(&key &optional) args))
		 ;; We must manipulate the arguments list.
		 (do ((head args (cdr head))
		      (in-key NIL)
		      (final nil))
		     ((null head)
		      (nreverse final))
		   (case (car head)
		     ((&optional &rest))
		     (&key
		      (setf in-key T))
		     (T
		      (let ((symbol (car head)))
			(if (listp symbol)
			    (setf symbol (car symbol)))
			(if in-key
			    (push (intern (symbol-name symbol)
					  (find-package "KEYWORD"))
				  final))
			(push symbol final)))))
		 ;; Arguments list is OK as is.
		 args)))
       (export ',macro-name))))

#-(and)
(defmacro gem-method (method-name (&rest args))
  (let ((macro-name (intern (symbol-name method-name) (find-package "GEM")))
	(has-rest (find '&rest args)))
    `(eval-when (:load-toplevel :execute :compile-toplevel)
       (defun ,macro-name (,@args)
	 (,(if has-rest 'APPLY 'FUNCALL)
	   (aref (g-value ,(car args) :METHODS)
		 ,(find-or-create-name method-name))
	   ,@(if (or has-rest (find '&optional args))
		 ;; We must manipulate the arguments list.
		 (mapcar #'(lambda (x) (if (listp x) (car x) x))
			 ;; Eliminate &optional, &rest from arguments
			 (delete '&optional
				 (delete '&rest (copy-list args))))
		 ;; Arguments list is OK as is.
		 args)))
       (export ',macro-name))))


;; This schema is kind of a bridge between GEM and OPAL. It's mostly
;; used in OPAL but because it's used in GEM as well, it needs to be
;; in this file for modularity's sake.

;; The :current-root slot indicates the current device. This is used
;; for all calls to Gem which occur in places where explicit device
;; information is not available. The :active-devices slot contains the
;; list of all the devices that have been initialized.
(create-schema 'device-info
	       (:current-root NIL)
	       (:active-devices NIL))

(defvar  *gem-device-initialized* nil)

(defun init-device ()
  ;; This schema stands for the top-level root window for the X
  ;; device.  We use create-schema to prevent any :initialize method
  ;; from firing.
  (create-schema '*root-window* (:is-a opal::window))
  ;; This schema points to the root window, and contains the slot
  ;; :methods which names all existing Gem method.  The slot is copied
  ;; into the root nodes of the windows and fonts hierarchies.
  (create-schema 'x-device (:root-window *root-window*) (:device-type :x))
  (attach-x-methods x-device)
  (let ((full-display-name (get-full-display-name)))
    (when (and full-display-name (not *gem-device-initialized*))
      (initialize-device-values (get-full-display-name) *root-window*)
      (s-value device-info :current-root *root-window*)
      (s-value device-info :current-device x-device)
      (pushnew x-device (g-value device-info :active-devices))
      (let ((display-info (x-initialize-device *root-window*)))
	(s-value *root-window* :drawable
		 (display-info-root-window display-info))
	(s-value *root-window* :display-info display-info))
      (x-set-draw-functions *root-window*)
      (setf *gem-device-initialized* t)
      *root-window*)))

;;; This is a utility function, used only for interactive debugging.
(defmacro adjust (name)
  `(progn
     (attach-method
      x-device
      (intern ,(symbol-name name) (find-package "KEYWORD"))
      (symbol-function (intern (concatenate 'string "X-" ,(symbol-name name))
			       (find-package "GEM"))))
     (set-window-methods opal::window gem::x-device)))

;;; Another debugging function
(defun trace-gem (device)
  (let ((prefix
	 (case device
	   (:X "GEM::X-")
	   (t
	    (error "Unknown device ~S in trace-gem" device)))))
    (dolist (key *method-names*)
      (let ((name (read-from-string
		   (concatenate 'simple-string prefix (symbol-name key)))))
	(eval `(trace ,name))
	(eval `(adjust ,key))))))

(defun untrace-gem (device)
  (let ((prefix
	 (case device
	   (:X "GEM::X-")
	   (t
	    (error "Unknown device ~S in trace-gem" device)))))
    (dolist (key *method-names*)
      (let ((name (read-from-string
		   (concatenate 'simple-string prefix (symbol-name key)))))
	(eval `(untrace ,name))
	(eval `(adjust ,key))))))

;;; Given an Opal window, return the display structure that is attached
;;; to it.
(defmacro the-display (window)
  `(display-info-display (g-value ,window :display-info)))

;;; RETURNS: the buffer for the <window>, if this is a double-buffered
;;; window, or the main drawable otherwise.
(defmacro the-drawable (window)
  `(or (g-local-value ,window :buffer) (g-value ,window :drawable)))
