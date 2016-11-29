;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*- ;;
;;-------------------------------------------------------------------;;
;;          The Garnet User Interface Development Environment.       ;;
;;-------------------------------------------------------------------;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;-------------------------------------------------------------------;;

;;; $Id$
;;

(in-package "COMMON-LISP")

(in-package "OPAL")

;;; Exports moved to exports.lisp.

(defvar garnet-image-date NIL)

;; The purpose of opal:drawable-to-window is to allow external references to
;; the function gem:window-from-drawable, without having to always explicitly
;; reference the DEVICE-INFO object.
(defun drawable-to-window (device-drawable)
  (gem:window-from-drawable (g-value gem:DEVICE-INFO :current-root)
			    device-drawable))

(defparameter *util_month-list*
  '("" "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun Time-To-String ()
  (multiple-value-bind
      (second minute hour date month year day-of-week savingsp time-zone)
      (get-decoded-time)
    (declare (ignore second time-zone day-of-week))
    (if (>= hour 12) (progn (setq savingsp " PM")
		       (when (> hour 12)(incf hour -12)))
	(setq savingsp " AM"))
    (concatenate 'string
               (nth month *util_month-list*) " "
               (princ-to-string date) ", " (princ-to-string year)
                      ", "
               (princ-to-string hour)
               ":"
               (if (< minute 10) "0" "")
               (princ-to-string minute) savingsp)))


(defun announce-debug-state ()
  (when (boundp 'cl-user::garnet-garnet-debug)
    (format t "*** Built with debugging")
    (if  cl-user::garnet-garnet-debug
	 (format t " *ON* (debugging version)")
	 (format t " *OFF* (production version)")
	 )
    (format t " ***~%")
    (format t "*** using the following compiler policy: ***~%")
    (format t "~A~%" cl-user::default-garnet-proclaim)))


(defun garnet-restart-function ()
  (format t "*** Restarting Garnet ~A image. ***~%"
	  common-lisp-user::Garnet-Version-Number)
  (when (and (boundp 'garnet-image-date) garnet-image-date)
    (format t "*** Image creation date: ~A ***~%" garnet-image-date))
  (announce-debug-state)
  (opal:reconnect-garnet))


#-(and)
(defun garnet-restart-function ()
  (format t "*** Restarting Garnet ~A image created on ~A ***~%"
	  common-lisp-user::Garnet-Version-Number
	  garnet-image-date)
  (announce-debug-state)
  (opal:reconnect-garnet))


(defun Extract-Image-Args (args)
  (let ((quit NIL)
	(gc T)
	(verbose T)
	(libfile NIL)
	(flush-source-info? NIL)
	(extra-args NIL))
    (do* ((args-aux args (cddr args-aux))
	  (arg1 (first args-aux) (first args-aux))
	  (arg2 (second args-aux) (second args-aux)))
	 ((null args-aux))
      (case arg1
	(:quit (setf quit arg2))
	(:verbose (setf verbose arg2))
	(:gc (setf gc arg2))
	(:libfile (setf libfile arg2))
	(:flush-source-info? (setf flush-source-info? arg2))
	(T (setf extra-args (append extra-args (list arg1 arg2))))))
    (values quit gc verbose libfile flush-source-info? extra-args)))


(defun make-image (filename &rest args)
  #-(or allegro ccl cmu sbcl)
    (error "Don't know how to automatically save an image for this lisp.
Please consult your lisp's user manual for instructions.~%")

  (multiple-value-bind (quit gc verbose libfile flush-source-info? extra-args)
      (Extract-Image-Args args)
    (declare (ignore libfile #+sbcl quit))
    #-allegro (declare (ignore flush-source-info?))

  ;; When the image is restarted, we want *readtable* to be restored to its
  ;; current value, instead of being reinitialized to the default.  This will
  ;; keep the #k<> and #f() reader macros active in the saved image.
  #+allegro
  (progn
    (when verbose (format t "~%Copying readtable..."))
    (copy-readtable *readtable* common-lisp-user::Garnet-Readtable)
    (setf (cdr (assoc '*readtable* excl:*cl-default-special-bindings*))
          'common-lisp-user::Garnet-Readtable)
    (when verbose (format t "copied.~%")))

  (progn
    (when verbose (format t "Disconnecting Garnet..."))
    (opal:disconnect-garnet)
    (when verbose (format t "disconnected.~%")))

  (setf garnet-image-date (time-to-string))

  ;; RGA --- kills extra source file info, which is not useful when
  ;; making a portable image.
  #+allegro
  (when flush-source-info?
    (format t "Flushing source file info . . .")
    (excl:discard-all-source-file-info)
    (format t "Fssssh-gurgle-hisss!~%"))

  #+(or allegro cmu ccl sbcl)
  (when gc
    (when verbose (format t "Garbage collecting..."))
    #+allegro (excl:gc T)
    #+(and cmu (not gencgc)) (ext:gc T)
    #+(and cmu gencgc) (ext:gc :full t)
    #+sbcl (sb-ext:gc :full t)
    #+ccl
    (progn
      (ccl::impurify)
      (ccl:gc)
      (ccl::purify))

    (when verbose (format t "collected.~%")))

  (defparameter common-lisp-user::*herald-items* nil)
  (setf (getf common-lisp-user::*herald-items* :garnet)
	`("    Garnet Version " ,common-lisp-user::Garnet-Version-Number))


  (when verbose (format t "Saving image..."))
  #+allegro (setq excl:*read-init-files* t)
  #+allegro (setq excl:*restart-init-function* #'garnet-restart-function)
  #+allegro
  (apply #'excl:dumplisp :name filename :checkpoint NIL extra-args)
  #+cmu
  (progn
    (setf (getf ext:*herald-items* :garnet)
	  `("    Garnet Version " ,common-lisp-user::Garnet-Version-Number))
    ;; Note: for x86/mp CMUCL, garnet-restart-function must get
    ;; called after the multiprocessing stuff gets initialized.
    ;; So we append garnet-restart-function to the end of the
    ;; initializations rather than pushing it onto the front.
    (setf ext:*after-save-initializations*
 	  (append  ext:*after-save-initializations* (list #'garnet-restart-function)))
    (apply #'ext:save-lisp filename extra-args))

  #+ccl
  (progn
    (pushnew #'garnet-restart-function ccl:*lisp-startup-functions*)
    (apply #'ccl:save-application filename extra-args))


  #+sbcl
  (progn
;;    (pushnew #'garnet-restart-function sb-ext:*init-hooks*)
    (setf sb-ext:*init-hooks*
	  (append sb-ext:*init-hooks* (list #'garnet-restart-function)))
    (apply #'sb-ext:save-lisp-and-die filename extra-args))

  (when verbose (format t "saved.~%"))

  #-sbcl ;; SBCL quits automatically.
  (cond
    (quit
     (when verbose (format t "Quitting lisp...~%"))
     #+allegro (excl:exit)
     #+cmu (ext:quit)
     )
    (t
     (when verbose (format t "Reconnecting Garnet..."))
     (opal:reconnect-garnet)
     (when verbose (format t "reconnected.~%"))
     ))
  ))

(defun Get-Garnet-Bitmap (bitmapname)
  (opal:read-image (merge-pathnames bitmapname cl-user::Garnet-Bitmap-PathName)))


;;;============================================================
;;; Clip-and-Map
;;;============================================================

;;; The Clip-and-Map procedure works as follows:
;;;    (Clip-and-Map (val val-1 val-2 target-val-1 target-val-2) takes val,
;;;    clips it to be in the range val-1 .. val-2, and if target-val-1 and
;;;    target-val-2 are provided, then scales and
;;;    translates the value (using linear-interpolation) to be between
;;;    target-val-1 and target-val-2.  Unless target-val-1 and target-val-2
;;;    are both integers, the mapping will be to a float.
;;; Val-1 is allowed to be less than or greater than Val-2.
;;;
(defun Clip-and-Map (val val-1 val-2 &optional target-val-1 target-val-2)
  (if (and target-val-1 target-val-2)
      ;; then do clip and map
      (if (eq val-1 val-2)
	  (cond ((< val val-1) target-val-1)
		(t target-val-2))
	  (cond ((< val val-1 val-2) target-val-1)
		((< val-1 val-2 val) target-val-2)
		((< val val-2 val-1) target-val-2)
		((< val-2 val-1 val) target-val-1)
		(t (+ target-val-1
		      (if (and (integerp target-val-1) (integerp target-val-2))
			  ; integer targets
			  (round (* (- val val-1)
				    (- target-val-2 target-val-1))
				 (- val-2 val-1))
		          ; float targets
			  (/ (* (- val val-1) (- target-val-2 target-val-1))
			     (- val-2 val-1)))))))

      ;; else, just do clip (no map)
      (cond ((< val val-1 val-2) val-1)
	    ((< val-1 val-2 val) val-2)
	    ((< val val-2 val-1) val-2)
	    ((< val-2 val-1 val) val-1)
	    ; now make sure that return value is integer if val-1 and val-2
	    ; are both integers (this comes in real handy sometimes)
	    (t (if (and (integerp val-1) (integerp val-2))
		   (round val) val)))))
