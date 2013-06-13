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

;;; CHANGE LOG:
;; 
;;  29/05/01 gilham   - Fix for cmucl 18 removing cmu17 from *features*.
;;  12/06/94 haible   - Referenced :SYSTEM package in CLISP instructions
;;  12/06/94 amickish - Added Russell Almond's libfile and flush-source-info?
;;             parameters to opal:make-image
;;  05/25/94 amickish - Bound and closed error-stream in opal:shell-exec
;;  05/05/94 amickish - Added Mac version of Directory-P
;;  01/16/94 amickish - Added :init-file argument to Mac's make-image command
;;  01/12/94 amickish - Added Drawable-To-Window
;;  01/08/94 amickish - Added Clip-And-Map (from movegrowinter.lisp)
;;  12/14/93 amickish - Added Mac version of Make-Image
;;  10/18/93 dzg - replaced 'string-char (which is obsolete in CLtL 2) with
;; 	       'character
;;  10/03/93 amickish - Added Extract-Image-Args so that opal:Make-Image can
;;             take arbitrary arguments
;;  09/30/93 amickish - Added Bruno Haible's industrial-strength DIRECTORY-P.
;;  09/22/93 amickish - In opal:make-image, (1) ignored gc for LispWorks
;;             and CLISP, (2) only copied readtable for Allegro
;;  09/20/93 amickish - Called system:os-wait for Allegro in opal:shell-exec
;;  09/06/93 amickish - Changed opal:directory-p's command-string to use
;;             TEST -d; Changed shell for opal:shell-exec to /bin/sh
;;  09/03/93 Bruno Haible - Added #+clisp switches
;;  08/17/93 amickish - Removed redundant "csh" from directory-p; added
;;             lispworks switches for opal:make-image
;;  08/15/93 rajan - Added directory-p
;;  08/13/93 amickish - When saving Allegro image, *do* read init file;
;;             copied *readtable* into user::Garnet-Readtable and used
;;             value in excl:*cl-default-special-bindings*
;;  05/04/93 amickish - Removed "total" GC for Lucid in opal:make-image
;;  04/22/93 amickish - Added Get-Garnet-Bitmap
;;  04/20/93 amickish - Added GC option to make-image
;;  03/30/93 amickish - Added RETURN-FROM in make-image to jump out of save
;;             function in CMUCL when restarting
;;  03/05/93 amickish - Created with shell-exec and make-image

(in-package "COMMON-LISP")

(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(shell-exec make-image get-garnet-bitmap directory-p
            time-to-string clip-and-map drawable-to-window)))

(defvar garnet-image-date NIL)

;; The purpose of opal:drawable-to-window is to allow external references to
;; the function gem:window-from-drawable, without having to always explicitly
;; reference the DEVICE-INFO object.
(defun drawable-to-window (device-drawable)
  (gem:window-from-drawable (g-value DEVICE-INFO :current-root)
			    device-drawable))

(defun shell-exec (command)
  ;; Alas, can't use with-open-file because there are two streams returned
  ;; by most of the lisp-specific commands.  Must close both streams.
  (multiple-value-bind (the-stream error-stream)
      #+allegro (excl:run-shell-command command :wait NIL :output :stream
					:error-output :stream)
      #+sbcl
      (let ((process
	     (sb-ext:run-program "/bin/sh" (list "-c" command)
				 :wait t :output :stream
				 :error :stream)))
	(values (sb-ext:process-output process)
		(sb-ext:process-error process)))
      #+cmu
      (let ((p
	     (ext:run-program "/bin/sh" (list "-c" command)
			  :wait NIL :output :stream :error :stream)))
	(values (ext:process-output p) (ext:process-error p)))
      #+ccl
      (let ((p
	     (ccl:run-program "/bin/sh" (list "-c" command)
			      :wait NIL :output :stream :error :stream)))
	(values (ccl:external-process-output-stream p)
		(ccl:external-process-error-stream p)))
      
      #-(or allegro cmu ccl sbcl)
      (error "Don't know how to execute shell functions in this lisp")
      
      (let ((output-string (make-array '(0)
				       :element-type 'character
				       :fill-pointer 0 :adjustable T)))
	(do ((next-char (read-char the-stream NIL :eof)
			(read-char the-stream NIL :eof)))
	    ((eq next-char :eof)
	     (close the-stream)
	     (if (streamp error-stream) (close error-stream))
	     #+allegro (system:os-wait))
	  (vector-push-extend next-char output-string))
	output-string)))


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


#-cmu
(defun garnet-restart-function ()
  (format t "*** Restarting Garnet ~A image created with opal:make-image ***~%"
	  common-lisp-user::Garnet-Version-Number)
  (if (boundp 'garnet-image-date)
      (format t "*** Image creation date: ~A ***~%" garnet-image-date))
  (opal:reconnect-garnet))


#+cmu
(defun garnet-restart-function ()
  (format t "*** Restarting Garnet ~A image created on ~A ***~%"
	  common-lisp-user::Garnet-Version-Number
	  garnet-image-date)
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
    (if verbose (format t "~%Copying readtable..."))
    (copy-readtable *readtable* common-lisp-user::Garnet-Readtable)
    (setf (cdr (assoc '*readtable* excl:*cl-default-special-bindings*))
          'common-lisp-user::Garnet-Readtable)
    (if verbose (format t "copied.~%")))

  (progn
    (if verbose (format t "Disconnecting Garnet..."))
    (opal:disconnect-garnet)
    (if verbose (format t "disconnected.~%")))

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
    (if verbose (format t "Garbage collecting..."))
    #+allegro (excl:gc T)
    #+(and cmu (not gencgc)) (ext:gc T)
    #+(and cmu gencgc) (ext:gc :full t)
    #+sbcl (sb-ext:gc :full t)
    #+ccl
    (progn
      (ccl::impurify)
      (ccl:gc)
      (ccl::purify))

    (if verbose (format t "collected.~%")))

  (if verbose (format t "Saving image..."))
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
    (pushnew #'garnet-restart-function sb-ext:*init-hooks*)
    (apply #'sb-ext:save-lisp-and-die filename extra-args))
  
  (if verbose (format t "saved.~%"))

  #-sbcl ;; SBCL quits automatically.
  (cond
    (quit
     (if verbose (format t "Quitting lisp...~%"))
     #+allegro (excl:exit)
     #+cmu (ext:quit)
     )
    (t
     (if verbose (format t "Reconnecting Garnet..."))
     (opal:reconnect-garnet)
     (if verbose (format t "reconnected.~%"))
     ))
  ))

(defun Get-Garnet-Bitmap (bitmapname)
  (opal:read-image (merge-pathnames bitmapname cl-user::Garnet-Bitmap-PathName)))

;; If the -d test is true, shell-exec returns "1".  Otherwise, it returns "".
;; This syntax works for all kinds of Unix shells: sh, csh, ksh, tcsh, ...
;;
(defun directory-p (pathname)
  #+(or sbcl allegro)
  ;; 1. Needn't call a shell if we can do the test ourselves.
  ;; 2. In case pathname contains Latin-1 characters. clisp is 8 bit clean,
  ;;    while most Unix shells aren't.
  (garnet-utils:probe-directory pathname)

  #-(or sbcl allegro)
  ;; command-string is the string that's going to be executed.
  (let ((command-string
	 (concatenate 'string "test -d " pathname " && echo 1")))
    (unless (equal "" (shell-exec command-string))
	    T))
)


;; This is an industrial-strength version of opal:directory-p.  The difference
;; is that extra work is done to ensure that single and double quotes are
;; passed to the shell correctly.  Since it does more work, only use this
;; version if you find you really need it.  This code was contributed by
;; Bruno Haible.
#+comment
(defun directory-p (pathname)
  ;; Must quote the pathname since Unix shells interpret characters like
  ;; #\Space, #\', #\<, #\>, #\$ etc. in a special way. This kind of quoting
  ;; should work unless the pathname contains #\Newline and we call csh.
  (flet ((shell-quote (string) ; surround a string by single quotes
	   (let ((qchar nil) ; last quote character: nil or #\' or #\"
		 (qstring (make-array 10 :element-type 'character
				      :adjustable t :fill-pointer 0)))
	     (map nil #'(lambda (c)
			  (let ((q (if (eql c #\') #\" #\')))
			    (unless (eql qchar q)
			      (when qchar (vector-push-extend qchar qstring))
			      (vector-push-extend (setq qchar q) qstring))
			    (vector-push-extend c qstring)))
		  string)
	     (when qchar (vector-push-extend qchar qstring))
	     qstring)))
    ;; command-string is the string that's going to be executed.
    (let ((command-string
	   (concatenate 'string "test -d " (shell-quote pathname) " && echo 1")))
      (unless (equal "" (shell-exec command-string))
	T))))
		   

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
