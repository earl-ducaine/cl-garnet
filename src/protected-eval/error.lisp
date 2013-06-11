;;; -*- Mode: COMMON-LISP; Package: GARNET-GADGETS -*-               ;;
;;-------------------------------------------------------------------;;
;;            Copyright 1993 Russell G. Almond                       ;;
;;-------------------------------------------------------------------;;
;; This code is in the Public Domain.  Anyone who can get some use   ;;
;; from it is welcome.                                               ;;
;; This code comes with no warranty.                                 ;;
;;-------------------------------------------------------------------;;

;;; $Id$
;;



;;;
;;  (do-abort &body <forms>)
;; 
;;  When I started writing this I rather assumed that there would
;;  always be a restart named abort which would restart you at the top
;;  level.  Thus calling (abort) would get you back to the top level
;;  if nothing else was done.  This was rather naive on my part.
;;  Therefore I've implemented the function do-abort which works as
;;  follows:  (1) If there is a restart named abort, use it.  (2) If
;;  not, do something system dependent which will return you to the
;;  top level, or the main event loop or whatever.  (3) If you can't
;;  figure out how to do that, call (abort) anyway and generate an
;;  error.  
;;  I've put in a patch which works for Allegro 4.1.  You'll need to
;;  fix this for other versions of lisp.

;;  (with-abort &body <forms>)
;;
;;  Establishes a specific abort restart, which skips over any
;;  unevaluated forms and returns the values (nil :abort). 
;; 

(in-package "GARNET-GADGETS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(do-abort with-abort)))


;;; Abort Restarts
;;

(defun do-abort ()
  "Never Exits!  This turns control over to the restart abort.  If
not, it tries to return control to the top level.  This will in
general be system dependent and need to be hacked."
  (if (find-restart 'abort) (abort)	;Worked!
    #+allegro
    (top-level:do-command :reset)
    #-allegro
    ;; this will generate error which means you need to find
    ;; a solution for your lisp.
    (abort)
    ))


(defmacro with-abort (&body forms)
  "Executes forms in an environment in which there exists an abort
restart.  The abort restart returns two values, nil and :abort"
  (let ((abort-block-tag (gensym "ABORT")))
    `(block ,abort-block-tag
       (restart-case (progn ,.forms)
       (abort () :report "Abandon Computation, Return nil"
       (return-from ,abort-block-tag (values nil :abort)))))))





