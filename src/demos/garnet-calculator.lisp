;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; $Id$


;;;  Garnet Calculator (based on xcalc) - by David S. Kosbie
;;;
;;;



;;;  Part I  -  k-framed-text
;;

;;  A simple framed text object for a text button. Far less advanced,
;;  but quicker, than the gadget equivalent. Basically just a
;;  RECTANGLE with a few patches.
;;
;;  NOTE: Do NOT let the size of the string be larger than the size
;;  of the frame!
;;
;;  Same slots as a RECTANGLE, but also the following:
;;     :string      "none"
;;     :font        default-font
;;
;;	:text-width  <formula>
;;	:text-height <formula>
;;
;;  Plus some internal slots (see defn below).
;;

(in-package "OPAL")

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(HALF K-FRAMED-TEXT)))
(defmacro half (n)
  `(truncate ,n 2))

(create-instance 'k-framed-text rectangle
  :declare ((:update-slots :visible :fast-redraw-p :top :left :width :height
			   :line-style :filling-style :draw-function
			   :string :font)
	    (:ignored-slots :depended-slots :update-slots :update-slots-values
			    :xfont :text-extents))
    (:string        "none")
    (:font          default-font)

  ;; Do not change the following line!
    (:actual-height-p NIL)
  )

(define-method :draw opal:k-framed-text (gob a-window)
  (let* ((update-vals (g-local-value gob :update-slots-values))
	 (left (aref update-vals +rect-left+))
	 (top (aref update-vals +rect-top+))
	 (width (aref update-vals +rect-width+))
	 (height (aref update-vals +rect-height+))
	 (lstyle (aref update-vals +rect-lstyle+))
	 (min-width-height (min width height))
	 (draw-fn (aref update-vals +rect-draw-function+))
	 (rect-fstyle (aref update-vals +rect-fstyle+))
	 (thickness (get-old-thickness gob +rect-lstyle+ update-vals))

         (font (aref update-vals +kft-font+))
         (string (aref update-vals +kft-string+))
	 (ascent (gem:max-character-ascent a-window font))
	 (text-height (+ ascent (gem:max-character-descent a-window font)))
         (text-left-offset (half (- width
				    (gem:text-width a-window font string))))
         (text-top-offset  (half (- height text-height)))
	 )

    (when (plusp min-width-height)  ; only draw if width, height > 0
      (if (>= (* 2 thickness) min-width-height) ; if rectangle too small,
	                                        ; just draw solid rectangle
	  (gem:draw-rectangle a-window left top width height draw-fn
			      NIL opal:black-fill)
	  (progn
	    (gem:draw-rectangle a-window left top width height draw-fn
				lstyle rect-fstyle)
	    (if (or (minusp text-left-offset) (minusp text-top-offset))
                (format t "Warning:  string larger than frame in object ~A~%"
			gob))
	    (gem:draw-text a-window
			   (+ left text-left-offset)
			   (+ top text-top-offset ascent)
			   string font draw-fn lstyle NIL))))))



;;;
;;;  Part II  -  The Calculator
;;;

;;; This file implements a floating point calculator based mostly on "xcalc".
;;; Some distinctions:
;;;
;;;	- gcalc has 10 memories instead of 1 -- eg, store with <STO><DIGIT>.
;;;
;;;	- gcalc can be driven in 3 ways:
;;;
;;;		- with the mouse (of course)
;;;		- with keyboard equivalents (hit the <KEYS> button to see map)
;;;		- with Lisp functions
;;;			- with the name #'<keyname>-BUTTON (eg, "X^2-BUTTON")
;;;			- taking 1 argument, the "app-object"
;;;
;;;	- gcalc is a little slower, but not grossly so
;;;
;;;	- gcalc has a less buggy expression parser
;;;		- try "2+3*4^5+6" on each -- xcalc gets this wrong!

;;; Some notes on implementation:
;;;
;;;	- gcalc is designed to help with research into KATIE, and so
;;;		contains code which may not be considered good
;;;		'Garnet-style' programming (eg, there are no constraints!).
;;;		However, gcalc runs just fine in an unaltered Garnet.
;;;
;;;	- gcalc uses no globals!  You can run multiple gcalc's simultaneously!
;;;
;;;	- thus, you cannot just call do-stop!  Instead:
;;;
;;;		- when you call do-go, you are returned an "app-object"
;;;		- you must supply this "app-object" as an argument to do-stop
;;;		- if you forget to save this object, use "(gd:ident)" to
;;;		  get the name of the app window, then you use
;;;		  "(kr:g-value <app-window> :app-object)"
;;;
;;;	- gcalc uses the special Opal object "k-framed-text" defined above 
;;;
;;;		- Clearly, this object must be loaded before running gcalc


(in-package :GARNET-CALCULATOR)

(defvar GARNET-CALCULATOR-INIT
  (unless (get :garnet-modules :error-gadget-utils)
    (common-lisp-user::garnet-load "gadgets:error-gadget-utils")))

(defvar *Demo-App-Obj* NIL)

(defconstant +main-win-top+      10)
(defconstant +main-win-left+    700)
(defconstant +main-win-width+   235)
(defconstant +main-win-height+  245)
(defvar      *main-win-color*   opal:motif-green)

(defconstant +frame-top+              5)
(defconstant +frame-left+             10)
(defconstant +frame-width+            158)
(defconstant +frame-height+           45)
(defvar      *frame-line-style*       opal:line-4)
(defconstant +frame-filling-style+    opal:motif-gray-fill)
(defconstant +result-top+             5)
(defconstant +result-right+           150)
(defvar      *result-font*            (opal:get-standard-font
					:fixed :bold :large))
(defconstant +drg-left+               10)
(defconstant +drg-top+                30)

(defconstant +cut-paste-width+        44)

(defconstant +button-top+            60)
(defconstant +button-left+           10)
(defconstant +button-width+          30)
(defconstant +button-height+         20)
(defconstant +button-h-spacing+       4)
(defconstant +button-v-spacing+       3)
(defvar      *button-filling-style*  opal:motif-gray-fill)
(defvar      *button-line-style*     opal:default-line-style)
(defvar      *button-font*           opal:default-font)

(defvar      *greeting-font*         (opal:get-standard-font
                                      :serif :italic :very-large))
(defconstant +greeting+
"

The Garnet Calculator

by

David S. Kosbie")


(defconstant +cut-paste-column+
       '(       ("Copy"  #\X    copy-button)
	        ("Paste" #\P    paste-button)
	        ("Keys"  #\K    keys-button)
	        ("Quit"  #\Q    quit-button) )  )

(defconstant +button-list+		 
  '(    (	("1/x"  #\i    1/x-button)
		("INV"  #\I    inv-button)
		("e"    #\e    e-button)
		("pi"   #\p    pi-button)
		("STO"  #\S    sto-button)
		("RCL"  #\R    rcl-button)
		("SUM"  #\$    sum-button)
		("EXC"  #\>    exc-button)	)

	(	("x^2"  #\@    x^2-button)
		("sin"  #\s    sin-button)
		("EE"   #\E    ee-button)
		("x!"   #\!    x!-button)
		("7"    #\7    7-button)
		("4"    #\4    4-button)
		("1"    #\1    1-button)
		("0"    #\0    0-button)	)

	(	("sqr"  #\q    sqr-button)
		("cos"  #\c    cos-button)
		("log"  #\l    log-button)
		("("    #\(    left-paren-button);))
		("8"    #\8    8-button)
		("5"    #\5    5-button)
		("2"    #\2    2-button)
		("."    #\.    .-button)	)

	(	("CE"   #\C    ce-button)
		("tan"  #\t    tan-button)
		("LN"   #\L    ln-button);((
		(")"    #\)    right-paren-button)
		("9"    #\9    9-button)
		("6"    #\6    6-button)
		("3"    #\3    3-button)
		("+/-"  #\~    +/--button)	)

	(	("AC"   #\A    ac-button)
		("DRG"  #\D    drg-button)
		("y^x"  #\^    y^x-button)
		("/"    #\/    /-button)
		("*"    #\*    *-button)
		("-"    #\-    --button)
		("+"    #\+    +-button)
		("="    #\=    =-button)	)	))

(defvar my-/ NIL)

(defun my-/ (a b) (if (zerop b) :error (/ a b)))

(defconstant ++-op-spec+ (cons #'+     1))
(defconstant +--op-spec+ (cons #'-     1))
(defconstant +*-op-spec+ (cons #'*     2))
(defconstant +/-op-spec+ (cons 'my-/  2))
(defconstant +^-op-spec+ (cons #'expt  3))


                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;;;;;; Support Functions  ;;;;;;;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro unnamed-instance (&rest body)
  `(kr:create-instance NIL ,@body))

(defun do-update (app-object &optional result-object)
  (let ((result (or result-object (kr:g-value app-object :result))))
    (kr:s-value result :left (- +result-right+ (kr:g-value result :width)))
    (opal:update (kr:g-value app-object :frame-window))))

(defun do-error (app-object)
 (let ((result (kr:g-value app-object :result)))
  (kr:s-value result :string "error")
  (do-update app-object result)
  :error))

(defun set-result (app-object new-value)
 (if (eq new-value :error)
     (return-from set-result (do-error app-object)))
 (let ((result (kr:g-value app-object :result)))
   (if (eql new-value 0)
       ;; amickish - 8/11/93 - optimize for the case where new-value = 0
       (kr:s-value result :string (format NIL "~D" new-value))
       (let* ((trunc  (truncate new-value))
	      (log    (log (abs new-value) 10))
	      (abslog (abs log)))
	 (cond ((and (= new-value trunc) (or (zerop trunc) (< abslog 10)))
		(setq new-value trunc)
		(kr:s-value result :string (format NIL "~D" new-value)))
	       ((< abslog 7)
		(kr:s-value result :string (format NIL "~13F" new-value)))
	       (T ;; exponent form
		(let ((minus? (minusp new-value)))
		  (setq new-value (abs new-value))
		  (multiple-value-bind (expt mantissa-expt) (truncate log)
		    (when (minusp mantissa-expt)
		      (decf expt 1)
		      (incf mantissa-expt 1.0))
		    (kr:s-value result :string
				(concatenate 'string
				   (if minus? "-" "")
				   (format NIL "~8F" (expt 10 mantissa-expt))
				   "E"
				   (if (minusp expt) "" "+")
				   (format NIL "~D" expt)))))))))
   (do-update app-object result)
   new-value))

(defun get-result (app-object)
  (let ((result-value (read-from-string
			(kr:g-value app-object :result :string))))
    (if (numberp result-value)
	result-value
	0)))

(defun get-radians-result (app-object)
  (let ((result-value (get-result app-object))
	(drg-value    (kr:g-value app-object :drg :string)))
    (cond ((string= drg-value "RAD") result-value)
	  ((string= drg-value "DEG") (* (/ result-value 180) pi))
	  (T ;; GRADS
	   (* (/ result-value 200) pi)))))

(defun add-to-end (app-object new-string)
 (let ((result (kr:g-value app-object :result)))
  (unless (stringp new-string)
    (setq new-string (princ-to-string new-string)))
  (kr:s-value result :string
	   (concatenate 'string
			(kr:g-value result :string)
			new-string))
  (do-update app-object result)))
  
(defun do-number (app-object digit)
 (let ((result-string (kr:g-value app-object :result :string))
       (memories      (kr:g-value app-object :memories))
       (prev-key      (kr:g-value app-object :prev-key)))
  (kr:s-value app-object :prev-key (code-char (+ (char-code #\0) digit)))
  (case prev-key
    (#\S (setf (aref memories digit) (get-result app-object)))
    (#\R (set-result app-object (aref memories digit)))
    (#\$ (incf (aref memories digit) (get-result app-object)))
    (#\> (let ((new-value (aref memories digit)))
		(setf (aref memories digit) (get-result app-object))
		(set-result app-object new-value)))
    (T
      (if (kr:g-value app-object :new-number?)
        (progn
	    (set-result app-object digit)
	    (kr:s-value app-object :new-number? NIL))
        (if (string= result-string "0")
            (set-result app-object digit)
            (add-to-end app-object digit)))))))
	 
(defun convert-to-drg (app-object number)
 (if (complexp number)
  :error
  (let ((drg-string (kr:g-value app-object :drg :string)))
   (cond ((string= drg-string "DEG") (/ (* 180 number) pi))
         ((string= drg-string "GRAD") (/ (* 200 number) pi))
         (T number)))))

(defmacro careful-set-result (app-object new-value)
  `(or (gg:careful-eval (set-result ,app-object ,new-value))
       (set-result ,app-object  :error)))

                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;;;;;; Button Functions  ;;;;;;;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun 1/X-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\i)
  (careful-set-result app-object (/ 1.0 (get-result app-object))))

(defun INV-BUTTON (app-object)
  (kr:s-value app-object :prev-key #\I))

(defun E-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\e)
  (set-result app-object  2.7182818284))

(defun PI-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\p)
  (set-result app-object 3.1415926535))

(defun STO-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\S))
(defun RCL-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\R))
(defun SUM-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\$))
(defun EXC-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\>))

(defun X^2-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\@)
  (let ((result-value (get-result app-object)))
    (careful-set-result app-object (* result-value result-value))))

(defun SIN-BUTTON (app-object)
 (let ((prev-key (kr:g-value app-object :prev-key))
       (result (get-result app-object)))
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\s)
  (careful-set-result app-object
    (cond ((eq prev-key #\I)
           (convert-to-drg app-object (asin (get-result app-object))))
          ((zerop result) 
           0)
          (t
           (sin (get-radians-result app-object)))))))

(defun EE-BUTTON (app-object)
  (kr:s-value app-object :prev-key #\E)
  (unless (position #\E (kr:g-value app-object :result :string))
    (add-to-end app-object "E+")))

(defun X!-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\!)
  (let* ((result-value (get-result app-object))
	 (new-value    1))
    (if (or (not (integerp result-value))
	    (minusp result-value)
	    (> result-value 60))
	(setq new-value :error)
	(dotimes (index result-value)
	    (setq new-value (* new-value (1+ index)))))
    (set-result app-object new-value)))

(defun 9-BUTTON (app-object) (do-number app-object 9))
(defun 8-BUTTON (app-object) (do-number app-object 8))
(defun 7-BUTTON (app-object) (do-number app-object 7))
(defun 6-BUTTON (app-object) (do-number app-object 6))
(defun 5-BUTTON (app-object) (do-number app-object 5))
(defun 4-BUTTON (app-object) (do-number app-object 4))
(defun 3-BUTTON (app-object) (do-number app-object 3))
(defun 2-BUTTON (app-object) (do-number app-object 2))
(defun 1-BUTTON (app-object) (do-number app-object 1))
(defun 0-BUTTON (app-object) (do-number app-object 0))

(defun SQR-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\q)
  (careful-set-result app-object (sqrt (get-result app-object))))

(defun COS-BUTTON (app-object)
 (let ((prev-key (kr:g-value app-object :prev-key)))
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\c)
  (careful-set-result app-object
    (if (eq prev-key #\I)
      (convert-to-drg app-object (acos (get-result app-object)))
      (cos (get-radians-result app-object))))))

(defun LOG-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\l)
  (let ((result (get-result app-object)))
    (cond ((zerop result)
	   (do-error app-object))
	  ((= result 1) 0)
          (t
           (careful-set-result app-object (log (get-result app-object) 10))))
  ))

(defun LEFT-PAREN-BUTTON (app-object)
 (let ((stack (kr:g-value app-object :stack)))
  (unless (null stack)
    (kr:s-value app-object :stack NIL)
    (kr:s-value app-object :stack-stack
       (cons stack (kr:g-value app-object :stack-stack))))
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\())) ;; )

(defun .-BUTTON (app-object)
  (kr:s-value app-object :prev-key #\.)
  (when (kr:g-value app-object :new-number?)
    (set-result app-object 0)
    (kr:s-value app-object :new-number? NIL))
  (let ((result-string (kr:g-value app-object :result :string)))
    (unless (or (position #\. result-string)
		(position #\E result-string))
      (add-to-end app-object "."))))

(defun CE-BUTTON (app-object)
  (kr:s-value app-object :prev-key #\C)
  (kr:s-value app-object :new-number? T)
  (set-result app-object 0))

(defun TAN-BUTTON (app-object)
 (let ((prev-key (kr:g-value app-object :prev-key)) 
       (result (get-result app-object)))
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\t)
  (set-result app-object
    (cond ((eq prev-key #\I)
           (convert-to-drg app-object (atan (get-result app-object))))
          ((zerop result) 
           0)
          (t
           (tan (get-radians-result app-object)))))))

(defun LN-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\L)
  (let ((result (get-result app-object)))
    (cond ((zerop result)
	   (do-error app-object))
	  ((= result 1) 0)
          (t
           (careful-set-result app-object (log (get-result app-object))))))
)

 ;; (
(defun RIGHT-PAREN-BUTTON (app-object)
 (let ((stack-stack (kr:g-value app-object :stack-stack)))
  (when stack-stack
    (eval-stack app-object 0)
    (kr:s-value app-object :stack (first stack-stack))
    (kr:s-value app-object :stack-stack (rest stack-stack)))
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\))))

(defun +/--BUTTON (app-object)
  (kr:s-value app-object :prev-key #\~)
  (let* ((result        (kr:g-value app-object :result))
         (result-string (kr:g-value result :string))
	 (E-position    (position #\E result-string)))
    (if E-position
	(progn
	  (setq result-string (copy-seq result-string))
	  (setf (elt result-string (1+ E-position))
		(if (eq (elt result-string (1+ E-position))
			#\+)
		    #\-
		    #\+))
	  (kr:s-value result :string result-string)
	  (do-update app-object result))
	(careful-set-result app-object (- (get-result app-object))))))

(defun AC-BUTTON (app-object)
  (kr:s-value app-object :prev-key #\A)
  (set-result app-object 0)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :stack NIL)
  (kr:s-value app-object :stack-stack NIL))

(defun DRG-BUTTON (app-object)
  (kr:s-value app-object :prev-key #\D)
  (let* ((drg        (kr:g-value app-object :drg))
         (old-string (kr:g-value drg :string)))
    (kr:s-value drg :string
	     (cond ((string= old-string "DEG") "RAD")
		   ((string= old-string "RAD") "GRAD")
		   (T "DEG")))
    (do-update app-object)))

;; stop-point is a precedence, the CDR part of an op-spec
(defun eval-stack (app-object stop-point)
  (prog ((stack (kr:g-value app-object :stack))
         prev-op prev-arg return-val popped-one?)
top-tag
    (setq prev-op (first stack))
    (when (and prev-op
	       (or (consp prev-op) (error "Invalid op on stack: ~A" stack))
	       (>= (cdr prev-op) stop-point))
	(unless (numberp (setq prev-arg (second stack)))
	  (error "Invalid number on stack: ~A" stack))
	(setq return-val
	  (careful-set-result app-object
	    (funcall (car prev-op) prev-arg (get-result app-object))))
	(setq popped-one? T)
	(setq stack (cddr stack))
	(go top-tag))
    (if popped-one? (kr:s-value app-object :stack stack))
    (return return-val)))

(defun do-binary-op (app-object prev-key op-spec)
  (eval-stack app-object (cdr op-spec))
  (kr:s-value app-object :stack
    (cons op-spec
          (cons (get-result app-object)
                (kr:g-value app-object :stack))))
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key prev-key))

(defun Y^X-BUTTON (app-object) (do-binary-op app-object #\^ +^-op-spec+))
(defun /-BUTTON (app-object)   (do-binary-op app-object #\/ +/-op-spec+))
(defun *-BUTTON (app-object)   (do-binary-op app-object #\* +*-op-spec+))
(defun --BUTTON (app-object)   (do-binary-op app-object #\- +--op-spec+))
(defun +-BUTTON (app-object)   (do-binary-op app-object #\+ ++-op-spec+))

(defun =-BUTTON (app-object)
  (eval-stack app-object 0)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\=))

(defun COPY-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\X)
  (let ((a-window (kr:g-value app-object :main-window)))
    (gem:set-cut-buffer a-window (kr:g-value app-object :result :string))
    (gem:flush-output a-window)))

(defun PASTE-BUTTON (app-object)
  (kr:s-value app-object :new-number? T)
  (kr:s-value app-object :prev-key #\P)
  (let* ((a-window (kr:g-value app-object :main-window))
	 (new-value (gg:careful-read-from-string
		     (gem:get-cut-buffer a-window))))
    (unless (numberp new-value) (setq new-value 0))
    (careful-set-result app-object new-value)))

(defun KEYS-BUTTON (app-object)
 (kr:s-value app-object :prev-key #\K)
 (let* ((main-agg (kr:g-value app-object :main-window :aggregate))
        (slot (if (kr:s-value main-agg :keys?
			(not (kr:g-value main-agg :keys?)))
                  :hot-key
                  :key-name)))
  (opal:do-all-components main-agg
	#'(lambda (object) (kr:s-value object :string
				(kr:g-value object slot))))
  (opal:update (kr:g-value app-object :main-window) T)))

(defun QUIT-BUTTON (app-object)
  (stop-calc app-object))

                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;;;;;; THE USER INTERFACE ;;;;;;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-button-column (app-object main-agg top left column proto)
  (let ((agg       (unnamed-instance opal:aggregate))
        (hashtable (kr:g-value app-object :key-hashtable))
        key-name hot-key dispatch-fn)
    (dolist (button-spec column)
      (setq key-name    (first button-spec))
      (setq hot-key     (second button-spec))
      (setq dispatch-fn (third button-spec))
      (opal:add-component agg (unnamed-instance proto
			   (:top         top)
			   (:left        left)
			   (:string      key-name)
			   (:key-name    key-name)
			   (:hot-key     (princ-to-string hot-key))
			   (:dispatch-fn dispatch-fn)))
      (setf (gethash hot-key hashtable) dispatch-fn)
      (incf top (+ +button-height+ +button-v-spacing+)))
    (opal:add-component main-agg agg)))

(defun setup-main-window (app-object double-buffered-p)
  (let ((main-window
	 (unnamed-instance inter:interactor-window
			   (:app-object       app-object)
			   (:top              +main-win-top+)
			   (:left             +main-win-left+)
			   (:width            +main-win-width+)
			   (:height           +main-win-height+)
			   (:title            "Garnet Calculator")
			   (:icon-title       "gcalc")
			   (:background-color *main-win-color*)
			   (:double-buffered-p double-buffered-p)))
	(main-agg (unnamed-instance opal:aggregate))
	greeting frame-agg proto-button proto-cut-paste-button)

    ;; If we get clobbered by the window manager, let the demos
    ;; controller know (if it's there).
    (when (fboundp 'common-lisp-user::Garnet-Note-Quitted)
      (pushnew
       #'(lambda (win)
	   (declare (ignore win))
	   (common-lisp-user::Garnet-Note-Quitted "GARNET-CALCULATOR"))
       (g-value main-window :destroy-hooks)))
   

    (kr:s-value main-window :aggregate main-agg)
    (kr:s-value app-object :main-window main-window)

    (setq greeting
	  (unnamed-instance opal:multi-text
			    (:string        +greeting+)
			    (:justification :center)
			    (:font          *greeting-font*)
			    (:top           (opal:half (- +main-win-height+
							  (opal:string-height
							   *greeting-font* +greeting+))))
			    (:left          (opal:half (- +main-win-width+
							  (opal:string-width
							   *greeting-font* +greeting+))))))

    (opal:add-component main-agg greeting)

    (opal:update main-window)

    (setq frame-agg (unnamed-instance opal:aggregate))

    (kr:s-value app-object :frame-window
		(unnamed-instance inter:interactor-window
				  (:left          +frame-left+)
				  (:top           +frame-top+)
				  (:width         +frame-width+)
				  (:height        +frame-height+)
				  (:border-width  4)
				  (:aggregate     frame-agg)
				  (:parent        main-window)
				  (:double-buffered-p double-buffered-p)))

    (opal:add-components frame-agg

			 (kr:s-value app-object :result
				     (unnamed-instance opal:text
						       (:top        +result-top+)
						       (:line-style *frame-line-style*)
						       (:font       *result-font*)))

			 (kr:s-value app-object :drg
				     (unnamed-instance opal:text
						       (:top        +drg-top+)
						       (:left       +drg-left+)
						       (:font       *button-font*)
						       (:line-style *frame-line-style*)
						       (:string     "DEG"))))

    (set-result app-object 0)

    (kr:s-value app-object :proto-button
		(setq proto-button
		      (unnamed-instance opal:k-framed-text
					(:width         +button-width+)
					(:height        +button-height+)
					(:filling-style *button-filling-style*)
					(:line-style    *button-line-style*)
					(:font          *button-font*))))

    (kr:s-value app-object :proto-cut-paste-button
		(setq proto-cut-paste-button
		      (unnamed-instance proto-button
					(:width     +cut-paste-width+)
					(:left     (+ +frame-left+ +frame-width+ +button-h-spacing+)))))

    (let ((left +button-left+))
      (dolist (column +button-list+)
	(make-button-column app-object main-agg +button-top+
			    left column proto-button)
	(incf left (+ +button-width+ +button-h-spacing+)))
      (make-button-column app-object
			  main-agg
			  (+ +frame-top+ +button-v-spacing+)
			  left
			  +cut-paste-column+
			  proto-cut-paste-button))

    (opal:remove-component main-agg greeting)
    (opal:destroy greeting)

    (opal:add-component main-agg
			(kr:s-value app-object :feedback-rect
				    (kr:create-instance NIL opal:rectangle
				      (:filling-style opal:black-fill)
				      (:line-style    NIL)
				      (:draw-function :xor)
				      (:fast-redraw-p T)
				      (:visible NIL))))

    (opal:update main-window T)))

                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;;;;;;; THE EVENT HANDLER  ;;;;;;;
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun do-button-press (app-object main-agg x y)
  (let ((dispatch-fn (kr:g-value (opal:point-to-leaf main-agg x y)
				  :dispatch-fn)))
    (if dispatch-fn (funcall dispatch-fn app-object))))

(defun setup-event-handling (app-object)
 (let* ((main-window (kr:g-value app-object :main-window))
        (main-agg    (kr:g-value main-window :aggregate)))
   (kr:s-value app-object :button-interactor
    (unnamed-instance inter:button-interactor
      (:window main-window)
      (:continuous T)
      (:start-where `(:leaf-element-of ,main-agg :type ,opal:k-framed-text))
      (:start-action
	  #'(lambda (inter button) (declare (ignore inter))
	    (let ((feedback-rect (kr:g-value app-object :feedback-rect)))
		(kr:s-value feedback-rect :left   (kr:g-value button :left))
		(kr:s-value feedback-rect :top    (kr:g-value button :top))
		(kr:s-value feedback-rect :width  (kr:g-value button :width))
		(kr:s-value feedback-rect :height (kr:g-value button :height))
		(kr:s-value feedback-rect :visible T))))
      (:outside-action
	  #'(lambda (inter button) (declare (ignore inter button))
	    (kr:s-value (kr:g-value app-object :feedback-rect) :visible NIL)))
      (:back-inside-action
	  #'(lambda (inter button) (declare (ignore inter button))
	    (kr:s-value (kr:g-value app-object :feedback-rect) :visible T)))
      (:abort-action
	  #'(lambda (inter button) (declare (ignore inter button))
	    (kr:s-value (kr:g-value app-object :feedback-rect) :visible NIL)))
      (:stop-action
	  #'(lambda(inter button) (declare (ignore inter))
	    (kr:s-value (kr:g-value app-object :feedback-rect) :visible NIL)
	    (let ((dispatch-fn (kr:g-value button :dispatch-fn)))
	     (if dispatch-fn
	         (funcall dispatch-fn app-object)
	         (error "Button ~A does not have a dispatch-fn"
		        button)))))))
   (kr:s-value app-object :text-interactor
    (unnamed-instance inter:text-interactor
	(:window main-window)
	(:start-where T)
	(:start-event :any-keyboard)
	(:continuous NIL)
	(:stop-action #'(lambda(inter &rest args)
			     (declare (ignore args))
			     (let* ((char (kr:g-value inter :start-char))
			            (dispatch-fn
			             (gethash
			              char
			              (kr:g-value app-object :key-hashtable))))
			       (when dispatch-fn
				(funcall dispatch-fn app-object))
			     )))))
    ))

(defun setup-application-object ()
  (unnamed-instance NIL
	(:app-name "gcalc")
	(:new-number?   NIL)
	(:prev-key      NIL)  ;; key (or key equivalent) last pressed by user
	(:main-window   NIL)
	(:frame-window  NIL)
	(:result        NIL)
	(:drg           NIL)
        (:key-hashtable (make-hash-table :test #'eq :size 50))
	(:memories      (make-array 10 :initial-element 0))
	(:stack         NIL)
	(:stack-stack   NIL)

	(:feedback-rect     NIL)
	(:proto-button      NIL)
	(:button-interactor NIL)
	(:text-interactor   NIL)
   ))


(defun start-calc (&key double-buffered-p)
 (let ((app-object (setup-application-object)))
  (setup-main-window app-object double-buffered-p)
  (setup-event-handling app-object)
  app-object))

(defun stop-calc (app-object &optional (destroy-app-object? T))
 (let (object)
  (dolist (slot '(:text-interactor :button-interactor
			:main-window :proto-button))
	(if (setq object (kr:g-value app-object slot))
	   (opal:destroy object)))
  (when destroy-app-object? (kr:destroy-schema app-object))
  ;;for demo-controller
  (when (fboundp 'Common-Lisp-User::Garnet-Note-Quitted)
    (common-lisp-user::Garnet-Note-Quitted "GARNET-CALCULATOR"))))




(defun do-go (&key dont-enter-main-event-loop double-buffered-p)
  (if (kr:schema-p *Demo-App-Obj*)
      (warn "Garnet-Calc demo is already running -- must do-stop first.")
      (setf *Demo-App-Obj* (start-calc :double-buffered-p double-buffered-p)))
  
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

(defun do-stop ()
  (when (kr:schema-p *Demo-App-Obj*)
    (stop-calc *Demo-App-Obj*)
    (setf *Demo-App-Obj* NIL)
    ;;for demo-controller
    (when (fboundp 'Common-Lisp-User::Garnet-Note-Quitted)
      (common-lisp-user::Garnet-Note-Quitted "GARNET-CALCULATOR"))))
