;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-UTILS; Base: 10 -*-
;;                                                                   ;;
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.                                                          ;;
;;*******************************************************************;;

;;; $Id$
;;

;; general.lisp
;;
;; by David S. Kosbie
;;
;; This file defines a host of Lisp utilities used by other Garnet code.


(in-package "GARNET-UTILS")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(WHILE
            UNTIL
            TILL
            DO2LISTS
            DOLIST2
            M
            M1
            STRING+
            ADD-TO-LIST
            VERIFY-BINDING
            SAFE-FUNCTIONP
            SHELL-EXEC
            DIRECTORY-P
            PROBE-DIRECTORY

            PI/2  PI3/2  2PI -2PI SHORT-PI
            )))


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


;;; Verify-Binding implementation
;;

;; Keep --- a demo uses this
(defun VERIFY-BINDING (string)
  "Takes a string and returns the symbol coercion of the string if the
symbol is bound.  Note: The suffix of the string is converted to all
uppercase characters before checking if it is bound in the package."
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




(defun VERIFY-BINDING-AUX (string start)
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

;;;
;;; (end) Verify-Binding

(defun safe-functionp (fn)
  (or (functionp fn)
      (and (symbolp fn) (fboundp fn))))

;;;
;; Note that I'm trying to move away from ad-hoc string hacking and
;; low-level approaches in the filesystem stuff in favor of the Lisp
;; pathname facilities.
(defun probe-directory (filename)
  #+clisp (let ((system::*error-handler*
                 #'(lambda (&rest args)
                     (declare (ignore args))
                     (return-from probe-directory nil))
                  ))
            ; The following causes an error if the directory does not exist
            (and (truename filename) t))

  ;; Garnet under SBCL requires sb-posix.
  #+sbcl
  (ignore-errors (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat filename))))
  #+allegro
  (excl:file-directory-p filename)

  #+(or cmucl ccl)
  (let ((truename (probe-file filename)))
    (and truename
         (not (pathname-name truename))
         (not (pathname-type truename))))

  #-(or clisp ccl cmucl sbcl allegro) (probe-file filename)
  )

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


;; If the -d test is true, shell-exec returns "1". Otherwise, it
;; returns "". This syntax works for all kinds of Unix shells: sh,
;; csh, ksh, tcsh, ...
;;
;; Avoid the use of the shell if possible (use probe-directory above).
(defun directory-p (pathname)
  #+(or sbcl allegro ccl cmucl)
  ;; 1. Needn't call a shell if we can do the test ourselves.
  ;; 2. In case pathname contains Latin-1 characters. clisp is 8 bit clean,
  ;;    while most Unix shells aren't.
  (garnet-utils:probe-directory pathname)

  #-(or sbcl allegro ccl cmucl)
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
                   
