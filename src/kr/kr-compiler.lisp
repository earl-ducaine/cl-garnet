;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER; Base: 10 -*-
;;
;;*******************************************************************;;
;;          The Garnet User Interface Development Environment.       ;;
;;*******************************************************************;;
;;  This code was written as part of the Garnet project at           ;;
;;  Carnegie Mellon University, and has been placed in the public    ;;
;;  domain.  If you are using this code or any part of Garnet,       ;;
;;  please contact garnet@cs.cmu.edu to be put on the mailing list.  ;;
;;*******************************************************************;;

;;; $Id::                                                             $
;;



;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (proclaim
;;    (if *debug-kr-mode*
;;        (and (boundp 'Garnet-Compile-Debug-Settings)
;; 	    Garnet-Compile-Debug-Settings)
;;        ;; Global default settings.
;;        (and (boundp 'Default-Garnet-Proclaim)
;; 	    Default-Garnet-Proclaim))))

;; (eval-when (:execute :load-toplevel :compile-toplevel)
;;   (garnet-mkdir-if-needed Garnet-KR-Pathname))

;; (Defparameter Garnet-KR-Files
;;   '("kr-macros"
;;     "kr-doc"
;;     "kr"
;;     "constraints"))

;; (with-compilation-unit ()
;;   (dolist (file Garnet-KR-Files)
;;     (let ((gfile (concatenate 'string "kr:" file)))
;;       (garnet-compile gfile)
;;       (garnet-load gfile))))

;; (garnet-copy-files Garnet-Kr-Src Garnet-Kr-Pathname
;; 		   '("kr-loader.lisp"))

;; (setf (get :garnet-modules :kr) T)
