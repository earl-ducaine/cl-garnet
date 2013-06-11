;;;-*- Mode: COMMON-LISP; Package: MULTI-GARNET -*-

(eval-when (:load-toplevel :compile-toplevel :execute)
  (unless (member :GARNET lisp:*features*)
    (cerror "continue" "Garnet must be loaded before loading or compiling Multi-Garnet"))
  #-(and)
  (unless (or (equal "2.2" user::garnet-version-number)
	      )
    (format t "~&Warning: Garnet version ~S is loaded~%" user::garnet-version-number)
    (format t "~&    Multi-Garnet v2.2 has been tested in Garnet v2.2~%")
    (format t "~&    Multi-Garnet will probably not work with this version of Garnet.~%")
    (cerror "load Multi-Garnet v2.2"
            "Incompatible version of Garnet loaded: continue at your own risk")
    )
  )

#-(and)
(in-package :multi-garnet :nicknames '(:mg)
	    :use '(:lisp :kr :kr-debug :garnet-debug))


(load "package")

(in-package "MULTI-GARNET")

;; make sure that multi-garnet is disabled when compiling or loading it
(eval-when (:load-toplevel :execute)
  (when (fboundp 'disable-multi-garnet)
    (disable-multi-garnet))
  )

(load "sky-blue")
(load "multi-garnet")

;; enable multi-garnet after all files are loaded
(eval-when (:load-toplevel :execute) (enable-multi-garnet))

