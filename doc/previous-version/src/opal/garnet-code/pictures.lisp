;;; -*- Mode: Lisp, Fill, Save; Package: GARNET; Log: opal.log -*-
;;;
;;; ______________________________________________________________________
;;;
;;; This code was written as part of the Garnet (User Interface) project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or anything developed as part of the Garnet
;;; Project, please contact Brad Myers (Brad.Myers@CS.CMU.EDU).
;;; ______________________________________________________________________
;;;
(in-package "GARNET" :use '("LISP" "KR" "XLIB"))

;;; These trash the links to objects we would rather not export from Opal - 
;;; namely:
;(dolist (x (get-values opal:window         :is-a-inv)) (opal:destroy x))
;(dolist (x (get-values opal:aggregate      :is-a-inv)) (opal:destroy x))
;(dolist (x (get-values opal:bitmap         :is-a-inv)) (opal:destroy x))
;(dolist (x (get-values opal:font-from-file :is-a-inv)) (opal:destroy x))
(destroy-slot opal:window         :is-a-inv)
(destroy-slot opal:aggregate      :is-a-inv)
(destroy-slot opal:bitmap         :is-a-inv)
(destroy-slot opal:font-from-file :is-a-inv)
(setq kr::*print-as-structure* nil)  ;;; so it won't print #k<...>

(in-package 'kr-debug)

(psgraph::graph opal:view-object "opal/doc/schemata.ps" t t)
(psgraph::graph opal:graphic-quality "opal/doc/gquality.ps" t t)
