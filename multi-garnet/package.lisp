;;; -*- Mode: COMMON-LISP; Package: COMMON-LISP-USER -*-
;;;

;;; Packages for Multi-Garnet.

(cl:in-package "COMMON-LISP-USER")

(defpackage "SKY-BLUE"
  (:nicknames :sb)
  (:use :common-lisp)
  (:export #:create-sb-constraint
	   #:create-sb-method
	   #:create-sb-variable

	   #:add-set-slot-fn
	   #:remove-set-slot-fn

	   #:get-sb-constraint-slot
	   #:set-sb-constraint-slot
	   #:get-sb-variable-slot
	   #:set-sb-variable-slot
	   #:get-sb-method-slot
	   #:get-sb-method-slot

	   #:sb-constraint-p
	   #:sb-method-p
	   #:sb-variable-p

	   #:get-sb-slot
	   #:set-sb-slot
	   #:sb-object-p

	   #:cn-variables
	   #:cn-strength
	   #:cn-methods
	   #:cn-selected-method
	   #:cn-mark

	   #:mt-code
	   #:mt-outputs

	   #:var-value
	   #:var-constraints
	   #:var-determined-by
	   #:var-walk-strength
	   #:var-mark
	   #:var-valid

	   #:sb-plan-p
	   #:sb-plan-valid
	   #:enforced
	   #:do-method-output-vars
	   #:do-selected-method-output-vars
	   #:do-method-input-vars
	   #:do-selected-method-input-vars
	   #:do-consuming-constraints

	   #:method-output-vars
	   #:selected-method-output-vars
	   #:method-input-vars
	   #:selected-method-input-vars
	   #:consuming-constraints

	   #:add-constraint
	   #:remove-constraint
	   #:*sky-blue-backtracking-warning*
	   #:*sky-blue-cycle-warning*

	   #:*strength-keyword-list*
	   #:*strength-list*
	   #:*max-strength*
	   #:*min-strength*
	   #:get-strength
	   #:get-strength-keyword
	   #:weaker
	   ;; backwards compatibility: support required, wekaest
	   #:*required-strength*
	   #:*weakest-strength*
	   ))


(defpackage :multi-garnet
  (:nicknames :mg)
  (:use :common-lisp :kr)
  (:import-from "SKY-BLUE"
		#:*sky-blue-backtracking-warning*
		#:*sky-blue-cycle-warning*
	   )
  (:export #:m-constraint
	   #:m-stay-constraint
	   #:s-value-strength
	   #:enable-multi-garnet
	   #:constraint-p
	   #:clone-constraint
	   #:*default-input-strength*
	   #:*max-path-updates*
	   #:*max-path-updates-warning*
	   #:*sky-blue-backtracking-warning*
	   #:*sky-blue-cycle-warning*
	   #:*unsatisfied-max-constraint-warning*
	   #:*multi-garnet-version*
	   #:*s-value-bad-schema-action*
	   ))
