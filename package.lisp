
(defpackage :garnet-utils
  (:use :common-lisp)
  (:nicknames :gu)
  (:export *garnet-break-key*
	   safe-functionp
	   while))

(defpackage :kr
  (:use :common-lisp )
  (:export schema
	   create-instance
	   create-prototype
	   create-relation
	   create-schema
	   formula
	   o-formula
	   schema-p
	   relation-p
	   is-a-p
	   has-slot-p
	   formula-p
	   s-value
	   g-value
	   g-local-value
	   gv
	   gvl
	   get-value
	   get-local-value
	   doslots
	   define-method
	   kr-send
	   call-prototype-method
	   *print-as-structure*
	   with-constants-disabled
	   with-types-disabled
	   change-formula
	   copy-formula
	   kr-path
	   mark-as-changed
	   mark-as-invalid
	   ps
	   g-type
	   DEF-KR-TYPE
	   s-type
	   kr-boolean
	   get-slot-doc
	   set-slot-doc
	   get-type-documentation
	   set-type-documentation
	   s-formula-value
	   self-old-value))

(defpackage :opal
  (:use :common-lisp :kr)


)

(defpackage :mge (:use :common-lisp :kr)
	    (:export do-go do-stop
		     create-piece destroy-piece destroy-all-pieces
		     go-initialize editor-show-window))
