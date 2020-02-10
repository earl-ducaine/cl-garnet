
(in-package "OPAL")

(defvar *debug-opal-mode* nil)
(setf (get :garnet-modules :opal) T)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (export '(*debug-opal-mode* bottom right center-x center-y
	    *garnet-windows*
	   aggregate
	    read-image
	    reconnect-garnet
	    rectangle
	    clip-and-map
	    drawable-to-window)))
