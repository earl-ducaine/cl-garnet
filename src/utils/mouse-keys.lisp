(in-package :COMMON-LISP-USER)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defvar REFRESH-INIT
    (progn
      (garnet-load "gadgets:text-buttons-loader")
      (garnet-load "gadgets:radio-buttons-loader"))))

(defvar SMALL-FONT (opal:get-standard-font NIL NIL :small))

(when (boundp 'REFRESH-WIN) (opal:destroy REFRESH-WIN))

(create-instance 'REFRESH-WIN inter:interactor-window
  (:left 515) (:top 365) (:height 110) (:width 120)
  (:title "Keys")
  (:aggregate (create-instance 'REFRESH-TOP-AGG opal:aggregate)))

(opal:update REFRESH-WIN)

(create-instance 'REFRESH gg:text-button
  (:left 10) (:top 10)
  (:text-offset 2) (:gray-width 3) (:shadow-offset 5)
  (:font SMALL-FONT)
  (:string "Refresh")
  (:final-feedback-p NIL)
  (:selection-function #'(lambda (gadget value)
                           (declare (ignore gadget value))
                           (opal:update-all T))))

(defun function-key-fn (gadget value)
  (declare (ignore gadget value))
  (setf inter::*leftdown-key* 105
        inter::*middledown-key* 107
        inter::*rightdown-key* 113))

(defun arrow-key-fn (gadget value)
  (declare (ignore gadget value))
  (setf inter::*leftdown-key* 123
        inter::*middledown-key* 124
        inter::*rightdown-key* 125))

(create-instance 'MOUSE-KEYS gg:radio-button-panel
  (:left 10) (:top 40)
  (:font SMALL-FONT)
  (:text-offset 2) (:gray-width 3) (:shadow-offset 5)
  (:button-diameter 20)
  (:items `(("Function keys" Function-key-fn)
            ("Arrow keys" Arrow-key-fn))))

(g-value MOUSE-KEYS :value)
(s-value MOUSE-KEYS :value "Function keys")

(opal:add-components REFRESH-TOP-AGG REFRESH MOUSE-KEYS)
(opal:update REFRESH-WIN)

(opal:update REFRESH-WIN T)
