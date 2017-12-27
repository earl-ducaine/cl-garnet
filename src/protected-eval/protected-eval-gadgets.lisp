
(in-package :garnet-gadgets)

(kr:create-instance 'protected-eval-error-gadget gg:motif-query-gadget
  (:documentation
   "Error Gadget Used by Garnet-Error-Handler to Display error
    messages." )
  (:parent-window nil)
  (:modal-p t)
  (:beep-p t)
  (:button-names '(:abort :debug :continue)))

(kr:create-instance 'error-prompter-gadget prompter-gadget
  (:documenatation
   "a version of the prompter-gadget for use with error recovery by
    prompting for a value to use/store.")
  (:modal-p t)
  (:beep-p t)
  (:parent-window nil)
  (:window-top (floor gem:*screen-height* 2))
  (:window-left (floor gem:*screen-width* 2)))

(defvar *normal-cursor-pair* (cons opal:Arrow-Cursor  opal:arrow-Cursor-Mask)
  "Cursor Pair for normal display (active input)")

;; Needs to be activated
(when opal::*main-event-loop-process*
  (opal:kill-main-event-loop-process)
  (opal:launch-main-event-loop-process))
