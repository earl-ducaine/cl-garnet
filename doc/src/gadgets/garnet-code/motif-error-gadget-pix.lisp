(when (boundp 'win) (opal:destroy win))

(create-instance 'error-gad garnet-gadgets:motif-error-gadget
  (:background-color opal:motif-gray))

(garnet-gadgets:display-error error-gad "Error: Invalid input from user.
Press OK to continue.")

			     
