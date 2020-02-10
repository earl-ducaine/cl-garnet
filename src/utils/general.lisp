(in-package :garnet-utils)

(defun safe-functionp (fn)
  (or (functionp fn)
      (and (symbolp fn) (fboundp fn))))
