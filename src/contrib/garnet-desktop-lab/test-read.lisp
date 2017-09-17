


(in-package :elisp)

(defun run-readchar ()
  (setf unrch 110)
  (let ((elisp-object (make-instance 'elisp-object)))
    (readchar elisp-object)))




(defun test-readchar ()
  (let ((test-cases (list '(elisp-object 110))))
    (dolist (test-case test-cases)
      (format t "
    (readchar-runner (car test-case) (cadr test-case)))))

(defun readchar-runner (class elisp-char)
  (setf unrch elisp-char)
  (if (eq elisp-char
	  (let ((elisp-thing (make-instance class)))
	    (readchar elisp-thing)))
      (format t "Test succeeded on ~s ~s~%" class elisp-char)))
