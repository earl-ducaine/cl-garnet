
(require :asdf)
(asdf:load-systems :clx-truetype)

(in-package :stumpwm)

(defparameter +base-font+ (make-instance 'xft:font :family "Consolas" :subfamily "Regular" :size 13))
;;(setf +base-font+ (make-instance 'xft:font :family "Consolas" :subfamily "Regular" :size 16))

;;; primitives.lisp
(defun truetype-font-height (drawable font)
  (+ (xft:font-ascent drawable font)
     (- (xft:font-descent drawable font))))

(defun truetype-font-height-lines (drawable font lines-count)
  (if (> lines-count 0)
      (+ (+ (xft:font-ascent drawable font)
            (- (xft:font-descent drawable font)))
         (* (1- lines-count) (+ (xft:font-ascent drawable font)
                                (- (xft:font-descent drawable font))
                                (xft:font-line-gap drawable font))))
      0))


;;; help.lisp
(defvar old-display-bindings-for-keymaps (symbol-function 'display-bindings-for-keymaps))a
(defun display-bindings-for-keymaps (key-seq &rest keymaps)
  (let* ((screen (current-screen))
         (data (mapcan (lambda (map)
                         (mapcar (lambda (b) (format nil "^5*~5a^n ~a" (print-key (binding-key b)) (binding-command b))) (kmap-bindings map)))
                       keymaps))
         (cols (ceiling (1+ (length data))
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (truetype-font-height (screen-number screen) +base-font+)))))
    (message-no-timeout "Prefix: ~a~%~{~a~^~%~}"
                        (print-key-seq key-seq)
                        (columnize data cols))))

(defcommand commands () ()
"List all available commands."
  (let* ((screen (current-screen))
         (data (all-commands))
         (cols (ceiling (length data)
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (truetype-font-height (screen-number screen) +base-font+)))))
    (message-no-timeout "~{~a~^~%~}"
                        (columnize data cols))))

;;; message-window.lisp
(defvar old-show-frame-indicator (symbol-function 'show-frame-indicator))
(defun show-frame-indicator (group &optional force)
  (show-frame-outline group)
  ;; FIXME: Arg, these tests are already done in show-frame-outline
  (when (find group (mapcar 'screen-current-group *screen-list*))
    (when (or force
              (and (or (> (length (tile-group-frame-tree group)) 1)
                       (not (atom (first (tile-group-frame-tree group)))))
                   (not *suppress-frame-indicator*)))
      (let ((frame (tile-group-current-frame group))
            (w (screen-frame-window (current-screen)))
            (string (if (stringp *frame-indicator-text*)
                        *frame-indicator-text*
                        (prin1-to-string *frame-indicator-text*)))
            (font (screen-font (current-screen))))
        ;; If it's already mapped it'll appear briefly in the wrong
        ;; place, so unmap it first.
        (xlib:unmap-window w)
        (xlib:with-state (w)
          (setf (xlib:drawable-x w) (+ (frame-x frame)
                                       (truncate (- (frame-width frame) 
                                                    (xft:text-line-width w +base-font+ string)) 2))
                (xlib:drawable-y w) (+ (frame-display-y group frame)
                                       (truncate (- (frame-height frame) 
                                                    (truetype-font-height w +base-font+)) 2))
                (xlib:window-priority w) :above))
        (xlib:map-window w)
        (echo-in-window w font (screen-fg-color (current-screen)) (screen-bg-color (current-screen)) string)
        (reset-frame-indicator-timer)))))


(defvar old-echo-in-window (symbol-function 'echo-in-window))
(defun echo-in-window (win font fg bg string)
  (let* ((gcontext (xlib:create-gcontext :drawable win
                                         :font font
                                         :foreground fg
                                         :background bg))
         (height (truetype-font-height win font))
         (width (xft:text-line-width win font string)))
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) width))
    (xlib:clear-area win)
    (xlib:display-finish-output *display*)
    (xft:draw-text-line win gcontext +base-font+ string 0 (xft:font-ascent win +base-font+))
    ;; (xlib:draw-image-glyphs win gcontext 0 (xlib:font-ascent font) string :translate #'translate-id :size 16)
    ))

(defvar old-echo-string-list (symbol-function 'echo-string-list))
(defun echo-string-list (screen strings &rest highlights)
  "Draw each string in l in the screen's message window. HIGHLIGHT is
  the nth entry to highlight."
  (when strings
    (unless *executing-stumpwm-command*
      (let ((width
              (render-antialiased-strings 
               screen (screen-message-cc screen) *message-window-padding* 0 strings '() nil)))
        (setup-message-window screen (length strings) width)
        (render-antialiased-strings 
         screen (screen-message-cc screen) *message-window-padding* 0 strings highlights))
      (setf (screen-current-msg screen)
            strings
            (screen-current-msg-highlights screen)
            highlights)
      ;; Set a timer to hide the message after a number of seconds
      (if *suppress-echo-timeout*
          ;; any left over timers need to be canceled.
          (when (timer-p *message-window-timer*)
            (cancel-timer *message-window-timer*)
            (setf *message-window-timer* nil))
          (reset-message-window-timer)))
    (push-last-message screen strings highlights)
    (xlib:display-finish-output *display*)
    (dformat 5 "Outputting a message:~%~{        ~a~%~}" strings)
    (apply 'run-hook-with-args *message-hook* strings)))

(defvar old-setup-message-window (symbol-function 'setup-message-window))
(defun setup-message-window (screen lines width)
  (let ((height (truetype-font-height-lines (screen-number screen) +base-font+ lines))
        (win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
            (xlib:window-priority win) :above)
      (setup-win-gravity screen win *message-window-gravity*))
    (xlib:map-window win)
    (incf (screen-ignore-msg-expose screen))
    ;; Have to flush this or the window might get cleared
    ;; after we've already started drawing it.
    (xlib:display-finish-output *display*)))


;;; input.lisp
(defvar old-setup-input-window (symbol-function 'setup-input-window))
(defun setup-input-window (screen prompt input)
  "Set the input window up to read input"
  (let* ((win (screen-input-window screen))
         (height (truetype-font-height win +base-font+)))
    ;; Window dimensions
    (xlib:with-state (win)
      (setf (xlib:window-priority win) :above
            (xlib:drawable-height win) height))
    (xlib:map-window win)
    ;; Draw the prompt
    (draw-input-bucket screen prompt input)))
    ;; Ready to recieve input

(defvar old-draw-input-bucket (symbol-function 'draw-input-bucket))
(defun draw-input-bucket (screen prompt input &optional (tail "") errorp)
  "Draw to the screen's input window the contents of input."
  (let* ((gcontext (screen-message-gc screen))
         (win (screen-input-window screen))
         (prompt-width (xft:text-line-width win +base-font+ prompt)
                       ;; (xlib:text-width (screen-font screen) prompt :translate #'translate-id)
                       )
         (line-content (input-line-string input))
         (string (if (input-line-password input)
                     (make-string (length line-content) :initial-element #\*)
                     line-content))
         (text-width (xft:text-line-width win +base-font+ string)
                     ;; (xlib:text-width (screen-font screen) string :translate #'translate-id)
                     )
         (space-width (xft:text-line-width win +base-font+ " ")
                      ;;(xlib:text-width (screen-font screen) " "    :translate #'translate-id)
                      )
         (tail-width (xft:text-line-width win +base-font+ tail)
           ;;(xlib:text-width (screen-font screen) tail   :translate #'translate-id)
           )
         (full-text-width (+ text-width space-width))
         (pos (input-line-position input))
         (width (+ prompt-width
                   (max 100 (+ full-text-width space-width tail-width)))))
    (xlib:with-state (win)
      (xlib:clear-area win :x (+ *message-window-padding*
                                 prompt-width
                                 text-width))
      (setf (xlib:drawable-width win) (+ width (* *message-window-padding* 2)))
      (setup-win-gravity screen win *input-window-gravity*))
    (xlib:with-state (win)
      (xft:draw-text-line win gcontext +base-font+
                       prompt *message-window-padding* (xft:font-ascent win +base-font+))
      ;; (xlib:draw-image-glyphs win gcontext
      ;;                         *message-window-padding*
      ;;                         (xlib:font-ascent (screen-font screen))
      ;;                         prompt
      ;;                         :translate #'translate-id
      ;;                         :size 16)
      (xft:draw-text-line win gcontext +base-font+ 
                       string (+ *message-window-padding* prompt-width)
                       (xft:font-ascent win +base-font+))
      ;; (xlib:draw-image-glyphs win gcontext
      ;;                         (+ *message-window-padding* prompt-width)
      ;;                         (xlib:font-ascent (screen-font screen))
      ;;                         string
      ;;                         :translate #'translate-id
      ;;                         :size 16)
      (xft:draw-text-line win gcontext +base-font+ 
                       tail (+ *message-window-padding* prompt-width full-text-width space-width)
                       (xft:font-ascent win +base-font+))
      ;; (xlib:draw-image-glyphs win gcontext
      ;;                         (+ *message-window-padding* prompt-width full-text-width space-width)
      ;;                         (xlib:font-ascent (screen-font screen))
      ;;                         tail
      ;;                         :translate #'translate-id
      ;;                         :size 16)
      ;; draw a block cursor
      (invert-rect screen win
                   (+ *message-window-padding*
                      prompt-width
                      (xft:text-line-width win +base-font+ (subseq string 0 pos))
                      ;; (xlib:text-width (screen-font screen) (subseq string 0 pos) :translate #'translate-id)
                      )
                   0
                   (xft:text-line-width win +base-font+ (if (>= pos (length string))
                                                         " "
                                                         (string (char string pos))))
                   ;; (xlib:text-width (screen-font screen) (if (>= pos (length string))
                   ;;                                           " "
                   ;;                                           (string (char string pos)))
                   ;;                  :translate #'translate-id)
                   (truetype-font-height win +base-font+)
                   ;; (+ (xlib:font-descent (screen-font screen))
                   ;;    (xlib:font-ascent (screen-font screen)))
                   )
      ;; draw the error
      (when errorp
        (invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win))
        (xlib:display-force-output *display*)
        (sleep 0.05)
        (invert-rect screen win 0 0 (xlib:drawable-width win) (xlib:drawable-height win))))))


;;; mode-line.lisp
(defvar old-resize-mode-line (symbol-function 'resize-mode-line))
(defun resize-mode-line (ml)
  (when (eq (mode-line-mode ml) :stump)
    ;; This is a StumpWM mode-line
    (setf (xlib:drawable-height (mode-line-window ml)) 
          (+ (* (1+ (count #\Newline (mode-line-contents ml) :test #'equal))
                (truetype-font-height (mode-line-window ml) +base-font+)
                ;;(font-height (xlib:gcontext-font (mode-line-gc ml)))
                )
             (* *mode-line-pad-y* 2))))
  (setf (xlib:drawable-width (mode-line-window ml)) (- (frame-width (mode-line-head ml))
                                                       (* 2 (xlib:drawable-border-width (mode-line-window ml))))
        (xlib:drawable-height (mode-line-window ml)) (min (xlib:drawable-height (mode-line-window ml))
                                                          (truncate (head-height (mode-line-head ml)) 4))
        (mode-line-height ml) (+ (xlib:drawable-height (mode-line-window ml))
                                 (* 2 (xlib:drawable-border-width (mode-line-window ml))))
        (mode-line-factor ml) (- 1 (/ (mode-line-height ml)
                                      (head-height (mode-line-head ml))))
        (xlib:drawable-x (mode-line-window ml)) (head-x (mode-line-head ml))
        (xlib:drawable-y (mode-line-window ml)) (if (eq (mode-line-position ml) :top)
                                                    (head-y (mode-line-head ml))
                                                    (- (+ (head-y (mode-line-head ml))
                                                          (head-height (mode-line-head ml)))
                                                       (mode-line-height ml)))))

(defvar old-redraw-mode-line (symbol-function 'redraw-mode-line))
(defun redraw-mode-line (ml &optional force)
  (when (eq (mode-line-mode ml) :stump)
    (let* ((*current-mode-line-formatters* *screen-mode-line-formatters*)
           (*current-mode-line-formatter-args* (list ml))
           (string (mode-line-format-string ml)))
      (when (or force (not (string= (mode-line-contents ml) string)))
        (setf (mode-line-contents ml) string)
        (resize-mode-line ml)
        (render-antialiased-strings (mode-line-screen ml) (mode-line-cc ml)
                                    *mode-line-pad-x*     *mode-line-pad-y*
                                    (split-string string (string #\Newline)) '())))))


;;; color.lisp
(defun render-antialiased-strings (screen cc padx pady strings highlights &optional (draw t))
  (let* ((height (xft:baseline-to-baseline (screen-number screen) +base-font+))
         (width 0)
         (gc (ccontext-gc cc))
         (win (ccontext-win cc))
         (px (ccontext-px cc))
         (*foreground* nil)
         (*background* nil)
         (*reverse* nil)
         (*color-stack* '())
         (*color-map* (screen-color-map-normal screen)))
    (when draw
      (when (or (not px)
                (/= (xlib:drawable-width px) (xlib:drawable-width win))
                (/= (xlib:drawable-height px) (xlib:drawable-height win)))
        (when px (xlib:free-pixmap px))
        (setf px (xlib:create-pixmap :drawable win
                                     :width (xlib:drawable-width win)
                                     :height (xlib:drawable-height win)
                                     :depth (xlib:drawable-depth win))
              (ccontext-px cc) px))
      (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
        (xlib:draw-rectangle px gc 0 0 (xlib:drawable-width px) (xlib:drawable-height px) t)))
    (loop for s in strings
          ;; We need this so we can track the row for each element
          for i from 0 to (length strings)
          do (let ((x 0) (off 0) (len (length s)))
               (loop
                 for st = 0 then (+ en (1+ off))
                 as en = (position #\^ s :start st)
                 do (progn
		     (let ((en (cond ((and en (= (1+ en) len)) nil)
				     ((and en (char= #\^ (char s (1+ en)))) (1+ en))
				     (t en))))
		       (when draw
                         (xft:draw-text-line px gc
                                          +base-font+
                                          (subseq s st en)
                                          (+ padx x)
                                          (+ pady (* i height)
                                             (xft:font-ascent px +base-font+))))
		       (setf x 
                             (+ x
                                (xft:text-line-width
                                 (screen-number screen) +base-font+ (subseq s st en)))
			     width (max width x)))
		     (when (and en (< (1+ en) len))
		       ;; right-align rest of string?
		       (if (char= #\> (char s (1+ en)))
			   (progn
			     (when draw
			       (setf x (- (xlib:drawable-width px) (* 2 padx)
					  ;; get width of rest of s
					  (render-antialiased-strings 
                                           (screen-number screen) cc padx pady
                                           (list (subseq s (+ en 2)))
                                           '() nil))
				     width (- (xlib:drawable-width px) (* 2 padx))))
			     (setf off 1))
			   (setf off (set-color screen cc s (1+ en))))))
		  while en))
          when (find i highlights :test 'eql)
          do (when draw (invert-rect screen px
                                     0 (* i height)
                                     (xlib:drawable-width px)
                                     height)))
    (when draw
      (xlib:copy-area px gc 0 0 (xlib:drawable-width px) (xlib:drawable-height px) win 0 0))
    (set-color screen cc "n" 0)
    width))
