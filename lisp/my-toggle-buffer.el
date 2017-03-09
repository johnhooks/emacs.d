;;; my-toggle-buffer.el --- A smarter way to toggle buffers

;;; Commentary:

;;; Requires: cl.el

;;; Code:

;; 'C-- C-x o' other-window with a negative prefix argument.

;; functions
;; (buffer-list) returns list of buffers
;; (window-list) returns list of windows
;; (window-buffer & WINDOW) returns buffer displayed in window.
;;   if WINDOW is omitted or nil, it defaults to the selected window.
;; (other-buffer & BUFFER) returns first buffer in the buffer list other
;;   than BUFFER. Usually, this is the buffer appearing in the most recently
;;   selected frame. Buffers whose name start with a space or not considered
;;   at all.
;; (switch-to-buffer buffer-or-name) attempts to display buffer-or-name in
;;   the selected window and make it the current buffer. Returns the buffer
;;   switched to. If buffer-or-name is nil it defaults to the buffer returned
;;   by `other-buffer'. If buffer-or-name is a string and a buffer cannot
;;   be found, a new buffer of that name is created.
;; (buffer-name & BUFFER) returns a name of the BUFFER, as a  string.
;;   BUFFER defaults wot the current buffer.
;; (get-buffer-window & buffer-or-name all-frams) returns first window
;;   displaying buffer-of-name in the cyclic ordering of windows, starting
;;   from the selected window. if buffer-or-name is omitted it defaults to
;;   current buffer.
;; (select-window WINDOW) makes WINDOW the selected window, and makes
;;   WINDOW's buffer current. Return value is WINDOW.

;; local variables
;;   buffer-display-time
;;   buffer-display-count
;; TODO look into buffer-list-update-hook, bury-buffer, unbury-buffer

(defun my/find (pred l)
  "Find first match to the predicate PRED in list L."
  (cond
     ((null l) nil)
     ((funcall pred (car l)) (car l))
     (t (my/find pred (cdr l)))))

(defun curry (fn &rest args)
  "Curry FN to apply with ARGS."
  ;; Arguments can be supplied individually or &optional or &rest
  ;; Think I will only be able to curry upto the &optional arguments.
  (let ((count (length (cadr fn))) ;; get the argument count
        (fn fn)
        (args args))
    (if (< count (length args)))
  (lambda (&rest more) (apply fn (append args more)))))

(defun my/find-buffer-in-window (buffer)
  "Find if BUFFER is visible, using `window-list'.  Return window otherwise nil."
  ;; Using `eq' rather that `equal', dude on irc#emacs confirmed it.
  ;; From `eq' docstring...
  ;;   Return t if the two args are the same Lisp object.
  (cl-find-if #'(lambda (window)
                  (eq buffer (window-buffer window)))
              (window-list)))

;; ** noticable problem Ibuffer captures my desired key chord of C-o
;; ** Bookmarks does too.

(defun my/toggle-buffer ()
  "Switch to previous buffer."

  (interactive) ; I don't think this has to be interactive.
  ;; It does have to be interactive. Any function that is bound to
  ;;   keychord must be interactive.

  ;; Local variable `buffer' assigned the most recent buffer excluding
  ;;   the `current-buffer' and including visible buffers.

  ;; Local variable `window' assigned first window in `window-list'
  ;;   containing `buffer'.

  ;; let* Sets each variable in sequence so the varlist can make use
  ;;   of variables defined earlier in the list.

  (let* ((buffer (other-buffer (current-buffer) t))
         (window (my/find-buffer-in-window buffer)))
    (message "%s" window) ; just for testing

    ;; Select the first window in `windows' if it is non nil.
    ;;  Otherwise switch to `buffer' in the current window.

    (if (windowp window)
        (select-window window)
      (switch-to-buffer buffer))))


(defun my/jump-to-window (buffer)
  "Jump to a window holding BUFFER using cyclical order.
BUFFER can be a buffer object or a string of a buffer name."

  ;; Would like to make the list only shows open windows.

  (interactive "bEnter buffer to jump to: ")

  (or (bufferp buffer)
      (setq buffer (get-buffer buffer))) ; set function argument... cool!

  (let ((window (my/find-buffer-in-window buffer)))
    (if (windowp window)
        (select-window window)
      (error "'%s' does not have visible window" (buffer-name buffer)))))


(defun my/jump-to-window (buffer)
  "Jump to a window holding BUFFER using cyclical order.
BUFFER can be a buffer object or a string of a buffer name."

  ;; Would like to make the list only shows open windows.

  (interactive "bEnter buffer to jump to: ")

  (or (bufferp buffer)
      (setq buffer (get-buffer buffer))) ; set function argument... cool!

  (let ((window (my/find-buffer-in-window buffer)))
    (if (windowp window)
        (select-window window)
      (error "'%s' does not have visible window" (buffer-name buffer)))))

(defun pretty-window-list (list &optional count)
  "Print a pretty numbers window list.  LIST COUNT."
  (or (numberp count) (setq count 1))
  (message "%s\n%s" count list)
  (cond
   ((null list) ())
   (t (cons
       (concat
        (number-to-string count)
        " : "
        (buffer-name (window-buffer (car list))))
       (pretty-window-list (cdr list) (1+ count))))))

(defun my/jump-to-window-test (buffer)
  "Jump to a window holding BUFFER using cyclical order.
BUFFER can be a buffer object or a string of a buffer name."

  (interactive (list (ido-completing-read
                      "Choose a window "
                      ;; (mapcar #'(lambda (window)
                      ;;             (buffer-name (window-buffer window)))
                      ;;         (window-list))
                      (pretty-window-list (window-list))
                      )))
  (string-match "^[1-9]" buffer)
  (let* ((number (string-to-number (match-string 0 buffer)))
         (window ()))
    (if (windowp window)
        (select-window window)
      (error "'%s' does not have visible window" (buffer-name buffer))))
  ;; (or (bufferp buffer)
  ;;     (setq buffer (get-buffer buffer))) ; set function argument... cool!

  ;; (let ((window (my/find-buffer-in-window buffer)))
  ;;   (if (windowp window)
  ;;       (select-window window)
  ;;     (error "'%s' does not have visible window" (buffer-name buffer))))
  )


(provide 'my-toggle-buffer)

;;; my-toggle-buffer.el ends here
