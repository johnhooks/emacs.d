;;; functions.el --- A collections of functions

;;; Commentary:

;;; Most of this was copied from Bozhidar Batsov's website 'emacsredux.com'

;;; Code:

(defun my/move-beginning-of-line (arg)
  "Move point back to indention of beginning of line.

Move point to the first non-whitespace character on current line.
If point is already there, move to the beginning of the line.

If ARG in not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my/open-line ()
  "Insert an empty line after the current line.
Possition the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun my/open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

;; A possibliy difference keybinding choice
;; (global-set-key [(control shift return)] 'smart-open-line-above)

;; (defun my/toggle-buffer ()
;;   "Switch to previous buffer."
;;   (interactive)
;;   (switch-to-buffer (other-buffer (current-buffer) nil)))

(defun my/toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun my/toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when
                           (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defun my/goto-match-beginning ()
  "To advise :after `isearch-exit' to return to beginning of search."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; move to better location
(advice-add 'isearch-exit :after 'my/goto-match-beginning)

(provide 'functions)
;;; functions.el ends here
