;; (package-initialize)

(add-to-list 'load-path (expand-file-name "site-lisp/org-8.3.6/lisp"
					  user-emacs-directory))
(require 'org)

;; possibly add in (byte-compile elisp-file)
(let ((org-file (expand-file-name "config.org" user-emacs-directory))
      (elisp-file (expand-file-name "config.el" user-emacs-directory)))
  (if (file-newer-than-file-p org-file elisp-file)
      (progn
        (find-file org-file)
        (org-babel-tangle nil elisp-file)
        (load-file elisp-file))
    (load-file elisp-file)))
