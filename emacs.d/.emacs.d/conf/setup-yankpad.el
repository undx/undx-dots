;;; setup-yankpad.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yankpad
  :init
  (setq yankpad-file (concat user-emacs-directory "templates/yankpad.org"))
  :config
  ;; If you want to complete snippets using company-mode (push 'company-yankpad company-backends)
  ;; If you want to expand snippets with hippie-expand
  (add-to-list 'hippie-expand-try-functions-list #'yankpad-expand)
  )


(provide 'setup-yankpad)
;;; setup-yankpad ends here
