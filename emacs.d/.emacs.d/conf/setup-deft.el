;;; setup-deft.el ---  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package deft :ensure t
  :config
  (setq
   ;;deft-directory (concat org-directory "notes/")
   deft-directory org-directory
   deft-default-extension "org"
   deft-extensions '("org" "md" "rst" "textile" "txt")
   deft-text-mode 'org-mode
   deft-recursive t
   deft-use-filename-as-title t
   deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))
(provide 'setup-deft)
;;; setup-deft.el ends here
