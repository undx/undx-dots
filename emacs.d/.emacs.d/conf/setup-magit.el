;;; setup-magit.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;; The seven rules of a great Git commit message
;; - Separate subject from body with a blank line
;; - Limit the subject line to 50 characters
;; - Capitalize the subject line
;; - Do not end the subject line with a period
;; - Use the imperative mood in the subject line
;; - Wrap the body at 72 characters
;; - Use the body to explain what and why vs. how



(use-package magit)


;; package to check
;; git-commit
;; diff-hl
;; git-gutter
;; (use-package git-timemachine :defer 1 :diminish) ;; see previous commits
;; (use-package git-commit
;;   :after magit
;;   :hook (git-commit-mode . my/git-commit-auto-fill-everywhere)
;;   :custom (git-commit-summary-max-length 50)
;;   :preface
;;   (defun my/git-commit-auto-fill-everywhere ()
;;     "Ensures that the commit body does not exceed 72 characters."
;;     (setq fill-column 72)
;;     (setq-local comment-auto-fill-only-comments nil)))



(provide 'setup-magit)
;;; setup-magit.el ends here
