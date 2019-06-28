;;; setup-ido.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package flx-ido :ensure t)
(use-package ido
  :init
  (setq ido-enable-flex-matching t
        ido-everywhere t)
  (setq ido-save-directory-list-file
        (concat undx-persistence-dir "ido-last"))
  (ido-mode 1)
  (flx-ido-mode 1)
  (setq ido-use-faces nil ;; disable ido faces to see flx highlights.
        ido-use-virtual-buffers t ;; use recentf in buffer switch
        ido-use-filename-at-point 'guess
        ido-create-new-buffer 'always
        ido-ignore-extensions t ;; use completion-ignored-extensions
        ido-file-extensions-order '(".org" ".rb" ".el" ".txt" ".py" ".emacs" ".java" ".xml" ".ini" ".cfg" ".cnf" ".html")))

(use-package ido-completing-read+ :ensure t
  :config (ido-ubiquitous-mode 1))
(use-package crm-custom :ensure t :config (crm-custom-mode 1))
(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-show-count t
        ido-use-faces t
        ;; up and down to navigate the options
        ;; ido-vertical-define-keys 'C-n-C-p-up-and-down
        ;; plus left and right to move through the history/directories.
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right
        ))
(use-package idomenu
  :ensure t
  :bind ("C-c i" . idomenu))
;; get rid of ido when writing file.
(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(setq org-completion-use-ido t)
(setq magit-completing-read-function 'magit-ido-completing-read)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)



(use-package smex
  :ensure t
  :config
  (setq smex-save-file (concat undx-persistence-dir "smex-items"))
  )
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)




(provide 'setup-ido)


;;; setup-ido.el ends here
