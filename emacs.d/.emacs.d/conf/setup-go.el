;;; setup-go.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:
;; go get golang.org/x/tools/cmd/godoc
;; go get golang.org/x/tools/cmd/goimports
;; go get golang.org/x/tools/cmd/guru
;; go get golang.org/x/lint/golint
;; go get github.com/rogpeppe/godef
;; go get github.com/nsf/gocode
;; go get github.com/kisielk/errcheck
;;
;; go get -u all
;; 
(use-package go-mode     :ensure t)
(use-package go-eldoc    :ensure t)
(use-package company-go  :ensure t)
(use-package go-guru     :ensure t)
(use-package go-errcheck :ensure t)
(add-to-list 'exec-path "~/Code/go/bin" t)
;;
;; beginning-of-defun (C-M-a)
;; end-of-defun (C-M-e)
;;
;; Jump to definition:
;; When point is atop a function name, press M-.
;; This should open the file where it’s defined and seek to the definition
;; To return back to original place press M-,

;; Use goimports instead of go-fmt for formatting with intelligent package addition/removal

(add-hook 'go-mode-hook (lambda ()
                          (setq indent-tabs-mode t)
                          (setq tab-width 4)
                          ;;
                          (setq gofmt-command "goimports")
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          ;; syntax highlight
                          (go-guru-hl-identifier-mode)
                          (go-eldoc-setup)
                          (message "applying go-mode hook")
                          ;; setting company-go mode...
                          (setq company-tooltip-limit 20)
                          (setq company-idle-delay .3)
                          (setq company-echo-delay 0)
                          (setq company-begin-commands '(self-insert-command))
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          ;;
                          (local-set-key (kbd "M-.") #'godef-jump)
                          (local-set-key (kbd "C-c c c")
                                         (lambda () (interactive)
                                           (compile "go build")))
                          ;;setting go-eldoc
                          (set-face-attribute 'eldoc-highlight-function-argument nil
                                              :underline t :foreground "green"
                                              :weight 'bold)
                          ))


;;
(provide 'setup-go)
;;; setup-go.el ends here
