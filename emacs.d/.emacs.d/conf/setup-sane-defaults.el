;;; setup-sane-defaults.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

;;
;;
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;;
(fset 'yes-or-no-p 'y-or-n-p) ;; "y or n" instead of "yes or no
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq mouse-yank-at-point t)
(setq visible-bell t)
;; tabs behaviour
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;;
(set-default 'fill-column 120)
(setq-default comment-column 70)        ; put comments at 
(eldoc-mode 1)
(setq scroll-preserve-screen-position t)
;;
(setq inhibit-startup-message t)
(setq frame-title-format '("" invocation-name ": "(:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%b"))))
(setq truncate-partial-width-windows nil) ; avoid line truncati
(tool-bar-mode -1)
(menu-bar-mode t)
(scroll-bar-mode -1)
(column-number-mode  1)
(global-font-lock-mode t)
(global-prettify-symbols-mode 1) ;; display “lambda” as “λ”
(global-hl-line-mode t) ;; highlight current line
(setq size-indication-mode t)
;;(setq-default indicate-buffer-boundaries 'right)
(setq-default indicate-empty-lines +1)
;; Show unfinished keystrokes early.
(setq echo-keystrokes 0.1)
;; Search highlight
(setq search-highlight t)
(setq query-replace-highlight t)
;;
;; deletes the region selected... rigth behaviour 4me
(delete-selection-mode 1)
(setq line-move-visual t) ;; ?????
;; ajouter une fin de ligne en fin de fichier
(setq require-final-newline t)
;; les phrases finissent par une espace et pas deux
(setq sentence-end-double-space nil)

(setq comment-multi-line t); continue comments on next-line.



(setq set-mark-command-repeat-pop t)

(provide 'setup-sane-defaults)


;;; setup-sane-defaults.el ends here
