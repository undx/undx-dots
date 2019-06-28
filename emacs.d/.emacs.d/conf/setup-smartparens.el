;;; setup-smartparens.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package smartparens
  :diminish " φ"
  :config
  (require 'smartparens-config)
  (require 'smartparens-ruby)
  (require 'smartparens-html)
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  ;;
  (add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)
  ;;
  (sp-local-pair 'emacs-lisp-mode "`'" "'")
  (sp-local-pair 'ruby-mode "`" "`")
  (sp-local-pair 'org-mode "*" "*" :actions '(wrap));; bold
  (sp-local-pair 'org-mode "/" "/" :actions '(wrap));; italic
  (sp-local-pair 'org-mode "_" "_");; underline
  (sp-local-pair 'org-mode "+" "+");; strike-through
  (sp-local-pair 'org-mode "=" "=");; verbatim
  (sp-local-pair 'org-mode "~" "~");; code
  ;;
  (sp-pair "(" ")")
  (sp-pair "[" "]")
  (sp-pair "'" "'" :actions '(wrap))          ;; only use '' pair for wrapping
  (sp-pair "%" "%" :actions '(insert))        ;; only use %% pair for auto insertion, never for wrapping
  (sp-pair "(" ")" :actions '(wrap insert))   ;; use () pair for both actions. This is default for each new pair
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil);; no '' pair in emacs-lisp-mode
  (sp-local-pair 'org-mode "\\left(" "\\right)" :insert "C-b l" :trigger "\\l(")
  ;;(sp-pair "'" nil :actions :rem)
  ;; The '' pair will autopair UNLESS the point is right after a word,
  ;; in which case you want to insert a single apostrophe.
  ;;(sp-pair "\"" nil :unless '(sp-point-after-word-p))
  (sp-local-tag 'org-mode "s" "```scheme" "```")
  (sp-local-tag 'html-mode "b" "<span class=\"bold\">" "</span>")
  (sp-with-modes '(rhtml-mode)
    (sp-local-pair "<" ">")
    (sp-local-pair "<%" "%>"))
  ;;(sp-local-pair 'ruby-mode "#{" "}")
  (sp-with-modes '(web-mode)
    (sp-local-pair "%" "%"
                   :unless '(sp-in-string-p)
                   :post-handlers '(((lambda (&rest _ignored)
                                       (just-one-space)
                                       (save-excursion (insert " ")))
                                     "SPC" "=" "#")))
    (sp-local-pair "<% "  " %>" :insert "C-c %")
    (sp-local-pair "<%= " " %>" :insert "C-c =")
    (sp-local-pair "<%# " " %>" :insert "C-c #")
    (sp-local-tag "%" "<% "  " %>")
    (sp-local-tag "=" "<%= " " %>")
    (sp-local-tag "#" "<%# " " %>"))
  )
(defun sp-wrap-with-parens(arg)(interactive "P")(sp-wrap-with-pair "("))
(defun sp-wrap-with-brackets(arg)(interactive "P")(sp-wrap-with-pair "["))
(defun sp-wrap-with-braces(arg)(interactive "P")(sp-wrap-with-pair "{"))
(defun sp-wrap-with-single-quotes(arg)(interactive "P")(sp-wrap-with-pair "'"))
(defun sp-wrap-with-double-quotes(arg)(interactive "P")(sp-wrap-with-pair "\""))
(defun sp-wrap-with-underscores(arg)(interactive "P")(sp-wrap-with-pair "_"))
(defun sp-wrap-with-back-quotes(arg)(interactive "P") (sp-wrap-with-pair "`"))

(provide 'setup-smartparens)
;;; setup-smartparens.el ends here
