;;; setup-python.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:


;; Code Navigation (using rope, jedi, python.el, find-file-in-project, and idomenu) Quickly jump to the definition of a function or class (M-.) or get an overview of the definitions in the current file (C-c C-o). You can also select any file in your project using ido’s completion (C-c C-f).

;;Inline Documentation (using rope, jedi or pydoc): Read documentation for an object at point with a quick key shortcut (C-c C-d).

;;Powerful code refactoring (using rope): Use C-c C-r to bring up a context-sensitive refactoring dialog. Use powerful refactoring options on symbols, modules, or regions.


;; M-left (elpy-nav-indent-shift-left)
;; M-right (elpy-nav-indent-shift-right)

;; pip install jedi flake8 autopep8
(use-package elpy :ensure t)
(elpy-enable)

;; C-c C-v run python check
;; C-c C-< shift indentation left
;; C-c C-> shift indentation right

(eval-after-load "elpy"
  '(cl-dolist (key '("M-<tab>" "M-<up>" "M-<down>"));; "M-<left>" "M-<right>"))
     (define-key elpy-mode-map (kbd key) nil)))

(setq tab-width 4)
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")
;(add-hook 'before-save-hook 'elpy-format-code)
(provide 'setup-python)

;;; setup-python.el ends here
