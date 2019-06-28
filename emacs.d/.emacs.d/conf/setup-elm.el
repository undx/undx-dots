;;; setup-elm.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package elm-mode :ensure t :config (elm-mode))
(add-hook 'before-save-hook 'elm-format)
(provide 'setup-elm)
;;; setup-elm.el ends here
