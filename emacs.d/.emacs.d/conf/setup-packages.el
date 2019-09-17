;;; setup-packages.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'package)
(package-initialize nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/"))

;; bug fixed in emacs 26.3 so keep it until got this release
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;
(package-initialize t)
(setq package-enable-at-startup nil)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(defvar my/install-packages
  '(
    aggressive-indent
    all-the-icons
    auto-yasnippet
    beacon
    bug-hunter
    company
    company-flx
    company-quickhelp
    copy-as-format
    counsel
    deft
    diminish
;;    dimmer
    drag-stuff
    duplicate-thing
    evil
    expand-region
    excorporate
    flx
    flycheck
    goto-chg
    helpful
    hydra
    i3wm
    idle-highlight-mode
    ivy-hydra
    ivy-rich
    language-detection
    magit
    major-mode-hydra
    markdown-mode
    markdown-preview-mode
    material-theme
    multiple-cursors
    nlinum
    orca
    org-bullets
    org-jira
    ox-jira
    persistent-scratch
    pos-tip
    projectile
    rainbow-delimiters
    rainbow-mode
    region-state
    smartparens
    smex
    spaceline-all-the-icons
    undo-tree
    use-package
    which-key
    winum
    yankpad
    yasnippet
    yasnippet-snippets
    ))

(defvar packages-refreshed? nil)
(dolist (pack my/install-packages)
  (unless (package-installed-p pack)
    (unless packages-refreshed?
      (package-refresh-contents)
      (setq packages-refreshed? t))
    (unwind-protect
        (condition-case ex
            (package-install pack)
          ('error (message "Failed to install package [%s], caught exception: [%s]" pack ex)))
      (message "Installed %s" pack))))
(defvar use-package-verbose t)
;;
(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages... %.3fs since startup." elapsed))
;;
(provide 'setup-packages)
;;; setup-packages.el ends here
