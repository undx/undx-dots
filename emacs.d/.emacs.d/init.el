;;
;;
;; if any error with it, remember to call M-x bug-hunter-init-file e
(setq debug-on-error t)
(setq debug-on-quit t)
;; using bug-hunter for troublesshooting init file:
;; M-x bug-hunter-init-file RET e
;; M-x bug-hunter-init-file RET i ;; interactive mode
;; M-x bug-hunter-init-file RET a (featurep 'cl) RET ;; assertion hunt
;;
;;
(require 'cl-lib)
;;
(defconst emacs-start-time (current-time))
(unless noninteractive
  (message "Loading %s..." load-file-name))
(setq message-log-max 16384)

(let ( (tmpconf (concat user-emacs-directory "conf/")))
  (load-file  (concat tmpconf (system-name) "-local-init.el"))
  (add-to-list 'load-path tmpconf)
  )
;;
;;
;; some specific setups
(require 'setup-paths)
(require 'setup-packages)
(require 'setup-sane-defaults)
(require 'setup-persistence)
(require 'setup-defuns)
;;
(use-package diminish)
;;
;;
(use-package aggressive-indent
  :defer t
  :config
  (global-aggressive-indent-mode 1)
  :diminish " ⌁")
;;
(use-package expand-region)
;;
(use-package multiple-cursors
  :init
  (setq mc/list-file (concat undx-persistence-dir (system-name) ".mc-lists"))
  :config
  (multiple-cursors-mode t))
;;
(use-package duplicate-thing)
;;
(use-package drag-stuff
  :defer t
  :config
  (drag-stuff-global-mode t)
  (add-to-list 'drag-stuff-except-modes 'org-mode)
  :diminish drag-stuff-mode)
;;
(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook
                  text-mode-hook
                  conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column nil)
  (setq whitespace-style '(tabs newline space-mark tab-mark newline-mark face))
  ;;Display pretty things for newlines and tabs (nothing for spaces)
  (setq whitespace-display-mappings
        '((space-mark nil)
          ;; 10 LINE FEED
          (newline-mark 10 [172 10])
          ;; 9 TAB, MIDDLE DOT (tab-mark 9 [183 9] [92 9])
          ;; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
          (tab-mark 9 [187 9] [9655 9] [92 9])
          ))
  (setq-default show-trailing-whitespace nil)
  :diminish whitespace-mode)
;;
(use-package yasnippet
  :config
  (yas-load-directory (concat user-emacs-directory "snippets"))
  (yas-global-mode t)
  :diminish yas-minor-mode)
;;
;;


(defun my/autoinsert-yas-expand() "Replace text in yasnippet template."
       (yas/expand-snippet (buffer-string) (point-min) (point-max)))
(use-package autoinsert
  :init
  (setq auto-insert-query nil
        auto-insert-directory (locate-user-emacs-file "templates")
        auto-insert-alist '(
                            (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                            (("\\.el\\'" . "Emacs Lisp")   . ["template.el" my/autoinsert-yas-expand])
                            (("\\.pl\\'" . "Perl script")  . ["template.pl" my/autoinsert-yas-expand])
                            (("\\.pm\\'" . "Perl module")  . ["template.pm" my/autoinsert-yas-expand])
                            ))
  :config (auto-insert-mode 1))

;; still in testing
(require 'setup-yankpad)
;;
(use-package uniquify)
;;
(use-package goto-chg)
;;
(use-package region-state
  :config
  (region-state-mode 1))
;;
;; spelling configuration
(load "ispell-setup.el")
;;
;;
;;
;; completion system
;;
(require 'setup-expand)
(require 'setup-ivy)
(require 'setup-company)

(setq completion-ignored-extensions
      (append completion-ignored-extensions
              (quote (".regtrans-ms" ".blf" ".dat" "NTUSER.DAT" "ntuser.ini" "ntuser.dat.LOG1" "ntuser.dat.LOG2" ))))

(require 'setup-magit)
(require 'setup-org)



;;
;;
;; Execute a command after saved a specific file.
(defvar *afilename-cmd*
  '(("/home/undx/.Xresources" . "xrdb -merge ~/.Xresources")
    ("/home/undx/.xbindkeysrc" . "xbindkeys -p"))
  "File association list with their respective command.")
(defun my/cmd-after-saved-file ()
  "Execute a command after saved a specific file."
  (let* ((match (assoc (buffer-file-name) *afilename-cmd*)))
    (when match
      (shell-command (cdr match)))))
(add-hook 'after-save-hook 'my/cmd-after-saved-file)
;;
;;
;; Other UI
;;
;;


;; font size according screen resolution
(let ((font-name "Ubuntu Mono")
      (size (if (> (display-pixel-width) 3000) 18 14)))
  (set-face-attribute 'default nil :font (format (concat font-name "-%d") size)))
;; disable linum if lines>1500
(global-linum-mode nil)
(use-package nlinum)
(global-nlinum-mode 1)
;;
(fringe-mode '(12 . 12)) ;; little bit larger than defaults.
;;
;;

;; show-paren-match
;; idle-highligh > region
;; 
(setq show-paren-delay 0.1)
(setq show-paren-style 'expression);; parenthesis, expression, mixed.
(show-paren-mode t)
(set-face-attribute 'show-paren-match nil :background "gold")
;;
;;
;;
;;; dimmer
;;(setq dimmer-fraction 0.5)
;;(dimmer-mode 1)
;;
;;
;; beacon - show cursor when scrolling
(beacon-mode 1)
(diminish beacon-mode "")
;;
;;
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq anzu-cons-mode-line-p nil)
(setq winum-auto-setup-mode-line nil)
;;
;;
;;; winum
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        map))
(require 'winum)
(setq winum-auto-assign-0-to-minibuffer t)
(defun winum-assign-9-to-calculator-8-to-flycheck-errors ()
  (cond
   ((equal (buffer-name) "*Calculator*") 9)
   ((equal (buffer-name) "*Flycheck errors*") 8)))
(defun winum-assign-7-to-neotree ()
  (when (string-match-p (buffer-name) ".*\\*NeoTree\\*.*") 7))
(add-to-list 'winum-assign-functions #'winum-assign-9-to-calculator-8-to-flycheck-errors)
(add-to-list 'winum-assign-functions #'winum-assign-7-to-neotree)
(winum-mode)
;;
;;
;; material
(load-theme 'material-light t)

(setq-default header-line-format '("%f (%I)" ))
;; Show the current function name in the header line
(which-func-mode)
(setq-default header-line-format
              '((which-func-mode ("%f (%I) " which-func-format " "))))
;;

;;
(use-package rainbow-delimiters
  :defer t
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;;
(use-package rainbow-mode
  :defer t
  :diminish rainbow-mode)
;;
;;
;;
;;
;; highlight word at point in the buffer
(use-package idle-highlight-mode 
  :init (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))
  :config (idle-highlight-mode t)
  :diminish)
;;
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
;; language and tools
;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------
(require 'setup-smartparens)
;;
(use-package flycheck
  :init
  (global-flycheck-mode t)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :diminish
  )
;;
(use-package markdown-preview-mode
  :config
  (add-to-list 'markdown-preview-stylesheets
               "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css"))
;;

;;(require 'setup-python)
;;(require 'setup-go)

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-relative-timestamps t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  ;;(global-undo-tree-mode 1)
  :diminish "τ")

;; 
(global-eldoc-mode 1)
(diminish 'eldoc-mode)
(diminish 'auto-fill-function " ﹎")

(setq-default mode-line-position
              '((-3 "%p"); (size-indication-mode ("/" (-4 "%I")))
                " "
                (line-number-mode ("%l" (column-number-mode ":%c")))))

;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
(setq-default major-mode
              (lambda ()
                (if buffer-file-name
                    (fundamental-mode)
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;Smaller compilation buffer
(setq compilation-window-height 14)
(defun my-compilation-hook ()
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

;;-------------------------------------------------- 
;;--------------- hooks ---------------
;;-------------------------------------------------- 
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)
(add-hook 'js2-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook 'trailing-whitespace)
(add-hook 'text-mode-hook #'dubcaps-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'trailing-whitespace)
(add-hook 'prog-mode-hook 'eldoc-mode)
(add-hook 'org-mode-hook  #'dubcaps-mode)
(add-hook 'org-mode-hook  'trailing-whitespace)
(add-hook 'org-mode-hook  (lambda () (linum-mode nil)))
;; to remove a hook
;; (remove-hook 'text-mode-hook 'turn-on-auto-fill)



(add-to-list 'auto-mode-alist '("\\.javajet\\'"   . java-mode))
;; ~/.authinfo.gpg or ~/.authinfo
;; machine your-site.atlassian.net login you@example.com password yourPassword port 80
;;
;;
;;
(require 'server)
(unless (server-running-p)
  (server-start))
;;
;;
;; Custom settings
(when (file-exists-p custom-file)
  (load custom-file))
;;
;; finally we bind keys
;;
(require 'setup-keybinding)
;;
;;
;; main conf file is loaded - housecleaning.
(setq debug-on-error nil)
(setq debug-on-quit nil)
(add-hook 'after-init-hook
          (lambda ()
            (let
                ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
              (message "XXX Loaded init file in %.3fs" elapsed))))

