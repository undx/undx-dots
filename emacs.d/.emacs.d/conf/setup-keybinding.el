;;; setup-keybinding.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;   Control (‘C-’),
;;   Shift (‘S-’),
;;   Meta (‘M-’),
;;   Alt (‘A-’),
;;   Super (‘s-’),
;;   Hyper (‘h-’)
;;


;;; Code:

;;
;; undefine some keys
;;
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-z"))
;;
;;
;;;
(global-set-key [remap move-beginning-of-line] 'my/move-beginning-of-line)
(global-set-key (kbd "M-<return>") 'comment-indent-new-line)
;; unset for org-mode
(define-key org-mode-map (kbd "M-<return>") 'org-meta-return)
;;
;;
(use-package which-key
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.4
        which-key-side-window-max-height 0.5
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-key-replacement-alist
        '(("<\\([[:alnum:]-]+\\)>" . "\\1")
          ("up"                    . "↑")
          ("right"                 . "→")
          ("down"                  . "↓")
          ("left"                  . "←")
          ("DEL"                   . "⌫")
          ("deletechar"            . "⌦")
          ("RET"                   . "⏎"))
        which-key-description-replacement-alist
        '(("Prefix Command" . "prefix")
          ;; Lambdas
          ("\\`\\?\\?\\'"   . "λ")
          ;; Prettify hydra entry points
          ("/body\\'"       . "|=")
          ;; Drop/shorten package prefixes
          ("projectile-"     . "prj-")
          ("Org"             . "O")
          ("\\(fly\\)?spell" . "✈")
          ("magit-"          . "GIT"))
        )
  :diminish which-key-mode)

;;; to redispatch
(which-key-add-key-based-replacements "C-x a" "abbrev")
(which-key-add-key-based-replacements "C-x g" "magit")
(which-key-add-key-based-replacements "C-x n" "narrowing")
(which-key-add-key-based-replacements "C-x r" "register/rectangle")
(which-key-add-key-based-replacements "C-x X" "debug")
(which-key-add-key-based-replacements "C-c !" "flycheck")
(which-key-add-key-based-replacements "C-c &" "yas")
(which-key-add-key-based-replacements "C-c w" "windows")
(which-key-add-key-based-replacements "C-x 8" "unicode")
(which-key-add-key-based-replacements "C-x RET" "coding")
(which-key-add-key-based-replacements "C-c T" "Text cleanup")
;;
;;
;;
;; restore eval and print in emacs-lisp-mode
(define-key emacs-lisp-mode-map (kbd "C-j") 'eval-print-last-sexp)

;;
;;
;; VCS operations
;;
(global-set-key (kbd "<f9> b d")
                (lambda ()
                  (interactive)
                  (diff-buffer-with-file (current-buffer))
                  )
                )
(global-set-key (kbd "<f9> b D") 'diff-buffer-with-file)
(which-key-add-key-based-replacements "<f9> b" "VCS Buffer")
(which-key-add-key-based-replacements "<f9> b d" "diff buffer with associated file")
(which-key-add-key-based-replacements "<f9> b D" "diff buffer with file")
;;
;; Some views
;;
(global-set-key (kbd "<f10> e") 'flycheck-list-errors)
(global-set-key (kbd "<f10> m") 'menu-bar-open)
;;
;;
(global-set-key (kbd "M-/") 'hippie-expand)
;;
(global-set-key (kbd "C-<tab>") 'ivy-switch-buffer)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-J") (lambda () (interactive) (join-line +1)))
(global-set-key (kbd "M-c") 'duplicate-thing)
;;

(drag-stuff-global-mode t)
(global-set-key (kbd "M-<up>")    #'drag-stuff-up)
(global-set-key (kbd "M-<down>")  #'drag-stuff-down)
(global-set-key (kbd "M-<left>")  #'drag-stuff-left)
(global-set-key (kbd "M-<right>") #'drag-stuff-right)
(add-to-list 'drag-stuff-except-modes 'org-mode)
;;
;; expand-region
;;
(global-set-key (kbd "C-=")  'er/expand-region)
(global-set-key (kbd "C--")  'er/contract-region)
;;
;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) ; When you have an active region that spans multiple lines, the following will add a cursor to each line
(global-set-key (kbd "C-S-c C-S-x") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-S-c C-S-v") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "C-<down-mouse-1>"))
(global-set-key   (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
;;
;;
;;
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)
;;
;;
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t t") 'transpose-chars)
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)
;;
;;
;; zap-up-to-char stops before char <> zap-to-char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)
;;
;;
;;M-k kills to the left
(global-set-key (kbd "M-k")
                '(lambda ()
                   (interactive)
                   (kill-line 0)))
;; bookmarks
;;
;; ivy keybindings
;;
(global-set-key (kbd "C-x x") 'swiper);; TODO redefine
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x B")  'ivy-switch-buffer-other-window)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c C") 'counsel-compile);; no C-c c it's org-capture !!!
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f1> u") 'counsel-unicode-char)
(global-set-key (kbd "<f1> z") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2>")   'counsel-bookmark)
(global-set-key (kbd "<C-f2>") 'bookmark-jump)
(global-set-key (kbd "M-<f2>") 'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
;;
;;
;;Ivy-based interface to shell and system tools
(global-set-key (kbd "M-i")     'counsel-imenu)
;;Use C-M-j (ivy-immediate-done).
;; Helpful if you want to create a new file, but a file already exists that matches the desired name.

;; (global-set-key "\C-s" 'swiper)
(define-key swiper-map         [escape] 'minibuffer-keyboard-quit)
(define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
;; (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; see https://oremacs.com/swiper/#minibuffer-key-bindings
;; Key bindings for single selection, action, then exit minibuffer
;;
;; C-m or RET (ivy-done) : Calls the default action and then exits the minibuffer.
;; M-o (ivy-dispatching-done):  Presents valid actions from which to choose. When only one action is available, there is no difference between M-o and C-m.
;; C-j (ivy-alt-done) : When completing file names, selects the current directory candidate and starts a new completion session there. Otherwise, it is the same as ivy-done.
;; TAB (ivy-partial-or-done) : Attempts partial completion, extending current input as much as possible. TAB TAB is the same as C-j (ivy-alt-done).
;; C-M-j (ivy-immediate-done): Exits with the current input instead of the current candidate (like other commands).
;;This is useful e.g. when you call find-file to create a new file, but the desired name matches an existing file. In that case, using C-j would select that existing file, which isn't what you want - use this command instead.
;; C-' (ivy-avy): Uses avy to select one of the candidates on the current candidate page. This can often be faster than multiple C-n or C-p keystrokes followed by C-m.



;;
;;
;;; org mode bindings
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(define-key org-jira-entry-mode-map (kbd "<f6>") 'hydra-jira/body)
;; (global-set-key (kbd "C-c o") 
;;                 (lambda () (interactive) (find-file org-default-notes-file)))

;; ;; documents management
;; (global-set-key [f8] 'deft)


;; to check and cleanup
;; (global-set-key (kbd "C-c T b") 'cleanup-buffer-safe)
;; (global-set-key (kbd "C-c T w") 'toggle-show-trailing-whitespace)
;; (global-set-key (kbd "C-c T W") 'whitespace-cleanup)
;; (global-set-key (kbd "C-c T f") 'auto-fill-mode)
;; ;;
;; (global-unset-key (kbd "<f2>"))
;; (global-set-key (kbd "<f2> T b") 'cleanup-buffer-safe)
;; (global-set-key (kbd "<f2> T w") 'toggle-show-trailing-whitespace)
;; (global-set-key (kbd "<f2> T W") 'whitespace-cleanup)
;; (global-set-key (kbd "<f2> T f") 'auto-fill-mode)
;; ;; spelling
;; (global-set-key (kbd "<f2> s a") 'spell-US)
;; (global-set-key (kbd "<f2> s f") 'spell-FR)
;; (global-set-key (kbd "<f2> s p") 'spell-PT)
;; (global-set-key (kbd "<f2> s r") 'flyspell-region)
;; (global-set-key (kbd "<f2> s b") 'flyspell-buffer)
;; (global-set-key (kbd "<f2> s s") 'flyspell-mode)
;; ;; running
;; (global-set-key [remap move-beginning-of-line] 'my/move-beginning-of-line)
;; (global-set-key (kbd "C-x p") 'pop-to-mark-command)
;; (global-set-key [(control .)] 'goto-last-change)
;; (global-set-key (kbd "C-M-<left>") 'goto-last-change)
;; (global-set-key (kbd "C-M-<right>") 'goto-last-change-reverse)
;; ;;
;; (global-set-key (kbd "C-<up>")   'sacha/search-word-backward)
;; (global-set-key (kbd "C-<down>") 'sacha/search-word-forward)


;;
;;
;;; auto-yasnippet
;; doc: https://github.com/abo-abo/auto-yasnippet
(which-key-add-key-based-replacements "C-c y" "auto-yasnippet")
(global-set-key (kbd "C-c y c") #'aya-create);; first create template
(global-set-key (kbd "C-c y e") #'aya-expand);; reuse it
(global-set-key (kbd "C-o") #'aya-open-line) ;; to expand
;;
;;
;; yankpad
;; https://github.com/Kungsgeten/yankpad
(global-set-key (kbd "C-c y C") 'yankpad-set-category)
(global-set-key (kbd "C-c y m") 'yankpad-map)
(global-set-key (kbd "C-c y x") 'yankpad-expand)
(global-set-key (kbd "C-c y i") 'yankpad-insert)

;;
;;
;; hydra and cie
(global-set-key (kbd "C-M-m") 'major-mode-hydra)

;;
;;
;;; helpful
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)
(global-set-key (kbd "C-h C") #'helpful-command)
;;
;;
;;; align-regexp
;;(global-set-key (kbd "C-x l") #'align-regexp)
;;
;;
;;; copy-as-format
(which-key-add-key-based-replacements "C-c w" "copy-as-format")
(global-set-key (kbd "C-c w s") 'copy-as-format-slack)
(global-set-key (kbd "C-c w g") 'copy-as-format-github)
(global-set-key (kbd "C-c w j") 'copy-as-format-jira)
;;
;;
;;
(provide 'setup-keybinding)
;;; setup-keybinding.el ends here
