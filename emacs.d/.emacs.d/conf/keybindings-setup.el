;; ----------------------------------------------------------------------
;; global key bindings
;; ----------------------------------------------------------------------
;; describe-prefix-bindings 
;; --- GLOBAL STUFF ---
(global-set-key (kbd "RET")	       'newline-and-indent) ;; instead of C-j
(global-set-key (kbd "<S-return>") 'insert-empty-line) 
(global-set-key [home]             'beginning-of-line)
(global-set-key [end]              'end-of-line)
;; window stuff 
(global-set-key (kbd "<C-down>")   'shrink-window)
(global-set-key (kbd "<C-left>")   'shrink-window-horizontally)
(global-set-key (kbd "<C-right>")  'enlarge-window-horizontally)
(global-set-key (kbd "<C-up>")     'enlarge-window)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>")'windmove-right)
(global-set-key (kbd "C-c <up>")   'windmove-up)
(global-set-key (kbd "<C-S-up>")   'buf-move-up)
(global-set-key (kbd "<C-S-down>") 'buf-move-down)
(global-set-key (kbd "<C-S-left>") 'buf-move-left)
(global-set-key (kbd "<C-S-right>")'buf-move-right)
;; convenience
;;(global-set-key [(meta shift up)]   'move-line-up)    ; meta <-> control on osx
;;(global-set-key [(meta shift down)] 'move-line-down); meta <-> control on osx

(cond
 ((eq system-type 'darwin)     (global-set-key (kbd "s-SPC") 'just-one-space)))
;; --------------------------
;; Function keys organization
;; --------------------------
;; F1 is help
;; F2 is ???
;; F3 start macro
;; F4 stop/run macro
;; F5 buffers
(global-set-key (kbd "<f5> <f5>") 'buffer-menu)
(global-set-key (kbd "<f5> <f6>") 'switch-to-list-buffers)
(global-set-key (kbd "<f5> a")    'ace-jump-buffer)
(global-set-key (kbd "<f5> k")    'kill-buffer)
;; F6 shells
(global-set-key (kbd "<f6><f6>")   'eshell)
(global-set-key (kbd "<f6><f5>")   'shell)
;; F7 often accessed files (init.el, org-mode-setup.el, etc.)
(global-set-key (kbd "<f7> i") 'open-emacsd-init)
(global-set-key (kbd "<f7> o") 'open-emacsd-elisp-org-mode)
;; F8 recent-files
(global-set-key (kbd "<f8> <f8>") 'recentf-open-files)
(global-set-key (kbd "<f8> <f7>") 'deft)
;; (global-set-key [f8] 'deft)
;; F9 Tools : org-mode, calc.
(global-set-key (kbd "<f9> <f9>") 'org-capture)
(global-set-key (kbd "<f9> a")    'org-agenda)
(global-set-key (kbd "<f9> c")    'calc)
;; should put this in abbrevs !!! TODO
(global-set-key (kbd "<f9> <return>") 'xah-run-current-file)
(global-set-key (kbd "<f9> <left>")  "←")
(global-set-key (kbd "<f9> <right>") "→")
(global-set-key (kbd "<f9> <up>")    "↑")
(global-set-key (kbd "<f9> <down>")  "↓")
(global-set-key (kbd "<f9> a") "α")
(global-set-key (kbd "<f9> b") "β")
(global-set-key (kbd "<f9> t") "θ")
;; F10 is menu bullshit, using comments instead;'comment-dwim) ; 'comment-region)
(global-set-key (kbd "<f10>") 'comment-or-uncomment-region)
(global-set-key (kbd "M-;") #'endless/comment-line)

;; F11 is fullscreen mode on osx (global-set-key (kbd "<F11>") 'compile)
;; F12 Buffer actions & configuration
(global-set-key (kbd "<f12> <f5>")  'eval-buffer) ;
(global-set-key (kbd "<f12> <f10>") 'compile)
(global-set-key (kbd "<f12> <f12>") 'indent-buffer) ;; M-C-\ runs the command indent-region
(global-set-key (kbd "<f12> c")     'flycheck-mode)
(global-set-key (kbd "<f12> l")     'linum-mode)
(global-set-key (kbd "<f12> r")     'rainbow-mode)
(global-set-key (kbd "<f12> s")     'flyspell-mode)
(global-set-key (kbd "<f12> v")     'visual-line-mode)
(global-set-key (kbd "<f12> w")     'whitespace-mode)
;; ---------------------
;; Control stuff
;; ---------------------
;; Emacs 24.4 features
;; Emacs 24.4 has a new command isearch-forward-symbol-at-point 【Alt+s .】.
;; It'll interactive search the word under cursor.
;; (【Ctrl+s】 for next occurrence, 【Ctrl+r】 for previous occurrence.)
;;emacs search ＆ highlight commands key	command
;; 【Alt+s .】	isearch-forward-symbol-at-point
;; 【Alt+s _】	isearch-forward-symbol
;; 【Alt+s o】	occur (same as list-matching-lines)
;; 【Alt+s w】	isearch-forward-word
;; 【Alt+s h .】	highlight-symbol-at-point
;; 【Alt+s h f】	hi-lock-find-patterns
;; 【Alt+s h l】	highlight-lines-matching-regexp
;; 【Alt+s h p】	highlight-phrase
;; 【Alt+s h r】	highlight-regexp
;; 【Alt+s h u】	unhighlight-regexp
;; 【Alt+s h w】	hi-lock-write-interactive-patterns
;; 【Ctrl+s】	isearch-forward

;; delete-duplicate-lines
;; rectangular selection
;; New command C-x SPC (rectangle-mark-mode) makes a rectangular region.
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "<C-tab>") 'helm-mini)
(global-set-key (kbd "C-.")     'ace-jump-mode)
(global-set-key (kbd "C-;")     'ace-jump-buffer)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-c SPC") 'ace-jump-mode-pop-mark)
;; vim i_CTRL-Y and i_CTRL-E behaviour
(global-set-key (kbd "C-x C-v") 'copy-below-char)
(global-set-key (kbd "C-x C-y") 'copy-above-char)
;;
(global-set-key (kbd "C-x \\") 'align-regexp)
;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)
;; multiple cursors - to quit <return> or <C-g>
(global-set-key (kbd "C-<")           'mc/mark-previous-like-this)
(global-set-key (kbd "C->")           'mc/mark-next-like-this)
(global-set-key (kbd "C-S-c C-S-c") 	'mc/edit-lines)
(global-set-key (kbd "C-c C-<")	  		'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
;; vim C-a C-x feature
(global-set-key (kbd "C-c +")         'my-increment-number-decimal)
(global-set-key (kbd "C-c -")         'my-decrement-number-decimal)
;;
(global-set-key (kbd "C-z")         'undo)
;; vim C-a C-x feature
(global-set-key (kbd "C-c +")         'my-increment-number-decimal)
(global-set-key (kbd "C-c -")         'my-decrement-number-decimal)
;;
(global-set-key (kbd "M-#")           'calc)
(global-set-key (kbd "M-/")           'hippie-expand)
(global-set-key (kbd "M-c")           'duplicate-thing)

(require 'flycheck)
                                        ;(define-key flyspell-mode-map (kbd "M-n") 'flyspell-goto-next-error) ;(define-key flyspell-mode-map (kbd "M-.") 'ispell-word)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)
;; C-c ! c flycheck-buffer
;; C-c ! l flycheck-list-errors
;; C-c ! n flycheck-next-error
;; C-c ! p flycheck-previous-error to navigate between error locations.

(global-set-key (kbd "H-n") 'flyspell-goto-next-error)
(global-set-key (kbd "H-p") 'flyspell-goto-next-error)
(define-key flyspell-mode-map (kbd "H-.") 'ispell-word)

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key (kbd "M-J") (lambda () (interactive) (join-line +1)))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-Y") 'yank-pop)
(global-set-key (kbd "M-z") 'zap-up-to-char)
;; old deprecated stuff still available.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; specific maps
;; Escape
(define-key minibuffer-local-map            [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map         [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map    [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-map            (kbd "C-u") 'kill-whole-line)
;; ---------------------
;; and remember this usefull bindings
;; C-M-h         'mark-defun
;; C-S-Backspace 'kill-whole-line
;; C-M-f runs forward-sexp, move forward over a balanced expression that can be a pair or a symbol.
;; C-M-b runs backward-sexp, move backward over a balanced expression that can be a pair or a symbol.
;; C-M-k runs kill-sexp, kill balanced expression forward that can be a pair or a symbol.
;; C-M-<SPC> or C-M-@ rusn mark-sexp, put mark after following expression that can be a pair or a symbol.
;; C-M-a runs beginning-of-defun, which moves point to beginning of a function.
;; C-M-e runs end-of-defun, which moves point to end of a function.
;; C-M-h runs mark-defun, which put a region around whole current or following function.


;; modes and specific setups




(provide 'keybindings-setup)
