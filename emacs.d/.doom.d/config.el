;;; package --- Summary $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; Commentary:

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;; Code:

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Emmanuel GALLOIS"
      user-mail-address "emmanuel.gallois@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:

(setq doom-font (font-spec :family "JetBrainsMono" :size 18)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Notes/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;; sane defaults even for doom emacs
;;;
;;;

(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'set-goal-column  'disabled nil)
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(setq truncate-partial-width-windows nil ; avoid line truncation
      confirm-kill-emacs nil
      size-indication-mode t)

(global-prettify-symbols-mode 1) ;; display “lambda” as “λ”
(global-hl-line-mode t) ;; highlight current line

;; http://thread.gmane.org/gmane.emacs.devel/115520/focus=115794
(setq-default major-mode
              (lambda ()
                (if buffer-file-name
                    (fundamental-mode)
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;;; UI management
;;;; work compmgr
;; should let the window manger handle it again;;;
(set-frame-parameter nil 'alpha nil)
(set-frame-parameter (selected-frame) 'alpha '(100 . 90))
(add-to-list 'default-frame-alist '(alpha . (100 . 90)))
;;;; outshine
(use-package! outshine
  :bind (:map outshine-mode-map
          ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode))
;;;; aggressive-indent
(use-package! aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))
;;;; idle-highlight-mode
(use-package! idle-highlight-mode
  :init (add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))
  :config (idle-highlight-mode t)
  :hook prog-mode
  :diminish)
;;;; emacs
(setq indicate-buffer-boundaries 'left
      indicate-empty-lines t
      show-paren-delay 0.1
      show-paren-style 'expression) ;; parenthesis, expression, mixed.
(set-face-attribute 'show-paren-match nil :background "gold")

(setq-default header-line-format '("%f (%I)" ))
;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("%f (%I) " which-func-format " "))))
;;;; symbol-overlay
;; Toggle all overlays of symbol at point: symbol-overlay-put
;; Jump between locations of symbol at point: symbol-overlay-jump-next & symbol-overlay-jump-prev
;; Switch to the closest symbol highlighted nearby: symbol-overlay-switch-forward & symbol-overlay-switch-backward
;; Minor mode for auto-highlighting symbol at point: symbol-overlay-mode
;; Remove all highlighted symbols in the buffer: symbol-overlay-remove-all
;; Copy symbol at point: symbol-overlay-save-symbol
;; Toggle overlays to be showed in buffer or only in scope: symbol-overlay-toggle-in-scope
;; Jump back to the position before a recent jump: symbol-overlay-echo-mark
;; Jump to the definition of symbol at point: symbol-overlay-jump-to-definition
;; Isearch symbol at point literally, without regexp-quote the symbol: symbol-overlay-isearch-literally
;; Query replace symbol at point: symbol-overlay-query-replace
;; Rename symbol at point on all its occurrences: symbol-overlay-rename
;; (require 'symbol-overlay)
;; (global-set-key (kbd "M-i") 'symbol-overlay-put)
;; (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
;; (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
;; (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
;; (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all)
;; Default key-bindings defined in symbol-overlay-map:
;; "i" -> symbol-overlay-put
;; "n" -> symbol-overlay-jump-next
;; "p" -> symbol-overlay-jump-prev
;; "w" -> symbol-overlay-save-symbol
;; "t" -> symbol-overlay-toggle-in-scope
;; "e" -> symbol-overlay-echo-mark
;; "d" -> symbol-overlay-jump-to-definition
;; "s" -> symbol-overlay-isearch-literally
;; "q" -> symbol-overlay-query-replace
;; "r" -> symbol-overlay-rename
;; You can re-bind the commands to any keys you prefer by simply writing
;;(define-key symbol-overlay-map (kbd "your-prefer-key") 'any-command)
(use-package! symbol-overlay
  :bind (:map symbol-overlay-mode-map
          ("M-h" . symbol-overlay-put)
          ("M-n" . symbol-overlay-jump-next)
          ("M-p" . symbol-overlay-jump-prev))
  :config
  (symbol-overlay-mode))

;;;; whitespace
(use-package! whitespace
  :init
  (dolist (hook '(prog-mode-hook
                  text-mode-hook
                  conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-line-column nil
        whitespace-style '(face tabs newline space-mark trailing tab-mark newline-mark)
        whitespace-display-mappings
        '((space-mark nil)
          (newline-mark 10 [172 10])
          (tab-mark 9 [187 9] [9655 9] [92 9])
          ))
  (setq-default show-trailing-whitespace t)
  (defun no-trailing-whitespace ()
    (setq-local show-trailing-whitespace nil))
  ;; (add-hook! 'minibuffer-setup-hook 'no-trailing-whitespace
  ;;            'gdb-mode-hook 'no-trailing-whitespace
  ;;            'help-mode-hook 'no-trailing-whitespace)
  :diminish whitespace-mode)
;;;; eye candy
(setq indicate-buffer-boundaries 'left
      indicate-empty-lines t)
(use-package! rainbow-mode
  :diminish rainbow-mode)
(use-package! beacon
  :config
  (setq beacon-blink-delay '0.5)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-when-buffer-changes t)
  (setq beacon-color  "gold")
  (beacon-mode))
(use-package! dimmer
  :config
  (dimmer-mode))
;;

;;; window management
;;
;; Notions
;; side-window:
;; side: top bottom left right
;;
;; slot: placement location. if 2 windows have the same slot (and placement),
;; new one will replace the other.
;; LEFT CENTER RIGHT
;; -n      0   +n
;;
;; window-parameters: additional parameters to control window behavior
;;  - no-other-window: do not switch to this window. Not selectable with =C-x o=.
;;  - no-delete-other-windows: this will not be deletable by =C-x 1=.

(use-package! window
  :init
  (setq display-buffer-alist
        '(;; top side window
          ("\\*\\(Flycheck\\|Package-Lint\\).*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . top)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (window-width . 1)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ;; bottom side window
          ("\\*\\(Output\\|Register Preview\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.16)       ; See the :hook
           (side . bottom)
           (slot . -1)
           (window-parameters . ((no-other-window . t))))
          (".*\\*Completions.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . 0)
           (window-parameters . ((no-other-window . t))))
          ("\\*e?shell.*"
           (display-buffer-in-side-window)
           (window-height . 0.16)
           (side . bottom)
           (slot . -5))
          ;; left side window
          ("\\*\\(Help\\|helpful\\).*"
           (display-buffer-in-side-window)
           (window-width . 0.20)       ; See the :hook
           (side . bottom)
           (slot . 2)
           (window-parameters . ((no-other-window . t))))
          ;; right side window
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 0)
           (window-parameters . ((no-other-window . t)
                                 (mode-line-format . (" "
                                                      mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.25)
           (side . right)
           (slot . 1))))
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  :hook ((help-mode . visual-line-mode)
         (custom-mode . visual-line-mode))
  :bind (("s-n" . next-buffer)
         ("s-p" . previous-buffer)
         ("s-o" . other-window)
         ("s-2" . split-window-below)
         ("s-3" . split-window-right)
         ("s-0" . delete-window)
         ("s-1" . delete-other-windows)
         ("s-5" . delete-frame)
         ("C-x +" . balance-windows-area)
         ("<f8>" . window-toggle-side-windows))) ;; useful toggle side-windows visibility

(add-to-list 'display-buffer-alist
             '(".*COMMIT_EDITMSG". ((display-buffer-pop-up-window) .
                                    ((inhibit-same-window . t)))))
;; Here CONDITION is that the buffer name ends with COMMIT_EDITMSG and that is
;; represented by ".*COMMIT_EDITMSG".
;; The FUNCTION to call is display-buffer-pop-up-window as we want to create a
;; window for this buffer. Instead of specifying the function by itself it's put
;; in as a list of a single function (display-buffer-pop-up-window)so that more
;; functions can be easily added to the list if needed in future.
;; The ALIST contains (inhibit-same-window . t) as we don't want this buffer to
;; open in the same window.
;;; search, completion and expansions
;;;; search
(use-package! isearch
  :config
  (setq search-whitespace-regexp ".*?"
        )
  )
;;;; ivy
(use-package! ivy
  :defer t
  :config
  (map! :map ivy-minibuffer-map         ; restore to default
        :e
        "M-o"   'ivy-dispatching-done
        "C-M-o" 'ivy-dispatching-call
        "C-o"   'hydra-ivy/body
        )
  (define-key swiper-map         [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  ;; ivy settings
  (setq ivy-re-builders-alist
        '(
          (ivy-switch-buffer . ivy--regex-plus)
          (swiper            . ivy--regex-plus)
          (t                 . ivy--regex-fuzzy))
        ivy-use-virtual-buffers t ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’
        ivy-count-format "(%d/%d) "
        counsel-find-file-at-point t
        ;; using ivy-format-function-arrow with counsel-yank-pop
        counsel-yank-pop-separator (propertize "\n→\n" 'face `(:foreground "#990000" :bold t :size 28))
        )
  ;; not sure to know what it does?
  (setq +ivy-buffer-preview 'everything)
  ;; ivy-rich
  (setq ivy-rich-abbreviate-paths t
        ivy-rich-switch-buffer-align-virtual-buffer t)
  ;; ivy-posframe
  (setq ivy-posframe-border-width 2
        ivy-posframe-parameters
        '((left-fringe . 5)
          (right-fringe . 5)
          (internal-border-width . 2)
          (font . "Ubuntu Mono 16")
          ))
  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . nil)
          (swiper-isearch . nil)
          (t . ivy-posframe-display-at-frame-center)))
  )

(defun reloading (cmd)
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))
(defun given-file (cmd prompt)
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))
(defun confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))

;; (ivy-add-actions
;;  'counsel-find-file
;;  `(
;;    ("k" ,(reloading #'confirm-delete-file) "delete")
;;    ("m" ,(reloading (given-file #'rename-file "Move")) "move")
;;    ))
;;;; hippie expand - dabbrev expand on steroids
;;;;; defuns
;;;;;; expand using dict
(defun try-expand-by-dict (old)
  ;; old is true if we have already attempted an expansion
  (unless (bound-and-true-p ispell-minor-mode)
    (ispell-minor-mode 1))
  ;; english-words.txt is the fallback dictionary
  (if (not ispell-alternate-dictionary)
      (setq ispell-alternate-dictionary
            (file-truename dicts-english-words)))
  (unless old
    (he-init-string (he-lisp-symbol-beg) (point))
    (if (not (he-string-member he-search-string he-tried-table))
        (setq he-tried-table (cons he-search-string he-tried-table)))
    (setq he-expand-list
          (and (not (equal he-search-string ""))
               (funcall 'ipsell-lookup-words (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
  (if (null he-expand-list)
      (if old (he-reset-string))
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t)
  )
;;
;;;;;; Create a hippie-expand function with a given list of strings
(setq dcsh-command-list
      '("all_registers" "check_design" "check_test" "compile" "current_design" "link"
        "uniquify" "report_timing" "report_clocks" "report_constraint"
        "get_unix_variable" "set_unix_variable" "set_max_fanout" "report_area"
        "all_clocks" "all_inputs" "all_outputs"))
(defun he-dcsh-command-beg ()
  (let ((p))
    (save-excursion
      (backward-word 1)
      (setq p (point)))
    p))

(defun try-expand-dcsh-command (old)
  (unless old
    (he-init-string (he-dcsh-command-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string (mapcar 'list dcsh-command-list))
                          'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    t))

;; another one
(defun my-try-add-expansion (old)
  "Add a new global abbrev"
  (he-init-string (he-dabbrev-beg) (point))
  (or (inverse-add-global-abbrev 1)
	    (he-reset-string)))
;;
(defun my-expand-file-name-at-point ()
  "Use hippie-expand to expand the filename"
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-complete-file-name-partially try-complete-file-name)))
    (call-interactively 'hippie-expand)))
;;;;; settings
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-expand-by-dict
        ))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-<f5>") (make-hippie-expand-function
                                '(try-expand-dcsh-command
                                  try-expand-dabbrev-visible
                                  try-expand-dabbrev
                                  try-expand-dabbrev-all-buffers) t))
;;; navigation
;;;
;; You can use M-. to jump to definitions, and M-, to jump back to places you've
;; looked from.

;; There's M-?, which can be used to find references in a project.

;; You can also use the mark ring to go back to places you've marked (for
;; instance with C-SPC, or by using isearch) before, but this is limited to a
;; buffer. You can pop the ring with C-u C-SPC. This can be handy, when you need
;; to import something you're about to use before you get back to what you were
;; writing as well.
;;;
;;;; bookmarks
(use-package! bookmark+)

;; (You can customize the format of autonamed bookmarks using options
;; bmkp-autoname-bookmark-function and bmkp-autoname-format.)

(defun my-next-line (&optional ARG TRY-VSCROLL)
  "`next-line' followed by `bookmark-bmenu-switch-other-window'."
  (interactive "^p\np")
  (call-interactively #'next-line)
  (bookmark-bmenu-switch-other-window))

(defun my-previous-line (&optional ARG TRY-VSCROLL)
  "`previous-line' followed by `bookmark-bmenu-switch-other-window'."
  (interactive "^p\np")
  (call-interactively #'previous-line)
  (bookmark-bmenu-switch-other-window))

(define-key bookmark-bmenu-mode-map [remap next-line]     'my-next-line)
(define-key bookmark-bmenu-mode-map [remap previous-line] 'my-previous-line)
;;
;;
(defun foo ()
  (let* ((ff    (function-called-at-point))
         (ff    (and ff  (symbolp ff)  (symbol-name ff)))
         (line  (format "%d" (line-number-at-pos))))
    (if ff (concat ff ":" line) line)))
;;  (setq bmkp-new-bookmark-default-names  (list 'foo))

;;; eshell
;;
;; History Interaction
;; !! Repeats the last command
;; !ls  Repeats the last command beginning with ls
;; !?ls Repeats the last command containing ls
;; !ls:n Extract the nth argument from the last command beginning with ls
;; !ls<tab> Using pcomplete, show completion results matches ls
;; ^old^new Quick substitution. Using the last command, replaceold with new and run it again. Appears to be buggy.
;; $_ Returns the last parameter in the last executed command.
;; Eshell also has some support for bash history modifiers (like !!:s/old/new/)
;;
;; To Emacs
;;  cat mylog.log >> #<buffer *scratch*>
;;  echo foo bar baz > #'myvar
;;  echo $(cadr myvar)
;;
(use-package! eshell)
;;; org-mode
;;;; base settings
(defvar org-default-projects-dir  (concat org-directory "projects/")  "Primary GTD directory.")
(defvar org-default-work-dir (concat org-directory "work/") "Directory of shareable notes.")
(defvar org-default-personal-dir  (concat org-directory "personal/")  "Directory of un-shareable, personal notes.")
(defvar org-default-completed-dir  "~/projects/trophies"            "Directory of completed project files.")
(defvar org-default-inbox-file    (concat org-directory "inbox.org")         "New stuff collects in this file.")
(defvar org-default-tasks-file     "~/projects/tasks.org"           "Tasks, TODOs and little projects.")
(defvar org-default-incubate-file  "~/projects/incubate.org"        "Ideas simmering on back burner.")
(defvar org-default-completed-file nil                              "Ideas simmering on back burner.")
(defvar org-default-notes-file     "~/personal/general-notes.org"   "Non-actionable, personal notes.")
(defvar org-default-media-file     "~/projects/media.org"           "White papers and links to other things to check out.")


(defvar org-default-notes-file    (concat org-directory "notes.org") "Personal notes.")

(defvar org-work-journal-file     (concat org-directory "work/journal.org"))
(defvar org-work-howto-file       (concat org-directory "work/howto.org"))
(defvar org-work-tacokit-file     (concat org-directory "work/tacokit.org"))
(defvar org-diary-file            (concat org-directory "personal/life.org"))
(defvar org-personal-buy-log-file (concat org-directory "personal/journal_achats.org"))
(defvar org-personal-stratif-file (concat org-directory "personal/stratif.org"))
(defvar org-personal-account-file (concat org-directory "personal/accounts/ledger.org"))
;; export settings
(defvar org-export-setup-dir             (concat org-directory "_setup/") "Templates for export.")
(defvar org-export-output-directory-prefix "export_" "Prefix of directory used for org export.")

;;(add-to-list 'org-agenda-files org-default-inbox-file)
;;(add-to-list 'org-agenda-files org-default-tasks-file)


;;;; settings
;; Record time and note when a task is completed
(setq org-log-done 'time)
;; Record time and note when the scheduled date of a task is modified
(setq org-log-reschedule 'time)
;; Record time and note when the deadline of a task is modified
(setq org-log-redeadline 'time)
;; Record time and note when clocking out of a task
(setq org-log-clock-out 'time)
;; Record time and note when a task is refiled
(setq org-log-refile 'time)
;; Record note when clocking out
(setq org-log-note-clock-out t)
;; Log everything into the LOGBOOK drawer
(setq org-log-into-drawer t)
;; allow to use shift for selection
(setq org-support-shift-select 'always)
;; no annoying _
(setq org-use-sub-superscripts nil)
;;;; Show headings up to level 2 by default when opening an org files
(setq org-startup-folded 'content)
;; Show inline images by default
(setq org-startup-with-inline-images t)
;; Add more levels to headlines that get displayed with imenu
(setq org-imenu-depth 5)

;;;; workflow
;; #+TODO: TODO IN-PROGRESS WAITING | DONE CANCELED
;; "WAIT(w@/!):
;; "w" fast item selection
;; "@" means to add a note (with time),
;; "!" means to record only the time of the state change.
;; With X and Y being either "@" or "!",
;; "X/Y" means use X when entering the state, and use Y when leaving the state if and only if the *target* state does not define X.
;; You may omit any of the fast-selection key or X or /Y, so WAIT(w@), WAIT(w/@) and WAIT(@/@) are all valid.

(setq org-todo-keywords
      '(
        (sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")
        (sequence "TODO(t@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|" "ASSIGNED(.@/!)" "CANCELLED(x@/!)" "DONE(d@/!)")
        ))



;;;; Agenda
;; week starts on monday
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day 1)
(setq calendar-latitude 48.2
      calendar-longitude 3.2833
      calendar-location-name "Sens, France")
;;
(setq org-agenda-files `(,org-work-journal-file
                         ,org-default-notes-file
                         ,org-diary-file))

(defun org-agenda-cts ()
  (and (eq major-mode 'org-agenda-mode)
       (let ((args (get-text-property
                    (min (1- (point-max)) (point))
                    'org-last-args)))
         (nth 2 args))))

(defhydra hydra-org-agenda-view (:hint none)
  "
_d_: ?d? day        _g_: time grid=?g?  _a_: arch-trees
_w_: ?w? week       _[_: inactive       _A_: arch-files
_t_: ?t? fortnight  _f_: follow=?f?     _r_: clock report=?r?
_m_: ?m? month      _e_: entry text=?e? _D_: include diary=?D?
_y_: ?y? year       _q_: quit           _L__l__c_: log = ?l?"
  ("SPC" org-agenda-reset-view)
  ("d" org-agenda-day-view (if (eq 'day (org-agenda-cts)) "[x]" "[ ]"))
  ("w" org-agenda-week-view (if (eq 'week (org-agenda-cts)) "[x]" "[ ]"))
  ("t" org-agenda-fortnight-view (if (eq 'fortnight (org-agenda-cts)) "[x]" "[ ]"))
  ("m" org-agenda-month-view (if (eq 'month (org-agenda-cts)) "[x]" "[ ]"))
  ("y" org-agenda-year-view (if (eq 'year (org-agenda-cts)) "[x]" "[ ]"))
  ("l" org-agenda-log-mode (format "% -3S" org-agenda-show-log))
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode (format "% -3S" org-agenda-follow-mode))
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode (format "% -3S" org-agenda-clockreport-mode))
  ("e" org-agenda-entry-text-mode (format "% -3S" org-agenda-entry-text-mode))
  ("g" org-agenda-toggle-time-grid (format "% -3S" org-agenda-use-time-grid))
  ("D" org-agenda-toggle-diary (format "% -3S" org-agenda-include-diary))
  ("!" org-agenda-toggle-deadlines)
  ("[" (let ((org-agenda-include-inactive-timestamps t))
         (org-agenda-check-type t 'timeline 'agenda)
         (org-agenda-redo)
         (message "Display now includes inactive timestamps as well")))
  ("q" (message "Abort") :exit t)
  ("v" nil))
;; Recommended binding:
;;(define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)


;;;; capture
;; Invoking the org-capture frame from outside Emacs
;;
;; The simplest way to use the org-capture frame is through the bin/org-capture
;; script. I’d recommend binding a shortcut key to it. If Emacs isn’t running,
;; it will spawn a temporary daemon for you.
;;
;;Alternatively, you can call:
;; +org-capture/open-frame directly, e.g. emacsclient --eval '(+org-capture/open-frame INTIAL-INPUT KEY)'
;;
;;;;; advices and hooks
;;;;;
;;;;;
;;
;; bindsym $mod+shift+o exec emacsclient -c -F '(quote (name . "org-agenda-quickview"))' -e '(org-agenda nil "w")'
;;
;;
;; bindsym $mod+o exec emacsclient -c -F '(quote (name . "org-protocol-capture"))' -e '(org-capture)'
;; bindsym $mod+o exec emacsclient binds Alt-o to launch emacsclient with some flags.
;; -c creates a new frame
;; -F '(quote (name . "org-protocol-capture"))' sets the frame name to “org-protocol-capture”. This will be handy later.
;; -e '(org-capture)' tells emacs to eval org-capture, which will show your capture templates to choose from.
(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "org-protocol-capture" (frame-parameter nil 'name))
      (delete-other-windows)))
(defun tl/post-capture ()
  (if (equal "org-protocol-capture" (frame-parameter nil 'name))
      (delete-frame)))
(add-hook 'org-capture-after-finalize-hook 'tl/post-capture)

;;;;;; doc
;; Capture bookmarklet or function:
;; location.href='org-protocol:///capture?template=c&url='+ encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&body='+ encodeURIComponent(window.getSelection());
;; %:link	URL of the web-page
;; %:description	The title of the web-page
;; %:initial	Selected text.
;;
;; Template expansions
;;
;; %U Inactive timestamp
;; %ˆ{Name} Prompt for something
;; %a Annotation (org-store-link)
;; %i Active region
;; %? Cursor ends up here
;; %:link URL of the web-page
;; %:description The title of the web-page
;; %:initial Selected text.

;; %[file]     Insert the contents of the file given by file.
;; %(sexp)     Evaluate Elisp sexp and replace with the result.
;; For convenience, %:keyword (see below) placeholders
;; within the expression will be expanded prior to this.
;; The sexp must return a string.
;; %<...>      The result of format-time-string on the ... format specification.
;; %t          Timestamp, date only.
;; %T          Timestamp, with date and time.
;; %u, %U      Like the above, but inactive timestamps.
;; %i          Initial content, the region when capture is called while the
;; region is active.
;; The entire text will be indented like %i itself.
;; %a          Annotation, normally the link created with org-store-link.
;; %A          Like %a, but prompt for the description part.
;; %l          Like %a, but only insert the literal link.
;; %c          Current kill ring head.
;; %x          Content of the X clipboard.
;; %k          Title of the currently clocked task.
;; %K          Link to the currently clocked task.
;; %n          User name (taken from user-full-name).
;; %f          File visited by current buffer when org-capture was called.
;; %F          Full path of the file or directory visited by current buffer.
;; %:keyword   Specific information for certain link types, see below.
;; %^g         Prompt for tags, with completion on tags in target file.
;; %^G         Prompt for tags, with completion all tags in all agenda files.
;; %^t         Like %t, but prompt for date.  Similarly %^T, %^u, %^U.
;; You may define a prompt like %^{Birthday}t.
;; %^C         Interactive selection of which kill or clip to use.
;; %^L         Like %^C, but insert as link.
;; %^{prop}p   Prompt the user for a value for property prop.
;; %^{prompt}  prompt the user for a string and replace this sequence with it.
;; You may specify a default value and a completion table with
;; %^{prompt|default|completion2|completion3...}.
;; The arrow keys access a prompt-specific history.
;; %\1 ... %\N Insert the text entered at the Nth %^{prompt}, where N is
;; a number, starting from 1.2
;; %?          After completing the template, position cursor here.
;;
;; Select-from-list prompts:
;;    %^{Tidbit type|quote|zinger|one-liner|textlet}


;;;;;; templates
(after! org-capture
  (setq org-capture-templates
        `(
          ("t" "Talend work" )
          ("ty" "YTB"   item  (file+function ,org-work-journal-file org-find-ytb) "- %?" :jump-to-captured t) ; :tree-type week
          ("tl" "Log"   entry (file+function ,org-work-journal-file org-find-log) " %? %^g")
          ("tt" "Task"  entry (file+function ,org-work-journal-file org-find-log) "**** %T - %^{Activity}  %^g%^{Type}p")
          ("th" "HOWTO" entry (file+function ,org-work-howto-file undx-select-heading) "** %T - %^{Activity}  %^g%^{Type}p")

          ("T" "TODO"  entry (file+headline ,org-default-notes-file "Tasks") "** TODO %? \n SCHEDULED: %^T \n")
          ("e" "Event" entry (file+headline ,org-default-notes-file "Event") "** %? \n %^T \n")
          ("i" "INC" entry (file+headline ,org-default-notes-file "Incidents")
           "** TODO %? \n
  %^{Ticket}p %^{PIN}p %^{Computer}p %^{Location}p \n
")

          ("b" "Book" entry (file+headline ,org-default-notes-file "Books")
           "* read /%^{Title}/ by %^{Author}
:PROPERTIES:
:CREATED: %T
:END:
%\\2, a very cool writer (I always say the same about all authors)%?" :kill-buffer t)

          ("w" "Protocol" entry (file+headline ,org-default-notes-file "Inbox") "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :empty-lines 1)
          ("L" "Link"     entry (file+headline ,org-default-notes-file "Inbox") "* %? [[%:link][%:description]] \nCaptured On: %U" :empty-lines 1)
          ))
;;;;;; clocked
  (add-to-list 'org-capture-templates
               `("c" "Item to Current Clocked Task" item
                 (clock)
                 "%i%?" :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("K" "Kill-ring to Current Clocked Task" plain
                 (clock)
                 "%c" :immediate-finish t :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("C" "Contents to Current Clocked Task" plain
                 (clock)
                 "%i" :immediate-finish t :empty-lines 1))
  )
;;;;

(defun region-to-clocked-task (START END)
  "Copies the selected text from START to END to the currently clocked in org task."
  (interactive "r")
  (org-capture-string (buffer-substring-no-properties start end) "C"))
(global-set-key (kbd "C-<F17>") 'region-to-clocked-task)


;;;;;; Add ID automatically on capture
(add-hook 'org-capture-prepare-finalize-hook 'org-id-store-link)


;;;; hydra
(defhydra hydra-org-refiler (:hint nil)
  "
  ^Navigate^      ^Refile^       ^Move^           ^Update^        ^Go To^        ^Dired^
  ^^^^^^^^^^---------------------------------------------------------------------------------------
  _k_: ↑ previous _t_: tasks     _m X_: projects  _T_: todo task  _g t_: tasks    _g X_: projects
  _j_: ↓ next     _i_: incubate  _m P_: personal  _S_: schedule   _g i_: incubate _g P_: personal
  _c_: archive    _p_: personal  _m T_: work      _D_: deadline   _g x_: inbox    _g T_: work
  _d_: delete     _r_: refile                   _R_: rename     _g n_: notes    _g C_: completed
  "
  ("<up>" org-previous-visible-heading)
  ("<down>" org-next-visible-heading)
  ("k" org-previous-visible-heading)
  ("j" org-next-visible-heading)
  ("c" org-archive-subtree-as-completed)
  ("d" org-cut-subtree)
  ("t" org-refile-to-task)
  ("i" org-refile-to-incubate)
  ("p" org-refile-to-personal-notes)
  ("r" org-refile)
  ("m X" org-refile-to-projects-dir)
  ("m P" org-refile-to-personal-dir)
  ("m T" org-refile-to-work-dir)
  ("T" org-todo)
  ("S" org-schedule)
  ("D" org-deadline)
  ("R" org-rename-header)
  ("g t" (find-file-other-window org-default-tasks-file))
  ("g i" (find-file-other-window org-default-incubate-file))
  ("g x" (find-file-other-window org-default-inbox-file))
  ("g c" (find-file-other-window org-default-completed-file))
  ("g n" (find-file-other-window org-default-notes-file))
  ("g X" (dired org-default-projects-dir))
  ("g P" (dired org-default-personal-dir))
  ("g T" (dired org-default-work-dir))
  ("g C" (dired org-default-completed-dir))
  ("[\t]" (org-cycle))
  ("s" (org-save-all-org-buffers) "save")
  ("q" nil "quit"))
;;
;;
;;
;;;; export
(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory."
  (when (not pub-dir)
    (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))
;;
;;; deft
(after! org
  (setq
   ;;deft-directory (concat org-directory "notes/")
   deft-directory org-directory
   deft-default-extension "org"
   deft-extensions '("org" "md" "rst" "textile" "txt")
   deft-text-mode 'org-mode
   deft-recursive t
   ;;deft-recursive-ignore-dir-regexp ""\\(?:\\.\\|\\.\\.\\|personal\\)$"
   ;;deft-ignore-file-regexp "\(?:^$\)"
   deft-use-filename-as-title t
   deft-file-naming-rules '((noslash . "-")
                            (nospace . "-")
                            (case-fn . downcase))))
;;; rest client
(after! restclient
  (setq restclient-same-buffer-response nil)
  (add-to-list 'company-backends 'company-restclient))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; persistence
;;;
;;;; setup places
;;;;
(defvar undx-tmp-dir "~/tmp/")
(defvar undx-persistence-dir (concat undx-tmp-dir "emacs/"))
(defvar undx-backups-dir (concat undx-persistence-dir "backups/"))
(defvar undx-private-dir "~/Dropbox/undx/private/home/.emacs.d/private/")

;;`(,undx-conf-dir ,undx-local-dir ,undx-private-dir ,undx-tmp-dir ,undx-backups-dir ,undx-persistence-dir)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      )

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq autosavedir
      (concat undx-persistence-dir "auto-saves-list/"))
(unless (file-exists-p autosavedir)
  (mkdir autosavedir))
(setq auto-save-list-file-prefix autosavedir)
(defun make-backup-file-name (FILE)
  "Make a backup of FILE."
  (let ((dirname (concat temporary-file-directory "backups/"
                         (format-time-string "%Y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

;; When you save a file, the auto save file is deleted.
;; if pb M-x recover-file
(add-hook 'auto-save-hook 'org-save-all-org-buffers)


;;;;
;;;; scratch
(use-package! persistent-scratch
  :init
  (setq persistent-scratch-save-file (concat undx-private-dir  (system-name) ".persistent-scratch"))
  :config
  (persistent-scratch-autosave-mode 1)
  (when (file-exists-p persistent-scratch-save-file)
    (persistent-scratch-restore))
  )

(use-package! recentf
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :init
  (setq recentf-save-file (concat undx-persistence-dir  "recentf")
        recentf-exclude '("~$"
                          "/.autosaves/"
                          "/elpa/"
                          "\\.pdfsync$" ; LaTeX
                          "\\.toc" ; LaTeX
                          "\\.aux$" ; LaTeX
                          "\\.elc$"
                          "/emacs.d/url/"
                          ".el.gz$"
                          "\\.ido.last$")
        recentf-max-saved-items 250)
  :config
  (recentf-mode 1))

(setq desktop-dirname             undx-persistence-dir
      desktop-base-file-name      "desktop"
      desktop-base-lock-name      "desktop.lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop t;; was ;nil
      )
(desktop-save-mode t) ; nil for off
;; bookmarks file
(setq bookmark-default-file (concat undx-persistence-dir "bookmarks"))
;;
(setq delete-by-moving-to-trash t)
;;
;;
;;

(use-package! groovy-mode
  :defer t
  :mode "\\.groovy\\'")

(base64-decode-string "d29ya2RheSNXb3JrZGF5I0lucHV0")
;;"workday#Workday#Input"


;;"datastore".equals(nested.getMetadata().get("configurationtype::type"))
;;"dataset".equals(nested.getMetadata().get("configurationtype::type"))


;; require gifsicle
(use-package! gif-screencast
  :bind
  ("<f8>" . gif-screencast-start-or-stop))

;;; dired
;;;; classic dired
(after! dired
  (setq dired-listing-switches "-aBhl  --group-directories-first"
        dired-dwim-target t
        dired-recursive-copies (quote always)
        dired-recursive-deletes (quote top)))
;;;; ranger
(use-package! ranger)
;;; spelling
(defvar dicts "~/.doom.d/dict/")
(defvar dicts-english-words (concat dicts "english-words.txt"))
(use-package! flyspell
  :defer t
  :init
  (ispell-change-dictionary "english")
  (setq ispell-personal-dictionary (concat dicts "personal_english.dic"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (defun undx/switch-dictionary()
    "Switch dictionary."
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (change (if (string= dic "francais") "english" "francais")))
      (setq ispell-personal-dictionary (concat dicts "personal_" change ".dic"))
      (ispell-change-dictionary change)
      (message "Dictionary switched from %s to %s." dic change)
      )))
(defun spell-FR ()
  "ispell change dictionary to French."
  (interactive)
  (ispell-change-dictionary "francais")
  (diminish 'flyspell-mode " ✈F"))
(defun spell-US ()
  "ispell change dictionary to American."
  (interactive)
  (ispell-change-dictionary "american")
  (diminish 'flyspell-mode " ✈E"))

;;; languages
;;;; js
(after! js2-mode
  :init
  (setq flycheck-javascript-eslint-executable "eslint_d"
        flycheck-eslint-args "")
  :config
  )


(defun xrdb-resource ()
  "Execute xrdb - X server resource database utility on saved resource."
  (when (eq major-mode 'conf-xdefaults-mode)
    (shell-command-to-string (format "xrdb %s" buffer-file-name))))
(add-hook 'after-save-hook #'xrdb-resource)


;;; defuns
;;;; editing
;;;;; vim like d-G d-gg
(defun delete-to-end-of-buffer ()
  "Deletes everything from point to 'point-max."
  (interactive)
  (kill-region (point) (point-max)))

(defun delete-to-beginning-of-buffer()
  (interactive)
  (kill-region (point) (point-min)))

(defun copy-current-buffer-name ()
  "Copy the current 'buffer-file-name in the kill ring."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(defun copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring.
Ease of use features:
 - Move to start of next line.
 - Appends the copy on sequential calls.
 - Use newline as last char even on the last line of the buffer.
 - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun simple-copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring."
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(autoload 'copy-from-above-command "misc"
  "Copy characters from previous nonblank line, starting just above point.

  \(fn &optional arg)"
  'interactive)

(defun copy-above-to-char (arg char)
  "Copy all characters from the previous line beginning with the character currently above the cursor up to the ARG th occurrence of CHAR."
  (interactive "p\ncCopy to char: ")
  (let* ((col (current-column))
         (n (save-excursion
              (forward-line -1)
              (move-to-column col)
              (search-forward (char-to-string char)
                              (line-end-position) nil arg)
              (- (current-column) col))))
    (copy-from-above-command n)))
;;;; moving
;;;;; dwim BOL
(defun undx/move-beginning-of-line ()
  "Move to indentation, or beginning of the line."
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (move-beginning-of-line 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://blog.binchen.org/posts/diff-regions-in-emacs.html I know M-x
;; ediff-regions-linewise. But it's kind of too generic. I only want to view
;; the different lines of two regions as quickly as possible.

;;;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E."
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))
    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare."
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a'."
  (interactive)
  (if (region-active-p)
      (let (rlt-buf
            diff-output
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))

        (setq rlt-buf (get-buffer-create "*Diff-region-output*"))
        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb)))
          ;; show the diff output
          (if (string= diff-output "")
              (message "Two regions are SAME!")
            (save-current-buffer
              (switch-to-buffer-other-window rlt-buf)
              (set-buffer rlt-buf)
              (erase-buffer)
              (insert diff-output)
              (diff-mode))))

        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))

;;;;; spelling stuff
;;;; spelling
;;;;; Ispell and Abbrev, the Perfect Auto-Correct
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;;;;; no double capitals
;; http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type/13975#13975
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))
;;(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
;;And the minor mode definition:
(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

;;;; misc functions

(defun write-packages-list ()
  "Writes the month's current packages list."
  (let* ((pl package-activated-list)
         (fpl (concat temporary-file-directory (system-name) ".packageslist-" (format-time-string "%Y%m"))))
    (unless (file-exists-p fpl)
      (message "writing packages in %s" fpl)
      (append-to-file (format "%s" pl) nil fpl))
    ))
(write-packages-list)


;;;; insertions
(defun insert-ts-sec ()
  "Insert TS unix epoq format."
  (interactive)
  (insert (format-time-string "%s")))
(defun insert-ts-iso ()
  "Insert TS ISO format."
  (interactive)
  (insert (format-time-string "%F")))

(defmacro dotfile (filename)
  "Define the function `filename' to edit the dotfile in question"
  (let ((filestr (symbol-name filename)))
    `(progn
       (defun ,(intern filestr) ()
         ,(format "Open %s for editing" filestr)
         (interactive)
         (find-file ,(concat "~/" filestr))))))
(dotfile .emacs)
(dotfile .vimrc)

;;
(defun wikipedia-search (search-term)
  "Search for SEARCH-TERM on wikipedia"
  (interactive
   (let ((term (if mark-active
                   (buffer-substring (region-beginning) (region-end))
                 (word-at-point))))
     (list
      (read-string
       (format "Wikipedia (%s):" term) nil nil term)))
   )
  (browse-url
   (concat
    "http://en.m.wikipedia.org/w/index.php?search="
    search-term
    ))
  )

;;----------------------------------------------------------------------
(defun get-above-char()
  "Get character from a line above"
  (let ( (o) )
    (setq o (- (point) (line-beginning-position)))
    (save-excursion
      (if (> (line-number-at-pos) 1)
          (progn
            (forward-line -1)
            (forward-char o)
            (char-after))))))
;;
(defun get-below-char ()
  "Get character below cursor"
  (let (o m)
    (setq o (- (point) (line-beginning-position)))
    (setq m (count-lines (point-min) (point-max)))
    (save-excursion
      (if (< (line-number-at-pos) m)
          (progn
            (forward-line 1)
            (forward-char o)
            (char-after))))))
;;
(defun copy-above-char()
  "Copy character from a line above"
  (interactive)
  (insert (get-above-char)))
;;
(defun copy-below-char()
  "Copy character below cursor"
  (interactive)
  (insert (get-below-char)))
;;efun move-line-up (
(defun undx/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
;;
(defun undx/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
;; indent whole buffer
(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    ))
;;
(defun switch-to-list-buffers ()
  (interactive)
  (save-excursion
    (list-buffers)
    (switch-to-buffer "*Buffer List*")
    ))
;;
(defun undx/increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))
;;
(defun undx/decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))
;;
(defun undx/increment-number-hexadecimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
              (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))
;;
(defun undx/format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
      (if (equal (mod val 2) 1)
          (setq result (concat "1" result))
        (setq result (concat "0" result)))
      (setq val (/ val 2))
      (setq width (1- width)))
    result))

(defun undx/increment-number-binary (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (my-format-bin answer field-width)))))))

;;
(defun copy-buffer-filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
;;
;;;;; cleanup
(defun undx/buffer-cleanup ()
  "Clean up the buffer"
  (interactive)
  (delete-blank-lines)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))


(defun trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(defun no-trailing-whitespace ()
  (setq-local show-trailing-whitespace nil))





;;;; transposing
;;;;; transpose buffers

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


;;;;; fs ops
(defun undx/ensure-dirs-exist (dirs)
  "Ensures that the directories exist, creates them otherwise."
  (dolist (d dirs ; `(,undx-conf-dir ,undx-local-dir ,undx-private-dir ,undx-tmp-dir ,undx-backups-dir ,undx-persistence-dir)
             )
    (message d)
    (unless (file-exists-p d)
      (mkdir d))
    )
  )
;;;;; org-mode
;;;;;; capture
;;;;;;; work
;; org-capture defuns
(defun org-find-ytb ()
  (org-datetree-find-date-create (calendar-current-date))
  (goto-char (point-at-eol))
  (when (not (re-search-forward
              (format org-complex-heading-regexp-format
                      (regexp-quote "YTB")) nil t))
    (insert "\n**** YTB\nY:\n- \nT:\nB:\n"))
  )

(defun org-find-log ()
  (org-datetree-find-date-create (calendar-current-date))
  (goto-char (point-at-eol))
  (when (not (re-search-forward
              (format org-complex-heading-regexp-format
                      (regexp-quote "LOG")) nil t))
    (insert "\n**** LOG\n"))
                                        ;(goto-char (point-max))
  )

(defun undx-org-find-or-create-headline ()
  ""
  (interactive)

  )

(defun undx-heading-candidates (level)
  (let (candidates)
    (org-map-entries
     (lambda ()
       (let ((comp (org-heading-components)))
         (if (= level (car comp))
             (push
              (nth 4 comp)
              candidates))
         )))
    (message "candidates: %s" candidates)
    (nreverse candidates)))

(defun undx-select-heading ()
  "Jump to a heading with completion."
  (interactive)
  (let ((cands (undx-heading-candidates 1)))
    (ivy-read "Heading: " cands)))


;; (defun bzg-find-location ()
;;   "Example: find my bzg.org file and the abcde string in the current buffer"
;;   (find-file "~/org/bzg.org")
;;   (goto-char (point-min))
;;   (re-search-forward "abcde" nil t)
;;   (newline 2))

;; (defun org-find-heading-in-datetree ()
;;   (org-datetree-find-date-create (calendar-current-date))
;;   (goto-char (point-at-eol))
;;   (when (not (re-search-forward
;;               (format org-complex-heading-regexp-format
;;                       (regexp-quote "Todo today")) nil t))
;;     (insert "\n**** Todo today\n"))
;;   (goto-char (point-max)))

;;;;; other
;; from http://www.howardism.org/Technical/Emacs/capturing-content.html
;;
(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil)))
;;
(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))
;;
(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name   (buffer-file-name))
         (file-base   (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "From [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "From ~%s~ (in [[file:%s::%s][%s]]):"
                                func-name file-name line-number
                                file-base))))
    (format "
   %s

   #+BEGIN_%s %s
%s
   #+END_%s" initial-txt type headers code-snippet type)))

(defun ha/get-linux-clipboard ()
  "Return the clipbaard for a Unix-based system. See `ha/get-clipboard'."
  (destructuring-bind (exit-code contents)
      (shell-command-with-exit-code "xclip" "-o" "-t" "text/html")
    (if (= 0 exit-code)
        (list :html contents)
      (list :text (shell-command-to-string "xclip -o")))))

(defun ha/external-capture-to-org ()
  "Calls `org-capture-string' on the contents of the Apple clipboard."
  (interactive)
  (org-capture-string (ha/org-clipboard) "C")
  (ignore-errors
    (delete-frame)))
;; /usr/local/bin/emacsclient -c -n -e "(ha/external-capture-to-org)"


;;;
;;; keybindings
;;   Control (‘C-’),
;;   Shift (‘S-’),
;;   Meta (‘M-’),
;;   Alt (‘A-’),
;;   Super (‘s-’),
;;   Hyper (‘h-’)
;;;; undefine some keys
(global-unset-key (kbd "<f1>"))
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f3>"))
;;(global-unset-key (kbd "<f4>")) ; let replay macro
(global-unset-key (kbd "<f8>"))
(global-unset-key (kbd "<f9>"))
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "C-z"))
;; get rid of these binding
(dolist (key '("\C-c w 0" "\C-c w 1" "\C-c w 2" "\C-c w 3" "\C-c w 4" "\C-c w 5"
               "\C-c w 6" "\C-c w 7" "\C-c w 8" "\C-c w 9" "\M-o"))
  (global-unset-key key))


(after! persp
  (setq persp-keymap-prefix (kbd "C-c $"))
  )


;;;
(global-set-key [remap move-beginning-of-line] 'undx/move-beginning-of-line)

;; specific maps
;; Escape
(define-key minibuffer-local-map            [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map         [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map    [escape] 'keyboard-escape-quit)
(define-key minibuffer-local-map            (kbd "C-u") 'kill-whole-line)

(global-set-key (kbd "M-<return>") 'comment-indent-new-line)
;; unset for org-mode
(define-key org-mode-map (kbd "M-<return>") 'org-meta-return)

;;
;;
;;;; Function keys
;;;;; F2 - flycheck / flyspell next
(global-set-key (kbd "<f2>") 'flycheck-next-error)
(global-set-key (kbd "S-<f2>") 'flyspell-goto-next-error)
;;;;; F2 - flycheck / flyspell  previous
(global-set-key (kbd "<f3>") 'flycheck-previous-error)
;;;;; F5 - buffers
(global-set-key (kbd "<F5> <F5>") 'buffer-menu)
(global-set-key (kbd "<f5> <F6>") 'switch-to-list-buffers)
(global-set-key (kbd "<F5> a")    'ace-jump-buffer)
(global-set-key (kbd "<f5> k")    'kill-buffer)
(global-set-key (kbd "<f5> e")  'eval-buffer)
(global-set-key (kbd "<f5> c") 'compile)
;;;;; F6 - shells
(global-set-key (kbd "<f6><f6>")   'eshell)
(global-set-key (kbd "<f6><f5>")   'shell)
;;;;; F8 - spelling
(global-set-key (kbd "<f8>") 'undx/switch-dictionary)

;;;; which-key = to redispatch
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
;; (global-set-key (kbd "C-=")  'er/expand-region)
;; (global-set-key (kbd "C--")  'er/contract-region)
;;
;; multiple-cursors
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines) ; When you have an active region that spans multiple lines, the following will add a cursor to each line
;; (global-set-key (kbd "C-S-c C-S-x") 'mc/edit-beginnings-of-lines)
;; (global-set-key (kbd "C-S-c C-S-v") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-unset-key (kbd "C-<down-mouse-1>"))
;; (global-set-key   (kbd "C-<mouse-1>") 'mc/add-cursor-on-click)
;;
;;
;;
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)
;;
;;
;; Transpose stuff with M-t
;; (global-unset-key (kbd "M-t")) ;; which used to be transpose-words
;; (global-set-key (kbd "M-t t") 'transpose-chars)
;; (global-set-key (kbd "M-t l") 'transpose-lines)
;; (global-set-key (kbd "M-t w") 'transpose-words)
;; (global-set-key (kbd "M-t s") 'transpose-sexps)
;; (global-set-key (kbd "M-t p") 'transpose-params)
;;
;;
;;;; "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "M-z") 'zap-up-to-char)
;;
;;
;;;; M-k kills to the left
(global-set-key (kbd "M-k")
                '(lambda ()
                   (interactive)
                   (kill-line 0)))
;;;; vim C-a C-x feature
(global-set-key (kbd "C-c +") 'undx/increment-number-decimal)
(global-set-key (kbd "C-c -") 'undx/decrement-number-decimal)
;; bookmarks
;;
;;;; ivy keybindings
;;
;;(global-set-key (kbd "C-x x") 'swiper);; TODO redefine
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-x b")  'ivy-switch-buffer)
(global-set-key (kbd "C-x B")  'counsel-buffer-or-recentf)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f1> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c C") 'counsel-compile);; no C-c c it's org-capture !!!
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f1> u") 'counsel-unicode-char)
(global-set-key (kbd "<f1> z") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f11>")   'counsel-bookmark)
(global-set-key (kbd "<C-f11>") 'bookmark-set)
(global-set-key (kbd "M-<f11>") 'bm-next)
(global-set-key (kbd "<S-f11>") 'bm-previous)
;;
;;
;;Ivy-based interface to shell and system tools
(global-set-key (kbd "M-i")     'counsel-imenu)
;;Use C-M-j (ivy-immediate-done).
;; Helpful if you want to create a new file, but a file already exists that matches the desired name.

;; (global-set-key "\C-s" 'swiper)
;;(define-key swiper-map         [escape] 'minibuffer-keyboard-quit)
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
;;;; org mode
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c O")
                (lambda () (interactive) (find-file org-default-notes-file)))
;;;; deft
(global-set-key [XF86Tools] 'deft)

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
;; (global-set-key (kbd "C-x p") 'pop-to-mark-command)
;; (global-set-key [(control .)] 'goto-last-change)
;; (global-set-key (kbd "C-M-<left>") 'goto-last-change)
;; (global-set-key (kbd "C-M-<right>") 'goto-last-change-reverse)
;; ;;
;; (global-set-key (kbd "C-<up>")   'sacha/search-word-backward)
;; (global-set-key (kbd "C-<down>") 'sacha/search-word-forward)


;;
;;
;;;; auto-yasnippet
;; doc: https://github.com/abo-abo/auto-yasnippet
;; type:
;;count_of_~red = get_total("~Red");
;;
;; or Inline text
;;`red'_total = get_total("`red'_values"); arbitrary text
;;
;; or multiple placeholders
;;~FooType get~Foo() {
;;// Get the ~foo attribute on this.
;;return this.~foo;
;;}
;;
;; then trigger aya-create
;; after then trigger aya-expand
(which-key-add-key-based-replacements "C-c y" "auto-yasnippet")
(global-set-key (kbd "C-c y c") #'aya-create);; first create template
(global-set-key (kbd "C-c y e") #'aya-expand);; reuse it
(global-set-key (kbd "C-c C-o") #'aya-open-line) ;; to expand
;;
;;
;;
;;;; align-regexp
;;(global-set-key (kbd "C-x l") #'align-regexp)
;;
;;
;;;; copy-as-format
(which-key-add-key-based-replacements "C-c w" "copy-as-format")
(global-set-key (kbd "C-c w k") 'copy-as-format-slack)
(global-set-key (kbd "C-c w g") 'copy-as-format-github)
(global-set-key (kbd "C-c w j") 'copy-as-format-jira)

;;;; Notes
;;;;; search
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
;;;;; defuns
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
;; C-M-x runs eval-defun
;;
;;
;; C-x z runs repeat
;;
;;; FINAL
(add-hook! 'after-init-hook
  (lambda ()
    (message "XXX Loaded init file in %.3fs" emacs-init-time)))


(provide 'config)
;;; config.el ends here
