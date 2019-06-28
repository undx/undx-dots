;;; setup-ivy.el ---  -*- lexical-binding: t; -*-

;;; Commentary:

;; Ivy, a generic completion mechanism for Emacs.
;; Counsel, a collection of Ivy-enhanced versions of common Emacs commands.
;; Swiper, an Ivy-enhanced alternative to isearch.

;; In Isearch, pressing C-w will insert the word-at-point into the minibuffer.
;; The same can be done in Ivy with M-j (ivy-yank-word) for any command, not just swiper.
;; On a related note, you can paste symbol-at-point into the search with M-n,
;; which is a common case for using C-w in Isearch.

;; (ivy-read "Pick:" (mapcar #'number-to-string (number-sequence 1 10)))
;; (ivy-read "My buffers: " (mapcar #'buffer-name (buffer-list)))
;;
;; ** Saving the current completion session to a buffer
;; - C-c C-o (ivy-occur) : Saves the current candidates to a new buffer and exits completion. The new buffer is read-only and has a few useful bindings defined.
;; - RET or j (ivy-occur-press) : Call the current action on the selected candidate.
;; - mouse-1 (ivy-occur-click) : Call the current action on the selected candidate.
;; - j (next-line) : Move to next line.
;; - k (previous-line) : Move to previous line.
;; - a (ivy-occur-read-action) : Read an action and make it current for this buffer.
;; - o (ivy-occur-dispatch) : Read an action and call it on the selected candidate.
;; - q (quit-window) : Bury the current buffer.

;; I call counsel-ag, to search for the string I want to modify
;; in the current directory. By default, ag (and counsel-ag) will
;; ignore files ignored by git.
;; ----
;;Call counsel-ag and type edit
;; -----
;;I press C-c C-o (ivy-occur) in the search result. It opens an ivy-occur buffer.
;; ----
;; I switch to this buffer and press C-x C-q (ivy-wgrep-change-to-wgrep-mode)
;; to edit it. I can now change each variable name globally using normal
;; search and replace techniques. I use Evil-ex commands :%s/pattern/replace/g
;; but you may use others as well (I have heard good things of Iedit too…).

;; Replace edit globally with potatoes. Replace edit globally with potatoes.

;; I then press C-c C-c (wgrep-finish-edit). Now every occurence of the word
;; edit in all files has been replaced with potatoes.


;;; Code:

(require 'smex)
(require 'flx) ; Enhance fuzzy matching

(ivy-mode 1)
(diminish 'ivy-mode "")
;; C-o m toggles the current regexp builder.
(setq ivy-re-builders-alist
      '(
        (ivy-switch-buffer . ivy--regex-plus)
        (swiper            . ivy--regex-plus)
        (t                 . ivy--regex-fuzzy)))
;;(setq ivy-initial-inputs-alist nil);; no regexp by default ^
(setq counsel-find-file-at-point t);; enable ffap feature
(setq counsel-find-file-ignore-regexp "\\.elc\\'");; "\(?:\‘[#.]\)\|\(?:[#~]\’\)"
(setq counsel-git-log-cmd "git log --grep \"%s\"")
(setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never \"%s\" %s")
(setq counsel-yank-pop-separator      (propertize "\n────────────────────────────────────────────────────────\n" 'face `(:foreground "#6272a4")));; using ivy-format-function-arrow with counsel-yank-pop
(setq enable-recursive-minibuffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-height 15) ; number of result lines to display
(setq ivy-on-del-error-function nil)
(setq ivy-use-selectable-prompt t) ; use C-p to preempt completion
(setq ivy-use-virtual-buffers t) ; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’
(setq ivy-virtual-abbreviate 'full)
(setq magit-completing-read-function 'ivy-completing-read)
(setq swiper-action-recenter t)

(defun reloading (cmd)
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))
(defun given-file (cmd prompt) ; needs lexical-binding
  (lambda (source)
    (let ((target
           (let ((enable-recursive-minibuffers t))
             (read-file-name
              (format "%s %s to:" prompt source)))))
      (funcall cmd source target 1))))
(defun confirm-delete-file (x)
  (dired-delete-file x 'confirm-each-subdirectory))

(defun ivy-yank-action (x)
  (kill-new x))


(ivy-add-actions
 'counsel-find-file
 `(("c" ,(given-file #'copy-file "Copy") "copy file")
   ("d" ,(reloading #'confirm-delete-file) "delete")
   ("m" ,(reloading (given-file #'rename-file "Move")) "move")
   ("x"  counsel-find-file-extern "open externally")
   ("y" ivy-yank-action "yank")
   ))

(ivy-add-actions
 'counsel-projectile-find-file
 `(("c" ,(given-file #'copy-file "Copy") "copy")
   ("d" ,(reloading #'confirm-delete-file) "delete")
   ("m" ,(reloading (given-file #'rename-file "Move")) "move")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ))


(with-eval-after-load 'projectile (setq projectile-completion-system 'ivy));; Integration with `projectile'
(with-eval-after-load 'magit      (setq magit-completing-read-function 'ivy-completing-read));; Integration with `magit'
;;
;; eye candy for ivy
;;
(require 'all-the-icons) ; More friendly display transformer for Ivy
(use-package ivy-rich
  :defines (all-the-icons-dir-icon-alist bookmark-alist)
  :functions (all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-auto-mode-match?
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))
  (defun ivy-rich-repo-icon (candidate)
    "Display repo icons in `ivy-rich`."
    (all-the-icons-octicon "repo" :height .9))
  ;; org capture
  (defun ivy-rich-org-capture-icon (candidate)
    "Display repo icons in `ivy-rich`."
    (message (concat "ivy-rich-org-capture-icon" candidate))
    (pcase (car (last (split-string (car (split-string candidate)) "-")))
      ("emacs"    (all-the-icons-fileicon "emacs" :height .68 :v-adjust .001))
      ("schedule" (all-the-icons-faicon   "calendar" :height .68 :v-adjust .005))
      ("tweet"    (all-the-icons-faicon   "commenting" :height .7 :v-adjust .01))
      ("link"     (all-the-icons-faicon   "link" :height .68 :v-adjust .01))
      ("memo"     (all-the-icons-faicon   "pencil" :height .7 :v-adjust .01))
      (_          (all-the-icons-octicon  "inbox" :height .68 :v-adjust .01))
      ))
  (defun ivy-rich-org-capture-title (candidate)
    (let* ((octl (split-string candidate))
           (title (pop octl))
           (desc (mapconcat 'identity octl " ")))
      (format "%-25s %s"
              title
              (propertize desc 'face `(:inherit font-lock-doc-face)))))
  ;; buffers
  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (when-let* ((buffer (get-buffer candidate))
                  (major-mode (buffer-local-value 'major-mode buffer))
                  (icon (if (and (buffer-file-name buffer)
                                 (all-the-icons-auto-mode-match? candidate))
                            (all-the-icons-icon-for-file candidate)
                          (all-the-icons-icon-for-mode major-mode))))
        (if (symbolp icon)
            (setq icon (all-the-icons-icon-for-mode 'fundamental-mode)))
        (unless (symbolp icon)
          (propertize icon
                      'face `(
                              :height 1.1
                              :family ,(all-the-icons-icon-family icon)
                              ))))))
  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let ((icon (if (file-directory-p candidate)
                      (cond
                       ((and (fboundp 'tramp-tramp-file-p)
                             (tramp-tramp-file-p default-directory))
                        (all-the-icons-octicon "file-directory"))
                       ((file-symlink-p candidate)
                        (all-the-icons-octicon "file-symlink-directory"))
                       ((all-the-icons-dir-is-submodule candidate)
                        (all-the-icons-octicon "file-submodule"))
                       ((file-exists-p (format "%s/.git" candidate))
                        (all-the-icons-octicon "repo"))
                       (t (let ((matcher (all-the-icons-match-to-alist candidate all-the-icons-dir-icon-alist)))
                            (apply (car matcher) (list (cadr matcher))))))
                    (all-the-icons-icon-for-file candidate))))
        (unless (symbolp icon)
          (propertize icon
                      'face `(
                              :height 1.1
                              :family ,(all-the-icons-icon-family icon)
                              ))))))
  ;; default
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  ;;
  ;;
  ;;
  :hook (ivy-rich-mode . (lambda ()
                           (setq ivy-virtual-abbreviate
                                 (or (and ivy-rich-mode 'abbreviate) 'name))))
  ;;
  ;;
  ;;
  :init
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon               :width 2)
            (ivy-rich-candidate                (:width 30))
            (ivy-rich-switch-buffer-size       (:width 7))
            (ivy-rich-switch-buffer-indicators (:width  4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 20 :face warning))
            (ivy-rich-switch-buffer-project    (:width 15 :face success))
            (ivy-rich-switch-buffer-path       (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 45))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 45))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 110))))
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 30))
            (ivy-rich-bookmark-info (:width 80))))
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          ivy-ghq-open
          (:columns
           ((ivy-rich-repo-icon)
            (ivy-rich-candidate)))
          ivy-ghq-open-and-fzf
          (:columns
           ((ivy-rich-repo-icon)
            (ivy-rich-candidate)))
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate)))
          counsel-org-capture
          (:columns
           ((ivy-rich-org-capture-icon)
            (ivy-rich-org-capture-title)
            ))
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-dir-transformer)))))
  )
(setq ivy-rich-parse-remote-buffer nil)
(setq ivy-rich-abbreviate-paths t)
(setq ivy-rich-switch-buffer-align-virtual-buffer t)
(ivy-rich-mode 1)



;;
;;
;;
(provide 'setup-ivy)
;; setup-ivy ends here
;;

