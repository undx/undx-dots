

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq autosavedir
      (concat undx-persistence-dir "auto-saves-list/"))
(unless (file-exists-p autosavedir)
  (mkdir autosavedir))
(setq auto-save-list-file-prefix autosavedir)

(defun make-backup-file-name (FILE)
  (let ((dirname (concat temporary-file-directory "backups/"
                         (format-time-string "%Y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))


(use-package persistent-scratch
  :init
  (setq persistent-scratch-save-file (concat undx-private-dir  (system-name) ".persistent-scratch"))
  (persistent-scratch-autosave-mode 1)
  )
(when (file-exists-p persistent-scratch-save-file)
  (persistent-scratch-restore))
;;
;;
(setq save-place-file  (concat undx-persistence-dir "places"))
(save-place-mode 1)

;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat undx-persistence-dir "savehist" ))
(savehist-mode t)

(use-package recentf
  :defer t
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
                          "\\.keyfreq$"
                          "/emacs.d/url/"
                          ".el.gz$"
                          "\\.ido.last$")
        recentf-max-saved-items 250)
  (recentf-mode 1))

(setq desktop-dirname             undx-persistence-dir
      desktop-base-file-name      "desktop"
      desktop-base-lock-name      "desktop.lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop t);; was nil
(desktop-save-mode t) ; nil for off

;; bookmarks file
(setq bookmark-default-file (concat undx-persistence-dir "bookmarks"))
;;
(setq delete-by-moving-to-trash t)



(provide 'setup-persistence)
